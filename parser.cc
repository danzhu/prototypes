#include <functional>
#include <iostream>
#include <memory>
#include <vector>

using Stream = std::string;

template <typename T>
struct ParserRes
{
    explicit ParserRes(Stream stream)
        : succ{false}, value{}, stream{std::move(stream)}
    {
    }
    ParserRes(T value, Stream stream)
        : succ{true}, value{std::move(value)}, stream{std::move(stream)}
    {
    }

    operator bool() { return succ; }

    bool succ;
    T value;
    Stream stream;
};

template <typename T>
using ParserFn = std::function<ParserRes<T>(Stream)>;

template <typename T>
struct Parser
{
    explicit Parser(ParserFn<T> parse) : parse{parse} {}

    ParserFn<T> parse;
};

template <typename T, typename U>
Parser<U> operator>>(Parser<T> par, std::function<Parser<U>(T)> fn)
{
    return Parser<U>{[=](auto s) {
        auto res = par.parse(s);
        if (!res)
            return ParserRes<U>{s};
        return fn(res.value).parse(res.stream);
    }};
}

template <typename T, typename U>
Parser<U> operator+(Parser<T> par, Parser<U> other)
{
    return Parser<U>{[=](auto s) {
        auto res = par.parse(s);
        if (!res)
            return ParserRes<U>{s};
        return other.parse(res.stream);
    }};
}

template <typename T>
Parser<T> operator|(Parser<T> par, Parser<T> other)
{
    return Parser<T>{[=](auto s) {
        auto res = par.parse(s);
        if (res)
            return std::move(res);
        return other.parse(s);
    }};
}

template <typename T, typename U, typename Fn>
Parser<U> fmap(Fn fn, Parser<T> par)
{
    return Parser<U>{[fn{std::move(fn)}, par{std::move(par)}](auto s) {
        if (auto res = par.parse(s))
            return ParserRes<U>{fn(std::move(res.value)), res.stream};
        return ParserRes<U>{s};
    }};
}

template <typename T>
Parser<T> pure(T val)
{
    return Parser<T>{[=](auto s) { return ParserRes<T>{val, s}; }};
}

template <typename T>
Parser<std::vector<T>> many(Parser<T> par)
{
    return Parser<std::vector<T>>{[=](auto s) {
        std::vector<T> vec;
        while (auto res = par.parse(s))
        {
            vec.emplace_back(res.value);
            s = std::move(res.stream);
        }
        return ParserRes<std::vector<T>>{vec, s};
    }};
}

Parser<char> one_of(std::string str)
{
    return Parser<char>{[=](auto s) {
        if (s.size() > 0 && str.find(s[0]) != std::string::npos)
            return ParserRes<char>{s[0], s.substr(1)};
        return ParserRes<char>{s};
    }};
}

template <typename T>
Parser<T> left_rec(Parser<T> par, std::function<Parser<T>(T)> rec)
{
    return Parser<T>{[=](auto s) {
        auto res = par.parse(s);
        if (!res)
            return res;
        while (auto cont = rec(std::move(res.value)).parse(s))
            res = std::move(cont);
        return res;
    }};
}

Parser<char> expect(char c)
{
    return Parser<char>{[=](auto s) {
        if (s.size() > 0 && s[0] == c)
            return ParserRes<char>{c, s.substr(1)};
        return ParserRes<char>{s};
    }};
}

class Expr
{
public:
    virtual ~Expr() {}
    virtual int eval() const = 0;
};

class IntExpr : public Expr
{
public:
    explicit IntExpr(int val) : m_val{val} {}

    int eval() const override { return m_val; }

private:
    int m_val;
};

class BinExpr : public Expr
{
public:
    BinExpr(char op, std::unique_ptr<Expr> left, std::unique_ptr<Expr> right)
        : m_op{op}, m_left{std::move(left)}, m_right{std::move(right)}
    {
    }

    int eval() const override
    {
        auto left = m_left->eval();
        auto right = m_right->eval();
        switch (m_op)
        {
        case '+':
            return left + right;
        case '-':
            return left - right;
        case '*':
            return left * right;
        case '/':
            return left / right;
        default:
            throw std::logic_error{"expected operator"};
        }
    }

private:
    char m_op;
    std::unique_ptr<Expr> m_left;
    std::unique_ptr<Expr> m_right;
};

int main()
{
    auto atom = fmap<std::vector<char>, std::unique_ptr<Expr>>(
        [](std::vector<char> vec) {
            return std::make_unique<IntExpr>(
                std::stoi(std::string{vec.begin(), vec.end()}));
        },
        many(one_of("0123456789")));
    auto factor = left_rec<std::unique_ptr<Expr>>(
        atom, [&](std::unique_ptr<Expr> left) -> Parser<std::unique_ptr<Expr>> {
            return one_of("*/") +
                   fmap<std::unique_ptr<Expr>, std::unique_ptr<Expr>>(
                       [left{std::move(left)}](std::unique_ptr<Expr> right)
                           -> std::unique_ptr<Expr> {
                               return std::make_unique<BinExpr>(
                                   '*', std::move(left), std::move(right));
                           },
                       atom);
        });

    std::string line;
    while (std::getline(std::cin, line))
    {
        auto res = factor.parse(line);
        if (res)
            std::cout << res.value->eval();
        else
            std::cout << "parse error";
        std::cout << '\n';
    }
}
