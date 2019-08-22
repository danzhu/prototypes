#include <functional>
#include <iostream>
#include <tuple>

namespace detail
{
template <typename T>
T read(std::istream &in)
{
    T val;
    in >> val;
    return val;
}

template <typename T>
void write(std::ostream &out, T val) { out << val; }

template <typename Fn, typename Tuple, std::size_t ...Is>
decltype(auto) apply_args(Fn fn, const Tuple &tup, std::index_sequence<Is...>)
{
    return fn(std::get<Is>(tup)...);
}
}

template <typename ...Args>
struct Creator;

template <>
struct Creator<>
{
    static std::tuple<> create_args(std::istream &in)
    {
        return std::make_tuple();
    }
};

template <typename T, typename ...Args>
struct Creator<T, Args...>
{
    static std::tuple<T, Args...> create_args(std::istream &in)
    {
        auto args = Creator<Args...>::create_args(in);
        auto arg = std::make_tuple(detail::read<T>(in));
        return std::tuple_cat(arg, args);
    }
};

template <typename Ret, typename ...Args>
class Proxy
{
public:
    explicit Proxy(Ret (*fn)(Args...)) : _fn{fn} {}

    void operator()(std::istream &in, std::ostream &out) const
    {
        auto args = Creator<Args...>::create_args(in);
        auto idcs = std::make_index_sequence<sizeof...(Args)>();
        auto res = detail::apply_args(_fn, args, idcs);
        detail::write(out, res);
    }

private:
    Ret (*_fn)(Args...);
};

template <typename ...Args>
class Proxy<void, Args...>
{
public:
    explicit Proxy(void (*fn)(Args...)) : _fn{fn} {}

    void operator()(std::istream &in, std::ostream &out) const
    {
        auto args = Creator<Args...>::create_args(in);
        auto idcs = std::make_index_sequence<sizeof...(Args)>();
        detail::apply_args(_fn, args, idcs);
    }

private:
    void (*_fn)(Args...);
};

template <typename Ret, typename ...Args>
std::function<void(std::istream &, std::ostream &)>
create_proxy(Ret (*fn)(Args...))
{
    return Proxy<Ret, Args...>{fn};
}

int test(int i, int j) { return 2 * i + j; }

void noop() {}

int main()
{
    create_proxy(test)(std::cin, std::cout);
    create_proxy(noop)(std::cin, std::cout);
}
