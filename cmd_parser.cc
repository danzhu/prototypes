#include <iostream>
#include <string>
#include <initializer_list>

using c_str = const char *;

class Option
{
public:
    template <typename T>
    Option(char short_name, c_str long_name, T &data);

    template <typename T>
    Option(T &data);

private:
    T &m_data;
};

class Parser
{
public:
    Parser(std::initializer_list<Option> options);
    void parse(int argc, const char *argv[]);
};

struct Point
{
    int x;
    int y;
};

std::istream &operator>>(std::istream &in, Point &p)
{
    return in >> p.x >> p.y;
}

std::ostream &operator<<(std::ostream &out, Point p)
{
    return out << p.x << ' ' << p.y;
}

int main(int argc, const char *argv[])
{
    int i;
    bool b;
    std::string s;
    Point p;

    Parser{argc, argv}
        .arg('i', "int", i)
        .arg('b', "bool", b)
        .arg('s', "string", s)
        .arg(p)
        ;

    std::cout << "i = " << i << '\n'
              << "b = " << b << '\n'
              << "s = " << s << '\n'
              << "p = " << p << '\n';
}
