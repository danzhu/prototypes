#include <iostream>

// template <typename... Args>
// struct Formatter {};

// template <>
// struct Formatter<> {};

struct Formatter
{
    std::size_t count;
};

constexpr std::size_t format_count(const char *str, std::size_t size)
{
    if (size == 0)
        return 0;
    return (*str == '%' ? 1 : 0) + format_count(str + 1, size - 1);
}

constexpr Formatter operator""_f(const char *str, std::size_t size)
{
    auto count = format_count(str, size);
    return Formatter{count};
}

int main()
{
    constexpr auto fmt = "% = %"_f;
    static_assert(fmt.count == 2, "format size");
    // auto str = fmt("x", 1);
    // std::cout << str << '\n';
}
