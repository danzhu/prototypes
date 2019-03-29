#include <functional>
#include <optional>

#include <iostream>
#include <map>
#include <set>
#include <vector>

template <typename T>
class Iterate
{
public:
    using ValueType = typename std::iterator_traits<T>::value_type;

    Iterate(T begin, T end) : m_begin{std::move(begin)}, m_end{std::move(end)}
    {
    }

    std::optional<ValueType> operator()()
    {
        if (m_begin == m_end)
            return {};

        ValueType val = *m_begin;
        ++m_begin;
        return {val};
    }

private:
    T m_begin;
    T m_end;
};

template <typename T>
class Range
{
public:
    using ValueType = T;

    Range(T start, T end, T step) : m_value{start}, m_end{end}, m_step{step} {}

    std::optional<ValueType> operator()()
    {
        if (m_value >= m_end)
            return {};

        ValueType val = m_value;
        m_value += m_step;
        return {val};
    }

private:
    T m_value;
    T m_end;
    T m_step;
};

template <typename Iter, typename Fn>
class Map
{
public:
    using ValueType = std::invoke_result_t<Fn, typename Iter::ValueType>;

    Map(Fn fn, Iter iter) : m_func{fn}, m_iter{iter} {}

    std::optional<ValueType> operator()()
    {
        if (auto val = m_iter())
            return {m_func(*val)};

        return {};
    }

private:
    Fn m_func;
    Iter m_iter;
};

template <typename Iter, typename Fn>
class Filter
{
public:
    using ValueType = typename Iter::ValueType;

    Filter(Fn fn, Iter iter) : m_func{fn}, m_iter{iter} {}

    std::optional<ValueType> operator()()
    {
        if (auto val = m_iter(); val && m_func(*val))
            return {*val};

        return {};
    }

private:
    Fn m_func;
    Iter m_iter;
};

template <typename Iter>
class Enumerate
{
public:
    using ValueType = std::pair<std::size_t, typename Iter::ValueType>;

    explicit Enumerate(Iter iter) : m_iter{iter} {}

    std::optional<ValueType> operator()()
    {
        if (auto val = m_iter())
            return {{m_count++, *val}};

        return {};
    }

private:
    Iter m_iter;
    std::size_t m_count{0};
};

template <typename Iter>
class Wrapper
{
public:
    class Iterator
    {
    public:
        using value_type = typename Iter::ValueType;
        using pointer = value_type *;
        using reference = value_type &;
        using difference_type = void;
        using iterator_category = std::input_iterator_tag;

        explicit Iterator(Iter *iter, std::optional<value_type> value)
            : m_iter{iter}, m_value{value}
        {
        }

        reference operator*() { return *m_value; }
        pointer operator->() { return &*m_value; }

        bool operator!=(const Iterator &) { return m_value.has_value(); }
        bool operator==(const Iterator &other) { return !(*this == other); }

        Iterator &operator++()
        {
            m_value = (*m_iter)();
            return *this;
        }

        Iterator operator++(int)
        {
            auto copy = *this;
            ++*this;
            return copy;
        }

    private:
        Iter *m_iter;
        std::optional<value_type> m_value;
    };

    explicit Wrapper(Iter iter) : m_iter{iter} {}

    template <typename Fn>
    auto map(Fn fn)
    {
        return Wrapper<Map<Iter, Fn>>{{fn, m_iter}};
    }

    template <typename Fn>
    auto filter(Fn fn)
    {
        return Wrapper<Filter<Iter, Fn>>{{fn, m_iter}};
    }

    auto enumerate() { return Wrapper<Enumerate<Iter>>{Enumerate{m_iter}}; }

    std::size_t count()
    {
        std::size_t counter{0};
        while (auto it = m_iter())
            ++counter;
        return counter;
    }

    template <typename Col>
    Col collect()
    {
        return Col(begin(), end());
    }

    Iterator begin() { return Iterator{&m_iter, m_iter()}; }
    Iterator end() { return Iterator{nullptr, {}}; }

private:
    Iter m_iter;
};

template <typename T>
auto iterate(T begin, T end)
{
    return Wrapper<Iterate<T>>{{std::move(begin), std::move(end)}};
}

template <typename T>
auto iterate(T &col)
{
    return iterate(std::begin(col), std::end(col));
}

template <typename T>
auto range(T start, T end, T step = T{1})
{
    return Wrapper<Range<T>>{
        {std::move(start), std::move(end), std::move(step)}};
}

template <typename T>
auto range(T end)
{
    return range(T{}, end);
}

int main()
{
    auto col = range(50)
                   .map([](auto i) { return i + 10; })
                   .filter([](auto i) { return i < 40; })
                   .enumerate()
                   .collect<std::map<int, int>>();

    std::cout << iterate(col).count() << '\n';

    for (auto[i, v] : col)
        std::cout << i << ' ' << v << '\n';
}
