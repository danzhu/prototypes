#include <algorithm>
#include <array>
#include <iostream>
#include <vector>

template <typename T, std::size_t Dim>
class GridIter;

template <typename T, std::size_t Dim>
class Grid
{
    static_assert(Dim > 0, "Dim must be greater than 0");

public:
    using size_type = std::array<std::size_t, Dim>;
    using iterator = GridIter<T, Dim>;

    explicit Grid(size_type sizes)
        : m_sizes{sizes},
          m_values(std::accumulate(sizes.begin(), sizes.end(), 0))
    {
    }

    T &operator[](size_type index)
    {
        std::size_t idx = index[0];
        for (std::size_t i = 1; i < Dim; ++i)
            idx = idx * m_sizes[i] + index[i];

        return m_values[idx];
    }

    iterator begin();
    iterator end();

private:
    size_type m_sizes;
    std::vector<T> m_values;
};

template <typename T, std::size_t Dim>
class GridSlice
{};

template <typename T>
class GridIter<T, 1>
{
public:
    GridIter(std::size_t *sizes, T *values)
        : m_sizes{sizes}, m_values{values}
    {}

    T &operator*() { return *m_values; }

    GridIter &operator++()
    {
        m_values += *m_sizes;
        return *this;
    }

    bool operator!=(const GridIter &other) { return m_values != other.m_values; }

private:
    std::size_t *m_sizes;
    T *m_values;
};

template <typename T, std::size_t Dim>
class GridIter
{
public:
    using value_type = GridIter<T, Dim - 1>;

    GridIter(std::size_t *sizes, T *values)
        : m_sizes{sizes}, m_values{values}
    {}

    value_type operator*() { return {m_sizes + 1, m_values + 1}; }

    GridIter &operator++()
    {
        m_values += *m_sizes;
        return *this;
    }

    bool operator!=(const GridIter &other) { return m_values != other.m_values; }

private:
    std::size_t *m_sizes;
    T *m_values;
};

int main()
{
    Grid<int, 2> grid{{{2, 3}}};
    grid[{{0, 2}}] = 2;
    grid[{{1, 1}}] = 1;

    for (auto r : grid)
    {
        for (auto i : r)
            std::cout << i;

        std::cout << '\n';
    }
}
