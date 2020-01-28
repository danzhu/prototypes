#include <array>
#include <iostream>
#include <numeric>
#include <vector>

template <typename T, std::size_t Dim>
class GridView;

template <typename T, std::size_t Dim>
class GridIter;

namespace std {
template <typename T, std::size_t Dim>
struct iterator_traits<GridIter<T, Dim>> {
    using value_type = GridView<T, Dim>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type;
    using reference = value_type;
    using iterator_category = std::random_access_iterator_tag;
};
} // namespace std

template <typename T, std::size_t Dim>
struct GridTrait {
    using iterator = GridIter<T, Dim - 1>;

    static iterator iter(T *data, std::size_t *offsets) {
        return {data, offsets};
    }
};

template <typename T>
struct GridTrait<T, 1> {
    using iterator = T *;

    static iterator iter(T *data, std::size_t *offsets) {
        (void)offsets;
        return data;
    }
};

template <typename T, std::size_t Dim>
class GridView {
public:
    using size_type = std::array<std::size_t, Dim>;
    using trait = GridTrait<T, Dim>;

    GridView(T *data, std::size_t *offsets)
        : m_data{data}, m_offsets{offsets} {}

    decltype(auto) operator[](std::size_t index) { return *(begin() + index); }

    T &operator[](size_type index) {
        std::size_t idx = index[Dim - 1];
        for (std::ptrdiff_t i = Dim - 2; i >= 0; --i)
            idx += m_offsets[i + 1] * index[i];
        return m_data[idx];
    }

    decltype(auto) at(std::size_t index) {
        auto it = begin() + index;
        if (it >= end())
            throw std::out_of_range{"index out of range"};
        return *it;
    }

    T &at(size_type index) {
        auto it = &(*this)[index];
        if (it >= _end())
            throw std::out_of_range{"index out of range"};
        return *it;
    }

    decltype(auto) begin() { return trait::iter(m_data, &m_offsets[1]); }
    decltype(auto) end() { return trait::iter(_end(), &m_offsets[1]); }

private:
    T *m_data;
    std::size_t *m_offsets;

    T *_end() { return m_data + m_offsets[0]; }
};

template <typename T, std::size_t Dim>
class GridIter {
    static_assert(Dim > 0, "dimension must be positive");

public:
    using trait = std::iterator_traits<GridIter>;
    using value_type = typename trait::value_type;
    using difference_type = typename trait::difference_type;

    GridIter(T *data, std::size_t *offsets)
        : m_data{data}, m_offsets{offsets} {}

    value_type operator*() { return {m_data, m_offsets}; }
    bool operator!=(const GridIter &other) { return m_data != other.m_data; }
    bool operator<(const GridIter &other) { return m_data < other.m_data; }
    GridIter operator+(difference_type diff) {
        return {m_data + diff * m_offsets[0], m_offsets};
    }

    bool operator>(const GridIter &other) { return other < *this; }
    bool operator<=(const GridIter &other) { return !(*this > other); }
    bool operator>=(const GridIter &other) { return !(*this < other); }
    GridIter operator-(difference_type diff) { return *this + (-diff); }
    GridIter &operator+=(difference_type diff) { return *this = *this + diff; }
    GridIter &operator-=(difference_type diff) { return *this = *this - diff; }
    GridIter &operator++() { return *this += 1; }
    GridIter &operator--() { return *this -= 1; }

private:
    T *m_data;
    std::size_t *m_offsets;
};

template <typename T, std::size_t Dim>
class Grid {
    static_assert(Dim > 0, "dimension must be positive");

public:
    using view_type = GridView<T, Dim>;
    using size_type = typename view_type::size_type;

    explicit Grid(size_type sizes)
        : m_data(std::accumulate(sizes.begin(), sizes.end(), 1,
                                 std::multiplies<>())) {
        std::size_t offset = 1;
        for (std::ptrdiff_t i = Dim - 1; i >= 0; --i) {
            offset *= sizes[i];
            m_offsets[i] = offset;
        }
    }

    decltype(auto) operator[](std::size_t index) { return view()[index]; }
    decltype(auto) operator[](size_type index) { return view()[index]; }

    decltype(auto) at(std::size_t index) { return view().at(index); }
    decltype(auto) at(size_type index) { return view().at(index); }

    decltype(auto) begin() { return view().begin(); }
    decltype(auto) end() { return view().end(); }

    view_type view() { return {m_data.data(), m_offsets.data()}; }

private:
    std::vector<T> m_data;
    size_type m_offsets;
};

int main() {
    Grid<int, 2> grid{{{2, 3}}};
    grid[{{0, 2}}] = 2;
    grid[1][1] = 1;

    std::cout << grid.at(0).at(2) << '\n';
    std::cout << grid.at({{1, 1}}) << '\n';

    try {
        // FIXME: this should throw
        grid.at({{0, 3}});
    } catch (const std::out_of_range &) {
        std::cout << "caught out of range\n";
    }

    for (auto r : grid) {
        for (auto i : r)
            std::cout << i << ' ';
        std::cout << '\n';
    }
}
