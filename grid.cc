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
    using iterator = typename trait::iterator;
    using reference = typename std::iterator_traits<iterator>::reference;

    GridView(T *data, std::size_t *offsets)
        : m_data{data}, m_offsets{offsets} {}

    reference operator[](std::size_t index) { return *(begin() + index); }

    T &operator[](size_type index) {
        std::size_t idx = index[Dim - 1];
        for (int i = Dim - 2; i >= 0; --i)
            idx += m_offsets[i + 1] * index[i];
        return m_data[idx];
    }

    iterator begin() { return trait::iter(m_data, &m_offsets[1]); }
    iterator end() { return trait::iter(m_data + m_offsets[0], &m_offsets[1]); }

private:
    T *m_data;
    std::size_t *m_offsets;
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

    bool operator!=(const GridIter &other) { return m_data != other.m_data; }
    value_type operator*() { return {m_data, m_offsets}; }

    GridIter operator+(difference_type diff) {
        return {m_data + diff * m_offsets[0], m_offsets};
    }

    GridIter operator-(difference_type diff) { return *this + (-diff); }
    GridIter operator+=(difference_type diff) { return *this = *this + diff; }
    GridIter operator-=(difference_type diff) { return *this = *this - diff; }

    GridIter &operator++() {
        *this += 1;
        return *this;
    }

    GridIter &operator--() {
        *this -= 1;
        return *this;
    }

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
    using iterator = typename view_type::iterator;
    using reference = typename view_type::reference;

    explicit Grid(size_type sizes)
        : m_data(std::accumulate(sizes.begin(), sizes.end(), 1,
                                 std::multiplies<>())) {
        std::size_t offset = 1;
        for (int i = Dim - 1; i >= 0; --i) {
            offset *= sizes[i];
            m_offsets[i] = offset;
        }
    }

    reference operator[](std::size_t index) { return view()[index]; }
    T &operator[](size_type index) { return view()[index]; }

    view_type view() { return {m_data.data(), m_offsets.data()}; }
    iterator begin() { return view().begin(); }
    iterator end() { return view().end(); }

private:
    std::vector<T> m_data;
    size_type m_offsets;
};

int main() {
    Grid<int, 2> grid{{{2, 3}}};
    grid[{{0, 2}}] = 2;
    grid[1][1] = 1;

    std::cout << grid[0][2] << '\n';

    for (auto r : grid) {
        for (auto i : r)
            std::cout << i << ' ';
        std::cout << '\n';
    }
}
