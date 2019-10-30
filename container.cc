#include <cassert>
#include <iostream>

template <typename T>
class Array {
public:
    using value_type = T;

    explicit Array(std::size_t capacity)
        : m_content{new value_type[capacity]}, m_capacity{capacity} {}
    Array(Array &&other) { swap(other); }
    ~Array() { delete[] m_content; }

    Array &operator=(Array &&other) {
        swap(other);
        return *this;
    }

    void swap(Array &other) {
        std::swap(m_content, other.m_content);
        std::swap(m_capacity, other.m_capacity);
        std::swap(m_size, other.m_size);
    }

    bool move(Array &&other) {
        if (m_size + other.m_size > m_capacity)
            return false;
        for (std::size_t i = 0; i < other.m_size; ++i) {
            m_content[m_size] = std::move(other.m_content[i]);
            ++m_size;
        }
        return true;
    }

    bool push_back(value_type item) noexcept {
        if (m_size == m_capacity)
            return false;

        m_content[m_size] = item;
        ++m_size;
        return true;
    }

    value_type &at(std::size_t index) {
        if (index >= m_size)
            throw std::out_of_range{"index out of range"};
        return m_content[index];
    }

    std::size_t capacity() const noexcept { return m_capacity; }

private:
    value_type *m_content = nullptr;
    std::size_t m_capacity = 0;
    std::size_t m_size = 0;
};

template <typename T>
class Dyn {
public:
    using value_type = typename T::value_type;

    Dyn() : m_container{0} {}

    void grow() noexcept {
        auto cap = m_container.capacity();
        T con{cap > 0 ? cap * 2 : 4};
        auto succ = con.move(std::move(m_container));
        assert(succ);
        std::swap(m_container, con);
    }

    void push_back(value_type item) noexcept {
        auto succ = m_container.push_back(std::move(item));
        if (succ)
            return;
        grow();
        succ = m_container.push_back(std::move(item));
        assert(succ);
    }

    value_type &at(std::size_t index) { return m_container.at(index); }

private:
    T m_container;
};

int main() {
    Dyn<Array<int>> arr;
    for (std::size_t i = 0; i < 10; ++i)
        arr.push_back(i);
    for (std::size_t i = 0; i < 10; ++i)
        std::cout << arr.at(i) << '\n';
}
