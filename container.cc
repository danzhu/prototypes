#include <iostream>

template <typename T>
class Array
{
public:
    using value_type = T;

    explicit Array(int capacity) : m_capacity{capacity} {}

    bool push_back(T item) noexcept
    {
        if (m_size == m_capacity)
            return false;

        m_content[m_size] = item;
        ++m_size;
        return true;
    }

    T &at(std::size_t index) noexcept { return m_content[index]; }

private:
    T *m_content;
    int m_size;
    int m_capacity;
};

template <typename T>
class Dyn
{
public:
    using value_type = typename T::value_type;

    Dyn() : m_container{0} {}

    void push_front(value_type item) noexcept
    {
        m_container.push_front(std::move(item));
    }

    void push_back(value_type item) noexcept
    {
        m_container.push_back(std::move(item));
    }

private:
    T m_container;
};

int main()
{
    Dyn<Array<int>> arr;
    arr.push_back(1);
}
