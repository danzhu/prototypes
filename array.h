#ifndef A1_ARRAY_H
#define A1_ARRAY_H

#include <array>
#include <cstddef>
#include <stdexcept>

template <typename T>
class Array
{
public:
    class Iterator
    {
    public:
        T &operator*() { return *m_item; }

        bool operator!=(const Iterator &other)
        {
            return m_item != other.m_item;
        }

        Iterator &operator++()
        {
            ++m_item;
            return *this;
        }

    private:
        explicit Iterator(T *item) : m_item{item} {}

        T *m_item;

        friend class Array;
    };

    template <std::size_t Size>
    Array(T (&data)[Size]) : m_data{data}, m_size{Size}
    {
    }
    template <std::size_t Size>
    Array(std::array<T, Size> &array) : m_data{array.data()}, m_size{Size}
    {
    }
    Array(T *data, std::size_t size) : m_data{data}, m_size{size} {}

    T &operator[](std::size_t index) { return m_data[index]; }

    T &at(std::size_t index)
    {
        if (index >= m_size)
            throw std::out_of_range{"index out of range"};
        return m_data[index];
    }

    Iterator begin() { return Iterator{m_data}; }
    Iterator end() { return Iterator{m_data + m_size}; }

    T *data() const { return m_data; }
    std::size_t size() const { return m_size; }

private:
    T *m_data;
    std::size_t m_size;
};

#endif
