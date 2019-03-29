#include <iostream>
#include <utility>

template <typename T>
class Box
{
public:
    using element_type = T;

    constexpr Box() noexcept : m_ptr{nullptr} {}
    constexpr explicit Box(T *ptr) noexcept : m_ptr{ptr} {}
    ~Box() { delete m_ptr; }

    Box(const Box &other) : m_ptr{other.m_ptr->clone()} {}
    Box(Box &&other) noexcept : m_ptr{other.release()} {}

    template <typename U,
              typename = std::enable_if_t<std::is_convertible<U *, T *>::value>>
    Box(Box<U> other) noexcept : m_ptr{other.release()}
    {
    }

    Box &operator=(Box other) noexcept
    {
        swap(other);
        return *this;
    }

    T &operator*() const noexcept { return *m_ptr; }
    T *operator->() const noexcept { return m_ptr; }

    operator bool() const noexcept { return m_ptr != nullptr; }

    T *get() const noexcept { return m_ptr; }
    T *release() noexcept { return std::exchange(m_ptr, nullptr); }
    void swap(Box &other) noexcept { std::swap(m_ptr, other.m_ptr); }
    Box *clone() const { return new Box{*this}; }

private:
    T *m_ptr;
};

template <typename T>
struct std::hash<Box<T>>
{
    std::size_t operator()(const Box<T> &box) const noexcept
    {
        return std::hash<T *>{}(box.get());
    }
};

template <typename T>
bool operator==(const Box<T> &a, std::nullptr_t) noexcept
{
    return a.get() == nullptr;
}
template <typename T>
bool operator!=(const Box<T> &a, std::nullptr_t) noexcept
{
    return a.get() != nullptr;
}
template <typename T>
bool operator==(std::nullptr_t, const Box<T> &b) noexcept
{
    return nullptr == b.get();
}
template <typename T>
bool operator!=(std::nullptr_t, const Box<T> &b) noexcept
{
    return nullptr != b.get();
}
template <typename T>
bool operator==(const Box<T> &a, const Box<T> &b) noexcept
{
    return a.get() == b.get();
}
template <typename T>
bool operator!=(const Box<T> &a, const Box<T> &b) noexcept
{
    return a.get() != b.get();
}
template <typename T>
bool operator<(const Box<T> &a, const Box<T> &b) noexcept
{
    return a.get() < b.get();
}
template <typename T>
bool operator>(const Box<T> &a, const Box<T> &b) noexcept
{
    return a.get() > b.get();
}
template <typename T>
bool operator<=(const Box<T> &a, const Box<T> &b) noexcept
{
    return a.get() <= b.get();
}
template <typename T>
bool operator>=(const Box<T> &a, const Box<T> &b) noexcept
{
    return a.get() >= b.get();
}

template <typename CharT, typename Traits, typename T>
std::basic_ostream<CharT, Traits> &
operator<<(std::basic_ostream<CharT, Traits> &os, const Box<T> &box) noexcept
{
    return os << box.get();
}

template <typename T>
void swap(Box<T> &a, Box<T> &b) noexcept
{
    a.swap(b);
}

template <typename T, typename... Args>
Box<T> make_box(Args &&... args)
{
    return Box<T>{new T{std::forward<Args>(args)...}};
}

// --- example ---
#include <string>
#include <unordered_set>

class Base
{
public:
    virtual ~Base() {}
    virtual Base *clone() const = 0;
    virtual void print() const = 0;
};

class Derived : public Base
{
public:
    explicit Derived(std::string val) : m_val{std::move(val)} {}
    Derived *clone() const override { return new Derived{*this}; }
    void print() const override { std::cout << m_val << '\n'; }

private:
    std::string m_val;
};

int main()
{
    Box<Base> p{make_box<Derived>("hello")};
    auto copy = p;
    std::cout << copy << '\n';

    swap(p, copy);

    std::unordered_set<Box<Base>> s{p};
    for (const auto &ptr : s)
        ptr->print();
}
