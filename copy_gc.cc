#include <cstdint>

class Allocator
{
public:
    void *alloc(std::size_t size);
    void free(void *ptr);
};

enum class GcMark
{
    ROOT,
    MARK,
    FREE,
};

template <typename T>
class GcCell
{
    T m_value;
    GcMark m_mark;
};

template <typename T>
class Gc
{
public:
    Gc() : m_ptr{nullptr} {}
    ~Gc()
    {
        delete (m_ptr)();
    }

private:
    GcCell<T> *m_ptr;
};
