#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define DEF_VECTOR(vec_t, elm_t)                                               \
    typedef struct                                                             \
    {                                                                          \
        elm_t *content;                                                        \
        size_t size;                                                           \
        size_t capacity;                                                       \
    } vec_t;                                                                   \
                                                                               \
    static vec_t vec_t##_new()                                                 \
    {                                                                          \
        vec_t v = {NULL, 0, 0};                                                \
        return v;                                                              \
    }                                                                          \
                                                                               \
    static void vec_t##_delete(vec_t self) { free(self.content); }             \
                                                                               \
    static size_t vec_t##_size(const vec_t *self) { return self->size; }       \
                                                                               \
    static size_t vec_t##_capacity(const vec_t *self)                          \
    {                                                                          \
        return self->capacity;                                                 \
    }                                                                          \
                                                                               \
    static void vec_t##_reserve(vec_t *self, size_t size)                      \
    {                                                                          \
        if (size <= self->capacity)                                            \
            return;                                                            \
                                                                               \
        self->capacity = self->capacity > 0 ? self->capacity * 2 : 4;          \
        while (size > self->capacity)                                          \
            self->capacity *= 2;                                               \
                                                                               \
        self->content =                                                        \
            (elm_t *)realloc(self->content, sizeof(elm_t) * self->capacity);   \
    }                                                                          \
                                                                               \
    static elm_t *vec_t##_get(vec_t *self, size_t idx)                         \
    {                                                                          \
        return &self->content[idx];                                            \
    }                                                                          \
                                                                               \
    static void vec_t##_push(vec_t *self, elm_t val)                           \
    {                                                                          \
        vec_t##_reserve(self, self->size + 1);                                 \
        self->content[self->size] = val;                                       \
        ++self->size;                                                          \
    }                                                                          \
                                                                               \
    static elm_t vec_t##_pop(vec_t *self)                                      \
    {                                                                          \
        --self->size;                                                          \
        return self->content[self->size];                                      \
    }

DEF_VECTOR(IntVec, int)

int main()
{
    IntVec v = IntVec_new();

    for (int i = 0; i < 20; ++i)
        IntVec_push(&v, i);

    assert(IntVec_pop(&v) == 19);
    assert(IntVec_capacity(&v) >= 19);

    size_t size = IntVec_size(&v);
    assert(size == 19);

    for (size_t i = 0; i < size; ++i)
        assert((int)i == *IntVec_get(&v, i));

    IntVec_delete(v);
}
