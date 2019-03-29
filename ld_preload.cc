#include <dlfcn.h>
#include <fcntl.h>
#include <iostream>

using open_t = int (*)(const char *, int);

const auto c_open = reinterpret_cast<open_t>(dlsym(RTLD_NEXT, "open"));

extern "C" int open(const char *file, int flags, ...) {
    auto ret = c_open(file, flags);

    std::cerr << "open: " << file << " -> " << ret << '\n';

    return ret;
}
