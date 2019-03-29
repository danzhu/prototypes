#include <execinfo.h>
#include <string.h>
#include <sys/ptrace.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <unistd.h>

#include <array>
#include <iostream>

template <typename... Args>
void log(Args &&... args) {
    (std::cerr << ... << std::forward<Args>(args)) << '\n';
}

template <typename... Args>
[[noreturn]] void panic(Args &&... args) {
    log("panic: ", std::forward<Args>(args)...);

    std::array<void *, 16> bt;
    auto size = backtrace(bt.data(), bt.size());
    backtrace_symbols_fd(bt.data(), size, STDERR_FILENO);

    for (std::size_t i = 0; i < size; ++i)
        std::cerr << bt[i] << '\n';

    std::exit(2);
}

struct Check {
    const char *msg;
};

template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
T operator|(T ret, Check c) {
    if (ret >= 0)
        return ret;

    if (ret == -1)
        panic(c.msg, " [errno = ", errno, ": ", strerror(errno), "]");
    else
        panic(c.msg, " [ret = ", ret, "]");
    return ret;
}

template <typename T>
T operator|(const T *ret, Check c) {
    if (ret == nullptr)
        panic(c.msg, " [nullptr]");
    return ret;
}

Check operator""_e(const char *msg, std::size_t) { return Check{msg}; }

int main() {
    if (auto pid = fork() | "fork"_e) {
        while (true) {
            int status;
            waitpid(pid, &status, 0) | "waitpid"_e;
            if (WIFSTOPPED(status) && WSTOPSIG(status) == SIGTRAP) {
                user_regs_struct regs;
                ptrace(PTRACE_GETREGS, pid, &regs) | "ptrace"_e;
                std::cerr << "syscall " << regs.rax << "\n";
            }
            if (WIFEXITED(status) || WIFSIGNALED(status))
                break;
            ptrace(PTRACE_SYSCALL, pid, nullptr, nullptr) | "ptrace"_e;
        }
        std::cerr << "end\n";
    } else {
        ptrace(PTRACE_TRACEME) | "ptrace"_e;
        std::array<const char *, 2> args{"/bin/ls", nullptr};
        execve(args[0], const_cast<char **>(args.data()), nullptr) | "execve"_e;
        return 0;
    }
}
