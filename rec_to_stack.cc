#include <cassert>
#include <vector>
#include <iostream>

int fib(int n) {
    if (n < 2)
        return n;
    return fib(n - 1) + fib(n - 2);
}

int fib_loop(int n) {
    enum class State {
        Init,
        Fib1,
        Fib2,
    };
    struct Frame {
        int n;
        State state;
        int fib1;

        Frame(int n): n{n}, state{State::Init}, fib1{0} {}
    };

    std::vector<Frame> frames;
    frames.emplace_back(n);
    int ret;
    while (!frames.empty()) {
        auto &f = frames.back();
        // std::cerr << static_cast<int>(f.state) << ' ' << f.n << '\n';
        switch (f.state) {
        case State::Init:
            if (f.n < 2) {
                ret = f.n;
                frames.pop_back();
            } else {
                f.state = State::Fib1;
                frames.emplace_back(f.n - 1);
            }
            break;
        case State::Fib1:
            f.fib1 = ret;
            f.state = State::Fib2;
            frames.emplace_back(f.n - 2);
            break;
        case State::Fib2:
            ret = f.fib1 + ret;
            frames.pop_back();
            break;
        default:
            assert(false);
        }
    }

    return ret;
}

int main() {
    int n;
    while (std::cin >> n) {
        std::cout << fib_loop(n) << '\n';
    }
}
