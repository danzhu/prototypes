#include <iostream>

template <typename T>
class Arr {};

int main() {
    std::size_t n;
    std::cin >> n;
    Arr<int[n]> arr;
}
