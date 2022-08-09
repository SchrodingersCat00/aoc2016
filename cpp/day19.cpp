#include <vector>
#include <iostream>

auto main() -> int {
    auto a = std::vector<unsigned int>{};
    for (auto i = 1; i < 3018458; i++) {
        a.push_back(i);
    }
    a.push_back(0);

    auto i = static_cast<unsigned int>(0);
    while (a[i] != i) {
        auto nb = a[a[i]];
        a[i] = nb;
        i = nb;
    }

    std::cout << i+1 << '\n';
}