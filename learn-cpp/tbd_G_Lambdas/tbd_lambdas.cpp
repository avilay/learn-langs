/*
 * The basic syntax is as follows:
 * auto lambda = [any, closure, args](any, local, args) { impl; };
 */
#include <iostream>

int main(int argc, char** argv)
{
    auto lambda = [](int a, int b) { return a + b; };
    std::cout << lambda(10, 20) << std::endl;

    auto lambda_in_lambda = [](int arg) {
        return [arg](int x) { return arg + x; };
    };
    int ans = lambda_in_lambda(10)(20);
    std::cout << ans << std::endl;
}
