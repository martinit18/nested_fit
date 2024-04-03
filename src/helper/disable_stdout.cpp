#include <iostream>
#include <cstdio>

extern "C" void DisableStdout()
{
    std::cout.rdbuf(NULL);
}
