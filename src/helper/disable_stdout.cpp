#include <iostream>

extern "C" void DisableStdout()
{
    std::cout.rdbuf(NULL);
}
