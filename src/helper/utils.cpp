#include <iostream>
#include <cstdio>
#include <filesystem>

extern "C" void DisableStdout()
{
    std::cout.rdbuf(NULL);
}

extern "C" void MakeDirectory(const char* path, bool* error)
{
	std::error_code ec;
	if(!std::filesystem::create_directories(path, ec)) *error = true;
	if(ec) *error = true;
}
