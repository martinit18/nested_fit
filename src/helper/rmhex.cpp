// Brief  : This file contains a small fast executable that reads files
// 			and outputs a c-style array file much like xxd does.
// Author : César Godinho
// Date   : 22/07/2025

#include <cstdint>
#include <filesystem>
#include <fstream>
#include <ios>
#include <iostream>
#include <string_view>
#include <vector>
#include <algorithm>

void usage()
{
	std::cout << "USAGE:" << std::endl;
	std::cout << "rmhex <inputfile> <outputfile>" << std::endl;
}

std::vector<std::uint8_t> ReadBinaryFile(const std::string_view& path)
{
	std::ifstream file(path.data(), std::ios::binary | std::ios::ate);

	if(!file)
	{
		std::cout << "Failed to open file: " << path << std::endl;
		return {};
	}

	std::streamsize size = file.tellg();
	file.seekg(0, std::ios::beg);
	std::vector<std::uint8_t> buffer(size);

	if(!file.read(reinterpret_cast<char*>(buffer.data()), size))
	{
		std::cout << "Failed to read file: " << path << std::endl;
	}

	return buffer;
}

// NOTE: (Cesar) Simplify linkage by not setting const or constexpr
void ProduceOutputFile(const std::vector<std::uint8_t>& data, const std::string_view& path, const std::string_view& name)
{
	std::ofstream file(path.data(), std::ios::trunc);
	std::string iname = name.data();
	std::replace(iname.begin(), iname.end(), '.', '_');
	std::replace(iname.begin(), iname.end(), '/', '_');

	file << "unsigned char " << iname << "[] = {\n\t";
	for(std::size_t i = 0; i < data.size(); i++)
	{
		file << "0x" << std::hex << std::uppercase << std::setfill('0') << std::setw(2) << static_cast<int>(data[i]) << ",";
		if(((i + 1) % 8) == 0)
		{
			file << "\n\t";
		}
	}
	file << "\n};\n";
	file << "unsigned long " << iname << "_len = " << std::dec << data.size() << ";";
	file << std::flush;
}

int main(int argc, char* argv[])
{
	if(argc != 3)
	{
		std::cout << "ERROR: 2 arguments are required." << std::endl;
		usage();
		return 0;
	}

	const std::string_view input_file = argv[1];
	const std::string_view output_file = argv[2];

	if(!std::filesystem::exists(input_file))
	{
		std::cout << "File " << input_file << " does not exist." << std::endl;
		usage();
		return 0;
	}

	const std::vector<std::uint8_t> data = ReadBinaryFile(input_file);
	ProduceOutputFile(data, output_file, input_file);
	return 0;
}
