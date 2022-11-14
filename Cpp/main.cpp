#include <iostream>
#include <fstream>
#include <sstream>

int main() {
    std::ifstream theFile("../inputs/input-2021-01.txt");
    if (!theFile.is_open()) {
        std::cerr << "Unable to read input file\n";
        return 1;
    }
    std::stringstream buffer;
    buffer << theFile.rdbuf();
    auto str = buffer.str();
    theFile.close();
}
