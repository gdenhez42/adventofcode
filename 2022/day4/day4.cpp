#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::fstream fs;
    fs.open("input.txt", std::fstream::in);

    int nbPairsCompletelyOverlapping = 0;
    int nbPairsOverlapping = 0;
    std::string line;
    while(!std::getline(fs, line).fail()) {
        size_t pos;
        int firstPairBeg = std::stoi(line, &pos);
        line = line.substr(pos+1);
        int firstPairEnd = std::stoi(line, &pos);
        line = line.substr(pos+1);
        int secondPairBeg = std::stoi(line, &pos);
        line = line.substr(pos+1);
        int secondPairEnd = std::stoi(line);
        
        if ((firstPairBeg >= secondPairBeg && firstPairEnd <= secondPairEnd) ||
            (secondPairBeg >= firstPairBeg && secondPairEnd <= firstPairEnd))
        {
            nbPairsCompletelyOverlapping++;
        }

        if ((firstPairBeg >= secondPairBeg && firstPairBeg <= secondPairEnd) ||
            (firstPairEnd >= secondPairBeg && firstPairEnd <= secondPairEnd) ||
            (secondPairBeg >= firstPairBeg && secondPairBeg <= firstPairEnd) ||
            (secondPairEnd >= firstPairBeg && secondPairEnd <= firstPairEnd))
        {
            nbPairsOverlapping++;
        }
    }
    std::cout << "Part1: " << nbPairsCompletelyOverlapping << std::endl;
    std::cout << "Part2: " << nbPairsOverlapping << std::endl;

    return 0;
}