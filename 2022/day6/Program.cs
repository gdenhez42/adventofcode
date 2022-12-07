string text = System.IO.File.ReadAllText("input.txt");

// Part 1
for(int i=0; i<text.Length-3; i++) {
    // too lazy to write a loop and not lazy enough to copy paste
    if (text[i] != text[i+1] &&
        text[i] != text[i+2] &&
        text[i] != text[i+3] &&
        text[i+1] != text[i+2] &&
        text[i+1] != text[i+3] &&
        text[i+2] != text[i+3])
    {
        Console.WriteLine(i + 4);
        break;
    }
}

// Part2
for(int i=0; i<text.Length-13; i++) {
    // ok you win... I'll write some loops
    bool allDifferent = true;
    for (int j = 0; j < 14; j++) {
        for (int k = j+1; k < 14; k++) {
            if (text[i+j] == text[i+k]) {
                allDifferent = false;
            }
        }
    }
    if (allDifferent)
    {
        Console.WriteLine(i + 14);
        break;
    }
}

