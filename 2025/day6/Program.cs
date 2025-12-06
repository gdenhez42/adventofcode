var lines = File.ReadLines("input.txt").ToArray();

long[][] numbers = new long[lines.Length - 1][];
for(int i = 0; i < lines.Length - 1; i++)
{
    var lineSplit = lines[i].Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(e => long.Parse(e)).ToArray();
    numbers[i] = lineSplit;
}

char[] operators = lines[lines.Length - 1].Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(e => e.ToCharArray()[0]).ToArray();

long total = 0;
for(int i = 0; i < operators.Length; i++)
{
    if (operators[i] == '*')
    {
        long opTotal = 1;
        for(int j = 0; j < numbers.Length; j++)
        {
            opTotal *= numbers[j][i];
        }
        total += opTotal;
    }

    if (operators[i] == '+')
    {
        long opTotal = 0;
        for(int j = 0; j < numbers.Length; j++)
        {
            opTotal += numbers[j][i];
        }
        total += opTotal;
    }
}

Console.WriteLine($"Part1: {total}");




char[][] numberCharArrays = new char[lines.Length - 1][];
for(int i = 0; i < lines.Length - 1; i++)
{
    numberCharArrays[i] = lines[i].ToCharArray();
}
char[] operatorsCharArray = lines[lines.Length - 1].ToCharArray();

List<long> numbersPart2 = [];
int line = operatorsCharArray.Length;
total = 0;
while (line > 0)
{
    long numberPart2 = 0;
    for (int j = 0; j < numberCharArrays.Length; j++)
    {
        if (numberCharArrays[j][line-1] != ' ') {
            numberPart2 *= 10;
            numberPart2 += (long)numberCharArrays[j][line-1] - 48;
        }
    }
    numbersPart2.Add(numberPart2);
    
    if (operatorsCharArray[line-1] != ' ') {
        if (operatorsCharArray[line-1] == '*')
        {
            long opTotal = 1;
            for(int j = 0; j < numbersPart2.Count; j++)
            {
                opTotal *= numbersPart2[j];
            }
            total += opTotal;
        }

        if (operatorsCharArray[line-1] == '+')
        {
            long opTotal = 0;
            for(int j = 0; j < numbersPart2.Count; j++)
            {
                opTotal += numbersPart2[j];
            }
            total += opTotal;
        }

        numbersPart2 = [];
        line--;
    }
    line--;
}

Console.WriteLine($"Part2: {total}");