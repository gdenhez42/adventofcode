var lines = File.ReadLines("input.txt");

long sum = 0;
foreach(var line in lines)
{
    var numbers = line.ToCharArray().Select(e => (int)e-48).ToArray();
    int max = 0;
    for(int i = 0; i < numbers.Length-1; i++)
    {
        for(int j=i+1; j < numbers.Length; j++)
        {
            int number = numbers[i]*10 + numbers[j];
            if (number > max)
            {
                max = number;
            }
        }
    }
    sum += max;
}
Console.WriteLine($"Part1: {sum}");

sum = 0;
foreach(var line in lines)
{
    var numbers = line.ToCharArray().Select(e => (int)e-48).ToArray();

    List<int> maxJoltagesDigits = [];
    int index = 0;
    for(int digit = 0; digit < 12; digit++)
    {
        int max = numbers[index];
        for(int i = index; i < numbers.Length-11+digit; i++)
        {
            if (numbers[i] > max)
            {
                max = numbers[i];
                index = i;
            }
        }
        index += 1;

        maxJoltagesDigits.Add(max);

    }

    long maxJoltage = 0;
    foreach(int digit in maxJoltagesDigits)
    {
        maxJoltage *= 10;
        maxJoltage += digit;
    }

    sum += maxJoltage;
}
Console.WriteLine($"Part2: {sum}");