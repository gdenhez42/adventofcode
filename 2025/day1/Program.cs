// See https://aka.ms/new-console-template for more information
var lines = File.ReadLines("input.txt").ToList();
int state = 50;
int count = 0;

foreach(var line in lines)
{
    var direction = line[0];
    var ticks = int.Parse(line.Substring(1));

    if (direction == 'R') state = (state + ticks) % 100;
    if (direction == 'L') state = (state - ticks) % 100;

    if (state == 0) count++;

    
}

Console.WriteLine($"part 1: {count}");

state = 50;
count = 0;

foreach(var line in lines)
{
    var direction = line[0];
    var ticks = int.Parse(line.Substring(1));
    if (direction == 'R') state = state + ticks;
    if (direction == 'L') state = state - ticks;

    while (state < 0 || state >= 100)
    {
        count++;
        if (state < 0) state += 100;
        if (state >= 100) state -= 100;
    }

    
}

Console.WriteLine($"part 2: {count}");