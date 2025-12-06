var lines = File.ReadLines("input.txt");

var ranges = lines
    .TakeWhile(e => e != "")
    .Select(e => {
        string[] r = e.Split('-');
        return (long.Parse(r[0]), long.Parse(r[1]));
    })
    .ToList();

var availables = lines
    .SkipWhile(e => e != "")
    .Skip(1)
    .Select(e => {
        return long.Parse(e);
    })
    .ToList();

int count = 0;
foreach (long a in availables)
{
    foreach ((long min, long max) in ranges)
    {
        if(a >= min && a <= max)
        {
            count++;
            break;
        }
    }
}
Console.WriteLine($"Part1 : {count}");

long nbFresh = 0;

// Sort by size of range in descending order so the loop below works
ranges.Sort(delegate((long,long) a, (long,long) b)
{
    (long a1,long a2) = a;
    (long b1,long b2) = b;
    return (b2-b1+1).CompareTo((a2-a1+1));
});

for (int i = 0; i < ranges.Count; i++)
{
    (long min, long max) = ranges[i];

    for (int j = 0; j < i; j++)
    {
        (long rMin, long rMax) = ranges[j];

        // If a previous range overlap the current one, shrink the current range
        // The current range will not need to be split because any previous range is larger thanks to the sort above
        if (min >= rMin && min <= rMax) min = rMax+1;
        if (max >= rMin && max <= rMax) max = rMin-1;

        if (min > max) break;
    }

    if (min <= max) nbFresh += max - min + 1;
}

Console.WriteLine($"Part2 : {nbFresh}");