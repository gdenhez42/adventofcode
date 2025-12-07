var lines = File.ReadLines("input.txt").ToArray();

var manifold = lines.Select(e => e.ToCharArray()).ToArray();

HashSet<int> beams = [];
for (int i = 0; i < manifold[0].Length; i++)
{
    if (manifold[0][i] == 'S')
    {
        beams.Add(i);
    }
}

int nbSplits = 0;
for (int i = 1; i < manifold.Length; i++)
{
    HashSet<int> newBeams = [];
    foreach (int beam in beams)
    {
        if (manifold[i][beam] == '.') newBeams.Add(beam);
        if (manifold[i][beam] == '^')
        {
            newBeams.Add(beam - 1);
            newBeams.Add(beam + 1);
            nbSplits++;
        }
    }
    beams = newBeams;
}
Console.WriteLine($"Part1: {nbSplits}");


List<(int,long)> beamWithTimelines = [];
for (int i = 0; i < manifold[0].Length; i++)
{
    if (manifold[0][i] == 'S')
    {
        beamWithTimelines.Add((i,1));
    }
}

// Note: the dummy recursive version took too long
for (int i = 1; i < manifold.Length; i++)
{
    List<(int,long)> newBeams = [];
    for (int bi = 0; bi < beamWithTimelines.Count; bi++)
    {
        (int beam, long nbTimelines) = beamWithTimelines[bi];

        if (manifold[i][beam] == '.')
        {
            if (newBeams.Count > 0 && newBeams.Last() is (int l, long t) && l == beam)
            {
                newBeams[newBeams.Count - 1] = (l, nbTimelines + t);
            }
            else
            {
                newBeams.Add((beam, nbTimelines));
            }
        } 
        if (manifold[i][beam] == '^')
        {
            if (newBeams.Count > 0 && newBeams[newBeams.Count - 1] is (int l1, long t1) && l1 == beam - 1)
            {
                newBeams[newBeams.Count - 1] = (l1, nbTimelines + t1);
            }
            else if (newBeams.Count > 1 && newBeams[newBeams.Count - 2] is (int l2, long t2) && l2 == beam - 1)
            {
                newBeams[newBeams.Count - 2] = (l2, nbTimelines + t2);
            }
            else
            {
                newBeams.Add((beam-1, nbTimelines));
            }
            newBeams.Add((beam + 1, nbTimelines));
        }
    }
    beamWithTimelines = newBeams;
}



Console.WriteLine($"Part2: {beamWithTimelines.Aggregate(0L, (acc, beamWithTimeline) => {
    (int beam, long nbTimelines) = beamWithTimeline;
    return acc + nbTimelines;
})}");