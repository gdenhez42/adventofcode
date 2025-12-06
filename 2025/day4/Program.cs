var lines = File.ReadLines("input.txt").ToList();

var grid = lines.Select(e1 => {
    return e1.ToCharArray().Select(e2 => e2 == '@' ? 1 : 0).ToArray();
}).ToArray();

int count = 0;
for (int i = 0; i < grid.Length; i++)
{
    for (int j = 0; j < grid[i].Length; j++)
    {
        if (grid[i][j] == 1)
        {
            int nbAdjacent = 0;
            if (i > 0)
            {
                if (j > 0) nbAdjacent += grid[i-1][j-1];
                nbAdjacent += grid[i-1][j];
                if (j < grid[i].Length-1) nbAdjacent += grid[i-1][j+1];
            }
            if (i < grid[i].Length-1)
            {
                if (j > 0) nbAdjacent += grid[i+1][j-1];
                nbAdjacent += grid[i+1][j];
                if (j < grid[i].Length-1) nbAdjacent += grid[i+1][j+1];
            }
            if (j > 0) nbAdjacent += grid[i][j-1];
            if (j < grid[i].Length-1) nbAdjacent += grid[i][j+1];

            if (nbAdjacent < 4) count++;
        }
    }
}
Console.WriteLine($"Part1: {count}");

bool canStillRemove = true;
count = 0;
while(canStillRemove)
{
    canStillRemove = false;
    for (int i = 0; i < grid.Length; i++)
    {
        for (int j = 0; j < grid[i].Length; j++)
        {
            if (grid[i][j] == 1)
            {
                int nbAdjacent = 0;
                if (i > 0)
                {
                    if (j > 0) nbAdjacent += grid[i-1][j-1];
                    nbAdjacent += grid[i-1][j];
                    if (j < grid[i].Length-1) nbAdjacent += grid[i-1][j+1];
                }
                if (i < grid[i].Length-1)
                {
                    if (j > 0) nbAdjacent += grid[i+1][j-1];
                    nbAdjacent += grid[i+1][j];
                    if (j < grid[i].Length-1) nbAdjacent += grid[i+1][j+1];
                }
                if (j > 0) nbAdjacent += grid[i][j-1];
                if (j < grid[i].Length-1) nbAdjacent += grid[i][j+1];

                if (nbAdjacent < 4)
                {
                    canStillRemove = true;
                    grid[i][j] = 0;
                    count++;
                }
            }
        }
    }
}

Console.WriteLine($"Part2: {count}");