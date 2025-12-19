

record Button(long Toggle);

record Machine(long DesiredState, List<Button> Buttons);

class Program
{
    public static void Part1()
    {
        var machines = File.ReadLines("input.txt")
            .Select(e =>
            {
                var splits = e.Split(' ');
                long desiredState = 0;
                var lights = splits[0].ToCharArray();
                for (int i = lights.Length-2; i > 0; i--)
                {
                    desiredState *= 2;
                    if (lights[i] == '#') desiredState += 1;
                    
                }

                List<Button> buttons = [];
                for (int i = 1; i < splits.Length - 1; i++)
                {
                    var toggle = splits[i]
                        .Substring(1, splits[i].Length - 2)
                        .Split(',')
                        .Select(e => int.Parse(e))
                        .Aggregate(0L, (acc, e) =>
                        {
                            return acc + (1L << e);
                        });
                    buttons.Add(new Button(toggle));

                }

                return new Machine(desiredState, buttons);

            })
            .ToList();

        long part1 = 0;

        foreach(Machine m in machines)
        {
            Dictionary<long,int> minimalPressesPerState = new() {
                [0L] = 0
            };
            
            List<long> visited = [0L];
            while (!minimalPressesPerState.ContainsKey(m.DesiredState))
            {
                List<long> nextVisited = [];

                foreach (Button b in m.Buttons)
                {
                    foreach (long v in visited)
                    {
                        int nbPress = minimalPressesPerState[v];
                        long newState = v ^ b.Toggle;
                        if (!minimalPressesPerState.ContainsKey(newState))
                        {
                            minimalPressesPerState[newState] = nbPress + 1;
                            nextVisited.Add(newState);
                        }
                    }
                }

                visited = nextVisited;
            }
            part1 += minimalPressesPerState[m.DesiredState];
            
        }
        Console.WriteLine($"Part1: {part1}");
    }

    public static void Part2()
    {
        var matrices = File.ReadLines("input.txt")
            .Select(e =>
            {
                var splits = e.Split(' ');

                List<int[]> buttons = [];
                for (int i = 1; i < splits.Length - 1; i++)
                {
                    var counters = splits[i]
                        .Substring(1, splits[i].Length - 2)
                        .Split(',')
                        .Select(e => int.Parse(e))
                        .ToArray();
                    buttons.Add(counters);
                }

                var desiredState = splits[splits.Length - 1]
                    .Substring(1, splits[splits.Length - 1].Length - 2)
                    .Split(',')
                    .Select(e => int.Parse(e))
                    .ToArray();

                long[,] matrix = new long[desiredState.Length, buttons.Count+1];

                for (int b = 0; b < buttons.Count; b++)
                {
                    for (int req = 0; req < buttons[b].Length; req++)
                    {
                        matrix[buttons[b][req],b] += 1;
                    }
                }

                for (int i = 0; i < desiredState.Length; i++)
                {
                    matrix[i,buttons.Count] = desiredState[i];
                }

                return matrix;

            })
            .ToList();


        long part2 = 0;
        foreach(var m in matrices)
        {
            long max = 0;
            for (int i = 0; i < m.GetLength(0); i++)
            {
                if (m[i, m.GetLength(1) - 1] > max) max = m[i, m.GetLength(1) - 1];
            }

            int rank = Simplify(m);
            long r = FewestButtonPress(m, rank, max);
            part2 += r;
        }

        Console.WriteLine($"Part2: {part2}");
    }

    // Takes a simplified matrix and bruteforce the missing parameters to find the fewest button presses
    public static long FewestButtonPress(long[,] matrix, long rank, long max)
    {
        int col = matrix.GetLength(1) - 2;
        long[] results = new long[rank];
        for (long i = 0; i < rank; i++)
        {
            results[i] = matrix[i, matrix.GetLength(1) - 1];
        }
        return FewestButtonPress(matrix, rank, max, col, results);

    }

    public static long FewestButtonPress(long[,] matrix, long rank, long max, long col, long[] results)
    {
        if (col < rank)
        {
            long sol = 0;
            for (long i = 0; i < rank; i++)
            {
                if (results[i] % matrix[i,i] != 0) return -1;
                results[i] /= matrix[i,i];
                if (results[i] < 0) return -1;
                sol += results[i];
            }
            return sol;
        }
        else
        {
            long sol = -1;
            for (long i = 0; i < max; i++)
            {
                long[] tempResults = new long[rank];
                for (long j = 0; j < rank; j++)
                {
                    tempResults[j] = results[j] - matrix[j, col]*i;
                }
                long tempSol = FewestButtonPress(matrix, rank, max, col-1, tempResults);
                if (tempSol != -1)
                {
                    if (sol == -1 || (tempSol + i) < sol) sol = tempSol + i;
                }
            }

            return sol;
        }

    }


    public static void DisplayMatrix(long[,] matrix)
    {
        for(int line = 0; line < matrix.GetLength(0); line++)
        {
            for(int col = 0; col < matrix.GetLength(1); col++)
            {
                Console.Write($"{matrix[line,col]},");
            }
            Console.WriteLine("");
        }
        Console.WriteLine("");
    }

    // Simplify the matrix with gaussian elimination. Returns the rank of matrix.
    public static int Simplify(long[,] matrix)
    {
        var rank = Math.Min(matrix.GetLength(0), matrix.GetLength(1)-1);

        // Makes an upper diagonal matrix
        for(int line = 0; line < rank; line++)
        {
            // Find a non zero element
            bool found = false;
            for (int j = line; j < matrix.GetLength(1) - 1 && !found; j++)
            {
                if (j != line)
                {
                    for (int i = 0; i < matrix.GetLength(0); i++)
                    {
                        long temp = matrix[i,j];
                        matrix[i,j] = matrix[i,line];
                        matrix[i,line] = temp;
                    }
                }

                for (int i = line; i < matrix.GetLength(0) && !found; i++)
                {
                    if (matrix[i,line] != 0)
                    {
                        found = true;
                        if (i != line)
                        {
                            for(int col = line; col < matrix.GetLength(1); col++)
                            {
                                long temp = matrix[i,col];
                                matrix[i,col] = matrix[line,col];
                                matrix[line,col] = temp;
                            }
                        }
                    }
                }
            }

            // Put other columns to 0
            if (!found)
            {
                rank = line;
            }
            else
            {
                long gcd = matrix[line,line];
                for(int col = line; col < matrix.GetLength(1); col++)
                {
                    if(matrix[line,col] != 0) gcd = Gcd(gcd, matrix[line,col]);
                }
                for(int col = line; col < matrix.GetLength(1); col++)
                {
                    matrix[line,col] /= gcd;
                }

                for (int i = line+1; i < matrix.GetLength(0); i++)
                {

                    long coef = matrix[i, line];
                    for(int col = line; col < matrix.GetLength(1); col++)
                    {
                        matrix[i, col] *= matrix[line,line];
                        matrix[i, col] -= coef*matrix[line,col];

                    }
                }

            }

        }

        // Makes an almost diagonal matrix
        for(int line = rank-1; line > 0; line--)
        {
            long gcd = matrix[line,line];
            for(int col = line; col < matrix.GetLength(1); col++)
            {
                if(matrix[line,col] != 0) gcd = Gcd(gcd, matrix[line,col]);
            }
            for(int col = line; col < matrix.GetLength(1); col++)
            {
                matrix[line,col] /= gcd;
            }
            if (matrix[line,line] < 0)
            {
                for(int col = line; col < matrix.GetLength(1); col++)
                {
                    matrix[line,col] *= -1;
                }
            }

            for (int i = line-1; i >= 0; i--)
            {
                long coef = matrix[i, line];
                for(int col = 0; col < matrix.GetLength(1); col++)
                {
                    matrix[i, col] *= matrix[line,line];
                    matrix[i, col] -= coef*matrix[line,col];

                }
            }
        }

        return rank;

    }

    public static long Gcd(long a, long b)
    {
        return b == 0 ? a : Gcd(b,a%b);
    }

    public static void Main()
    {
        Part1();
        Part2();
    }

}


