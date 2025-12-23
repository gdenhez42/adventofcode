class Program
{
    public static void Main()
    {
        Part1();
        Part2();
    }

    public static void Part1()
    {
        var connections = File.ReadLines("input.txt")
            .Select(line =>
            {
                var split = line.Split(':');
                return (split[0], split[1].Split(' ', StringSplitOptions.RemoveEmptyEntries));
            })
            .ToDictionary();
        
        List<(string, HashSet<string>)> paths = connections["you"].Select(e => {
            HashSet<string> p = new();
            p.Add("you");
            p.Add(e);
            return (e, p);
        }).ToList();
        int part1 = 0;
        while (paths.Count > 0)
        {
            List<(string, HashSet<string>)> newPaths = [];
            foreach ((string lastNode, HashSet<string> p) in paths)
            {
                if (lastNode == "out")
                {
                    part1++;
                }
                else
                {
                    foreach (string node in connections[lastNode])
                    {
                        if (!p.Contains(node))
                        {
                            HashSet<string> s = new(p);
                            s.Add(node);
                            
                            newPaths.Add((node,s));
                        }
                    }
                }
            }
            
            paths = newPaths;
        }
        Console.WriteLine($"Part1: {part1}");

    }

    public static void Part2()
    {
        var connections = File.ReadLines("input.txt")
            .Select(line =>
            {
                var split = line.Split(':');
                return (split[0], split[1].Split(' ', StringSplitOptions.RemoveEmptyEntries));
            })
            .ToDictionary();
        
        string startDevice = "svr";
        string dac = "dac";
        string fft = "fft";
        string endNode = "out";

        long part2 = GetPathsNoLoops(startDevice, fft, connections) *
            GetPathsNoLoops(fft, dac, connections) *
            GetPathsNoLoops(dac, endNode, connections)
            + GetPathsNoLoops(startDevice, dac, connections) *
            GetPathsNoLoops(dac, fft, connections) *
            GetPathsNoLoops(fft, endNode, connections);
        Console.WriteLine($"Part2: {part2}");

    }

    public static long GetPathsNoLoops(string startNode, string endNode, Dictionary<string, string[]> connections)
    {
        Dictionary<string, List<string>> reverseConnections = new();
        foreach ((string key, string[] cs) in connections)
        {
            if (HasPath(startNode, key, connections))
            {
                foreach (string c in cs)
                {
                    if (reverseConnections.TryGetValue(c, out List<string>? rev))
                    {
                        rev.Add(key);
                    }
                    else
                    {
                        reverseConnections[c] = [key];
                    }
                }
            }
        }


        Dictionary<string,long> nbPaths = new();
        nbPaths[startNode] = 1;
        bool progress = true;
        while (progress)
        {
            progress = false;
            foreach((string key, List<string> rev) in reverseConnections)
            {
                if (!nbPaths.ContainsKey(key))
                {
                    bool canCount = true;
                    long nb = 0;
                    foreach(string r in rev)
                    {
                        if (nbPaths.TryGetValue(r, out long n))
                        {
                            nb += n;
                        }
                        else
                        {
                            canCount = false;
                        }
                    }
                    if (canCount)
                    {
                        nbPaths[key] = nb;
                        progress = true; 
                    }
                }

            }
        }

        //foreach((string key, long nb) in nbPaths)
        //{
        //    Console.WriteLine($"   {key}: {nb}");
        //}
        return nbPaths.TryGetValue(endNode, out long result) ? result : 0;
    }


    public static bool HasPath(string startNode, string endNode, Dictionary<string, string[]> connections)
    {
        HashSet<string> visited = new();
        visited.Add(startNode);
        List<string> lastVisited = [startNode];
        while (lastVisited.Count > 0)
        {
            List<string> lastLastVisited = [];
            foreach (string v in lastVisited)
            {
                if (v == endNode)
                {
                    return true;
                }

                if (connections.TryGetValue(v, out string[]? visitedNeighbors))
                {
                    foreach(string n in visitedNeighbors)
                    {
                        if (!visited.Contains(n))
                        {
                            lastLastVisited.Add(n);
                            visited.Add(n);
                        }
                    }
                }
            }
            lastVisited = lastLastVisited;
        }
        return false;

    }

}