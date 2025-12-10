
record Box(long X, long Y, long Z);

record Connection(Box B1, Box B2, double Distance);

record Circuit(HashSet<Box> Boxes);

class Program
{
    static double Distance(Box b1, Box b2)
    {
        return Math.Sqrt(Math.Pow(b1.X - b2.X, 2) + Math.Pow(b1.Y - b2.Y, 2) + Math.Pow(b1.Z - b2.Z, 2));
    }

    static void Main()
    {

        var boxes = File.ReadLines("input.txt")
            .Select(e => {
                string[] split = e.Split(',');
                return new Box(long.Parse(split[0]), long.Parse(split[1]), long.Parse(split[2]));  
            })
            .ToArray();
        int nbConnectionsPart1 = 1000;

        List<Connection> potentialConnections = [];
        for (int i = 0; i < boxes.Length; i++)
        {
            for (int j = i+1; j < boxes.Length; j++)
            {
                potentialConnections.Add(new Connection(boxes[i], boxes[j], Distance(boxes[i], boxes[j])));
            }
        }
        potentialConnections.Sort(delegate(Connection c1, Connection c2){
            return c1.Distance.CompareTo(c2.Distance);
        });

        List<Circuit> circuits = boxes.Select(e => new Circuit(new HashSet<Box>() {e})).ToList();
        
        for (int i = 0; i < nbConnectionsPart1; i++)
        {
            Connection c = potentialConnections[i];
            int indexBox1 = 0;
            int indexBox2 = 0;
            for(int j = 0; j < circuits.Count; j++)
            {
                if (circuits[j].Boxes.Contains(c.B1)) indexBox1 = j;
                if (circuits[j].Boxes.Contains(c.B2)) indexBox2 = j;
            }
            if (indexBox1 != indexBox2) {
                Circuit removed = circuits[indexBox2];
                circuits[indexBox1].Boxes.UnionWith(removed.Boxes);
                circuits.RemoveAt(indexBox2);
            }
        }

        List<long> circuitLength = circuits.Select(e => (long)e.Boxes.Count).ToList();
        circuitLength.Sort(delegate(long i1, long i2)
        {
            return i2.CompareTo(i1);
        });

        Console.WriteLine($"Part1: {circuitLength[0]*circuitLength[1]*circuitLength[2]}");

        int indexLastConnection = nbConnectionsPart1-1;
        while(circuits.Count > 1)
        {
            indexLastConnection++;
            Connection c = potentialConnections[indexLastConnection];
            int indexBox1 = 0;
            int indexBox2 = 0;
            for(int j = 0; j < circuits.Count; j++)
            {
                if (circuits[j].Boxes.Contains(c.B1)) indexBox1 = j;
                if (circuits[j].Boxes.Contains(c.B2)) indexBox2 = j;
            }
            if (indexBox1 != indexBox2) {
                Circuit removed = circuits[indexBox2];
                circuits[indexBox1].Boxes.UnionWith(removed.Boxes);
                circuits.RemoveAt(indexBox2);
            }
        }
        Connection lastConnection = potentialConnections[indexLastConnection];
        Console.WriteLine($"Part2: {lastConnection.B1.X * lastConnection.B2.X}");
    }
}