
record Tile(long X, long Y);

// The vertical wall of the perimeter of the green and red tiles
record VerticalWall(long X, long Y1, long Y2);

class Program
{   


    public static void Main()
    {
        var redTiles = File.ReadLines("input.txt")
        .Select(e => {
            var split = e.Split(',');
            return new Tile(long.Parse(split[0]), long.Parse(split[1]));
        })
        .ToArray();

        long max = 0;
        for (int i = 0; i < redTiles.Length; i++)
        {
            for (int j = i+1; j < redTiles.Length; j++)
            {
                Tile redTile1 = redTiles[i];
                Tile redTile2 = redTiles[j];
                long dx = redTile1.X > redTile2.X ? (redTile1.X - redTile2.X + 1) : (redTile2.X - redTile1.X + 1);
                long dy = redTile1.Y > redTile2.Y ? (redTile1.Y - redTile2.Y + 1) : (redTile2.Y - redTile1.Y + 1);
                
                long rectangleSize = dx*dy;
                if (rectangleSize > max) max = rectangleSize;
            }
        }

        Console.WriteLine($"Part1: {max}");

        // Check if tile are given in clockwise or counter clock wise order
        int leftmost = 0;
        for (int i = 0; i < redTiles.Length; i++)
        {
            if (redTiles[i].X < redTiles[leftmost].X) leftmost = i;
        }
        if (leftmost == 0 && redTiles[redTiles.Length-1].X == redTiles[0].X) leftmost = redTiles.Length - 1;

        bool clockWise = redTiles[leftmost].Y > redTiles[(leftmost+1)%redTiles.Length].Y;
        
        List<VerticalWall> verticalWalls = [];

        // Find vertical walls of red and green tiles
        for (int i = 0; i < redTiles.Length; i = i + 2)
        {
            Tile tPrev = redTiles[(i + leftmost - 1 + redTiles.Length) % redTiles.Length];
            Tile t1 = redTiles[(i + leftmost) % redTiles.Length];
            Tile t2 = redTiles[(i + leftmost + 1) % redTiles.Length];
            Tile tNext = redTiles[(i + leftmost + 2) % redTiles.Length];

            long y1 = t1.Y;
            long y2 = t2.Y;
            
            // I don't know how to simplify but this is supposed to handle the corners
            if (t1.Y > t2.Y)
            {
                if (tPrev.X < t1.X && clockWise)
                {
                    y1--;
                }
                if (tPrev.X > t1.X && !clockWise)
                {
                    y1--;
                }

                if (tNext.X < t2.X && clockWise)
                {
                    y2++;
                }
                if (tNext.X > t2.X && !clockWise)
                {
                    y2++;
                }
                verticalWalls.Add(new VerticalWall(t1.X, y2, y1));
            }

            if (t1.Y < t2.Y)
            {
                if (tPrev.X > t1.X && clockWise)
                {
                    y1++;
                }
                if (tPrev.X < t1.X && !clockWise)
                {
                    y1++;
                }

                if (tNext.X > t2.X && clockWise)
                {
                    y2--;
                }
                if (tNext.X < t2.X && !clockWise)
                {
                    y2--;
                }
                verticalWalls.Add(new VerticalWall(t1.X, y1, y2));
            }
        }
        verticalWalls.Sort(delegate(VerticalWall v1, VerticalWall v2)
        {
            return v1.X.CompareTo(v2.X);
        });

        max = 0;
        for (int i = 0; i < redTiles.Length; i++)
        {
            for (int j = i+1; j < redTiles.Length; j++)
            {
                Tile redTile1 = redTiles[i];
                Tile redTile2 = redTiles[j];
                long minX = redTile1.X > redTile2.X ? redTile2.X : redTile1.X;
                long maxX = redTile1.X > redTile2.X ? redTile1.X : redTile2.X;
                long minY = redTile1.Y > redTile2.Y ? redTile2.Y : redTile1.Y;
                long maxY = redTile1.Y > redTile2.Y ? redTile1.Y : redTile2.Y;
                
                //Console.WriteLine($"{(minX,minY,maxX,maxY)}:");
                long rectangleSize = (maxX-minX+1)*(maxY-minY+1);
                if (rectangleSize > max)
                {
                    bool hasOnlyRedAndGreen = true;
                    long nextY = minY;
                    while (nextY <= maxY && hasOnlyRedAndGreen)
                    {
                        long y = nextY;
                        nextY = maxY + 1;

                        bool isRedOrGreen = false;
                        long lastNonRedOrGreen = 0;
                        foreach(VerticalWall v in verticalWalls)
                        {
                            if (v.Y1 <= y && v.Y2 >= y)
                            {
                                if (v.Y2 + 1 < nextY) nextY = v.Y2 + 1;
                                
                                if (!isRedOrGreen &&
                                    lastNonRedOrGreen != v.X - 1 &&
                                    !(v.X - 1 < minX || lastNonRedOrGreen > maxX))
                                    {
                                        hasOnlyRedAndGreen = false;
                                        //Console.WriteLine($"  area of non green: {(y, minX,maxX,lastNonRedOrGreen, v.X, v.Y1, v.Y2)}");
                                    }
                                    
                                    
                                isRedOrGreen = !isRedOrGreen;
                                
                                if (!isRedOrGreen) lastNonRedOrGreen = v.X+1;

                            }
                            //Console.WriteLine($"  redOrGreen: {(y,v.Y1,v.Y2,isRedOrGreen)}");
                        }
                        if (lastNonRedOrGreen <= maxX && hasOnlyRedAndGreen) {
                            hasOnlyRedAndGreen = false;
                            //Console.WriteLine($"  area of non green: {(maxX,lastNonRedOrGreen)}");
                        }

                        //Console.WriteLine($"  {hasOnlyRedAndGreen}");

                    }



                    if (hasOnlyRedAndGreen) max = rectangleSize;

                }
            }
        }
        Console.WriteLine($"Part2: {max}");


    }
}


