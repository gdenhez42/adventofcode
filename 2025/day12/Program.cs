using System.Text;

record GiftPosition(int X, int Y, int O);

class Gift
{
    public const int Size = 3;

    bool[,] _gift;

    public Gift(string block)
    {
        _gift = new bool[Size, Size];
        var lines = block.Split("\r\n").Skip(1).ToArray();
        for (int i = 0; i < Size; i++)
        {
            var chars = lines[i].ToCharArray();
            for (int j = 0; j < Size; j++)
            {
                _gift[i,j] = chars[j] == '#';
            }
        }
    }

    public bool this[int x, int y]
    {
        get
        {
            return _gift[x,y];
        }
    }
}

class Grid
{
    private bool[,] _grid;


    public Grid(int l, int h)
    {
        _grid = new bool[h, l];
    }

    public int L => _grid.GetLength(1);
    public int H => _grid.GetLength(0);

    public override string ToString()
    {
        StringBuilder sb = new();
        for (int i = 0; i < _grid.GetLength(0); i++)
        {
            for (int j = 0; j < _grid.GetLength(1); j++)
            {
                sb.Append(_grid[i,j] ? "#" : ".");
            }
            sb.Append('\n');
        }
        return sb.ToString();
    }

    public Grid PlaceGift(Gift gift, GiftPosition pos)
    {
        Grid newGrid = new(_grid.GetLength(1), _grid.GetLength(0));
        Array.Copy(this._grid, newGrid._grid, this._grid.Length);

        for (int i = 0; i <  Gift.Size; i++)
        {
            for (int j = 0; j < Gift.Size; j++)
            {
                bool val = pos.O switch
                {
                    0 => gift[i,j],
                    1 => gift[Gift.Size-1-j, i],
                    2 => gift[Gift.Size-1-i, Gift.Size-1-j],
                    3 => gift[j,Gift.Size-1-i],
                    _ => gift[i,j]
                };


                newGrid._grid[i+pos.Y,j+pos.X] |= val;
            }
        }
        return newGrid;
    }

    public bool CanPlaceGift(Gift gift, GiftPosition pos)
    {
        bool canPlace = true;


        for (int i = 0; i <  Gift.Size && canPlace; i++)
        {
            for (int j = 0; j < Gift.Size && canPlace; j++)
            {
                bool val = pos.O switch
                {
                    0 => gift[i,j],
                    1 => gift[Gift.Size-1-j, i],
                    2 => gift[Gift.Size-1-i, Gift.Size-1-j],
                    3 => gift[j,Gift.Size-1-i],
                    _ => gift[i,j]
                };


                canPlace = !(_grid[i+pos.Y,j+pos.X] && val);
            }
        }
        return canPlace;
    }

}


class Program
{
    public static void Main()
    {
        Part1();
    }

    public static void Part1()
    {
        using StreamReader reader = new("input.txt");
        string text = reader.ReadToEnd();
        string[] blocks = text.Split("\r\n\r\n");
        Gift[] gifts = blocks.Take(blocks.Length - 1).Select(e =>
            new Gift(e)
        ).ToArray();

        int part1 = 0;
        foreach(string region in blocks[blocks.Length - 1].Split("\r\n"))
        {
            string[] split1 = region.Split(':');
            int[] dims = split1[0].Split('x').Select(e => int.Parse(e)).ToArray();
            int[] nbGifts = split1[1].Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(e => int.Parse(e)).ToArray();
            
            Grid grid = new Grid(dims[0], dims[1]);
            if(CanPlaceAllGifts(grid, nbGifts, gifts))
            {
                part1++;
            }
        
        }
        Console.WriteLine($"Part1: {part1}");
    }

    public static bool CanPlaceAllGifts(Grid grid, int[] nbGifts, Gift[] gifts)
    {
        int sum = 0;
        foreach(int nb in nbGifts)
        {
            sum += nb;
        }
        int nbFit = (grid.L/Gift.Size) * (grid.H/Gift.Size);

        if (sum <= nbFit)
        {
            return true;
        }

        sum = 0;
        for(int gindex = 0; gindex < nbGifts.Length; gindex++)
        {
            for (int i = 0; i < Gift.Size; i++)
            {
                for (int j = 0; j < Gift.Size; j++)
                {
                    if (gifts[gindex][i,j]) sum += nbGifts[gindex];
                }
            }
        }

        if (sum > grid.L * grid.H)
        {
            return false;
        }

        Console.WriteLine("On sait pas");
        return false;
    }




}
