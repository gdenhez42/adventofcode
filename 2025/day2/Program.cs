
long GetNbDigits(long nb)
{
    long nbDigits = 0;
    while(nb > 0) {
        nbDigits++;
        nb = nb/10;
    }
    return nbDigits;
}

long Pow(long b, long e)
{
    long result = 1;
    for(long i = 0; i < e; ++i)
    {
        result = result*b;
    }
    return result;
}

List<(long,long)> Factors(long nb)
{
    List<(long,long)> factors = [];

    if (nb == 1)
    {
        return factors;
    }

    factors.Add((1,nb));
    long factor = 2;
    while(nb > 0 && nb > factor)
    {
        if (nb % factor == 0) {
            nb = nb / factor;
            factors.Add((factor,nb));

            if (factor != nb) {
                factors.Add((nb, factor));
            }
        }
        factor++;
    }
    return factors;
}

long MakeInvalidID(long prefix, long nbRepeat)
{
    long id = prefix;
    long l = GetNbDigits(prefix);
    for (var i = 1; i < nbRepeat; i++)
    {
        id = id * Pow(10,l)  + prefix;
    }

    return id;
}


using StreamReader reader = new("input.txt");
string text = reader.ReadToEnd();

// Part 1
long count = 0;
foreach(var rangeStr in text.Split(','))
{
    string[] range = rangeStr.Split('-');

    long min = long.Parse(range[0]);
    long max = long.Parse(range[1]);

    
    long upperHalf = min / Pow(10, range[0].Length/2);
    // there cannot be invalid number with an odd number of digits
    if (range[0].Length % 2 == 1)
    {
        upperHalf = Pow(10, range[0].Length/2);
    }
    long nbDigitsUpperHalf = GetNbDigits(upperHalf);
    long nextInvalid = upperHalf * Pow(10, nbDigitsUpperHalf) + upperHalf;

    while(nextInvalid <= max)
    {
        if (nextInvalid >= min)
        {
            count += nextInvalid;
        }

        upperHalf++;
        nbDigitsUpperHalf = GetNbDigits(upperHalf);
        nextInvalid = upperHalf * Pow(10, nbDigitsUpperHalf) + upperHalf;
    }
}

Console.WriteLine($"Part1 : {count}");

// Part 2
count = 0;
foreach(var rangeStr in text.Split(','))
{
    string[] range = rangeStr.Split('-');

    long min = long.Parse(range[0]);
    long max = long.Parse(range[1]);

    long minDigits = GetNbDigits(min);
    long maxDigits = GetNbDigits(max);

    Console.WriteLine(rangeStr);
    HashSet<long> invalids = [];
    for(long nbDigits = minDigits; nbDigits <= maxDigits; nbDigits++)
    {
        foreach (var (prefixLen, nbRepeat) in Factors(nbDigits)) {

            var prefix = Pow(10, prefixLen-1);
            if (nbDigits == minDigits) {
                prefix = min / Pow(10, minDigits-prefixLen);
            }
            var maxPrefix = Pow(10, prefixLen);
            
            long nextInvalid = MakeInvalidID(prefix, nbRepeat);
            while(nextInvalid <= max && prefix < maxPrefix)
            {
                if (nextInvalid >= min) {
                invalids.Add(nextInvalid);
                }

                prefix++;
                nextInvalid = MakeInvalidID(prefix, nbRepeat);
            }

        }
        
    }
    Console.WriteLine($"  {String.Join(", ", invalids)}");
    foreach(var invalid in invalids){
        count += invalid;
    }
}

Console.WriteLine($"Part2 : {count}");