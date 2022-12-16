open(my $in,  "<",  "input.txt")  or die "Can't open input.txt: $!";

my @lines = <$in>;

my @sensors = ();

foreach my $line (@lines) {
    my @parts = split(" ", $line);
    my $sensorX = int(substr(@parts[2], 2, -1));
    my $sensorY = int(substr(@parts[3], 2, -1));

    my $beaconX = int(substr(@parts[8], 2, -1));
    my $beaconY = int(substr(@parts[9], 2));

    my $dist = abs($sensorX-$beaconX) + abs($sensorY-$beaconY);

    push(@sensors, {
        sx => $sensorX,
        sy => $sensorY,
        bx => $beaconX,
        by => $beaconY,
        d => $dist,
    });
}

# part1
my $y = 2000000;
my %positions = ();
foreach (@sensors) {
    my %sensor = %$_;
    my $dist = abs($sensor{"sy"}-$y);
    if ($dist <= $sensor{"d"}) {
        my $dx = $sensor{"d"} - $dist;
        for ($i = 0; $i <= $dx; $i++) {
            $positions{$sensor{"sx"} + $i} = 1;
            $positions{$sensor{"sx"} - $i} = 1;
        }
    }

    if ($sensor{"by"} == $y) {
        delete($positions{$sensor{"bx"}});
    }
}

@keys = keys %positions;
$size = @keys;
print $size, "\n";

# part2
my $max = 4000000;
my @map = (0) * $max;
print $map[1];