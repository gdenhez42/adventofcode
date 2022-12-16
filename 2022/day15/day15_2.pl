use List::Util qw(max);
use List::Util qw(min);

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

my $max = 4000000;
for ($y = 0; $y <= $max; $y++) {
    my $x = 0;
    while($x < $max){
        my $found = 0;
        foreach (@sensors) {
            my %sensor = %$_;
            my $dist = abs($sensor{"sy"}-$y);
            if ($dist <= $sensor{"d"}) {
                my $dx = $sensor{"d"} - $dist;
                if ($x >= $sensor{"sx"} - $dx && $x <= $sensor{"sx"} + $dx) {
                    $x = $sensor{"sx"} + $dx;
                    $found = 1;
                    last;
                }
            }
        }
        unless ($found) {
            print $x*$max + $y, "\n";
        }
        $x += 1;
    }
}