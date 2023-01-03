
function Snafu-To-Decimal {
    param (
        $snafu
    )

    $result = 0
    foreach ($letter in $snafu.ToCharArray()) {
        $result = $result * 5
        Switch ($letter)
        {
            '0' { $result += 0 }
            '1' { $result += 1 }
            '2' { $result += 2 }
            '-' { $result -= 1 }
            '=' { $result -= 2 }
        }
    }
    $result
}

function Decimal-To-Snafu {
    param (
        $decimal
    )
    $result = ""
    while ($decimal -gt 0) {
        $digit = $decimal % 5
        $decimal = ($decimal - $digit) / 5
        if ($digit -eq 4) {
            $result = "-" + $result
            $decimal += 1
        } elseif ($digit -eq 3) {
            $result = "=" + $result
            $decimal += 1
        } else {
            $result = $digit.ToString() + $result
        }
    }
    $result
}


$rez_decimal = 0
foreach($line in Get-Content .\input.txt) {
    $dec = Snafu-To-Decimal $line
    $rez_decimal += $dec
}

$part1 = Decimal-To-Snafu $rez_decimal
write-host $part1