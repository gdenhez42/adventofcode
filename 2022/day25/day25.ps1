
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

foreach($line in Get-Content .\input_test.txt) {
    $dec = Snafu-To-Decimal $line
    write-host $dec
}