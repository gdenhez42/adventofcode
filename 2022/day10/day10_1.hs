

read_forces :: Int -> Int -> Int -> [String] -> Int
read_forces current_force current_cycle acc ("noop":commands) =
    let multiplier = if (current_cycle-20) `mod` 40 == 0 then current_cycle else 0
    in read_forces current_force (current_cycle + 1) (acc + multiplier*current_force) commands
read_forces current_force current_cycle acc (('a':'d':'d':'x':' ': x):commands) =
    let multiplier = case ((current_cycle-20) `mod` 40) of
                        39 -> current_cycle + 1
                        0  -> current_cycle
                        _  -> 0
        variation = read x :: Int
    in read_forces (current_force + variation) (current_cycle + 2) (acc + multiplier*current_force) commands
read_forces _ _ _ (_:_) = error "unexpected line!"
read_forces _current_force _current_cycle acc [] =
    acc

solve_part1 :: String -> Int
solve_part1 input =
    let commands = lines input
    in read_forces 1 1 0 commands

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solve_part1 input