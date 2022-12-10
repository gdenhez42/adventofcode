pixel_value :: Int -> Int -> Char
pixel_value current_position current_cycle =
    if current_position >= (crt_pos - 1) && current_position <= (crt_pos + 1) then '#' else '.'
    where crt_pos = (current_cycle - 1) `mod` 40

update_board :: Int -> Int -> String -> String
update_board current_position current_cycle acc =
    case (current_cycle `mod` 40) of
        0 -> ('\n': pixel_value current_position current_cycle: acc)
        _ -> (pixel_value current_position current_cycle: acc)

draw_board :: Int -> Int -> String -> [String] -> String
draw_board current_position current_cycle acc ("noop":commands) =
    draw_board current_position (current_cycle + 1) (update_board current_position current_cycle acc) commands

draw_board current_position current_cycle acc (('a':'d':'d':'x':' ': x):commands) =
    let new_acc   = update_board current_position (current_cycle + 1) $ update_board current_position current_cycle $ acc
        variation = read x :: Int
    in draw_board (current_position + variation) (current_cycle + 2) new_acc commands

draw_board _ _ _ (_:_) = error "unexpected line!"

draw_board _current_position _current_cycle acc [] = acc

solve_part2 :: String -> String
solve_part2 input =
    let commands = lines input
    in reverse $ draw_board 1 1 [] commands

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ solve_part2 input