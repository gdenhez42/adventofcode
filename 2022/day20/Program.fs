
open System.IO

let read_input () =
    File.ReadLines("input.txt") |> List.ofSeq |> List.map int64

let rec decrypt_input l decrypted input =
    match input with
    | [] -> decrypted
    | (i, elem) :: tail ->
        let index = Seq.findIndex (fun (ii, _) -> ii = i) decrypted |> int64
        let new_index =
            if elem < 0L then
                match (elem + index) % (l - 1L) with
                | 0L -> l - 1L
                | el_shifted when el_shifted < 0L -> el_shifted + l - 1L
                | el_shifted -> el_shifted
            else
                (elem + index) % (l - 1L)
        let new_decrypted = decrypted |> List.removeAt (int(index)) |> List.insertAt (int(new_index)) (i, elem)
        decrypt_input l new_decrypted tail

let rec decrypt_input_n_times times l decrypted input =
    match times with
    | 0 -> decrypted
    | _ ->
        let new_decrypted = decrypt_input l decrypted input
        decrypt_input_n_times (times - 1) l new_decrypted input

let part1 input =
    let decrypted_init = List.mapi (fun i e -> (int64(i), e)) input
    let l = List.length input |> int64
    let decrypted = decrypt_input l decrypted_init decrypted_init
    let index_zero = Seq.findIndex (fun (_, e) -> e = 0L) decrypted |> int64
    let i1 = (1000L + index_zero) % l |> int
    let i2 = (2000L + index_zero) % l |> int
    let i3 = (3000L + index_zero) % l |> int
    [List.item i1 decrypted; List.item i2 decrypted; List.item i3 decrypted] |> List.fold (fun acc (_, e) -> e + acc) 0L

let part2 input =
    let decrypted_init = List.mapi (fun i e -> (i, e*811589153L)) input
    let l = List.length input |> int64
    let decrypted = decrypt_input_n_times 10 l decrypted_init decrypted_init
    let index_zero = Seq.findIndex (fun (_, e) -> e = 0L) decrypted |> int64
    let i1 = (1000L + index_zero) % l |> int
    let i2 = (2000L + index_zero) % l |> int
    let i3 = (3000L + index_zero) % l |> int
    [List.item i1 decrypted; List.item i2 decrypted; List.item i3 decrypted] |> List.fold (fun acc (_, e) -> e + acc) 0L


// For more information see https://aka.ms/fsharp-console-apps
let input = read_input ()
input |> part1 |> printfn "%d"
input |> part2 |> printfn "%d"
