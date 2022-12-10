#!/bin/bash
Hx=0
Hy=0
Tx=0
Ty=0
visited=()

input="input.txt"
while IFS= read -r line; do
    arr=($line)
    dir=${arr[0]}
    nb_steps=${arr[1]}
    while [ $nb_steps -gt 0 ]; do
        case $dir in
        "U")
            let Hy=$Hy-1
            if [ $Ty -gt $(($Hy+1)) ]; then
                let Ty=$Ty-1
                if [ $Tx -ne $Hx ]; then
                    let Tx=$Hx
                fi
            fi
            ;;
        "D")
            let Hy=$Hy+1
            if [ $Ty -lt $(($Hy-1)) ]; then
                let Ty=$Ty+1
                if [ $Tx -ne $Hx ]; then
                    let Tx=$Hx
                fi
            fi
            ;;
        "R")
            let Hx=$Hx+1
            if [ $Tx -lt $(($Hx-1)) ]; then
                let Tx=$Tx+1
                if [ $Ty -ne $Hy ]; then
                    let Ty=$Hy
                fi
            fi
        ;;
        "L")
            let Hx=$Hx-1
            if [ $Tx -gt $(($Hx+1)) ]; then
                let Tx=$Tx-1
                if [ $Ty -ne $Hy ]; then
                    let Ty=$Hy
                fi
            fi
        ;;
        esac
        let nb_steps=$nb_steps-1
        coords="$Tx,$Ty"
        visited+=( $coords )
    done
done < "$input"

visited_uniq=($(echo ${visited[@]} | tr ' ' '\n' | sort -u | tr '\n' ' '))

echo ${#visited_uniq[@]}
