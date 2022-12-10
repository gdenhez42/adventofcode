#!/bin/bash
knotsX=(0 0 0 0 0 0 0 0 0 0)
knotsY=(0 0 0 0 0 0 0 0 0 0)
visited=()

input="input.txt"
while IFS= read -r line; do
    arr=($line)
    dir=${arr[0]}
    nb_steps=${arr[1]}
    while [ $nb_steps -gt 0 ]; do
        case $dir in
        "U")
        let knotsY[0]=${knotsY[0]}-1
        ;;
        "D")
        let knotsY[0]=${knotsY[0]}+1
        ;;
        "R")
        let knotsX[0]=${knotsX[0]}+1
        ;;
        "L")
        let knotsX[0]=${knotsX[0]}-1
        ;;
        esac
        i=1
        while [ $i -lt 10 ]; do
            prev=$(($i-1))
            let Hx=${knotsX[$prev]}
            let Hy=${knotsY[$prev]}
            let Tx=${knotsX[$i]}
            let Ty=${knotsY[$i]}

            if [ $Ty -gt $(($Hy+1)) ]; then
                let Ty=$Ty-1
                if [ $Tx -ne $Hx ]; then
                    if [ $Tx -gt $Hx ]; then
                        let Tx=$Tx-1
                    else
                        let Tx=$Tx+1
                    fi
                fi
            elif [ $Ty -lt $(($Hy-1)) ]; then
                let Ty=$Ty+1
                if [ $Tx -ne $Hx ]; then
                    if [ $Tx -gt $Hx ]; then
                        let Tx=$Tx-1
                    else
                        let Tx=$Tx+1
                    fi
                fi
            elif [ $Tx -lt $(($Hx-1)) ]; then
                let Tx=$Tx+1
                if [ $Ty -ne $Hy ]; then
                    if [ $Ty -gt $Hy ]; then
                        let Ty=$Ty-1
                    else
                        let Ty=$Ty+1
                    fi
                fi
            elif [ $Tx -gt $(($Hx+1)) ]; then
                let Tx=$Tx-1
                if [ $Ty -ne $Hy ]; then
                    if [ $Ty -gt $Hy ]; then
                        let Ty=$Ty-1
                    else
                        let Ty=$Ty+1
                    fi
                fi
            fi
            knotsX[$prev]=$Hx
            knotsY[$prev]=$Hy
            knotsX[$i]=$Tx
            knotsY[$i]=$Ty
            ((i++))
        done
        let nb_steps=$nb_steps-1
        coords="${knotsX[9]},${knotsY[9]}"
        visited+=( $coords )
    done
done < "$input"

visited_uniq=($(echo ${visited[@]} | tr ' ' '\n' | sort -u | tr '\n' ' '))

echo ${#visited_uniq[@]}
