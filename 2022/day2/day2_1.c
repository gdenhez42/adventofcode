#include <stdio.h>
#include <stdlib.h>

int main() {

    FILE* fptr = fopen("input.txt", "r");

    if(fptr == NULL)
    {
        printf("Error!");   
        exit(1);             
    }

    int totalPoints = 0;

    int c;
    while((c = fgetc(fptr)) != EOF) {
        int opponentMove = c;
        c = fgetc(fptr);
        int myMove = fgetc(fptr);
        c = fgetc(fptr);

        // Number of points for my move
        switch (myMove)
        {
            case 'X':
            totalPoints += 1;
            break;

            case 'Y':
            totalPoints += 2;
            break;
            
            case 'Z':
            totalPoints += 3;
            break;
        }

        // Number of point for outcome
        if (myMove == 'X' && opponentMove == 'A') {
            totalPoints += 3;
        }
        else if (myMove == 'Y' && opponentMove == 'B') {
            totalPoints += 3;
        }
        else if (myMove == 'Z' && opponentMove == 'C') {
            totalPoints += 3;
        }
        else if (myMove == 'X' && opponentMove == 'C') {
            totalPoints += 6;
        }
        else if (myMove == 'Y' && opponentMove == 'A') {
            totalPoints += 6;
        }
        else if (myMove == 'Z' && opponentMove == 'B') {
            totalPoints += 6;
        }
    }

    fclose(fptr);

    printf("Points: %d\n", totalPoints);
    return 0;
}