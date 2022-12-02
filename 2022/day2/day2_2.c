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
        int outcome = fgetc(fptr);
        c = fgetc(fptr);

        // Number of points outcome
        switch (outcome)
        {
            case 'X':
            totalPoints += 0;
            break;

            case 'Y':
            totalPoints += 3;
            break;
            
            case 'Z':
            totalPoints += 6;
            break;
        }

        // Number of point for my move

        // If need to play rock
        if ((outcome == 'X' && opponentMove == 'B') ||
            (outcome == 'Y' && opponentMove == 'A') ||
            (outcome == 'Z' && opponentMove == 'C'))
        {
            totalPoints += 1;
        }

        // If need to play paper
        if ((outcome == 'X' && opponentMove == 'C') ||
            (outcome == 'Y' && opponentMove == 'B') ||
            (outcome == 'Z' && opponentMove == 'A'))
        {
            totalPoints += 2;
        }

        // If need to play scisor
        if ((outcome == 'X' && opponentMove == 'A') ||
            (outcome == 'Y' && opponentMove == 'C') ||
            (outcome == 'Z' && opponentMove == 'B'))
        {
            totalPoints += 3;
        }
    }

    fclose(fptr);

    printf("Points: %d\n", totalPoints);
    return 0;
}