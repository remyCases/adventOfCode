#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dayOne.h"
#include "dayTwo.h"

int main(int argc, char**argv) {
    
    // expected argument DAY PART
    if (argc != 3) {
        return EXIT_FAILURE;
    }

    int chosen_day = 0u;
    int chosen_part = 0u;

    (void)sscanf(argv[1], "%i", &chosen_day);
    (void)sscanf(argv[2], "%i", &chosen_part);

    switch (chosen_day) {
    case 1:
        day_one_solution(chosen_part);
        break;
    case 2:
        day_two_solution(chosen_part);
        break;
    default:
        printf("Incorrect day was given.\n");
        break;
    }
}