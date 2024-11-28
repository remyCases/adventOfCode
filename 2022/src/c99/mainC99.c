// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dayOne.h"
#include "dayTwo.h"
#include "dayThree.h"
#include "dayFour.h"

int main(int argc, char**argv) 
{    
    // expected argument DAY PART
	if (argc != 3) 
	{
        return EXIT_FAILURE;
    }

    int chosen_day = 0u;
    int chosen_part = 0u;

    (void)sscanf(argv[1], "%i", &chosen_day);
    (void)sscanf(argv[2], "%i", &chosen_part);

    switch (chosen_day) 
	{
    case 1:
        day_one_solution(chosen_part);
        break;
    case 2:
        day_two_solution(chosen_part);
        break;
    case 3:
        day_three_solution(chosen_part);
        break;
    case 4:
        day_four_solution(chosen_part);
        break;
    default:
        printf("Incorrect combination of day and part. Day %d and part %d does not exist (yet).\n", chosen_day, chosen_part);
        break;
    }
}
