// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

#include "dayOne.h"

#define MAX_DIAL 100

int parseRotation(char* input, int* increment, char* orientation, int* cycles)
{
	*orientation = *input;
	if (*orientation != 'L' && *orientation != 'R') return -1;
	
	errno = 0;
	char* end = 0;
	const long i = strtol(input+1, &end, 10);
	if (errno == ERANGE) return -1;
	
	*increment = i % MAX_DIAL;
	*cycles = i / MAX_DIAL;
	return 0;
}

int day_one_solution(int part) 
{
    FILE* fp = fopen("2025/data/input_day_one", "r");
    if (!fp) /* validate file open for reading */
	{
        return EXIT_FAILURE;
    }
	
    io_buffer_t io_buffer = { 0 };
    set_io_buffer(&io_buffer);

	int dial = 50;
	int increment = 0;
	int cycles = 0;
	char orientation = 0;
	int dial_at_zero = 0;

    while(io_buffer.iteration < MAX_ITERATION_ALLOWED) 
	{
        io_buffer.out_fgets = fgets(io_buffer.buf, BUFFER_SIZE, fp); /* read each line in file */
        if (!io_buffer.out_fgets) /* reach end of file */
		{
			break;
        }
		
		if (parseRotation(io_buffer.buf, &increment, &orientation, &cycles))
		{
			printf("Error parsing %s\n", io_buffer.buf);
			return EXIT_FAILURE;
		}
		switch(part)
		{
		case 1:
			if (orientation == 'L')
			{
				dial = (dial + MAX_DIAL - increment) % MAX_DIAL;
			}
			else if (orientation == 'R')
			{
				dial = (dial + increment) % MAX_DIAL;
			}
			if (dial == 0) dial_at_zero++;
			break;
		case 2:
			dial_at_zero += cycles;
			if (!increment) continue;
			
			if (orientation == 'L')
			{
				if (dial <= increment && dial) dial_at_zero++;
				dial = (dial + MAX_DIAL - increment) % MAX_DIAL;
			}
			else if (orientation == 'R')
			{
				if (dial + increment >= MAX_DIAL) dial_at_zero++;
				dial = (dial + increment) % MAX_DIAL;
			}
			break;
		default:
			printf("Invalid part\n");
			return EXIT_FAILURE;
		}
		
        io_buffer.iteration++;
    }

    if (io_buffer.iteration == MAX_ITERATION_ALLOWED) 
	{
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }

    printf("DIAL AT ZERO: %d\n", dial_at_zero);
    
    if(fclose(fp)) 
	{
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
