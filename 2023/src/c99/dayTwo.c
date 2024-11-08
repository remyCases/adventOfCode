// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "dayTwo.h"

#define MAX_RED 12
#define MAX_GREEN 13
#define MAX_BLUE 14

int day_two_solution(int part) 
{
    FILE* fp = fopen("2023/data/input_day_two", "r");
    if (!fp) /* validate file open for reading */
	{
        return EXIT_FAILURE;
    }

    io_buffer_t io_buffer = { 0 };
    set_io_buffer(&io_buffer);

	int id = 0;
	int sum_id = 0;
	int green = 0;
	int red = 0;
	int blue = 0;

    while(io_buffer.iteration < MAX_ITERATION_ALLOWED) 
	{
        FGETS(fp); // read new line
		char* b = io_buffer.ptr;
		int m_green = 0;
		int m_red = 0;
		int m_blue = 0;

		while(*b)
		{
			if (*b == ':')
			{
				*b = 0;
#define EXPECTED_ARGUMENTS 1
				PARSE_ELEMENT("Game %d", &id);
#undef EXPECTED_ARGUMENTS
				*b = ':';
				io_buffer.ptr = b + 1;
			}
			else if (*b == ',' || *b == ';' || *b == '\n')
			{
				char svd = *b;
				*b = 0;
#define EXPECTED_ARGUMENTS 1
				switch(*(b-1))
				{
				case 'n':
					PARSE_ELEMENT(" %d green", &green);
					if (m_green < green) m_green = green;
					break;	
				case 'd':
					PARSE_ELEMENT(" %d red", &red);
					if (m_red < red) m_red = red;
					break;
				case 'e':
					PARSE_ELEMENT(" %d blue", &blue);
					if (m_blue < blue) m_blue = blue;
					break;
				default:
					printf("wtf");
				}
#undef EXPECTED_ARGUMENTS
				*b = svd;
				io_buffer.ptr = b + 1;
			}
			b++;			
		}

		switch(part)
		{
		case 1:
    		if (m_red <= MAX_RED && m_blue <= MAX_BLUE && m_green <= MAX_GREEN)
			{
            	sum_id += id;
			}
			break;
		case 2:
			sum_id += m_red * m_blue * m_green;
			break;
		default:
			printf("Invalid part\n");
			return EXIT_FAILURE;
			break;
		}

    }

    if (io_buffer.iteration == MAX_ITERATION_ALLOWED)
	{
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }
    
    printf("POWER OF GAMES: %u\n", sum_id);
    
    if(fclose(fp)) 
	{
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
