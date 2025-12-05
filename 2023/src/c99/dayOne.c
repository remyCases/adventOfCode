// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "dayOne.h"

#define NB_ELEMENTS 18
char* VALID_STR[NB_ELEMENTS] = {"1", "2", "3", "4", "5", "6", "7", "8", "9",
	"one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };

int VALID_INT[NB_ELEMENTS] = { 1, 2, 3, 4, 5, 6, 7, 8, 9,
	1, 2, 3, 4, 5, 6, 7, 8, 9 };

int is_valid_string(char* input, int low, int high)
{
	for (int i = 0; i < NB_ELEMENTS; i++)
	{
		char* str = VALID_STR[i];
		int j = low;
		if ((int)strlen(str) != high - low + 1) continue; // size should be the same
		while(*str)
		{
			if (*str != input[j]) goto next_str;
			str++;
			j++;
		}
		// at this point same size and size chars
		// then input[low .. high] == str
		// return the equivalent int value
		return VALID_INT[i];

		next_str:;
	}
	return -1; // cant parse
}

int parseTwoDigits(char* input)
{
	int res = 0;
	char first_digit = 1;
	char stored_char = 0;
	while(*input)
	{
		if(*input <= '9' && *input >= '0')
		{
			stored_char = *input;
			if (first_digit)
			{
				res += (*input - '0')*10;
				first_digit = 0;
			}
		}
		input++;
	}
	res += (stored_char - '0');

	return res;
}

int parseTwoDigitsWithString(char* input)
{
	int len = strlen(input);
	int low = 0;
	int high = low;
	int first_digit = 0;
	int second_digit = 0;
	
	// finding first digit
	while(low < len)
	{
		first_digit = is_valid_string(input, low, high);
		if (first_digit != -1)
		{
			break;
		}
		high += 1;
		if (high == len || high - low > 5)
		{
			low += 1;
			high = low;
		}
	}
	
	// finding the second one
	high = len - 1;
	low = high;
	while(high >= 0)
	{
		second_digit = is_valid_string(input, low, high);
		if (second_digit != -1)
		{
			break;
		}
		low -= 1;
		if (high == len || high - low > 5)
		{
			high -= 1;
			low = high;
		}
	}

	return first_digit*10 + second_digit;
}

int day_one_solution(int part) 
{
    FILE* fp = fopen("2023/data/input_day_one", "r");
    if (!fp) /* validate file open for reading */
	{
        return EXIT_FAILURE;
    }
	
    io_buffer_t io_buffer = { 0 };
    set_io_buffer(&io_buffer);

	int calib = 0;

    while(io_buffer.iteration < MAX_ITERATION_ALLOWED) 
	{
        io_buffer.out_fgets = fgets(io_buffer.buf, BUFFER_SIZE, fp); /* read each line in file */
        if (!io_buffer.out_fgets) /* reach end of file */
		{
			break;
        }
		
		switch(part)
		{
		case 1:
			calib += parseTwoDigits(io_buffer.buf);
			break;
		case 2:
			calib += parseTwoDigitsWithString(io_buffer.buf);
			break;
		default:
			printf("Invalid part\n");
			return EXIT_FAILURE;
			break;
		}
		
        io_buffer.iteration++;
    }

    if (io_buffer.iteration == MAX_ITERATION_ALLOWED) 
	{
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }

    printf("CALIBRATION VALUE: %d\n", calib);
    
    if(fclose(fp)) 
	{
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
