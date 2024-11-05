// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "dayOne.h"

#define SIZE_MAX_ARRAY 3u
#define EXPECTED_ARGUMENTS 1

static void add_to_sorted_array(uint32_t p_array[SIZE_MAX_ARRAY], int size, uint32_t new_element) {

    for (int i=0; i < size; i++) {
        if (new_element < p_array[i]) {
            for(int j=0; j < i - 1; j++) { /* shift data */
                p_array[j] = p_array[j+1];
            }

            if (i > 0) p_array[i - 1] = new_element; /* add new element */
            return; /* quit */
        }
    }
    for(int j=0; j < size - 1; j++) {
        p_array[j] = p_array[j+1];
    }

    p_array[size - 1] = new_element;
    return;
}

int day_one_solution(int part) {

    FILE* fp = fopen("2022/data/input_day_one.txt", "r");
    if (!fp) /* validate file open for reading */
    { 
		return EXIT_FAILURE;
    }

    io_buffer_t io_buffer = { 0 };
    set_io_buffer(&io_buffer);

    uint32_t calories_count = 0u;
    uint32_t calories_array[SIZE_MAX_ARRAY] =  {0u, 0u, 0u};
    uint32_t calories_input = 0u;
    uint32_t calories_sum = 0u;

    while(io_buffer.iteration < MAX_ITERATION_ALLOWED) 
	{
		FGETS(fp); // read new line
        if (io_buffer.buf[0] == '\n') /* empty line */
		{
			add_to_sorted_array(calories_array, SIZE_MAX_ARRAY, calories_count);
            calories_count = 0;
        } 
		else 
		{
            PARSE_ELEMENT("%u", &calories_input)
            calories_count += calories_input;
        }
    }

    if (io_buffer.iteration == MAX_ITERATION_ALLOWED) 
	{
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }

    add_to_sorted_array(calories_array, SIZE_MAX_ARRAY, calories_count); /* last check if eof happened before a new empty line */
    
    switch(part) {
    case 1:
        printf("MAX CALORIES: %d\n", calories_array[SIZE_MAX_ARRAY-1]);
        break;
    case 2:
        for (unsigned int i = 0; i<SIZE_MAX_ARRAY; i++) 
		{
            calories_sum += calories_array[i];
        }
        printf("MAX CALORIES: %d\n", calories_sum);
        break;
    default:
        break;
    }
    
    if(fclose(fp)) {
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
