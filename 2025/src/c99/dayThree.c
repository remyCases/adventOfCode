// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

#include "dayTwo.h"

#define INITIAL_SIZE 2
#define NBATTERIES 12

typedef struct vec_uint64_s {
	uint64_t* buf;
	size_t len;
	size_t capacity;
} vec_uint64_t;

static void init_vec_uint64(vec_uint64_t* p_vec_uint64)
{
	p_vec_uint64->buf = (uint64_t*)calloc(INITIAL_SIZE, sizeof(uint64_t));
	p_vec_uint64->len = 0;
	p_vec_uint64->capacity = INITIAL_SIZE;
}

static void push_vec_uint64(vec_uint64_t* p_vec_uint64, uint64_t u)
{
	p_vec_uint64->buf[p_vec_uint64->len] = u;
	p_vec_uint64->len++;
	if (p_vec_uint64->len == p_vec_uint64->capacity)
	{
		p_vec_uint64->capacity *= 2;
		p_vec_uint64->buf = (uint64_t*)realloc(p_vec_uint64->buf, p_vec_uint64->capacity * sizeof(uint64_t));
	}
}

uint64_t compute_joltage(vec_uint64_t* p_batteries)
{
	uint64_t joltage[2] = {0, 0};
	uint64_t indeces[2] = {0, 0};

	for (size_t i = indeces[0]; i < (p_batteries->len-1); i++)
	{
        if (p_batteries->buf[i] > joltage[0])
		{
            joltage[0] = p_batteries->buf[i];
            indeces[1] = i+1;
		}
	}

    for (size_t i = indeces[1]; i < p_batteries->len; i++)
	{
        if (p_batteries->buf[i] > joltage[1])
		{
            joltage[1] = p_batteries->buf[i];
		}
	}

    return (joltage[0] * 10 + joltage[1]);
}

uint64_t compute_joltage_from_twelve_banks(vec_uint64_t* p_batteries)
{
    uint64_t joltage[NBATTERIES];
    uint64_t indeces[NBATTERIES];

	for (size_t i = 0; i < NBATTERIES; i++)
	{
		joltage[i] = 0;
		indeces[i] = 0;
	}
    uint64_t val_joltage = 0;
    
    for (size_t i = 0; i < NBATTERIES; i++)
	{
		for (size_t j = indeces[i]; j < (p_batteries->len-NBATTERIES+1+i); j++)
		{
			if (p_batteries->buf[j] > joltage[i])
			{
                joltage[i] = p_batteries->buf[j];
                if (i != NBATTERIES-1) indeces[i+1] = j+1;
			}
		}
        val_joltage = val_joltage * 10 + joltage[i];
	}
    return val_joltage;
}
    

int day_three_solution(int part) 
{
    FILE* fp = fopen("2025/data/input_day_three", "r");
    if (!fp) /* validate file open for reading */
	{
        return EXIT_FAILURE;
    }
	
    io_buffer_t io_buffer = { 0 };
    set_io_buffer(&io_buffer);
	
	vec_uint64_t batteries;
	init_vec_uint64(&batteries);
	
	uint64_t sum_joltage = 0;

	while(io_buffer.iteration < MAX_ITERATION_ALLOWED)
	{
		FGETS(fp); // read new line

		batteries.len = 0;
		while(*io_buffer.ptr != '\n' && *io_buffer.ptr)
		{
			push_vec_uint64(&batteries, (uint64_t)(*io_buffer.ptr - '0'));
			io_buffer.ptr++;
		}

		switch(part)
		{
		case 1:
			sum_joltage += compute_joltage(&batteries);
			break;
		case 2:
			sum_joltage += compute_joltage_from_twelve_banks(&batteries);
			break;
		default:
			printf("Invalid part\n");
			return EXIT_FAILURE;
		}
    }

    if (io_buffer.iteration == MAX_ITERATION_ALLOWED) 
	{
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }

    printf("TOTAL JOLTAGE: %llu\n", sum_joltage);
    
    if(fclose(fp)) 
	{
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
