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

typedef enum PARSING_STATUS {
	START,
	END,
} PARSING_STATUS;

typedef struct range_s {
	uint64_t start;
	uint64_t end;
} range_t;

typedef struct vec_range_s {
	range_t* buf;
	size_t len;
	size_t capacity;
} vec_range_t;

typedef struct vec_uint64_s {
	uint64_t* buf;
	size_t len;
	size_t capacity;
} vec_uint64_t;

void init_vec_range(vec_range_t* p_vec_range)
{
	p_vec_range->buf = (range_t*)calloc(INITIAL_SIZE, sizeof(range_t));
	p_vec_range->len = 0;
	p_vec_range->capacity = INITIAL_SIZE;
}

void push_vec_range(vec_range_t* p_vec_range, range_t* p_r)
{
	p_vec_range->buf[p_vec_range->len] = *p_r;
	p_vec_range->len++;
	if (p_vec_range->len == p_vec_range->capacity)
	{
		p_vec_range->capacity *= 2;
		p_vec_range->buf = (range_t*)realloc(p_vec_range->buf, p_vec_range->capacity * sizeof(range_t));
	}
}

void init_vec_uint64(vec_uint64_t* p_vec_uint64)
{
	p_vec_uint64->buf = (uint64_t*)calloc(INITIAL_SIZE, sizeof(uint64_t));
	p_vec_uint64->len = 0;
	p_vec_uint64->capacity = INITIAL_SIZE;
}

void push_vec_uint64(vec_uint64_t* p_vec_uint64, uint64_t u)
{
	p_vec_uint64->buf[p_vec_uint64->len] = u;
	p_vec_uint64->len++;
	if (p_vec_uint64->len == p_vec_uint64->capacity)
	{
		p_vec_uint64->capacity *= 2;
		p_vec_uint64->buf = (uint64_t*)realloc(p_vec_uint64->buf, p_vec_uint64->capacity * sizeof(uint64_t));
	}
}

void insert_vec_uint64(vec_uint64_t* p_vec_uint64, uint64_t u)
{
	for (uint64_t i = 0; i < p_vec_uint64->len; i++)
	{
		if (p_vec_uint64->buf[i] == u) return;
	}

	push_vec_uint64(p_vec_uint64, u);
}

uint64_t sum_vec_uint64(vec_uint64_t* p_vec_uint64)
{
	uint64_t sum = 0;
	for (uint64_t i = 0; i < p_vec_uint64->len; i++)
	{
		sum += p_vec_uint64->buf[i];
	}
	return sum;
}

uint64_t ilog10(uint64_t e)
{
	uint64_t ilog = 0;
	while(e >= 10)
	{
		ilog++;
		e /= 10;
	}
	return ilog;
}

uint64_t ipow10(uint64_t e)
{
	uint64_t ipow = 1;
	while(e > 0)
	{
		ipow *= 10;
		e--;
	}
	return ipow;
}

uint64_t find_repeated_twice_ids(uint64_t s, uint64_t e)
{
    uint64_t n = ilog10(s) + 1;
    vec_uint64_t ids;
	init_vec_uint64(&ids);

	if (n % 2)
	{
		return 0;
	}
	uint64_t u = ipow10(n / 2) + 1;
	
	uint64_t us = !(s % u) ? s / u : s / u + 1;
	uint64_t ue = e / u;
	for (uint64_t i = us; i <= ue; i++)
	{
		insert_vec_uint64(&ids, i * u);
	}

    return sum_vec_uint64(&ids);
}

uint64_t find_repeated_ids(uint64_t s, uint64_t e)
{
    uint64_t n = ilog10(s) + 1;
    vec_uint64_t ids;
	init_vec_uint64(&ids);

    for (uint64_t div = 2; div <= n; div++)
	{
        if (n % div)
		{
            continue;
        }
        uint64_t size_repetition = n / div;
        uint64_t u = 0;
		
		for (uint64_t i = 0; i < div; i++)
		{
			u += ipow10(size_repetition * i);
		}
        uint64_t us = !(s % u) ? s / u : s / u + 1 ;
        uint64_t ue = e / u;
        for (uint64_t i = us; i <= ue; i++)
		{
            insert_vec_uint64(&ids, i * u);
        }
    }
    return sum_vec_uint64(&ids);
}

int day_two_solution(int part) 
{
    FILE* fp = fopen("2025/data/input_day_two", "r");
    if (!fp) /* validate file open for reading */
	{
        return EXIT_FAILURE;
    }
	
    io_buffer_t io_buffer = { 0 };
    set_io_buffer(&io_buffer);

	char* e;
	uint64_t start = 0;
	uint64_t end = 0;
	uint64_t invalid_ids = 0;
	int parsing_status = START;
	vec_range_t vec_range_ids;
	init_vec_range(&vec_range_ids);

	while(io_buffer.iteration < MAX_ITERATION_ALLOWED)
	{
		FGETS(fp); // read new line

		do {
			switch (parsing_status)
			{
			case START:
				start = strtoull(io_buffer.ptr, &e, 10);
				if (io_buffer.ptr == e) // no number found
				{
					parsing_status = START;
					break;
				}
				else
				{
					parsing_status = END;
				}
				io_buffer.ptr = e + 1;
				break;
			case END:
				end = strtoull(io_buffer.ptr, &e, 10);
				if (io_buffer.ptr == e) // no number found
				{
					parsing_status = END;
					break;
				}
				else
				{
					parsing_status = START;
					range_t r = {start, end};
					push_vec_range(&vec_range_ids, &r);
				}
				io_buffer.ptr = e + 1;
				break;
			}
		} while(*e == ',' || *e == '-');
    }

	switch(part)
	{
	case 1:
		for (size_t i = 0; i < vec_range_ids.len; i++)
		{
			uint64_t s = vec_range_ids.buf[i].start;
			uint64_t e = vec_range_ids.buf[i].end;
			uint64_t ns = ilog10(s);
			uint64_t ne = ilog10(e);
			while (ns < ne)
			{
				invalid_ids += find_repeated_twice_ids(s, ipow10(ns+1)-1);
				ns += 1;
				s = ipow10(ns);
			}
			invalid_ids += find_repeated_twice_ids(s, e);
		}
		break;
	case 2:
		for (size_t i = 0; i < vec_range_ids.len; i++)
		{
			uint64_t s = vec_range_ids.buf[i].start;
			uint64_t e = vec_range_ids.buf[i].end;
			uint64_t ns = ilog10(s);
			uint64_t ne = ilog10(e);
			while (ns < ne)
			{
				invalid_ids += find_repeated_ids(s, ipow10(ns+1)-1);
				ns += 1;
				s = ipow10(ns);
			}
			invalid_ids += find_repeated_ids(s, e);
		}
		break;
	default:
		printf("Invalid part\n");
		return EXIT_FAILURE;
	}

    if (io_buffer.iteration == MAX_ITERATION_ALLOWED) 
	{
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }

    printf("INVALID IDS: %llu\n", invalid_ids);
    
    if(fclose(fp)) 
	{
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
