// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

#ifndef UTILSIO_H_   /* Include guard */
#define UTILSIO_H_

#define BUFFER_SIZE 300u
#define MAX_ITERATION_ALLOWED 10000u

#define FGETS(FILE_PTR)		io_buffer.out_fgets = fgets(io_buffer.buf, BUFFER_SIZE, FILE_PTR); \
							io_buffer.ptr = io_buffer.buf; \
        					io_buffer.iteration++; \
							if (!io_buffer.out_fgets) { \
								break; \
							}

#define PARSE_ELEMENT(...)  io_buffer.count = sscanf(io_buffer.ptr, __VA_ARGS__); \
                            if (io_buffer.count == EOF) { \
                                if (ferror(fp)) { \
                                    perror("fscanf"); \
                                    return EXIT_FAILURE; \
                                } \
                                break; \
                            } \
                            else if (io_buffer.count != EXPECTED_ARGUMENTS) {  \
                                fprintf(stderr, \
                                "Error: sscanf successfully matched and assigned %i input items but %i were expected\n", \
                                io_buffer.count, EXPECTED_ARGUMENTS); \
                                return EXIT_FAILURE; \
                            }

typedef struct io_buffer_s 
{
    char buf[BUFFER_SIZE];
	char* ptr;
    char* out_fgets;
    size_t iteration;
    int count;
} io_buffer_t;

void set_io_buffer(io_buffer_t* p_io_buffer);

#endif // UTILSIO_H_
