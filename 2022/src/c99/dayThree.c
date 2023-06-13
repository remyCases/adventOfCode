#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "dayThree.h"

#define SIZE_BUFFER 7
#define NBITS 8
#define NELEMENTS (SIZE_BUFFER * NBITS)

static uint8_t char_to_index_in_bitfield(char ch) {
    // lower case between 97 and 122, which will placed between 0 and 25
    // upper case between 65 and 90, which will placed between 26 and 51

    uint8_t int_ch = (int)ch;

    if (int_ch >= (int)'a' && int_ch <= (int)'z') {
        return int_ch - (int)'a';
    }
    else if (int_ch >= (int)'A' && int_ch <= (int)'Z') {
        return int_ch - (int)'A' + 26;
    }

    return (uint8_t)(-1);
}

int day_three_solution(int part) {

    FILE* fp = fopen("2022/data/input_day_three.txt", "r");
    if (!fp) { /* validate file open for reading */
        return EXIT_FAILURE;
    }

    io_buffer_t io_buffer = { 0 };
    set_io_buffer(&io_buffer);

    size_t len = 0;
    uint8_t first_bitfield[SIZE_BUFFER]; // 52 bits are need, which translate to 6.5 octets
    uint8_t second_bitfield[SIZE_BUFFER]; // 52 bits are need, which translate to 6.5 octets
    uint8_t third_bitfield[SIZE_BUFFER]; // 52 bits are need, which translate to 6.5 octets
    uint8_t index_in_bitfield = 0u;

    uint8_t flag = 0u;
    uint16_t priorities = 0u;

    while(io_buffer.iteration < MAX_ITERATION_ALLOWED) {

        io_buffer.out_fgets = fgets(io_buffer.buf, BUFFER_SIZE, fp); /* read each line in file */
        if (!io_buffer.out_fgets) { /* reach end of file */
            break;
        }

        if (io_buffer.buf[0] == '\n') { /* empty line */
        } else {
            len = strlen(io_buffer.buf);

            if (part == 1) {
                memset(first_bitfield, 0, SIZE_BUFFER * sizeof(uint8_t));
                memset(second_bitfield, 0, SIZE_BUFFER * sizeof(uint8_t));

                for (uint8_t i = 0; i < (len-1)/2; i++) {
                    index_in_bitfield = char_to_index_in_bitfield(io_buffer.buf[i]);
                    if (index_in_bitfield == (uint8_t)(-1)) break;

                    first_bitfield[index_in_bitfield/NBITS] |= 1 << (index_in_bitfield % NBITS);
                }
                for (uint8_t i = (len-1)/2; i < len-1; i++) {
                    index_in_bitfield = char_to_index_in_bitfield(io_buffer.buf[i]);
                    if (index_in_bitfield == (uint8_t)(-1)) break;

                    second_bitfield[index_in_bitfield/NBITS] |= 1 << (index_in_bitfield % NBITS);
                }

                for (uint8_t i = 0; i < NELEMENTS; i++) {
                    flag = first_bitfield[i/NBITS] & second_bitfield[i/NBITS] & (1 << i % NBITS);
                    if(flag) {
                        priorities += i + 1;
                    }
                }
            }
            else if (part == 2) {
                switch (io_buffer.iteration%3)
                {
                case 0:
                    for (uint8_t i = 0; i < len-1; i++) {
                        index_in_bitfield = char_to_index_in_bitfield(io_buffer.buf[i]);
                        if (index_in_bitfield == (uint8_t)(-1)) break;

                        first_bitfield[index_in_bitfield/NBITS] |= 1 << (index_in_bitfield % NBITS);
                    }
                    break;
                case 1:
                    for (uint8_t i = 0; i < len-1; i++) {
                        index_in_bitfield = char_to_index_in_bitfield(io_buffer.buf[i]);
                        if (index_in_bitfield == (uint8_t)(-1)) break;

                        second_bitfield[index_in_bitfield/NBITS] |= 1 << (index_in_bitfield % NBITS);
                    }
                    break;
                case 2:
                    for (uint8_t i = 0; i < len-1; i++) {
                        index_in_bitfield = char_to_index_in_bitfield(io_buffer.buf[i]);
                        if (index_in_bitfield == (uint8_t)(-1)) break;

                        third_bitfield[index_in_bitfield/NBITS] |= 1 << (index_in_bitfield % NBITS);
                    }
                    for (uint8_t i = 0; i < NELEMENTS; i++) {
                        flag = first_bitfield[i/NBITS] & second_bitfield[i/NBITS] & third_bitfield[i/NBITS];
                        flag = flag & (1 << i % NBITS);
                        if(flag) {
                            priorities += i + 1;
                        }
                    }
                    memset(first_bitfield, 0, SIZE_BUFFER * sizeof(uint8_t));
                    memset(second_bitfield, 0, SIZE_BUFFER * sizeof(uint8_t));
                    memset(third_bitfield, 0, SIZE_BUFFER * sizeof(uint8_t));
                    break;
                default:
                    break;
                }
            }
        }

        io_buffer.iteration ++;
    }

    if (io_buffer.iteration == MAX_ITERATION_ALLOWED) {
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }
    
    switch(part) {
    case 1:
        printf("Priorities: %d\n", priorities);
        break;
    case 2:
        printf("Priorities: %d\n", priorities);
        break;
    default:
        break;
    }
    
    if(fclose(fp)) {
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}