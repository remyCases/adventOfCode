#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "dayFour.h"

#define EXPECTED_ARGUMENTS 4

int day_four_solution(int part) {

    FILE* fp = fopen("2022/data/input_day_four.txt", "r");
    if (!fp) { /* validate file open for reading */
        return EXIT_FAILURE;
    }

    io_buffer_t io_buffer = { 0 };
    set_io_buffer(&io_buffer);

    uint16_t fs_range = 0u;
    uint16_t fe_range = 0u;
    uint16_t ss_range = 0u;
    uint16_t se_range = 0u;

    uint32_t count = 0u;

    while(io_buffer.iteration < MAX_ITERATION_ALLOWED) {

        io_buffer.out_fgets = fgets(io_buffer.buf, BUFFER_SIZE, fp); /* read each line in file */
        if (!io_buffer.out_fgets) { /* reach end of file */
            break;
        }

        if (io_buffer.buf[0] == '\n') { /* empty line */
        } else {
            PARSE_ELEMENT("%hu-%hu,%hu-%hu", &fs_range, &fe_range, &ss_range, &se_range)
            if (fs_range <= ss_range && se_range <= fe_range) {
                count += 1u;
            } 
            else if (ss_range <= fs_range && fe_range <= se_range) {
                count += 1u;
            }
        }
        io_buffer.iteration ++;
    }

    if (io_buffer.iteration == MAX_ITERATION_ALLOWED) {
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }
    
    switch(part) {
    case 1:
        printf("Counts: %d\n", count);
        break;
    case 2:
        printf("part2: WIP\n");
        break;
    default:
        break;
    }
    
    if(fclose(fp)) {
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}