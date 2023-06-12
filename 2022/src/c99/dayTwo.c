#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "dayTwo.h"

#define EXPECTED_ARGUMENTS 2

const uint32_t pointsFromResultsArray[9] = {
    3, 6, 0,
    0, 3, 6,
    6, 0, 3
};

const uint32_t pointFromYourChoiceArray[9] = {
    3, 1, 2,
    1, 2, 3,
    2, 3, 1
};

static uint32_t compute_points(char ch1, char ch2, int part) {
    uint32_t index1 = (int)(ch1 - 'A');
    uint32_t index2 = (int)(ch2 - 'X');
    uint32_t points = 0u;
    
    switch (part)
    {
    case 1:
        points = pointsFromResultsArray[3 * (index1) + index2] + index2 + 1;
        break;
    case 2:
        points = pointFromYourChoiceArray[index1 * 3 + index2] + index2 * 3;
        break;
    
    default:
        break;
    }

    return points;
}

int day_two_solution(int part) {

    FILE* fp = fopen("2022/data/input_day_two.txt", "r");
    if (!fp) { /* validate file open for reading */
        return EXIT_FAILURE;
    }

    io_buffer_t io_buffer = { 0 };
    set_io_buffer(&io_buffer);

    char opponent_choice = '\0';
    char your_choice = '\0';

    uint32_t total_points = 0u;

    while(io_buffer.iteration < MAX_ITERATION_ALLOWED) {

        io_buffer.out_fgets = fgets(io_buffer.buf, BUFFER_SIZE, fp); /* read each line in file */
        if (!io_buffer.out_fgets) { /* reach end of file */
            break;
        }

        if (io_buffer.buf[0] == '\n') { /* empty line */
        } else {
            PARSE_ELEMENT("%c %c", &opponent_choice, &your_choice)
            total_points += compute_points(opponent_choice, your_choice, part);
        }

        io_buffer.iteration ++;
    }

    if (io_buffer.iteration == MAX_ITERATION_ALLOWED) {
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }
    
    switch(part) {
    case 1:
        printf("score: %u\n", total_points);
        break;
    case 2:
        printf("score: %u\n", total_points);
        break;
    default:
        break;
    }
    
    if(fclose(fp)) {
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}