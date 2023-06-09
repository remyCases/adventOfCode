#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define MAX_ITERATION_ALLOWED 100000u

int main(int argc, char**argv) {
    FILE* fp = fopen("2022/data/input_day_one.txt", "r");
    if (!fp) {
        return EXIT_FAILURE;
    }

    uint16_t calories_count = 0u;
    uint16_t calories_input = 0u;
    int count = 0;
    size_t iteration = 0;

    while(iteration < MAX_ITERATION_ALLOWED) {
        count = fscanf(fp, "%10d", &calories_input);
        if (count == EOF) {
            if (ferror(fp)) {
                perror("fscanf");
                return EXIT_FAILURE;
            }
            break;
        }
        else if (count != 1) {
            fprintf(stderr, "Error: fscanf successfully matched and assigned %i input items, 1 expected\n", count);
            return EXIT_FAILURE;
        }
        calories_count += calories_input;
        iteration ++;
    }

    if (iteration == MAX_ITERATION_ALLOWED) {
        printf("Max iteration allowed (%u), result can be false.\n", MAX_ITERATION_ALLOWED);
    }
    printf("calorieTotal: %d", calories_count);
    if(fclose(fp)) {
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}