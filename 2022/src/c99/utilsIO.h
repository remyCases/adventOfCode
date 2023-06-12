#ifndef UTILSIO_H_   /* Include guard */
#define UTILSIO_H_

#define BUFFER_SIZE 100u
#define MAX_ITERATION_ALLOWED 10000u

#define PARSE_ELEMENT(...)  io_buffer.count = sscanf(io_buffer.buf, __VA_ARGS__); \
                            if (io_buffer.count == EOF) { \
                                if (ferror(fp)) { \
                                    perror("fscanf"); \
                                    return EXIT_FAILURE; \
                                } \
                                break; \
                            } \
                            else if (io_buffer.count != EXPECTED_ARGUMENTS) {  \
                                fprintf(stderr, \
                                "Error: fscanf successfully matched and assigned %i input items, %i expected\n", \
                                io_buffer.count, EXPECTED_ARGUMENTS); \
                                return EXIT_FAILURE; \
                            }

typedef struct io_buffer_s {
    char buf[BUFFER_SIZE];
    char* out_fgets;
    size_t iteration;
    int count;
} io_buffer_t;

void set_io_buffer(io_buffer_t* p_io_buffer);

#endif // UTILSIO_H_