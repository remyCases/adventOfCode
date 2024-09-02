// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

#include <string.h>
#include "utilsIO.h"

void set_io_buffer(io_buffer_t* p_io_buffer)
{
    memset(p_io_buffer->buf, 0, BUFFER_SIZE * sizeof(char));
    p_io_buffer->ptr = NULL;
    p_io_buffer->out_fgets = NULL;
    p_io_buffer->iteration = 0u;
    p_io_buffer->count = 0;
}
