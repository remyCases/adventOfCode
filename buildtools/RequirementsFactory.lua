-- Copyright (C) 2025 Rémy Cases
-- See LICENSE file for extended copyright information.
-- This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

require 'utils/argparser'

local function append_mypy(filename)
    local file = io.open(filename,"a")
    if not file then
        print(string.format("Error, can't open %s.", filename))
        return nil
    end
    file:write("mypy", "\n")
    file:close()
end

AddArgs(1, "filename", nil)
local _, filename = ParseArg(arg[1], 1)
local file = io.open(filename,"rb")

-- file does not exist
if not file then
    append_mypy(filename)
    return nil
end
file:close()

-- file does exist
for line in io.lines(filename) do
    if string.find(line, "mypy") then
        goto endProgram
    end
end

append_mypy(filename)

::endProgram::