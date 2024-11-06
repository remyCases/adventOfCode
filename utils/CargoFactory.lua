-- Copyright (C) 2024 Rémy Cases
-- See LICENSE file for extended copyright information.
-- This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

require 'utils/argparser'

function generate_exe(year)
    file:write("[[bin]]", "\n")
    file:write(string.format("name=\"main_rust%d\"", year), "\n")
    file:write(string.format("path=\"../%d/src/rust/main_rust.rs\"", year), "\n")
end

function generateCargo(year_table)
    template = io.open("utils/CargoFactory.toml","r")
    if not template then return nil end
    local content = template:read("*a")
    template:close()

    file:write(content, "\n")
    for key, val in pairs(year_table) do
        generate_exe(val)
    end
end

addArgs(1, "years", "+")
local args = {}
local i = 1

while arg[i] do
	_, val = parseArg(arg[i], i)
	args[i] = val

	i = i + 1
end

file = io.open("utils/Cargo.toml","w")
generateCargo(args)
file:close()