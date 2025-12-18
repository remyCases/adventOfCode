-- Copyright (C) 2024 Rémy Cases
-- See LICENSE file for extended copyright information.
-- This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

local utils = require 'utils.argparser'

local function generate_exe(f, year)
    f:write("[[bin]]", "\n")
    f:write(string.format("name=\"main_rust%d\"", year), "\n")
    f:write(string.format("path=\"%d/src/rust/main_rust.rs\"", year), "\n")
end

local function generateCargo(f, year_table)
    local template = io.open("buildtools/CargoFactory.toml","r")
    if not template then return nil end
    local content = template:read("*a")
    template:close()

    f:write(content, "\n")
    for key, val in pairs(year_table) do
        generate_exe(f, val)
    end
end

utils.addArgs(1, "years", "+")
local args = {}
local i = 1

while arg[i] do
	local _, val = utils.parseArg(arg[i], i)
	args[i] = val

	i = i + 1
end

local file = io.open("Cargo.toml","w")
if not file then
    print("Error, can't open Cargo.toml.")
    return nil
end
generateCargo(file, args)
file:close()
