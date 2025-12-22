-- Copyright (C) 2025 Rémy Cases
-- See LICENSE file for extended copyright information.
-- This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

local DialMax = 100
local Left = 1
local Right = -1

local function parseRotation(line)
    local o = line:byte(1)
    local rotation = {["orientation"]= 0, ["increment"]= 0, ["cycles"] = 0}
    if o == string.byte('L') then
        rotation.orientation = Left
    elseif o == string.byte('R') then
        rotation.orientation = Right
    else
        error(string.format("Error parsing %s", line))
    end

    local val = tonumber(string.sub(line,2))

    if not val then
        error(string.format("Error parsing %s", line))
    end

    rotation.increment = val % DialMax
    rotation.cycles = val // DialMax
    return rotation
end

local function readFileAndCompute(filename, option)
    local dial_at_zero = 0
    local dial = 50

    local file = io.open(filename,"rb")
    if not file then
        return nil
    end
    file:close()

    for line in io.lines(filename) do
        local rotation = parseRotation(line)

        if option == '1' then
            if rotation.increment == 0 then
                goto continue
            end

            if rotation.orientation == Left then
                dial = (dial + DialMax - rotation.increment) % DialMax
            elseif rotation.orientation == Right then
                dial = (dial + rotation.increment) % DialMax
            end

            if dial == 0 then
                dial_at_zero = dial_at_zero + 1
            end

        elseif option == '2' then
            dial_at_zero = dial_at_zero + rotation.cycles
            if rotation.increment == 0 then
                goto continue
            end

            if rotation.orientation == Left then
                if dial <= rotation.increment and dial ~= 0 then
                    dial_at_zero = dial_at_zero + 1
                end
                dial = (dial + DialMax - rotation.increment) % DialMax
            elseif rotation.orientation == Right then
                if dial + rotation.increment >= DialMax then
                    dial_at_zero = dial_at_zero + 1
                end
                dial = (dial + rotation.increment) % DialMax
            end
        else
            print("Invalid part")
            return
        end

        ::continue::
    end
    print(string.format("DIAL AT ZERO: %d", dial_at_zero))
end

-- table of functions accessibles through require
local M = { }

local function main(part)
    local filename = "2025/data/input_day_one"
    readFileAndCompute(filename, part)
end
M.main = main

return M