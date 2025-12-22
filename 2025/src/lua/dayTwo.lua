-- Copyright (C) 2025 Rémy Cases
-- See LICENSE file for extended copyright information.
-- This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

local function ilog10(e)
	local ilog = 0
	while(e >= 10) do
		ilog = ilog + 1
		e = e / 10
    end
	return ilog
end

local function ipow10(e)
	local ipow = 1
	while(e > 0) do
		ipow = ipow * 10
		e = e - 1
    end
	return ipow
end

local function find_repeated_twice_ids(s, e)
    local n = ilog10(s) + 1
    local ids = {}
	local sum = 0

	if (n % 2 ~= 0) then
		return 0
    end
	local u = ipow10(n / 2) + 1

	local us
	if not (s % u) then
        us = s // u
    else
        us = s // u + 1
    end
	local ue = e // u
	for i = us, ue do
		local new_item = i * u
		if ids[new_item] == nil then
			sum = sum + new_item
		end
		ids[new_item] = true
	end
    return sum
end

local function find_repeated_ids(s, e)
    local n = ilog10(s) + 1
    local ids = {}
	local sum = 0

    for div = 2, n do
        if (n % div ~= 0) then
			goto continue
		end
        local size_repetition = n // div
        local u = 0

		for i = 0, div-1 do
			u = u + ipow10(size_repetition * i)
		end
        local us
		if not (s % u) then
			us = s // u
		else
			us = s // u + 1
		end
        local ue = e / u;
        for i = us, ue do
			local new_item = i * u
			if ids[new_item] == nil then
				sum = sum + new_item
			end
			ids[new_item] = true
		end
		::continue::
    end
    return sum
end

local function parseRange(line, ranges)
	for s, e in string.gmatch(line, "(%d+)-(%d+)") do
		table.insert(ranges, {["start"]=tonumber(s), ["end"]=tonumber(e)})
	end
end

local function readFileAndCompute(filename, option)
    local invalid_ids = 0
	local ranges = {}

    local file = io.open(filename,"rb")
    if not file then
        return nil
    end
    file:close()

    for line in io.lines(filename) do
        parseRange(line, ranges)
    end

	for _, r in pairs(ranges) do
		local s = r["start"]
		local e = r["end"]
		local ns = ilog10(s) + 1
		local ne = ilog10(e) + 1
		while (ns < ne) do
			if option == '1' then
				invalid_ids = invalid_ids + find_repeated_twice_ids(s, ipow10(ns)-1);
			elseif option == '2' then
				invalid_ids = invalid_ids + find_repeated_ids(s, ipow10(ns)-1);
			else
				print("Invalid part")
				return
			end

			s = ipow10(ns)
			ns = ns + 1
		end
		if option == '1' then
			invalid_ids = invalid_ids + find_repeated_twice_ids(s, e);
		elseif option == '2' then
			invalid_ids = invalid_ids + find_repeated_ids(s, e);
		else
			print("Invalid part")
			return
		end
	end

    print(string.format("INVALID IDS: %d", invalid_ids))
end

-- table of functions accessibles through require
local M = { }

local function main(part)
    local filename = "2025/data/input_day_two"
    readFileAndCompute(filename, part)
end
M.main = main

return M