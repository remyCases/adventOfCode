-- Copyright (C) 2024 Rémy Cases
-- See LICENSE file for extended copyright information.
-- This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

-- arguments

local position = {}
local name = {}
local prevNamed = nil
local nextPosArg = 0

function AddArgs(x, y, flag)
	if type(x) == "number" then
		position[x] = { y, flag }
	else
		name["-" .. x] = { y, flag }
		name["--" .. y] = { y, flag }
	end
end

local function printname()
	for key, val in pairs(name) do
		io.write(string.format("%s :", key))
		for k, v in pairs(val) do
			print(k, v)
		end
	end
end

function ParseArg(ar, i)
	local tmp, i_saved
	if i == 1 then
		nextPosArg = 0
	end

	if prevNamed then
		tmp = prevNamed
		prevNamed = nil
		return tmp, ar

	elseif name[ar] then
		nextPosArg = 0 -- a named is found, no pos can be found anymore
		if name[ar][2] and string.find(name[ar][2], 'b') then
			return name[ar][1], name[ar][1]
		else
			prevNamed = name[ar][1]
		end

	elseif position[i] then
		if position[i][2] and string.find(position[i][2], '+') then
			nextPosArg = i + 1
			i_saved = i
		end
		return position[i][1], ar

	elseif i == nextPosArg then
		nextPosArg = nextPosArg + 1
		return position[i_saved][1], ar
	else
		print(string.format("does not undertand arg %s in position %i", ar, i))
	end
end

function HelpArgs()
	for k,v in ipairs(position) do
		if string.find(v[2], '+') then
			io.write(string.format("%s_1 ... %s_n ", v[1], v[1]))
		else
			io.write(string.format("%s ", v[1]))
		end
	end

	for k,v in pairs(name) do
		if not string.find(k, "--", 1, true) then
			if not v[2] or not string.find(v[2], "b") then
				io.write(string.format("%s ",k))
				io.write(string.format("%s ", string.upper(v[1])))
			else
				io.write(string.format("[%s] ",k))
			end
		end
	end
	io.write("\n")
end
