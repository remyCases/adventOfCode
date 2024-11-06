#!/usr/bin/env lua

-- Copyright (C) 2024 Rémy Cases
-- See LICENSE file for extended copyright information.
-- This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

require 'utils/argparser'

-- locals
local os_name = package.config:sub(1,1) == "\\" and "win" or "unix"
local lang = 0
local i = 1
local earlyEnd = false
local index = 1
local maxLang = 6

-- tables
local langFlag = {
	["cobol"] = 1,
	["cob"] = 1,
	["c99"] = 2,
	["c"] = 2,
	["nim"] = 3,
	["nimlang"] = 3,
	["rust"] = 4,
	["zig"] = 5,
	["asm"] = 6,
}

local cmdFlag = {
	[1] = {"./build/%d/bin/mainCob %d%d", "[[cobol]]"},
	[2] = {"./build/%d/bin/mainC99 %d %d", "[[c99]]"},
	[3] = {"./build/%d/bin/mainNim --day %d --part %d", "[[nim]]"},
	[4] = {"./build/rust/release/main_rust%d --day %d --part %d", "[[rust]]"},
	[5] = {"./build/%d/bin/mainZig --day %d --part %d", "[[zig]]"},
	[6] = {"./build/%d/bin/mainAsm %d%d", "[[asm]]"},
}

local function addFlag (x)
	if not langFlag[x] then
		print(string.format("Given language [[%s]] is not supported (yet?).", x))
	else
		lang = lang + (1 << (langFlag[x] - 1))
	end	
end

-- arguments for parser

AddArgs("h", "help", "b")
AddArgs("r", "rebuild", "b")
AddArgs("d", "day", nil)
AddArgs("p", "part", nil)
AddArgs(1, "lang", "+")
AddArgs("y", "year", nil)
local args = {}

-- help function
local function help ()
	HelpArgs()
end

-- parsing arguments

while arg[i] do
	local key, val = ParseArg(arg[i], i)
	if key then
		if key == "lang" then 
			addFlag(val)
		else
			args[key] = val
		end
	end
	i = i + 1
end

if args.help then
	help()
	goto endParse
end

-- test if all arguments were given
if not args.year then
	print("No year given, end of program.")
	earlyEnd = true
end
if not args.day then
	print("No day given, end of program.")
	earlyEnd = true
end
if not args.part then
	print("No part given, end of program.")
	earlyEnd = true
end

if earlyEnd then
	goto endParse
end

if args.rebuild then
	os.execute("make build_all")
end

if lang == 0 then
	lang = (1 << maxLang) - 1
end

-- apply arguments
while lang > 0 do
	if (lang&1) == 1 then
		local res = cmdFlag[index]
		local cmd = res[1]
		if os_name == "win" then cmd = cmd:gsub("/", "\\") end
		local lng = res[2]
		io.write(lng .. ":\t")
		local parsed_cmd = string.format(cmd, args.year, args.day, args.part)
	    local handle = io.popen(parsed_cmd)
		if not handle then goto nextParse end
		local output = handle:read("*a")
		local rc = {handle:close()}
		if (rc[1] == true) then -- error handling
			io.write(output)
		end
	end
	::nextParse::
	index = index + 1
	lang = lang >> 1
end

::endParse::
