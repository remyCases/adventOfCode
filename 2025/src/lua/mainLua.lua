-- Copyright (C) 2025 Rémy Cases
-- See LICENSE file for extended copyright information.
-- This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

local argparser = require 'utils.argparser'
local dayOne = require '2025.src.lua.dayOne'
local dayTwo = require '2025.src.lua.dayTwo'

-- locals
local i = 1

-- arguments for parser
argparser.addArgs("d", "day", nil)
argparser.addArgs("p", "part", nil)
local args = {}

-- parsing arguments
while arg[i] do
	local key, val = argparser.parseArg(arg[i], i)
	if key then
		args[key] = val
	end
	i = i + 1
end

if args.day == '1' then
    dayOne.main(args.part)
elseif args.day == '2' then
    dayTwo.main(args.part)
else
    local error_message = "Incorrect combination of day and part. Day %s and part %s does not exist (yet)."
    print(string.format(error_message, args.day, args.part))
end