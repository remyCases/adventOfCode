#!/usr/bin/lua

require 'utils/argparser'

-- locals
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
	[4] = {"./build/rust/bin/release/main_rust%d --day %d --part %d", "[[rust]]"},
	[5] = {"./build/%d/bin/mainZig --day %d --part %d", "[[zig]]"},
	[6] = {"./build/%d/bin/mainAsm %d%d", "[[asm]]"},
}

function addFlag (x)
	if not langFlag[x] then
		print(string.format("Given language [[%s]] is not supported (yet?).", x))
	else
		lang = lang + (1 << (langFlag[x] - 1))
	end	
end

-- arguments for parser

addArgs("h", "help", "b")
addArgs("r", "rebuild", "b")
addArgs("d", "day", nil)
addArgs("p", "part", nil)
addArgs(1, "lang", "+")
addArgs("y", "year", nil)
args = {}

-- help function
function help ()
	helpArgs()
end

-- parsing arguments

while arg[i] do
	key, val = parseArg(arg[i], i)
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
		res = cmdFlag[index]
		cmd = res[1]
		lng = res[2]
		io.write(lng .. ":\t")
	    local handle = io.popen(string.format(cmd, args.year, args.day, args.part))
		local output = handle:read("*a")
		local rc = {handle:close()}
		if (rc[1] == true) then -- error handling
			io.write(output)
		end
	end
	index = index + 1
	lang = lang >> 1
end

::endParse::
