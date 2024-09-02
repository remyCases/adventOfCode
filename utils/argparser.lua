-- arguments

local position = {}
local name = {}
local prevNamed = nil
local namedStart = false

function addArgs(x, y, flag)
	if type(x) == "number" then
		position[x] = { y, flag }
	else
		name["-" .. x] = { y, flag }
		name["--" .. y] = { y, flag }
	end
end

function printname()
	for key, val in pairs(name) do
		io.write(string.format("%s :", key))
		for k, v in pairs(val) do
			print(k, v)
		end
	end
end

function parseArg(ar, i)
	if i == 1 then
		namedStart = false
	end

	print(string.format("currently %d and %s", i, ar))
	if prevNamed then
		tmp = prevNamed
		prevNamed = nil
		return tmp, ar

	elseif name[ar] then
		namedStart = true
		if name[ar][2] and string.find(name[ar][2], 'b') then
			return name[ar][1], name[ar][1]
		else
			prevNamed = name[ar][1]
		end

	elseif position[i] and not namedStart then
		return position[i][1], ar
	
	else
		print(string.format("does not undertand arg %s in position %i", ar, i))
	end
end
