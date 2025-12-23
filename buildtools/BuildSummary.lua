-- Copyright (C) 2025 Rémy Cases
-- See LICENSE file for extended copyright information.
-- This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

local function print_summary(prefix, filename)
    local ctr = 0
    for _ in io.lines(filename) do
        ctr = ctr + 1
    end
    print(string.format(prefix, ctr))

    for ll in io.lines(filename) do
        print(ll)
    end
end

local success = io.open("build/success.log","rb")
local failure = io.open("build/failure.log","rb")


print("========================================")
print("BUILD SUMMARY")
print("========================================")

if success then
		print_summary("Successful builds: %d", "build/success.log")
else
		print("Successful builds: 0")
end
	print("")

if failure then
		print_summary("Failed builds: %d", "build/failure.log")
else
		print("Failed builds: 0")
end
print("========================================")

if success then
    success:close()
end

if failure then
    failure:close()
end