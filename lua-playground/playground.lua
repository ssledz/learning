#! /usr/bin/lua5.3

--[[
execute playground.lua and start an interactive session
]]
-- lua5.3 -i playground.lua

function fact (n)
    if n == 0 then
        return 1
    else
        return n * fact(n - 1)
    end
end

print("lua-playground")
print()
a = 4

--[[
a = io.read("*n") -- reads a number
-- ]]

print(fact(a))
print(math.pi / 4)
print("a = " .. tostring(a))
print("a ^ 2 = " .. tostring(a ^ 2))
print("a + 2 = " .. tostring(a + 2))

print()
print("there are eight basic types in Lua: nil, boolean, number, string, userdata, function, thread, and table.")
-- function type give the type name of a given value
print(type(nil))               -- nil
print(type(true))              -- boolean
print(type(10.4 * 3))          -- number
print(type("Hello world"))     -- string
print(type(io.stdin))              -- userdata
print(type(print))                 -- function
print(type(type))                  -- function
print(type({}))                -- table
print(type(type(X)))               -- string

print()
print("Lua is a dynamically-typed language")
print("b is " .. type(b))
b = "a string!!"
print("b is " .. type(b))
b = nil
print("b is " .. type(b))
b = 3
print("b is " .. type(b))

--[[
  Type Nil
  Nil is type with a single value, nil. A kind of non-value, to represent the absence of a value
--]]

--[[
  Type Boolean
  The Boolean type has two values: true and false
  Any value can represent a condition.
  Boolean false and nil == false
  Boolean true and everything else == true
--]]

print("4 and 5 = " .. tostring(4 and 5))
print("nil and 13 = " .. tostring(nil and 13))
print("false and 13 = " .. tostring(false and 13))
print("0 or 5 = " .. tostring(0 or 5))
print("false or \"hi\" = " .. tostring(false or "hi"))
print("nil or false = " .. tostring(nil or false))

--[[
  Type String
--]]

print("string.gsub(\"one string\", \"one\", \"another\") = " .. string.gsub(a, "one", "another"))
print("ALO\n123\"" == '\x41LO\10\04923"')

page = [[
<html>
<head>
  <title>An HTML Page</title>
</head>
<body>
  <a href="http://www.lua.org">Lua</a>
</body>
</html>
]]
print(page)

code = [===[
a = b[c[i]]
]===]

print(code)

print("10" + 1)
local utf8 = require('utf8')
print(utf8.len("résumé"))
print(utf8.char(114, 233, 115, 117, 109, 233))

polyline = {
    color = "blue", thickness = 2, npoints = 4,
    { x = 0, y = 0 }, { x = -10, y = 0 }, { x = -10, y = 1 }, { x = 0, y = 1 }
}

print(polyline.color)
print(polyline[1].x)

opnames = {["+"] = "add", ["-"] = "sub", ["*"] = "mul", ["/"] = "div"}
print(opnames["-"])