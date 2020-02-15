## Calling lua from shell 
```bash
lua -e 'print("Hello world")'
```

## Lua types

Lua is a dynamically-typed language

```lua
type(a) -- nil
a = 4
type(a) -- number
a = "a string !!!"
type(a) -- string
a = nil
type(a) -- nil
```

There are eight basic types in Lua: 

* nil
* boolean
* number
* string
* userdata
* function
* thread
* table

### Boolean type

The Boolean type has two values: true and false. Any value can represent a condition.
* Boolean `false` and `nil` == `false`
* Boolean `true` and everything else == `true`

```
4 and 5              --> 5
nil and 13           --> nil
false and 13         --> false
0 or 5               --> 0
false or "hi"        --> hi
nil or false         --> false
not nil              --> true
not false            --> true
not 0                --> false
```

### Number type

```
4                    --> 5
0.4                  --> 0.4
4.57e-3              --> 0.00457
0.3e12               --> 300000000000.0
5E+20                --> 5e+20
type(3)              --> number
type(3.5)            --> number
1 == 1.0             --> true
0.2e3 == 200         --> true
math.type(3)         --> integer
math.type(3.0)       --> float
0xff                 --> 255
0x1A3                --> 419
math.type(2^53)      --> float
math.type(3 + 0.0)   --> float   ; force int to be float
math.type(2^53 | 0)  --> integer ;force float to be int
math.type(2^53 | 0)  --> integer ;force float to be int
```

#### match library

```
math.sin(math.pi / 2)          --> 1.0
math.max(10.4, 7, -3, 20)      --> 20
math.huge                      --> inf
random                         --> [0, 1)
random(6)                      --> [1, 6]
math.floor(3.3)                --> 3
math.floor(-3.3)               --> -4
math.ceil(3.3)                 --> 4
math.ceil(-3.3)                --> -3
math.modf(3.3)                 --> 3    ;rounds toward 0
math.modf(-3.3)                --> -3
math.tointeger(3.0)            --> 3    ;convert number to int
math.tointeger(3.1)            --> nil  ;not an integral value
```

to convert number to `int` when possible
```lua
function cond2int (x) return math.tointeger(x) or x end
```

setting seed for random
```lua
math.randomseed(os.time())
```

limits
```
math.maxinteger + 1 == math.mininteger     --> true
math.mininteger - 1 == math.maxinteger     --> true
-math.mininteger == math.mininteger        --> true
math.mininteger // -1 == math.mininteger   --> true
```
### String type

Strings in Lua are immutable values.

```lua
a = "hello"
print(#a)                   --> 5             ;string length
print(#"good bye")          --> 8
print("Hello " .. "World")  --> Hello World   ;string concat
print("result is " .. 3)    --> result is 3   ;if one of the arguments is an int lua converts it to string
print('Hello "World"')      --> Hello World   ;there is no difference between "" and ''
print("Hello 'World'")      --> Hello World
```

escape sequences

```
\a          --> bell
\b          --> back space
\f          --> form feed
\n          --> newline
\r          --> carriage return
\t          --> horizontal tab
\v          --> vertical tab
\\          --> backslash
\"          --> double quote
\'          --> single quote
\ddd        --> ddd is a sequence of up to three decimal digits
\xhh        --> hh is a sequence of exactly two hexadecimal digits
\u{h... h}  --> any number of hexadecimal digits inside the brackets
\z          --> skips all subsequent space characters in the string until the first non-space character
```

example

```
"ALO\n123\"" == '\x41LO\10\04923"'        --> 'A' = 0x41, '\n' = 10, '1' = 049
data = "\x00\x01\x02\x03\x04\x05\x06\x07\z
\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F"         --> \z at the end of the first line skips the following end-of-line and the 
                                          --> indentation of the second line, so that the byte \x08 directly 
                                          --> follows \x07 in the resulting string
```

#### long string

```lua
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
```

```lua
code = [===[
a = b[c[i]]
]===]

print(code)
```
#### coercions

Lua provides automatic conversions between numbers and strings at run time. Any numeric 
operation applied to a string tries to convert the string to a number.

```lua
print(10 .. 20)         --> 1020
print("10" + 1)         --> 11.0
```

explicitly conversion (second parameter is `base` between `2` and `36`)

```lua
tonumber(" -3 ")        --> -3
tonumber("100101", 2)   --> 37
tonumber("fff", 16)     --> 4095
tonumber("-ZZ", 36)     --> -1295
tonumber("987", 8)      --> nil
```

#### string functions

```lua
string.gsub("one string", "one", "another")  --> another string   ;replace one string with another
string.rep("abc", 3)                         --> abcabcabc        ;replicates string
string.reverse("A Long Line!")               --> !eniL gnoL A
string.lower("A Long Line!")                 --> a long line!
string.upper("A Long Line!")                 --> A LONG LINE!
string.sub("[in brackets]", 2, -2)           --> in brackets      ;extracts a piece of the string s, from the i-th to the j-th
string.sub("[in brackets]", 1, 1)            --> [
string.sub(s, -1, -1)                        --> ]
string.byte("a")                             --> 97               ;converts from character to internal numeric representation
string.byte("abc")                           --> 97               ;it takes first character
string.byte("abc", 2)                        --> 98               ;it takes second character
string.byte("abc", -1)                       --> 99               ;it takes last character
string.byte("abc", 1, 2)                     --> 97 98            ;it returns multiple values between indices i and j (inclusive)
string.char(97)                              --> a                ;converts from numeric to char representation
string.format("x = %d y = %d", 10, 20)       --> x = 10 y = 20
string.format("x = %f", 200)                 --> x = 200.000000
string.format("pi = %.4f", math.pi)          --> pi = 3.1416
string.format("%02d/%02d/%04d", d, m, y)     --> 05/11/1990
```

## Operator precedence

```
^
unary operators            (- # ~ not)
* / // %
+ -
..                         (concatentation)
<< >>                      (bitwise shifts)
&                          (bitwise AND)
~                          (bitwise exclusive OR)
|                          (bitwise OR)
< > <= >= ~= ==
and
or
```

## Lua idioms

It sets `x` to a default value `v` when `x` is not set (provided that `x` is not set to `false`).
```lua
x = x or v
```
same as
```lua
if not x then x = v end
```

---

Equivalent to `C` expression `x = a ? b : c`
```lua
x = a and b or c
```
For example to select maximum of two numbers x and y
```lua
z = (x > y) and x or y
```