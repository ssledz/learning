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
4 and 5              -- 5
nil and 13           -- nil
false and 13         -- false
0 or 5               -- 0
false or "hi"        -- hi
nil or false         -- false
not nil              -- true
not false            -- true
not 0                -- false
```

### Number type

```
4                -- 5
0.4              -- 0.4
4.57e-3          -- 0.00457
0.3e12           -- 300000000000.0
5E+20            -- 5e+20
type(3)          -- number
type(3.5)        -- number
1 == 1.0         -- true
0.2e3 == 200     -- true
math.type(3)     -- integer
math.type(3.0)   -- float
0xff             -- 255
0x1A3            -- 419
```

#### match library

```
math.sin(math.pi / 2)          -- 1.0
math.max(10.4, 7, -3, 20)      -- 20
math.huge                      -- inf
random                         -- [0, 1)
random(6)                      -- [1, 6]
math.floor(3.3)                -- 3
math.floor(-3.3)               -- -4
math.ceil(3.3)                 -- 4
math.ceil(-3.3)                -- -3
math.modf(3.3)                 -- 3  ;rounds toward 0
math.modf(-3.3)                -- -3
```

setting seed for random
```lua
math.randomseed(os.time())
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