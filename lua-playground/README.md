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