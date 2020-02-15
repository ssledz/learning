#! /usr/bin/lua5.3

function fact (n)
    if n == 0 then
        return 1
    else
        return n * fact(n - 1)
    end
end

print("lua-playground")
-- reads a number
-- a = io.read("*n")
a = 4
print(fact(a))
print(math.pi / 4)
print("a = " .. tostring(a))
print("a ^ 2 = " .. tostring(a ^ 2))
print("a + 2 = " .. tostring(a + 2))