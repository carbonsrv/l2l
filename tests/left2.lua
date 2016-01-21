local grammar = require("l2l.grammar2")
local reader = require("l2l.reader3")
local itertools = require("l2l.itertools")

local span = grammar.span
local any = grammar.any
local mark = grammar.mark
local skip = grammar.skip
local option = grammar.option
local repeating = grammar.repeating
local factor = grammar.factor


-- Grammar from https://en.wikipedia.org/wiki/Left_recursion#Pitfalls
read_expression = factor("expression", function()
    return any(
        span(read_expression, "-", read_term),
        span(read_expression, "+", read_term),
        read_term)
end)

read_term = factor("term", function()
    return any(span(read_term, "*", read_factor), read_factor)
end)

read_factor = factor("factor", function()
    return any(span("(", read_expression, ")"), read_integer)
end)

read_integer = factor("integer", function()
    return any("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
end)

local bytes = itertools.tolist("1-(1-9*(7-3))*(4-7)*7")
local environment = reader.environ(bytes)

local t = "1-(1-9*(7-3))*(4-7)*7"

local repeated = itertools.repeated
local take = itertools.take
local tovector = itertools.tovector
local show = itertools.show
-- luajit 210 lua 74 lines per second

-- 500, takes 0.004 second in lua, 0.66 seconds in luajit - 165x slow.
-- 2.4 seconds in lua.
-- local text = table.concat(tovector(take(500, repeated(t))), "+")

local text = table.concat(tovector(take(20, repeated(t))), "+")
print(#text)
local profile = require("l2l.profile")
profile.profile(function() 
    -- print(text)
    local _, values, meta = read_expression(reader.environ(text))
    -- print(itertools.tolist(values))
    -- print(table.concat(tovector(take(400, repeated(t))), "+"))
end)

print(#text)