local grammar = require("l2l.grammar2")
local reader = require("l2l.reader3")
local itertools = require("l2l.itertools")

local span = grammar.span
local any = grammar.any
local mark = grammar.mark
local repeating = grammar.repeating
local factor = grammar.factor

one = factor("one", function() return
    any(span(number, "1"), "1")
end)

two = factor("two", function() return
    any(span(number, "2"), "2")
end)

two_ = factor("two_", function() return
    span(two)
end)

number = factor("number", function() return
    any(one, two_,
        span("(", number, ")", mark(one, repeating)))
end)

local bytes = itertools.tolist("1")
local environment = reader.environ("1")

-- print(number(reader.environ("(1)(1)1")))
print(number(reader.environ("(1)(1)1")))

--[[
number(
    "(",number(one("1")), ")",
    one(
        number(
            "(", number(
                    one("1")), ")"), 
        "1"))

]]--

-- return number(environment, bytes)
