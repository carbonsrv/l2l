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


-- print(read_expression(reader.environ("1-(1-9*(7-3))*(4-7)*7"))) -- why fail?
print(read_expression(reader.environ("1-9*(7-3)")))
-- print(read_factor(reader.environ("(7-3)")))
-- print(read_factor(reader.environ("(7-3)")))
-- return read_expression(environment, bytes)

