local grammar = require("l2l.grammar2")
local reader = require("l2l.reader3")
local itertools = require("l2l.itertools")

local span = grammar.span
local any = grammar.any
local factor = grammar.factor


-- This example demonstrates mutual recursion.
-- The grammar is taken from:
-- https://theantlrguy.atlassian.net/wiki/display/ANTLR3/Left-Recursion+Removal
b = factor("b", function(left)
    return any(span(a, integer), integer)
end)


a = factor("a", function(left)
    return any(span(b, integer), integer)
end)

integer = factor("integer", function()
    return any("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
end)


local bytes = itertools.tolist("123456")
--------------------------------ababab
--------------------------------bababa
--------------------------------Depending on if a or b is called first.
--[[
b {(1) (1) (1) (1) (1) -}
--{ 5   4   3   2   1  0}
--> top = b, (a int)  5
--> top = a, (b int)  4
--> top = b, (a int)  3
--> top = a, (b int)  2
--> top = b, (a int)  1
--> top = a, (int)    0
a {(1) (1) (1) (1) (1) -}

]]--
local environment = reader.environ(bytes)

print(itertools.show(a(environment, bytes)))
-- return a(environment, bytes)
