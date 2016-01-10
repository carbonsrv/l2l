local grammar = require("l2l.grammar2")
local reader = require("l2l.reader3")
local itertools = require("l2l.itertools")
local show = itertools.show
local span = grammar.span
local any = grammar.any
local factor = grammar.factor
local mark = grammar.mark
local repeating = grammar.repeating

char = factor("char", function()
    return any(a, b, "d", span(char, "e"), q, "z")
end)


b = factor("b", function()
  return any(span(char, b), "b", "b")
end)


a = factor("a", function()
  return any(span(char, a), "a", "A")
end)

q = factor("q", function()
  return any(span(char, "A ", "D"), char)
end)

local rest, values, metas = char(reader.environ("bbabe"))

print(rest, values, metas)
-- local bytes = itertools.tolist("12345")
-- local environment = reader.environ(bytes)

-- return a(environment, bytes)
