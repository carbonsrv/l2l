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
    return any(span(char, "e"), a, b, "d", q, "z")
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

-- char(a(char(a(b(b("b"), "b"), "a")), "a")) -- bbba
-- char(b(char(b(char(b("b")), b("b"))), b("b")))

local rest, values, metas = char(reader.environ("bbababababb"))

---------------------------------------char(char(a(char(b(char("b"), "b")), "a")), "e")
print(rest, values, metas)
-- local bytes = itertools.tolist("12345")
-- local environment = reader.environ(bytes)

-- return a(environment, bytes)
