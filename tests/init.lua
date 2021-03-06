local _, compiler = require("l2l.compat"), require("l2l.compiler")
local reader = require("l2l.reader")
local itertools = require("l2l.itertools")
local eval = require("l2l.eval")
local show, cons = itertools.show, itertools.cons

local add = eval.dofile("tests/add.lisp")
assert(add == 65, "Addition failed! ".. tostring(add))

local mathy = eval.dofile("tests/math.lisp")
assert(mathy == -40, "Math failed! " .. tostring(mathy))

local listy = eval.dofile("tests/list.lisp")
assert(listy[1] == 5, "List car failed! " .. tostring(listy[1]))
assert(listy[2][1] == 9, "List cadr failed.")
assert(listy[2][2] == 51, "List cddr failed.")
assert(show(listy[2]) == show(itertools.cons(9, 51)))

local fac, fib = unpack(eval.dofile("tests/y.lisp"))
assert(fac(8) == 40320, "Factorial failed!")
assert(fib(16) == 987, "Fibonacci failed!")