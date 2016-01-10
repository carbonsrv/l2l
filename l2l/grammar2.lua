local itertools = require("l2l.itertools")
local reader = require("l2l.reader3")
local exception = require("l2l.exception2")

local car = itertools.car
local cdr = itertools.cdr
local cons = itertools.cons
local drop = itertools.drop
local id = itertools.id
local list = itertools.list
local tolist = itertools.tolist
local slice = itertools.slice
local concat = itertools.concat
local tovector = itertools.tovector
local empty = itertools.empty
local show = itertools.show
local tonext = itertools.tonext
local queue = itertools.queue
local search = itertools.search
local map = itertools.map
local join = itertools.join
local unique = itertools.unique
local range = itertools.range
local filter = itertools.filter
local bind = itertools.bind
local indexof = itertools.indexof
local index = itertools.index
local compose = itertools.compose
local zip = itertools.zip
local foreach = itertools.foreach
local asvector = itertools.asvector
local take = itertools.take

local raise = exception.raise
local execute = reader.execute
local Meta = reader.Meta

local ExpectedNonTerminalException =
  exception.Exception("ExpectedNonTerminalException",
    "Expected %s, at %s")
local ParseException =
  exception.Exception("ParseException",
    "An exception occurred while parsing `%s`:\n  %s")
local GrammarException =
  exception.Exception("GrammarException",
    "An exception occurred while generating grammar for `%s`:\n  %s")


local mark, span, any, terminal, factor

local function ismark(obj, flag)
  return getmetatable(obj) == mark and (not flag or obj[flag])
end

local function isgrammar(obj, kind)
  local mt = getmetatable(obj)
  if kind then
    return mt == kind
  end
  return mt == mark or mt == span or mt == any or 
    mt == terminal or mt == factor
end

local function iscompound(obj)
  local mt = getmetatable(obj)
  return mt == mark or mt == span or mt == any
end

local function factor_terminal(value, f)
  local count = #value
  local cache = {}
  return terminal(value, function(_, bytes)
    if not bytes then
      return
    end
    if not cache[bytes] then
      -- Reduce calls to `span` and `concat` by checking next byte.
      if bytes and string.byte(bytes[1]) ~= string.byte(value) then
        return bytes
      end
      local first, rest = itertools.span(count, bytes)
      if table.concat(first, "") == value then
        cache[bytes] = {rest, list(value), list(Meta(bytes, rest))}
      elseif bytes then
        cache[bytes] = {bytes}
      end
    end
    return unpack(cache[bytes], 1, 3)
  end, f)
end

local function mod_apply(self, apply)
  self.apply = apply
  return self
end

-- These are attributes that can be marked onto an expression grammar.
local skip = "skip"
local peek = "peek"
local option = "option"
local repeating = "repeating"

mark = setmetatable({
  __mod = mod_apply,
  __call = function(self, invariant, bytes, ...)
    local term, values, metas, rest = self[1], queue(), queue(), bytes
    local ok, value, meta, prev
    while true do
      prev = rest
      rest, vals, mets = term(invariant, rest, ...)
      values:extend(tovector(vals))
      metas:extend(tovector(mets))
      if not vals then
        if not self.repeating and not self.option then
          return bytes
        end
      end
      if not self.repeating then
        break
      end
      if not rest then
        break
      end
    end
    return rest, self.apply(values, metas)
  end,
  __tostring = function(self)
    local text = tostring(car(self))
    if self.option then
      text = "["..text.."]"
    end
    if self.repeating then
      text = "{"..text.."}"
    end
    if self.skip then
      text = "~"
    end
    return text
  end
}, {
  __call = function(_, term, ...)
    if type(read) == "string" then
      term = factor_terminal(term)
    end
    local self = setmetatable({term, apply=id}, mark)
    assert(term, "missing `term` argument.")
    for _, value in ipairs({...}) do
      self[value] = true
    end
    return self
  end,
  __tostring = function()
    return "mark"
  end
})

mark.__index = mark

-- create a terminal rule.
terminal = setmetatable({
  __call = function(self, invariant, bytes)
    return self.read(invariant, bytes)
  end,
  __tostring = function(self)
    return show(self.name)
  end
}, {
  __call = function(_, name, read)
    return setmetatable({read=read, name=name}, terminal)
  end,  
  __tostring = function()
    return "terminal"
  end
})
terminal.__index = terminal

local function toany(nextvalue, invariant, state)
  return any(itertools.unpack(nextvalue, invariant, state))
end


local function tospan(nextvalue, invariant, state)
  return span(itertools.unpack(nextvalue, invariant, state))
end


any = setmetatable({
  __mod = mod_apply,
  __call = function(self, invariant, bytes, ...)
    for _, term in ipairs(self) do
      if term then
        local rest, values, meta = term(invariant, bytes, ...)
        if values and rest ~= bytes then
          return rest, self.apply(values, meta)
        end
      end
    end
    return bytes
  end,
  __tostring = function(self)
    return "any("..concat(", ", map(show, asvector(self)))..")"
  end,
  -- Return a grammar where every span in `self` is applied to `f`.
  -- @param self grammar to apply `f` to.
  -- @return every choice in `self` applied with `f`.
  map = function(self, f)
    if isgrammar(self, any) then
      return toany(map(function(value)
        return any.map(value, f)
      end, asvector(self)))
    end
    return f(self)
  end
}, {
  __call = function(_, ...)
    local self = setmetatable(tovector(map(function(term)
        return type(term) == "string" and factor_terminal(term) or term
      end,
      itertools.filter(id, {...}))), any)
    self.apply = id
    return self
  end,
  __tostring = function()
    return "any"
  end
})

any.__index = any


span = setmetatable({
  __mod = mod_apply,
  __call = function(self, invariant, bytes, ...)
    local rest, values, metas = bytes, queue(), queue()
    for _, term in ipairs(self) do
      if term then
        local prev, vals, mets = rest
        rest, vals, mets = term(invariant, rest, ...)
        if not vals
            and not term.option
            and not term.repeating then
          return bytes
        end
        if ismark(term, peek) then
          rest = prev
        elseif not ismark(read, skip) then
          values:extend(tovector(vals))
          metas:extend(tovector(mets))
        end
      end
    end
    return rest, self.apply(values, metas, bytes)
  end,
  __tostring = function(self)
    return "span("..concat(", ", map(show, asvector(self)))..")"
  end,
  -- Return a grammar where every value in `self` is applied to `f`.
  -- @param self grammar to apply `f` to.
  -- @return every choice in `self` applied with `f`.
  map = function(self, f)
    if isgrammar(self, span) then
      return tospan(map(function(value)
        return span.map(value, f)
      end, asvector(self)))
    end
    return f(self)
  end
}, {
  __call = function(_, ...)
    local self = setmetatable(tovector(map(function(term)
        return type(term) == "string" and factor_terminal(term) or term
      end,
      itertools.filter(id, {...}))), span)
    self.apply = id
    return self
  end,
  __tostring = function()
    return "span"
  end
})

-- Return an iterable of various choices of rule `term`.
-- Refactors constructions like ((a | b) | c) into (a | b | c)
-- @param term grammar to refactor.
-- @return an iterable of the choices.
local function flatten(term)
  if isgrammar(term, any) then
    return join(map(flatten, asvector(term)))
  end
  return {term}
end

-- Return whether `term` can start with `target.
local function canhaveatindex(i, target, term)
  if term == target then
    return term
  end
  if isgrammar(term, span) then
    return canhaveatindex(i, target, index(i, term))
  end
  if isgrammar(term, any) then
    return search(bind(canhaveatindex, i, target), term)
  end
  return not isgrammar(term, terminal)
end

-- Return whether `term` can have `target` at `index`.
local function canhaveotherthanatindex(i, target, term)
  if term == target or isgrammar(term, span) and index(i, term) == target then
    return false
  end
  if isgrammar(term, any) then
    return search(bind(canhaveotherthanatindex, i, target), term)
  end
  return true
end

-- Return a version of `term` where each span with `target` at index `i` are
-- removed.
local function withotherthanatindex(i, target, term)
  if canhaveotherthanatindex(i, target, term) then
    if isgrammar(term, any) then
      return toany(filter(function(t)
        return not canhaveatindex(i, target, t)
      end, term))
    end
    if isgrammar(term, span) then
      local cur = term[i > 0 or #term + i + 1]
      term[i > 0 or #term + i + 1] = withotherthanatindex(i, target, cur)
      if isgrammar(index(i, term), any) and #index(i, term) == 0 then
        return any()
      end
    end
    return term
  end
  return any()
end

local function unwrap(term)
  while (isgrammar(term, any) or isgrammar(term, span)) and #term == 1 do
    term = term[1]
  end
  return term
end


-- create a nonterminal rule.
factor = setmetatable({
  canonical = function(self)
    if not self.canon then
      self.canon = self.def()
    end
    return self.canon
  end,
  -- Return the suffix of each span of `term` where `self` begins the
  -- span. The suffix includes all terms except the first of the span.
  suffixof = function(self, term)
    if canhaveatindex(1, self, term) then
      if self == term then
        return span()
      end
      if isgrammar(term, span) then
        return tospan(drop(1, term))
      end
      if isgrammar(term, any) then
        return compose(unwrap, toany, flatten, toany)(
          map(bind(self.suffixof, self), asvector(term)))
      end
      return term
    end
    return any()
  end,
  -- Return list of suffixes of `self` where it starts with a nonterminal
  -- that, when expanded, recurses into itself.
  left_suffixes = function(self)
    if not self._left_suffixes then
      self._left_suffixes = tolist(map(
        compose(
          compose(
            bind(withotherthanatindex, -1, self),
            bind(self.expandatindex, self, -1),
            bind(self.suffixof, self),
            bind(self.expandatindex, self, 1)),
          indexof(self.canon)),
        self:left_indices()))
    end
    return self._left_suffixes
  end,
  -- Expand each nonterminal in `term` as long as that nonterminal is not
  -- `self`, and the first element of the span it belongs to is not `self`.
  expandatindex = function(self, i, term)
    local action = true
    while action do
      action = false
      term = any.map(term, function(t)
        if isgrammar(t, factor) and t ~= self then
          local action = true
          return t:canonical()
        end
        if isgrammar(t, terminal) then
          return t
        end
        if isgrammar(t, span) and index(i, t) ~= self then
          return span.map(t, function(s) return self:expandatindex(i, s) end)
        end
        return t
      end)
    end
    return term
  end,
  -- Return whether `self` can start with `parent`.
  -- Note: `factor.startswith` arguments are switched compared to `startswith`.
  canstartwith = function(self, parent)
    return search(bind(canhaveatindex, 1, parent), parent:expandatindex(1, self))
  end,
  -- Return list of sorted indices of `self` where it starts with a nonterminal
  -- that, when expanded, recurses into itself.
  left_indices = function(self)
    if self._left_indices == nil then
      local term = self:canonical()
      if isgrammar(term, any) then
        self._left_indices = tolist(filter(function(i)
            if isgrammar(term[i], factor) then
              return term[i]:canstartwith(self)
            else
              return canhaveatindex(1, self, self:expandatindex(1, term[i]))
            end
          end, range(#term))) or false
      else
        self._left_indices = false
      end
    end
    return self._left_indices
  end,
  -- Returns a function that, given an index, will return a grammar that is
  -- equivalent to `canonical()`, except hiding all non-index-th
  -- left-recursion spans.
  factorize = function(self)
    local indices = self:left_indices()
    if isgrammar(self:canonical(), any) and indices then
      return function(index)
        local term = self.def()
        local left, lefts = car(indices)
        return toany(filter(function(_, i)
            if i == left then
              if lefts then
                left, lefts = car(lefts)
              end
              return i == index
            end
            return true
          end, term))
      end
    else
      return self.def
    end
  end,
  -- Return a list of expansion of left-recurring paths, with any 
  -- left-recurring paths from the expansion back to `self` replaced with any().
  -- By `terminal` it doesn't mean a simple string terminal, but meaning
  -- running it will terminate and won't cause a stack overflow.
  left_terminals = function(self)
    if not self._left_terminals then
      self._left_terminals = tolist(map(
        compose(
          compose(
            bind(withotherthanatindex, 1, self),
            bind(self.expandatindex, self, 1)),
          indexof(self.canon)),
        self:left_indices()))
    end
    return self._left_terminals
  end,
  -- Return a grammar where:
  --  1. Each left-recurring choice of `self` is transformed, such that:
  --    The choices of the left recurring choice, where it left-recurses into
  --    `self`, are removed.
  --  2. The non-left-recurring choices are also returned as part of the grammar,
  --  untouched.
  prefix = function(self)
    local term = self.def()
    for _, l in zip(self:left_indices(), self:left_terminals()) do
      i, t = unpack(l)
      term[i] = t
    end
    return toany(flatten(term))
  end,
  __call = function(self, invariant, bytes, state)
    -- if not bytes then
    --   return bytes
    -- end
    if not self.factory then
      self.factory = self:factorize()
    end
    if not self:left_indices() then
      return self.canon(invariant, bytes, state)
    end
    if not self.empty then
      self.empty = self.factory()
    end
    if not state or state.factor == self then
      if not bytes then
        return bytes, nil
      end
      if state and state.path then
        local i
        i, state.path = car(state.path)
        return self.factory(i)(invariant, bytes, state)
      elseif not state then
        state = {factor=self, cache={}}
        local rest, prefix, meta = self:prefix()(invariant, bytes, state)

        if not prefix then
          return self.empty(invariant, bytes, state)
        end

        if not rest then
          local independent = toany(flatten(toany(self:left_terminals())))
          return independent(invariant, bytes, state)
        end
        local values
        rest = bytes
        state.path = list()
        while rest do
          values = nil
          local suffix, i = search(function(suffix)
            rest, values = suffix(invariant, rest, state)
            return values
          end, self:left_suffixes())
          if not i then
            rest, values = self.empty(invariant, rest, state)
          end
          if values then
            state.path = list.insert(state.path,
              i ~= nil and index(i, self:left_indices()) or nil)
          else
            break
          end
        end
        return self(invariant, bytes, state)
      end
    else
      return self.canon(invariant, bytes, state)
    end
  end,
  __tostring = function(self)
    return tostring(self.name)
  end
}, {
  __call = function(_, name, canonical)
    return setmetatable({name=name, def=canonical}, factor)
  end,  
  __tostring = function()
    return "factor"
  end
})
factor.__index = factor

return {
  skip=skip,
  option=option,
  repeating=repeating,
  span=span,
  any=any,
  grammar=grammar,
  mark=mark,
  -- Terminal=Terminal,
  -- NonTerminal=NonTerminal,
  ismark=ismark,
  isgrammar=isgrammar,
  factor=factor,
  factor_terminal=factor_terminal,
  filter=filter,
  -- ExpectedNonTerminalException=ExpectedNonTerminalException,
  ParseException=ParseException,
  GrammarException=GrammarException
}
