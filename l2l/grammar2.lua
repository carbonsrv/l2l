local exception = require("l2l.exception2")
local itertools = require("l2l.itertools")
local reader = require("l2l.reader3")

local asvector = itertools.asvector
local bind = itertools.bind
local car = itertools.car
local cdr = itertools.cdr
local compose = itertools.compose
local concat = itertools.concat
local cons = itertools.cons
local contains = itertools.contains
local drop = itertools.drop
local each = itertools.each
local filter = itertools.filter
local flip = itertools.flip
local id = itertools.id
local index = itertools.index
local indexof = itertools.indexof
local isinstance = itertools.isinstance
local join = itertools.join
local list = itertools.list
local map = itertools.map
local queue = itertools.queue
local range = itertools.range
local search = itertools.search
local show = itertools.show
local tolist = itertools.tolist
local tovector = itertools.tovector
local unique = itertools.unique

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
    return mt == kind or (type(obj) == "string" and kind == terminal)
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
  return terminal(value, function(_, bytes, limit)
    if not bytes or bytes == limit then
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
  __call = function(self, invariant, bytes, limit, ...)
    local term, values, metas, rest = self[1], queue(), queue(), bytes
    local ok, value, meta, prev
    while true do
      prev = rest
      rest, vals, mets = term(invariant, rest, limit, ...)
      values:extend(tovector(vals))
      metas:extend(tovector(mets))
      if not vals then
        if not self.repeating and not self.option then
          return bytes
        else
          break
        end
      end
      if not self.repeating then
        break
      end
      if not rest or rest == limit then
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
    if type(term) == "string" then
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
    return unique(join(map(flatten, asvector(term))))
  end
  return {term}
end

-- Remove recurring layers of single length `any` or `span` wrappings.
-- E.g. Transform `span(any(span(span(t))))` to `t`.
-- @param term The grammar to refactor.
local function unwrap(term, remove_mark)
  while (isgrammar(term, any) or isgrammar(term, span)) and #term == 1
    or (remove_mark and isgrammar(term, mark)) do
    term = term[1]
  end
  return term
end

-- Return a simplified `any` given an `iterable` triple.
local function toflatany(iterable, ...)
  return unwrap(toany(flatten(toany(unique(iterable, ...)))))
end

-- Return the stack of left-recurring nonterminals by recursively looking the 
-- first element of each `span` of `term`.
-- @param term The grammar to look through.
-- @param ignore For internal use only. *Do not provide this argument.*
local function left_nonterminals(term, ignore)
  ignore, term = term, ignore
  if ignore and not term then
    term, ignore = ignore, {}
  end
  if isgrammar(term, span) then
    return #term > 0 and left_nonterminals(ignore, term[1]) or nil
  elseif isgrammar(term, any) then
    return tolist(join(map(bind(left_nonterminals, ignore), term)))
  else
    while isgrammar(term, mark) do
      term = term[1]
    end
    if isgrammar(term, factor) and not ignore[term] then
      ignore[term] = true
      return cons(term, left_nonterminals(ignore, term:canonical()))
    end
  end
end

-- Return whether `origin` is left recursive.
-- @param `origin` must be a nonterminal or a`mark` wrapping a nonterminal
local function is_left_nonterminal(origin)
  origin = unwrap(origin, true)
  return contains(origin, left_nonterminals(origin:canonical()))
end

-- Return whether running `origin` will invoke a left-recursive path.
local function is_left_recursive(origin)
  origin = unwrap(origin, true)
  if isgrammar(origin, factor) then
    return is_left_nonterminal(origin)
  elseif isgrammar(origin, span) then
    return #origin == 0 or is_left_recursive(origin[1])
  elseif isgrammar(origin, any) then
    return search(function(term) return is_left_recursive(term) end, any)
  end
end

-- Remove any left nonterminal that can occur as a first term of `term`.
local function truncate_left_nonterminal(term)
  assert(isgrammar(term, span) or isgrammar(term, factor),
    "`term` must be a `factor` or `span`.: "..tostring(term))
  if not is_left_recursive(term) then
    return term
  end
  term = unwrap(term, true)
  if isgrammar(term, factor) then
    return span()
  end
  return tospan(drop(1, term))
end

-- This function is *not* the inverse of `truncate_left_nonterminal`.
-- Remove `nonterminal` from the right side when it occurs as a last term of
-- `term`.
local function truncate_right_nonterminal(term, nonterminal)
  if unwrap(term, true) == nonterminal then
    return
  elseif isgrammar(term, any) then
    return toany(filter(id, map(function(t)
        return truncate_right_nonterminal(t, nonterminal)
      end, term)))
  elseif isgrammar(term, span) then
    return tospan(each(function(t, i)
        if i == #term then
          return truncate_right_nonterminal(t, nonterminal)
        else
          return t
        end
      end, term))
  end
  return term
end

-- Return a subset of `nonterminal` grammar that is guaranteed to consume a 
-- token, representing terminating form of the left nonterminal of `origin`.
local function toconsume(origin, ignore)
  origin, ignore = unwrap(origin, true), ignore or {{}, {}}
  if isgrammar(origin, terminal) then
    return origin
  elseif isgrammar(origin, factor) then
    if is_left_nonterminal(origin) then
      if ignore[1][origin] then
        return
      end
      ignore[1][origin] = true
      return toconsume(origin:canonical(), ignore)
    end
    return origin:canonical()
  elseif isgrammar(origin, any) then
    return toflatany(filter(id, map(bind(flip(toconsume), ignore), origin)))
  elseif isgrammar(origin, span) then
    local first = unwrap(origin[1], true)
    if isgrammar(first, factor) then
      if is_left_nonterminal(first) then
        if ignore[2][first] then
          return
        end
        ignore[2][first] = true
        return toconsume(origin[1], ignore)
      end
      return origin
    elseif isgrammar(first, any) then
      -- if first element is `any` then need to expand it like.
      -- In python vararg notation, this is the transformation:
      -- span(any(*choices), *rest) => any(span(x, *rest) for x in choices)
      local rest = tolist(drop(1, origin))
      return toany(map(function(term)
          return tospan(cons(toconsume(term, ignore), rest))
        end, first))
    else
      return tospan(cons(toconsume(origin[1], ignore),
        tolist(drop(1, origin))))
    end
  end
end

-- Return a subset of `nonterminal` grammar that is guaranteed to consume a 
-- token, representing terminating form of `origin`.
local function toterminal(origin, ignore)
  origin, ignore = unwrap(origin, true), ignore or {{}, {}}
  if isgrammar(origin, terminal) then
    return origin
  elseif isgrammar(origin, factor) then
    if is_left_nonterminal(origin) then
      if ignore[1][origin] then
        return origin:factory()
      end
      ignore[1][origin] = true
      return toterminal(origin:canonical(), ignore)
    end
    return origin
  elseif isgrammar(origin, any) then
    return toflatany(filter(id, map(bind(flip(toterminal), ignore), origin)))
  elseif isgrammar(origin, span) then
    local first = unwrap(origin[1], true)
    if isgrammar(first, factor) then
      if is_left_nonterminal(first) then
        if ignore[2][first] then
          return
        end
        ignore[2][first] = true
        return tospan(cons(toterminal(origin[1], ignore),
          tolist(drop(1, origin))))
      end
      return origin
    elseif isgrammar(first, any) then
      -- if first element is `any` then need to expand it like.
      -- In python vararg notation, this is the transformation:
      -- span(any(*choices), *rest) => any(span(x, *rest) for x in choices)
      local rest = tolist(drop(1, origin))
      return toany(map(function(term)
          return tospan(cons(toterminal(term, ignore), rest))
        end, first))
    else
      return tospan(cons(toterminal(origin[1], ignore),
        tolist(drop(1, origin))))
    end
  end
end

-- Nonterminal type.
factor = setmetatable({
  -- Return the canonical definition of `self`, i.e. without any modifications.
  canonical = function(self)
    if not self.canon then
      self.canon = self.def()
    end
    return self.canon
  end,
  -- Return whether `term` is left-recurring involving `self.
  is_left_recursive = function(self, term)
    if not is_left_recursive(term) then
      return false
    end
    return contains(self, left_nonterminals(term))
  end,
  -- Return left recurring choices in `self`, if its definition is an `any`.
  left_indices = function(self)
    if self._left_indices == nil then
      local canon = self:canonical()
      if isgrammar(canon, any) then
        self._left_indices = tolist(filter(function(i) return
            self:is_left_recursive(canon[i])
          end, range(#canon))) or false
      else
        self._left_indices = false
      end
    end
    return self._left_indices
  end,
  left_terms = function(self)
    if self._left_terms == nil then
      self._left_terms = toany(map(indexof(self:canonical()),
        self:left_indices()))
    end
    return self._left_terms
  end,
  factory = function(self, i)
    self._factory, i = self._factory or {}, i or false
    if not self._factory[i] then
      local canon = self:canonical()
      if not isgrammar(canon, any) then
        self._factory[i] = canon
      elseif i then
        self._factory[i] = canon[i]
      else
        self._factory[i] = toany(filter(id,
          each(function(term, j) return
            not contains(j, self:left_indices()) and term or nil
          end, canon)))
      end
    end
    return self._factory[i]
  end,
  -- Return terminals in `term` that occur after `self`.
  -- @param term A left recurring path that will recur into `self`.
  left_suffix = function(self, term)
    term = unwrap(term)
    if isgrammar(term, span) then      
      local first = unwrap(term[1], true)
      if isgrammar(first, any) then
        local rest = tolist(drop(1, origin))
        return toflatany(filter(id, map(function(term)
            return self:left_suffix(tospan(cons(term, rest)))
          end, filter(bind(self.is_left_recursive, self), first))))
      elseif self:is_left_recursive(first) then
        if is_left_recursive(term[2]) then
          if isgrammar(unwrap(term[2], true), factor) then
            return tospan(cons(term[2]:factory(), tolist(drop(2, term))))
          end
          -- Untested case.
          return toconsume(tospan(tolist(drop(2, term))))
        end
        return tospan(tolist(drop(1, term)))
      end
    elseif isgrammar(term, any) then
      return toflatany(filter(id, map(function(t)
        return self:left_suffix(t)
      end, term)))
    elseif isgrammar(term, factor) then
      if term == self then
        return
      end
      return self:left_suffix(term:canonical())
    else
      return
    end
  end,
  -- Return terminals in left recurring choices that can occur after `self`,
  -- if its definition is an `any`.
  left_suffixes = function(self)
    if not self._left_suffixes then
      self._left_suffixes = tolist(map(
        compose(bind(self.left_suffix, self), indexof(self.canon)),
        self:left_indices()))
    end
    return self._left_suffixes
  end,
  initialize = function(self, rest, values, metas)
    return rest, self.init(values, metas)
  end,
  execute = function(self, invariant, bytes, limit, paths)
    if not self:left_indices() then
      local canon = self:canonical()
      return canon(invariant, bytes, limit, paths)
    elseif paths[bytes] then
      local i, step, lim
      step, paths[bytes] = car(paths[bytes])
      i, lim = unpack(step)
      paths[bytes] = paths[bytes] or cons({nil, lim})
      return self:factory(i)(invariant, bytes, lim, paths)
    end
    local rest, values, metas
    rest, values = self.consume(invariant, bytes, limit, paths)
    if not values then
      return bytes
    end
    metas = cons({bytes=bytes, rest=rest})

    -- Lookahead until end of input or no more valid parse.
    while rest and rest ~= limit do
      local from = rest
      local suffix, i = search(function(suffix)
          rest, values = suffix(invariant, rest, limit, paths)
          return values
        end, self:left_suffixes())
      if i then
        metas = list.insert(metas, {bytes=from, rest=rest})
      else
        break
      end
    end

    -- Construct a path for each suffix.
    local top, path = self
    while cdr(metas) do
      local meta = car(metas)
      local _, i = search(function(suffix)
          rest, values = suffix(invariant, meta.bytes, meta.rest, paths)
          return values and rest == meta.rest
        end, top:left_suffixes())
      assert(i)
      local j = index(i, top:left_indices())
      local term = top:factory(j)
      if #truncate_left_nonterminal(term) > 0 then
        metas = cdr(metas)
      end
      path = list.insert(path, {j, meta.rest, top})
      top = car(left_nonterminals(term))
    end

    -- Find the final step for the prefix.
    while metas do
      local meta = car(metas)
      local canon = top:canonical()
      canon = isgrammar(canon, any) and canon or any(canon)
      local indices = tovector(filter(function(i)
          return not is_left_recursive(top.canon[i])
            or isgrammar(unwrap(top.canon[i], true), factor)
        end, range(#canon)))
      -- This is the prefix. Prioritise non-left recursive terms.
      table.sort(indices, function(i, j)
          return is_left_recursive(canon[j])
        end)
      -- Find the actual step by matching each branch.
      local i = search(function(i)
        local term = canon[i]
        local consume
        if isgrammar(term, factor) then
          term.consume = term.consume
            or truncate_right_nonterminal(toconsume(term), term)
          consume = term.consume
        else
          consume = toconsume(term)
        end
        rest, values = consume(invariant, meta.bytes, meta.rest, paths)
        return values and rest == meta.rest
      end, indices)
      assert(i)
      local term = top:factory(i)
      -- Terminate only if the selected step is not a proxy nonterminal.
      if not isgrammar(unwrap(term, true), factor)
          or not is_left_recursive(term) then
        metas = cdr(metas)
      end
      path = list.insert(path, {i, meta.rest, top})
      if is_left_recursive(term) then
        top = car(left_nonterminals(term))
      end
    end
    paths[bytes] = list.reverse(path)
    -- print(self, limit, paths[bytes])
    return self(invariant, bytes, limit, paths)
  end,
  __call = function(self, invariant, bytes, limit, paths)
    assert(not limit or isinstance(limit, list))
    self.canon = self:canonical()
    self.consume = self.consume or truncate_right_nonterminal(
      toconsume(self), self)
    paths = paths or {}
    self.cache = self.cache or {}
    if not bytes or bytes == limit then
      return bytes
    end
    if not self.cache[bytes] then
      self.cache[bytes] = {
        path=paths[bytes],
        self:initialize(self:execute(invariant, bytes, limit, paths))
      }
    end
    return unpack(self.cache[bytes], 1, 3)
  end,
  __tostring = function(self)
    return tostring(self.name)
  end
}, {
  __call = function(_, name, canonical, init)
    return setmetatable({name=name, def=canonical, init=init or id}, factor)
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
  ismark=ismark,
  isgrammar=isgrammar,
  factor=factor,
  factor_terminal=factor_terminal,
  filter=filter,
  ParseException=ParseException,
  GrammarException=GrammarException
}
