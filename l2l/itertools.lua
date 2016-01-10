local function id(...)
  return ...
end

--- Returns a function that returns whether it's argument `text` matches
-- against any pattern in `...`.
-- @params pattern the lua pattern
local function match(...)
  local patterns = {...}
  return function(text)
    assert(type(text) == "string", type(text))
    for _, pattern in ipairs(patterns) do
      local matches = text:match(pattern)
      if matches then
        return matches
      end
    end
  end
end


local function index(t)
  return function(value)
    return t[value]
  end
end

local function resolve(str, t)
  local obj = t or _G
  for name in str:gmatch("[^.]+") do
    if obj then
      obj = obj[name]
    end
  end
  return obj
end

local function pack(...)
  return {...}, select("#", ...)
end

local function dict(...)
  local count = select('#', ...)
  if count % 2 ~= 0 then
    error("dict takes an even number of arguments. Received "..tostring(count))
  end
  local t = {}
  for i=1, count, 2 do
    t[select(i, ...)] = select(i+1, ...)
  end
  return t
end

local function vector(...)
  return {...}
end

local function bind(f, ...)
  assert(f, "missing f argument")
  local count = select("#", ...)
  local parameters = {...}
  return function(...)
    local count2 = select("#", ...)
    local all = {}
    for i=1, count do
      all[i] = parameters[i]
    end
    for i=1, count2 do
      all[count + i] = select(i, ...)
    end
    return f(table.unpack(all, 1, count + count2))
  end
end

local function show(...)
  local objs = {}
  local count = select("#", ...)
  for i=1, count do
    local obj = select(i, ...)
    if type(obj) == "table" and getmetatable(obj) == nil then
      local t = {}
      for name, value in pairs(obj) do
        table.insert(t, show(name))
        table.insert(t, show(value))
      end
      obj = "{" .. table.concat(t, " ") .. "}"
    elseif type(obj) == "function" then
      if debug then
        local info = debug.getinfo(obj)
        obj = "<"..tostring(obj)..
          tostring(" from "..info.short_src..":"..info.linedefined)..">"
      else
        obj = "<"..tostring(obj)..">"
      end
    elseif type(obj) ~= 'string' then
      obj = tostring(obj)
    else
      obj = '"' .. obj
        :gsub("\\", "\\\\")
        :gsub('"', '\\"')
        :gsub("\n", "\\n")
        ..'"'
    end
    objs[i] = obj
  end
  return unpack(objs, 1, count)
end

local function cadr(t)
  return t[2][1]
end

local function car(t)
  -- assert(t, "car: `t` missing."..debug.traceback())
  return t[1], t[2]
end

local function cdr(t)
  -- assert(type(t) == "table", debug.traceback())
  return t[2]
end

local function nextchar(invariant, index)
  index = (index or 0) + 1
  if index > #invariant then
    return nil
  end
  return index, invariant:sub(index, index)
end

local function nextnil() end

local list

local function tonext(obj, invariant, state)
  local t = type(obj)
  if t == "string" then
    return nextchar, obj, 0
  elseif getmetatable(obj) == list then
    return ipairs(obj)
  elseif t == "table" then
    if obj[1] ~= nil then
      return ipairs(obj)
    end
    return pairs(obj)
  elseif t == "function" then
    return obj, invariant, state
  elseif obj == nil then
    return nextnil, invariant, state
  else
    error(("object %s of type \"%s\" is not iterable"):format(obj, type(obj)))
  end
end

local function tovector(nextvalue, invariant, state)
  local t = {}
  for i, value in tonext(nextvalue, invariant, state) do
    t[i] = value
  end
  return t
end

local pair = function(t)
  return setmetatable(t, list)
end

local function tolist(nextvalue, invariant, state)
  -- tolist({1, 2, 3, 4})
  -- tolist({1, 2, 3}, 1) -- make improper list.
  -- tolist(map(f, {1, 2, 3}))
  local obj
  local mt = getmetatable(nextvalue)
  if mt == list or mt == nil then
    if mt and not invariant and not state then
      return nextvalue
    end
    if invariant and not state then
      obj = invariant
    end
  end
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  local value
  state, value = nextvalue(invariant, state)
  if state then
    return setmetatable({value,
      proper=true,
      nextvalue=nextvalue,
      invariant=invariant,
      state=state,
      ending=obj}, list)
  end
end

-- Returns a next, invariant, state triple for a coroutine function that
-- `yield`s values one by one. The function will be given an `index` argument,
--  which represents the number of values already calculated plus 1, as well as
-- a `yield` argument which the function must call for each value it generates.
-- @param f A function that givens an index and `yield`, calls each value with
--    `yield`.
local function generate(f)
  local routine = coroutine.create(f)
  return function(_, index)
    local ok, value = coroutine.resume(routine, coroutine.yield, index)
    if not ok then
      error(value)
    end
    if value ~= nil then
      return index + 1, value
    end
  end, f, 0
end

-- Returns a next, invariant, state triple for an iterate function that
-- `return`s values one by one. The function will be given a single `index`
-- argument, which represents the number of values already calculated plus 1.
-- @param f A function that givens an index, returns a value.
local function iterate(f)
  return function(_, index)
    local value = f(index)
    if value ~= nil then
      return index + 1, value
    end
  end, f, 0
end

-- Returns a next, invariant, state triple that represents infinitely repeated
-- `value`s.
local function repeated(value)
  return iterate(function() return value end)
end

-- Return whether the arguments do not contain values that have yet to be
-- calculated.
local function finalized(nextvalue, invariant, state)
  if type(nextvalue) == "function" or invariant or state then
    return false
  end
  if type(nextvalue) == "string" then
    return true
  end
  local mt = getmetatable(nextvalue)
  if not mt then
    return true
  end
  return mt == list and not list.nextvalue
end

-- Returns the iterable. Force evaluation of the iterable by iterating through
-- it. Relies on the iterable's implementation to cached calculated values.
local function finalize(nextvalue, invariant, state)
  if not finalized(nextvalue, invariant, state)
      or getmetatable(nextvalue) == list then
    for _, _ in tonext(nextvalue, invariant, state) do
      -- Do nothing.
    end
  end
  return nextvalue, invariant, state
end

local queue

-- Returns a list containing values of the iterable, and the last state. Force
-- evaluation of the iterable by iterating through it. Caches calculated values
-- in a list, and returns the state generated by the iterator at last value,
-- which is usually length + 1.
local function traverse(nextvalue, invariant, state)
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  local values = queue()
  local keys = queue()
  local index = 0
  local rest = state
  while true do
    index = index + 1
    state, value, key = nextvalue(invariant, state)
    if state ~= nil or value ~= nil or key ~= nil then
      queue.insert(values, value)
      queue.insert(keys, key)
    end
    if state == nil then
      break
    end
    rest = state
  end

  return rest, tolist(values), tolist(keys)
end

local function copy(obj)
  local t =  {}
  for k, value in pairs(obj) do
    t[k] = value
  end
  return setmetatable(t, getmetatable(obj))
end

list = setmetatable({
  -- Static
  iterate = function(f)
    return tolist(iterate(f))
  end,
  -- Static
  generate = function(f)
    return tolist(generate(f))
  end,
  extend = function(self, l)
    error("not implemented")
  end,
  unpack = function(self)
    if self then
      if getmetatable(self) ~= list then
        return self
      end
      return self[1], list.unpack(self[2])
    end
  end,
  -- Static
  next = function(cache, index)
    local self = cache[index]
    if self ~= nil then
      cache[index + 1] = self[2]
      return index + 1, self[1]
    end
  end,
  __ipairs = function(self)
    return list.next, {[0]=self}, 0
  end,
  reverse = function(self)
    if not self then
      return
    end
    if not self[2] then
      return self
    end
    local first = list.reverse(self[2])
    self[2][2]=self
    self[2]=nil
    return first
  end,
  insert = function(self, obj, ...)
    if not ... then
      return pair({obj, self})
    else
      return list.insert(pair({obj, self}), ...)
    end
  end,
  count = function(self, obj)
    local value = 0
    if not self then
      return 0
    end
    for _, v in ipairs(self) do
      if v == obj then
        value = value + 1
      end
    end
    return value
  end,
  concat = function(self, separator)
    separator = separator or ""
    if self == nil then
      return ""
    end
    return table.concat(tovector(self), separator)
  end,
  __len = function(self)
    if not self then
      return 0
    end
    local count = 0
    for _, _ in ipairs(self) do
      count = count + 1
    end
    return count
  end,
  __eq = function(self, other)
    while self and other do
      if self == nil and other == nil then
        return true
      end
      if self == nil and other ~= nil or
         self ~= nil and other  == nil then
        return false
      end
      if self[1] ~= other[1] then
        return false
      end
      self = self[2]
      other = other[2]
    end
    return self == other
  end,
  __tostring = function(self)
    local str = "("
    repeat
      if getmetatable(self) ~= list then
        return str..")"
      end
      str = str .. show(self[1])
      self = self[2]
      if getmetatable(self) == list then
        str = str .. " "
      elseif self ~= nil then
        str = str .. " . " .. tostring(self)
      end
    until not self
    return str .. ")"
  end,
  __index = function(self, i)
    local nextvalue = rawget(self, "nextvalue")
    if i == 2 and nextvalue then
      self.nextvalue = nil
      local invariant = self.invariant
      local state, value = nextvalue(invariant, self.state)
      if state == nil then
        self[2] = self.ending
        return self.ending
      end
      local rest
      if rawget(self, "proper") then
        rest = setmetatable({value,
          proper=true,
          nextvalue=nextvalue,
          invariant=invariant,
          state=state,
          ending=self.ending}, list)
      else
        rest = value
      end
      self[2] = rest
      return rest
    end
    return rawget(list, i)
  end
}, {__call = function(_, ...)
    local origin = setmetatable({}, list)
    local last = origin
    local count = select('#', ...)
    for i=1, count do
      last[2] = setmetatable({select(i, ...), nil}, list)
      last = last[2]
    end
    return origin[2]
  end,
  __tostring = function()
    return "list"
  end})

-- t = {aasdahkjsdga=5, 1, 2, 3, 4, z=1}

-- function nextdict(invariant, index)
--     local state = invariant[index]
--     local value
--     invariant[index + 1], value = next(t, state)
--     if not invariant[index + 1] then
--       return
--     end
--     return index + 1, value, invariant[index + 1]
-- end


-- for i, k, v in nextdict, {}, 0 do
--   print(i, k, v)
-- end

-- os.exit()


local function call(funcs, ...)
  if not funcs then
    return ...
  end
  local func, rest = car(funcs), cdr(funcs)
  return func(call(rest, ...))
end

local function compose(...)
  local funcs = {...}
  return function(...)
    return call(tolist(funcs), ...)
  end
end

local function foreach(f, nextvalue, invariant, state)
  local t = {}
  for i, value in tonext(nextvalue, invariant, state) do
    t[i] = f(value, i)
  end
  return t
end

local function foreacharg(f, ...)
  local t = {}
  local count = select("#", ...)
  for i=1, count do
    t[i] = f(select(i, ...), i)
  end
  return t
end

local function each(f, nextvalue, invariant, state)
  f = f or id
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  return function(_, index)
    local value
    index, value = nextvalue(invariant, index)
    if index ~= nil then
      return index, f(value, index)
    end
  end, invariant, state
end

local function map(f, nextvalue, invariant, state)
  return each(function(value) return f(value) end, nextvalue, invariant, state)
end

local function cons(a, b)
  return pair({a, b})
end

local function last(nextvalue, invariant, state)
  local obj
  for _, value in tonext(nextvalue, invariant, state) do
    obj = value
  end
  return obj
end

local function mapcar(f, l)
  f = f or id
  return function(invariant, index)
    local state = invariant[index]
    if not state then
      return nil
    end
    invariant[index + 1] = cdr(state)
    return index + 1, f(state)
  end, {[0]=l}, 0
end

queue = setmetatable({
  -- `queue` inserts into the back, and removes from the front.
  insert = function(self, value)
    -- Set the cdr of the last element.
    self[1][2] = cons(value)
    -- Update the last element to it's cdr.
    self[1] = self[1][2]
    return self
  end,
  extend = function(self, nextvalue, invariant, state)
    if getmetatable(nextvalue) == queue then
      -- Set the cdr of the last element.
      self[1][2] = nextvalue[2]
      -- Update the last element to last of `other`.
      self[1] = nextvalue[1][2] or self
    elseif getmetatable(nextvalue) == list then
      self[1][2] = last(mapcar(id, nextvalue))
      self[1] = self[1][2] or self
    else
      for i, value in tonext(nextvalue, invariant, state) do
        self:insert(value)
      end
    end
    return self
  end,
  remove = function(self)
    if self[2] then
      local value = cadr(self)
      self[2] = self[2][2]
      return value
    end
  end,
  __ipairs = function(self)
    return list.next, {[0]=self[2]}, 0
  end,
  __tostring = function(self)
    local symbol = require("l2l.reader3").symbol
    return tostring(cons(symbol("queue"), self[2]))
  end,
  __len = function(self)
    return list.__len(self[2])
  end,
  __eq = function(self, other)
    return getmetatable(self) == getmetatable(other)
      and self[2] == other[2] and self[1] == other[1]
  end,
  count = function(self, obj)
    return list.count(self[2], obj)
  end,
  concat = function(self, separator)
    return list.concat(self[2], separator)
  end
}, {__call = function(queue, ...)
    local origin = cons(nil)
    local last = origin
    for i=1, select("#", ...) do
      last[2] = cons(select(i, ...))
      last = last[2]
    end
    origin[1] = last
    return setmetatable(origin, queue)
  end
})

queue.__index = queue


local function arguments(...)
  local count = select("#", ...)
  local parameters = {...}
  return function(_, index)
    if index < count then
      return index + 1, parameters[index + 1]
    end
  end, parameters, 0
end

local eacharg = function(f, ...)
  return each(f, arguments(...))
end

local maparg = function(f, ...)
  return map(f, arguments(...))
end

local function range(start_or_stop, stop, step)
  local start = (stop and (start_or_stop or 1)) or 1
  stop = stop or start_or_stop
  step = step or (start <= stop and 1 or -1)
  return function(_, index)
    local value = start + index * step
    if not stop or value <= stop then
      return index + 1, start + index * step
    end
  end, step, 0
end

local function fold(f, initial, nextvalue, invariant, state)
  -- assert(not invariant or state, "`nextvalue` is a required argument.")
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  local value
  repeat
    state, value = nextvalue(invariant, state)
    if state then
      initial = f(initial, value)
    end
  until state == nil
  return initial
end


-- Returns a concatenation of each sequence returned by 
-- `nextvalue, invariant, state`.
local function join(nextvalue, invariant, state)
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  local cnext, cinv, cstate
  state, cnext, cinv, cstate  = nextvalue(invariant, state)
  return function(current, index)
    if not current[0] then
      return
    end
    local value
    current[3], value = current[1](current[2], current[3])

    if not current[3] then
      local cnext, cinv, cstate
      current[0], cnext, cinv, cstate = nextvalue(invariant, current[0])
      current[1], current[2], current[3] = tonext(cnext, cinv, cstate)
      current[3], value = current[1](current[2], current[3])
    end
    if current[3] then
      return index + 1, value
    end
  end, {[0]=state, tonext(cnext, cinv, cstate)}, 0
end

local function filter(f, nextvalue, invariant, state)
  f = f or id
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  return function(cache, index)
    assert(index)
    if cache[index * 2 + 2] then
      return cache[index * 2 + 2], cache[index * 2 + 3]
    end
    local value
    state = cache[index * 2]
    repeat
      state, value = nextvalue(invariant, state)
    until not state or f(value, state)
    if state then
      cache[index * 2 + 2] = state
      cache[index * 2 + 3] = value
      return index + 1, value
    end
  end, {[0] = state}, 0
end

local function search(f, nextvalue, invariant, state)
  for i, v in tonext(nextvalue, invariant, state) do
    if f(v) then
      return v, i
    end
  end
end

local function contains(target, nextvalue, invariant, state)
  for i, v in tonext(nextvalue, invariant, state) do
    if v == target then
      return i
    end
  end
  return false
end

local function keys(nextvalue, invariant, state)
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  return function(_, index)
    state = nextvalue(invariant, state)
    if state ~= nil then
      return index + 1, state
    end
  end, invariant, 0
end

local function flip(f) return
  function(b, a, ...) return
    f(a, b, ...)
  end
end

-- Returns an identity function that, for the same `list` instance, returns
-- the same reference, rather than a new reference if it's wrapped in a
-- `tonext`. For non-`list`s, returns the same arguments. Optionally provide
-- `f` to be applied to each value. Used for `drop` and `span`, so that:
-- `rawtostring(drop(1, l))` == `rawtostring(cdr(l))`.
-- @param target The object to identify.
-- @param f The function to apply to each value of `target`.
local function identify(target, _, _, f)
  local islist = getmetatable(target) == list
  return function(nextvalue, invariant, index)
    if islist and invariant and finalized(invariant[index]) then
      if f then
        invariant, index = f(invariant, index)
      end
      return invariant[index]
    end
    if f then
      nextvalue, invariant, index = tonext(nextvalue, invariant, index)
      return function(...)
        return nextvalue(f(...))
      end, invariant, index
    end
    return nextvalue, invariant, index
  end
end

local function span(n, nextvalue, invariant, state)
  if n == 0 then
    return nil, tolist(nextvalue, invariant, state)
  end
  local identity = identify(nextvalue, invariant, state)

  local first = {}
  local value

  nextvalue, invariant, state = tonext(nextvalue, invariant, state)

  if type(n) == "number" then
    while state do
      state, value = nextvalue(invariant, state)
      if not state then
        break
      end
      first[state] = value
      if state == n then
        break
      end
    end
  else
    while state do
      state, value = nextvalue(invariant, state)
      if not state then
        break
      end
      first[state] = value
      if n(value, state) then
        break
      end
    end
  end
  if not state then
    return tovector(nextvalue, invariant, state)
  end
  return first, tolist(identity(nextvalue, invariant, state))
end

local function take(n, nextvalue, invariant, state)
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  local t = type(n) == "number"
  return function(_, index)
    local value
    index, value = nextvalue(invariant, index)
    if index and (t and index <= n or not t and n(value, index)) then
      return index, value
    end
  end, invariant, state
end


local function drop(n, nextvalue, invariant, state)
  if n == 0 then
    return nextvalue, invariant, state
  end
  local count, value = 0
  local identity = identify(nextvalue, invariant, state,
    function(_, index)
      if type(n) == "number" then
        while count < n and index do
          index, value = nextvalue(invariant, index)
          count = count + 1
        end
      else
        local previous
        repeat
          previous = index
          index, value = nextvalue(invariant, index)
          if not index then
            break
          end
        until not n(value, index)
        n = 0
        index = previous
      end
      return invariant, index
    end)
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  return identity(nextvalue, invariant, state)
end

local function scan(f, initial, nextvalue, invariant, state)
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  local value
  return function(_, index)
    index, value = nextvalue(invariant, index)
    if index then
      initial = f(initial, value)
      return index, initial
    end
  end, invariant, state
end


local function slicecar(start, finish, l) return
  take(function(node) return node ~= finish end,
    drop(function(node) return node ~= start end,
      mapcar(id, l)))
end

--- Returns array inclusive of start and finish indices.
-- 1 is first position. 0 is last position. -1 is second last position.
-- @param objs iterable to slice.
-- @param start first index.
-- @param finish second index
local function slice(start, finish, nextvalue, invariant, state)
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)

  if finish and finish <= 0 then
    finish = #tovector(nextvalue, invariant, state) + finish
  end

  return function(cache, index)
    local value
    state, value = nextvalue(invariant, cache[index])
    while state and state < start and (not finish or state <= finish) do
      state, value = nextvalue(invariant, state)
    end
    if state and state >= start and (not finish or state <= finish) then
      cache[index + 1] = state
      return index + 1, value
    end
  end, {[0] = state}, 0
end


local function zip(...)
  local invariants = {}
  local nextvalues = {}
  return function(cache, index)
    local complete = false
    local states = {}
    local values = tovector(each(function(state, i)
        local nextvalue = nextvalues[i]
        local invariant, value = invariants[i]
        states[i], value = nextvalue(invariant, state)
        if states[i] == nil then
          complete = true
        end
        return value
      end, cache[index]))
    if complete == false then
      cache[index + 1] = states
      return index + 1, values
    end
  end, {[0] = tovector(eacharg(function(argument, i)
      local nextvalue, invariant, state = tonext(argument)
      invariants[i] = invariant
      nextvalues[i] = nextvalue
      return state
    end, ...))}, 0
end

local function concat(separator, nextvalue, invariant, state)
  separator = separator or ""
  local mt = getmetatable(nextvalue)
  if type(nextvalue) == "table" and not mt then
    return table.concat(nextvalue, separator)
  end
  if mt == list then
    return list.concat(nextvalue, separator)
  end
  nextvalue, invariant, state = tonext(nextvalue, invariant, state)
  local text, value
  state, text = nextvalue(invariant, state)
  if state then
    while true do
      state, value = nextvalue(invariant, state)
      if not state then
        break
      end
      text = text .. separator .. value
    end
  end
  return text
end

local unpack = unpack

local function _unpack(nextvalue, invariant, state)
  if type(nextvalue) == "table" and not getmetatable(nextvalue) then
    return unpack(nextvalue)
  end
  return unpack(tovector(nextvalue, invariant, state))
end

local function rawtostring(obj)
  if type(obj) ~= "table" then
    return tostring(obj)
  end
  local mt = getmetatable(obj)
  setmetatable(obj, nil)
  local text = tostring(obj)
  setmetatable(obj, mt)
  return text
end

-- Return the calling of `f` with the given arguments.
-- @param f Function to call.
-- @param ... The arguments to call with.
local function apply(f, ...)
  return f(...)
end

local function isinstance(value, mt)
  return getmetatable(value) == mt
end

return {
  apply=apply,
  index=index,
  bind=bind,
  cadr = cadr,
  car=car,
  cdr=cdr,
  compose=compose,
  concat=concat,
  cons=cons,
  contains=contains,
  copy=copy,
  dict=dict,
  drop=drop,
  each=each,
  empty=nextnil,
  filter=filter,
  finalize=finalize,
  finalized=finalized,
  flip=flip,
  fold=fold,
  foreach=foreach,
  foreacharg=foreacharg,
  id=id,
  isinstance=isinstance,
  iterate=iterate,
  join=join,
  keys=keys,
  last=last,
  list=list,
  map=map,
  maparg=maparg,
  mapcar=mapcar,
  match=match,
  pack=pack,
  pair=pair,
  queue=queue,
  range=range,
  rawtostring = rawtostring,
  repeated=repeated,
  resolve=resolve,
  scan=scan,
  search=search,
  show=show,
  slice=slice,
  slicecar=slicecar,
  span=span,
  take=take,
  tolist=tolist,
  tonext = tonext,
  tovector=tovector,
  traverse=traverse,
  unlift=unlift,
  unpack = _unpack,
  vector=vector,
  zip=zip
}
