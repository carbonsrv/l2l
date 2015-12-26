local function id(...)
  return ...
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

local function show(obj)
  if type(obj) == "table" and getmetatable(obj) == nil then
    local t = {}
    for name, value in pairs(obj) do
      table.insert(t, show(name))
      table.insert(t, show(value))
    end
    return "{" .. table.concat(t, " ") .. "}"
  elseif type(obj) ~= 'string' then
    obj = tostring(obj)
  else
    obj = '"' .. obj:gsub('"', '\\"'):gsub("\n", "\\n") .. '"'
  end
  return obj
end

local function car(t)
  assert(t, "car: `t` missing.")
  return t[1]
end

local function cdr(t)
  assert(t)
  return t[2]
end

local function nextchar(invariant, index)
  local index = (index or 0) + 1
  if index > #invariant then
    return nil
  end
  return index, invariant:sub(index, index)
end

local function nextnil() end


local list

local function generate(obj, invariant, state)
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

local pair = function(t)
  return setmetatable(t, list)
end

list = setmetatable({
  unpack = function(self)
    if self then
      if getmetatable(self) ~= list then
        return self
      end
      return self[1], list.unpack(self[2])
    end
  end,
  push = function(self, obj, ...)
    if not ... then
      return pair({obj, self})
    else
      return list.push(pair({obj, self}), ...)
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
    local str = tostring(self[1])
    if self[2] then
      str = str .. separator .. self[2]:concat(separator)
    end
    return str
  end,
  __len = function(self)
    if not self then
      return 0
    end
    local count = 0
    for i, value in ipairs(self) do
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
  next = function(cache, index)
    local self = cache[index]
    if self ~= nil then
      cache[index + 1] = cdr(self)
      return index + 1, car(self)
    end
  end,
  __ipairs = function(self)
    return list.next, {[0]=self}, 0
  end,
  __index = function(self, i)
    local nextvalue = rawget(self, "next")
    if i == 2 and nextvalue then
      local invariant = self.invariant
      local state, value = nextvalue(invariant, self.state)
      if state == nil then
        return self.ending
      end
      local rest = setmetatable({value,
        next=nextvalue,
        invariant=invariant,
        state=state,
        ending=self.ending}, list)
      self[2] = rest
      return rest
    end
    return rawget(list, i)
  end
}, {__call = function(_, ...)
    local orig = setmetatable({}, list)
    local last = orig
    for i=1, select('#', ...) do
      last[2] = setmetatable({select(i, ...), nil}, list)
      last = last[2]
    end
    return orig[2]
  end})

local function tolist(nextvalue, invariant, state)
  -- tolist({1, 2, 3, 4})
  -- tolist(map(f, {1, 2, 3}))
  nextvalue, invariant, state = generate(nextvalue, invariant, state)
  local state, value = nextvalue(invariant, state)
  if state then
    return setmetatable({value,
      next=nextvalue,
      invariant=invariant,
      state=state,
      ending=obj}, list)
  end
end

local function tovector(nextvalue, invariant, state)
  local t = {}
  for i, value in generate(nextvalue, invariant, state) do
    t[i] = value
  end
  return t
end

local function each(f, nextvalue, invariant, state)
  f = f or id
  nextvalue, invariant, state = generate(nextvalue, invariant, state)
  return function(invariant, state)
    local state, value = nextvalue(invariant, state)
    if state ~= nil then
      return state, f(value, state)
    end
  end, invariant, state
end

local function map(f, nextvalue, invariant, state)
  return each(function(value) return f(value) end, nextvalue, invariant, state)
end

local function mapcar(f, l)
  if l == nil then
    return nil
  end
  return cons(f(l), mapcar(f, cdr(l)))
end

local function arguments(...)
  local count = select("#", ...)
  local parameters = {...}
  return function(invariant, index)
    if index < count then
      return index + 1, parameters[index + 1]
    end
  end, invariant, 0
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
  return function(invariant, index)
    local value = start + index * step
    if not stop or value <= stop then
      return index + 1, start + index * step
    end
  end, step, 0
end

local function fold(f, initial, nextvalue, invariant, state)
  -- assert(not invariant or state, "`nextvalue` is a required argument.")
  nextvalue, invariant, state = generate(nextvalue, invariant, state)
  local value
  repeat
    state, value = nextvalue(invariant, state)
    if state then
      initial = f(initial, value)
    end
  until state == nil
  return initial
end

local function filter(f, nextvalue, invariant, state)
  f = f or id
  nextvalue, invariant, state = generate(nextvalue, invariant, state)
  return function(cache, index)
    local state, value = cache[index]
    repeat
      state, value = nextvalue(invariant, state)
    until not state or f(value, state)
    if state then
      cache[index + 1] = state
      return index + 1, value
    end
  end, {[0] = state}, 0
end

local function search(f, nextvalue, invariant, state)
  for i, v in generate(nextvalue, invariant, state) do
    if f(v) then
      return v
    end
  end
end

local function contains(target, nextvalue, invariant, state)
  for i, v in generate(nextvalue, invariant, state) do
    if v == target then
      return i
    end
  end
  return false
end

local function keys(nextvalue, invariant, state)
  nextvalue, invariant, state = generate(nextvalue, invariant, state)
  return function(invariant, index)
    local value
    state, value = nextvalue(invariant, state)
    if state ~= nil then
      return index + 1, state
    end
  end, invariant, 0
end


local function last(nextvalue, invariant, state)
  local obj
  for i, value in generate(nextvalue, invariant, state) do
    obj = value
  end
  return obj
end

local function flip(f)
  return function(b, a, ...)
    return f(a, b, ...)
  end
end

--- Returns array inclusive of start and finish indices.
-- 1 is first position. 0 is last position. -1 is second last position.
-- @objs iterable to slice.
-- @start first index.
-- @finish second index
local function slice(start, finish, nextvalue, invariant, state)
  nextvalue, invariant, state = generate(nextvalue, invariant, state)

  if finish and finish <= 0 then
    finish = #tolist(nextvalue, invariant, state) + finish
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

local function cons(a, b)
  return pair({a, b})
end

local function force(nextvalue, invariant, state)
  for i, value in generate(nextvalue, invariant, state) do
    -- Do nothing.
  end
  return nextvalue, invariant, state
end

local function span(n, nextvalue, invariant, state)
  nextvalue, invariant, state = generate(nextvalue, invariant, state)
  local finalized, secondstate, secondvalue = false
  local t = type(n) == "number"
  local value
  local first = tolist(function(invariant, index)
      if finalized then
        return
      end
      state, value = nextvalue(invariant, state)
      if state and (t and state <= n or not t and n(value, state)) then
        secondstate = state
        secondvalue = value
        return state, value
      else
        finalized = true
      end
    end, invariant, 0)
  return first,
    tolist(function(invariant, index)
      if not finalized then
        -- evaluate first
        force(first)
      end
      if t ~= nil then
        t = nil
        return index + 1, secondvalue
      end
      state, value = nextvalue(invariant, state)
      if state ~= nil then
        return index + 1, value
      end
    end, invariant, secondstate)
end

local function take(n, nextvalue, invariant, state)
  nextvalue, invariant, state = generate(nextvalue, invariant, state)
  local t = type(n) == "number"
  return function(invariant, state)
    local value
    state, value = nextvalue(invariant, state)
    if state and (t and state <= n or not t and n(value, state)) then
      return state, value
    end
  end, invariant, state
end

local function drop(n, nextvalue, invariant, state)
  nextvalue, invariant, state = generate(nextvalue, invariant, state)
  local t, value = type(n) == "number"
  while state and (t and state < n or true) do
    state, value = nextvalue(invariant, state)
    if not t and not n(value, state) then
      break
    end
  end
  return function(invariant, index)
    if t ~= nil then
      t = nil
      return index + 1, value
    end
    state, value = nextvalue(invariant, state)
    if state ~= nil then
      return index + 1, value
    end
  end, invariant, 0
end

local function scan(f, initial, nextvalue, invariant, state)
  nextvalue, invariant, state = generate(nextvalue, invariant, state)
  local value
  return function(invariant, state)
    state, value = nextvalue(invariant, state)
    if state then
      initial = f(initial, value)
      return state, initial
    end
  end, invariant, state
end

local function zip(...)
  local cache = {[0] = {}}
  local invariants = {}
  local nextvalues = {}
  cache[0] = tovector(eacharg(function(argument, i)
      local nextvalue, invariant, state = generate(argument)
      invariants[i] = invariant
      nextvalues[i] = nextvalue
      return state
    end, ...))
  return function(cache, index)
    local complete = false
    local states = {}
    local values = force(tolist(each(function(state, i)
        local nextvalue = nextvalues[i]
        local invariant = invariants[i]
        states[i], value = nextvalue(invariant, state)
        if states[i] == nil then
          complete = true
        end
        return value
      end, cache[index])))
    if complete == false then
      cache[index + 1] = states
      return index + 1, values
    end
  end, cache, 0
end


local function foreach(f, nextvalue, invariant, state)
  return force(each(f, nextvalue, invariant, state))
end

return {
  vector=vector,
  dict=dict,
  pair=pair,
  cons=cons,
  list=list,
  zip=zip,
  map=map,
  fold=fold,
  show=show,
  foreach=foreach,
  pack=pack,
  resolve=resolve,
  bind=bind,
  contains=contains,
  slice=slice,
  each=each,
  tolist=tolist,
  tovector=tovector,
  id=id,
  scan=scan,
  span=span,
  last=last,
  flip=flip,
  car=car,
  cdr=cdr,
  take=take,
  drop=drop,
  filter=filter,
  keys=keys,
  search=search,
  mapcar = mapcar
}
