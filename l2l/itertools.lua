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

local function fold(f, initial, objs)
  if objs == nil then
    return
  end
  for _, v in ipairs(objs or {}) do
    initial = f(initial, v)
  end 
  return initial
end

local function foreach(f, objs)
  local orig = {}
  for i, v in pairs(objs or {}) do
    orig[i] = f(v, i)
  end 
  return orig
end

local list, pair

list = setmetatable({
  unpack = function(self)
    if self then
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
  contains = function(self, obj)
    if not self then
      return false
    end
    for _, v in ipairs(self) do
      if v == obj then
        return true
      end
    end
    return false
  end,
  concat = function(self, separator)
    if self == nil then
      return ""
    end
    local str = tostring(self[1])
    if self[2] then
      str = str .. separator .. self[2]:concat(separator)
    end
    return str
  end,
  __eq = function(self, other)
    if self == nil and other == nil then
      return true
    end
    if self == nil and other ~= nil or
       self ~= nil and other  == nil then
      return false
    end
    return self[1] == other[1] and self[2] == other[2]
  end,
  __ipairs = function(self)
    local orig = self
    local i = 0
    return function() 
      if self then
        if self[2] ~= nil and getmetatable(self[2]) ~= list then
          error("cannot iterate improper list "..show(orig))
        end
        local obj = self[1]
        self = self[2]
        i = i + 1
        return i, obj 
      end 
    end
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

list.__index = list

local function id(...)
  return ...
end

local function tolist(t, obj)
  -- tolist({1, 2}, 3) == '(1 2 . 3)
  local orig = setmetatable({}, list)
  local last = orig
  local maxn = table.maxn or function(tb) return #tb end
  for i=1, maxn(t) do
    last[2] = setmetatable({t[i], nil}, list)
    last = last[2]
  end
  last[2] = obj
  return orig[2]
end

local function zip(...)
  local parameters = {}
  local smallest
  for i=1, select("#", ...) do
    local collection = select(i, ...)
    local count = 0
    for j, obj in ipairs(collection) do
      if smallest and j > smallest then
        break
      end
      if i == 1 then
        parameters[j] = {}
      end
      parameters[j][i] = obj
      count = count + 1
    end
    smallest = math.min(smallest or count, count)
  end
  local trimmed = {}
  for i = 1, smallest do
    trimmed[i] = parameters[i]
  end
  return tolist(trimmed)
end

pair = function(t)
  return setmetatable(t, list)
end

local function cons(a, b)
  return pair({a, b})
end

local function map(f, objs)
  if objs == nil then
    return nil
  end
  local orig = pair({nil})
  local last = orig
  for _, v in ipairs(objs or {}) do
    last[2] = pair({f(v), nil})
    last=last[2]
  end 
  return orig[2]
end

local function each(f, objs)
  if objs == nil then
    return nil
  end
  local orig = pair({nil})
  local last = orig
  for i, v in ipairs(objs or {}) do
    last[2] = pair({f(v, i), nil})
    last=last[2]
  end 
  return orig[2]
end

local function contains(objs, target)
  for _, v in pairs(objs or {}) do
    if v == target then
      return target
    end
  end
  return false
end

--- Returns array inclusive of start and finish indices.
-- 1 is first position. 0 is last position. -1 is second last position.
-- @objs iterable to slice.
-- @start first index.
-- @finish second index
local function slice(objs, start, finish)
  if finish <= 0 then
    finish = #objs + finish
  end

  local orig = {}
  for i, v in ipairs(objs) do
    if i >= start and i <= finish then
      table.insert(orig, v)
    end
  end
  return orig
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
  id=id
}
