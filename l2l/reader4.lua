local exception = require("l2l.exception3")
local itertools = require("l2l.itertools")
local operator = require("l2l.operator")
local raise = exception.raise

local tolist = itertools.tolist

local EOFException =
  exception.Exception("EOFException",
    "End of file")
local UnmatchedReadMacroException =
  exception.Exception("UnmatchedReadMacroException",
    "Cannot find matching read macro for byte '%s'.")
local UnmatchedRightParenException =
  exception.Exception("UnmatchedRightParenException",
    "Unmatched right parenthesis. Try remove ')'.")
local UnmatchedRightBraceException =
  exception.Exception("UnmatchedRightBraceException",
    "Unmatched right brace. Try remove '}'.")
local UnmatchedRightBracketException =
  exception.Exception("UnmatchedRightBracketException",
    "Unmatched right bracket. Try remove ']'.")
local UnmatchedDoubleQuoteException =
  exception.Exception("UnmatchedDoubleQuoteException",
    "Unmatched double quote. Possibly missing '\"'.")
local LuaSemicolonException =
  exception.Exception("LuaSemicolonException",
    "Expected semicolon ';' to conclude lua expression.")
local LuaBlockException =
  exception.Exception("LuaBlockException",
    "Expected lua block after `in`.")
local LuaException =
  exception.Exception("LuaException",
    "Expected lua expression or block after `\\`.")

--- Returns the index of the read macro in `_R` that should be 
-- evaluated, given the next byte is `byte`.
-- @param _R The read macro table.
-- @param byte The next byte.
local function matchreadmacro(_R, byte)
  -- Keep in mind this function is run a lot, everytime an expression needs
  -- to be read, which is all the time during compilation.
  if not byte or not _R then
    return nil
  end

  -- O(1) Single byte macros.
  if _R[byte] then
    for i=1, #_R[byte] do
      if _R[byte][i] then
        return byte, _R[byte]
      end
    end
  else
    -- O(N) Pattern macros; N = number of read macro indices.
    for pattern, _ in pairs(_R) do
      if type(pattern) == "string"  -- Ignore default macro.
          and #pattern > 1          -- Ignore single byte macro.
          and string.char(byte):match(pattern)   -- Matches pattern.
          and _R[pattern]           -- Is not `false`.
          and #_R[pattern] > 0 then -- Pattern has read macros.
        return pattern
      end
    end
  end

  -- O(N) Default macros; N = number of read macro indices.
  return 1, _R[1]
end

-- Returns byte at `position` in `invariant.src`, and the next position.
local function byteat(invariant, position)
  return string.char(invariant.src:byte(position))
end

local concat = itertools.concat
local function Meta(read, index, length, children)
  return {read, index, length, children}
end

-- Create a new type `symbol`.
local symbol = setmetatable({
  __tostring = function(self)
    return tostring(self[1])
  end,
  __eq = function(self, other)
    return getmetatable(self) == getmetatable(other) and
      tostring(self) == tostring(other)
  end
}, {__call = function(symbol, name)
    return setmetatable({name}, symbol)
  end})

symbol.__index = symbol


local read, readifnot
local function read_predicate(invariant, position, skip, transform, predicate)
  error("not implemented")
  local token = ""
  local length = #invariant.src
  for i=position-1, length do
    local nextindex = i + 1
    local token = invariant.src:sub(position, nextindex)
    local byte = byteat(invariant, i + 1)
    if not predicate(token, byte) or i == length then
      if nextindex == position or skip then
        return nextindex
      else
        return nextindex, {transform(previous)},
          {Meta(position, nextindex - position)}
      end
    end
    previous = token
  end
end

local function read_symbol(invariant, position, skip, strip)
  -- Any byte that is not defined as a read macro can be part of a symbol.
  -- ".", "-" and "[0-9]" can always be part of a symbol if it is not the first
  -- character.
  local byte = byteat(invariant, position)
  return read_predicate(invariant, position, skip,
    symbol, function(_, byte)
      return byte
        and (matchreadmacro(invariant._R, byte) == 1
          or byte == "."
          or byte == "-"
          or byte:match("[0-9]"))
    end)
end

local function read_until(invariant, position, skip, strip, stop, n)
  local count, metas, values, meta, value = n
  local v, m
  if not skip then
    values = {}
    if not strip then
      metas = {}
    end
    for i=1, n do
      values[i] = false
      if not strip then
        metas[i] = false
      end
    end
  end

  local i, ok = position
  while i do
    ok, i, value, meta = readifnot(invariant, i, skip, strip, stop)
    if ok == false then
      break
    end
    if not skip then
      if value and type(value) ~= "table" then
        count = count + 1
        values[count] = value
        if not strip then
          metas[count] = meta
        end
      else
        for j=1, #value do
          count = count + 1
          values[count] = value[j]
          if not strip then
            metas[count] = meta[j]
          end
        end
      end
    end
  end
  return i, values, metas
end

local function read_right_paren(invariant, position)
  raise(UnmatchedRightParenException(invariant.src, position))
end

local function read_right_brace(invariant, position)
  raise(UnmatchedRightBraceException(invariant.src, position))
end

local function read_right_bracket(invariant, position)
  raise(UnmatchedRightBracketException(invariant.src, position))
end

-- Private function
local function wraps(name, stop)
  local function wrapped(invariant, position, skip, strip)
    local i, values, metas = read_until(invariant, position, skip,
      stop, strip, 1)
    if skip then
      return i
    end
    values[1] = symbol("name")
    if strip then
      return i + 1, {values}
    end
    metas[1] = {wrapped, position, 1}
    return i + 1, {values}, {metas}
  end
  return wrapped
end

local function read_list(invariant, position, skip, strip)
  local i, values, metas = read_until(invariant, position + 1, skip, strip,
    read_right_paren, 0)
  if skip then
    return i
  end
  if strip then
    return i + 1, {values}
  end
  return i + 1, {values}, {{read_list, position, i + 1 - position, metas}}
end

local read_vector = wraps("vector", read_right_bracket)
local read_dict = wraps("dict", read_right_brace)


local whitespace = {
  [string.byte(" ")] = true,
  [string.byte("\t")] = true,
  [string.byte("\r")] = true,
  [string.byte("\n")] = true
}
local function read_whitespace(invariant, position, skip, strip)
  local src, rest = invariant.src
  for i=position, #src do
    local byte = src:byte(i)
    if not whitespace[byte] then
      rest = i
      break
    end
  end
  if skip then
    return rest
  end
  local value = invariant.src:sub(position, rest-1)  
  if strip then
    return rest, value
  end
  return rest, value, {read_whitespace, position, rest - position}
end

local minus = ("-"):byte()
local function read_number(invariant, position, skip, strip)
  local pattern = number_pattern
  local sign, numbers = position
  local src = invariant.src
  if src:byte(position) == minus then
    sign = position + 1
  end
  local length = #src
  local dot, zero, nine = 46, 48, 57
  local decimal = false
  local rest
  for i=sign, length do
    local byte = src:byte(i)
    if i ~= sign and byte == dot and decimal == false then
      decimal = true
    elseif not (byte >= zero and byte <= nine) then
      rest = i
      break
    end
  end
  if rest == sign then
    -- Not a number
    return position
  end
  if skip then
    return rest
  end
  local value = tonumber(src:sub(position, rest))
  if strip then
    return rest, value
  end
  return rest, value, {read_number, position, rest-position}
end

local function read_string(invariant, position, skip, strip)
  local length = #invariant.src
  if position > length then
    return position
  end
  local escaped = false
  local quotechar = byteat(invariant, position)
  local byte
  for i=position + 1, length do
    if not escaped and byte == "\\" then
      escaped = true
    else
      if escaped and byte == "n" then
        byte = "\n"
      end
      escaped = false
    end
    byte = byteat(invariant, i)
    if byte == quotechar and not escaped then
      if skip then
        return i + 1
      end
      local value = invariant.src:sub(position, i)
      if strip then
        return i + 1, value
      else
        return i + 1, value, {read_string, position, i + 1 - position}
      end
    end
  end
  raise(UnmatchedDoubleQuoteException(invariant.src, position))
end

local function wrap(name)
  local function wrapped(invariant, position, skip, strip)
    local rest, values, metas = read(invariant, position + 1, skip, strip)
    if skip then
      return rest
    end
    table.insert(values, 1, symbol(name))
    if strip then
      return rest, values
    end
    table.insert(metas, 1, {wrapped, position, 1})
    return rest, values, {{wrapped, position, rest-position, metas}}
  end
  return wrapped
end

local read_quasiquote = wrap("quasiquote")
local read_quasiquote_eval = wrap("quasiquote-eval")
local read_quote = wrap("quote")


-- Create an `invariant` for the read function for a string.
-- E.g. read(environ('[1 2]'))
-- @param src a string.
local function environ(src, position)
  return {
    _R = {
      [string.byte("\\")] = {read_lua},
      [string.byte("(")] = {read_list},
      [string.byte(")")] = {read_right_paren},
      [string.byte('{')] = {read_dict},
      [string.byte('}')] = {read_right_brace},
      [string.byte('[')] = {read_vector},
      [string.byte(']')] = {read_right_bracket},
      [string.byte("\"")] = {read_string},
      [string.byte('`')] = {read_quasiquote},
      [string.byte(',')] = {read_quasiquote_eval},
      [string.byte("'")] = {read_quote},
      [string.byte("-")]={read_number, read_symbol},

      -- Implement read_whitespace as a single byte read macro, because
      -- read_whitespace is the most common read_macro evaluated and pattern
      -- macros are relatively expensive.
      [string.byte(" ")]={read_whitespace},
      [string.byte("\t")]={read_whitespace},
      [string.byte("\n")]={read_whitespace},
      [string.byte("\r")]={read_whitespace},

      -- Mark read_whitespace as a "skipped" read macro whose values are 
      -- ignored.
      [read_whitespace] = false,

      [string.byte("1")]={read_number},

      -- Pattern indices should not overlap with any other pattern index.
      ["[0-9]"]={read_number},

      -- Default read macro.
      {read_symbol}
    },
    src = src
  }, position or 1
end

function readifnot(invariant, position, skip, strip, stop)
  if type(invariant) == "string" then
    invariant, position = environ(invariant, position)
  end

  local _R, src = invariant._R, invariant.src
  local rest, len, macro = position, #src

  -- Reading an expression, but no bytes available, is an error.
  if position > len then
    raise(EOFException(src, position))
  end

  while (not macro or _R[macro] == false) and rest <= len do
    local index = matchreadmacro(_R, src:byte(rest))
    local values, metas

    local origin = rest
    for i=1, #_R[index] do
      macro = _R[index][i]
      if stop == macro then
        return false, rest
      end
      rest, values, metas = macro(invariant, rest,
        _R[macro] == false or skip, strip)
      -- A read macro return `position` to indicate it does not match
      -- and this function should continue.
      if rest ~= origin then
        break
      end
    end
    if rest ~= origin then
      if _R[macro] ~= false then
        return true, rest, values, metas
      end
    else
      raise(UnmatchedReadMacroException(src, position,
        byteat(invariant, position)))
    end
  end
  raise(EOFException(src, len))
end

read = function(invariant, position, skip, strip)
  return select(2, readifnot(invariant, position, skip, strip))
end

if debug.getinfo(3) == nil then
  local ones = ("1 "):rep(13500000)
  print(#ones)
  local invariant, position = environ('`('..ones..')')
  -- local p = require("l2l.profile")
  --   p.profile(function()
  local rest, values, metas = read(invariant, position, false, true)
  -- print(tolist(values), tolist(metas))
  -- end)
end

return {
  symbol = symbol,
  read = read,
  read_whitespace = read_whitespace,
  read_predicate = read_predicate,
  read_number = read_number,
  read_string = read_string,
  read_dict = read_dict,
  read_right_brace = read_right_brace,
  read_list = read_list,
  read_right_paren = read_right_paren,
  read_symbol = read_symbol,
  environ=environ,
  EOFException = EOFException,
  UnmatchedReadMacroException = UnmatchedReadMacroException,
  UnmatchedRightParenException = UnmatchedRightParenException,
  UnmatchedRightBraceException = UnmatchedRightBraceException,
  UnmatchedRightBracketException = UnmatchedRightBracketException,
  UnmatchedDoubleQuoteException = UnmatchedDoubleQuoteException,
  LuaSemicolonException = LuaSemicolonException,
  LuaBlockException = LuaBlockException,
  LuaException = LuaException,
  Meta=Meta
}

