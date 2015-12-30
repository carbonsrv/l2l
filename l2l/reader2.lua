local itertools = require("l2l.itertools")
local exception = require("l2l.exception2")
local operator = require("l2l.operator")

local bind = itertools.bind
local car = itertools.car
local cdr = itertools.cdr
local cons = itertools.cons
local id = itertools.id
local list = itertools.list
local pack = itertools.pack
local tolist =  itertools.tolist

local raise = exception.raise


local UnmatchedRightParenException =
  exception.Exception("UnmatchedRightParenException",
    "Unmatched right parenthesis. Try remove ')'.")
local UnmatchedRightBraceException =
  exception.Exception("UnmatchedRightBraceException",
    "Unmatched right brace. Try remove '}'.")
local UnmatchedRightBracketException =
  exception.Exception("UnmatchedRightBracketException",
    "Unmatched right bracket. Try remove ']'.")
local EOFException =
  exception.Exception("EOFException",
    "End of file")
local UnmatchedReadMacroException =
  exception.Exception("UnmatchedReadMacroException",
    "Cannot find matching read macro for byte '%s'.")
local UnmatchedLeftParenException =
  exception.Exception("UnmatchedLeftParenException",
    "Unmatched left parenthesis. Possibly missing ')'.")
local UnmatchedDoubleQuoteException =
  exception.Exception("UnmatchedDoubleQuoteException",
    "Unmatched double quote. Possibly missing '\"'.")
local SemicolonException = 
  exception.Exception("SemicolonException",
    "Expected semicolon ';' to conclude lua expression.")

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

--- Returns the index of the read macro in `environment._R` that should be 
-- evaluated, given the next byte is `byte`.
-- @param environment The environment
-- @param byte The next byte.
local function matchreadmacro(environment, byte)
  -- Keep in mind this function is run a lot, everytime an expression needs
  -- to be read, which is all the time during compilation.
  if not byte then
    return nil
  end
  local _R = environment._R

  -- O(1) Single byte macros.
  if _R[byte] and #_R[byte] > 0 then
    return byte
  else
    -- O(N) Pattern macros; N = number of read macro indices.
    for pattern, _ in pairs(_R) do
      if type(pattern) == "string"  -- Ignore default macro.
          and #pattern > 1          -- Ignore single byte macro.
          and byte:match(pattern)   -- Matches pattern.
          and _R[pattern]           -- Is not `false`.
          and #_R[pattern] > 0 then -- Pattern has read macros.
        return pattern
      end
    end
  end

  -- O(N) Default macros; N = number of read macro indices.
  return 1
end

--- Evaluate a function with an environment whose read macro table is patched
-- with `R`. Assign `false` to a read macro index in `R` to hide it from `f`.
-- Example of how to remove the `"("` `read_list` read macro.:
-- ```
--    local value, rest = with_R({
--      _META={},
--      _R = default_R()
--    }, {["("] = false}, function(environment)
--      return read(environment, tolist(" (a)"))
--    end)
-- ```
-- @param environment The environment
-- @param R new _R read macro table.
-- @param inherit whether new _R will inherit the existing one.
-- @param f function that will be evaluated with `environment` as the first
--          argument.
-- @param ... Arguments that will be given to `f`.
local function with_R(environment, inherit, R, f, ...)
  local _R = environment._R
  local newR = {}
  if inherit then
    for k, v in pairs(_R) do
      newR[k] = v
    end
  end
  for k, v in pairs(R or {}) do
    newR[k] = v
  end
  environment._R = newR
  local objs, count = pack(f(environment, ...))
  environment._R = _R
  return table.unpack(objs, 1, count)
end

local default_R

local function environ(bytes)
  return {
    _R=default_R(),
    _META={
      origin=bytes,
      source=list.concat(bytes, "")
    }
  }
end

local function execute(reader, environment, bytes, ...)
  environment = environment or environ(bytes)
  local values, rest = reader(environment, bytes, ...)
  if bytes and values and rest ~= bytes then
    environment._META[bytes] = {
      read=reader,
      values=values,
      rest=rest
    }
  end
  return values, rest
end

--- Returns a list of Lua values read from next lisp expression, from `bytes`,
-- as well as list of remaining bytes that have not been read.
-- @param environment the context to read the bytes with.
-- @param bytes a list of strings, each one character long.
-- @return list of values from next lisp expression, list of remaining bytes
local function read(environment, bytes)
  environment = environment or environ(bytes)

  -- Store the entry point to the program.
  if not environment._META.origin then
    environment._META.origin = bytes
    environment._META.source = list.concat(bytes, "")
  end

  -- Reading an expression, but no bytes available, is an error.
  if not bytes then
    raise(EOFException(environment, bytes))
  end
  local rest
  local byte = car(bytes)
  local index = matchreadmacro(environment, byte)
  local _R = environment._R
  
  for _, reader in ipairs(_R[index] or {}) do
    if reader then
      local values
      values, rest = execute(reader, environment, bytes)
      -- A read macro return nil to indicate it does not match and the reader
      -- should continue.
      if values ~= nil or rest ~= bytes then
        return values, rest, reader
      end
    end
  end
  raise(UnmatchedReadMacroException(environment, bytes, byte))
end


local function read_predicate(_, bytes, transform, predicate)
  local token = ""
  local previous
  while true do
    if not bytes then
      break
    end
    local byte = bytes[1]
    previous = token
    token = token..byte
    if not predicate(token, byte) then
      token = previous
      break
    end
    bytes = bytes[2]
  end
  if #token == 0 then
    return nil, bytes
  end
  return list(transform(token)), bytes
end

-- local function read_predicate(environment, bytes, transform, predicate)
--   itertools.finalize(map(function(token, byte)
--       print(token[1], token[2])
--       os.exit()
--   end, zip(scan(operator[".."], "", bytes),
--     itertools.mapcar(id, bytes))))
-- end

--[[--
-- Slow
local function read_predicate(environment, bytes, transform, predicate)
  local tokens, rest = span(car,
    map(function(token, byte) return
        predicate(token, byte) and {token, byte} or {nil, byte}
      end, scan(operator[".."], "", bytes), bytes))
  tokens = map(car, tokens)
  rest = map(cdr, rest)
  local value = (transform or id)(last(tokens))
  return tolist({value}), rest
end
--]]--

local function read_symbol(environment, bytes)
  -- Any byte that is not defined as a read macro can be part of a symbol.
  -- ".", "-" and "[0-9]" can always be part of a symbol if it is not the first
  -- character.
  return read_predicate(environment, bytes,
    symbol, function(_, byte)
      return byte
        and (matchreadmacro(environment, byte) == 1
          or byte == "."
          or byte == "-"
          or byte:match("[0-9]"))
    end)
end

local function read_number(environment, bytes)
  local negative, rest = read_predicate(environment, bytes,
    id, bind(operator["=="], "-"))
  local numbers
  numbers, rest = read_predicate(environment, rest,
    tonumber, match("^%d+%.?%d*$", "^%d*%.?%d+$"))
  if not numbers then
    -- Not a number.
    return nil, bytes
  end
  local number = car(numbers)
  return list(negative and -number or number), rest
end

local function read_quote(environment, bytes)
  local values, rest = read(environment, cdr(bytes))
  return list(cons(symbol('quote'), values)), rest
end

local function read_right_paren(environment, bytes)
  raise(UnmatchedRightParenException(environment, bytes))
end

local function read_right_brace(environment, bytes)
  raise(UnmatchedRightBraceException(environment, bytes))
end

local function read_right_bracket(environment, bytes)
  raise(UnmatchedRightBracketException(environment, bytes))
end

local function read_whitespace(environment, bytes)
  return read_predicate(environment, bytes, id,
    match("^%s+$"))
end

--- Return next expression after whitespace.
-- @param environment The environment.
-- @param bytes List of bytes to read from.
local function skip_whitespace(environment, bytes)
  local _, rest = execute(read_whitespace, environment, bytes)
  return read(environment, rest)
end

local function read_list(environment, bytes)
  local origin = list(nil)
  local last = origin
  local rest = bytes[2]
  return with_R(environment, true, {
    -- ["."] = cons(read_attribute, environment._R["."]),
    -- [":"] = cons(read_method, environment._R[":"])
  }, function()
    local ok, values, _ = true
    while ok do
      ok, values, rest = pcall(read, environment, rest)
      if ok then
        last[2] = values
        last = last[2] or last
      elseif getmetatable(values) == UnmatchedRightParenException then
        return cons(origin[2]), cdr(values.bytes)
      else
        raise(values)
      end
    end
  end)
end

local function read_vector(environment, bytes)
  local origin = list(nil)
  local last = origin
  local rest = bytes[2]
  local ok, values, _ = true
  while ok do
    ok, values, rest = pcall(read, environment, rest)
    if ok then
      last[2] = values
      last = last[2] or last
    elseif getmetatable(values) == UnmatchedRightBracketException then
      return list(cons(symbol("vector"), origin[2])), cdr(values.bytes)
    else
      raise(values)
    end
  end
end

local function read_table(environment, bytes)
  local origin = list(nil)
  local last = origin
  local rest = bytes[2]
  local ok, values, _ = true
  while ok do
    ok, values, rest = pcall(read, environment, rest)
    if ok then
      last[2] = values
      last = last[2] or last
    elseif getmetatable(values) == UnmatchedRightBraceException then
      return list(cons(symbol("dict"), origin[2])), cdr(values.bytes)
    else
      raise(values)
    end
  end
end

local function read_string(environment, bytes)
  local text, byte = "", ""
  local escaped = false
  repeat
    if not escaped and byte == '\\' then
      escaped = true
    else
      if escaped and byte == "n" then
        byte = "\n"
      end
      text = text..byte  
      escaped = false
    end
    bytes = cdr(bytes)
    byte = bytes and car(bytes) or nil
  until not byte or (byte == '"' and not escaped)
  if not byte then
    raise(UnmatchedDoubleQuoteException(environment, bytes))
  end
  return list(text), cdr(bytes)
end

local function read_quasiquote(environment, bytes)
  local values, rest = read(environment, cdr(bytes))
  return list(cons(symbol('quasiquote'), values)), rest
end

local function read_quasiquote_eval(environment, bytes)
  local values, rest = read(environment, cdr(bytes))
  return list(cons(symbol('quasiquote-eval'), values)), rest
end


local function read_lua(environment, bytes)
  -- Syntax.
  -- ```
  -- (let
  --   (z 8)
  --   (print (<- (x y z)
  --     local x = 1;
  --     y = x + 1;
  --     z = y * 2;)))
  --   (print (<- 1 + 2)
  -- ```
  local lua = require("l2l.lua")
  local ok, values, reader, arguments, semicolon
  local rest = cdr(bytes)
  local origin = rest


  ok, values, rest, reader = pcall(skip_whitespace, environment, cdr(bytes))

  -- Either 1. `\ (a b) <block>` or 2. `\ <exp>;` is valid.

  -- If first item is a list, then it's case 1., otherwise case 2..
  if ok and values and reader == read_list then
    -- Using 1., continue reading block.
    arguments = car(values)
    values, rest = with_R(environment, false, lua.block_R(),
      function()
        return skip_whitespace(environment, rest)
      end)
  else
    -- Using 2., go back to `origin` and read again.
    arguments = nil
    values, rest = with_R(environment, false, lua.explist_R(),
      function()
        return read(environment, origin)
      end)

    origin = rest

    -- Consume ending semicolon.
    semicolon, rest = read_predicate(environment, rest, id, match("^[;]$"))
    if not semicolon then
      raise(SemicolonException(environment, origin))
    end
  end

  return values, rest
end

--- Return the default _R table.
function default_R()
  return {
    -- Single byte read macros are prioritised.
    ["\\"] = list(read_lua),
    ["("] = list(read_list),
    [")"] = list(read_right_paren),
    ['{'] = list(read_table),
    ['}'] = list(read_right_brace),
    ['['] = list(read_vector),
    [']'] = list(read_right_bracket),
    ["\""] = list(read_string),
    ['`'] = list(read_quasiquote),
    [','] = list(read_quasiquote_eval),
    ["'"] = list(read_quote),
    ["-"]=list(read_number, read_symbol),

    -- Implement skip_whitespace as a single byte read macro, because
    -- skip_whitespace is the most common read_macro evaluated and pattern
    -- macros are relatively expensive.
    [" "]=list(skip_whitespace),
    ["\t"]=list(skip_whitespace),
    ["\n"]=list(skip_whitespace),
    ["\r"]=list(skip_whitespace),
    ["\r\n"]=list(skip_whitespace),

    -- Pattern indices should not overlap.
    ["[0-9]"]=list(read_number),

    -- Default pattern
    list(read_symbol)
  }
end

if debug.getinfo(3) == nil then
  local bytes = tolist([[\ (x y z) local x=1 local \x=2 local z=\3]])
  local values, rest = read(environ(bytes), bytes)

  --(lua-binop)??
  --(lua-block ?)
  -- print(values[1][2][1][2][2][1][1]:representation())
  print(values, rest)
end

-- (. mario x y)
-- print(value, rest)

return {
  symbol = symbol,
  read = read,
  with_R = with_R,
  default_R = default_R,
  skip_whitespace = skip_whitespace,
  read_whitespace = read_whitespace,
  execute = execute,
  read_predicate = read_predicate,
  read_number = read_number,
  read_string = read_string,
  read_table = read_table,
  read_right_brace = read_right_brace,
  read_list = read_list,
  read_right_paren = read_right_paren,
  read_symbol = read_symbol,
  match=match,
  environ=environ
}

