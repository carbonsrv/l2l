local exception = require("l2l.exception2")
local itertools = require("l2l.itertools")
local operator = require("l2l.operator")
local raise = exception.raise

local bind = itertools.bind
local tonext = itertools.tonext
local tolist = itertools.tolist
local list = itertools.list
local cdr = itertools.cdr
local car = itertools.car
local search = itertools.search
local id = itertools.id
local queue = itertools.queue
local cons = itertools.cons
local traverse = itertools.traverse
local show = itertools.show
local match = itertools.match
local join = itertools.join

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
  if _R[byte] and search(id, _R[byte]) then
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

local function Meta(bytes, rest, children)
  return {bytes=bytes, rest=rest, children=children}
end

local function nextreadmacro(invariant, bytes)
  local matched = true
  local _R = invariant._R
  while bytes and (_R[matched] == false or matched == true) do
    local index = matchreadmacro(_R, car(bytes))
    matched = search(id, _R[index])
    if _R[matched] == false then
      bytes = matched(invariant, bytes)
    end
  end
  return bytes, matched
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


local read
local function read_predicate(bytes, transform, predicate)
  local token = ""
  local previous
  local rest = bytes
  while true do
    if not rest then
      break
    end
    local byte = rest[1]
    previous = token
    token = token..byte
    if not predicate(token, byte) then
      token = previous
      break
    end
    rest = rest[2]
  end
  if #token == 0 then
    return bytes
  end
  return rest, list(transform(token)), Meta(bytes, rest)
end

local function read_symbol(invariant, bytes)
  -- Any byte that is not defined as a read macro can be part of a symbol.
  -- ".", "-" and "[0-9]" can always be part of a symbol if it is not the first
  -- character.
  return read_predicate(bytes,
    symbol, function(_, byte)
      return byte
        and (matchreadmacro(invariant._R, byte) == 1
          or byte == "."
          or byte == "-"
          or byte:match("[0-9]"))
    end)
end

local function nextuntil(stop)
  return function(invariant, rest)
    local bytes, matched = nextreadmacro(invariant, rest)
    if matched == stop then
      return
    end
    return read(invariant, rest)
  end
end

local function read_right_paren(invariant, bytes)
  raise(UnmatchedRightParenException(invariant.first, bytes))
end

local function read_right_brace(invariant, bytes)
  raise(UnmatchedRightBraceException(invariant.first, bytes))
end

local function read_right_bracket(invariant, bytes)
  raise(UnmatchedRightBracketException(invariant.first, bytes))
end

local nextnonparen = nextuntil(read_right_paren)
local function read_list(invariant, bytes)
  local rest, values, children = traverse(nextnonparen, invariant, bytes[2])
  rest = cdr(nextreadmacro(invariant, rest))
  return rest, cons(tolist(join(values))), Meta(bytes, rest, children)
end

local nextnonbrace = nextuntil(read_right_brace)
local function read_dict(invariant, bytes)
  local rest, values, children = traverse(nextnonbrace, invariant, bytes[2])
  rest = cdr(nextreadmacro(invariant, rest))
  return rest, cons(cons(symbol("dict"), tolist(join(values)))),
    Meta(bytes, rest, children)
end

local nextnonbracket = nextuntil(read_right_bracket)
local function read_vector(invariant, bytes)
  local rest, values, children = traverse(nextnonbracket, invariant, bytes[2])
  rest = cdr(nextreadmacro(invariant, rest))
  return rest, cons(cons(symbol("vector"), tolist(join(values)))),
    Meta(bytes, rest, children)
end

local function read_whitespace(_, bytes)
  return read_predicate(bytes, id, match("^%s+$"))
end

local function read_number(invariant, bytes)
  local numbers
  local pattern = match("^%d+%.?%d*$", "^%d*%.?%d+$")
  local rest, negative = read_predicate(bytes, id, bind(operator["=="], "-"))
  rest, numbers = read_predicate(rest, tonumber, pattern)
  if not numbers then
    -- Not a number.
    return bytes
  end
  local number = car(numbers)
  return rest, list(negative and -number or number), Meta(bytes, rest)
end

local function read_string(invariant, bytes)
  if not bytes then
    return bytes
  end
  local text, byte = "", ""
  local escaped = false
  local rest = cdr(bytes)
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
    byte = rest and car(rest) or nil
    rest = cdr(rest)
  until not byte or (byte == '"' and not escaped)
  if not byte then
    raise(UnmatchedDoubleQuoteException(invariant.first, rest))
  end
  return rest, list(text), Meta(bytes, rest)
end

local function read_quasiquote(invariant, bytes)
  local rest, values, children = read(invariant, cdr(bytes))
  return rest,
    list(cons(symbol('quasiquote'), values)),
    Meta(bytes, rest,
      list.insert(children, Meta(bytes, cdr(bytes))))
end

local function read_quasiquote_eval(invariant, bytes)
  local rest, values, children = read(invariant, cdr(bytes))
  return rest,
    list(cons(symbol('quasiquote-eval'), values)),
    Meta(bytes, rest,
      list.insert(children, Meta(bytes, cdr(bytes))))
end

local function read_quote(invariant, bytes)
  local rest, values, children = read(invariant, cdr(bytes))
  return rest,
    list(cons(symbol('quote'), values)),
    Meta(bytes, rest,
      list.insert(children, Meta(bytes, cdr(bytes))))
end

-- "Mint" an `invariant` for the read function for a list or a string of bytes.
-- "An invariable environment".
-- @param bytes a list of single length strings, or a string.
local function invarymint(bytes)
  if type(bytes) == "string" then
    bytes = tolist(bytes)
  end
  return {
    _R = {
      ["("] = list(read_list),
      [")"] = list(read_right_paren),
      ['{'] = list(read_dict),
      ['}'] = list(read_right_brace),
      ['['] = list(read_vector),
      [']'] = list(read_right_bracket),
      ["\""] = list(read_string),
      ['`'] = list(read_quasiquote),
      [','] = list(read_quasiquote_eval),
      ["'"] = list(read_quote),
      ["-"]=list(read_number, read_symbol),

      -- Implement read_whitespace as a single byte read macro, because
      -- read_whitespace is the most common read_macro evaluated and pattern
      -- macros are relatively expensive.
      [" "]=list(read_whitespace),
      ["\t"]=list(read_whitespace),
      ["\n"]=list(read_whitespace),
      ["\r"]=list(read_whitespace),
      ["\r\n"]=list(read_whitespace),

      -- Mark read_whitespace as a "skipped" read macro whose values are 
      -- ignored.
      [read_whitespace] = false,

      -- Pattern indices should not overlap with any other pattern index.
      ["[0-9]"]=list(read_number),

      -- Default read macro.
      list(read_symbol)
    },
    first = bytes
  }, bytes
end

function read(invariant, bytes)
  if not bytes and getmetatable(invariant) == list
    or type(invariant) == "string" then
    invariant, bytes = invarymint(invariant)
  end

  if not invariant then
    invariant, bytes = invarymint(bytes)
  end

  -- Reading an expression, but no bytes available, is an error.
  if not bytes then
    raise(EOFException(first, bytes))
  end

  local first = invariant.first
  local _R = invariant._R
  local rest = bytes
  local matched

  while not matched or _R[matched] == false do
    local byte = car(rest)
    local index = matchreadmacro(_R, byte)
    local values, meta
    for _, read in tonext(_R[index]) do
      if read then
        rest, values, meta = read(invariant, rest)
        -- A read macro return bytes, nil to indicate it does not match and
        -- this function should continue.
        if values ~= nil or rest ~= bytes then
          matched = read
          break
        end
      end
    end
    if matched and _R[matched] ~= false then
      meta.read = read
      return rest, values, meta
    elseif not matched then
      raise(UnmatchedReadMacroException(first, bytes, byte))
    end
  end
end
if debug.getinfo(3) == nil then
  local join = itertools.join
  local rest, values, meta = show(read([[`(1 ,2)]]))
  print(values)
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
  invarymint=invarymint,
  EOFException = EOFException,
  UnmatchedReadMacroException = UnmatchedReadMacroException,
  UnmatchedRightParenException = UnmatchedRightParenException,
  UnmatchedRightBraceException = UnmatchedRightBraceException,
  UnmatchedRightBracketException = UnmatchedRightBracketException,
  UnmatchedDoubleQuoteException = UnmatchedDoubleQuoteException,
  LuaSemicolonException = LuaSemicolonException,
  LuaBlockException = LuaBlockException,
  LuaException = LuaException
}

