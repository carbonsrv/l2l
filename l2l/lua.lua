local itertools = require("l2l.itertools")
local reader = require("l2l.reader3")
local exception = require("l2l.exception2")
local show = itertools.show
local list = itertools.list
local car = itertools.car
local cdr = itertools.cdr
local cons = itertools.cons
local take = itertools.take
local drop = itertools.drop
local tolist = itertools.tolist
local map = itertools.map

local raise = exception.raise

local execute = reader.execute
local read_predicate = reader.read_predicate
local match = reader.match

local ExpectedNonTerminal =
  exception.Exception("Expected %s")


local function NonTerminal(name)
  -- Consists of one or more of the same type
  local non_terminal = setmetatable({
    representation = function(self)
      local origin = list(self.read)
      local last = origin
      for i, value in ipairs(self) do
        last[2] = cons(value:representation())
        last = last[2]
      end
      return origin
    end,
    __tostring = function(self)
      local repr = {}
      for i, value in ipairs(self) do
          table.insert(repr, itertools.show(value))
      end
      table.insert(repr, "")
      return table.concat(repr, "")
    end,
    __eq = function(self, other)
      return getmetatable(self) == getmetatable(other) and
        tostring(self) == tostring(other)
    end
  }, {__call = function(non_terminal, ...)
      return setmetatable({name=name, is_terminal=false, ...}, non_terminal)
    end,
    __tostring = function(self)
      return name
    end})

  non_terminal.__index = non_terminal
  return non_terminal
end

-- Consists of one or more of the same type
local Terminal = setmetatable({
  representation = function(self)
    return show(self)
  end,
  __tostring = function(self)
    return tostring(self[1])
  end,
  __eq = function(self, other)
    return getmetatable(self) == getmetatable(other) and
      tostring(self) == tostring(other)
  end
}, {__call = function(Terminal, value)
    return setmetatable({value, is_terminal=true}, Terminal)
  end})

Terminal.__index = Terminal

local SKIP = "SKIP"
local OPT = "OPT"
local REPEAT = "REPEAT"

local READ

local function is(reader, flag)
  if getmetatable(reader) ~= READ then
    return false
  end
  return reader[flag]
end

READ = setmetatable({
  representation = function(self)
    return tostring(self)
  end,
  __call = function(self, environment, bytes)
    return car(self)(environment, bytes)
  end,
  __tostring = function(self)
    local text = tostring(car(self))
    if is(self, OPT) then
      text = "["..text.."]"
    end
    if is(self, REPEAT) then
      text = "{"..text.."}"
    end
    if is(self, SKIP) then
      text = ""
    end
    return text
  end
}, {
  __call = function(READ, reader, ...)
    local self = setmetatable({reader}, READ)
    for i, value in ipairs({...}) do
      self[value] = true
    end
    return self
  end
})

READ.__index = READ

local SET = setmetatable({
  __call = function(self, environment, bytes)
    local values, rest = car(self)(environment, bytes)
    return map(function(value)
        if type(value) ~= "table" and not value.representation then
          value = Terminal(value)
        end
        return value
      end, values), rest
  end,
  __tostring = function(self)
    return tostring(self.target)
  end
}, {
  __call = function(SET, target, reader, ...)
    assert(target)
    local self = setmetatable({reader, target=target}, SET)
    target.read = self
    return self
  end
})
SET.__index = SET

local ANY = setmetatable({
  __call = function(self, environment, bytes)
    for i, reader in ipairs(self) do
      assert(not is(reader, OPT))
      assert(not is(reader, SKIP))
      assert(not is(reader, REPEAT))
      assert(reader)
      local ok, values, rest = pcall(execute, reader, environment, bytes)
      if ok and values and rest ~= bytes then
        return values, rest
      end
      if not ok and getmetatable(values) ~= ExpectedNonTerminal then
        raise(values)
      end
    end
    return nil, bytes
  end,
  __tostring = function(self)
    local repr = {"ANY("}
    for i, value in ipairs(self) do
        table.insert(repr, itertools.show(value))
        if i ~= #self then
          table.insert(repr, ",")
        end
    end
    table.insert(repr, ")")
    return table.concat(repr, "")
  end
}, {
  __call = function(ANY, ...)
    return setmetatable({...}, ANY)
  end
})


local ALL = setmetatable({
  __call = function(self, environment, bytes)
    local values, rest, all, ok = nil, bytes, {}
    for i, reader in ipairs(self) do
       while true do
        assert(reader)
        if is(reader, OPT) or is(reader, REPEAT) then
          local prev = rest
          ok, values, rest = pcall(execute, reader, environment, rest)
          if not ok then
            rest = prev -- restore to previous point.
            if getmetatable(values) == ExpectedNonTerminal then
              break
            else
              raise(values)
            end
          end
        else
          values, rest = execute(reader, environment, rest)
        end
        if not values then
          if is(reader, REPEAT) then
            break
          elseif not is(reader, OPT) then
            return nil, bytes
          end
        end
        if not is(reader, SKIP) then
          for j, value in ipairs(values or {}) do
            table.insert(all, value)
          end
        end
        if not is(reader, REPEAT) then
          break
        end
      end
    end
    return tolist(all), rest
  end,
  __tostring = function(self)
    local repr = {"ALL("}
    for i, value in ipairs(self) do
        table.insert(repr, itertools.show(value))
        if i ~= #self then
          table.insert(repr, ",")
        end
    end
    table.insert(repr, ")")
    return table.concat(repr, "")
  end
}, {
  __call = function(ALL, ...)
    return setmetatable({...}, ALL)
  end
})

local function read_terminal(terminal)
  local value = tostring(terminal)
  local reader = SET(terminal, function(environment, bytes)
    if list.concat(take(#value, bytes)) == value then
      return list(terminal), drop(#value, bytes)
    end
    return nil, bytes
  end)
  return reader
end

local function read_nonterminal(nonterminal, factory)
  assert(factory, "missing `origin` argument")
  local reader = SET(nonterminal, function(environment, bytes)
    local origin = factory()
    local values, rest = execute(origin, environment, bytes)
    if #({list.unpack(values)}) == 0 then
      raise(ExpectedNonTerminal(environment, bytes, origin))
    end
    return list(nonterminal(list.unpack(values))), rest
  end)
  return reader
end

local function TERM(text)
  return read_terminal(Terminal(text))
end

-- Lua Grammar
-- chunk ::= block
-- block ::= {stat} [retstat]
-- stat ::=  ‘;’ | 
--      varlist ‘=’ explist | 
--      functioncall | 
--      label | 
--      break | 
--      goto Name | 
--      do block end | 
--      while exp do block end | 
--      repeat block until exp | 
--      if exp then block {elseif exp then block} [else block] end | 
--      for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end | 
--      for namelist in explist do block end | 
--      function funcname funcbody | 
--      local function Name funcbody | 
--      local namelist [‘=’ explist] 

-- retstat ::= return [explist] [‘;’]
-- label ::= ‘::’ Name ‘::’
-- funcname ::= Name {‘.’ Name} [‘:’ Name]
-- varlist ::= var {‘,’ var}
-- var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 
-- namelist ::= Name {‘,’ Name}
-- explist ::= exp {‘,’ exp}
-- exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef | 
--      prefixexp | tableconstructor | exp binop exp | unop exp 
-- prefixexp ::= var | functioncall | ‘(’ exp ‘)’
-- functioncall ::=  prefixexp args | prefixexp ‘:’ Name args 
-- args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString 
-- functiondef ::= function funcbody
-- funcbody ::= ‘(’ [parlist] ‘)’ block end
-- parlist ::= namelist [‘,’ ‘...’] | ‘...’
-- tableconstructor ::= ‘{’ [fieldlist] ‘}’
-- fieldlist ::= field {fieldsep field} [fieldsep]
-- field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
-- fieldsep ::= ‘,’ | ‘;’
-- binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ | 
--      ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ | 
--      ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ | 
--      and | or
-- unop ::= ‘-’ | not | ‘#’ | ‘~’

unop = Terminal('-')
_elseif = Terminal('elseif')
_if = Terminal('if')
_return = Terminal('return')
semicolon = Terminal(";")
coloncolon = Terminal('::')

Name = NonTerminal("Name")
label = NonTerminal("label")
funcname = NonTerminal("funcname")
varlist = NonTerminal("varlist")
exp = NonTerminal("exp")
stat = NonTerminal("stat")
block = NonTerminal("block")
retstat = NonTerminal("retstat")
whitespace = NonTerminal("whitespace")
_goto = NonTerminal("goto")
_while = NonTerminal("while")
exp = NonTerminal("exp")
prefixexp = NonTerminal("prefixexp")

local read_semicolon = read_terminal(semicolon)
local read_coloncolon = read_terminal(coloncolon)
local read_return = read_terminal(_return)

local operator = require("l2l.operator")
local scan = itertools.scan
local span = itertools.span
local last = itertools.last
local id = itertools.id
local match = reader.match


local read_whitespace = SET(whitespace, function(environment, bytes)
-- * Mandatory read_whitespace after keywords should not be "SKIP"ed.
--   Otherwise when output the code could produce like "gotolabel",
--   which is not valid.
-- * In "ALL", read_whitespace prepend content elements that can have
--   whitespaces prepending it.
-- * At the end of "ALL", there should be a read_whitespace appending the list
--   if the nonterminal in question can be appended with whitespace.
-- * That should take care of whitespace before and after each element. Avoid 
--   read_whitespace elsewhere.
  local patterns = {}
  local bounds = {
    ["("]=true, 
    [","]=true,
    [";"]=true,
  }
  for byte, _ in pairs(bounds) do
    table.insert(patterns, "^%"..byte.."()$")
  end

  local values, rest = read_predicate(environment, id,
    match("^%s+$", unpack(patterns)), bytes)

  -- Convert boundary characters into zero string tokens.
  if values and bounds[car(values)] then
    return list(""), bytes
  end
  return values, rest
end)

local read_Name  = SET(Name, function(environment, bytes)
  -- Names (also called identifiers) in Lua can be any string of letters,
  -- digits, and underscores, not beginning with a digit and not being a
  -- reserved word. Identifiers are used to name variables, table fields, and
  -- labels.
  return read_predicate(environment,
    tostring, function(token, byte)
      return (token..byte):match("^[%w_][%w%d_]*$")
    end, bytes)
end)

local read_label = read_nonterminal(NonTerminal("label"),
  -- label ::= ‘::’ Name ‘::’
  function() return ALL(
    TERM('::'),
    READ(read_whitespace, SKIP, OPT),
    READ(read_Name),
    READ(read_whitespace, SKIP, OPT),
    TERM('::')
  ) end)

local read_goto = read_nonterminal(_goto,
  -- goto Name
  function() return ALL(
    TERM('goto'),
    READ(read_whitespace),
    read_Name
  ) end)

local read_exp
local read_prefixexp = read_nonterminal(prefixexp,
  -- prefixexp ::= var | functioncall | ‘(’ exp ‘)’
  function() return ALL(
    TERM("("),
    READ(read_whitespace, SKIP, OPT),
    read_exp,
    TERM(")"),
    READ(read_whitespace, SKIP, OPT)
  ) end)

read_exp = read_nonterminal(exp,
  -- exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ |  
  --      functiondef | prefixexp | tableconstructor | exp binop exp | unop exp 
  function() return ALL(
    ANY(
      TERM("nil"),
      READ(read_prefixexp)
    ),
    READ(read_whitespace, SKIP, OPT)
  ) end)

local read_block
local read_while = read_nonterminal(_while,
  -- while exp do block end
  function() return ALL(
    TERM("while"),
    READ(read_whitespace, SKIP),
    READ(read_exp),
    TERM("do"),
    READ(read_whitespace), -- required to prevent "doreturn"
    READ(read_block, OPT),
    TERM("end")
  ) end)

local read_stat = read_nonterminal(stat,
  -- stat ::=  ‘;’ | 
  --      varlist ‘=’ explist | 
  --      functioncall | 
  --      label | 
  --      break | 
  --      goto Name | 
  --      do block end | 
  --      while exp do block end | 
  --      repeat block until exp | 
  --      if exp then block {elseif exp then block} [else block] end | 
  --      for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end | 
  --      for namelist in explist do block end | 
  --      function funcname funcbody | 
  --      local function Name funcbody | 
  --      local namelist [‘=’ explist] 
  function() return ANY(
    TERM(';'),
    read_label,
    TERM('break'),
    read_goto,
    read_while
  ) end)


-- read_stat = read_nonterminal(
--   ANY(
--     read_semicolon,
--     ALL(read_varlist, read_equals, read_explist),
--     read_functioncall,
--     read_label,
--     read_break,
--     ALL(read_goto, read_Name),
--     ALL(read_do, read_block, read_end)
-- )

local read_retstat = read_nonterminal(retstat,
  function() return ALL(
    read_return,
    READ(read_whitespace, SKIP),
    READ(read_exp, OPT), --should be explist
    READ(read_semicolon, OPT)
  ) end)

read_block = read_nonterminal(block,
  function() return ALL(
    READ(read_whitespace, SKIP, OPT),
    READ(read_stat, REPEAT),
    READ(read_whitespace, SKIP, OPT),
    READ(read_retstat, OPT)
  ) end)

--- Return the default _R table.
local function block_R()
  return {
    -- put block level stuff here? or in read_block's own _R
    list(read_block)
  }
end

return {
    block_R = block_R
}
