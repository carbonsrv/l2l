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
local filter = itertools.filter
local contains = itertools.contains
local keys = itertools.keys
local flip = itertools.flip
local bind = itertools.bind
local slice = itertools.slice

local raise = exception.raise

local execute = reader.execute

local read_predicate = reader.read_predicate
local read_number = reader.read_number
local match = reader.match


local ExpectedNonTerminalException =
  exception.Exception("ExpectedNonTerminalException",
    "Expected %s")
local ParseException =
  exception.Exception("ParseException",
    "An exception occurred while parsing `%s`:\n  %s")
local GrammarException =
  exception.Exception("GrammarException",
    "An exception occurred while generating `%s`:\n  %s")


local function NonTerminal(name)
  -- Consists of one or more of the same type
  local non_terminal = setmetatable({
    representation = function(self)
      local origin = list(self.read)
      local last = origin
      for i, value in ipairs(self) do
        local repr = type(value) == "string" and value or value:representation()
        last[2] = cons(repr)
        last = last[2]
      end
      return origin
    end,
    is_valid = function(self)
      return pcall(execute, self.read, nil, tolist(tostring(self)))
    end,
    __tostring = function(self)
      local repr = {}
      for i, value in ipairs(self) do
          table.insert(repr, tostring(value))
      end
      table.insert(repr, "")
      return table.concat(repr, "")
    end,
    __eq = function(self, other)
      return getmetatable(self) == getmetatable(other) and
        tostring(self) == tostring(other)
    end
  }, {__call = function(non_terminal, ...)
      return setmetatable({
        name=name,
        is_terminal=false,
        ...}, non_terminal)
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
    return self
  end,
  is_valid = function(self)
    return pcall(execute, self.read, nil, tolist(tostring(self)))
  end,
  __tostring = function(self)
    return tostring(self[1])
  end,
  __eq = function(self, other)
    return getmetatable(self) == getmetatable(other) and
      tostring(self) == tostring(other)
  end
}, {__call = function(Terminal, value)
    return setmetatable({
      value,
      is_terminal=true}, Terminal)
  end})

Terminal.__index = Terminal

local SKIP = "SKIP"
local PEEK = "PEEK"
local OPT = "OPT"
local REPEAT = "REPEAT"

local READ, ALL, ANY

local function is(reader, flag)
  local rule = getmetatable(reader)
  if flag == nil then -- verify `reader` is a rule
    return rule == ALL or rule == ANY or rule == READ
  end
  if flag == ALL or flag == ANY or flag == READ then
    return rule == flag
  end
  assert(contains({SKIP, PEEK, OPT, REPEAT}, flag))
  if rule ~= READ then
    return false
  end
  return reader[flag]
end

READ = setmetatable({
  representation = function(self)
    return tostring(self)
  end,
  __call = function(self, environment, bytes, targets)
    return self[1](environment, bytes, targets)
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
      text = "~"
    end
    return text
  end
}, {
  __call = function(READ, reader, ...)
    local self = setmetatable({reader}, READ)
    assert(reader, "missing `reader` argument.")
    for i, value in ipairs({...}) do
      self[value] = true
    end
    return self
  end,
  __tostring = function()
    return "READ"
  end
})

READ.__index = READ

local SET = setmetatable({
  __call = function(self, environment, bytes, targets)  
    -- CALL THIS WITH THE NONTERMINAL STATE? SO I CAN CHECK WHERE ITS LOOPING FROM.
    -- SOME SORT OF STACK?
    return self[1](environment, bytes, list.push(targets, self.target))
  end,
  __tostring = function(self)
    return tostring(self.target)
  end
}, {
  __call = function(SET, target, reader, factory)
    assert(target)
    local self = setmetatable({reader, target=target}, SET)
    target.read = self
    target.factory = factory
    return self
  end,
  __tostring = function()
    return "SET"
  end
})
SET.__index = SET

ANY = setmetatable({
  __call = function(self, environment, bytes, targets)
    for i, reader in ipairs(self) do
      if reader ~= nil then
        assert(not is(reader, OPT))
        assert(not is(reader, SKIP))
        assert(not is(reader, REPEAT))
        assert(reader)
        local ok, values, rest = pcall(execute, reader, environment, bytes,
          targets)
        if ok and values and rest ~= bytes then
          return values, rest
        end
        if not ok and getmetatable(values) ~= ExpectedNonTerminalException then
          raise(values)
        end
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
    return setmetatable({list.unpack(filter(id, list(...)))}, ANY)
  end,
  __tostring = function()
    return "ANY"
  end
})


ALL = setmetatable({
  __call = function(self, environment, bytes, targets)
    local values, rest, all, ok = nil, bytes, {}
    for i, reader in ipairs(self) do
      if reader ~= nil then
        while true do
          assert(reader)
          local prev = rest
          local prev_meta = environment._META[rest]
          if is(reader, OPT) or is(reader, REPEAT) then
            ok, values, rest = pcall(execute, reader, environment, rest,
              targets)
            if not ok then
              rest = prev -- restore to previous point.
              if getmetatable(values) == ExpectedNonTerminalException then
                break
              else
                raise(values)
              end
            end
          else
            values, rest = execute(reader, environment, rest, targets)
          end
          if not values then
            if is(reader, REPEAT) then
              break
            elseif not is(reader, OPT) then
              return nil, bytes
            end
          end
          if is(reader, PEEK) then
            -- Restore any metadata at this point
            -- We don't want a PEEK operation to affect state.
            -- We didn't consume any input, it should not be recorded as we
            -- have.
            environment._META[prev] = prev_meta
            rest = prev
          elseif not is(reader, SKIP) then
            targets = list()
            for j, value in ipairs(values or {}) do
              table.insert(all, value)
            end
          end

          if not is(reader, REPEAT) then
            break
          end
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
    return setmetatable({list.unpack(filter(id, list(...)))}, ALL)
  end,
  __tostring = function()
    return "ALL"
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

local function read_nonterminal(nonterminal, factory, const)
  assert(factory, "missing `factory` argument")
  local origin, ok
  local reader = SET(nonterminal, function(environment, bytes, targets)
    if not origin or not const then
      ok, origin = pcall(factory, environment, bytes, targets)
      if not ok then
        local err = origin
        raise(GrammarException(environment, bytes, nonterminal, err))
      end
    end
    -- print("[", nonterminal, bytes)
    local ok, values, rest = pcall(execute, origin, environment, bytes,
      targets)
    -- print("]", nonterminal, bytes, ok, values)
    if not ok then
      local err = values
      if getmetatable(values) == ExpectedNonTerminalException then
        raise(err)
      elseif getmetatable(values) == ParseException then
        raise(err)
      else
        raise(ParseException(environment, bytes, nonterminal, err))
      end
    end
    if #({list.unpack(values)}) == 0 then
      raise(ExpectedNonTerminalException(environment, bytes, origin))
    end
    return list(nonterminal(list.unpack(values))), rest
  end, factory)
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


local keywords ={
  ["and"] = true, 
  ["break"] = true, 
  ["do"] = true, 
  ["else"] = true, 
  ["elseif"] = true, 
  ["end"] = true,
  ["for"] = true, 
  ["function"] = true, 
  ["if"] = true, 
  ["in"] = true, 
  ["local"] = true, 
  ["not"] = true, 
  ["or"] = true, 
  ["repeat"] = true, 
  ["return"] = true, 
  ["then"] = true, 
  ["until"] = true, 
  ["while"] = true
}


_elseif = Terminal("elseif")
_if = Terminal("if")

--[[[
${x} means hash
$ => hash next lisp symbol into Name.
${} => quasiquote eval???

(print ${<div>Hello</div>})
(print $.{1 + 2})
(print ${ x << local x = 1 + $z + $(+ 7 8)})


(let
  (z 8)
  (print ${x <- local x = 1 + $z + $(+ 7 8)}) ;; Prints 9
  (print `${x <- local x = 1 + ${z} + $z))

(let 
  (z-x 7)
  (print ${} (x) <<
  local $x = 0;
  local $y = 7;
   x = x + y + $z-x;))

(print $ 7 + 8; (+ 1 3))
(print `${print("hello"..${7 + 8})})

(print (quote (LuaExp (LuaFunctionCall (LuaName "print") Terminal("(") ))))....

LuaName
LuaLabel
LuaFunctionName
LuaVariableList
LuaExpression
LuaStatement
LuaBlock
LuaReturnStatement
LuaWhitespace
LuaGoto
LuaWhile
LuaPrefixExpression
LuaExpressionList
LuaUnaryOperation
LuaLabel
LuaNumber
LuaVariable
--?>

]]--

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
prefixexp = NonTerminal("prefixexp")
explist = NonTerminal("explist")
unop = NonTerminal("unop")
label = NonTerminal("label")
number = NonTerminal("number")
var = NonTerminal("var")
functioncall = NonTerminal("functioncall")
_args = NonTerminal("args")

read_number = SET(number, function(environment, bytes)
  local values, rest = reader.read_number(environment, bytes)
  if values then
    return list(Terminal(car(values))), rest
  end
  return nil, bytes
end)


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
  local patterns = {}
  local bounds = {
    ["("]=true,
    [")"]=true,
    ["{"]=true,
    ["}"]=true,
    [","]=true,
    [";"]=true,
  }

  if not bytes then
    return list(whitespace("")), bytes
  end

  for byte, _ in pairs(bounds) do
    table.insert(patterns, "^%"..byte.."$")
  end

  local values, rest = read_predicate(environment, whitespace,
    match("^%s+$", unpack(patterns)), bytes)

  -- Convert boundary characters into zero string tokens.
  if values and bounds[tostring(car(values))] then
    return list(whitespace("")), bytes
  end
  return values, rest
end)

local read_Name  = SET(Name, function(environment, bytes)
  -- print("[", "read_Name", bytes)
  -- Names (also called identifiers) in Lua can be any string of letters,
  -- digits, and underscores, not beginning with a digit and not being a
  -- reserved word. Identifiers are used to name variables, table fields, and
  -- labels.
  local values, rest = read_predicate(environment,
    Name, function(token, byte)
      return (token..byte):match("^[%w_][%w%d_]*$")
    end, bytes)
  if values and car(values) and keywords[tostring(car(values))] then
    return nil, bytes
  end
  -- print("]", "read_Name", bytes, true, values)
  return values, rest
end)

local read_unop = read_nonterminal(unop,
  -- unop ::= ‘-’ | not | ‘#’ | ‘~’
  function() return ANY(
    TERM("-"),
    ALL(TERM("not"), read_whitespace),
    TERM("#"),
    TERM("~")
  ) end, true)

local read_label = read_nonterminal(label,
  -- label ::= ‘::’ Name ‘::’
  function() return ALL(
    TERM("::"),
    READ(read_whitespace, SKIP, OPT),
    READ(read_Name),
    READ(read_whitespace, SKIP, OPT),
    TERM("::")
  ) end, true)

local read_goto = read_nonterminal(_goto,
  -- goto Name
  function() return ALL(
    TERM("goto"),
    READ(read_whitespace),
    read_Name
  ) end, true)

local read_exp
local read_var
local read_prefixexp
local read_block
local read_explist


local read_args = read_nonterminal(_args,
  -- args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString 
  function() return ANY(
    ALL(
      TERM("("),
      READ(read_whitespace, SKIP, OPT),
      READ(read_explist, OPT),
      READ(read_whitespace, SKIP, OPT),
      TERM(")"))
  ) end)

local read_functioncall = read_nonterminal(functioncall,
  -- functioncall ::=  prefixexp args | prefixexp ‘:’ Name args 
  function(environment, bytes, targets)
    local has_prefixexp, has_functioncall
    -- if environment._META[bytes] then
    --   if environment._META[bytes].read == read_prefixexp then
    --     has_prefixexp = true
    --   end
    -- end
    -- if environment._META[bytes] then
    --   if environment._META[bytes].read == read_functioncall then
    --     has_functioncall = true
    --   end
    -- end

    -- print( ">>", has_prefixexp, has_functioncall, list.contains(targets, functioncall), bytes)
    return ANY(
      -- list.contains(targets, functioncall) and ALL(
      --   read_functioncall,
      --   READ(read_whitespace, SKIP, OPT),
      --   read_args),
      ALL(
        read_prefixexp,
        READ(read_whitespace, SKIP, OPT),
        read_args)
      -- ALL(
      --   ALL(
      --   read_prefixexp,
      --   READ(read_whitespace, SKIP, OPT),
      --   read_args))
    -- ALL(
    --   not is_looping and read_prefixexp,
    --   READ(read_whitespace, SKIP, OPT),
    --   TERM(":"),
    --   READ(read_whitespace, SKIP, OPT),
    --   read_Name,
    --   READ(read_whitespace, SKIP, OPT),
    --   read_args)
  ) end)

local read_while = read_nonterminal(_while,
  -- while exp do block end
  function() return ALL(
    TERM("while"),
    read_whitespace, -- omit SKIP to prevent "whilenil"
    READ(read_exp),
    TERM("do"),
    read_whitespace, -- omit SKIP to prevent "doreturn"
    READ(read_block, OPT),
    TERM("end"),
    read_whitespace
  ) end, true)

read_exp = read_nonterminal(exp,
  -- exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ |  
  --      functiondef | prefixexp | tableconstructor | exp binop exp | unop exp 
  function() return ALL(
    ANY(
      ALL(ANY(
        TERM("nil"),
        TERM("false"),
        TERM("true"),
        READ(read_number),
        TERM("...")),
        READ(read_whitespace)),
      read_prefixexp,
      ALL(read_unop, read_exp)
    )
  ) end, true)


local function lookahead(environment, bytes, targets, functioncall, reader)
  local functioncall_count = list.count(targets, functioncall)
  local maximum_count = 2
  -- print(">>: count", functioncall_count)
  local values, rest
  while true do
    values, rest = ANY(
        functioncall_count < maximum_count and reader
    )(environment, bytes, targets)

    print("?", values, rest, rest == bytes, reader, maximum_count, functioncall_count)
    if values then
      print(values, functioncall_count, rest)
      -- os.exit()
    else
      return 0
    end
    maximum_count = maximum_count + 1
    if maximum_count > 4 then
      break
    end
  end
  return 4
  -- print(values, rest)

end

local function tree_filter(f_child, rule)
  assert(is(rule), rule)
  local origin = list(nil)
  local last = origin
  for i, value in ipairs(rule) do
    if type(value) == "table" then
      if f_child(value, rule) then
        last[2] = cons(value)
        last = last[2]
      end
      if is(value) and not is(value, SKIP) then
        last[2] = tree_filter(f_child, value)
        if last[2] then
          last = last[2]
        end
      end
    end
  end
  return origin[2]
end

local function find_maximum_count(environment, bytes, targets, rule, nonterminal)
  if not targets then
    return 0
  end
  local head = nonterminal.factory(environment, bytes)
  local sections = tree_filter(function(value, parent)
      if getmetatable(value) ~= ALL then
        return false
      end
      local found = false
      for i, reader in ipairs(value) do
        if type(reader) == "table" and reader.target == nonterminal then
          found = true
          break
        end
      end
      return found
    end, rule)

  local values, rest = head(environment, bytes, targets)
  if not values and rest == bytes then
    return 1
  end
  sections = map(function(section)
    local nonterminals = map(bind(operator["[]"], "target"), section)
    local index = contains(nonterminals, nonterminal)
    return ALL(unpack(slice(section, index + 1)))
  end, sections)
  local repeats = ALL(READ(ANY(unpack(sections)), REPEAT))
  return list.__len(repeats(environment, rest))
end

read_prefixexp = read_nonterminal(prefixexp,
  -- prefixexp ::= var | functioncall | ‘(’ exp ‘)’
  function(environment, bytes, targets) 
    -- Targets is a stack of nonterminals that have been called for the current
    -- byte.
    local target = targets and cdr(targets) and car(cdr(targets))
    -- print(target)
    local from_functioncall = target == functioncall
    local from_var = target == var
    -- print("from_functioncall", from_functioncall)
    -- not from_functioncall and 
    -- print(list.count(targets, functioncall))
    local functioncall_count = targets and list.count(cdr(targets), functioncall) or 0
    local functioncall_maximum_count = not from_var and find_maximum_count(
      environment,
      bytes,
      targets,
      functioncall.factory(environment, bytes),
      prefixexp) or 0--> get the bits after prefixexp inside functioncall, then READ(ANY(bits), REPEAT) and return the count. 
    -- print(bytes, show(environment._META))
    -- print("begin")

    -- print(functioncall_maximum_count)

    
    -- local functioncall_maximum_count = 1
    -- if functioncall_count > 0 then
    --   maximum_count = lookahead(environment, bytes, targets, functioncall, read_functioncall)
    -- end
    -- print("end")
    -- print(">>", functioncall_count)
    -- print(targets)
    -- print(from_var)
    return ANY(
      -- I don't know how many times i need to loop
      -- need to guess...
      -- can't be too many or too little
      functioncall_count < functioncall_maximum_count and ALL(
        -- Give the parser a hint to avoid infinite loop on Left-recursion.
        -- When it comes down to it `functioncall` can only begin with a 
        -- Name or "(".
          READ(ANY(read_Name, TERM("(")), PEEK),
          read_functioncall),
      not from_var and ALL(
        -- Give the parser a hint to avoid infinite loop on Left-recursion.
        -- When it comes down to it `var` can only begin with a Name or "(".
        READ(ANY(read_Name, TERM("(")), PEEK),
        read_var),
      ALL(
        TERM("("),
        READ(read_whitespace, SKIP, OPT),
        read_exp,
        READ(read_whitespace, SKIP, OPT),
        TERM(")"),
        READ(read_whitespace, SKIP, OPT)
      )
    ) end)

read_var = read_nonterminal(var,
  -- var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 
  function(environment, bytes)
    local has_prefixexp
    -- Check if read_prefixexp is already read
    if environment._META[bytes] then
      if environment._META[bytes].read == read_prefixexp then
        has_prefixexp = true
      end
    end
    return ANY(
      ALL(
        -- ANY cuts out at read_Name for a.b, if we don't have this. 
        read_Name,
        READ(read_whitespace, SKIP, OPT),
        TERM("."),
        READ(read_whitespace, SKIP, OPT),
        read_Name),
      read_Name,
      ALL(
        not has_prefixexp and read_prefixexp,
        READ(read_whitespace, SKIP, OPT),
        TERM('['),
        READ(read_whitespace, SKIP, OPT),
        read_exp,
        READ(read_whitespace, SKIP, OPT),
        TERM("]"),
        READ(read_whitespace, SKIP, OPT)),
      ALL(
        not has_prefixexp and read_prefixexp,
        READ(read_whitespace, SKIP, OPT),
        TERM("."),
        READ(read_whitespace, SKIP, OPT),
        read_Name)
    ) end)

local read_varlist = read_nonterminal(varlist,
  -- varlist ::= var {‘,’ var}
  function() return ALL(
    read_var,
    READ(ALL(
      READ(read_whitespace, SKIP, OPT),
      TERM(","),
      READ(read_whitespace, SKIP, OPT),
      read_var), REPEAT)
  ) end, true)

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
    ALL(TERM(";"), READ(read_whitespace, SKIP, OPT)),
    ALL(
      read_varlist,
      READ(read_whitespace, SKIP, OPT),
      TERM("="),
      READ(read_whitespace, SKIP, OPT),
      read_explist),
    read_functioncall,
    -- do
    --repeat
    -- if
    -- for
    -- funnction
    -- local function
    -- local name list
    read_label,
    TERM("break"),
    read_goto,
    read_while
  ) end, true)


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

read_explist = read_nonterminal(explist,
  -- explist ::= exp {‘,’ exp}
  function() return ALL(
    read_exp,
    READ(read_whitespace, SKIP, OPT),
    READ(ALL(      
      TERM(","),
      READ(read_whitespace, SKIP, OPT),
      read_exp), REPEAT)
  ) end, true)

local read_retstat = read_nonterminal(retstat,
  -- retstat ::= return [explist] [‘;’]
  function() return ALL(
    TERM("return"),
    READ(read_whitespace),
    READ(read_explist, OPT), --should be explist
    READ(TERM(";"), OPT)
  ) end, true)

read_block = read_nonterminal(block,
  function() return ALL(
    READ(read_whitespace, SKIP, OPT),
    READ(read_stat, REPEAT),
    READ(read_whitespace, SKIP, OPT),
    READ(read_retstat, OPT)
  ) end, true)

--- Return the default _R table.
local function block_R()
  return {
    ["("] = false,
    [")"] = false,
    -- put block level stuff here? or in read_block"s own _R
    list(read_block)
  }
end

-- print(car(execute(read_block, nil, tolist("while not true do return a, b end"))))
return {
    block_R = block_R,
    read_functioncall = read_functioncall,
    read_retstat = read_retstat
}
