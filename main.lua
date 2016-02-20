--[[

usage:
{
	ctx = ml.c "CODE"

	ctx.some_function(args,...)
	local data = ctx.some_data()
}
e.g.
{
	ctx = ml.c "fun twice x = x + x"
	print(ctx.twice(3)) --> 6

	ctx = ml.c "fun mag_sqr (x,y) = x*x + y*y"
	print(ctx.mag_sqr(3, 4)) --> 25

	ctx = ml.c "val magic_number = 42"
	print(ctx.magic_number()) --> 42

	ctx = ml.c "datatype color = R|G|B  fun isRed R = true | isRed _ = false"
	print(ctx.isRed(ctx.G())) --> false
}


]]--


function push (a, t)
	a[#a + 1] = t
end
function pop (a)
	local t = a[#a]
	a[#a] = nil
	return t
end

-----------------------------
----[[ Lexical Scanner ]]----
-----------------------------

local REG_NUMBER = "%d+"
local REG_IDENT = "[%a_][%w_%^]*"
local REG_MAYBE_NUMBER = "%d"
local REG_OPEN_COMMENT = "%(%*"
local REG_CLOSE_COMMENT = "%*%)"
local REG_SPACES = "%s+"
local SYMS = {
	{"(",   pat="%("},  {")",   pat="%)"},
	{"[",   pat="%["},  {"]",   pat="%]"},
	{"{",   pat="{"},   {"}",   pat="}"},
	{"|",   pat="|"},   {".",   pat="%."},
	{",",   pat=","},   {"+",   pat="%+"},
	{"-",   pat="%-"},  {"*",   pat="%*"},
	{"/",   pat="/"},   {"%",   pat="%%"},
	{";",   pat=";"},   {"=>",  pat="=>"},
	{"=",   pat="="},   {"<>",  pat="<>"},
	{">=",  pat=">="},  {"<=",  pat="<="},
	{">",   pat=">"},   {"<",   pat="<"},
	{"::",  pat="::"},  {"@",   pat="@"}
}
local KEYWORDS = {
	"fn",   "if",   "in",   "of",
	"end",  "fun",  "let",  "val",
	"case", "else", "then", "type"
}
local function Scanner (input)
	local scan_pos = 1
	local len = #input

	-- ml uses "~" as negative
	function ml_tonumber (s)
		if string.find(s, "~") == 1 then
			return -tonumber(string.sub(s, 2))
		else
			return tonumber(s)
		end
	end

	-- somewhat pretty
	function die (msg, p)
		if p == nil then p = scan_pos end
		local line_num = 1
		local j = 1

		while true do
			j,i = string.find(input, "\n", j)
			if j == nil or j > p then
				break
			else
				j = j + 1
				line_num = line_num + 1
			end
		end

		return error("error, line " .. tostring(line_num) .. ": " .. msg, 0)
	end

	-- trim whitespace and comments from left side
	function triml ()
		local i,j
		while scan_pos <= len do
			-- find comment (*
			i,j = string.find(input, REG_OPEN_COMMENT, scan_pos)
			local start_pos = i
			if i == scan_pos then
				scan_pos = j + 1
				-- find comment *)
				i,j = string.find(input, REG_CLOSE_COMMENT, scan_pos)
				if i == scan_pos then
					scan_pos = j + 1
				else
					-- none!
					return die("unclosed (*", start_pos)
				end
			end

			-- find spaces
			i,j = string.find(input, REG_SPACES, scan_pos)
			if i == scan_pos then
				scan_pos = j + 1
			else
				-- no more spaces
				break
			end
		end
	end

	-- scan: symbols
	function scan_sym ()
		for _,sym in pairs(SYMS) do
			-- match pattern
			local i,j = string.find(input, sym.pat, scan_pos)
			if i == scan_pos then
				scan_pos = j + 1
				-- return on success
				return sym[1]
			end
		end
		-- not a symbol?
		return nil
	end

	-- scan: numbers
	function scan_num ()
		local i,j = string.find(input, REG_NUMBER, scan_pos)
		if i == scan_pos then
			scan_pos = j + 1
			return ml_tonumber(string.sub(input, i, j))
		else
			return die("invalid number literal")
		end
	end

	-- scan: keywords
	function scan_word ()
		-- just parse the identifier
		local i,j = string.find(input, REG_IDENT, scan_pos)
		if i == scan_pos then
			scan_pos = j + 1
		else
			return die("invalid token '" ..
			           string.sub(input, scan_pos, scan_pos) .. "'")
		end

		-- check for known keywords
		local word = string.sub(input, i, j)
		for _,kw in pairs(KEYWORDS) do
			if word == kw then
				return true,kw
			end
		end
		return false,word
	end

	-- move the lexer
	function advance (self)
		self.span = scan_pos
		self.val = nil
		triml()

		if scan_pos > len then
			self.tok = "<eof>"
			return self
		end

		-- check symbol first
		local sym = scan_sym()
		if sym ~= nil then
			self.tok = sym
			return self
		end

		-- then number
		local i,j = string.find(input, REG_MAYBE_NUMBER, scan_pos)
		if i == scan_pos then
			local num = scan_num()
			self.tok = "<num>"
			self.val = num
			return self
		end

		-- then keyword
		local is_kw,word = scan_word()
		if is_kw then
			self.tok = word
		else
			self.tok = "<id>"
			self.val = word
		end

		return self
	end

	-- return current token and move to next
	function shift (self)
		local tok,val,span = self.tok, self.val, self.span
		self:advance()
		return tok,val,span
	end

	-- expect given token
	function eat (self, t)
		if self.tok == t then
			local tok,val,span = self:shift()
			return val,span
		else
			return die("expected '" .. t ..
			           "', got '" .. self.tok .. "'", self.span)
		end
	end

	-- scanner object
	return ({
		advance = advance,
		shift = shift,
		eat = eat,
		die = function (self, msg, p)
			return die(msg, p)
		end,

		span = 1,
		tok = "<empty>",
		val = nil
	}):advance()
end





------------------------
----[[ AST Parser ]]----
------------------------

local Parse = {}

--[[ Declarations ]]--
function Parse.decl (sc)
	local tok,_,span = sc:shift()

	if tok == "fun" then
		return Parse.funDecl(sc, span)
	elseif tok == "val" then
		return Parse.valDecl(sc, span)
	else
		return sc:die("invalid declaration: " .. sc.tok, span)
	end
end
function Parse.funDecl (sc, span)
	local name = sc.val
	local sigs = {}

	-- at least one signature
	Parse.funSig(sc, name, sigs)
	while sc.tok == "|" do
		sc:shift()
		Parse.funSig(sc, name, sigs)
	end

	return {decl="fun", span=span, name=name, sigs=sigs}
end
function Parse.funSig (sc, name, sigs)
	-- check they entered the name correctly
	given_name,span = sc:eat("<id>")
	if given_name ~= name then
		return sc:die("expected function name '" .. name ..
		              "', got '" .. given_name .. "'", span)
	end

	-- parse the argument
	local arg = Parse.pattern(sc)

	-- function body
	sc:eat("=")
	local body = Parse.expr(sc)

	push(sigs, {decl="sig", span=span, arg=arg, body=body})
end
function Parse.valDecl (sc, span)
	local pat = Parse.pattern(sc)
	sc:eat("=")
	local body = Parse.expr(sc)

	return {decl="val", span=span, pat=pat, body=body}
end


--[[ Expressions ]]--
function Parse.expr (sc)
	if sc.tok == "let" then -- let ... in ...
		local _,_,span = sc:shift()
		return Parse.letExpr(sc, span)

	elseif sc.tok == "case" then -- case ... of ... => ...
		local _,_,span = sc:shift()
		return Parse.caseExpr(sc, span)

	elseif sc.tok == "fn" then -- fn ... => ...
		local _,_,span = sc:shift()
		local pat,body = Parse.patternCase(sc)
		return {k="lambda", span=span, arg=pat, body=body}

	elseif sc.tok == "if" then -- if ... then ... else ...
		local _,_,span = sc:shift()
		local e1 = Parse.expr(sc)
		sc:eat("then")
		local e2 = Parse.expr(sc)
		sc:eat("else")
		local e3 = Parse.expr(sc)
		return {k="if", span=span, e1, e2, e3}

	else -- term | expr term
		return Parse.infix(sc)
	end
end
-- let <decls> in <expr> end
function Parse.letExpr (sc, span)
	local decls = {Parse.decl(sc)}
	while sc.tok ~= "in" do
		push(decls, Parse.decl(sc))
	end
	sc:shift()
	local body = Parse.expr(sc)
	sc:eat("end")
	return {k="let", span=span, decls=decls, body=body}
end
-- case <expr> of <cases> end
function Parse.caseExpr (sc, span)
	local cond = Parse.expr(sc)
	local cases = {}
	sc:eat("of")
	cases[1] = Parse.patternCase(sc)
	while sc.tok == "|" do
		sc:shift()
		push(cases, Parse.patternCase(sc))
	end
	sc:eat("end")
	return {k="case", span=span, cond=cond, cases=cases}
end
-- <pat> => <expr>
function Parse.patternCase (sc)
	local pat = Parse.pattern(sc)
	sc:eat("=>")
	local expr = Parse.expr(sc)
	return {pat,expr}
end
-- <expr> {<op> <expr>}
function Parse.infix (sc)
	local fst = Parse.appl(sc)
	local exprs = {fst}
	local opers = {}

	-- pop ops and expr and combine
	local function reduce ()
		local op = pop(opers)
		local e2 = pop(exprs)
		local e1 = pop(exprs)
		local e = {k="infix", span=op.span, op=op.name, e1, e2}
		push(exprs, e)
	end
	-- based on simultaneous shunting yard and RPN reducer
	local function add_shunting (op)
		while #opers > 0 and precedes(op, opers[#opers]) do
			reduce()
		end
		push(opers, op)
	end
	-- does op1 precede op2?
	local function precedes (op1, op2)
		if op1.is_right then
			return op1.prec < op2.prec
		else
			return op1.prec <= op2.prec
		end
	end

	local op = Parse.infixOp(sc)
	while op ~= nil do
		add_shunting(op)
		push(exprs, Parse.appl(sc))
		op = Parse.infixOp(sc)
	end
	while #opers > 0 do
		reduce()
	end
	return exprs[1]
end
local INFIX = {
	{";",   0, false},
	{"=",   1, false},
	{"<>",  1, false},
	{">",   1, false},
	{"<",   1, false},
	{">=",  1, false},
	{"<=",  1, false},
	{"::",  4, true},
	{"@",   4, true},
	{"+",   2, false},
	{"-",   2, false},
	{"*",   3, false},
	{"/",   3, false},
	{"%",   3, false}
}
function Parse.infixOp (sc)
	for _,v in ipairs(INFIX) do
		local t,prec,r = unpack(v)

		if sc.tok == t then
			local _,_,span = sc:shift()
			return {name=t, is_right=r, prec=prec}
		end
	end
	return nil
end

function Parse.term (sc, opt)
	if sc.tok == "<id>" then
		return opt.parseId(sc)

	elseif sc.tok == "<num>" then
		local _,val,span = sc:shift()
		return {k="num", span=span, val=val}

	elseif sc.tok == "(" then
		local _,_,span = sc:shift()
		-- unit: ()
		if sc.tok == ")" then
			sc:shift()
			return {k="unit", span=span}
		end

		-- tuple: (e1, e2, ...)
		local e = opt.parseOne(sc)
		if sc.tok == "," then
			e = Parse.tuple(sc, opt, span, e)
		end
		sc:eat(")")
		return e

	else
		return sc:die("invalid term: " .. sc.tok, sc.span)
	end
end
function Parse.tuple (sc, opt, span, fst)
	local elems = {fst}
	while sc.tok == "," do
		-- parse one and add it
		sc:shift()
		push(elems, Parse.expr(sc))
	end

	elems.k = "tuple"
	elems.span = span
	return elems
end
function Parse.isTerm (tok)
	return tok == "<id>" or
		tok == "<num>" or
		tok == "("
end


function Parse.appl (sc)
	local acc = Parse.term(sc, Parse.exprTerm)
	while Parse.isTerm(sc.tok) do
		local t = Parse.term(sc, Parse.exprTerm)
		acc = {k="app", span=t.span, acc, t}
	end
	return acc
end
function Parse.pattern (sc)
	local pat = Parse.term(sc, Parse.patTerm)
	if sc.tok == "::" then
		local _,_,span = sc:shift()
		local tail = Parse.pattern(sc)
		return {k="cons", span=span, pat, tail}
	else
		return pat
	end
end
Parse.exprTerm = {
	parseOne = Parse.expr,
	parseId = function (sc)
		local name,span = sc:eat("<id>")
		return {k="var", span=span, name=name}
	end
}
Parse.patTerm = {
	parseOne = Parse.pattern,
	parseId = function (sc)
		local name,span = sc:eat("<id>")
		if sc.tok == "(" then
			local arg = Parse.pattern(sc)
			return {k="enum", span=span, name=name, arg=arg}
		else
			-- might be 'enum', to be determined by context
			return {k="var", span=span, name=name}
		end
	end
}






---------------------------
----[[ For Debugging ]]----
---------------------------
	
function ASTKind (e)
	return e.k or e.decl
end
function show (v)
	if type(v) == "table" and ASTKind(v) ~= nil then
		return showAST(v)
	elseif type(v) == "table" then
		return showASTList(v)
	elseif type(v) == "string" then
		return string.format("%q", v)
	else
		return tostring(v)
	end
end
function showASTList (lst)
	local buf = {"["}
	for i,v in ipairs(lst) do
		if #buf > 1 then
			push(buf, ", ")
		end
		push(buf, show(v))
	end
	push(buf, "]")
	return table.concat(buf)
end
function showAST (e)
	local buf = {ASTKind(e), " {"}
	local function ignore (key)
		if type(key) ~= "string" then
			return true
		elseif key == "k" or key == "decl"
				or key == "span" then
			return true
		else
			return false
		end
	end
	for key,v in pairs(e) do
		if not ignore(key) then
			if #buf > 2 then
				push(buf, ", ")
			end
			push(buf, tostring(key))
			push(buf, " = ")
			push(buf, show(v))
		end
	end
	for i,v in ipairs(e) do
		if #buf > 2 then
			push(buf, ", ")
		end
		push(buf, show(v))
	end
	push(buf, "}")
	return table.concat(buf)
end

function go (input)
	local scanner = Scanner(input)

	local fn = Parse.decl(scanner)
	for i,sig in ipairs(fn.sigs) do
		print(show(sig.arg))
		print("  " .. show(sig.body))
	end
end



go [[

fun sum 0 = 0
  | sum n = n + sum (pred n)

]]