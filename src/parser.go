package main

import (
	"fmt"
	"strconv"
)

type Node interface{}

type Program struct {
	Package string
	Imports []string
	Stmts   []Stmt
}

type Stmt interface {
	Node
}

type Expr interface {
	Node
}

type VarDecl struct {
	Name  string
	Type  string
	Value Expr
}

type AssignStmt struct {
	Name  string
	Value Expr
}

type ExprStmt struct {
	Expr Expr
}

type IfStmt struct {
	Cond Expr
	Body []Stmt
	Else []Stmt
}

type PackageDecl struct {
	Name string
}

type ImportDecl struct {
	Path string
}

type ReturnStmt struct {
	Value Expr
}

type IntLiteral struct {
	Value int
}

type StringLiteral struct {
	Value string
}

type Ident struct {
	Name string
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

type CharLiteral struct {
	Value string
}

type PunctLiteral struct {
	Value string
}

type KeywordLiteral struct {
	Value string
}

type BlockStmt struct {
	Stmts []Stmt
}

type CallExpr struct {
	Func Expr
	Args []Expr
}

type SelectorExpr struct {
	X   Expr
	Sel string
}

type FuncDecl struct {
	Name       string
	Params     []VarDecl
	ReturnType string
	Body       *BlockStmt
}

type Parser struct {
	l    *Lexer
	cur  Token
	peek Token
}

func NewParser(l *Lexer) *Parser {
	p := &Parser{l: l}
	p.next()
	p.next()
	return p
}

func (p *Parser) next() {
	p.cur = p.peek
	tok, err := p.l.Next()
	if err != nil {
		panic(err)
	}
	p.peek = tok
}

func (p *Parser) expect(expected string) {
	if p.cur.sval != expected {
		panic(fmt.Sprintf("expected '%s', got '%s'", expected, p.cur.sval))
	}
	p.next()
}

func (p *Parser) parseStmt() Stmt {
	if p.cur.sval == ";" {
		p.next()
		return p.parseStmt()
	}

	tok := p.cur

	if tok.typ == T_KEYWORD && tok.sval == "func" {
		return p.parseFuncDecl()
	}

	if tok.typ == T_KEYWORD && tok.sval == "var" {
		return p.parseVarDecl()
	}

	if tok.typ == T_KEYWORD && tok.sval == "return" {
		p.next()
		var value Expr
		if p.cur.sval != ";" && p.cur.sval != "}" && p.cur.typ != T_EOF {
			value = p.parseExpr()
		}
		return &ReturnStmt{Value: value}
	}

	if tok.typ == T_KEYWORD && tok.sval == "if" {
		return p.parseIfStmt()
	}

	if tok.sval == "{" {
		return p.parseBlock()
	}

	if tok.typ == T_IDENT {
		if p.peek.sval == "=" || p.peek.sval == ":=" {
			return p.parseAssignStmt()
		}
	}

	expr := p.parseExpr()
	return &ExprStmt{Expr: expr}
}

func (p *Parser) parseFuncDecl() *FuncDecl {
	p.expect("func")

	if p.cur.typ != T_IDENT {
		panic("expected function name after func")
	}
	funcName := p.cur.sval
	p.next()

	p.expect("(")

	var params []VarDecl
	for p.cur.sval != ")" && p.cur.typ != T_EOF {
		if p.cur.typ != T_IDENT {
			panic("expected parameter name")
		}
		paramName := p.cur.sval
		p.next()

		paramType := ""

		if p.cur.sval == "," {
			p.next()
			if p.cur.typ != T_IDENT {
				panic("expected parameter name after comma")
			}
		} else if p.cur.typ == T_IDENT {
			paramType = p.cur.sval
			p.next()

			if paramType != "" {
				for i := len(params) - 1; i >= 0; i-- {
					if params[i].Type == "" {
						params[i].Type = paramType
					} else {
						break
					}
				}
			}
		} else if p.cur.sval != ")" {
			panic(fmt.Sprintf("expected parameter type or ')' after parameter '%s'", paramName))
		}

		params = append(params, VarDecl{Name: paramName, Type: paramType, Value: nil})

		if p.cur.sval == "," {
			p.next()
		}
	}

	for _, param := range params {
		if param.Type == "" {
			panic(fmt.Sprintf("parameter '%s' missing type", param.Name))
		}
	}

	p.expect(")")

	returnType := ""
	if p.cur.typ == T_IDENT {
		returnType = p.cur.sval
		p.next()
	}

	body := p.parseBlock()

	p.validateReturns(body, returnType, funcName)

	return &FuncDecl{Name: funcName, Params: params, ReturnType: returnType, Body: body}
}

func (p *Parser) parseVarDecl() *VarDecl {
	p.expect("var")

	if p.cur.typ != T_IDENT {
		panic("expected variable name after var")
	}
	name := p.cur.sval
	p.next()

	var typ string
	var value Expr

	if p.cur.typ == T_IDENT {
		typ = p.cur.sval
		p.next()
	}

	if p.cur.sval == "=" {
		p.next()
		value = p.parseExpr()
	}

	return &VarDecl{Name: name, Type: typ, Value: value}
}

func (p *Parser) parseAssignStmt() *AssignStmt {
	if p.cur.typ != T_IDENT {
		panic("expected identifier in assignment")
	}
	name := p.cur.sval
	p.next()

	if p.cur.sval == "=" || p.cur.sval == ":=" {
		p.next()
		value := p.parseExpr()
		return &AssignStmt{Name: name, Value: value}
	}

	panic("expected = or := in assignment")
}

func (p *Parser) parseIfStmt() *IfStmt {
	p.expect("if")
	cond := p.parseExpr()
	body := p.parseBlock()

	var elseStmts []Stmt
	if p.cur.typ == T_KEYWORD && p.cur.sval == "else" {
		p.next()
		if p.cur.sval == "{" {
			elseBlock := p.parseBlock()
			elseStmts = elseBlock.Stmts
		} else if p.cur.typ == T_KEYWORD && p.cur.sval == "if" {
			elseIf := p.parseIfStmt()
			elseStmts = []Stmt{elseIf}
		}
	}

	return &IfStmt{Cond: cond, Body: body.Stmts, Else: elseStmts}
}

func (p *Parser) validateReturns(block *BlockStmt, returnType, funcName string) {
	p.validateReturnsInStmts(block.Stmts, returnType, funcName)
}

func (p *Parser) validateReturnsInStmts(stmts []Stmt, returnType, funcName string) {
	for _, stmt := range stmts {
		switch s := stmt.(type) {
		case *ReturnStmt:
			if returnType == "" && s.Value != nil {
				panic(fmt.Sprintf("function '%s' has no return type but return statement has value", funcName))
			}
			if returnType != "" && s.Value == nil {
				panic(fmt.Sprintf("function '%s' expects return type '%s' but return statement has no value", funcName, returnType))
			}
		case *BlockStmt:
			p.validateReturnsInStmts(s.Stmts, returnType, funcName)
		case *IfStmt:
			p.validateReturnsInStmts(s.Body, returnType, funcName)
			p.validateReturnsInStmts(s.Else, returnType, funcName)
		}
	}
}

func (p *Parser) parseBlock() *BlockStmt {
	p.expect("{")

	var stmts []Stmt
	for p.cur.sval != "}" && p.cur.typ != T_EOF {
		stmt := p.parseStmt()
		if stmt != nil {
			stmts = append(stmts, stmt)
		}
	}

	p.expect("}")
	return &BlockStmt{Stmts: stmts}
}

func (p *Parser) parseExpr() Expr {
	return p.parseLogicalOr()
}

func (p *Parser) parseLogicalOr() Expr {
	left := p.parseLogicalAnd()

	for p.cur.sval == "||" {
		op := p.cur.sval
		p.next()
		right := p.parseLogicalAnd()
		left = &BinaryExpr{Left: left, Op: op, Right: right}
	}

	return left
}

func (p *Parser) parseLogicalAnd() Expr {
	left := p.parseEquality()

	for p.cur.sval == "&&" {
		op := p.cur.sval
		p.next()
		right := p.parseEquality()
		left = &BinaryExpr{Left: left, Op: op, Right: right}
	}

	return left
}

func (p *Parser) parseEquality() Expr {
	left := p.parseComparison()

	for p.cur.sval == "==" || p.cur.sval == "!=" {
		op := p.cur.sval
		p.next()
		right := p.parseComparison()
		left = &BinaryExpr{Left: left, Op: op, Right: right}
	}

	return left
}

func (p *Parser) parseComparison() Expr {
	left := p.parseAddition()

	for p.cur.sval == "<" || p.cur.sval == ">" || p.cur.sval == "<=" || p.cur.sval == ">=" {
		op := p.cur.sval
		p.next()
		right := p.parseAddition()
		left = &BinaryExpr{Left: left, Op: op, Right: right}
	}

	return left
}

func (p *Parser) parseAddition() Expr {
	left := p.parseMultiplication()

	for p.cur.sval == "+" || p.cur.sval == "-" {
		op := p.cur.sval
		p.next()
		right := p.parseMultiplication()
		left = &BinaryExpr{Left: left, Op: op, Right: right}
	}

	return left
}

func (p *Parser) parseMultiplication() Expr {
	left := p.parseUnary()

	for p.cur.sval == "*" || p.cur.sval == "/" || p.cur.sval == "%" {
		op := p.cur.sval
		p.next()
		right := p.parseUnary()
		left = &BinaryExpr{Left: left, Op: op, Right: right}
	}

	return left
}

func (p *Parser) parseUnary() Expr {
	if p.cur.sval == "!" || p.cur.sval == "-" || p.cur.sval == "+" {
		op := p.cur.sval
		p.next()
		expr := p.parseUnary()
		return &UnaryExpr{Op: op, Expr: expr}
	}

	return p.parsePostfix()
}

func (p *Parser) parsePostfix() Expr {
	expr := p.parsePrimary()

	for {
		if p.cur.sval == "." {
			p.next()
			if p.cur.typ != T_IDENT {
				panic("expected identifier after .")
			}
			expr = &SelectorExpr{X: expr, Sel: p.cur.sval}
			p.next()
		} else if p.cur.sval == "(" {
			expr = p.parseCall(expr)
		} else {
			break
		}
	}

	return expr
}

func (p *Parser) parsePrimary() Expr {
	tok := p.cur

	switch tok.typ {
	case T_INT:
		val, err := strconv.Atoi(tok.sval)
		if err != nil {
			panic("invalid integer literal: " + tok.sval)
		}
		p.next()
		return &IntLiteral{Value: val}

	case T_STRING:
		p.next()
		return &StringLiteral{Value: tok.sval}

	case T_CHAR:
		p.next()
		return &CharLiteral{Value: tok.sval}

	case T_IDENT:
		p.next()
		return &Ident{Name: tok.sval}

	case T_PUNCT:
		if tok.sval == "(" {
			p.next()
			expr := p.parseExpr()
			p.expect(")")
			return expr
		}
		fallthrough

	default:
		panic("unexpected token in expression: " + tok.sval)
	}
}

func (p *Parser) parseCall(fn Expr) *CallExpr {
	p.expect("(")

	var args []Expr
	for p.cur.sval != ")" && p.cur.typ != T_EOF {
		args = append(args, p.parseExpr())
		if p.cur.sval == "," {
			p.next()
		}
	}

	p.expect(")")
	return &CallExpr{Func: fn, Args: args}
}

func (p *Parser) ParseProgram() *Program {
	program := &Program{
		Package: "",
		Imports: make([]string, 0),
		Stmts:   make([]Stmt, 0),
	}

	if p.cur.typ == T_KEYWORD && p.cur.sval == "package" {
		p.next()
		if p.cur.typ != T_IDENT {
			panic("expected package name after 'package'")
		}
		program.Package = p.cur.sval
		p.next()
	} else {
		panic("files must start with package declaration")
	}

	for p.cur.typ == T_KEYWORD && p.cur.sval == "import" {
		imports := p.parseImports()
		program.Imports = append(program.Imports, imports...)
	}

	for p.cur.typ != T_EOF {
		stmt := p.parseStmt()
		if stmt != nil {
			program.Stmts = append(program.Stmts, stmt)
		}
	}

	return program
}

func (p *Parser) parseImports() []string {
	p.expect("import")

	var imports []string

	if p.cur.typ == T_STRING {
		imports = append(imports, p.cur.sval)
		p.next()
	} else if p.cur.sval == "(" {
		p.next()
		for p.cur.sval != ")" && p.cur.typ != T_EOF {
			if p.cur.typ == T_STRING {
				imports = append(imports, p.cur.sval)
				p.next()
			} else if p.cur.sval == ";" || p.cur.sval == "\n" {
				p.next()
			} else {
				panic("expected string literal in import block")
			}
		}
		if p.cur.sval != ")" {
			panic("expected ) at end of import block")
		}
		p.next()
	} else {
		panic("expected string literal or ( after import")
	}

	return imports
}

func PrintAST(node Node, indent string) {
	switch n := node.(type) {
	case *Program:
		fmt.Printf("%sProgram:\n", indent)
		if n.Package != "" {
			fmt.Printf("%s  Package: %s\n", indent, n.Package)
		}
		if len(n.Imports) > 0 {
			fmt.Printf("%s  Imports:\n", indent)
			for _, imp := range n.Imports {
				fmt.Printf("%s    %s\n", indent, imp)
			}
		}
		if len(n.Stmts) > 0 {
			fmt.Printf("%s  Statements:\n", indent)
			for _, stmt := range n.Stmts {
				PrintAST(stmt, indent+"    ")
			}
		}

	case *VarDecl:
		fmt.Printf("%sVarDecl: %s", indent, n.Name)
		if n.Type != "" {
			fmt.Printf(" (%s)", n.Type)
		}
		if n.Value != nil {
			fmt.Printf(" =")
			fmt.Println()
			PrintAST(n.Value, indent+"  ")
		} else {
			fmt.Println()
		}

	case *AssignStmt:
		fmt.Printf("%sAssignStmt: %s =\n", indent, n.Name)
		PrintAST(n.Value, indent+"  ")

	case *ExprStmt:
		fmt.Println(indent + "ExprStmt:")
		PrintAST(n.Expr, indent+"  ")

	case *ReturnStmt:
		fmt.Println(indent + "ReturnStmt:")
		if n.Value != nil {
			PrintAST(n.Value, indent+"  ")
		}

	case *IfStmt:
		fmt.Println(indent + "IfStmt:")
		fmt.Println(indent + "  Condition:")
		PrintAST(n.Cond, indent+"    ")
		fmt.Println(indent + "  Body:")
		for _, stmt := range n.Body {
			PrintAST(stmt, indent+"    ")
		}
		if len(n.Else) > 0 {
			fmt.Println(indent + "  Else:")
			for _, stmt := range n.Else {
				PrintAST(stmt, indent+"    ")
			}
		}

	case *BlockStmt:
		fmt.Println(indent + "BlockStmt:")
		for _, stmt := range n.Stmts {
			PrintAST(stmt, indent+"  ")
		}

	case *FuncDecl:
		fmt.Printf("%sFuncDecl: %s", indent, n.Name)
		if n.ReturnType != "" {
			fmt.Printf(" -> %s", n.ReturnType)
		}
		fmt.Println()
		if len(n.Params) > 0 {
			fmt.Println(indent + "  Params:")
			for _, param := range n.Params {
				fmt.Printf("%s    %s", indent, param.Name)
				if param.Type != "" {
					fmt.Printf(" (%s)", param.Type)
				}
				fmt.Println()
			}
		}
		fmt.Println(indent + "  Body:")
		PrintAST(n.Body, indent+"    ")

	case *IntLiteral:
		fmt.Printf("%sIntLiteral: %d\n", indent, n.Value)

	case *StringLiteral:
		fmt.Printf("%sStringLiteral: %q\n", indent, n.Value)

	case *CharLiteral:
		fmt.Printf("%sCharLiteral: %q\n", indent, n.Value)

	case *Ident:
		fmt.Printf("%sIdent: %s\n", indent, n.Name)

	case *BinaryExpr:
		fmt.Printf("%sBinaryExpr: %s\n", indent, n.Op)
		fmt.Println(indent + "  Left:")
		PrintAST(n.Left, indent+"    ")
		fmt.Println(indent + "  Right:")
		PrintAST(n.Right, indent+"    ")

	case *UnaryExpr:
		fmt.Printf("%sUnaryExpr: %s\n", indent, n.Op)
		PrintAST(n.Expr, indent+"  ")

	case *SelectorExpr:
		fmt.Println(indent + "SelectorExpr:")
		fmt.Println(indent + "  X:")
		PrintAST(n.X, indent+"    ")
		fmt.Printf("%s  Sel: %s\n", indent, n.Sel)

	case *CallExpr:
		fmt.Println(indent + "CallExpr:")
		fmt.Println(indent + "  Func:")
		PrintAST(n.Func, indent+"    ")
		if len(n.Args) > 0 {
			fmt.Println(indent + "  Args:")
			for i, arg := range n.Args {
				fmt.Printf("%s    Arg %d:\n", indent, i)
				PrintAST(arg, indent+"      ")
			}
		}

	default:
		fmt.Printf("%sUnknown node: %T %+v\n", indent, n, n)
	}
}
