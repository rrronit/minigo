package main

import (
	"errors"
	"fmt"
)

type TokenType int

const (
	T_EOF TokenType = iota
	T_INT
	T_STRING
	T_CHAR
	T_IDENT
	T_PUNCT
	T_KEYWORD
)

var keywords = []string{
	"break",
	"default",
	"func",
	"interface",
	"select",
	"case",
	"defer",
	"go",
	"map",
	"struct",
	"chan",
	"else",
	"goto",
	"package",
	"switch",
	"const",
	"fallthrough",
	"if",
	"range",
	"type",
	"continue",
	"for",
	"import",
	"return",
	"var",
}

type Token struct {
	typ    TokenType
	sval   string
	line   int
	column int
}

func (t Token) String() string {
	typeNames := map[TokenType]string{
		T_EOF:     "EOF",
		T_INT:     "INT",
		T_STRING:  "STRING",
		T_CHAR:    "CHAR",
		T_IDENT:   "IDENT",
		T_PUNCT:   "PUNCT",
		T_KEYWORD: "KEYWORD",
	}
	return fmt.Sprintf("%s:%d:%d %s '%s'", typeNames[t.typ], t.line, t.column, typeNames[t.typ], t.sval)
}

func NewLexer(source string) *Lexer {
	return &Lexer{whole_input: source, rest_input: source, index: 0, line: 1, column: 1}
}

type Lexer struct {
	whole_input string
	rest_input  string
	index       int
	line        int
	column      int
}

func (l *Lexer) Next() (Token, error) {
	l.skipWhitespace()
	if len(l.rest_input) == 0 {
		return Token{typ: T_EOF, line: l.line, column: l.column}, nil
	}

	ch := l.rest_input[0]

	if ch == '"' || ch == '`' || ch == '\'' {
		return l.lexString()
	}

	if len(l.rest_input) >= 2 {
		twoChar := l.rest_input[:2]
		switch twoChar {
		case ":=", "==", "!=", "<=", ">=", "&&", "||", "<<", ">>", "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&^":
			startCol := l.column
			l.advance()
			l.advance()
			return Token{typ: T_PUNCT, sval: twoChar, line: l.line, column: startCol}, nil
		}
	}

	if len(l.rest_input) >= 3 {
		threeChar := l.rest_input[:3]
		switch threeChar {
		case "<<=", ">>=", "&^=":
			startCol := l.column
			l.advance()
			l.advance()
			l.advance()
			return Token{typ: T_PUNCT, sval: threeChar, line: l.line, column: startCol}, nil
		}
	}

	if isLetter(ch) {
		startCol := l.column
		ident := ""
		for len(l.rest_input) > 0 && (isLetter(l.rest_input[0]) || isDigit(l.rest_input[0])) {
			ident += string(l.advance())
		}
		if isKeyword(ident) {
			return Token{typ: T_KEYWORD, sval: ident, line: l.line, column: startCol}, nil
		}
		return Token{typ: T_IDENT, sval: ident, line: l.line, column: startCol}, nil
	}

	if isDigit(ch) || (ch == '.' && len(l.rest_input) > 1 && isDigit(l.rest_input[1])) {
		return l.lexNumber()
	}

	if isPunct(ch) {
		startCol := l.column
		return Token{typ: T_PUNCT, sval: string(l.advance()), line: l.line, column: startCol}, nil
	}

	return Token{}, errors.New("unexpected character: " + string(ch))
}

func (l *Lexer) lexString() (Token, error) {
	startCol := l.column
	quote := l.advance()
	strVal := ""

	for len(l.rest_input) > 0 {
		ch := l.rest_input[0]

		if ch == quote {
			l.advance()
			tokenType := T_STRING
			if quote == '\'' && len(strVal) == 1 {
				tokenType = T_CHAR
			}
			return Token{typ: tokenType, sval: strVal, line: l.line, column: startCol}, nil
		}

		if quote != '`' && ch == '\\' && len(l.rest_input) > 1 {
			l.advance()
			if len(l.rest_input) == 0 {
				break
			}
			escaped := l.advance()
			switch escaped {
			case 'n':
				strVal += "\n"
			case 't':
				strVal += "\t"
			case 'r':
				strVal += "\r"
			case '\\':
				strVal += "\\"
			case '"':
				strVal += "\""
			case '\'':
				strVal += "'"
			default:
				strVal += string(escaped)
			}
		} else {
			strVal += string(l.advance())
		}
	}

	return Token{}, errors.New("unterminated string literal")
}

func (l *Lexer) lexNumber() (Token, error) {
	startCol := l.column
	num := ""

	if l.rest_input[0] == '0' && len(l.rest_input) > 1 && (l.rest_input[1] == 'x' || l.rest_input[1] == 'X') {
		num += string(l.advance())
		num += string(l.advance())
		for len(l.rest_input) > 0 && isHexDigit(l.rest_input[0]) {
			num += string(l.advance())
		}
		return Token{typ: T_INT, sval: num, line: l.line, column: startCol}, nil
	}

	if l.rest_input[0] == '0' && len(l.rest_input) > 1 && isDigit(l.rest_input[1]) {
		for len(l.rest_input) > 0 && isOctalDigit(l.rest_input[0]) {
			num += string(l.advance())
		}
		return Token{typ: T_INT, sval: num, line: l.line, column: startCol}, nil
	}

	for len(l.rest_input) > 0 && isDigit(l.rest_input[0]) {
		num += string(l.advance())
	}

	if len(l.rest_input) > 0 && l.rest_input[0] == '.' {
		num += string(l.advance())
		for len(l.rest_input) > 0 && isDigit(l.rest_input[0]) {
			num += string(l.advance())
		}
	}

	return Token{typ: T_INT, sval: num, line: l.line, column: startCol}, nil
}

func (l *Lexer) advance() byte {
	if len(l.rest_input) == 0 {
		return 0
	}

	ch := l.rest_input[0]
	l.rest_input = l.rest_input[1:]
	l.index++

	switch ch {
	case '\n':
		l.line++
		l.column = 1
	case '\t':
		l.column += 4
	default:
		l.column++
	}

	return ch
}

func (l *Lexer) skipWhitespace() {
	for len(l.rest_input) > 0 {
		ch := l.rest_input[0]
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
			l.advance()
		} else if ch == '/' && len(l.rest_input) > 1 {
			if l.rest_input[1] == '/' {
				for len(l.rest_input) > 0 && l.rest_input[0] != '\n' {
					l.advance()
				}
			} else if l.rest_input[1] == '*' {
				l.advance()
				l.advance()
				for len(l.rest_input) >= 2 {
					if l.rest_input[0] == '*' && l.rest_input[1] == '/' {
						l.advance()
						l.advance()
						break
					}
					l.advance()
				}
			} else {
				break
			}
		} else {
			break
		}
	}
}

func isLetter(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}

func isHexDigit(ch byte) bool {
	return isDigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
}

func isOctalDigit(ch byte) bool {
	return ch >= '0' && ch <= '7'
}

func isKeyword(s string) bool {
	for _, kw := range keywords {
		if s == kw {
			return true
		}
	}
	return false
}

func isPunct(ch byte) bool {
	puncts := "+-*/%=!<>|&^~?:.,;()[]{}#"
	for i := 0; i < len(puncts); i++ {
		if ch == puncts[i] {
			return true
		}
	}
	return false
}
