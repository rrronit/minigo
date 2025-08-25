package main

import (
	"fmt"
	"os"
)

func main() {
	content, err := os.ReadFile("examples/hello_world.go")
	if err != nil {
		fmt.Println("Error reading file:", err)
	}
	lexer := NewLexer(string(content))
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	PrintAST(program, "")

}
