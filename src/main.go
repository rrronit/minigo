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
	for {
		token, err := lexer.Next()
		if err != nil {
			fmt.Println("Error:", err)
			break
		}
		if token.typ == T_EOF {
			break
		}
		fmt.Println(token)
	}

}
