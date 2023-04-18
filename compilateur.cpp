//  A compiler from a very simple Pascal-like structured language LL(k)
//  to 64-bit 80x86 Assembly langage
//  Copyright (C) 2019 Pierre Jourlin
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Build with "make compilateur"


#include <string>
#include <iostream>
#include <cstdlib>
#include <set>
#include <FlexLexer.h>
#include "tokeniser.h"
#include <cstring>

using namespace std;

enum OPREL {EQU, DIFF, INF, SUP, INFE, SUPE, WTFR};
enum OPADD {ADD, SUB, OR, WTFA};
enum OPMUL {MUL, DIV, MOD, AND ,WTFM};
enum TYPE {UNSIGNED_INT, INT, DOUBLE, BOOL, STRING, EXPR, IDENTIFIER, ERR};

TOKEN current;				// Current token


FlexLexer* lexer = new yyFlexLexer; // This is the flex tokeniser
// tokens can be read using lexer->yylex()
// lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h)
// and lexer->YYText() returns the lexicon entry as a string

	
set<string> DeclaredVariables;
unsigned long TagNumber=0;

bool IsDeclared(const char *id){
	return DeclaredVariables.find(id)!=DeclaredVariables.end();
}


void Error(string s){
	cerr << "Ligne n°"<<lexer->lineno()<<", lu : '"<<lexer->YYText()<<"'("<<current<<"), mais ";
	cerr<< s << endl;
	exit(-1);
}

// Program := [DeclarationPart] StatementPart
// DeclarationPart := "[" Letter {"," Letter} "]"
// StatementPart := Statement {";" Statement} "."
// Statement := AssignementStatement
// AssignementStatement := Letter "=" Expression

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
// SimpleExpression := Term {AdditiveOperator Term}
// Term := Factor {MultiplicativeOperator Factor}
// Factor := Number | Letter | "(" Expression ")"| "!" Factor
// Number := Digit{Digit}

// AdditiveOperator := "+" | "-" | "||"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// Letter := "a"|...|"z"
	
		
enum TYPE Identifier(void){
	cout << "\tpush "<<lexer->YYText()<<endl;
	current=(TOKEN) lexer->yylex();
	return UNSIGNED_INT;
}

enum TYPE Number(void){
	cout <<"\tpush $"<<atoi(lexer->YYText())<<endl;
	current=(TOKEN) lexer->yylex();
	return INT;
}

enum TYPE Expression(void);			// Called by Term() and calls Term()

enum TYPE Factor(void){
	if(current==RPARENT){
		current=(TOKEN) lexer->yylex();
		Expression();
		if(current!=LPARENT)
			Error("')' était attendu");		// ")" expected
		else
			current=(TOKEN) lexer->yylex();
			return EXPR;
 
	}
	else {
		if (current==NUMBER) {
			Number();
			return INT;
		}
		else
			if(current==ID) {
				Identifier();
				return IDENTIFIER;
			}
			else
				Error("'(' ou chiffre ou lettre attendue");
	}
}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator(void){
	OPMUL opmul;
	if(strcmp(lexer->YYText(),"*")==0)
		opmul=MUL;
	else if(strcmp(lexer->YYText(),"/")==0)
		opmul=DIV;
	else if(strcmp(lexer->YYText(),"%")==0)
		opmul=MOD;
	else if(strcmp(lexer->YYText(),"&&")==0)
		opmul=AND;
	else opmul=WTFM;
	current=(TOKEN) lexer->yylex();
	return opmul;
}

// Term := Factor {MultiplicativeOperator Factor}
enum TYPE Term(void){
	OPMUL mulop;
	Factor();
	while(current==MULOP){
		mulop=MultiplicativeOperator();		// Save operator in local variable
		enum TYPE type = Factor();
		cout << "\tpop %rbx"<<endl;	// get first operand
		cout << "\tpop %rax"<<endl;	// get second operand
		switch(mulop){
			case AND:
				cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
				cout << "\tpush %rax\t# AND"<<endl;	// store result
				break;
			case MUL:
				cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
				cout << "\tpush %rax\t# MUL"<<endl;	// store result
				break;
			case DIV:
				cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
				cout << "\tdiv %rbx"<<endl;			// quotient goes to %rax
				cout << "\tpush %rax\t# DIV"<<endl;		// store result
				break;
			case MOD:
				cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
				cout << "\tdiv %rbx"<<endl;			// remainder goes to %rdx
				cout << "\tpush %rdx\t# MOD"<<endl;		// store result
				break;
			default:
				Error("opérateur multiplicatif attendu");
				return ERR;
			return type;
		}
	}
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator(void){
	OPADD opadd;
	if(strcmp(lexer->YYText(),"+")==0)
		opadd=ADD;
	else if(strcmp(lexer->YYText(),"-")==0)
		opadd=SUB;
	else if(strcmp(lexer->YYText(),"||")==0)
		opadd=OR;
	else opadd=WTFA;
	current=(TOKEN) lexer->yylex();
	return opadd;
}

// SimpleExpression := Term {AdditiveOperator Term}
enum TYPE SimpleExpression(void){
	OPADD adop;
	enum TYPE type = Term();
	while(current==ADDOP){
		adop=AdditiveOperator();		// Save operator in local variable
		Term();
		cout << "\tpop %rbx"<<endl;	// get first operand
		cout << "\tpop %rax"<<endl;	// get second operand
		switch(adop){
			case OR:
				cout << "\taddq	%rbx, %rax\t# OR"<<endl;// operand1 OR operand2
				break;			
			case ADD:
				cout << "\taddq	%rbx, %rax\t# ADD"<<endl;	// add both operands
				break;			
			case SUB:	
				cout << "\tsubq	%rbx, %rax\t# SUB"<<endl;	// substract both operands
				break;
			default:
				Error("opérateur additif inconnu");
				return ERR;
		}
		cout << "\tpush %rax"<<endl;			// store result
		return type;
	}

}

// DeclarationPart := "[" Ident {"," Ident} "]"
void DeclarationPart(void){
	if(current!=RBRACKET)
		Error("caractère '[' attendu");
	cout << "\t.data"<<endl;
	cout << "\t.align 8"<<endl;
	
	current=(TOKEN) lexer->yylex();
	if(current!=ID)
		Error("Un identificater était attendu");
	cout << lexer->YYText() << ":\t.quad 0"<<endl;
	DeclaredVariables.insert(lexer->YYText());
	current=(TOKEN) lexer->yylex();
	while(current==COMMA){
		current=(TOKEN) lexer->yylex();
		if(current!=ID)
			Error("Un identificateur était attendu");
		cout << lexer->YYText() << ":\t.quad 0"<<endl;
		DeclaredVariables.insert(lexer->YYText());
		current=(TOKEN) lexer->yylex();
	}
	if(current!=LBRACKET)
		Error("caractère ']' attendu");
	current=(TOKEN) lexer->yylex();
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
OPREL RelationalOperator(void){
	OPREL oprel;
	if(strcmp(lexer->YYText(),"==")==0)
		oprel=EQU;
	else if(strcmp(lexer->YYText(),"!=")==0)
		oprel=DIFF;
	else if(strcmp(lexer->YYText(),"<")==0)
		oprel=INF;
	else if(strcmp(lexer->YYText(),">")==0)
		oprel=SUP;
	else if(strcmp(lexer->YYText(),"<=")==0)
		oprel=INFE;
	else if(strcmp(lexer->YYText(),">=")==0)
		oprel=SUPE;
	else oprel=WTFR;
	current=(TOKEN) lexer->yylex();
	return oprel;
}

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
enum TYPE Expression(void){
	OPREL oprel;
	enum TYPE type = SimpleExpression();
	if(current==RELOP){
		oprel=RelationalOperator();
		if (oprel != EQU && oprel != DIFF && oprel != INF && oprel != SUP
			&& oprel != INFE && oprel != SUPE && oprel != WTFR) { // Return BOOL type if there is no relational operator
			type = BOOL;
		}
		SimpleExpression();
		cout << "\tpop %rax"<<endl;
		cout << "\tpop %rbx"<<endl;
		cout << "\tcmpq %rax, %rbx"<<endl;
		switch(oprel){
			case EQU:
				cout << "\tje Vrai"<<++TagNumber<<"\t# If equal"<<endl;
				break;
			case DIFF:
				cout << "\tjne Vrai"<<++TagNumber<<"\t# If different"<<endl;
				break;
			case SUPE:
				cout << "\tjae Vrai"<<++TagNumber<<"\t# If above or equal"<<endl;
				break;
			case INFE:
				cout << "\tjbe Vrai"<<++TagNumber<<"\t# If below or equal"<<endl;
				break;
			case INF:
				cout << "\tjb Vrai"<<++TagNumber<<"\t# If below"<<endl;
				break;
			case SUP:
				cout << "\tja Vrai"<<++TagNumber<<"\t# If above"<<endl;
				break;
			default:
				Error("Opérateur de comparaison inconnu");
				return ERR;
		}
		cout << "\tpush $0\t\t# False"<<endl;
		cout << "\tjmp Suite"<<TagNumber<<endl;
		cout << "Vrai"<<TagNumber<<":\tpush $0xFFFFFFFFFFFFFFFF\t\t# True"<<endl;	
		cout << "Suite"<<TagNumber<<":"<<endl;
		return type;
	}
}

// AssignementStatement := Identifier ":=" Expression
enum TYPE AssignementStatement(void){
	string variable;
	if(current!=ID)
		Error("Identificateur attendu");
	if(!IsDeclared(lexer->YYText())){
		cerr << "Erreur : Variable '"<<lexer->YYText()<<"' non déclarée"<<endl;
		exit(-1);
	}
	variable=lexer->YYText();
	current=(TOKEN) lexer->yylex();
	if(current!=ASSIGN)
		Error("caractères ':=' attendus");
	current=(TOKEN) lexer->yylex();
	enum TYPE type = Expression();
	if (strcmp(lexer->YYText(),variable.c_str())) {
		Error("La variable et l'expression ne sont pas du meme type.");
	}
	else {
		cout << "\tpop "<<variable<<endl;
	}
}

void Statement(void);			// Called by Statements functions


// WhileStatement := "WHILE" Expression "DO" Statement
void WhileStatement(void) {
	unsigned long long etiquette;
	current = (TOKEN) lexer->yylex();
	if(!strcmp(lexer->YYText(),"WHILE")) {
		cout << "\tcmp $0, %rax"<<endl;
		enum TYPE type = Expression();
		if (!type == BOOL) {
			Error("L'expression n'est pas de type BOOL.");
		}
		cout << "\tjmp FinWhile" << etiquette;
	}
	else
		Error("mot-cle IF attendu");
	current = (TOKEN) lexer->yylex(); 
	if(!strcmp(lexer->YYText(),"DO")) {
		Statement();
		cout << "\tjmp DebutWhile" << etiquette << endl;
	}
}

// ForStatement := "FOR" AssignementStatement "To" Expression "DO" Statement
void ForStatement(void) {
	unsigned long long etiquette;
	current = (TOKEN) lexer->yylex();
	if(!strcmp(lexer->YYText(),"FOR")) {
		cout << "\tmov $0, %rax"<<endl;
		AssignementStatement();
	}
	else
		Error("mot-cle FOR attendu");
	current = (TOKEN) lexer->yylex();
	if(!strcmp(lexer->YYText(),"TO")) {
		cout << "\tcmp $0, %rax"<<endl;
		Expression();
	}
	else
		Error("mot-cle To attendu");
	current = (TOKEN) lexer->yylex();
	if(!strcmp(lexer->YYText(),"DO")) {
		cout << "\tja FinFor"<< etiquette << endl;
		Statement();
		cout << "\tjmp BoucleFor" << etiquette << endl;
	}
	else
		Error("mot-cle DO attendu");
}

// BlockStatement := "BEGIN" Statement { ";" Statement } "END"
void BlockStatement(void) {
	unsigned long long etiquette;
	current = (TOKEN) lexer->yylex();
	if(!strcmp(lexer->YYText(),"BEGIN")) {
		cout << "\tmov $0, %rax"<<endl;
		Statement();
	}
	else
		Error("mot-cle BEGIN attendu");
	current = (TOKEN) lexer->yylex(); // Read LCURLY
	if(!strcmp(lexer->YYText(),"{")) {
		cout << "\t;" << endl;
		Statement();
		current = (TOKEN) lexer->yylex(); // Read RCURLY
		cout << "\tEND" << endl;
	}
	else {
		Error("mot-cle {} attendu");
	}
}

// Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
void Statement(void){
	AssignementStatement();
	WhileStatement();
	ForStatement();
	BlockStatement();
}

// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]

void IfStatement(void) {
	unsigned long long etiquette;
	current = (TOKEN) lexer->yylex();
	if(!strcmp(lexer->YYText(),"IF")) {
		enum TYPE type = Expression();
		if (type != BOOL) {
			Error("L'expression n'est pas de type BOOL.");
		}
	}
	else
		Error("mot-cle IF attendu");
	current = (TOKEN) lexer->yylex();
	if(!strcmp(lexer->YYText(),"THEN"))
		Statement();
	else
		Error("mot-cle THEN attendu");
	current = (TOKEN) lexer->yylex();
	if(current==RBRACKET) {
		if(!strcmp(lexer->YYText(),"ELSE")) {
			Statement();
			current = (TOKEN) lexer->yylex(); // Read RBRACKET
		}
	}
}


// StatementPart := Statement {";" Statement} "."
void StatementPart(void){
	cout << "\t.text\t\t# The following lines contain the program"<<endl;
	cout << "\t.globl main\t# The main function must be visible from outside"<<endl;
	cout << "main:\t\t\t# The main function body :"<<endl;
	cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top"<<endl;
	Statement();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();
		Statement();
	}
	if(current!=DOT)
		Error("caractère '.' attendu");
	current=(TOKEN) lexer->yylex();
}

// Program := [DeclarationPart] StatementPart
void Program(void){
	if(current==RBRACKET)
		DeclarationPart();
	StatementPart();	
}


int main(void){	// First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	cout << "\t\t\t# This code was produced by the CERI Compiler"<<endl;
	// Let's proceed to the analysis and code production
	current=(TOKEN) lexer->yylex();
	Program();
	// Trailer for the gcc assembler / linker
	cout << "\tmovq %rbp, %rsp\t\t# Restore the position of the stack's top"<<endl;
	cout << "\tret\t\t\t# Return from main function"<<endl;
	if(current!=FEOF){
		cerr <<"Caractères en trop à la fin du programme : ["<<current<<"]";
		Error("."); // unexpected characters at the end of program
	}
}
		
			





