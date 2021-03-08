%{
	#include <stdio.h>
	#include <stdlib.h>
	#include "table.h"
	#include "List.h"


	int yyerror(char* s);
	int yylex();
	int isBinary(int num);
	int and(int a, int b);
	int or(int a, int b);
	int not(int a);
	int equal(int a, int b);

	extern FILE *yyout;
	extern FILE *yyin;
%}

%union
{
	int num;
	char id[30];
	void* values;
}

%start INPUT

%token	KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET
		KW_DEFFUN KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE
		OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_DBLMULT OP_OC OP_CC OP_COMMA
		COMMENT

%token <num> VALUE
%token <id> IDENTIFIER

%type <num> INPUT
%type <num> EXPI
%type <num> EXPB
%type <values> EXPLISTI
%type <values> LISTVALUE
%type <values> VALUES


%%

INPUT:
	EXPI { printf("Syntax OK.\nResult: %d\n\n", $1);  fprintf(yyout,"Syntax OK.\nResult: %d\n\n", $1);}
	| 
	INPUT EXPI { printf("Syntax OK.\nResult: %d\n\n", $2); fprintf(yyout,"Syntax OK.\nResult: %d\n\n", $2);}
	| 
	EXPLISTI { printf("Syntax OK.\nResult: "); printList($1); printf("\n"); fprintf(yyout,"Syntax OK.\nResult: "); fprintList($1,yyout);  fprintf(yyout,"\n"); }
	|
	INPUT EXPLISTI { printf("Syntax OK.\nResult: "); printList($2); printf("\n"); fprintf(yyout,"Syntax OK.\nResult: "); fprintList($2,yyout); fprintf(yyout,"\n");}
	|
	INPUT COMMENT { printf("Syntax OK.\nResult: Comment\n\n"); fprintf(yyout,"Syntax OK.\nResult: Comment\n\n");}
	|
	COMMENT { printf("Syntax OK.\nResult: Comment\n\n"); fprintf(yyout,"Syntax OK.\nResult: Comment\n\n");}
	;

EXPI:
	VALUE {$$ = $1;}
	|
	IDENTIFIER  { 
					Variable var = getVariable($1); 
					$$ = var.value; 
				}
	|
	OP_OP OP_PLUS EXPI EXPI OP_CP {$$ = $3 + $4;}
	|
	OP_OP OP_MINUS EXPI EXPI OP_CP {$$ = $3 - $4;}
	|
	OP_OP OP_MULT EXPI EXPI OP_CP {$$ = $3 * $4;}
	|
	OP_OP OP_DIV EXPI EXPI OP_CP {$$ = $3 / $4;}
	|
	OP_OP KW_SET IDENTIFIER EXPI OP_CP { $$ = $4; addToTable($3, $4); } // (set Id EXPI)
	|
	OP_OP KW_IF EXPB EXPI OP_CP { 
									if($3 == 1 )
										$$ = $4;
									else
										$$ = 0;

								} 
	;

EXPB:
	KW_TRUE { $$ = 1; }
	|
	KW_FALSE { $$ = 0; }
	|
	OP_OP KW_AND EXPB EXPB OP_CP { $$ = and($3,$4); }
	|	
	OP_OP KW_OR EXPB EXPB OP_CP { $$ = or($3,$4); }
	|
	OP_OP KW_NOT EXPB OP_CP { $$ = not($3); }
	|
	OP_OP KW_EQUAL EXPB EXPB OP_CP { $$ = equal($3,$4); }
	|
	OP_OP KW_EQUAL EXPI EXPI OP_CP { $$ = equal($3,$4); }
	;	

EXPLISTI:
	OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP { $$ = concat($3, $4); }
	|
	OP_OP KW_APPEND EXPI EXPLISTI OP_CP { $$ = append($4, $3); }
	|
	OP_OP KW_IF EXPB EXPLISTI OP_CP { 
									if($3 == 1 )
										$$ = $4;
									else
										$$ = NULL;

								} 
	|
	LISTVALUE { $$ = $1; }
	;

LISTVALUE:
	OP_OP KW_LIST VALUES OP_CP { $$ = $3; }
	|
	OP_OP KW_LIST OP_CP { $$ = createList(); }
	|
	KW_NIL { $$ = NULL; }
	;

VALUES:
	VALUES VALUE { $$ = addToList($1, $2); }
	|
	VALUE { $$ = addToList(NULL, $1); }
	;

%%


int yyerror(char* s){
	printf("SYNTAX_ERROR Expression not recognized\n\n");
	fprintf(yyout,"SYNTAX_ERROR Expression not recognized\n\n");
	
}

int isBinary(int num){
	if(num == 0 || num == 1 ){
		return num; 
	}
	return -1;
}

int and(int a, int b){
	if(a == 1 && b == 1)
		return 1;
	return 0;
}

int or(int a, int b){
	if(a == 1 || b == 1)
		return 1;
	return 0;
}

int not(int a){
	if( a == 1)
		return 0;
	return 1;
}

int equal(int a, int b){
	if(a == b)
		return 1;
	return 0;
}

int main(int argc, char* argv[])
{
	FILE* istream;
	yyout = fopen("output.txt","w");
	
	if(argc == 2){
    	istream = fopen(argv[1], "r");

		if(istream == NULL){
			printf("\nFile can not be found!\n");
		}
		else{
			yyin = istream;
		}
    }
    else if(argc > 2){
    	printf("\nToo many arguments...\n");
    }

	createTable();
	yyparse();
	return 0;
}