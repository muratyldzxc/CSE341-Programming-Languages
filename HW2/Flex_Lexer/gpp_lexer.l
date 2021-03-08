%{ 
#include <stdio.h>
#include "tokens.h"
#include <string.h>

int op_oc = 0;

%} 

%%

[aA][nN][dD]					return KW_AND;
[oO][rR]						return KW_OR;
[nN][oO][tT]					return KW_NOT;
[eE][qQ][uU][aA][lL] 			return KW_EQUAL;
[lL][eE][sS][sS]				return KW_LESS;
[nN][iI][lL]					return KW_NIL;
[lL][iI][sS][tT]				return KW_LIST;
[aA][pP][pP][eE][nN][dD]		return KW_APPEND;
[cC][oO][nN][cC][aA][tT]		return KW_CONCAT;
[sS][eE][tT]					return KW_SET;
[dD][eE][fF][fF][uU][nN]		return KW_DEFFUN;
[fF][oO][rR]					return KW_FOR;
[iI][fF]						return KW_IF;
[eE][xX][iI][tT]				return KW_EXIT;
[lL][oO][aA][dD]				return KW_LOAD;
[dD][iI][sS][pP]				return KW_DISP;
[tT][rR][uU][eE]				return KW_TRUE;
[fF][aA][lL][sS][eE]			return KW_FALSE;

[+] 							return OP_PLUS;
[-] 							return OP_MINUS;
[/] 							return OP_DIV;
[*][*] 							return OP_DBLMULT;
[*] 							return OP_MULT;
[(] 							return OP_OP;
[)] 							return OP_CP;
["] 							{
									int temp = op_oc;
									if(op_oc == 0)
					    				op_oc = 1;
					    			else if(op_oc == 1)
					    				op_oc = 0;
		
									return (OP_OC + temp);
								}

[,]								return OP_COMMA;

[;][;] 							return COMMENT;

[a-zA-Z_][a-zA-Z0-9_]*			return IDENTIFIER;

[0]|[1-9][0-9]*					return VALUE;		   



[\n]							return NEWLINE;
[ \t]+							return WHITESPACE;
.								return UNKNOWN;

%% 

int yywrap(){}

int main(int argc, char *argv[]){

	char tokens[][15] = {
						 "", "KW_AND", "KW_OR", "KW_NOT", "KW_EQUAL", "KW_LESS", "KW_NIL", "KW_LIST", "KW_APPEND", "KW_CONCAT",
						 "KW_SET", "KW_DEFFUN", "KW_FOR","KW_IF", "KW_EXIT", "KW_LOAD", "KW_DISP", "KW_TRUE", "KW_FALSE",
    					 "OP_PLUS", "OP_MINUS", "OP_DIV", "OP_MULT", "OP_OP", "OP_CP", "OP_DBLMULT",
    					 "OP_OC", "OP_CC", "OP_COMMA", "COMMENT", "IDENTIFIER", "VALUE", "NEWLINE", "WHITESPACE"
    					};
    FILE *fpi;
    FILE *fpo = fopen("parsed_cpp.txt", "w+");
    int starting = 1;
    int empty = 1;
    int file_given = 0;

    if(argc == 2){
    	fpi = fopen(argv[1], "r");

		if(fpi == NULL){
			printf("\nFile can not be found!\n");
			starting = 0;
		}
		else{
			yyin = fpi;
			file_given = 1;
		}
    }
    else if(argc > 2){
    	printf("\nToo many arguments...\n");
    	starting = 0;
    }

    int finish = 0;

    int token_code = NEWLINE;

    
    while(starting && token_code > 0 && !finish){

    	if(token_code == NEWLINE && !file_given){
    		printf("\n> ");
    		empty = 1;
    	}

    	token_code = yylex();

    	if(token_code == NEWLINE && !file_given && empty)
    		finish = 1;
	
		if(token_code != NEWLINE)
    		empty = 0;
    		
		if(token_code == COMMENT){

			printf("%s\n", tokens[token_code]);
			fprintf(fpo,"%s\n", tokens[token_code]);
			token_code = yylex();

			while(token_code != NEWLINE && token_code > 0)
				token_code = yylex();	
		}
		else if(token_code > 0 && token_code != WHITESPACE && token_code != NEWLINE){
			char err_text [250];
			int Ttoken_code = 0;
			int err = 0;
			if(token_code == VALUE || token_code == IDENTIFIER || token_code == UNKNOWN){
				
				strcpy(err_text, yytext);
				Ttoken_code = yylex();

				if((Ttoken_code > 0 && Ttoken_code != WHITESPACE && Ttoken_code != NEWLINE && Ttoken_code != COMMENT && !(Ttoken_code > 18 && Ttoken_code < 29)) || token_code == UNKNOWN)
					err = 1;
				else
					err = 0;

				while(Ttoken_code > 0 && Ttoken_code != WHITESPACE && Ttoken_code != NEWLINE && Ttoken_code != COMMENT && !(Ttoken_code > 18 && Ttoken_code < 29 )){
					strcat(err_text, yytext);
					Ttoken_code = yylex();
				}

				

			}
			
			if(err){

				printf("SYNTAX ERROR %s cannot be tokenized\n", err_text);
				fprintf(fpo,"SYNTAX ERROR %s cannot be tokenized\n", err_text);

				if(Ttoken_code > 18 && Ttoken_code < 29){
					printf("%s\n", tokens[Ttoken_code]);
					fprintf(fpo,"%s\n", tokens[Ttoken_code]);
				}
				else if(Ttoken_code == NEWLINE && !file_given){
					printf("\n> ");
	    			empty = 1;
				}
				else if(Ttoken_code == COMMENT){

					printf("%s\n", tokens[Ttoken_code]);
					fprintf(fpo,"%s\n", tokens[Ttoken_code]);
					token_code = yylex();

					while(token_code != NEWLINE && token_code > 0)
						token_code = yylex();	
				}
			}
			else{
				printf("%s\n", tokens[token_code]);
				fprintf(fpo,"%s\n", tokens[token_code]);

				if(Ttoken_code > 18 && Ttoken_code < 29){
					printf("%s\n", tokens[Ttoken_code]);
					fprintf(fpo,"%s\n", tokens[Ttoken_code]);
				}
				else if(Ttoken_code == NEWLINE && !file_given){
					printf("\n> ");
	    			empty = 1;
				}
				else if(Ttoken_code == COMMENT){

					printf("%s\n", tokens[Ttoken_code]);
					fprintf(fpo,"%s\n", tokens[Ttoken_code]);
					token_code = yylex();

					while(token_code != NEWLINE && token_code > 0)
						token_code = yylex();	
				}
			}			
    		
		}		
			
    }
   
    return 0;
}