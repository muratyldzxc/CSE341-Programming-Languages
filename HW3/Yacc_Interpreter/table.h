#ifndef TABLE_H
#define TABLE_H

#include <stdlib.h>
#include <string.h>

typedef struct
{
	char name[30];
	int value;
}Variable;

typedef struct
{
	Variable* variables;
	int size;
	int capacity;
}Table;

Table symbolTable;

void createTable()
{
	symbolTable.size = 0;
	symbolTable.capacity = 2;
	symbolTable.variables = (Variable*)malloc(sizeof(Variable) * 2);
}

// returns the index where variable indexed
// returns -1 if variable is not at the symbol table
int indexOfVariable(char* name)
{
	for(int i=0 ; i < symbolTable.size ; i++)
		if(strcmp(symbolTable.variables[i].name, name) == 0) //  if variable in symbol table
			return i;
	return -1;
}

Variable getVariable(char* name)
{
	int index = indexOfVariable(name);

	if(index != -1)
		return symbolTable.variables[index];

	Variable v;
	strcpy(v.name, name);
	v.value = 50;

	return v;
}


void addToTable(char* name, int value)
{	
	int index = indexOfVariable(name);

	if( index == -1){

		if(symbolTable.size >= symbolTable.capacity){
			symbolTable.capacity *= 2;
			symbolTable.variables = (Variable*)realloc(symbolTable.variables, sizeof(Variable) * symbolTable.capacity);
		}
		
		strcpy(symbolTable.variables[symbolTable.size].name, name);
		symbolTable.variables[symbolTable.size].value = value;
		symbolTable.size += 1;
	}
	else
		symbolTable.variables[index].value = value;

}


#endif