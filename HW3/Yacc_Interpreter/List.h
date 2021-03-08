#ifndef LIST_H
#define LIST_H

typedef struct
{
	int* values;
	int size;
	int capacity;
}List;


void* createList()
{
	List* list = (List*)malloc(sizeof(List));

	list->size = 0;
	list->capacity = 1;
	list->values = (int*)malloc(sizeof(int) * 2);

	return list;
}

void* addToList(void* list, int item)
{

	if(list == NULL)
		list = createList();

	List * castedList = list;

	if(castedList->size == castedList->capacity){
		castedList->capacity *= 2;
		castedList->values = (int*)realloc(castedList->values, sizeof(int) * castedList->capacity);
	}

	castedList->values[castedList->size] = item;
	castedList->size += 1;

	return castedList;
	
}


void* concat(void* list1, void* list2)
{
	List* castedList;
	List* concatList = createList();

	if(list1 != NULL){

		castedList = list1;

		for(int i=0 ; i < castedList->size ; i++)
			addToList(concatList, castedList->values[i]);
	}

	if(list2 != NULL){

		castedList = list2;

		for(int i=0 ; i < castedList->size ; i++)
			addToList(concatList, castedList->values[i]);
	}

	return concatList;
}


List* append(void* list, int item)
{
	List* castedList;
	List* appendList = createList();

	addToList(appendList, item);


	if(list != NULL){

		castedList = list;

		for(int i=0 ; i < castedList->size ; i++)
			addToList(appendList, castedList->values[i]);
	}

	return appendList;
}

void printList(void* list)
{
	List* castedList;

	if(list == NULL)
		printf("NULL\n");

	else{

		castedList = list;

		if(castedList->size == 0)
			printf("NULL\n");
		else{
			printf("(");

			for(int i=0 ; i < castedList->size ; i++)
			{
				printf("%d",castedList->values[i]);

				if(i < castedList->size - 1)
					printf(" ");
			}
			printf(")\n");
		}
	}
	
}


void fprintList(void* list, FILE* ostream)
{
	List* castedList;

	if(list == NULL)
		fprintf(ostream,"NULL\n");

	else{

		castedList = list;

		if(castedList->size == 0)
			fprintf(ostream,"NULL\n");
		else{
			fprintf(ostream,"(");

			for(int i=0 ; i < castedList->size ; i++)
			{
				fprintf(ostream,"%d",castedList->values[i]);

				if(i < castedList->size - 1)
					fprintf(ostream," ");
			}
			fprintf(ostream,")\n");
		}
	}
	
}


#endif