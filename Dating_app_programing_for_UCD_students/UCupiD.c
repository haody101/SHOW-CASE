#include <stdio.h> // tutor Phillip helped.
#include <stdlib.h>
#include <string.h>
#include <math.h>

struct LinkListNode{
	int SID;
	char first_name[30];
	char sex;
	char sex_orientation;
	int age;
	int pol_leaning;
	char major_hobby[30];
	char minor_hobby[30];
	int height;
	int weight;
	struct LinkListNode* next;
} LinkListNode;

struct LinkedList{

	struct LinkListNode* head;
} LinkedList;

void insert(struct LinkedList* list, struct LinkListNode* elementToInsert)
{
	if(list->head == NULL)
	{
		list->head = elementToInsert;
	}
	else
	{	
		struct LinkListNode* current;
		struct LinkListNode* prev;

		for(current = list->head; current != NULL; current = current->next)
		{
			prev = current;
		}

		prev->next = elementToInsert;
	}
}

struct LinkListNode* Search(struct LinkedList list, int SID2)
{
	struct LinkListNode* current;

	for(current = list.head; current != NULL; current = current->next)
	{
		if(current->SID == SID2)
		{
			return current;
		}
	stdout,}
	return NULL;

}

void Match(struct LinkListNode* person, struct LinkedList list, FILE *fp)
{
	struct LinkListNode* current = malloc(sizeof(struct LinkListNode));

	fprintf(fp, "Matches for user %d (%s): \n", person -> SID, person->first_name);

	for(current = list.head; current != NULL; current = current->next)
	{
		if(current->SID == person->SID)
		{
			continue;
		}
		if(person->sex == current->sex_orientation && current->sex == person -> sex_orientation)
		{
			if(abs(current->age - person->age) <= 10 && (abs(current->pol_leaning - person->pol_leaning) <= 2) )
			{
					fprintf(fp,"- %d (%s)\n", current->SID, current->first_name);
			}
			else if(strcmp(current->major_hobby, person->major_hobby) == 0 && strcmp(current->minor_hobby, person->minor_hobby) == 0)
			{	

				fprintf(fp,"- %d (%s)\n", current->SID, current->first_name);
			}
			else if(abs(current -> height - person -> height) < (.10 * person->height) && abs(current->weight - person->weight) < (.10 * person-> weight))
			{	

				fprintf(fp,"- %d (%s)\n", current->SID, current->first_name);
			}
		}
	}
}

void Read_Fun (char* line, FILE* f, struct LinkedList* list) {

	while(fgets(line, 256, f)){

		struct LinkListNode* entry = malloc(1 * sizeof(struct LinkListNode));
		
		sscanf(line, "%d,%[^,],%c,%c,%d,%d,%[^,],%[^,],%d,%d",
				&entry->SID,
				entry->first_name,
				&entry->sex, 
				&entry->sex_orientation, 
				&entry->age, 
				&entry->pol_leaning, 
				entry->major_hobby,
				entry->minor_hobby, 
				&entry->height, 
				&entry->weight);

		insert(list, entry);
	
	}
}



void Purge_List(struct LinkedList* l){
	struct LinkListNode* current = l->head;
	struct LinkListNode* temp;
	do{
		temp = current -> next;
		free(current);
		current = temp;
	} while( current != NULL);
	
	free(temp);
}

int main(int argc, char** argv)
{

	FILE* f = fopen(argv[1], "r");
	FILE* fp;
	fp = fopen(filename,"w");
	char* line = malloc(256 * sizeof(char));
	struct LinkedList list;
	char filename[30];		
	strcpy(filename, "match_");
	strcat(filename, argv[2]);
	strcat(filename, ".txt");
	list.head = NULL;
	
	if(argc < 3 || fp == NULL)
	{
		puts("Usage: ./UCupiD db_file SID");
		exit(1);
	}
	if(f == NULL || fp == NULL)
	{
		fprintf(stdout,"Error: cannot open %s\n", argv[1]);
		return 1;
	}

	
	Read_Fun(line, f, &list);

	struct LinkListNode* found = Search(list, atoi(argv[2]));
	if(found != NULL)
	{
		Match(found, list,fp);
	}
	else{
		printf("User with SID %s not found\n", argv[2] );
		return 1;
	}
	Purge_List(&list);
	fclose(fp);
	fclose(f);
	return 0;
}
