#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


struct automata
{
    int state;
    int index;
    char buff[6]; // = {0};
    short b_index;
    char* message;
    int* new_message;  
};


int init_automata(struct automata* fap, char* message);

int next(struct automata* fap);

char* udecode(char* message);
