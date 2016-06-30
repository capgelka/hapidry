#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>


struct automata
{
    uint8_t state;
    int index;
    char buff[6];
    uint8_t b_index;
    char* message;
    uint16_t* new_message;  
};


int init_automata(struct automata* fap, char* message);

int next(struct automata* fap);

uint16_t* udecode(char* message);
