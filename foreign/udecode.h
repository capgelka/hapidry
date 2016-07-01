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
    int m_length;
    uint16_t* new_message;  
};


inline int init_automata(struct automata* fap, char* message);

inline int next(struct automata* fap);

uint16_t* udecode(char* message);
