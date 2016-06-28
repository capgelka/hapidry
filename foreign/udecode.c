#include "udecode.h"


int init_automata(struct automata* fap, char* message)
{
    fap->state = 0;
    fap->index = 0;
    bzero(fap->buff, 6);
    fap->b_index = 0;
    fap->message = message;
    fap->new_message = (uint16_t*) malloc(sizeof(uint16_t) * strlen(message));
    bzero(fap->new_message, strlen(message));
    return 0;
}

int next(struct automata* fap)
{
    switch(fap->state)
    {
        case 0:
            if(*(fap->message) == '\\')
            {
                fap->state = 2;
            } else 
            {
                fap->state = 1;
                fap->new_message[fap->index++] = *(fap->message);
            }
            break;
        case 1:
            if(*(fap->message) == '\\')
            {
                fap->state = 2;
            }
            else
            {
                fap->state = 1;
                fap->new_message[fap->index++] = *(fap->message);
            }
            break;
        case 2:
            if(*(fap->message)== 'u')
            {
                fap->state = 3;
            }
            else
            {
                fap->state = 1;
                fap->new_message[fap->index++] = '\\';
                fap->new_message[fap->index++] = *(fap->message);
            }
            break;
        case 3:
            if(isxdigit(*(fap->message)))
            {
                fap->buff[fap->b_index++] = *(fap->message);
            }
            else if (fap->b_index > 3 && fap->b_index < 6)
            {   
                short replaced = *(fap->message) == '\\';
                fap->state = replaced ? 2 : 1;
                if (replaced)
                {
                    fap->buff[fap->b_index++] = '\0';    
                    fap->new_message[fap->index++] =(int) strtol(fap->buff, NULL, 16);
                    bzero(fap->buff, 6);
                    fap->b_index = 0;
                } else
                {
                    fap->buff[fap->b_index++] = '\0';    
                    fap->new_message[fap->index++] =(int) strtol(fap->buff, NULL, 16);
                    bzero(fap->buff, 6);
                    fap->b_index = 0;
                    fap->new_message[fap->index++] = *(fap->message);
                }
            }
            else
            {
                short replaced = *(fap->message) == '\\';
                fap->state = replaced ? 2 : 1;
                fap->new_message[fap->index++] = '\\';
                fap->new_message[fap->index++] = 'u';
                if (fap->b_index)
                {
                    int i = 0;
                    do
                    {   
                        fap->new_message[fap->index++] = fap->buff[i];
                      
                    } while(++i < fap->b_index);
                    bzero(fap->buff, 6);
                    fap->b_index = 0;
                }
                if(!replaced)
                    fap->new_message[fap->index++] = *(fap->message);
            }
            break;
        default:
            return -1;
    }
    (fap->message)++;
    return fap->index;
}

uint16_t* udecode(char* message)
{
    int i;
    struct automata fa;
    init_automata(&fa, message);
    for(i=0; i<strlen(message); i++)
    {
        next(&fa);
    }
    return fa.new_message;
}