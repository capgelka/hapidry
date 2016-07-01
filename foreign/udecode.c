#include "udecode.h"

#define DEFAULT 0
#define ON_SLASH 1
#define ON_UNICODE 2
#define BUFFSIZE 6

inline int init_automata(struct automata* fap, char* message)
{
    fap->state = DEFAULT;
    fap->index = 0;
    bzero(fap->buff, BUFFSIZE);
    fap->b_index = 0;
    fap->message = message;
    fap->new_message = (uint16_t*) malloc(sizeof(uint16_t) * strlen(message));
    fap->m_length = strlen(message);
    bzero(fap->new_message, fap->m_length);
    return 0;
}

inline int next(struct automata* fap)
{
    switch(fap->state)
    {
        case DEFAULT:
            if(*(fap->message) == '\\')
            {
                fap->state = ON_SLASH;
            } else 
            {
                fap->new_message[fap->index++] = *(fap->message);
            }
            break;
        case ON_SLASH:
            if(*(fap->message) == 'u')
            {
                fap->state = ON_UNICODE;
            }
            else
            {
                fap->state = DEFAULT;
                fap->new_message[fap->index++] = '\\';
                fap->new_message[fap->index++] = *(fap->message);
            }
            break;
        case ON_UNICODE:
            if(isxdigit(*(fap->message)))
            {
                fap->buff[fap->b_index++] = *(fap->message);
            }
            else if (fap->b_index > 3 && fap->b_index < BUFFSIZE)
            {   
                if (*(fap->message) == '\\')
                {
                    fap->buff[fap->b_index++] = '\0';    
                    fap->new_message[fap->index++] =(int) strtol(fap->buff, NULL, 16);
                    bzero(fap->buff, BUFFSIZE);
                    fap->b_index = 0;
                    fap->state = ON_SLASH;
                } else
                {
                    fap->buff[fap->b_index++] = '\0';    
                    fap->new_message[fap->index++] =(int) strtol(fap->buff, NULL, 16);
                    bzero(fap->buff, BUFFSIZE);
                    fap->b_index = 0;
                    fap->new_message[fap->index++] = *(fap->message);
                    fap->state = DEFAULT;
                }
            }
            else
            {
                uint8_t replaced = *(fap->message) == '\\';
                fap->state = replaced ? ON_SLASH : DEFAULT;
                fap->new_message[fap->index++] = '\\';
                fap->new_message[fap->index++] = 'u';
                if (fap->b_index)
                {
                    uint8_t i = 0;
                    do
                    {   
                        fap->new_message[fap->index++] = fap->buff[i];
                      
                    } while(++i < fap->b_index);
                    bzero(fap->buff, BUFFSIZE);
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
    struct automata fa;
    init_automata(&fa, message);
    for(int i=0; i<fa.m_length; i++)
    {
        next(&fa);
    }
    return fa.new_message;
}