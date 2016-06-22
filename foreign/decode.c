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


int init_automata(struct automata* fap, char* message)
{
    fap->state = 0;
    fap->index = 0;
    bzero(fap->buff, 6);
    fap->b_index = 0;
    fap->message = message;
    fap->new_message = (int*) malloc(sizeof(int) * strlen(message));
    bzero(fap->new_message, strlen(message));
    return 0;
}

int next(struct automata* fap)
{
    switch(fap->state)
    {
        case 0:
            if(*(fap->message)== '\\')
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
        case 3:
            if(isxdigit(*(fap->message)))
            {
                fap->buff[fap->b_index++] = *(fap->message);
            }
            else if (fap->b_index > 3 && fap->b_index < 6)
            {   
                short replaced = *(fap->message) == '\\';
                fap->new_message[fap->index++] = *(fap->message);
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
            break;
        default:
            return -1;
    }
    (fap->message)++;
    return fap->index;
}

int* decode(char* message)
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

int main(int argc, char const *argv[])
{
    char* example = "error:\\u041d\\u0435\\u0432\\u0435\\u0440\\u043d\\u044b\\u0439\\u100cc";
    int* res = decode(example);
    int i;
    for (i=0; i < 10; i++)
    {
        if (res[i] < 256)
        {
            printf("%c",(char)res[i]);
        }
        else
        {
            printf("%d\n", res[i]);
        }
    }


    return 0;
}