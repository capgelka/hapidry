// #include <stdio.h>
// #include <stdlib.h>
// #include <string.h>
// #include <ctype.h>
#include "udecode.h"

// struct automata
// {
//     int state;
//     int index;
//     char buff[6]; // = {0};
//     short b_index;
//     char* message;
//     int* new_message;  
// };


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
            else
            {
                // printf("LOOK: %c\n", *(fap->message));
                short replaced = *(fap->message) == '\\';
                fap->state = replaced ? 2 : 1;
                fap->new_message[fap->index++] = '\\';
                fap->new_message[fap->index++] = 'u';
                // printf(" !!b %d\n", fap->b_index);
                if (fap->b_index)
                {
                    int i = 0;
                    do
                    {
                        fap->new_message[fap->index++] = fap->buff[i];
                        // printf(" b %d\n", i);
                    } while(++i < fap->b_index);
                    bzero(fap->buff, 6);
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

char* udecode(char* message)
{
    int i;
    struct automata fa;
    init_automata(&fa, message);
    for(i=0; i<strlen(message); i++)
    {
        next(&fa);
    }
    char* decoded =  (char*) malloc(sizeof(char) * strlen(message));
    char* tmptr;
    int j = 0;
    // printf("%d %d\n", i, fa.index );
    for (i=0; i < fa.index; i++)
    {
        // printf("%d %d\n", i, fa.index );
        if (fa.new_message[i] < 256)
        {
            decoded[j++] = (char)fa.new_message[i];
            // printf("%s %s\n", );
        }
        else 
        {
            tmptr = (char*)(fa.new_message + i);
            // printf("%d % d %d %d \n", tmptr[0], tmptr[1], tmptr[2], tmptr[3]);
            decoded[j++] = tmptr[1];
            decoded[j++] = tmptr[0];
        }
    }
    decoded[j++] = '\0';
    free(fa.new_message);
    return decoded;
}



// int main(int argc, char const *argv[])
// {
//     char* example = "just ascii: 1-9*&\\//\\0 \\urrwe\\ufeg eee";// "error:\\u041d\\u0435\\u0432\\u0435\\u0440\\u043d\\u044b\\u0439\\u100cc";
//     char* res = udecode(example);
//     // int i;
//     printf("%s\n", res);
//     // for (i=0; i < 15; i++)
//     // {
//     //     if (res[i] < 256)
//     //     {
//     //         printf("%c -- %d\n",(char)res[i], i);
//     //     }
//     //     else
//     //     {
//     //         printf("%d -- %d\n", res[i], i);
//     //     }
//     // }


//     return 0;
// }