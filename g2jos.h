#ifndef JOS_INCLUDED
#define JOS_INCLUDED

#include "g2types.h"


typedef unsigned int josindex;  /* 32 bit index of joseki pointer */


/* for josekis[].jflag[] */

# define NORM 0
# define URGT 1
# define BAD 2
# define COMP 3
# define THFR 4
# define FRFR 5
# define SHIM 6
# define FOLL 7
# define TRIK 8
# define IGNR 9
# define KAKA 10
# define THTH 11
# define FRKA 12
# define PINC 13
# define LNRM 14
# define LFOL 15

/* from g2jos.c joseki routines */

int isjoseki(sqr_t s, int corner, int color);
sqr_t getonejoseki(int corner, int color, int num);
int josekiincorner(sqr_t s);
void jupdate(sqr_t s,int c);
void joseki(int color);
void initjosreasons(void);
void jupdatec(sqr_t s,int c,int corner);
void getxyjlib(josindex,int *x,int *y);
unsigned int getflag(josindex);
unsigned int j2next(josindex);
int sibling(josindex);
int nextjos(sqr_t sqr, int corner, int color);
int firstjos(int corner, int color);
int getlastcolor(int corner);
unsigned int j2more(unsigned int ptr);
#endif

