#ifndef LIST_INCLUDED
#define LIST_INCLUDED

#ifndef WIN95
#define _fastcall
#endif

#ifdef ALLOCLISTS
void _fastcall alloclists(void);
#endif
listval_t _fastcall gtflist(list_t *);
int _fastcall cmflist(list_t,list_t);
int _fastcall comlistm(list_t,list_t,int mask);
int _fastcall comlist(list_t,list_t);
void _fastcall cpylistm(list_t,list_t *, int mask);
void _fastcall cpylist(list_t,list_t *);
void _fastcall catlist(list_t *, list_t *);
void _fastcall ecatlist(list_t *, list_t *);
void _fastcall mrflist(list_t *, list_t *, int skip);
int _fastcall mrglist(list_t, list_t *);
int _fastcall andlist(list_t,list_t,list_t *);
int _fastcall addlist(listval_t,list_t *);
int _fastcall adflist(listval_t,list_t *);
int _fastcall aeflist(listval_t,list_t *);
int _fastcall dlflist(listval_t,list_t *);
int _fastcall dellist(listval_t,list_t *);
int _fastcall dellistm(listval_t,list_t *, unsigned int);
void _fastcall killist(list_t *);
int _fastcall cntlist(list_t *);
int _fastcall newlist(list_t,list_t);
int _fastcall inlist(listval_t,list_t *);
int _fastcall inlistm(listval_t,list_t *, unsigned int);
int _fastcall inflist(listval_t,list_t *);
list_t _fastcall unflist(list_t);
list_t revlist(list_t *head);
int numlistused(void);
#endif
