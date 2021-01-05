/* number of liberties equivalent to making life */
#define LIVELIBERTIES 12
/* semeai move evaluation value for each liberty (MUST BE AT LEAST 8) */
#define LIBVALUE 8
/* number of liberties to add for not being able to approach */
#define CANTAPPROACH 128
#define LIBMASK 0x7f

extern int semrunlibs[NUMRUN]; /* how many new liberties per running type */
extern int semrunminlibs[NUMRUN]; /* minimum extra liberties for running here */
