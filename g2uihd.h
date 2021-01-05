
/* copyright 1995 David Fotland */

/* declarations for basic g2 data structures 
 */
 
#ifndef G2UIHD_INCLUDED
#define G2UIHD_INCLUDED

// problem solving
#define SOLVEPROBLEM 1
#define SOLVEFULLPROBLEM 2
#define SOLVELDPROBLEM 3


/* move descriptions */  
extern unsigned int msptr;			/* next move number.  move stack pointer*/
extern sqr_t mvs[NUMMOVES];		/* move square for each move */
extern list_t mvcapt[NUMMOVES];		/* list of groups captured by move */
extern list_t mvconn[NUMMOVES];		/* list of group connected by move */
extern char mvcolor[NUMMOVES];		/* color of move */
extern short mvnext[NUMMOVES];		/* mvs index for next piece in group or -1 */
extern sqr_t kosave[NUMMOVES];		/* saved ko square */

/* groups */

extern short grpieces[NUMGROUPS];	/* move stack index of first piece in group */
extern char grcolor[NUMGROUPS];
extern group_t board[NUMSQUARES];
extern group_t maxgr;

/* lists */

#ifdef ALLOCLIST
extern listval_t *list;
extern list_t *link;
#else
extern listval_t list[NUMLIST];
extern list_t link[NUMLIST];
#endif


/* joseki */

extern int jflag[4];
extern int jreflect[4];
extern unsigned int jptr2[4];
extern int jcolor[4];


/* misc */

extern sqr_t boardsize;
extern sqr_t boardsquare;
extern char xval[NUMSQUARES];
extern char yval[NUMSQUARES];
extern unsigned int debug;
extern int komi;
extern int scoreval[NUMSQUARES];
extern int playlevel;
extern int showtactics;
extern char problemflag;
extern sqr_t kosquare;
extern list_t armygroups[NUMARMIES];   /* list of groups in army */
extern army_t grarmy[NUMGROUPS];     /* army number for group */
extern unsigned char gralive[NUMGROUPS];

#endif
