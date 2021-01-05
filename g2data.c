/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* declaration of global data structures for go engine */

#include "g2basic.h"
#include "g2hd.h"

/* This program requires:
 * char is at least 8 bits (signed or unsigned is OK)
 * int is at least 16 bits
 * long is at least 32 bits
 */


/* Scoring in this program internally is based on chinese style counting
 * with the value 50 equivalent to one point.
 *
 * A move value is the double gote value of this side moving vs the other
 * side moving.  Sente moves are worth double since there is only one move
 * difference between sides.
 *
 * This value corresponds to twice the CGB temperature of the local game.
 *
 * A dame, worth zero in Japanese rules, is worth 2 points in gote in Chinese.
 * A point in gote in japanese is worth 3 points in gote in Chinese
 * In general, a double gote play is worth 2 points more in Chinese than 
 *  Japanese.
 * 
 * An endgame one point ko has temperature 1/3, hence is 2/3 points in gote,
 * hence in chinese rules is 2 2/3 points in gote.
 */ 

int maxmove;		/* move number for computer to stop playing */


/* data structures attatched to each square */

/* constant data */
char edge[NUMSQUARES];	/* distance to closest edge, 0 for corner */
char edge2[NUMSQUARES];	/* dist to 2nd closest edge */
char fdir[NUMSQUARES];   /* first direction index for squares */
			/* first direction is away from edge, second, third along edge, 4th toward edge */
			/* if on diagonal, first two directions are equivalent, along two edges */
char xval[NUMSQUARES];	/* x value for square starting at zero for current boardsize */
char yval[NUMSQUARES];	/* y value for square starting at zero */

group_t board[NUMSQUARES];    /* group number of the piece on that square */
list_t newpatbrd[NUMSQUARES];      /* lists of new pattern records matched at each square */
list_t bestseq[NUMSQUARES];    /* expected best sequence from move at this square, including PASS */
                            /* list of moves.  */
char ld[NUMSQUARES];        /* neighboring pieces info - use getldval() during evaluation - nonincremental */
			   /* NOLD (99) - no stones adjacent */
			   /* NEUTRALLD (1) - two colors of stone adjacent */
			   /* 2 for nbr, 1 for nbr of empty nbr */
group_t lgr[NUMSQUARES];       /* deadest group on or adjacent to this point */
unsigned char lnbf[NUMSQUARES][2];   /* number of full neighboring squares by color */
unsigned char lnbn[NUMSQUARES];      /* number of empty neighboring squares */
list_t nbgrp[NUMSQUARES][2];  /* unsorted list of adjacent groups by color */
									/* nbgrp ONLY VALID AT EMPTY SQUARES */
list_t nblbp[NUMSQUARES];	   /* lists of empty neighboring squares */

char ltr1[NUMSQUARES];      /* amount of territory from this square */
char ltrgd[NUMSQUARES];		/* flag if territory is good 
				 * 0-good territory
				 * 1-off to side of stone
				 * 2-neutral or threatened (next to stones of both colors and neither is dead)
				 * 3-under stone next to enemy stone (enemy can put a live stone here)
				 * 4 under stone and undercut one side
				 * 5 off to side and undercut
				 * 7 under stone next to enemy and undercut
				 * 8 undercut 2 sides 
				 * 9 off to side and undercut 2 sides
				 */
char ltrcolor[NUMSQUARES];	/* color of controlling ltrgd, nocolor if no controller, or if controller is dead stone */
eye_t eyerec[NUMSQUARES];		/* eye record number for this eye 0=none */
				/* corresponds to eyeptr */
				/* a point on board can only be part of one eye*/
                                /* only ld values >= 6 have eyerecs */
list_t eyevitrec[NUMSQUARES];	/* unsorted list of eye records that have this vital point */
								/* or'ed with bits to indicate if only remove or only add */

struct strategy strat[NUMSTRATS];  /* strategy records */
listval_t nextstrat;           /* next strategy record to use */

list_t stratreasons[NUMSQUARES];    /* lists of strategy records */
int stratguess[MAXSEARCHDEPTH][NUMSQUARES];    /* guess for move value of move at this point */
int stratbestpat[NUMSQUARES];	/* best pattern rule here */

short stratvalue[NUMSQUARES];		/* the best strategic value among endpints evaluated for this move, for debug */
short stratpatvalue[NUMSQUARES];		/* the best pattern strategic value among endpints evaluated for this move, for debug */
char stratgoodreasons[NUMSQUARES];  /* TRUE if there is a good reason for move other than a pattern match */

int scoreval[NUMSQUARES];	/* save lookahead score (raw, not relative to pass) */

short runterv[NUMSQUARES][2];	/* radiated influence for seeing if a group is surrounded */
short rterv[NUMSQUARES][2];	/* radiated territory value for square by color */
short terv[NUMSQUARES];		/* territory value for each square, positive for white */
list_t ldrflag[NUMSQUARES];       /* ladder lists for moves */
list_t cnbrd[NUMSQUARES];         /* lists of connection records by point */
list_t lkbrd[NUMSQUARES];		/* lists of conn records for linkages */
                                /*    (2 pt jump and knight move ) */
list_t llbrd[NUMSQUARES];          /* lists of conn records for long linkages */
                                /* (3 pt jump and large knight move) */
list_t ollbrd[NUMSQUARES];		/* conn records fore xtra long linkages (4 pt jump, extra long knight move) */
list_t ddbrd[NUMSQUARES];     /* double diagonal connections (center and edge ) */                                
sqr_t sqrbrd[NUMSQUARES][4];	/* nearest occ sqr in each direction */
				/* (or NOSQUARE). */
char dstbrd[NUMSQUARES][4];	/* dist to nearest occ sqr in each dir */
				/* number of empty points between here and there (0-3). */

/* new pattern data structures attached to board
 * one for each matching pattern
 */

list_t newpatfreelist;

/* eye record data structures */

list_t eyefreelist;	/* unsorted list of free eye records */
char eyetype[NUMEYERECS];	/* type of eye */
				/* NOEYE, ONEPOINTEYE, TWOPOINTEYE, DEADEYE, 
				   THRTEYE, BIGEYE */
list_t eyeptr[NUMEYERECS];		/* list of points in this eye */
list_t eyevital[NUMEYERECS];	/* list of vital points for this eye */
				/* a vital point is where you can 
				 * play to increase the potential.
				 * To increase the value of an eye from eyeval
				 * to eyepot you must control the vital
				 * point.  If the vital point is on a group
				 * then this group is threatened and must
				 * be settled to settle the eye.  An eye can
				 * have more than one vital point.
				 * A vital point can also be a place where
				 * the opponent can play to force a response (if eyeval==eyepot)
				 * EYEADDONLY and EYERMONLY are or'ed in indicate that
				 * this vital point only adds to or removes the eye potential
				 * to get the vital point, must mask with EYEPOINTMASK
				 * NOTE: a point can be in list multiple times with different flag values
				 */
/* int eyethreat[NUMEYERECS];    list of points that threaten this eye */
                                /* This eye has eyeval of 8 or more and eyemin 
                                 * less than eyeval.  These are points where
                                 * opponent can play to reduce eyeval to eyemin
                                 * requiring a response.
                                 */

char eyeval[NUMEYERECS];	/* number of eyes if opponent moves first (min estimate if uncertain) */
char eyevalmax[NUMEYERECS]; /* max estimate of number of eyes if opponent moves first */
char eyepot[NUMEYERECS];	/* number of eyes if I move first (max estimate if uncertain) */
char eyemin[NUMEYERECS];	/* min number of eyes if don't respond to
					opponents move (min estimate if uncertain) */ 
char eyeko[NUMEYERECS];  /* must win ko for eye - partially implemented - type of ko 1-4, or 0 for no ko */					
						/* he needs threat	(kotype 4) eyeval is 5 */
						/* he can capture (kotype 3) eyeval is 5 */
						/* I can capture (kotype 2) eyeval is zero */
						/* I need threat (kotype 1) eyeval is zero */
char eyecolor[NUMEYERECS];  /* whose side is this an eye for */
			/* char eyestatus[NUMEYERECS];     status of eye:
				 * ACC - value in eyeval is accurate
				 * QUICK - value in eyeval is quick estimate
				 * SLOW - value in eyeval is slow estimate
				 */
list_t armyfreelist;          /* list of free army records */

list_t charmy;                /* list of armies which changed groups */
 			/* actually squares of stone in army */
list_t chalive;               /* list of armies whose aliveness changed */

struct potential pots[NUMPOTENTIAL];  /* potentials for armies */

/* information kept per army */

list_t armypot[NUMARMIES];      /* list of potential records per army */
list_t armygroups[NUMARMIES];   /* sorted list of groups in army */
list_t armydeadgroups[NUMARMIES]; /* list of dead groups in army */
short armysize[NUMARMIES];   /* number of stones in army */
short armyeyes[NUMARMIES];   /* number of eyes of army 8 = 1 eye */
short armysafeeyes[NUMARMIES];  /* number of eyes of army if opponent gets two moves in a row 8 = 1 eye */
			   /* counts point eyes and dead groups */
list_t armyrun[NUMARMIES][NUMRUN];  /* lists of liberties where can run */
                        /* 0-5 no enemy stone nearby, wide open */
			/* 0 - friendly influence only */
			/* 1 - Wide open - no influence, no enemy stones */
			/* 2 - friendly and unfriendly influence  more friendly*/
			/* 3 - friendly and unfriendly influence, more unfr.*/
                        /* 4 - unfriendly only, but running away from */
			/* 5 - Unfriendly influence only, running toward enemy */
                        /* 6 - hole can push thru for more liberties */
                        /* 7 - short of liberties - can't jump here */
						/* 8 ? */
short armyrn_pot[NUMARMIES];	/* potential eyes from running */
							/* 0 - can't run */
							/* 1-2 - surrounded but some run potential */
							/* 3 can connect to run away */
							/* 8 or greater, should be able to run away, but is not yet stable */
							/* 16 or greater, stable group */
short armycnrn_pot[NUMARMIES];	/* running potential can connect to */
short armywk_pot[NUMARMIES];    /* potential eyes from neighboring weak groups */
                               /* don't count thratened groups */
short armybestpot[NUMARMIES];  /* conservative best potential eye value for army for move at bestmove.  Counts all potentials through
								bestmove point, except POTTHREAT, and only counts one of multiple connections to same army */
short armybestmax[NUMARMIES];	/* optimistic best potential value for army */
sqr_t armybestmove[NUMARMIES];  /* best move for eyes, counting all potentials */
short armybestpot2[NUMARMIES];  /* second best potential eye value */
sqr_t armybestmove2[NUMARMIES];  /* second best move for making eyes */
                
short armybestshared[NUMARMIES];  /* best eye potential thru shared connection */
                
sqr_t armykillmove[NUMARMIES];  /* best potential reducing move */
sqr_t armykillmove2[NUMARMIES];  /* second best potential reducing move */

short armysecond[NUMARMIES];   /* best val after opponent moves at armykillmove */
sqr_t armysecondmove[NUMARMIES]; /* best move after opponent moves */

short armyrest[NUMARMIES];  /* rest of eye potential */
short armypthreat[NUMARMIES];  /* pthreat potential for army, not counted in rest */
short armynumrest[NUMARMIES];  /* number of eye moves in rest */
short armykopot[NUMARMIES]; /* ko potential eyes */
short armyweakpot[NUMARMIES]; /* connection wo weak group potential */
short armyeyepotential[NUMARMIES]; /* total extra eye potential 8 = 1 eye eyes plus dead groups plus territory 
								don't use this for life, just unsettled/potential/etc. doesn't include pthreat */
short armylibs[NUMARMIES];   /* number of liberties for army */
list_t armylbp[NUMARMIES];		/* army liberty list */
list_t armynbp[NUMARMIES];    /* list of neighboring armies for army (not always valid) */
                           /* includes touching armies and shared liberties */
list_t armyeyerecs[NUMARMIES]; 	/* list of eye records for this army */
list_t armyvitalpoints[NUMARMIES]; /* list of vital points for all eyes in army */
	/* these are places where enemy can play to eliminate potential */
	/* for more eyes. can have constants or'ed in */
	 /* EYEADDONLY and EYERMONLY are or'ed in indicate that */
	/* this move only adds or only removes the eye */
    /* NOTE: MUST AND WITH EYEPOINTMASK TO GET THE POINT! */
short armyeyespace[NUMARMIES];    /* total eyespace (conservative) for army */
short armyeyespacemax[NUMARMIES];  /* total eyespace (optimistic) for army */
int armyaji[NUMARMIES];			/* aji for this army - points to be taken later in attack or deense */
int armyminsemeyes[NUMARMIES];	/* min eyes from certain semeai capture (if enemy goes first) */
int armymaxsemeyes[NUMARMIES];	/* max eyes from certain semeai capture (if enemy goes first) */

sqr_t xmax,xmin,ymax,ymin;  /* area to reevaluate edge territory */
char lookaheadflag;
list_t lookldr = EOL;	  /* points where ladders happened due to stone
			     being put down last eval */
sqr_t kosquare;             /* illegal move square for ko */
sqr_t boardsize;		/* size of edge of board */
sqr_t boardsquare;	/* boardsize * boardsize */
sqr_t firstsquare;	/* index of first square on board */
sqr_t lastsquare;		/* firstsquare + boardsquare */
sqr_t pcls[NUMPCLS];   /* pieces changed since last eval */
int pclsnext;


/* int scr;	 total score for current position */
int tscr;	/* total territory score for liberties and open territory */
int pscr;	/* total score for pieces on board */
int ajiscr;	/* aji score - penalties for weak groups */
int dscr;	/* score ofr dead groups inside dead groups */
int bigscr;	/* biggest group to attack or catpure */
int ctscr;	/* total center territory based on orthogonals */
int thrscr;     /* total score for weak and unsettled groups on board */
int passscr;	/* score for pass moves - Japanese rules for handicap and extra end of game passes */
int obaval = 350;  /* 1/2 value of taking a big gote point.  set once at the start of a search by calling getobaval */
				/* side to move gets oba added to bring scores back to normal. 7 points at start of game */

list_t eyelist;	/* unsorted list of points where eyes need to be reevaluated */
				/* points which are currently part of an eye, or could become */
				/* part of an eye */

list_t grldr[NUMGROUPS+NUMCONNS+NUMEYERECS];  /* ladder lists for gr,cn,li */
                        /* has list of points that impact a tactical search */
list_t cnchgd;             /* list of connections needing eval */
list_t nbply[NUMMOVES],lbply[NUMMOVES];  /* more tactical  (lists)*/

			/* Josekis */
int jflag[4];		
                        /* 1 if still in joseki tree */
                        /* 2 if one move past joseki */
                        /* 3 or more if additional moves played past joseki */
int jreflect[4];	/* 0 - symetric corner, 1 - reflected, 2 - as shown */
unsigned int jptr2[4];           /* points in jlib2 to move just played by corner */
int jcolor[4];		/* color that moved first in corner */

/* move descriptions */  
unsigned int msptr;			/* next move number.  move stack pointer */
sqr_t mvs[NUMMOVES];		/* move square for each move */
list_t mvcapt[NUMMOVES];		/* list of groups captured by move */
list_t mvconn[NUMMOVES];		/* list of group connected by move */
char mvcolor[NUMMOVES];		/* color of move, 0:black, 1:white */
short mvnext[NUMMOVES];		/* mvs index for next piece in group or -1 */
sqr_t kosave[NUMMOVES];		/* saved ko sqaure */
/* group data structure */
char grcolor[NUMGROUPS];	/* color of group (grcolor[NOGROUP] is NOCOLOR)*/
short grlibs[NUMGROUPS];	/* number of liberties */
short grpieces[NUMGROUPS];	/* move stack index of first piece in group */

signed char gralprob[NUMGROUPS];  /* probability of living (-50 to 50) per group, as function of color to move. get in getalprob() */
int gralval[NUMGROUPS][2];		/* probability of living if I move first[0], enemy moves first[1] */
int groldalprob[NUMGROUPS];
int grsemval[NUMGROUPS];	/* win semeai prob for group (-50 to 50) - prob of winning a semeai that give two eyes */

char savegral[2][NUMGROUPS];  /* saved alive values */
char savevalid[2];				/* is the saved life data valid */
unsigned int savemove[2];				/* msptr value associated with saved life */
int gralvalid;					/* are the gralive values valid */
unsigned char gralive[NUMGROUPS];	/* aliveness of group (0-25) 
	HERE DOWN ARE DEAD
	25 - tactically captured unconditionally
	24 - unused.
	23 - Temp used for weak groups undecided yet
	22 - No eyespace or potential and nbrs all alive
	21 - probably dead some eye space or potential, nbrs alive
	20 - in semeai loses 
	HERE DOWN ARE WEAK - PROBABLY WILL DIE
	19 - no running ability, weak nbrs and some eye potential)
	18 - can't run, lots of eye potential, only one eye
		has aji to live, or can be used as ko threats     
	17 - in a semeai. behind - aji for later
	16 - poor running ability - can't live in one move
	HERE DOWN ARE UNSETTLED
	15 - ko for life, must capture ko, then fill to live
	14 - must run away to live - adjacent enemy groups settled
	13 - in a semeai. unsettled
	12 - surrounded, can live or die in one move
	12 - would be alive, but tactically threatened
	11 - running fight - have to run to live, and adjacent group also has to run
	10 - ko for life, fill ko to live
	9 - in a semeai. Ahead or temporary seki
	8 - unsettled - can live in one move or limp away
	HERE DOWN ARE ALIVE (or settled for now)
	7 - can run away easily, no eyes
	6 - can run away easily, one eye
	    needs two moves to make second eye 
	6 - can live in one move or run easily
	5 - Alive because wins semeai 
	4 - at least a seki - may be actual seki, or temporary seki
	3 - miai for barely space for two eyes (dangerous)
	2 - barely territory for two eyes (dangerous)
	HERE DOWN ARE VERY ALIVE
	1 - miai for lots of eye space - 3 or more ways to make
		second eye
	1 - absolutely unconditionally alive - two small eyes
	    or lots of territory
	0 - temporary value for newly created group
	*/

unsigned char grthreatened[NUMGROUPS]; /* TRUE if group is tactically unsettled - */
		 	/* 1 if captured if attacker wins all kos, and defender answers all atari threats */
			/* 2 if unconditionally captured when attacker moves first, defender wins kos */
sqr_t grcapmove[NUMGROUPS];  /* move that captures threatened group */
sqr_t grsavemove[NUMGROUPS];  /* move that prevents group from being captured */
army_t groldarmy[NUMSQUARES];  /* old army value */
int grolddefv[NUMSQUARES];	/* value for defending old army here */
char groldalive[NUMSQUARES]; /* aliveness of group after opponent's last move and 
								before lookahead by square - 0 if there was no stone on that point. used to check 1 ply rules */
signed char sqoldalprob[NUMSQUARES];	/* gralprob before lookahead */
short groldrun[NUMSQUARES];  /* old running ability */
char groldthreatened[NUMSQUARES]; /* threatened before lookahead by square */
char grlv[NUMGROUPS];		/* does group exist */
unsigned short grsize[NUMGROUPS];	/* number of stones in group */
list_t grlbp[NUMGROUPS];   /* liberty lists */
list_t grcnp[NUMGROUPS];   /* connection record lists */
list_t grnbp[NUMGROUPS];   /* enemy neighbor lists */
army_t grarmy[NUMGROUPS];     /* army number for group */
army_t grdeadarmy[NUMGROUPS]; /* army number in which this is dead group */
int grdefval[NUMGROUPS];	/* value of defending the army of which this is a member */
							/* set in setdefvals and urgdefenough */
int gratkval[NUMGROUPS];	/* value of attacking the army of which this is a member */
							/* set in setatkvals() and urgatkenough */
int numpris[2];		/* number of prisoners for each color */

/* connection descripers */
list_t cnfreelist;            /* free connection records */
group_t cngr1[NUMCONNS],cngr2[NUMCONNS];	/* group numbers for connection */
char cncnum[NUMCONNS],cnlknum[NUMCONNS],cnllnum[NUMCONNS],cnollnum[NUMCONNS];	/* number of conns and links*/
                           /* 1, 2, 3, 4 space linkages */
char cnddnum[NUMCONNS];  /* number of double diagonal connections - center point of connection must have >= 2 adjacent empty points */
list_t cnptr[NUMCONNS];  /* list of connection points (solid)*/
list_t cnlkptr[NUMCONNS]; /* list of connection points (linkages) */
list_t cnllptr[NUMCONNS];	/* list of connection points (long linkages) */
list_t cnollptr[NUMCONNS];	/* list of connection points (extra long linkages) */
list_t cnddptr[NUMCONNS];   /* list of double diagonal conn points (center points) */
                          /* conn and link points */
list_t cnpathptr[NUMCONNS];	/* list of all points in potential connection path */
char cnprot[NUMCONNS];		/* is connection unbreakable AJI_CONNECT, etc */
char cntype[NUMCONNS];		/* type of connection */
group_t cnshcent[NUMCONNS];	/* only vaid if type is SHARED_CONNECT.  The group that is the center of the 3 involved in sharing */
							/* this group has the option of connecting to either of the other two (evaled with bestshared) */
short cnpat[NUMCONNS];		/* pattern that determined the value of this connection */

char problemflag = 0;	/* solve tesuji and life/death probs (2 for full board problems, 3 for life and death problems) - set in newgame() */
group_t maxgr;
int maxpc;
int playlevel;   /* playing level 0-5 (18k, 15k, 12k, 9k, 6k, 4k)
		  */

int ccl[3] = {-1, 1, 0};  /* values for colors */


/* LEVELS: 0  1  2   3   4   5  (19, 16, 13, 10, 7, 5 kyu) 6 is for monte carlo*/
int maxmoves[NUMLEVELS] =   /* maximum number of moves to try on full board */
        {  1, 2, 4, 10, 15, 20, 1 };  /* can look at more if have extra time, up to twice */
int maxvariations[NUMLEVELS] =  /* max number of children per move tried in main search */
        {  1, 1, 1,  1,  5, 10, 1 };
char maxscorebrdepth[NUMLEVELS] =	/* max depth for any branches in Q search */
		{  0, 0, 1,  1,  2,  2, 1 }; 
char maxscoredepth[NUMLEVELS] = /* max depth for local q search, other than tac captures */
        {  0, 0, 1,  2,  3,  3, 1 };
int maxiterations[NUMLEVELS] = /* max iteration count (depth before Q search */
        {  1, 1, 1,  1,  3,  8, 1 };
int maxlifecalls[NUMLEVELS] =  /* 1.5-2xx each level, unlimited makes little difference at ply 3 */
        {  1, 5,10,100,1000, 10000, 10000 };
double randomness[NUMLEVELS] =   /* amount of random variation per level as a function of obaval */
		{  6, 1, 1, 1, 0.25, 0.25, 0.25, 
};

/* LEVELS: 0  1  2   3   4   5 */
unsigned char taclibs[NUMLEVELS] =     /* max liberties in a tactical fight */
        {  1, 1, 2,  3,  3,  3, 3 };
unsigned char eyetaclibs[NUMLEVELS] =     /* max liberties for eye diagonal */
        {  1, 1, 2,  2,  3,  3, 2 }; 
int cancapsize[NUMLEVELS] = /* size of search in canbecaptured */
        {  3, 5,20, 70,100,150,100 };
unsigned char eyecapsize[NUMLEVELS] = /* size of search for eyes diags */
        {  3, 5,10, 15, 25, 30, 20 }; 
unsigned char eyecapdepth[NUMLEVELS] = /* depth of search for eyes diags */
        {  1, 1, 3,  3,  4,  5, 4 };
unsigned char vitalcapsize[NUMLEVELS] = /* size of search for eyes diags in tv pot */
        {  2, 2, 3,  3,  5, 10, 5 }; 
unsigned char vitalcapdepth[NUMLEVELS] = /* depth of search for eyes diags in tv pot*/
        {  1, 1, 2,  3,  2,  3, 2};
unsigned char conntaclibs[NUMLEVELS] =     /* max liberties for connection search */
        {  1, 1, 2,  2,  3,  3, 3 }; 
unsigned char conncapsize[NUMLEVELS] = /* size of search for connections */
        {  2, 5,10, 20, 30, 50, 50 };
unsigned char conncapdepth[NUMLEVELS] = /* depth of search for connections */
        {  1, 2, 5,  6,  8, 11, 11 };
char mvmost[NUMLEVELS] =    /* number of moves considered for ladder at each ply */
        {  1, 1, 1,  2,  2,  3, 3 };
char eyemost[NUMLEVELS] =    /* number of moves considered for ladder at each ply */
        {  1, 1, 1,  2,  2,  2, 2 };
char connmost[NUMLEVELS] =    /* number of moves considered for ladder at each ply */
        {  1, 1, 1,  2,  2,  2, 2 };
int maxbranchdepth[NUMLEVELS] = /* maximum depth for branches in tactical move tree (unless move values are close) */
        {  1, 1, 1,  2,  2,  3, 3 };
int maxtacdiff[NUMLEVELS] =  /* maximum difference between best tac move and this move*/
        {  8,16,16, 64, 90,120,120 };
int mintacval[NUMLEVELS] =   /* minimum value move has to be considered tacticaly */
		{  10, 0, 0,  0,-10,-16,-16 };
int numpotmoves[NUMLEVELS] =   /* Number of moves to read for adpot() to capture group */
		{  0, 0, 0,  0,  1,  1, 1 };

        
/* LEVELS: 0  1  2   3   4   5 */
unsigned char mdist[NUMLEVELS] =      /* distance to radiate influence from live groups */
        {  1, 1, 2,  3,  9, 10, 6 };

/* Fights: no fight reading below level 4 */
/* LEVELS: 0  1  2   3   4   5 */
char maxfightdepth[NUMLEVELS] = /* max depth for reading life and death fight, not counting forced moves */
        {  0, 0, 0,  1,  3, 15, 15 };
int maxfightsize[NUMLEVELS] = /* max number of nodes in a life and death tree */
		{  0, 0, 0, 10, 50,500,500 };
int maxsemdiff[NUMLEVELS] =  /* maximum difference between best semeai move and this move*/
		{  8, 8,16, 32, 60,100,100 };

int numnodes;	/* number of tactical nodes looked at total (calls to lupdate) */
int nummoves;	/* update move count statistics for debug */

sqr_t brddir[4] = { -19,-1,1,19 };  
  
char ldir[52] = {
   2,2,
   4,4,
   6,6,
   8,8,
   11,11,11,
   14,14,14,
   17,17,17,
   20,20,20,
   24,24,24,24, 
   28,28,28,28, 
   32,32,32,32, 
   36,36,36,36, 
   40,40,40,40, 
   44,44,44,44, 
   48,48,48,48, 
   52,52,52,52 }; 
  
int nbr[52]; /* offsets for each direction.  index based on point is */
					/* from fdir.  first two indexes are along edge. */
					/* third (if not in corner) is toward center */
					/* (third is toward edge if along diagonal) */
					/* fourth is toward edge. */

char ldiag[52] = {
   1,1,
   3,3,
   5,5,
   7,7,
   10,10,10,
   13,13,13,
   16,16,16,
   19,19,19,
   24,24,24,24, 
   28,28,28,28, 
   32,32,32,32, 
   36,36,36,36, 
   40,40,40,40, 
   44,44,44,44, 
   48,48,48,48, 
   52,52,52,52,
};

int diags[52];	/* offsets for each diagonal direction */
  
char *clr[] = {
"Black",
"White" };
  

int showtactics;
/* level to show tactics
 * 0 - none
 * 1 - full board reading
 * 2 - full board and tactics
 */

unsigned int debug;
/* debug value for program:
 * if nonzero, require return after each tactical move, and at full board
 *  endpoints
 * 2GGG show tactics for group GGG
 * 200 detailed tactics output
 */


int phase;      /* phase of game, type of game.  */
int ahead;      /* how are we doing?  0 way behind, 1 behind, 2 about even,
		                      3 ahead, 4 way ahead */
                /* used to adjust strategic values */
int agressiveness;  /* how agressive to play 0-timid, 10-all out, 5 normal */
int easyrun[3] = { EASY_RUN,EASY_RUN,EASY_RUN };  /* how much running potential to run easily, even if opponent moves first */
int komi;   /* how much komi (in 50ths of points ) */

  
unsigned char bdw[4][8][4][32];  /* bit vectors for the board 19 + 7 + 6 (for overlap)  (white, empty, black bits) */
unsigned char bde[4][8][4][32];  /* 4 orientations.  corner matches left */
unsigned char bdb[4][8][4][32];  /* edge rows 7-25 across top filled with off board */
								 /* left edge of board is one byte in from left */
								 /* right edge has only 5 points off board, so don't try to match too close */
/* 0 - bits left to right (bytes top to bottom) */
/* 1 - bits right to left (bytes top to bottom) */
/* 2 - bits top to bottom (bytes left to right) */
/* 3 - bits bottom to top (bytes left to right) */
/* 4 orient, 8 shifts, 4 wide, 32 tall */

/* HERE ARE ALL PARAMETERS THAT VARY WITH ALIVENESS! */


/* about 350 parameters here */

/* these must all have the same value!  since aliveness can change between
 * first pass and second pass.  This is for running potential only.  All stones the same
 * excpet obviously alive and obviously dead.
 */

double rtalive1[NUMALIVE] = {	/* maximum value for rt for each aliveness for runterv */
0,
1,   /* alive */  
1,.7,               /* alive */
.7,.7,.7,.7,  /* alive */
.7,.7,.7,.7,
.7,.7,.7,.7, /* unsettled */
.7,.7,.7,
.7,.7,.7,.7,
.7,0,0 };    /* weak stones radiate some 1st pass (.7) */

/* radiated terr is always positive */
/* value here is maximum.  actual value is between this and 0, depending */
/* on the gralprob for the stones. 50 for this value, -50 for 0. */
double rtalive2[NUMALIVE] = {	/* maximum value for rt for each aliveness for rterv*/
0,            
1,1,
.95,	/*  alive */
.95,.95,.9,.9,	/* alive */
.8,.5,.5,.5,.5,.5,.5,.5,  /* unsettled, leave positive */
.3,.2,.1,  /* up to 18 not zero, since stones still have aji */
0,  
0,0,0,    
0,0,0 };

/* this value is added to enemy's color */
double rtalive3[NUMALIVE] = {	/* maximum value for rt for each aliveness */
0,            /* second pass (note - no credit for opposite color!) */
0,
0,0,	/*  alive */
0,0,0,0,	/* alive */
0,0,0,.4,.5,.5,.5,.5,  /* unsettled - give value to enemy to cancel territory */
.7,.7,.7,.7,  /* weak */
.8,.8,.8,	/* don't get too much since enemy can threaten to pull group ou t to play nearby */
.8,.8,.8 };


/* is this group worth trying to save (leave out semeai) */

int savealive[NUMALIVE] = {
0,
0,
0,0,0,0,0,0,  /* alive */
1,0,1,1,1,0,1,1,   /* unsettled */
1,1,1,   /* 16,17,18 (weak) */
0,0,0,0,
0,0,0 };
  
/* can this group live in one move */

int livealive[NUMALIVE] = {
0,
0,
0,0,0,0,0,0,  /* alive */
1,0,1,1,1,0,1,1,   /* unsettled */
1,0,0,   /* 16,17,18 */
0,0,0,0,
0,0,0 };
  
/* is group with this aliveness threatened with capture in one move.  Used
   for sente and for adjusting score */

int thalive[NUMALIVE] = {
0,
0,
0,0,0,0,0,0,  /* alive */
0,0,0,0,1,0,0,1,   /* unsettled */
1,1,1,   /* 16,17,18 */
0,0,0,0,
0,0,0 };

/* is group with this aliveness threatened with capture in one move.  Used
   for in getscore for groups to try to kill */

int thalive2[NUMALIVE] = {
0,
0,
0,0,0,0,0,0,  /* alive */
1,0,0,0,1,0,0,1,   /* unsettled */
1,0,0,   /* 16,17,18 */
0,0,0,0,
0,0,0 };

/* should this group be considered for urgent save ? */

int urgalive[NUMALIVE] = {
0,
0,
0,0,0,0,0,0,  /* alive */
1,1,1,1,1,1,1,1,   /* unsettled 8-15 */
1,0,0,   /* 16,17,18 */
0,0,0,0,
0,0,0 };

/* is this group in a semeai? */

int semalive[NUMALIVE] = {
0,
0,
0,0,0,0,0,0,
0,1,0,0,0,1,0,0,   /* unsettled */
0,1,0,   /* 16,17,18 */
0,0,0,0,
0,0,0 };

/* is this group attackable by a group in a semeai? */

int semnbralive[NUMALIVE] = {
0,
0,
0,0,0,0,1,0,
1,1,1,1,1,1,0,1,   /* unsettled */
1,1,1,   /* 16,17,18 */
0,0,0,0,
0,0,0 };

/* is group with this aliveness attackable */
int attackable[NUMALIVE] = {
0,
0,
0,1,0,  /* miai */
0,0,1,  /* alive */
1,1,1,1,0,1,1,1,  /* unsettled */
1,1,0,
0,0,0,0,
0,0,0, };

/* value of making sente attack on group */
int senteattack[NUMALIVE] = {
0,
0,
0,50,0,
0,0,100,  /* alive */
50,100,100,100,0,200,100,100,  /* unsettled */
200,200,0,
0,0,0,0,
0,0,0, };

/* pfac values (gralprob) as a function of running ability for
 * alive 16 groups 
 */
 
int pfac16[16] = {
-15,-15,-15,-10, 0, 0, 0, 5, 5,10,20,30,35,40,42,45,
};

/* pfac values (gralprob) as a function of running ability for
 * alive 11 groups 
 */
 
int pfac11[16] = {
-10,-10, -5,  0, 5,10,15,25,30,35,40,45,45,45,45,45,
};

/* pfac values (gralprob) as a function of running ability for
 * alive 8 groups 
 */
 
int pfac8[16] = {
-10,-10,0,10,15,20,25,30,35,37,39,41,42,43,44,45,
};


/* values for pieces on board by aliveness 
 * values for open points by aliveness 
 * note that liberties in a semeai don't count since will have to
 * be filled when group captured (alive 20, 17, 13)
 * alive 11 and 16 use pfac 11 and 16 values above based on
 * running ability
 * gralprob is used instead where the group is known.
 */
  
int pfac[NUMALIVE] = {
0,
50,	        /* has two eyes or space for two eyes */
/* old values 50,47,47,45,45,40,  /* alive */
50,50,50,50,50,50,  /* alive, so gets full value */
20,50,18,15,0,0,0,-15,   /* unsettled */
0,-40,-40,-48,       /* has some eye potential */
					/* value for 17 is adjusted for semeai win prob, from pfac to 0 */
-50,-50,-50, /* probably dead */
0,		/* placeholder during evaluation */
-50,		/* unsued */
-50 };		/* captured (DEAD) */
 

/* values for defending a group in order to attack this neighboring group. */

int defatkv[NUMALIVE] = {
0,
0,
0,0,0,0,5,15,  /* alive */
20,20,30,40,40,30,30,20,  /* unsettled */
25,30,20,10,
5,5,5,
0,0,0
	};

/* values for attacking group for each stone or liberty */

int atkv[NUMALIVE] = {
0,
0,	/* live groups */
5,5,0,5,10,30,	/* groups which can get 2 eyes */
30,20,30,40,40,40,40,40,	/* unsettled groups */
40,30,20,10,	/* groups with some eyespace */
5,5,5,	/* dead groups */
0,2,0 };

/* value of cutting off a group in addition to the attacking value */

int ctval[NUMALIVE] = {
0,
0, /* living groups */
5,10,5,10,30,30,  /* living groups */
40,40,50,50,75,75,75,75,  /* unsettled groups */
85,85,75,75,  /* has some eye potential */
100,100,100,  /* dead groups */
50,100,25 };

/* value of connecting a group */

int connval[NUMALIVE] = {
0,
0,  /* living groups */
4,15,4,10,15,25,  /* living groups */
30,30,20,20,20,20,20,20,  /* unsettled groups */
50,50,75,50,  /* has some eye potential */
100,100,100,  /* dead groups */
100,50,0 };

/* bonus for changing group from alive or unsettled to weak or dead */

int kval[NUMALIVE] = {
0,
0,  /* alive */
0,0,0,0,0,0,  /* alive */
0,0,0,0,0,0,0,0,  /* unsettled */
0,30,40,50,
50,50,50,
0,
90,
95 };


/* sumeyes is numer of eyes for territory ( 8 is one eye ) */

/* 3 points of territory must be an eye or corner invasion under 3-3
 * point ends up dead.  5 points of terr must be potential for two eyes
 * or 3-3 invasion isn't unsettled after opponent surrounds and connects
 */

int sumeyesmax[41] = { /* 1/03 - gradual change to kepp order optimal */
0,2,4,8,10,12,16,17,18,19,20,20,20,20,20,20,24,24,24, /* up to MAXTERR */
26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26 };

int sumeyes[41] = { 
0,2,2,4,8, 8,12,16,16,16,20,20,20,20,20,20,24,24,24, /* up to MAXTERR */
26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26 };


int sumpots[41] = {
0,0,0,0,0, 4, 4, 0, 0, 0, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };


/* territory for extend down open side from 1 stone wall
 * by edge value
 */

int terr1[5] =  /* should match right column of terr5 */
{ 0,0,3,7,7 };

/* territory for crawling along edge under enemy stone
 * by edge value.  small since block will also count same place.
 * only give territory beyond this point.
 */

int terr2[5] = 
{ 0,0,1,2,3 };

/* territory for extending down edge from a two stone wall
 * by edge value of stone nearest edge
 */

int terr3[5] =
{ 0,1,6,7,8 };

/* territory for extending down edge when have to go down
 * a line to get under an enemy stone
 */

int terr4[5] = 
{ 0,0,1,3,5 };

/* terr5 is the territory for extending down side on a line toward a
 * enemy stone at a distance. Enemy stone is on same line as stone
 * extending from
 */

int terr5[5][6] =
{
	{ 0,0,0,0,0,0 },	/* edge = 0 */
	{ 0,0,0,0,0,0 },	/* edge = 1 */
	{ 0,1,2,2,2,3 },	/* edge = 2 */
	{ 0,1,2,3,4,7 },	/* edge = 3 */
	{ 0,2,5,6,7,7 },	/* edge = 4 */
};

int rtthreshold;  /* if one side is over rtthreshold, other side gets no
                     credit for territory. Set at distance 3 since group
		     can do a one point jump. */



