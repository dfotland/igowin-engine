/* Copyright 1984-2001 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#ifndef G2DEF_INCLUDED
#define G2DEF_INCLUDED


# include "g2types.h"
#include "assert.h"

/* assertion from Writing Solid Code */

#if !defined(ASSERT)
#ifdef CHECK
void _Assert(char *,unsigned);  /* prototype */
# define ASSERT(f) \
	assert(f) 
//  if (!(f))  \
//      _Assert(__FILE__,__LINE__)
# else 
# define ASSERT(f)
#endif
#endif

/* MSVC2005 needs this to prevent warnings on stdio functions */
#define _CRT_SECURE_NO_DEPRECATE 1

#define g2abs(x) (((x) > 0)?(x):(-(x)))

struct strategy {
	short value;       /* extra strategic value for this move */
	listval_t param;       /* square in group to check aliveness after lookahead */
	                 /* if square is enemy stone, it should be deader */
	                 /* NOSQUARE means use the group on the square moved to */
	                 /* index into newpat[] if reason is pattern matched */
	short pattern;	 /* pattern number for pattern matching reason */
	sqr_t patsqr;	/* where does the pattern match? */
	short pato;		/* pattern orientation */
	short patcolor;	/* color of pattern match */
	short patmove;	/* index of next move */
	short guess;		/* extra guess value for rule */
	unsigned char reason;      /* reason to try this move (rule #) */
	char goodrule;    /* TRUE if this rule is satisfied after lookahead */
	};

struct rulestruct {
	int value;		/* value of rule (added after lookahead if rule applies) */
	int guess;      /* guessed value of rule (used to select moves to try) */
	unsigned short weakest;		/* weakest group this rule applies to move made or point specified in rule */
	unsigned short attack;		/* modifier describing how to interpret the rule */
	};



# define PATYSIZE 6
# define PATXSIZE 6
# define MAXSIZE 5


struct potential {
	unsigned short pot_where; /* square for undercut */
	               /* connection number for connect */
	               /* square for vital */
                   /* army number of threatened group for threat */
	               /* liberty for extend */
	unsigned char pot_val;  /* conservative 8 per eye, for undercut, extend is amount of territory */
							/* value for threat does not include eye value */
							/* for threatened type, only value for connection, since vital points to capture group have rest */
	unsigned char pot_max;  /* maximum value for this eye potential */
	unsigned char pot_type;  /* extend, vital, connect, threat, undercut */
	};

/* the following group of defines is safe to change since no harm comes
 * except stupidness if they are two small
 */


/* above this playlevel, read life and death before anything else */
#define READLIFELEVEL 4
/* full joseki above this level, simple joseki at this level */
#define JOSEKILEVEL 4
#define FUSEKILEVEL 4
#define KOLEVEL 2
#define SQUIRMLEVEL 4
#define SAFELEVEL 3
#define PATMOVELEVEL 2
#define PATVALLEVEL 2
#define PATSURRLEVEL 3
#define ATKDEFLEVEL 3
#define SENTELEVEL 3
#define READCONNLEVEL 3
#define SHIMARILEVEL 2

/* release values - not demo */

/* won't crash if nummoves is too small */
/* MAXMOVE is maximum number of moves allowed in a game */
/* typical game has about 250 moves.  about 400 moves is highest you usually see */
# define MAXMOVE 2000
# define NUMMOVES (MAXMOVE + 100)
/* MAXLADDER must be greater than MAXMOVE! */
/* must have room for maximum number of generated moves in iscaptured */
# define MAXLADDER (NUMMOVES-200)
/* 
 * Each move makes at most one new group.
 * group numbers are not recycled 
 * leave a few for TRYTOKILL and NOGROUP, etc 
 */
# define NUMGROUPS ((group_t)MAXMOVE)  
/* won't crash if numstrats too small, just gets a little stupid */
# define NUMSTRATS 20000
/* won't crash if NUMPLY too small - just sets maximum depth for ladder searches */
# define NUMPLY 70
/* won't crash if NUMPOTENTIAL too small, just gets a little stupid */
/* 500 is enough for ordinary positions */
# define NUMPOTENTIAL 2000
/* usually no more than one new army per move, but could be several.
 * army numbers are recycled, so this is the maximum number of armies on the
 * board at one time. */
/* shouldn't crash if NUMARMIES too small, just will stop accepting moves */
/* it's possible to make positions with over 255 separate groups :-( */
# define NUMARMIES ((army_t)500) 
/* 
 * eyes are reused and each point can only be in at most one eye
 */
# define NUMEYERECS ((eye_t)500) 
/* won't crash if NUMNEWPATS is too small, just gets stupid, since can't remember all matches */
/* 2470 is most I have seen used */
# define NUMNEWPATS 3000
/* shouldn't crash if NUMCONNS is too small, just stops accepting moves */
/* 250 is plenty for ordinary games, but positions can be constructed with up to 1000 */
# define NUMCONNS 2000


# define MAXGROUP (NUMGROUPS-20)  /* max group when making a move - leave a few for iscaptured */
# define NUMPCLS (NUMMOVES)
# define ARMIESTOMOVE 20
# define ARMIESTOUPDATE 8
# define EYESTOMOVE 20
# define EYESTOUPDATE 8
# define CONNSTOMOVE 30
# define CONNSTOUPDATE 15 

#define BIGEYEDIST 6
#define BIGEYEMINDIST 10


/* The worst I have found 
 * need 76,000 list elements at level 9. 
 * typical games need 15000 list elements
 */

# define NUMNBLB 1368

# define NUMLIST (65000)


/* numsquares has two extra for NOSQUARE and PASS */
# define NUMSQUARES (19 * 19 + 2)

# define RUNEXTENDENEMY 8
# define RUNEXTENDFRIEND 7
# define NUMRUN 14
# define MAXOPENRUN 8
# define MAXEASYRUN 5
# define NEUTRALRUN 10
# define JUMPENEMY 13

# define G2ERROR 0xf000
# define EOL (NUMLIST-1)
# define NOCONN -1
# define NOSQUARE ((sqr_t)(NUMSQUARES-2))

# ifndef PASS
# define PASS (NOSQUARE+1)
# endif

# define NOGROUP ((group_t)(NUMGROUPS-1))
# define NOARMY ((army_t)(NUMARMIES-1))
# define MULTIARMY ((army_t)(NUMARMIES))
# define NOCORNER 4
# define NOEDGE 20
# define BLACKCOLOR 0
# define WHITECOLOR 1
# define NOCOLOR 2
# define NOLD 99
# define NEUTRALLD 1
# define BIGNUM 32767
# define SMALLNUM (-BIGNUM)
#define MAXSEARCHDEPTH 30

/* for cntype[] */

# define CN_UNKNOWN 0
# define CN_ONEPOINTJUMP 1
# define CN_KNIGHTSMOVE 2
# define CN_TWOPOINTJUMP 3
# define CN_HANE 4
# define CN_BAMBOOJOINT 5
# define CN_DIAGONAL 6
# define CN_MULTIPLE 7
# define CN_ONESIDEDEAD 8
# define CN_THREAT 9
# define CN_HALFKNIGHT 10
# define CN_THREEPOINTJUMP 11
# define CN_LARGEKNIGHT 12
# define CN_HALFLARGEKNIGHT 13
# define CN_DOUBLEDIAG 14
# define CN_CORNER 15
# define CN_MULTIHALFKNIGHT 16
# define CN_EXTRALARGEKNIGHT 17 
# define CN_HALFEXTRAKNIGHT 18
# define CN_FOURPOINTJUMP 19

#define UNSETTLEDALVAL 40

/* for gralive[] */

# define NUMALIVE 26

# define DEAD 25
# define LIFEUNUSED 24
# define WEAK_GROUP 23
# define VERY_WEAK 23
# define MUST_BE_DEAD 22
# define LOOKS_DEAD 21
# define LOSE_SEMEAI 20
# define WEAK 19
# define WEAK_POTENTIAL 18
# define WEAK_SEMEAI 17
# define WEAK_LIMP 16
# define UNSETTLED 15
# define WEAK_KO 15
# define UNSETTLED_RUN 14
# define SEMEAI 13
# define UNSETTLED_DEAD 12
# define UNSETTLED_THREATENED 12
# define RUNNING_FIGHT 11
# define STRONG_KO 10
# define STRONG_SEMEAI 9
# define UNSETTLED_LIMP 8
# define ALIVE 7
# define RUN_NO_EYES 7
# define RUN_OR_LIVE 6
# define WINS_SEMEAI 5
# define SEKI 4
# define MIAI 3
/* 2 eyes, but in one eyespace, and can't run.  ko threats here and maybe tactical threats  
 * squirm will try to kill it
 * def_val will give it extra value
 */
# define BARELY_ALIVE 2
/* completely alive, no ko threats, no possible tacts */
# define VERY_ALIVE 1
# define STRONG_MIAI 1
# define HAS_TWO_EYES 1
# define NEW_ALIVE 0

# define UNSET_RUN 6
# define SEMEAI_LIMP 6
# define SEMEAI_RUN 8
/* how much run to make it impossible to win semeai */
#define WINSEM_RUN 9
/* how much run to be unsettled, can run away with one move */
# define LIMP_RUN 9
/* how much running to be stable.  adjusted by the handicap */
/* must be 16 or have to readjust all the running values */
# define EASY_RUN 16
/* this much or less runing ability to be attacked in obvious answer. */
/* don't attack groups that can easily run away.  they are already stable enough */
# define OBVIOUS_RUN 6

/* for potential */
/* threat is actual, certain threat of capture, POTTHREAT is combined threat, or ko POTDEAD is dead group between two groups */
# define EXTEND 0
# define VITAL 1
# define THREAT 2
# define POTTHREAT 3
# define CONNECT 4
# define UNDERCUT 5
# define POTDEAD 6
# define NOPOT 7

# define BAD_MOVE -20000

# define HP150 0
# define HP239X 2
# define HP264X 1

# define UP 0
# define LEFT 1
# define RIGHT 2
# define DOWN 3

/* for eyevitrec[] */

/* # define EYEMASK 0xff */
/* # define EYEVALSHIFT 8 */
/* # define EYEVALMASK (int)0x003f */

/* cached value in eyevitval is not valid */
# define EYEVALNONE 63

/* liberties for reading at vital points */
# define EYEVITLIBS 2

/* for eyevital list */
# define EYEPOINTMASK 0x3fff
# define EYEFLAGMASK 0xc000
# define EYEADDONLY 0x8000
# define EYERMONLY 0x4000
/*# define V_NOEYEVAL (EYEVALNONE<<EYEVALSHIFT) */
/* # define V_EYE(v) ((eye_t)(v&EYEMASK)) */
/* # define V_VAL(v) ((v>>EYEVALSHIFT) & EYEVALMASK) */

/* for eyetype[] */

# define NOEYE 0
# define ONEPOINTEYE 1
# define TWOPOINTEYE 2
/* DEADEYE is a single dead group, not including liberties */
# define DEADEYE 3
# define THRTEYE 4
# define BIGEYE 5
# define LINEEYE 6
# define FOURPTBLOCKEYE 7
# define OPENLINEEYE 8
# define CORNEREYE 9
# define NEAREDGEEYE 10
# define VERYBIGEYE 11
# define UNKNOWNEYE 12

/* for eyestatus[] */

# define ACC 0
# define QUICK 1
# define SLOW 2

/* for move reasons */

# define ATTACK 1
# define DEFEND 2
# define PATTERNMV 4
# define DEFVAL1 8
# define DEFVAL2 16
# define RUN 32
# define ATKVAL1 64
# define ATKVAL2 128
# define JOSEKIMOVE 256
# define PATBETTER 512
# define NAMEGROUP 1024
/* this is a big move - only the biggest value should be added */
#define BIGMOVE 2048

# define FUSEKI 0
# define MIDDLE 1
# define ENDGAME 2

/* for cnprot[] */

# define CANCONN(x) ((x) >= CAN_CONNECT && !PROTCVAL(x))
# define PROTCONN(x) (PROTCVAL(cnprot[x]))
# define PROTCVAL(x) ((x) > KO_CONNECT)
 /* 10/5/95 was >= KO_CONNECT! */

/* for cnprot[] */ 
/* cuttable connections */
/* can't connect must be zero for canconnlink */
/* any nonzero value must be able to connect */
/* update protnam if change these */

# define CANT_CONNECT 0
# define MIGHT_CONNECT 1
/* can connect in one move, or it can be cut in one move */
# define CAN_CONNECT 2
/* THRT is a can connect where one side is threatened, and otherwise it would be a solid connection */
# define THRT_CONNECT 3
/* can connect in one move.  If opponent tries to cut, can connect this link or another, and opponent gets to cut one */
# define SHARED_CONNECT 4
# define KO_CONNECT 5

/* uncuttable connections */

# define AJI_CONNECT 6
# define SOLID_CONNECT 7

# ifndef TRUE
# define FALSE 0
# define TRUE 1
# endif

# define NO 0
# define YES 1
# define MAYBE 2
  
# define MAXRTVAL 200
	/* divisor for adjusting radiated territory when done 
           MAXRTVAL is one point */
# define URGENT 1
# define NOT_URGENT 0

/* User interface types */

# define TERMINAL 0
# define X10 1
# define X11 2
# define IBMPC 3
# define MACINTOSH 4

/* data structure defines */

/* board */

# define S_EDGE(s) edge[(s)]
# define S_EDGE2(s) edge2[(s)]
# define S_XVAL(s) xval[(s)]
# define S_YVAL(S) yval[(s)]

# define S_GROUP(s) board[(s)]
# define S_ARMY(s) grarmy[board[(s)]]
# define S_COLOR(s) grcolor[board[(s)]]
# define S_EYE(s) eyerec[(s)]
# define S_URGENT(s) urgent[(s)]
# define S_SCORE(s) scoreval[(s)]
# define S_SHAPE_L(s) shapebrd[(s)]
# define S_ALIVE(s) gralive[board[(s)]]
# define S_NUMLIBS(s) grlibs[board[(s)]]
# define S_THREATENED(s) grthreatened[board[s]]
# define S_UNCTHREATENED(s) (grthreatened[board[s]] == 2)
# define S_NEUTRAL(s) (ld[s] == NEUTRALLD)
# define S_EMPTYNBRS(s) (lnbn[s])
# define S_KEYPOINT(s) mvs[grpieces[list[armygroups[grarmy[board[s]]]]]]

/* groups */

# define G_PIECE_L(g) grpieces[(g)]
# define G_ARMY(g) grarmy[(g)]
# define G_COLOR(g) grcolor[(g)]

# define G_SIZE(g) grsize[(g)]
# define G_LIB_L(g) grlbp[(g)]
# define G_NUMLIBS(g) grlibs[(g)]
# define G_NBR_L(g) grnbp[(g)]
# define G_ALIVE(g) gralive[(g)]
# define G_THREATENED(g) grthreatened[(g)]
# define G_ALPROB(g) gralprob[(g)] 
/* key reference point for a group for strat_rules for attack and defense */
# define G_KEYPOINT(g) mvs[grpieces[list[armygroups[grarmy[g]]]]]

/* armies */

# define A_GROUP_L(a) armygroups[(a)]
# define A_DEADGROUP_L(a) armydeadgroups[(a)]
# define A_GROUP(a) (list[armygroups[a]])
# define A_EYE_L(a) armyeyerec[(a)]
# define A_COLOR(a) grcolor[list[armygroups[(a)]]]

# define A_SIZE(a) armysize[(a)]
# define A_LIB_L(a) armylbp[(a)]
# define A_NUMLIBS(a) armylibs[(a)]
# define A_NBR_L(a) (list[armynbp[(a)]] == EOL? getarmynbp(a) : armynbp[a])
# define A_ALIVE(a) gralive[list[armygroups[(a)]]]
# define A_THREATENED(a) grthreatened[list[armygroups[(a)]]]
# define A_REALTHREATENED(a) (grthreatened[list[armygroups[(a)]]]==2)
# define A_POTTHREATENED(a) (grthreatened[list[armygroups[(a)]]]==1)
# define A_NUMEYES(a) armyeyes[(a)]
# define A_ALPROB(a) gralprob[list[armygroups[(a)]]]
# define A_KEYPOINT(a) mvs[grpieces[list[armygroups[a]]]]

/* moves */

# define M_SQUARE(m) mvs[(m)]
# define M_CAPT_L(m) mvcapt[(m)]
# define M_CONN_L(m) mvconn[(m)]
# define M_COLOR(m) mvcolor[(m)]
# define M_NEXT(m) mvnext[(m)]
# define M_KOSAVE(m) kosave[(m)]

/* eyes */

# define E_TYPE(e) eyetype[(e)]
# define E_SQR_L(e) eyeptr[(e)]
# define E_VITAL_L(e) eyevital[(e)]
# define E_VAL(e) eyeval[(e)]
# define E_POT(e) eyepot[(e)]
# define E_MIN(e) eyemin[(e)]


# define ADDBITS(s,c) addbits(s,c)
# define DELBITS(s,c) delbits(s,c)

# define MAXBOXR 22
# define MAXERROR 180
# define ERRORLENGTH MAXBOXR


/* maximum distance to radiate influence */
# define MAXDIST 14

/*
 * number of hash values for one position on the board
 * 16 for the full board - rotation and reflection, color reversal
 * 4 for each of 4 11x19 edge - reflection and color reversal
 * 4 for each of 4 11x11 corners - reflection and color reversal
 * 2 for each of 8 6x11 half-edge - color reversal 
 */
#define HASH_ARRAY_SIZE (16 + 4*4 + 4*4 + 8*2)

#endif
