/* Copyright 1984-2001 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* Header code for pattern matching */

/* for machine independence use the following typedefs */


typedef unsigned char UINT8;	/* 8 bit unsigned integer */
typedef unsigned short UINT16;  /* 16 bit unsigned integer */
typedef unsigned int UINT32;  /* 32 bit unsigned integer */




# define NUMATTS 8
/* hashfactor is average number of hash chains pattern is in */
# define HASHFACTOR 10
# define NUMHASHCHAINS 256
# define BHASHFUNC(bmp) ((int)( (((bmp)[1]) & 0xe0) | ((((bmp)[2]) >> 3) & 0x1c) | ((((bmp)[0]) >> 5) & 0x3) ))
# define PHASHFUNC(bmp) ((int)( ((~(bmp)[1]) & 0xe0) | (((~(bmp)[2]) >> 3) & 0x1c) | (((~(bmp)[0]) >> 5) & 0x3) ))

/* attribute is one of following conditions */

#define AT_NONE 0
#define AT_GTALIVE (1<<5)
#define AT_LTALIVE (2<<5)
#define AT_EQALIVE (3<<5)
#define AT_THICK 27
#define AT_ALIVE 28
#define AT_UNSETTLED 29
#define AT_WEAK 30
#define AT_DEAD 31
#define AT_GTLIB (4<<5)
#define AT_LTLIB (5<<5)
#define AT_EQLIB (6<<5)
#define AT_THREATENED 1
#define AT_NOTTHREATENED 2
#define AT_G1 3
#define AT_G2 4
#define AT_H1 5
#define AT_H2 6
#define AT_H3 7
#define AT_STABLE 8

# define P_ANYWHERE (0<< 6)
# define P_EDGE (1<<6)
# define P_CORNER (2<<6)
# define WHEREMASK 0xc0

# define MV_COLOR 0x80
# define MV_SIBLING 0x40
# define MV_FINAL 0x20

# define PT_NORMAL 0
# define PT_CUT 1
# define PT_SURROUND 2
# define PT_KILL 3
# define PT_INVADE 4
# define PT_ENDGAME 5
# define PT_LOCAL 20
#define PT_NOCONN 0
#define PT_CANCONN 1
#define PT_SHARED 2
#define PT_KO 3
#define PT_AJI 4
#define PT_SOLID 5
#define PT_READCUT 6
#define PT_READSHR 7
#define PT_READAJI 8
# define TYPEMASK 0x3f

# define PM_SIBLING(ptr) (pm[ptr].csfv & MV_SIBLING)
# define PM_FINAL(ptr) (pm[ptr].csfv & MV_FINAL)
# define PM_COLOR(ptr) (((pm[ptr].csfv & MV_COLOR) >> 7) & 1)
# define PM_VALUE(ptr) (pmvalue[(pm[ptr].csfv & 0xf)])
# define PM_VAL(ptr)  (pm[ptr].csfv & 0xf)

# define NUMPATFILES 11  /* total number of pattern files */
# define NORMALFILES 9   /* number of files of normal patterns */
# define OBVIOUSFILE 9  /* file number for obvious moves */
# define ONEPOINTFILE 10 /* one point jump connections */
# define TEACHFILE 0
# define MAXPATTERNS 3000


/* do all stores of patterns using chars and comparisons using longs!
 * that way it doesn't matter what order chars are stored in the long
 */

union pat_union {
	UINT8 cv[8];
	UINT32 lv[2];
	};

#ifdef MAINPATTERN

UINT8 cbitmask[] = {
	0x80,0x40,0x20,0x10,0x8,0x4,0x2,0x1 };

int pevaltm[3] = { -1,1,0 };

#else
extern UINT8 cbitmask[];
extern int pevaltm[3];
#endif

/* Each 8x8 pattern is represented by bit vectors
 * from msb of first char, each vector starts in the upper
 * left corner and goes to the right.  small patterns are upper left
 * justified.  Edge is at the left and corner is in upper left.
 * pattern comparisons are done with longs.
 *
 *  All patterns are in one array, with all patterns in a file together.
 *
 *  IMPORTANT: long must be at least 32 bits!
 */

struct newpattern {    /* 44 bytes per pattern plus 2 bytes per move */
                       /* bit vectors have upper row in msbyte of first word, etc */
	union pat_union pw;    /* bit vector for white (1 if match white) */
	union pat_union pb;    /* bit vector for black (1 if match black) */
	union pat_union pe;    /* bit vector for empty (1 if match empty) */
	UINT8 attsqr[NUMATTS]; /* square with attribute 4 bits x (l-r) 4 bits y (t-b) */
			/* 0xff means end of attributes */
	UINT8 attval[NUMATTS]; /* attributes for stone on square */
	/* all attributes must be true for pattern to match */
	/* 3 bits type:     5 bits value
	   1  > aliveness   aliveness value
	   2  < aliveness      "
	   3  = aliveness      "
	   4  > liberties   liberty count
	   5  < liberties      "
	   6  = liberties      "
	   7  Special       pattern file specific
	                    for onepoint, all values have move trees
	                    	and G1 and G2 are groups to cut or connect
	                    
	   0  other         1 Threatened
	                    2 not threatened
                        3 1st group of cut or connection
                        4 2nd group of cut or connection
                                (1st and second group must be in different
                                 armies for pattern to match)
				 Specify G1 before G2!  if g1 or g2 are in urgdefarmies,
				 the move in the pattern is urgent.
				 		5 - first handle
				 		6 - second handle
				 		7 - third handle
           */
				 
	UINT16 moves;     /* index to first move in move tree */
	UINT8 wheretype;    /* Anywhere, edge, corner (2 msb) */
		     /* type of pattern: 6 lsb
		      FOR full board patterns (files 0-8):
			  0 - normal pattern - no special bonuses
			  1 - cut/connect pattern
			  2 - surround/escape pattern
			  3 - live/die pattern
			  4 - invade/surround
			  5 - endgame pattern
			  6 - pro?
			  7 - local?
			  8 - tesuji?
				
			  FOR one point jump file
              for the onepoint file, 
              0 can't connect
              1 can connect
              2 shared connect
              3 ko connect
              4 aji connect
              5 read for can/can't
              6 read for shared
              7 read for ko
              
			*/
	UINT8 size;     /* x and y size size of smallest pattern (in upper left) */
	               /* each is 1-9.  x is in left 4 bits */
};


/* move tree format (each move is two bytes)
 *
 *  byte 1 - XY  (4 bits X, 4 bits Y, 00 is upper left corner)
 *  byte 2 - CSFV 
 *           C 0-Black, 1-White
 *           S 1-move has sibling
 *           F 1-move is final in branch
 *           V Value 0-awful, 1-7 OK, 8-15 urgent - don't tenuki
 */

struct pmove {
	UINT8 xy;  /* 4 bits x, 4 bits y 0,0 is upper left*/
	UINT8 csfv;
};


/*
 * actual patterns.
 * -1 is end of chain
 */

struct phash {
	UINT16 pattnum;
	UINT16 next;
};

	
/* one struct for each current pattern match. */
	
struct newpatstruct {
	UINT8 orient; /* orientation of this pattern */
	UINT8 color;  /* color that matched 0-pattern, 1-reversed */
	UINT16 sqr;     /* square where pattern matches */
	UINT16 num;     /* pattern number of this pattern */
	UINT16 move;    /* index of next move from this pattern */
	UINT16 msptr;   /* msptr value when pattern matched */
};
	
struct pfile {
	UINT16 numpats;   /* number of patterns */
	UINT16 nummoves;  /* number of moves */
	UINT16 size;  /* number of bytes needed for this file - patterns, moves,
		      and hash chains */
	UINT16 firstpat;   /* index of first pattern from this file */
	UINT16 nextpat;    /* index of pattern after last from this file */
	UINT16 realmoves;  /* actual number of moves read from file */
};

/* access functions into g2pat.c */
int gettype(int pat);
