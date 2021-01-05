/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

# include "g2basic.h"
#include <memory.h>  
#ifdef G2DEBUGOUTPUT
# include <stdio.h>
#endif

# define NUMGEN 120
# undef S_NEUTRAL
# undef NEUTRALLD            

static int livesordies(sqr_t starts,int maxlibs,int tm,int color,int ply,listval_t ldrno,int *efflibs, int *maxefflibs, group_t gs, int maxply,int *result, int winsko, int nodesleft);
static void defonelib(int ply,sqr_t starts,group_t g,int tm,group_t gs,listval_t ldrno,int winsko);
static void gendefmoves(int ply, sqr_t starts,group_t g,int tm,listval_t ldrno,int maxlibs,int winsko,int maxbr, int efflibs, int maxefflibs);
/*static void deftwolibs(int ply, sqr_t starts,group_t g,int tm,group_t gs); */
static void genatkmoves(int ply, sqr_t starts,group_t g,int tm,listval_t ldrno,int maxlibs,int winsko,int maxbr);
static void genrestatk(int ply, sqr_t starts,group_t g,int tm,group_t gs,listval_t ldrno,int maxlibs,int winsko);
static void sortmoves(int mvp, int winsko, int ply, int maxbr, int tm, char *name, group_t g, group_t gs, int maxlibs);
static sqr_t jump(sqr_t lib, group_t g);
static void generatemoves(int ply, sqr_t starts, group_t g, int tm, listval_t ldrno, int maxlibs, int winsko, int maxbr, int efflibs, int maxefflibs);
static int makeldrmove(int ply, sqr_t starts, group_t g, int tm, listval_t ldrno, int maxlibs, int winsko, int maxbr);
int solidconnect(sqr_t s, int c, int winsko);

extern int cfac[3];
extern int numtacnodes;		/* number of tactical nodes used in a single search */
extern int maxbranchdepth[NUMLEVELS];
extern unsigned char conncapsize[NUMLEVELS];
extern char connmost[NUMLEVELS];
  
static int numnn;  /* number of nodes that have been visited so far */
static sqr_t mmov[NUMGEN];  /* move generated during lookahead */
static int mval[NUMGEN];  /* value of move generated */
static int mvp;	  /* pointer into these arrays */
static int maxnply[NUMPLY]; /* maximum nodes to try at this ply or below */
static int sply[NUMPLY];  /* first move in mvs[] at each ply */
static int eply[NUMPLY];  /* last move in mvs[] at each ply */

/*#define MOVESORT */

#ifdef MOVESORT

#define NUMBEST 4

static int mprob[NUMGEN];	/* prob that this move will work */
static sqr_t bestresp[PASS][NUMBEST];
static int bestcount[PASS][NUMBEST+1];
static int worstcount[PASS][NUMBEST+1];

#endif

void initbestresp() {
#ifdef MOVESORT
	int i, j;
	for (i = 0; i < PASS; ++i) {
		for (j = 0; j < NUMBEST; ++j) {
			bestresp[i][j] = NOPOINT;
			bestcount[i][j] = 0;
			worstcount[i][j] = 0;
		}
		bestcount[i][NUMBEST] = 0;
		worstcount[i][NUMBEST] = 0;
	}
#endif
}

#ifdef MOVESORT

/* 0 to 100 likelihood that this move will work. -1 is unknown */
int bestprob(sqr_t move, sqr_t resp) {
	int i;
//	return -1;
	for (i = 0; i < NUMBEST; ++i) {
		if (bestresp[move][i] == resp && bestcount[move][i] > 50)
			return 100*bestcount[move][i]/(bestcount[move][i]+worstcount[move][i]);
	}
	return -1;
}


/* resp refutes move */
void addbestresp(sqr_t move, sqr_t resp) {
	int i;
#ifdef CHECK
	if (move < 0 || move > PASS || resp < 0 || resp > PASS)
		outerr("bad move");
#endif
	for (i = 0; i < NUMBEST; ++i) {
		if (bestresp[move][i] == resp) {
			bestcount[move][i]++;
			return;
		}
	}
	for (i = 0; i < NUMBEST; ++i) {
		if (bestcount[move][i] == 0) {
			bestresp[move][i] = resp;
			bestcount[move][i] = 1;
			return;
		}
	}
	bestcount[move][NUMBEST]++;	/* unmatched */
}

/* resp refutes move */
void addworstresp(sqr_t move, sqr_t resp) {
	int i;
	for (i = 0; i < NUMBEST; ++i) {
		if (bestresp[move][i] == resp) {
			worstcount[move][i]++;
			return;
		}
	}
	worstcount[move][NUMBEST]++;	/* unmatched */
}

#endif

#ifdef G2DEBUGOUTPUT

// dump generated tactical moves for group at cursorpos.
// tm moves first, winsoko wins the kos

void dumpgenmoves(sqr_t cursorpos, int tm, int winsko) {
	group_t g;
	int debugtmp;
	int efflibs, maxlibs, result, ret;
	char buf[100];
	g = board[cursorpos];
	if (g == NOGROUP)
		return;
	debugtmp = debug;
	debug = 2000+g;
	numnn = 0;
	ret = livesordies(cursorpos, taclibs[playlevel], tm, S_COLOR(cursorpos), 0, NOGROUP, &efflibs, 
		&maxlibs, board[cursorpos], 10, &result, winsko, 100);
	efflibs = getefflibs(g, 4 ,NOGROUP, &maxlibs);
	sprintf(buf, "libs %d, eff libs %d-%d, livesordies: %d, %d\n", grlibs[g], efflibs, maxlibs, ret, result);
	outerr(buf);
	generatemoves(1, cursorpos, g, tm, NOGROUP, taclibs[playlevel], winsko, mvmost[playlevel], efflibs, maxlibs);
	debug = debugtmp;
}

#endif

/* ldrhash are xor of the zobhash for all w or b stones
 * added or removed from the board.  xor a zob when a stone is played
 * and for each stone is captured group.  xor again for stones removed
 * or groups restored.  ldrhash is a 64 bit code for each
 * position.
 */
int ldrhash[2];


static int hasht[256][2];	/* hash table of ldrhash values for all white stone changes */
/* 0-lose, 1-win, 5-not set yet */ 
static char hres[256];   /* result for this position */
static int hkosquare[256];	/* ko square must be the same as well as piece positions */
static int htomove[256];    /* side to move must be the same as well */

/* hash ldrhash down to 8 bits */
int makehash(void) {
	int h = ldrhash[0] ^ ldrhash[1];
	h = h ^ (h>>16);
	h = h ^ (h >> 8);
	return (h&0xff);
	}

void sethash(int score, int tm) {
	int h = makehash();		       /* set a hash table entry */
	hres[h] = score;
	hasht[h][0] = ldrhash[0];
	hasht[h][1] = ldrhash[1];
	hkosquare[h] = kosquare;
	htomove[h] = tm;
	}

/* put stone at s, color c and check if group with stone at gs can be captured
 * if ctm moves first.  Return TRUE if can be captured.with winsko winning all kos
 */

int canbecaptured(sqr_t s, int c, sqr_t gs, int ctm, int winsko, int depth, int size, int libs, listval_t ldrno)
{
	int can_cap;
	sqr_t tmp;

	mvs[msptr] = s;
	mvcolor[msptr] = (char)c;
	if (lupdate(msptr)) {
#ifdef CHECK
		if (debug && showtactics > 1)
			outstone(s,"P");
#endif		
		++msptr;
		if (board[gs] != NOGROUP)
	   		can_cap = iscaptured(board[gs], depth, size, libs, connmost[playlevel], ctm, ldrno, &tmp, winsko);
		else
			can_cap = TRUE;
		--msptr;
	}
	else
		can_cap = FALSE;
	ldndate(msptr);
	return(can_cap);
}

static void zerowins() {}
static void zerowinseye() {}
static void firstwins() {}
static void firstwinseye() {}
static void secondwins() {}
static void secondwinseye() {}
static void thirdwins() {}
static void thirdwinseye() {}
static void fourthwins() {}
static void fourthwinseye() {}
static void morewins() {}
static void morewinseye() {}
static void returnsfalselibs() {}
static void returnsfalselibseye() {}
/* static void returnsfalsenodes() {} */
static void returnsfalse() {}
static void returnsfalseeye() {}
static void returnstrue() {}
static void returnstrueeye() {}
  
/* iscaptured does a tactical analysis of a group.  It returns true if
 * the group is captured and false if it can escape.  g is the group number
 * to be checked.  maxply, maxnodes, and maxlibs control the depth, size
 * and complexity of the search.  
 * maxply is the maximum depth of the search
 * maxnodes is the maximum number of nodes to look at not counting
 *   nodes where the move generator only generates one move
 *   (only counts nodes that branch)  MUST BE < 128!
 * maxlibs is the maximum number of liberties.  if the group gets more
 *   liberties than maxlibs it has escaped.
 * maxbr is the maximum branch factor.  Look at maxbr+1 moves for first 3
 *   ply, maxbr moves up to maxbranch, and one or two moves after maxbranch.
 * tm is the color to move first.
 * ldrno is the index of this search in the grldr array (or NOGROUP)
 * return the move which worked at the first ply in move
 * moves made in this search will be linked in to the
 * grldr and ldrflag lists unless ldrno is NOGROUP
 * winsko is the color that wins any ko (BLACKCOLOR, WHITECOLOR, NOCOLOR)
 */

int iscaptured(group_t g, int maxply, int maxnodes, int maxlibs, int maxbr,
	int tm, listval_t ldrno, sqr_t *movethatworks, int winsko) {

	int scrply[NUMPLY];  /* score at each ply 1=wins, -1 = loses */
	int newnode,done,ply,leaf,color,madeamove;
	int h;
	sqr_t starts;
	int nbptr,ptr,efflibs,maxefflibs;
	group_t g2;
#ifdef CHECK
	char buf[100];
	int oldmaxnply;
#endif
	ASSERT(list[EOL] == 0xffff);
  
	*movethatworks = NOSQUARE;
	if (grlibs[g] == 1) {
		*movethatworks = list[grlbp[g]];
		if (tm == 1-grcolor[g] && grsize[g] == 1 && snapback(g, winsko)) {
			return FALSE;	/* 9/05 can't capture stone in a snapback */
		}
		if (tm == grcolor[g])
			for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr])
				if (grlibs[list[ptr]] == 1 && grsize[list[ptr]] > 1)
					*movethatworks = list[grlbp[list[ptr]]];
	}

	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr])
		if (lnbn[list[ptr]] == 3)*movethatworks = list[ptr];
#ifdef CHECK
	if (ldrno >= NUMGROUPS+NUMCONNS+NUMEYERECS ||
	   ldrno < NUMGROUPS-1 && !grlv[ldrno]) {
		sprintf(buf,"bad ldrno %d in iscaptured\n",ldrno);
		outerror(buf);
	}

	if (g == NOGROUP) {
		outerror("iscaptured called with NOGROUP!\n");
		return TRUE;
	}
#endif


	numnn = 0;
	if (maxply > NUMPLY - 5)
		maxply = NUMPLY - 5;
	starts = mvs[grpieces[g]];
  
/*  mark neighboring groups */
	if (ldrno != NOGROUP) {
		for (nbptr = grnbp[g]; nbptr != EOL; nbptr = link[nbptr]) {
			g2 = (group_t)list[nbptr];
			if (grlibs[g2] > 4)continue;
  
			if (addlist(ldrno,&ldrflag[mvs[grpieces[g2]]])) {
				adflist(mvs[grpieces[g2]],&grldr[ldrno]);
			}
		}
	}

	ldrhash[0] = ldrhash[1] = 0;	/* clear the hash and hash table. */
	memset(hres, 5, 256);
	done = FALSE;
	color = grcolor[g];
	ply = 1;
	sply[0] = msptr - 1;
	eply[0] = msptr;
	sply[1] = eply[0];
	scrply[0] = -1;
	scrply[1] = -1;

	maxnply[1] = maxnodes;  

	newnode = TRUE;
	while(!done) {    /* this should be recursive */
		leaf = FALSE;
		madeamove = FALSE;
		if (newnode) {  /* first time to this node */
			h = makehash();
			if (hres[h] != 5 && 
				hasht[h][0] == ldrhash[0] && 
				hasht[h][1] == ldrhash[1] &&
				hkosquare[h] == kosquare &&
				htomove[h] == tm) {
				scrply[ply] = hres[h];
				leaf = TRUE;
#ifdef CHECK
				if (debug == 2000+(unsigned int)g && leaf) {
					sprintf(buf, "hash table hit found. val %d color %d.  Hit enter\n", hres[h], tm);
					outerr(buf);
					waitaction(); clearerror();
					}
#endif
				}
			else 
			/* is this end of this branch? */
				leaf = livesordies(starts,maxlibs,tm,color,ply,ldrno,&efflibs,&maxefflibs,g,maxply,&scrply[ply],winsko,maxnply[ply]);
#ifdef CHECK
			if (debug == 2000+(unsigned int)g && leaf) {
				outerr("Leaf found.  Hit enter\n"); waitaction(); clearerror();
				}
#endif
			if (!leaf) {  /* not end of branch, generate move list */
				generatemoves(ply,starts,g,tm,ldrno,maxlibs,winsko,maxbr,efflibs,maxefflibs);
#ifdef CHECK
				if (eply[ply]-sply[ply] > 20)
					outerr("too many moves, over 20");
#endif
				}
			}
#ifdef MOVESORT
		if (ply > 2 && !newnode && scrply[ply] == 1) {	/* have a winning move (sply[ply-1]) */
			addbestresp(mvs[sply[ply-2]], mvs[sply[ply-1]]);
		}
		if (ply > 2 && !newnode && scrply[ply] == -1) {	/* have a winning move (sply[ply-1]) */
			addworstresp(mvs[sply[ply-2]], mvs[sply[ply-1]]);
		}
#endif

		if (!leaf &&  /* need to make a move to next ply */
			scrply[ply] != 1 &&	  /* not beta cutoff */ 
			(newnode || numnn < maxnply[ply])) { /* no new siblings if out of nodes */ 
#ifdef CHECK
			if (debug == 2000+(unsigned int)g) {
				sprintf(buf,"ply %d numnn-%d maxnply-%d\n",ply,numnn,maxnply[ply]);
				outerr(buf);
			}
#endif
			madeamove = makeldrmove(ply,starts,g,tm,ldrno,maxlibs,winsko,maxbr);
			if (madeamove) {
				if (ply == 1) {
					*movethatworks = mvs[sply[ply]];
				}
				++ply;
				tm = 1-tm;
				scrply[ply] = -1;  /* assume not winning at new ply */
				newnode = TRUE;
			}
		}
		if (leaf || !madeamove) {  /* reached bottom of this line or end of moves */
			if (ply == 1) {
				done = TRUE;
			}
			else {
				sethash(scrply[ply], tm);
				--ply;
#ifdef CHECK
				oldmaxnply = maxnply[ply+1];
				if (debug == 2000+(unsigned int)g) {
					fixsd(sply[ply],FALSE);
				}
#endif
				ldndate(sply[ply]);
				tm = 1 - tm;
				++sply[ply];
				scrply[ply] = -scrply[ply+1];
				if (sply[ply] < eply[ply] && ply <= 2 && maxnply[ply] - numnn < maxnodes/5)
					maxnply[ply] = numnn + maxnodes/5;	/* 1/03 make sure all moves tried at first two ply */
				if (sply[ply] < eply[ply] && /* at least one more move left */
					maxnply[ply] - numnn >= 25)
					maxnply[ply+1] = numnn + ((maxnply[ply]-numnn)*4)/5;
				else
					maxnply[ply+1] = maxnply[ply];
				newnode = FALSE;
#ifdef CHECK
	            if (debug == 2000+(unsigned int)g) {
					if (leaf)
			        	outerr("Backing up - leaf node");
					else if (scrply[ply+1] == 1)
	    	        	outerr("Backing up - have a win - no need for more moves");	
					else if (sply[ply+1] >= eply[ply+1])
						outerr("Backing up - tried all moves");
					else {
						sprintf(buf, "backing up - numnn %d too big ( >= %d)", numnn, oldmaxnply);
	    	        	outerr(buf);	
					}
					waitaction();  clearerror();
				}
#endif
			}
		}
	}  /* end of while !done loop */
	if ((scrply[1] * cfac[tm == color]) == 1) {
		returnsfalse();
		if (maxlibs == 2)
			returnsfalseeye();
		if (sply[1] == eply[0]) {
			returnsfalselibs();
			if (maxlibs == 2)
				returnsfalselibseye();
			}
		return(FALSE);
		}
	else {
		if (sply[1] == eply[0]) {
			zerowins();
			if (maxlibs == 2)
				zerowinseye();
			}
		else if (sply[1] == eply[0]+1) {
			firstwins();
			if (maxlibs == 2)
				firstwinseye();
			}
		else if (sply[1] == eply[0]+2) {
			secondwins();
			if (maxlibs == 2)
				secondwinseye();
			}
		else if (sply[1] == eply[0]+3) {
			thirdwins();
			if (maxlibs == 2)
				thirdwinseye();
			}
		else if (sply[1] == eply[0]+4) {
			fourthwins();
			if (maxlibs == 2)
				fourthwinseye();
			}
		else {
			morewins();
			if (maxlibs == 2)
				morewinseye();
			}
		returnstrue();
		if (maxlibs == 2)
			returnstrueeye();
		return(TRUE);
		}
	}


/* estimate how many new (not total!) liberties can get for liberty list of g at s */
/* s is liberty of g or captures single liberty adjacent group */
/* can stop early if reach max */

static int newliberties(group_t g, list_t liblist, sqr_t s, int max) {
	int c,newlibs = 0;
	list_t ptr,ptr2,ptr3,connlist = EOL;
	c = grcolor[g];
	for (ptr = nbgrp[s][1-c]; ptr != EOL; ptr = link[ptr])
		if (grlibs[list[ptr]] == 1) {  /* move capures enemy */
			newlibs += grsize[list[ptr]];
			for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
				if (list[ptr2] != g)
					newlibs += mrglist(grlbp[list[ptr2]],&connlist); 
			}
	if (connlist == EOL && link[nbgrp[s][c]] == EOL)  /* simple case */
		return newlist(liblist,nblbp[s]) + newlibs - 1;
	mrglist(liblist,&connlist);
	newlibs += mrglist(nblbp[s],&connlist) - 1;
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr])
		if (list[ptr] != g) {  /* friendly connection */
			newlibs += mrglist(grlbp[list[ptr]],&connlist);
			if (newlibs >= max)
				break;
			for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if ((sqr_t)list[ptr2] == s)continue;
				if (lnbn[list[ptr2]] <= 1 && lnbf[list[ptr2]][1-c] == 0)
					for (ptr3 = nbgrp[list[ptr2]][c]; ptr3 != EOL; ptr3 = link[ptr3])
						if (list[ptr3] != list[ptr])
							newlibs += mrglist(grlbp[list[ptr3]],&connlist);
				}
			}
	if (connlist != EOL)killist(&connlist);
	return newlibs;
	}
     
#ifdef G2DEBUGOUTPUT     
void dumpnewlibs(sqr_t s) {
	list_t ptr;
	int libs;
	char buf[20];
	if (board[s] == NOGROUP) {
		outerr("Click on a group");
		return;
		}
	for (ptr = grlbp[board[s]]; ptr != EOL; ptr = link[ptr]) {
		libs = newliberties(board[s], grlbp[board[s]], list[ptr], 100);
		sprintf(buf,"%d",libs);
		outstone(list[ptr],buf);
		}
	}
#endif	

/* generate moves */
	
static void generatemoves(int ply, sqr_t starts, group_t g, int tm, listval_t ldrno, int maxlibs, int winsko, int maxbr, int efflibs, int maxefflibs) {
	ASSERT(list[EOL] == 0xffff);

	maxnply[ply+1] = maxnply[ply];
	/* generate move list for this ply */
	if (tm == grcolor[g])
		gendefmoves(ply,starts,g,tm,ldrno,maxlibs,winsko,maxbr,efflibs, maxefflibs);
	else
		genatkmoves(ply,starts,g,tm,ldrno,maxlibs,winsko,maxbr);

#ifdef CHECK
	if (eply[ply]-sply[ply] > 6)
		outerr("too many moves, over 6");
#endif
	if (eply[ply] - sply[ply] == 1)--numnn;  /* don't count forced moves */
	}


	
/* make a move from the generated move list */
	
static int makeldrmove(int ply, sqr_t starts, group_t g, int tm, listval_t ldrno, int maxlibs, int winsko, int maxbr) {
	int mademove;
	group_t g2;
#ifdef CHECK
	char buf[100];
#endif

	/* make best move */
	mademove = FALSE;
/*	if (numnn <= maxnply[ply]) */
		while(!mademove && sply[ply] < eply[ply]) {  
			upldrflags(sply[ply],ldrno);
			mademove = lupdate(sply[ply]);
			if (kosquare != NOSQUARE && 1-tm == winsko)mademove = FALSE;
			if (mvs[sply[ply]] != PASS) {
				g2 = board[mvs[sply[ply]]];
				if (g2 == board[starts] && grlibs[g2] == 1)
				  	mademove = FALSE;
				else if (grlibs[g2] == 1 && grsize[g2] > 4)  /* throwins ok */
					mademove = FALSE;
			}
			if (!mademove) {
#ifdef CHECK
				if (debug == 2000+(unsigned int)g) {
					sprintf(buf,"bad or illegal move, tm %d, winsko %d ",tm,winsko);
					outerr(buf);
					ssqr(mvs[sply[ply]],buf);
					outerr(buf);
				}
#endif
				ldndate(sply[ply]);
				++sply[ply];
			}
		}
	if (mademove) {  /* found a move */
		++numnn;  /* count a node */
		++numtacnodes;
#ifdef CHECK
		if (debug == 2000+(unsigned int)g) {
			fixlm(mvs[sply[ply]], mvcolor[sply[ply]]);
		}
#endif
	}
	return(mademove);
}

	

/* generate moves to defend one liberty group gs
 * rules:
 *	pull out of atari, if end with two liberties
 *	  by entering open space or connecting
 *	capture neighbor with one liberty
 * make a ko if winsko
 */


static void defonelib(int ply, sqr_t starts, group_t g, int tm, group_t gs, listval_t ldrno, int winsko) {
	int mvptr,i,connect,numlibs,twolibs,l1,l2;
	sqr_t s, sn;
	int c, cancapture = FALSE;
	list_t tmplist=EOL, ptr, ptr2;
#ifdef CHECK	
	char buf[60],buf2[10];
#endif	
	mvptr = sply[ply];

	g = g;
	starts = starts;
	c = grcolor[gs];
	s = list[grlbp[gs]];

	/* capture one liberty neighbor */

	for (ptr = grnbp[gs]; ptr != EOL; ptr = link[ptr]) {
		if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]])) {
			adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
			}
		if (grlibs[list[ptr]] == 1 &&
		   ((sqr_t)list[grlbp[list[ptr]]] != kosquare || 
		    winsko == tm)) {
			if (lnbn[s] != 0 || (sqr_t)list[grlbp[list[ptr]]] != s || 
			    grsize[list[ptr]] > 1) {
				mvs[mvptr] = list[grlbp[list[ptr]]];
				mvcolor[mvptr++] = (char)tm;
				if ((sqr_t)list[grlbp[list[ptr]]] == s)cancapture = TRUE;
#ifdef CHECK
				if (debug == 2000+(unsigned int)g) {
					outerr("defonelib captures\n");
					if (winsko == tm)
						outerr("I win kos");
					}
#endif
				}
			}
		}


	/* pull out of atari */

	twolibs = lnbn[s] >= 2;
	connect = FALSE;
	if (lnbn[s] < 2) {	/* can connect or capture? */
		numlibs = lnbn[s];
		cpylist(nblbp[s],&tmplist);
		addlist(s,&tmplist);
		for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr])
			if ((group_t)list[ptr] != gs)
				numlibs += mrglist(grlbp[list[ptr]],&tmplist);
		for (ptr = nbgrp[s][1-c]; ptr != EOL; ptr = link[ptr])
			if (grlibs[list[ptr]] == 1) {
				numlibs += grsize[list[ptr]];;
				if (numlibs < 2)
					for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
						if (list[ptr2] != gs)
							numlibs += mrglist(grlbp[list[ptr2]],&tmplist);
				}
		if (numlibs > 1)connect = TRUE;
		killist(&tmplist);
		}
	if (lnbn[s] == 2 && lnbf[s][tm] == 1 && !connect) {
		l1 = list[nblbp[s]];
		l2 = list[link[nblbp[s]]];
		if (lnbf[l1][1-c] == 0 && lnbf[l2][1-c] == 0 && 
		   ((lnbn[l1] >= 2 && lnbn[l2] <= 2) ||
		    (lnbn[l2] >= 2 && lnbn[l1] <= 2)))twolibs = FALSE;
		}
	if (!cancapture && (twolibs || connect)) {
		mvs[mvptr] = s;
		mvcolor[mvptr++] = (char)tm;
# ifdef CHECK
		if (debug == 2000+(unsigned int)g)outerr("defonelib pulling out of atari\n");
#endif
		}


	/* make a ko */
	if (winsko == tm && grsize[gs] == 1 && lnbf[s][1-c] == 0 && lnbn[s] == 1) { 
		sn = list[nblbp[s]];
		if (lnbn[sn] > 1 || lnbf[sn][c]) {
			mvs[mvptr] = sn;
			mvcolor[mvptr++] = (char)tm;
# ifdef CHECK
			if (debug == 2000+(unsigned int)g)outerr("defonelib trying for ko\n");
#endif
			}
		}

	eply[ply] = mvptr;
	for (i = sply[ply]; i < eply[ply]; ++i) {
		mvcolor[i] = (char)tm;
#ifdef CHECK
		if (debug == 2000+(unsigned int)g) {
			sprintf(buf,"defone genning %s",ssqr(mvs[i],buf2)); 
			outerr(buf);
			}
#endif
		}
	}



/* defend nbr of lastmove in atari 
 * defend if nbr has one liberty
 * and can get more by pulling out
 * or by capturing
 * makes double ataris work 
 */

int defatari(int ply, group_t g, int tm, group_t gs, int lastmove, int winsko) {
	int mvptr,numlibs,j,i,ldtmp;
	sqr_t s,sn;
	group_t gn;
	list_t ptr2;
#ifdef CHECK	
	char buf[80],buf2[10];
#endif
	g = g;	
	if (winsko == grcolor[gs])
		return FALSE;  /* if I win kos also ignore threats to other groups */
	if (lastmove == PASS)return(FALSE);
   	mvptr = sply[ply];

	i = fdir[lastmove];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		s = lastmove+nbr[i];
		if (S_COLOR(s) != G_COLOR(gs))continue;
		gn = board[s];
		if (gn == gs)continue;
		if (grlibs[gn] > 1)continue;  /* find one liberty groups adjacent to last move */
		if (grsize[gn] < grsize[gs] - 2 &&  /* small group */
			grpieces[gn] > sply[0]) {		   /* created by move during this fight */
			continue;    /* OK not to answer atari */
			}
/*		if ((S_ALIVE(s)&31) >= WEAK)continue;  ALIVE is not generally valid here */
		/* gn is a one liberty group adjacent to the last move */
		for (ptr2 = grnbp[gn]; ptr2 != EOL; ptr2 = link[ptr2])
			if (grlibs[list[ptr2]] == 1 && 
			   (grsize[list[ptr2]] > 2 ||
			    list[ptr2] == board[lastmove]))  /* first capture to save it */
				mvs[mvptr++] = list[grlbp[list[ptr2]]];
		numlibs = 0;
		sn = list[grlbp[gn]];
		if (inlist(sn,&grlbp[gs]))continue;  /* don't fill liberty of group chased */
		numlibs += lnbn[sn];

		for (ptr2 = nbgrp[sn][grcolor[gs]]; ptr2 != EOL; ptr2 = link[ptr2])
			if (grlibs[list[ptr2]] >= 2)
				numlibs += newlist(nblbp[sn],grlbp[list[ptr2]])-1;

		for (ptr2 = nbgrp[sn][1-grcolor[gs]]; ptr2 != EOL; ptr2 = link[ptr2])
			if (grlibs[list[ptr2]] == 1)
				numlibs += grsize[list[ptr2]];

		if (numlibs > 1) /* gain more liberties by saving, so save group */
			mvs[mvptr++] = sn;
		for (ptr2 = grnbp[gn]; ptr2 != EOL; ptr2 = link[ptr2])
			if (grlibs[list[ptr2]] == 1 && 
				(sqr_t)list[grlbp[list[ptr2]]] != kosquare && 
				list[grlbp[list[ptr2]]] != list[grlbp[gn]] && 
				grsize[list[ptr2]] <= 2 && list[ptr2] != board[lastmove])
				mvs[mvptr++] = list[grlbp[list[ptr2]]];  /* capture to save it */
		eply[ply] = mvptr;
		for (j = sply[ply]; j < eply[ply]; ++j) {
			mvcolor[j] = (char)tm;
#ifdef CHECK
			if (debug == 2000+(unsigned int)g) {
				sprintf(buf,"defatari saving group %s",ssqr(mvs[j],buf2));
				outerr(buf);
				waitaction();
				}
#endif
			}
		if (eply[ply] != sply[ply])return(TRUE);
		}
	return(FALSE);
	}

/* g is a one liberty, one stone group.  Return TRUE if capturing
 * g leds to a snapback - play back in point where G is to
 * capture group that just played
 *
 *  Three interesting cases:
 *   
 *  ko:  recapture depends on ko threat return FALSE
 *
 *     * * *
 *     * + *
 *     O * O O
 *     + O * *
 *
 *  real snapback: recapturing stone is safe return TRUE
 *
 *     * * * * *
 *     * O O O *
 *     * O + * *
 *     * O * O O
 *     * * O O +
 *
 *     * * * * *
 *     * O O O *
 *     * O + O *
 *     * O * O *
 *     * * O * *
 *
 * 
 *  short of liberties:  recapturing stone can be captured itself
 *
 *    * * *  
 *    * O *
 *    * + *
 *    O * O
 *    O O O
 *
 */

	
int snapback(group_t g, int winsko) {
	sqr_t s;
	int c;
	list_t ptr;
	int nbr = FALSE;
	if (grlibs[g] > 1)  /* Must be one liberty group being captured */
		return FALSE;
	if (grsize[g] > 1)  /* must be one stone group */
		return FALSE;
	s = (sqr_t)list[grlbp[g]];  /* liberty of group being captured */
	c = grcolor[g];		/* color of group being captured */
	if (lnbn[s] != 0)return(FALSE);   /* will get more than one liberty */
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr])
		if (list[ptr] != g && grlibs[list[ptr]] == 1)
			return(FALSE);  /* another capture */
	for (ptr = nbgrp[s][1-c]; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[list[ptr]] > 1)return(FALSE);
		if (inlist(list[ptr], &grnbp[g]))
			nbr = TRUE;  /* will get at least 2 liberties */
		}
	if (nbgrp[s][1-c] == EOL && winsko == 1-c)  /* ko */
		return FALSE;
	if (!nbr)
		return FALSE;  /* case 3 */
	return(TRUE);
	}
	
	
/* return true if g can be captured in a snapback at s1 
 * s2 is g's other liberty 
 * already know that g has 2 liberties and no one liberty neighbors
 * note: one move earlier than snapback()
 */

static int issnapback(sqr_t s1, sqr_t s2, group_t g, int c) {
	list_t ptr;
	if (lnbn[s1] != 1 || (sqr_t)list[nblbp[s1]] != s2 || lnbf[s1][1-c] != 0)
		return FALSE;
	if (lnbn[s2] != 1 || link[nbgrp[s2][c]] != EOL)
		return FALSE; 
	for (ptr = nbgrp[s2][1-c]; ptr != EOL; ptr = link[ptr])
		if (grlibs[list[ptr]] == 1)
			return FALSE;
	return TRUE;			
	}


/* defender attack a neighboring group grl with fewer or same liberties.
 * gs group being defended.  grl is the neighboring group to attack.
 * grp is the group solidly connected to gs that is adjacent to this group
 * grp may be NOGROUP
 * g is for debug output.
 * libamt is effective libs of gs
 * grllibs is effective libs of grl
 * winsko is color that wins all kos
 * only generate full value for one attacking move to try to kill each nbr 
 * unless group chased has only two libs
 * don't generate attacking move which can be captured (unless it looks like
 * a good throwin - captures 1 or 2 stones at a cutting point).
 * value each neighboring liberty reduced at 16 and each total liberty at 16 also
 */

#define MAXATK 10

int def_atk_nbr(group_t gs, group_t grp, group_t grl, int mvp, group_t g, int libamt, int maxlibamt, int grllibs, int tm, listval_t ldrno, int winsko) {
#ifdef CHECK
	int i;
#endif	
	int val;   /* value of capturing group grl overall */
	int j,illegal_move,mvvtmp,mvvmax,mvls=0,next_to,c;
	int throwinok; /* it's ok to throwin here - put down stone with one liberty */
	int numcuts,iscut,savemvp,iscapture,newlibs,islib,nlibs,clibs, onelibnbr = FALSE;
	list_t lblist = EOL, lptr2, ptr, cptr, ptr2, commlibs = EOL;
	int connected;
	int highest = 0;  /* highest liberty of thhis group */
	int numexpand = 0;
	int nblibs = 0;	/* number of liberties can get from neighbor of grl */
	sqr_t s, sl;
	int passflag = TRUE;	/* pass if enemy can't get more liberties, might be a seki */
	sqr_t atksqr[MAXATK];
	int atkval[MAXATK];
	int nextatk = 0;
#ifdef CHECK
	char buf[10];
	char buf2[80];
#endif	
	
	g = g;
	c = grcolor[gs];
	savemvp = mvp;
	val = 16*libamt-16*grllibs;  /* 4/01 changed to grllibs and 8 to 16 value of attack of grl depends on number of liberties */
	if (val < 0)val = 0;
	if (grlibs[grl] == 2) {
		if (grsize[grl] > 4)val += 40;
		else 
			val += 8*grsize[grl];  /* attack is sente */
		}

	if (grlibs[grl] == 1 ) { /* can capture nbr */ 
		passflag = FALSE;
		val = 32 * lnbn[list[grlbp[grl]]];
		cpylist(nblbp[list[grlbp[grl]]],&lblist);
		if (grsize[grl] > 1) {
			val += 32*grsize[grl]+8;
			}
		else if (snapback(grl,winsko))
			return(mvp);  /* can't capture due to snapback */
		else val += 40;  /* since might make an eye */
		nlibs = 0;
		mrglist(grlbp[gs], &lblist);
		if (grp != NOGROUP)
			mrglist(grlbp[grp],&lblist);
		for (j = grnbp[grl]; j != EOL; j = link[j]) {
			if (list[j] != gs && list[j] != grp) {	/* cutting stones */
				andlist(grlbp[gs], grlbp[list[j]], &commlibs);
				connected = FALSE;
				for (ptr2 = commlibs; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (solidconnect(list[ptr2], c, winsko) == 1) {
						connected = TRUE;
						break;
						}
					}
				killist(&commlibs);
				if (!connected)
					nlibs += mrglist(grlbp[list[j]],&lblist);
				else 
					mrglist(grlbp[list[j]],&lblist);   /* already connected - no new liberties */
				if (grlibs[list[j]] == 1)
					onelibnbr = TRUE;
				}
			}
		if (onelibnbr)
			val += 16;  /* capture to save the neighbor */
		if (nlibs > 5)
			nlibs = 5;
		val += 32 * nlibs;
		if (lblist != EOL)killist(&lblist);
		if (!onelibnbr && lnbn[list[grlbp[grl]]] <= 1 && lnbf[list[grlbp[grl]]][1-c] == 1) {
			val -= 32;  /* already captured */
			if (lnbn[list[grlbp[grl]]] == 1) {
				mval[mvp] = val-16;
				mmov[mvp] = list[nblbp[list[grlbp[grl]]]];  /* try for eyespace */
				mvp++;
				}
			}
		}
	else {	   /* set val (value of capturing) if more than one liberty group */
		for (ptr = grlbp[grl]; ptr != EOL; ptr = link[ptr]) {
			if (edge[list[ptr]] > highest)
				highest = edge[list[ptr]];
			if (lnbn[list[ptr]] == 3 || link[nbgrp[list[ptr]][grcolor[grl]]] != EOL)
				++numexpand;	/* number of points can get more liberties */
			}
		if (highest > 5)highest = 5;
		val += 10-highest*2;   /* prefer to attack groups nearer edge */
		val += 8 * (1-numexpand);

		/* add possible extra liberties for capturing cutting stones */
		nblibs = 0;
		for (ptr = grnbp[grl]; ptr != EOL; ptr = link[ptr]) {
			if (list[ptr] == gs || list[ptr] == grp)continue;
			if (comlist(grlbp[list[ptr]], grlbp[gs]))continue;
			if (grlibs[list[ptr]] > nblibs)
				nblibs = grlibs[list[ptr]];
			}
		if (nblibs > 4)nblibs = 4;
		if (numexpand == 1)
			val += 4 * nblibs;
		else if (numexpand == 0)
			val += 6 * nblibs;
		}
	for (lptr2 = grnbp[grl]; lptr2 != EOL; 
			lptr2 = link[lptr2]) { 
			if (grlibs[list[lptr2]] == 1) {
				passflag = FALSE;
				onelibnbr = TRUE;
				if (grsize[list[lptr2]] > 2)
					val /= 2;	/* he can capture in response so attack is not so good */
			}
		}

	/* now have value of making an attack.  find moves to attack */
	if (val > 0 && grllibs <= libamt)  /* have time to save nbr of nbr with one liberty */
		for (lptr2 = grnbp[grl]; lptr2 != EOL; /* since saving this group counts as part of lbamt */
			lptr2 = link[lptr2]) {  /* save nbr of grl */
			if (list[lptr2] != gs && list[lptr2] != grp && grlibs[list[lptr2]] == 1 &&
				!inlist(list[grlbp[list[lptr2]]], &grlbp[gs])) { /* will be added in play_next */
				if (lnbn[list[grlbp[list[lptr2]]]] > 1 ||  /* liberties */
					lnbf[list[grlbp[list[lptr2]]]][c] > 1 || /* or friendly stones */
					link[grnbp[list[lptr2]]] != EOL) { /* or friendly nbrs */
					mval[mvp] = val + 16 + 8*grsize[list[lptr2]];
					if (grllibs == 1)
						mval[mvp] = val - 8;  /* prefer to capture nbr */
					if (grllibs == libamt)
						mval[mvp] /= 2;  /* might not work since libamt might not include saving this group if there is ako */
					mmov[mvp] = list[grlbp[list[lptr2]]];
					mvp++;
					passflag = FALSE;
					}
				for (ptr = grnbp[list[lptr2]]; ptr != EOL; ptr = link[ptr]) {
					if (grlibs[list[ptr]] == 1) {
						mmov[mvp] = list[grlbp[list[ptr]]];
						mval[mvp] = val + 16 + 8*grsize[list[lptr2]] + 4*grsize[list[ptr]];
						mvp++;
						passflag = FALSE;
						}
					}
				}
			}

	/* look at moves in liberties of grl */
	next_to = NOSQUARE;
	mvvmax = 0;
	numcuts = 0;
	for (lptr2 = grlbp[grl]; lptr2 != EOL; lptr2 = link[lptr2]) { /* take away a liberty from grl */
		sl = list[lptr2];  /* liberty of enemy group */
		islib = inlist(sl,&grlbp[gs]);  /* counted already in play_next_to_group */
		if (lnbn[sl] > 1)passflag = FALSE;
		mvvtmp = val + edge[sl];	/* value of making move at sl */
		if (grlibs[grl] > 1)	 /* lnbn already added for 1 lib group */
			mvvtmp += 16*(lnbn[sl]-comlist(grlbp[grl],nblbp[sl])-1);
		if (grlibs[grl] == 2 && !onelibnbr) {
			s = list[grlbp[grl]];
			if (s == sl)
				s = list[link[grlbp[grl]]];	 /* s is the other liberty from sl */
			if (link[nbgrp[s][grcolor[grl]]] == EOL && /* no connection */
				lnbn[s] <= 1 && 
				(lnbn[sl] > 1 || 
				lnbf[sl][c] && 
				    grlibs[list[nbgrp[sl][c]]] - 1 + newlist(grlbp[list[nbgrp[sl][c]]], nblbp[sl]) > 1 )) {
				mvvtmp += grsize[grl]*24;  /* might be able to capture */
				for (ptr = nbgrp[sl][c]; ptr != EOL; ptr = link[ptr])
					if (list[ptr] != gs)
						mvvtmp += (grlibs[list[ptr]]-1)*24;	/* get liberties of nearby groups also */
				}
			}
		if (lnbf[sl][c] != 0 &&
			islib) {  /* filling own liberty */
			mvvtmp += 8 * (lnbn[sl] - comlist(nblbp[sl],grlbp[gs]) - 1);
			}
		if (sl == kosquare)
			mvvtmp /= 2;	/* prefer not to fight ko */
		if (grlibs[grl] > 1 && lnbn[sl] == 1 && lnbf[sl][c] &&
			link[nbgrp[sl][c]] == EOL && /* defend snapback or throwin */
			grlibs[list[nbgrp[sl][c]]] == 2 &&
			list[nbgrp[sl][c]] != gs && list[nbgrp[sl][c]] != grp &&
			inlist(list[nblbp[sl]],&grlbp[list[nbgrp[sl][c]]])) {
			mmov[mvp] = list[nblbp[sl]];
			mval[mvp++] = mvvtmp + grsize[list[nbgrp[sl][c]]]*8;
			}

		/* attacking move also threatens to connect */
		if (!islib && 
			(comlist(nblbp[sl], grlbp[grp]) ||
			 comlist(nblbp[sl], grlbp[gs])) &&
			(lnbn[sl] > 2 || 
			lnbf[sl][c] > 0 && lnbn[sl] > 1)) {
				nlibs = lnbn[sl]-1;
				for (ptr = nbgrp[sl][c]; ptr != EOL; ptr = link[ptr])
					if (list[ptr] != grp && list[ptr] != grp)
						nlibs += grlibs[list[ptr]] - 1;
				if (nlibs > 5)nlibs = 5;
				if (grlibs[grl] == 2)
					mvvtmp += nlibs*32 - 16;  /* get these libs for sure */
				else
					mvvtmp += nlibs*8;  /* sente to get liberties */
			}
#ifdef NEVER		
		if (!islib && grlibs[grl] == 2 &&  /* atari and then connect to gs */
			comlist(nblbp[sl],grlbp[grp]) && (lnbn[sl] > 2 ||
			lnbf[sl][c] > 0 && lnbn[sl] > 1)) {
			mvvtmp += lnbn[sl] * 32 - 48;
#ifdef NEVER
			mmov[mvp] = sl;  /* can atari and connect */
			mval[mvp++] = mvvtmp + lnbn[sl] * 32 - 48;
/*			continue;  no need to continuw here - loses other bonus 
			values later for connecting to another group here*/
#endif
			}
#endif
		throwinok = grlibs[grl] == 1;  /* throwin OK if captures */
		iscut = 0;  /* number of liberties if play here */
		iscapture = FALSE;
		if (grlibs[grl] != 1 && /* no special value for cut if can capture */
			link[nbgrp[sl][grcolor[grl]]] != EOL) { /* this move is a cut */
			passflag = FALSE;
			for (ptr = nbgrp[sl][grcolor[grl]]; ptr != EOL; ptr = link[ptr]) {
				if (list[ptr] != grl) {  /* list[ptr] is group grl can connect to */
					if (grlibs[list[ptr]] == 1)iscapture = TRUE;
					else if (lnbn[sl] <= 1 && lnbf[sl][c] == 0) {
						continue;  /* single stone throwin can't cut */
						}
					else if (lnbn[sl] == 0 && lnbf[sl][c] == 1 &&
						grlibs[list[nbgrp[sl][c]]] == 2)
						continue;  /* sacrifice group can't cut */
					clibs = comlist(grlbp[grl],grlbp[list[ptr]]);
					if (clibs > 1) {
						if (grlibs[list[ptr]] > 4)
							mvvtmp -= (grlibs[list[ptr]]-4) * 16;  /* grl can connect for lots of libs */
						continue;   /* can't cut */
						}
					iscut += grlibs[list[ptr]]+islib-clibs;
					iscut += lnbn[sl] - comlist(nblbp[sl],grlbp[grl]);
					if (grlibs[list[ptr]] == 1)  /* capture */
						mvvtmp += 16*grsize[grl];
					else if (grlibs[list[ptr]] == 2 && grlibs[grl] == 2)
						mvvtmp += 16*grsize[grl];  /* double atari */
					else if (grlibs[list[ptr]] == 3 && grlibs[grl] == 2)
						mvvtmp += 16+grsize[grl]*8;  /* another atari to come */
					else if (grlibs[list[ptr]] > 5)
						mvvtmp += 20;
					else
						mvvtmp += 12*(grlibs[list[ptr]]-2);
					}
				  /* make cutting move */
				}
			if (lnbn[sl] == 0 && lnbf[sl][c] == 1 &&
				grlibs[list[nbgrp[sl][c]]] == 2 &&
				grsize[list[nbgrp[sl][c]]] == 1)
				throwinok = TRUE;
			if (grlibs[grl] == 2 && lnbf[sl][c] == 0 && lnbn[sl] == 1 && lnbn[list[nblbp[sl]]] < 4)
				throwinok = TRUE;	/* reduce liberties for cut and squeeze */
			}
		if (lnbn[sl] == 1 && lnbf[sl][c] == 0 && /* 10/97 moved out of if cut loop so will throw into two point eyes */
			(inlist(list[nblbp[sl]],&grlbp[grl]) &&
			 lnbn[list[nblbp[sl]]] == 1 || /* throwin kills a liberty */
			 grlibs[grl] == 2 && lnbn[list[nblbp[sl]]] <= 2 &&
			 lnbf[list[nblbp[sl]]][1-grcolor[grl]] == 0 ||  /* 11/02 changed grcolor != to 1-grcolor == for graded2l-73 */
			 lnbn[list[nblbp[sl]]] == 1 && lnbf[list[nblbp[sl]]][grcolor[grl]] == 0))  /* throwin makes ko */
				throwinok = TRUE;	 /* make a single stone throwin */
		/* 4/01 put second stone into a 3 point eye  or 3rd into 4 point eye */
		if (lnbn[sl] <= 1 && lnbf[sl][c] == 1 && list[nbgrp[sl][c]] != gs &&
			grlibs[list[nbgrp[sl][c]]] == 2 && grsize[list[nbgrp[sl][c]]] <= 2 &&
			comlist(grlbp[list[nbgrp[sl][c]]], grlbp[grl]) == 2)
			throwinok = TRUE;
		if (iscut + grlibs[grl] - 2 > libamt)numcuts++;  /* -2 since one cut/one conn fills two liberties */
/* 		if (islib)continue;  /* 10/96 let this code do adjacent groups - more accurate! already counted - needed to check for numcuts tho */
		
		if (lnbn[sl] == 1 && lnbf[sl][tm] == 0 &&
			grllibs < maxlibamt && grlibs[grl] > 1)
			next_to = list[nblbp[sl]];  /* play next to liberty */


			/* don't gen illegal moves - move to spot with no
			liberties surrounded by enemies and chased group
			is not captured or trying to save group 
			also don't gen moves which are liberties of group
			chased since they have already been examined*/

		if (lnbn[sl] == 0 && grlibs[grl] > 1 && 
			lnbf[sl][c] == 0 && !iscapture ||
			lnbn[sl] == 1 && grlibs[grl] > 1 &&
			lnbf[sl][c] == 0 && !iscapture && !throwinok) {
#ifdef CHECK
			if (debug == 2000+(unsigned int)g || debug == 200) {
				outerr("defatknbr not genning first illegal atk ");
				outerr(ssqr(sl,buf));
				outerr("\n");
				}
#endif
			if (lnbn[sl] == 1 && grlibs[grl] < libamt && 
				lnbn[list[nblbp[sl]]] > 1 && !inlist(list[nblbp[sl]], &grlbp[gs])) {
				mmov[mvp] = list[nblbp[sl]];
				mval[mvp++] = mvvtmp-10;	/* 5/03 attack from a distance for maeda1 prob 13 */
				}
			for (ptr = nbgrp[sl][1-c]; ptr != EOL; ptr = link[ptr])
				if (list[ptr] != grl && grlibs[list[ptr]] == 2)
					for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
						if ((sqr_t)list[ptr2] != sl && mvvtmp > 10 &&
							(lnbn[list[ptr2]] != 0 || lnbf[list[ptr2]][c]))
							{
							mmov[mvp] = list[ptr2];
							mval[mvp++] = mvvtmp;
							}
			mvvtmp = 0;
			continue;
			}

		if (lnbn[sl] <= 1 && grlibs[grl] > 1) {
			illegal_move = FALSE;
			newlibs = lnbn[sl];  /* delta in relative liberties between good and enemy groups */
			if (inlist(sl,&grlbp[grl]))newlibs++;  /* fills an enemy liberty */
			if (lnbn[sl] == 0 && inlist(sl,&grlbp[gs]))newlibs--;  /* fills own liberty */

			for (ptr = nbgrp[sl][c]; ptr != EOL; ptr = link[ptr]) {
				mrglist(grlbp[list[ptr]],&lblist);	 /* number of liberties will end up with */
				if (list[ptr] == gs || list[ptr] == grp)continue;
				if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]])) {
					adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
					}
				if (link[nbgrp[sl][c]] == EOL &&  /* no connection - only one group */
					grlibs[list[ptr]] == 2 && lnbn[sl] == 1 &&
					inlist(list[nblbp[sl]],&grlbp[list[ptr]])) { /* fills own liberty */
					illegal_move = 2;
					if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]])) {
						adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
						}
					break;
					}
				if (grlibs[list[ptr]] > 1) {
					if (grlibs[list[ptr]] > 5) {
						newlibs += 5;
						mvvtmp += 20;
						} 
					else {
						newlibs += grlibs[list[ptr]] - 1;
						mvvtmp += 4 * (grlibs[list[ptr]] - 1);
						}
					}
				}
			if (lnbf[sl][1-c] != 0 && cntlist(&lblist) + lnbn[sl] <= 2) {
				illegal_move = 2;
				}
			for (ptr = nbgrp[sl][1-c]; ptr != EOL; ptr = link[ptr]) {
				if (list[ptr] == grl)continue;
				if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]])) {
					adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
					}
				if (grlibs[list[ptr]] <= 2) {
					if (grlibs[list[ptr]] == 1 && (winsko == c || kosquare != sl))
						illegal_move = FALSE;
					if (kosquare != sl)						
						mvvtmp += 16;
					}
				}
			killist(&lblist);
			if (newlibs <= 0 && grlibs[gs] == grlibs[grl])illegal_move = TRUE;
			if (illegal_move) {
				for (ptr = nbgrp[sl][c]; ptr != EOL; ptr = link[ptr]) { /* connect for more liberties */
					for (cptr = grlbp[list[ptr]]; cptr != EOL; cptr = link[cptr]) {
						if (link[nbgrp[list[cptr]][c]] != EOL) {  /* connection here */
							mmov[mvp] = list[cptr];
							mval[mvp++] = mvvtmp-5;
							}
						}
					}
				if (lnbn[sl] == 0 && !throwinok) {
#ifdef CHECK
					if (debug == 2000+(unsigned int)g || debug == 200) {
						outerr("defatknbr not genning illegal atk ");
						outerr(ssqr(sl,buf));
						outerr("\n");
						}
#endif
					continue;
					}
				else if (illegal_move == 2 && !throwinok) {
#ifdef CHECK
					if (debug == 2000+(unsigned int)g || debug == 200) {
						outerr("defatknbr not genning bad atk ");
						outerr(ssqr(sl,buf));
						outerr("\n");
						}
#endif
					continue;
					}
				else if (!throwinok || grllibs > 2) {
#ifdef CHECK
					if (debug == 2000+(unsigned int)g || debug == 200) {
						outerr("defatknbr setting very low value on ");
						outerr(ssqr(sl,buf));
						outerr("\n");
						}
#endif
					mvvtmp = 0;
					}
				}
			}
		if (nextatk < MAXATK) {
			atksqr[nextatk] = sl;
			atkval[nextatk++] = mvvtmp;
			if (mvvtmp > mvvmax) {
				mvvmax = mvvtmp;
				mvls = sl;
				}	
			}
#ifdef NEVER
		if (libamt <= 2 && mvvtmp >= 32) {
			mval[mvp] = mvvtmp;
			mmov[mvp++] = sl;
			}
		else if (mvvtmp > mvvmax) {
			mvvmax = mvvtmp;
			mvls = sl;
			}
#endif
		}
	if (numcuts > 1 && grlibs[grl] > 3)return(savemvp);  /* no point attacking since has two ways
	                                to connect for more liberties */
	for (j = 0; j < nextatk; ++j) {
		mmov[mvp] = atksqr[j];
		if (atksqr[j] == mvls || libamt <= 2 && atkval[j] >= 64)	/* best one gets full value */
			mval[mvp] = atkval[j];
		else
			mval[mvp] = atkval[j]/2;	/* others get reduced value, so will itnerleave with best of other groups to attack */
		mvp++;
		if (atksqr[j] == next_to)
			next_to = NOSQUARE;
		}
	if (next_to != NOSQUARE) {
		mmov[mvp] = next_to;
		if (val > 16)val = 16;  /* small value on peeping move */
		mval[mvp++] = val;
		}	
#ifdef NEVER
	if (mvvmax > 0 && (libamt > 2 || mvvmax < 32)) {	/* don't double count */
		mval[mvp] = mvvmax;
		mmov[mvp++] = mvls;
		}
	else if (next_to != NOSQUARE) {
		mmov[mvp] = next_to;
		if (val > 16)val = 16;  /* small value on peeping move */
		mval[mvp++] = val;
		}
#endif
	if (passflag) {
		mval[mvp] = 10;
		mmov[mvp++] = PASS;
		}
#ifdef CHECK
	if (debug == 2000+(unsigned int)g || debug == 200) {
		if (mvp != savemvp) {
			outerr("def_atk_nbr genned move ");
			for (i = savemvp; i < mvp; ++i) {
				sprintf(buf2, "%s(%d) ", ssqr(mmov[i],buf), mval[i]);
				outerr(buf2);
				}
			}
		outerr("\n");
		}
	if (mvp > NUMGEN-1) {
		outerror("def atk nbr move list overflow\n");
		}
#endif
	return(mvp);
	}







/* jump to escape 
 *
 * two interesting cases:
 *
 *      X
 *  O + a X
 *  O + X
 *    Y
 * 1: jump to point a if no enemy stones on X
 * if there are only 2 liberties, no enemy stone on Y either
 *
 *   O +
 *   + a X
 *     X
 * 2: jump to point a if both X are empty, 
 *    or connect to a friend on an X
 *    or a is on edge and enemy group with few liberties is adjacent
 */


int jump_to_escape(group_t gs, int mvp) {
	list_t ptr, ptr2, ptr3;
	sqr_t s;
	int libs;
	list_t found = EOL;

	for (ptr = grlbp[gs]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (lnbn[s] > 1 && comlist(nblbp[s], grlbp[gs])) {	/* case 1 */
			for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2])
				if (lnbf[list[ptr2]][1-grcolor[gs]] == 0 && !inlist(list[ptr2], &grlbp[gs]) && addlist(list[ptr2], &found)) {
					mmov[mvp] = list[ptr2];
					mval[mvp] = 40;
					mvp++;		/* TODO: eliminate case when only 2 liberties, and immediate capture */
				}
		}
		for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {	/* case 2 */
			if (!inlist(list[ptr2], &grlbp[gs]) &&
				comlist(nblbp[list[ptr2]], grlbp[gs]) == 2 && addlist(list[ptr2], &found)) {
				if (lnbn[list[ptr2]] == 4) {	/* wide open into the center */
					mmov[mvp] = list[ptr2];
					mval[mvp] = 40;
					mvp++;
				}
				else if (lnbf[list[ptr2]][grcolor[gs]]) {		/* makes a connection */
					libs = 0;
					for (ptr3 = nbgrp[list[ptr2]][grcolor[gs]]; ptr3 != EOL; ptr3 = link[ptr3])
						libs += (grlibs[list[ptr3]]-1-comlist(grlbp[list[ptr3]], grlbp[gs]));
					if (libs > 0) {
						mmov[mvp] = list[ptr2];
						mval[mvp] = libs*32;
						mvp++;
					}
				}
				else if (edge[list[ptr2]] == 1 && lnbf[list[ptr2]][1-grcolor[gs]] == 1 &&
					grlibs[list[nbgrp[list[ptr2]][1-grcolor[gs]]]] <= 3) {
					mmov[mvp] = list[ptr2];
					mval[mvp] = 30;
					mvp++;
				}
			}
		}
	}
	killist(&found);
	return(mvp);	/* can't use ld[] here */
/*
	int i,s,lptr,ldtmp,sn,flag,j,ldtm2;   
      	for (lptr = grlbp[gs]; lptr != EOL;
          	lptr = link[lptr]) {
         	s = list[lptr];
         	if (ld[s] <= 2 || edge[s] <= 1 || lnbn[s] <= 1)continue;
            	i = fdir[s];
            	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
               		sn = s + nbr[i];
               		if (board[sn] != NOGROUP)continue;
                  	flag = TRUE;
                  	j = fdir[sn];
                  	for (ldtm2 = ldir[j]; j < ldtm2; ++j)
                     		if (board[sn+nbr[j]] == gs)flag = FALSE;
                  	if (flag) {
                     		mmov[mvp] = sn;
                     		mval[mvp] = 96;
                     		++mvp;
                     		}
                  	}
               	}
      	return(mvp);
*/
      	}



   /* play next to group chased to get more liberties
    * gs is group chased.  g is original group number for debug output.
    * defend group by extending in a liberty
    * get 32 points for each new liberty
    */
static int oppcolor[3] = { 1, 0, 2 };
static int play_next_to_group(group_t gs, int mvp, group_t g, int el, int maxlibs, listval_t ldrno, int winsko) {
#ifdef CHECK
	char buf2[10];
#endif
	group_t grp;
	sqr_t sn, sn2;
	sqr_t s;  /* a liberty of the group being chased */
	sqr_t l;
	int passflag,poteyes,newlibs,maxsecond,connlibs,enemy_nbr;
	int eyeflag,cancapture,isconn,val,extra,extra2;
	int min_enemy_libs,makes_eye,c,capture,capnbr;
	int eff_libs;  /* number of effective libs after move */
	list_t ptr, ptr2, ptr3, lptr, connlist = EOL, tmplist = EOL, halfconn = EOL;
	list_t enemy_list = EOL, gptr, bigeye = EOL, commlibs = EOL, extraptr = EOL;
    int friend_atari = FALSE;
	int conn_saves, connected;
	int enemycanplay;	/* enemy can make a move here */
	int canblock, i, ldtmp, count;
	int final_libs;	/* liberties of this group after this move */
	int clibs;
	int dblatarilibs;	/* liberties gained through a double atari */
	int totlibs;
	int foundcut;
	sqr_t make_eye;
	g = g;
	passflag = TRUE;
	c = grcolor[gs];
	poteyes = 0;
	for (lptr = grlbp[gs]; lptr != EOL; lptr = link[lptr]) {
		s = list[lptr];	/* s is a liberty of group being chased */
		newlibs = newlist(grlbp[gs],nblbp[s]);
		eff_libs = el + newlibs - 1;
		final_libs = grlibs[gs] + newlibs - 1;
		if (newlibs > 1)passflag = FALSE;  /* don't pass if can get more libs*/
		mval[mvp] = newlibs * 32 - 32;
		if (lnbn[s] == 1) {
			sn = list[nblbp[s]];
			if (grlibs[gs] > 2)
				mval[mvp] += 4*lnbn[sn];  /* prefer to head for more new liberties */
			canblock = lnbf[sn][1-c] || 
				lnbn[sn] == 4 || 
				grlibs[gs] == 2 && lnbn[sn] == 3;  /* 4/01 atari allows block */
			if (!canblock) {
				for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr])
					if (lnbn[list[ptr]] <= 2 &&
						lnbf[list[ptr]][c] == 0)
						canblock = TRUE;	/* hanging block */
				}
			if (!canblock)
				mval[mvp] += 4*lnbn[sn];
			}
		mmov[mvp] = s;
		if (edge[s] <= 5)mval[mvp] += edge[s]*4-2*5;  /* drive toward edge */
		else mval[mvp] += 10;
		maxsecond = 0;
		makes_eye = FALSE;
		capture = FALSE;
		conn_saves = FALSE;
		connlibs = 0;	/* number of new libs for connection */
		isconn = FALSE;
		enemy_nbr = 0;	/* most number of libs of enemy nbr */
		min_enemy_libs = 1000; /* least number of libs of enemy nbr */
		cpylist(grlbp[gs],&tmplist);
		mrglist(nblbp[s],&tmplist); /* already counted adjacent liberties */

		enemycanplay = FALSE;
		if (nbgrp[s][1-c] == EOL && lnbn[s] == 1) {	/* 11/00 for ko to play here with atari */
			sn = list[nblbp[s]];
			if (lnbn[sn] == 1 && lnbf[sn][c] == 0)
				enemycanplay = TRUE;	/* enemy can play here with ko */
		}
		dblatarilibs = 0;
		foundcut = FALSE;
		for (gptr = nbgrp[s][1-c]; gptr != EOL; gptr = link[gptr]) {
			grp = (group_t)list[gptr];  /* enemy nbr group */
			if (grlibs[grp] + lnbn[s] - 1 > 1)
				enemycanplay = TRUE;
			if (grlibs[grp] > enemy_nbr)enemy_nbr = grlibs[grp];
			if (grlibs[grp] < min_enemy_libs)
				min_enemy_libs = grlibs[grp];
			if (grlibs[grp] >= 3 &&  /* try to prevent cut */
				lnbn[s] == 0 && 
				link[nbgrp[s][c]] != EOL) {
				if (!foundcut) {  /* only count prevent cut once */
					mval[mvp] += 16;
					for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr])
						if (list[ptr] != gs && grlibs[list[ptr]] == 2)
							mval[mvp] += 16;	/* prevent the atari cut */
					foundcut = TRUE;
				}
				if (grlibs[grp] == 3)  /* fill outer liberty */
				  for (ptr = grlbp[grp]; ptr != EOL; ptr = link[ptr]) {
					if (list[ptr] == s)
						continue;
					if (lnbn[list[ptr]] == 0 && lnbf[list[ptr]][c] == 0)
						continue;
					mmov[mvp+1] = mmov[mvp];
					mval[mvp+1] = mval[mvp];
					mmov[mvp] = list[ptr];
					mval[mvp] = (lnbn[list[ptr]]+lnbf[list[ptr]][c])*8;  /* for now, just a reasonable value */
					mvp++;
					}
				}
			/* try for snapback */
			if (grlibs[grp] == 2 && link[nbgrp[s][1-c]] == EOL &&
				lnbn[s] == 1 && inlist(list[nblbp[s]], &grlbp[grp])) {
				mmov[mvp+1] = mmov[mvp];
				mval[mvp+1] = mval[mvp];
				mmov[mvp] = list[nblbp[s]];
				mval[mvp] = grsize[grp]*16;  /* for now, just a reasonable value */
				mvp++;
				}
			/* try to prevent connection in other liberty */
			if (grlibs[grp] == 2 && link[nbgrp[s][1-c]] == EOL && lnbn[s] == 0) { /* can't play here until gets more liberties */
				sn = list[grlbp[grp]];
				if (sn == s)
					sn = list[link[grlbp[grp]]];  /* he has to play at sn first */
				if (lnbf[sn][c] == 0 && lnbn[sn] == 0 && link[nbgrp[sn][1-c]] != EOL) {
					for (ptr = nbgrp[sn][1-c]; ptr != EOL; ptr = link[ptr])
						if (list[ptr] != grp && grlibs[list[ptr]] <= 4) {  /* can prevent the connection and atari */
							for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
								if (lnbf[list[ptr2]][c] || lnbn[list[ptr2]] > 1) {
									mmov[mvp+1] = mmov[mvp];
									mval[mvp+1] = mval[mvp];
									mmov[mvp] = list[ptr2];
									mval[mvp] = (lnbn[list[ptr]]+lnbf[list[ptr]][c])*8;  /* for now, just a reasonable value */
									mvp++;
									}
								}
							}
					}
				}
			if (grlibs[grp] == 1) {  /* can capture */
			/* 10/96 - let the code in defatknbr do it better */
				/* 7/98 - defatknbr doesn't always find these */
				if (!inflist(grp, &nbgrp[gs][1-c])) {
					mval[mvp] += grsize[grp] * 32 - 16;  /* bias toward capture */
					for (ptr = grnbp[grp]; ptr != EOL; ptr = link[ptr])
						if (list[ptr] != gs) {

							andlist(grlbp[gs], grlbp[list[ptr]], &commlibs);
							connected = FALSE;
							for (ptr2 = commlibs; ptr2 != EOL; ptr2 = link[ptr2]) {
								if (solidconnect(list[ptr2], c, winsko) == 1) {
									connected = TRUE;
									break;
									}
								}
							killist(&commlibs);
							if (!connected)  /* don't count liberties I already have connected to */
								mval[mvp] += (grlibs[list[ptr]] -  /* connects */
									comlist(grlbp[gs],grlbp[list[ptr]]))*32;
							}
					}
				if (link[grnbp[grp]] == EOL && list[grnbp[grp]] == gs &&
					(lnbn[s] > 1 || link[nbgrp[s][1-c]] != EOL))
					mval[mvp] += 32;  /* make an eye */
				capture = TRUE;
				eff_libs += grsize[grp];
				final_libs += grsize[grp];
				passflag = FALSE;  /* don't pass if can capture */
				}
			else if (grlibs[grp] <= grlibs[gs] + newlibs-1) {
#ifdef NEVER
				mval[mvp] += grsize[grp]*16;  /* attack group */
				if (lnbn[s] > 1 && grlibs[grp] == 2) /* double counted below */
					mval[mvp] += (lnbn[s]-1)*32;  /* count again since sente */
#endif
				for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {  /* prevent capture to get away */
					if (grlibs[list[ptr]] == 1)
						mval[mvp] += 16;
					}
				if (grlibs[grp] == 2) {
					capture = TRUE; /* can attack group */
					capnbr = FALSE;
					l = list[grlbp[grp]];
					if (l == s)l = list[link[grlbp[grp]]];
					if ((lnbn[l] <= 1 || lnbn[l] == 2 && inlist(s,&nblbp[l])) && 
						lnbf[l][1-c] == 1) {  /* can't get away by playing in other liberty */
						capnbr = TRUE;
						totlibs = 0;
						for (ptr = grnbp[grp]; ptr != EOL; ptr = link[ptr]) {
							if (grlibs[list[ptr]] == 1 && list[grlbp[list[ptr]]] != s) {
								capnbr = FALSE;	/* he can get away with a capture */
								}
							else
								totlibs += grlibs[list[ptr]];
							}
						if (capnbr) {
							mval[mvp] += (grsize[grp]-1) * 24 +  /* capture */
								totlibs * 32;  /* get at least 1 lib per neighbor */
							}
						}
					if (!capnbr) {  /* can atari with sente */
						extra = -1;  /* lose one liberty for second move */
						cpylist(grlbp[gs], &extraptr);
						for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
							if (!inlist(list[ptr],&grlbp[gs])) {
								extra += mrglist(nblbp[list[ptr]], &extraptr);
								for (ptr2 = nbgrp[list[ptr]][c]; ptr2 != EOL; ptr2 = link[ptr2])
									extra += mrglist(grlbp[list[ptr2]], &extraptr)-1;
						/*		extra += lnbn[list[ptr]]+lnbf[list[ptr]][c]-1;  8/99 old code here */
								}
							}
						killist(&extraptr);
							/* can use sente to get these liberties or connections */
						extra2 = grsize[grp]-1;  /* liberties if he gives up group */
						cpylist(grlbp[gs], &extraptr);
						extra2 += mrglist(nblbp[l], &extraptr);
						for (ptr = grnbp[grp]; ptr != EOL; ptr = link[ptr])
							if (list[ptr] != gs)
								extra2 += mrglist(grlbp[list[ptr]], &extraptr);
						killist(&extraptr);
						if (dblatarilibs == 0)
							dblatarilibs = extra2;
						else {
							if (extra2 < dblatarilibs)
								dblatarilibs = extra2;
							mval[mvp] += 24*dblatarilibs;	/* have a double atari here, so take the least number of new liberties */
						}
						if (extra > extra2)
							extra = extra2;  /* since he could give up group */
						if (extra > 0)
							mval[mvp] += 24*extra;  /* almost full value for these liberties, since this is pretty accurate 8/99 - was 16* */
						if (lnbf[l][c] == 0 && lnbn[l] <= 1)
							mval[mvp] += 24;	/* force him to fill an eye */
						}
					}
				}
			else if (grlibs[grp] == 3 && newlibs == 1 && grlibs[gs] == 2 && comlist(grlbp[gs], grlbp[grp]) > 1) {
				mval[mvp] += 16;  /* try for seki */
				}
			else if (grlibs[grp] != 1 && grlibs[grp] <= grlibs[gs] + newlibs &&
				comlist(grlbp[grp],grlbp[gs]) >= grlibs[grp]-1) {
				capture = TRUE;
				mval[mvp] += 16;  /* try for seki or liberty shortage */
				}
			if (grlibs[grp] < 4 && grlibs[grp] != 1)mval[mvp] += 10 - 3*grlibs[grp];
			} /* end of looking at adjacent enemy groups */

		make_eye = NOSQUARE;
		for (gptr = nbgrp[s][c]; gptr != EOL; gptr = link[gptr]) {
			grp = (group_t)list[gptr]; /* friendly nbr group */
			if (link[nbgrp[s][c]] != EOL && 
				/* lnbf[s][1-c] != 0 && 11/00 for ko */ enemycanplay && 
				grlibs[grp] == 2) /* see if protects eye in other liberty */
			{
				sn = list[grlbp[grp]];
				if (sn == s)
					sn = list[link[grlbp[grp]]];
				if (make_eye != sn) {	/* don't double count eye */
					make_eye = sn;
					if (lnbf[sn][1-c] == 0 && lnbn[sn] == 0) {
						mval[mvp] += 32;  /* prevent atari to make eye */
					}
				}
			}
			if (grp == gs)continue;
			if (link[nbgrp[s][c]] != EOL &&
				lnbf[s][1-c] == 0 &&
				lnbn[s] == 1 &&
				lnbf[list[nblbp[s]]][1-c] == 0 &&
				grlibs[grp] == 2)  /* defend adjacent eye from throw in */
			{
				if (inlist(list[nblbp[s]], &grlbp[grp]))
					mval[mvp] += 32;  /* throwin not so bad */
				else
					mval[mvp] += 64;  /* can throwin and atari from outside to kill the eye. 32 for eye, 32 for filling own liberty */
			}
			isconn = TRUE;
			if (lnbf[s][1-c] != 0 &&
				comlist(grlbp[gs],grlbp[grp]) == 2 &&
				andlist(grnbp[gs],grnbp[grp],&enemy_list)) {
				for (ptr = enemy_list; ptr != EOL; ptr = link[ptr])
					if (grlibs[list[ptr]] <= 2 && !inlist(s,&grlbp[list[ptr]]) &&
						comlist(grlbp[list[ptr]],grlbp[gs]))
						mval[mvp] += 64;  /* try for seki */
				killist(&enemy_list);
				}
			if (grlibs[grp] == 1) {
				conn_saves = TRUE;
				friend_atari = TRUE;
				mval[mvp] += 32;  /* connect costs a liberty, but still may be good */
				if (winsko == c && grsize[grp] == 1 && lnbf[list[grlbp[grp]]][1-grcolor[grp]] == 0 &&
					lnbn[list[grlbp[grp]]] == 1) {	/* make a ko */
					mmov[mvp+1] = mmov[mvp];
					mval[mvp+1] = mval[mvp];
					mmov[mvp++] = list[nblbp[list[grlbp[grp]]]];
					}
				}
			if (grlibs[gs] == 2 && grlibs[grp] == 2 &&
				(lnbn[s] > 1 || lnbf[s][1-c] && lnbn[s] > 0))
				mval[mvp] += 48;   /* prevent a double atari */
		/*	if (grlibs[grp] == 1)passflag = FALSE;  should try to connect, not pass */
			if (lnbn[s] == 1 && lnbf[s][1-c] == 0 &&  /* snapback */
				grlibs[gs] == 2 && lnbn[list[nblbp[s]]] == 1 &&
				inlist(list[nblbp[s]],&grlbp[gs]) &&
				link[nbgrp[list[nblbp[s]]][c]] == EOL) {  /* other point not a connection too */
				clibs = mrglist(grlbp[grp],&tmplist);
				final_libs += clibs;
				connlibs += clibs;
				}
			else if (solidconnect(s,c,winsko) == TRUE) {  /* I win ko, or truely solid - these liberties are already counted */
				/* if (lnbf[s][1-c] == 0 && lnbn[s] <= 1 || 
				lnbf[s][1-c] == 1 && lnbn[s] == 0 && 
				grlibs[list[nbgrp[s][1-c]]] <= 2) */
				addlist(grp,&connlist);  /* already solidly connected */
				clibs = mrglist(grlbp[grp], &tmplist);	
				final_libs += clibs;
				if (solidconnect(s,c,oppcolor[winsko]) == 2)  /* full credit for connecting a ko */
					connlibs += clibs;
				else
					connlibs += clibs/2;	/* 3/02 still give some credit for connecting here */
				}
			else if (grlibs[gs] == 2 && grlibs[grp] == 2 && cntlist(&nbgrp[s][c]) == 2) {  /* 5/01 shortage of liberties */
				clibs = mrglist(grlbp[grp],&tmplist);  
				final_libs += clibs;
				connlibs += clibs+1; /* since actual liberties are short by one */
				mval[mvp+1] = mval[mvp];
				mmov[mvp+1] = mmov[mvp];
				mval[mvp] = 5;
				sn = list[grlbp[grp]];
				if (sn == s)
					sn = list[link[grlbp[grp]]];
				mmov[mvp] = sn;
				mvp++;
				}
			else if (inlist(grp,&halfconn)) {
				addlist(grp,&connlist);  /* already solidly connected */
				clibs = mrglist(grlbp[grp],&tmplist); /* must be same as next "else" */
				final_libs += clibs;
				connlibs += clibs;
				}
			else {
				addlist(grp,&halfconn);  /* one connection point */
				clibs = mrglist(grlbp[grp],&tmplist);
				final_libs += clibs;
				connlibs += clibs;
				}
			}   /* end of looking at adjacent friendly groups */
			
		for (gptr = nblbp[s]; gptr != EOL; gptr = link[gptr]) {
			sn = list[gptr];
			if (lnbn[sn] == 1 && lnbf[sn][1-c] == 0) {
				if (lnbn[s] > 1 || lnbf[s][1-c] != 0)
					mval[mvp] += 64; /* makes a potential full eye at sn */
				else
					mval[mvp] += 32;  /* already a potential 2 pt eye */
				makes_eye = TRUE;
				}
			else if ((lnbn[s] > 1 || lnbf[s][1-c] != 0) &&
				lnbn[sn] == 2 && lnbf[sn][1-c] == 0) {
				for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr])
					if ((sqr_t)list[ptr] != s && lnbn[list[ptr]] <= 2 && 
						lnbf[list[ptr]][1-c] == 0) {
						if (lnbf[list[ptr]][1-c] == 0) {
							if (lnbn[list[ptr]] == 1)
								mval[mvp] += 48; /* makes a potential 2 pt eye */
							else mval[mvp] += 16;  /* makes potential longer eye */
							if (edge[sn] != 0) /* don't expand in corner, play on 1-2 point */
								makes_eye = TRUE;  /* can't run from interior liberties */
							}
						else if (lnbf[list[ptr]][1-c] == 1 &&
							lnbf[list[ptr]][c] != 0) {
							makes_eye = TRUE;  /* makes potential half eye */
							mval[mvp] += 16;
							}
						}
					}
			else if (lnbn[sn] > 2 && !inlist(sn,&grlbp[gs])) {
				val = newlist(grlbp[gs],nblbp[sn])-1;
				if (val > 0) {
					if (lnbf[sn][1-c] == 0)
						mval[mvp] += 8;  /* since he has to block without friendly support */
					if (val*16 > maxsecond)
						maxsecond = val*16;
					mval[mvp] += val * 16;
					}
				}
			if (!inlist(sn,&grlbp[gs])) {
				for (ptr = nbgrp[sn][c]; ptr != EOL; ptr = link[ptr])
					if (grlibs[list[ptr]] >= 3 && newlibs >= 1 && 
						grlibs[gs] + newlibs - 1 > 2)  /* 4/01 don't push thru into atari */
						mval[mvp] += (newlibs-1) * 16 + 8*(grlibs[list[ptr]]-2);  /* sente threat to connect */
				}
				if (!lnbf[sn][1-c])
					for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr]) {
						if (!inlist(list[ptr], &grlbp[gs]) && lnbf[list[ptr]][c]) {   /* threaten long connection */
							mval[mvp] += (4-lnbn[sn])*4;
							if (!lnbf[list[ptr]][1-c])
								mval[mvp] += 4;
							}
					}
			}
		killist(&tmplist);
		eff_libs += connlibs;
		if (connlibs >= 2)passflag = FALSE;
		if (newlibs == 0 && enemy_nbr <= grlibs[gs])poteyes++;

		if (enemy_nbr && connlibs > 0 ||
			connlibs > 1)mval[mvp] += 32 * connlibs + 12;
		else if (enemy_nbr && halfconn != EOL && connlibs != 0)
			mval[mvp] += 24;  /* partial symettry since added some for other half of connection */
		if (!capture && connlibs == 1 && enemy_nbr <= grlibs[gs])mval[mvp] += 40;
			/* attack nbr and save group */
		else if (!capture && min_enemy_libs < eff_libs)
			mval[mvp] += 32;  /* attacking nbr group */
		if (connlibs > 2 && enemy_nbr)passflag = FALSE;
		mval[mvp] -= maxsecond;
		if (lnbn[s] == 0 && lnbf[s][1-c] == 0 && 
			!capture && !friend_atari)mval[mvp] -= 32;
		                 /* fills potential eye */
		if (final_libs > 1 && /* 3/02 don't make self-atari move */
			(connlibs + newlibs >= 1 || capture || lnbn[s] >= 2 || makes_eye ||
			passflag && isconn || conn_saves)) {  /* try for seki */
			++mvp; /* generate the move */
			}
#ifdef CHECK
		else {
			if (debug == 2000+(unsigned int)g) {
				outerr("play_next not genning ");
				outerr(ssqr(mmov[mvp],buf2));
				outerr(" no new libs\n");
				}
			}
#endif
		/* capture one liberty neighbor of one liberty nearby friendly group */
		if (lnbf[s][1-c] == 0 && lnbn[s] <= 1 ||
			lnbf[s][1-c] == 1 && grlibs[list[nbgrp[s][1-c]]] <= 2 && lnbn[s] <= 1) { /* see if can make hanging connection */
			for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
				if (list[ptr] == gs)
					continue;
				if (grlibs[list[ptr]] == 1)
					for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
						if (grlibs[list[ptr2]] == 1) {
							mmov[mvp] = list[grlbp[list[ptr2]]];
							mval[mvp] = 32*grsize[list[ptr2]] + 16;
							for (ptr3 = grnbp[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3])
								if (list[ptr3] != gs && list[ptr3] != list[ptr])
									mval[mvp] += grlibs[list[ptr3]]*32;
							mvp++;
							}
				}
			}

		if (lnbf[s][1-c] == 0 && lnbn[s] <= 2 ||
			edge[s] == 2 && lnbf[s][1-c] == 0 && grlibs[gs] > 2 ||	/* 2/01 can hane up or jump from second line */
			lnbf[s][1-c] == 1 && grlibs[list[nbgrp[s][1-c]]] <= 2 && lnbn[s] == 1) { /* see if can make hanging connection */
			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
				sn = list[ptr];
				if (!inlist(sn,&grlbp[gs])) {
					for (gptr = nbgrp[sn][1-c]; gptr != EOL; gptr = link[gptr]) {
						if (grlibs[list[gptr]] == 1) {
							mval[mvp] = grsize[list[gptr]]*32+16;
							mmov[mvp++] = sn;
							break;
							}
						}
					if (lnbf[sn][c] != 0) {
						for (gptr = nbgrp[sn][c]; gptr != EOL; gptr = link[gptr]) {
							if (!inlist(list[gptr],&connlist)) { /* new length 2 connection */
								mval[mvp] = newlist(grlbp[gs],grlbp[list[gptr]]) * 32 - 32;
								mmov[mvp++] = sn;
								}
							}
						}
					else if (lnbn[sn] >= 2) {   /* just shape hane */
						mval[mvp] = newlist(nblbp[sn],grlbp[gs]) * 32;
						mmov[mvp++] = sn;
						}
					else if ((edge[sn] > 1 || edge[sn] == 1 && edge[s] == 1) && board[sn+sn-s] == NOGROUP &&
						lnbf[sn+sn-s][c] != 0) { /* 10/17/97 hane to make length 3 connection */
						for (gptr = nbgrp[sn+sn-s][c]; gptr != EOL; gptr = link[gptr]) {
							if (!inlist(list[gptr],&connlist)) { /* new connection */
								mval[mvp] = newlist(grlbp[gs],grlbp[list[gptr]]) * 32 - 32;
								if (lnbf[sn+sn-s][1-c])
									mval[mvp] /= 2;
								mmov[mvp++] = sn;

								}
							}
						}
					}
				}
			}

		if (makes_eye && lnbf[s][1-c] == 0 && lnbn[s] == 2) {  /* make bigger eye */
			sn = list[nblbp[s]];
			if (lnbf[sn][1-c] == 0)
				sn = list[link[nblbp[s]]];
			if (!inlist(sn,&grlbp[gs]) && !inlist(sn,&bigeye) &&
				(lnbf[sn][c] != 0 && lnbf[sn][1-c] != 0 || 
				lnbn[sn] > 2)) {
				mval[mvp] = 64;
				mmov[mvp++] = sn;
				addlist(sn,&bigeye);
				}
			}
                                                          
		if (lnbn[s] == 1 &&   /* make eye */
			!enemy_nbr && grlibs[gs] > 1 && 
				!inlist(list[nblbp[s]],&grlbp[gs])) {	/* don't double count */
			sn = list[nblbp[s]];
			eyeflag = TRUE;
			if (lnbf[sn][1-c] == 3)eyeflag = FALSE;
			if (edge[sn] == 1) {
				if (lnbf[sn][1-c] == 2)
					eyeflag = FALSE;
				if (lnbf[sn][1-c] == 1 && edge[s] == 1 &&
					board[sn+sn-s] == NOGROUP)
					eyeflag = FALSE;  /* 6/99 eye on edge is false */
				}
			if (edge[sn] == 0 && lnbf[sn][1-c] == 1 &&	/* 11/00 eye in corner is false */
				grlibs[list[nbgrp[sn][1-c]]] > 1)
				eyeflag = FALSE;
			cancapture = FALSE;
			for (gptr = nbgrp[sn][1-c]; gptr != EOL; gptr = link[gptr])
				if (grlibs[list[gptr]] == 1)
					cancapture = TRUE;
			for (gptr = nbgrp[sn][c]; gptr != EOL; gptr = link[gptr])
				if (grlibs[list[gptr]] == 2 &&
					lnbn[sn] == 1 && lnbf[sn][c] == 1)eyeflag = FALSE;
			if (eyeflag) {	/* 3/02 check the diagonals of s.  if it is false, no eye */
				count = 0;
				if (edge[s] <= 1)
					count = 1;
				i = fdir[s];
				for (ldtmp = ldiag[i]; i < ldtmp; ++i) {
					sn2 = s + diags[i];
					if (S_COLOR(sn2) == 1-c && S_NUMLIBS(sn2) >= 2)
						count++;
				}
				if (count >= 2)
					eyeflag = FALSE;
			}

			if ((cancapture || eyeflag) && (lnbf[sn][1-c] != 0 || lnbn[sn] > 1)) {
				mmov[mvp] = sn;
				if (edge[s] == 1 && edge[sn] == 1 && lnbf[sn][c] == 0)
					mval[mvp++] = 20;  /* 6/99 don't make funny looking eyes on edge */
				else
					mval[mvp++] = 64;
				if (lnbf[sn][1-c] == 0 && lnbn[sn] == 3 && edge[sn] == 1 && edge[s] == 1) {  /* bigger eye on edge */
					for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr]) {
						if (edge[list[ptr]] == 3) {
							mmov[mvp] = list[ptr];
							mval[mvp++] = 64;
							}
						}
					}
				if (lnbf[sn][1-c] == 0 && lnbn[sn] == 2) {  /* make bigger eye */
					if ((sqr_t)list[nblbp[sn]] != s)sn = list[nblbp[sn]];
					else sn = list[link[nblbp[sn]]];
					if (!inlist(sn,&grlbp[gs])) {  /* don't double count */
						mmov[mvp] = sn;
						mval[mvp++] = 64;
						}
					}
				}
			}
		if (edge[s] == 1 && lnbn[s] == 2 && grlibs[gs] > 2) { /* hane on edge */
			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
				if (lnbn[list[ptr]] == 2 && lnbf[list[ptr]][1-c] == 1) {
					if (edge[list[ptr]] != 1)continue;
					mmov[mvp] = list[ptr];
#ifdef CHECK		
					if (board[mmov[mvp]] != NOGROUP)
						outerror("hane bad move");
#endif		
					mval[mvp] = 32;
					for (gptr = nblbp[list[ptr]]; gptr != EOL; gptr = link[gptr]) {
						if ((sqr_t)list[gptr] == s)continue;
						if (lnbf[list[gptr]][c])
							mval[mvp] += 32;  /* likely connection on edge */
						}
					mvp++;
					passflag = FALSE;
					}
				}
			}
		}   /* end of loop looking at each liberty */
	killist(&bigeye);

	/* 4/00 expand connlist */
	if (eff_libs > 1)	/* 1/05 for 1 or 2 lib groups that can't escape */
	  for (ptr = connlist; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[list[ptr]] + grlibs[gs] - 1 > maxlibs)
			continue;	// no need to expand this further
		for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (lnbn[list[ptr2]] > 1 || lnbf[list[ptr2]][1-c] != 0)
				continue;
			if (link[nbgrp[list[ptr2]][c]] == EOL)
				continue;
			if (inlist(list[ptr2], &grlbp[gs]))
				continue;
			if (solidconnect(list[ptr2], c, NOCOLOR) == TRUE)
				for (ptr3 = nbgrp[list[ptr2]][c]; ptr3 != EOL; ptr3 = link[ptr3])
					if (grlibs[list[ptr3]] + grlibs[list[ptr]] + grlibs[gs] - 2 <= maxlibs &&  // need to connect twice to here...
															// don't expand further if this is enough
						list[ptr3] != list[ptr])
						addlist(list[ptr3], &tmplist);
		}
	}
	if (tmplist != EOL) {
		mrglist(tmplist, &connlist);
		killist(&tmplist);
	}
	  /* play in liberty of group connected to or capture nbr */
	for (ptr = connlist; ptr != EOL; ptr = link[ptr]) {
      	for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
	      	if (inlist(list[ptr2],&grlbp[gs])) {
				if (grlibs[gs] == 2 && grlibs[list[ptr]] == 2 &&
					comlist(grlbp[gs], grlbp[list[ptr]]) == 1) {
					mval[mvp] = 16;			/* 10/96 hope it doesn't break seki's! */
					mmov[mvp++] = list[ptr2];  /* have to connect since short of liberties */
					}
				continue;
				}
			if (lnbn[list[ptr2]] == 0 && lnbf[list[ptr2]][1-c] == 0 && grlibs[list[ptr]] > 1)
				poteyes++;
	      	if (lnbn[list[ptr2]] == 0 && lnbf[list[ptr2]][c] == 1)continue;
	      	if (lnbn[list[ptr2]] == 1 && lnbf[list[nblbp[list[ptr2]]]][1-c] == 0 &&
	      		lnbn[list[nblbp[list[ptr2]]]] <= 1)
	      		mval[mvp] = 18;  /* make potential eye */
	      	if (lnbn[list[ptr2]] <= 1 && lnbf[list[ptr2]][1-c] == 0)
				mval[mvp] = 16;  /* opp can't fill this lib without approaching */
	      	else {
		      	mval[mvp] = 19 + 24*(lnbn[list[ptr2]]-
		      		comlist(nblbp[list[ptr2]],grlbp[list[ptr]])-
		      		comlist(nblbp[list[ptr2]],grlbp[gs])-1);
		      	if (mval[mvp] < 19)
		      		mval[mvp] = 19;
		      	}
		  	for (ptr3 = nbgrp[list[ptr2]][c]; ptr3 != EOL; ptr3 = link[ptr3])
		  		if (!inlist(list[ptr3],&connlist) && 
		  			(group_t)list[ptr3] != gs && 
		  			grlibs[list[ptr3]] > 2) {
		  			if (lnbn[list[ptr2]] == 0 && lnbf[list[ptr2]][1-c] == 0)
		  				mval[mvp] = 8;  /* don't fill eye */
		  			else {
						newlibs = grlibs[list[ptr3]]-comlist(grlbp[list[ptr3]], list[ptr])-1;
						if (newlibs  > 5)
							newlibs = 5;	/* limit total liberties to 5 */
			  			mval[mvp] += newlibs * 24;  /* connects for some more liberties */ 
			  			}
		  			}
	      	mmov[mvp++] = list[ptr2];                                      
	      	}
		for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr2]]]])) {
				adflist(mvs[grpieces[list[ptr2]]],&grldr[ldrno]);
				}
			if (grlibs[list[ptr2]] == 1) {
				mval[mvp] = 32 * grsize[list[ptr2]];  /* capture enemy group */
				mmov[mvp++] = list[grlbp[list[ptr2]]];
				}
			else if (grlibs[list[ptr2]] <= grlibs[gs] && 
				grlibs[list[ptr2]] <= grlibs[list[ptr]]) {   /* attack enemy group */
				for (ptr3 = grlbp[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3])
					if (!inlist(list[ptr3],&grlbp[gs]) && 
						!inlist(list[ptr3],&grlbp[list[ptr]])) {
						mval[mvp] = 8 * lnbn[list[ptr3]];
						mmov[mvp++] = list[ptr3];
						}
				for (ptr3 = grnbp[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3])
					if (grlibs[list[ptr3]] == 1) { /* save friendly to attack enemy */
						mval[mvp] = 24;
						mmov[mvp++] = list[grlbp[list[ptr3]]];
						}
				}
			}
  		}
   	killist(&connlist);
	if (passflag  && poteyes > 1) {
		mmov[mvp] = PASS;
		mval[mvp++] = 10;
		}
	killist(&halfconn);
#ifdef CHECK
	if (mvp > NUMGEN-1) {
		outerror("play_next move list overflow\n");
		}
#endif
	return(mvp);
	}


/* figure the number of effective liberties of group g
 * this is the number of moves it takes to capture g if
 * g's color passes every time or responds once.  Assume g's enemy plays
 * first.  Start with g's liberties. 
 * If g has two or more points where approach moves are
 * required, g's liberties are increased.  If g has two or more places where
 * he can connect for more liberties or extend for more liberties, g's 
 * liberties are increased.  If g has protected connection and more than 2 liberties,
 * libs are inced
 * If g has enemy neighbors in atari whose liberty
 * is not shared with g, g's liberties are increased.
 * can stop when get to n+1 libs
 * if g has two eyes, return 10.
 * a single 4 space eye is worth 5 liberties
 * if a liberty of g is shared with a 2 liberty group, g's liberties are reduced.
 * protected liberty can be a ko throwin
 *
 * return a conservative (on the low side) numbe rof liberties.
 * maxlibs returns on optimistic value (on the high side)
 */

int getefflibs(group_t g, int n, listval_t ldrno, int *maxlibs) {
	sqr_t s, sn;
	int el,c,max,l1,l2,protconn = 0,sumlibs,com,numeyepoints;
	int friendly_capture,must_defend,defended,deflibs,undeflibs;
	int newlibs,bestdefended,numeyes,undefended[2],numdefended,capcount;
	list_t connliblist = EOL, gptr, ptr, ptr2;
	int numrealeyes = 0,num2geyes = 0, cap;
	int haveatari;
	int atarilibs = 0;	/* number of liberties where opponent can atari adjacent group */
	int kothrowin = FALSE;
	group_t fgroup1, fgroup2;
	if (grlibs[g] == 1) {
		*maxlibs = 1;
		return(1);
		}
	if (grlibs[g] > n) {
		*maxlibs = grlibs[g];
		return(grlibs[g]);
		}
	c = grcolor[g];
	if (grlibs[g] == 2) {
		l1 = list[grlbp[g]];
		l2 = list[link[grlbp[g]]];
		if ((lnbn[l1] > 1 || lnbf[l1][1-c] != 0) &&
			(lnbn[l2] > 1 || lnbf[l2][1-c] != 0)) {
			*maxlibs = 2;
			return(2);
			}
		}
	numeyes = 0;
	numeyepoints = 0;  /* no enemy and no new liberties here */
	numdefended = 0;
	bestdefended = 0;
	deflibs = 0;
	undefended[0] = undefended[1] = 0;  /* best and second best undefended connections liberties gained */
	undeflibs = 0;
	fgroup2 = NOGROUP;
	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		defended = FALSE;
		friendly_capture = FALSE;
		fgroup1 = NOGROUP;

		/* figure out how many new liberties get by playing here */
		if (link[nbgrp[s][c]] == EOL) {  /* simple case - no friendly connection - just extend */
			newlibs = lnbn[s] - comlist(nblbp[s],grlbp[g]) - 1;
			}
		else {  /* check for connection, friendly capture */
			
			newlibs = lnbn[s] - 1;
			cpylist(nblbp[s],&connliblist);
			capcount = 0;
			haveatari = FALSE;
			for (gptr = nbgrp[s][c]; gptr != EOL; gptr = link[gptr]) {
				if ((group_t)list[gptr] == g)continue;
				if (grlibs[list[gptr]] == 1) { /* find friendly capture */
					capcount++;  /* kosquare != s */
					if (grsize[list[gptr]] == 1 || lnbn[s] > 0 ||
						lnbf[s][1-c] != 0)
						friendly_capture = TRUE;
					}
					/* Two kinds of snapbacks:
					 *
					 *   @ @ @ @
					 *   @ O O O @
					 *   O + + O @
					 *   @ O O O @
					 *   @ @ @ @ @
					 *
					 *   @ @ @ @
					 *   @ O O O @
					 *   O + + O @
					 *   O O @ @ @
					 *   @ @ @ 
					 *  
					 */
				else if (lnbf[s][1-c] == 0 && /* snapback*/
					lnbn[s] == 1 &&
					lnbn[list[nblbp[s]]] == 1 &&
					grlibs[list[gptr]] == 2 && 
					inlist(list[nblbp[s]],&grlbp[list[gptr]]) &&
					link[nbgrp[list[nblbp[s]]][c]] == EOL) {
					friendly_capture = TRUE;
					}
				else if (grlibs[list[gptr]] == 2 &&
					(lnbn[s] > 1 || lnbf[s][1-c] != 0))
					haveatari = TRUE;
				}
			if (haveatari)
				atarilibs++;
			if (capcount > 1)
				friendly_capture = TRUE;
			for (gptr = nbgrp[s][c]; gptr != EOL; gptr = link[gptr]) {
				if ((group_t)list[gptr] == g)continue;
				if (!friendly_capture && lnbf[s][1-c] == 0 && /* solid connection */
					(lnbn[s] == 0 ||  /* safe */
					 lnbn[s] == 1 && 
					 	(lnbn[list[nblbp[s]]] >= 2 || /* not snapback for g */
					 	 grlibs[g] > 2 || 
					 	 !inlist(list[nblbp[s]],&grlbp[g]) &&
					 	 link[nbgrp[list[nblbp[s]]][c]] != EOL))) {
					com = comlist(grlbp[list[gptr]],grlbp[g]);
					if (lnbn[s] == 1 && lnbf[list[nblbp[s]]][c] != 0)
					 	com++;  /* potential throwin */
					if (grlibs[list[gptr]] - com > protconn)
						protconn = grlibs[list[gptr]] - com;
					}
				if (fgroup1 == NOGROUP)fgroup1 = (group_t)list[gptr];
				else if (list[gptr] != fgroup1)fgroup1 = NOGROUP+1;
				newlibs += mrglist(grlbp[list[gptr]],
						     &connliblist);

				if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[gptr]]]])) {
					adflist(mvs[grpieces[list[gptr]]],&grldr[ldrno]);
					}
				}
			if (newlibs > 0)newlibs -= comlist(grlbp[g],connliblist);
			killist(&connliblist);
			}

		/* have newlibs value */
        if (newlibs <= 0 && lnbf[s][1-c] == 0 && !friendly_capture)
        	++numeyepoints;
        
		if (lnbf[s][1-c] == 0 && lnbn[s] == 0 && !friendly_capture) {
			defended = TRUE;  /* need aproach move here - eye or false eye */
			if (fgroup1 == NOGROUP)++numrealeyes;
			else if (fgroup1 != NOGROUP+1) {
				if (fgroup1 == fgroup2 || fgroup2 == NOGROUP) {
					++num2geyes;
					fgroup2 = fgroup1;
					}
				}
			}


		else if (lnbn[s] == 1 && lnbf[s][1-c] == 0 && !friendly_capture &&
			!inlist(list[nblbp[s]],&grlbp[g])) {
			defended = TRUE; /* need approach, tigers mouth */
			sn = list[nblbp[s]];
			if (lnbn[sn] == 1 && lnbf[sn][c] == 0)
				kothrowin = TRUE;	/* 5/02 can make a ko */
			}

		else if (lnbn[s] == 0 && !friendly_capture) {  /* we know already have enemy stone adjacent */
			                     /* defended due to shortage of libs */
			sumlibs = 0;  /* max possible remaining liberties after enemy moves here */
						/* conservative since enemies may share other libs */
			for (gptr = nbgrp[s][1-c]; gptr != EOL; gptr = link[gptr]) {
				if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[gptr]]]])) {
					adflist(mvs[grpieces[list[gptr]]],&grldr[ldrno]);
					}
				sumlibs += grlibs[list[gptr]]-1;
				}
			defended = sumlibs <= 1;
			}

		if (lnbn[s] == 0 && lnbf[s][1-c] == 0 && !friendly_capture)
			numeyes++;
		if (defended && newlibs > bestdefended) {
			bestdefended = newlibs;
			deflibs = grlibs[g]+bestdefended-1;
			if (deflibs > n && grlibs[g] > 2) {
				*maxlibs = deflibs;
				return(deflibs);
				}
			}
		if (!defended && newlibs > 0) {
			if (newlibs > undefended[0]) {
				undefended[1] = undefended[0];
				undefended[0] = newlibs;
				}
			else if (newlibs > undefended[1])
				undefended[1] = newlibs;
			undeflibs = grlibs[g]+undefended[1]-1-atarilibs;
			if (undeflibs > n) {
				*maxlibs = undeflibs;
				return(undeflibs);
				}
			}
		if (defended)++numdefended;
		}

	if (numrealeyes >= 2 || num2geyes >= 2) {
		*maxlibs = 10;
		return(10);
		}
	if (numeyepoints == 4) {
		*maxlibs = 5;
		return 5;  /* big eye */
		}


	/* look for single lib neighbors he must defend */
	must_defend = FALSE;
	for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[list[ptr]] == 1) {  /* can capture */
			if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]])) {
				adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
				}
			newlibs = grsize[list[ptr]];
			if (inlist(list[grlbp[list[ptr]]],&grlbp[g]))
				--newlibs;
			else {
				if (newlibs > 1)
					must_defend = TRUE;
			}
			if (must_defend) {
				if (newlibs > undefended[1])
					undeflibs = grlibs[g]+newlibs-1-atarilibs;
				else
					undeflibs = grlibs[g]+undefended[1]-1-atarilibs;
				if (undeflibs > 2) {
					cap = FALSE;
					for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr2]]]])) {
							adflist(mvs[grpieces[list[ptr2]]],&grldr[ldrno]);
							}
						if (grlibs[list[ptr2]] == 1) {
							cap = TRUE;
							if (inlist(list[grlbp[list[ptr2]]], &grlbp[g])) {
								must_defend = FALSE;	/* defend fills a liberty so no savings */
								}
							break;
							}
						}
					if (!cap && undeflibs > n) {
						*maxlibs = undeflibs;
						return(undeflibs);
						}
					}
				if (must_defend) {
					if (newlibs > undefended[0]) {
						undefended[1] = undefended[0];
						undefended[0] = newlibs;
						}
					else if (newlibs > undefended[1])
						undefended[1] = newlibs;
					}
				}
			}
		}

	el = grlibs[g] + must_defend - atarilibs;  /* option 1, let him fill all liberties */
	if (numdefended > 1) {
		el += numdefended-1;
		if (kothrowin)
			el--;
		}
	if (undefended[1] > 1)
		el += undefended[1]-1;	/* can connect to friend no matter what */
	/* 12/31/98 added atarilibs term for graded1b prob 25 to see double atari */
	max = el;
	if (numdefended > 1 && kothrowin)
		max++;
	if (protconn > 2) {
		max += protconn - 2;
		if (atarilibs == 0 || grlibs[g] >= 4)el += protconn - 2;  /* answer once to connect */
	}

    *maxlibs = max + atarilibs;
    if (protconn == 2 || protconn == 1)
    	(*maxlibs)++;  /* may not have to connect */
	return(el);  /* 12/31/98 returned el rather than max */
	}

/* generate moves to attack a 2 liberty group (g) to defend group (gs)
 * return TRUE if moves were generated
 */


int attack2libs(group_t gs, group_t g, int *mvp, int tm) {
	int connflag,nearflag,illegal;
	int val,retval = FALSE;
	sqr_t s;
	list_t ptr, gptr;

	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
		val = 0;
		illegal = FALSE;
		nearflag = FALSE;
		connflag = FALSE;
		s = list[ptr];
		if (lnbn[s] <= 1 && lnbf[s][tm] == 0)continue;
		if (inlist(s,&grlbp[gs]))nearflag = TRUE;

		for (gptr = nbgrp[s][tm]; gptr != EOL; gptr = link[gptr]) {
			if (link[nbgrp[s][tm]] == EOL &&
				lnbn[s]+grlibs[list[gptr]] <= 2) {
					illegal = TRUE;
					break;
				}
			if (list[gptr] != gs)
				connflag = TRUE;
			}

		for (gptr = nbgrp[s][1-tm]; gptr != EOL; gptr = link[gptr])
			if (list[gptr] != g) {
				if (grlibs[list[gptr]] == 2)val = 96;  /* double atari */
				else if (grlibs[list[gptr]] == 3)val = 64;
				}

		for (gptr = nblbp[s]; gptr != EOL; gptr = link[gptr])
			if (inlist(list[gptr],&grlbp[gs]))
				nearflag = TRUE;
		if (illegal)continue;
		if (nearflag) {  /* will be examined later in play_next... */
			val += 8;
#ifdef NEVER
			val += 32*lnbn[s];
			if (connflag)val += 64;
#endif			
			}
		if (val != 0) {
			mval[*mvp] = val;
			mmov[(*mvp)++] = s;
			retval = TRUE;
			}
		}
#ifdef CHECK
	if (debug == 2000+(unsigned int)gs || debug == 200) {
		if (retval)outerr("attack2libs genned move");
		}
#endif
	return(retval);
	}

/* True if group g has enough liberties to attack neighboring group gnbr 
 * eff is maximum effective liberties of g.  If return TRUE, then effnbr has
 * neighbors effective liberties
 */

int canattack(group_t g, group_t gnbr, int eff, int *effnbr, listval_t ldrno) {
	int comlibs, maxnbr;
	list_t ptr, tmplist = EOL;
	sqr_t s;
	
	if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[gnbr]]])) {
		adflist(mvs[grpieces[gnbr]],&grldr[ldrno]);
		}
	if (grlibs[g] > 1)
		comlibs = comlist(grlbp[g],grlbp[gnbr]);
	else comlibs = 0;
	if (eff + comlibs < grlibs[gnbr])return(FALSE);
	if (grlibs[gnbr] == 1) {
		*effnbr = 1;
		return(TRUE);
		}
	*effnbr = getefflibs(gnbr,eff+comlibs+1,ldrno,&maxnbr);
	if (eff >= *effnbr)return(TRUE);
	if (eff + comlibs < grlibs[gnbr])return(FALSE);  /* can't make seki */
	if (*effnbr > 5)return FALSE;	/* 4/01 - too complex to read this */
	if (*effnbr == 5 && comlibs < 3)return FALSE;	/* 5/05 seki w 3 common libs */
	if (comlibs == 0)return(FALSE);  /* need two common to make seki */
	if (comlibs == 1 && *effnbr > 3 && eff + comlibs < *effnbr)
		return(FALSE);  /* with one common lib can fill his outside so he can't atari me */
	if (comlibs == 1 && eff == 2 && *effnbr > 3)
		return FALSE;  /* can't prevent atari */
	if (comlibs == 1 && *effnbr == 3) {  /* see if can fill outside liberty to prevent atari */
		andlist(grlbp[g],grlbp[gnbr],&tmplist);
		s = list[tmplist];
		killist(&tmplist);
		if (newliberties(gnbr,grlbp[gnbr],s,1) >= 1)
			return FALSE;  /* he can still atari me - gets a new liberty when fills by liberty */
		}
	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
		if (!inlist(list[ptr],&grlbp[gnbr]))continue;
		if (lnbn[list[ptr]]-1 > comlist(nblbp[list[ptr]],grlbp[gnbr]))
			return(FALSE);  /* he gets new liberty when fills mine */
		}
	return(TRUE);  /* try for seki or short his liberties */
	}


/* check to see if s is an unvreakable connection.  return TRUE if
 * it is.  c is the color being connected.  if c is winsko, then
 * a ko is solid.
 *
 * return TRUE if connection is uncuttable, even if opponent wins all kos
 * return FALSE if connection can be cut
 * return 2 if connection can be cut with ko, and opponent winsko
 */

int solidconnect(sqr_t s, int c, int winsko) {
	int onelibcount, twolibcount, morelibcount, onestonecount, gets2libs;
	int snapflag, havetarget;
	list_t ptr, ptr2;
	group_t g = NOGROUP;
	sqr_t s2;
	if (lnbn[s] <= 1 && lnbf[s][1-c] == 0 ||  /* he plays I capture him */
		lnbf[s][1-c] == 1 && lnbn[s] == 0 &&  /* he can only suicide */
		grlibs[list[nbgrp[s][1-c]]] <= 2) {
#ifdef NEVER
		   && grsize[list[nbgrp[s][1-c]]] > 1)) {
#endif
		if (nbgrp[s][1-c] != EOL &&  /* check for push in two stone sacrifice */
			grsize[list[nbgrp[s][1-c]]] == 1 &&
			grlibs[list[nbgrp[s][1-c]]] == 2) {
			g = (group_t)list[nbgrp[s][1-c]];	/* changed 1/10/98 */
			snapflag = TRUE;
			for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
				if (grlibs[list[ptr]] == 2) {
					s2 = list[grlbp[list[ptr]]];
					if (s2 == s)
						s2 = list[link[grlbp[list[ptr]]]];
					if (!inlist(s2, &grlbp[g]))
						snapflag = FALSE;
					}
				else
					snapflag = FALSE;
				}
			if (snapflag)
				return FALSE;	/* he might make a push then snapback */
			}
		onelibcount = onestonecount = twolibcount = morelibcount = 0;
		gets2libs = FALSE;
		havetarget = FALSE;
		for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
			/* is there a snapback here? */
			if (grlibs[list[ptr]] == 2 && lnbn[s] == 1 && 
				lnbn[list[nblbp[s]]] == 1 && inlist(list[nblbp[s]], &grlbp[list[ptr]]) &&
				link[nbgrp[list[nblbp[s]]][c]] == EOL)
				return FALSE;
			/* is there shortage of liberties here? */
			if (grlibs[list[ptr]] == 2) {
				twolibcount++;
				if (!havetarget) {
					havetarget = TRUE;
					for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
						if (grlibs[list[ptr2]] == 1) {
							havetarget = FALSE;
							break;
						}
					s2 = list[grlbp[list[ptr]]];
					if (s2 == s)
						s2 = list[link[grlbp[list[ptr]]]];
					if (lnbf[s2][1-c] == 0 && lnbn[s2] <= 1)
						havetarget = FALSE;	/* needs approach move */
					if (lnbf[s2][1-c] == 1 && lnbn[s2] == 0 &&
						grlibs[list[nbgrp[s2][1-c]]] <= 2)
						havetarget = FALSE;	/* needs approach move */
				}
			}
			if (grlibs[list[ptr]] > 2)
				morelibcount++;
			if (grlibs[list[ptr]] == 1) {
				onelibcount++;
				if (grsize[list[ptr]] == 1)
					onestonecount++;
				if (nbgrp[s][1-c] != EOL) {
					g = (group_t)list[nbgrp[s][1-c]];	/* changed 3/02 */
					if (grlibs[g] > 1)
						gets2libs = TRUE;
					}
				}
			}
		if (onelibcount > 1 ||  /* more than one capture */ 
			onelibcount == 1 &&	/* one capture and ... */
			(lnbn[s] != 0 || gets2libs/* ||   	/* capturing stone gets two liberties */
			 /* onestonecount == 1 &&  winsko == 1-c 10/97 still worth trying even if don't win ko*/
			 ))  /* capture one stone and opponent can win ko */
			return FALSE;
		if (lnbn[s] == 0 && onelibcount == 1 && onestonecount == 1 && winsko == 1-c)
			return 2;	/* not connected due to ko */
		if (lnbn[s] == 0 && twolibcount == 2 && morelibcount == 0 && havetarget)
			return FALSE;	/* shortage of liberties */

		return TRUE;
		}
	return FALSE;
	}


/* generate moves to defend group at starts 
 * if last move was atari, defend group in atari
 * if group has one liberty, call defonelib
 * if group has 2 liberties and no weak neighbors, call deftwolib
 * otherwise:
 *   try to jump to escape
 *   try to extend for more liberties
 *   try to attack a weak neighbor
 */

static void gendefmoves(int ply, sqr_t starts, group_t g, int tm, listval_t ldrno,int maxlibs, int winsko, int maxbr, int eff_libs, int maxefflibs)
{
	int mvp, common;
	list_t ptr, ptr2, nbatk = EOL, gptr;
	group_t gs, grp;
	int can_attack_nbr;
	int lastmove, nbeff_libs, two_libs_attack;
	int solid_conn = FALSE, sconn;

	lastmove = mvs[sply[ply-1]];
	gs = board[starts];
	sply[ply] = eply[ply-1];

	mvp = 0;
	if (ply > 1 && defatari(ply,g,tm,gs,lastmove,winsko))
		return;

	if (grlibs[gs] == 1 &&
		(winsko == 1-tm || grsize[gs] > 1 ||
		 lnbf[list[grlbp[gs]]][1-tm] ||  /* 9/99 can fight on even if another stone of ko is in atari graded2l prob 51 */
		 lnbn[list[grlbp[gs]]])) {
		/* !iskopoint((sqr_t)list[grlbp[gs]], &tmp))) { /* 10/97 can fight on if there is a ko here - don't have to connect */
		defonelib(ply, starts, g, tm, gs, ldrno, winsko);
		return;
	}

	can_attack_nbr = FALSE;
	two_libs_attack = FALSE;
	for (ptr = grnbp[gs]; ptr != EOL; ptr = link[ptr]) {
		if (canattack(gs,(group_t)list[ptr],maxefflibs,&nbeff_libs,ldrno)) {
			can_attack_nbr = TRUE;
			mvp = def_atk_nbr(gs, NOGROUP, (group_t)list[ptr], mvp, g, eff_libs, maxefflibs, nbeff_libs, tm, ldrno, winsko);
			addlist(list[ptr],&nbatk);
		}
		else if (grlibs[list[ptr]] == 2 && attack2libs(gs,(group_t)list[ptr], &mvp, tm)) {
			two_libs_attack = TRUE;
			addlist(list[ptr],&nbatk);
		}
	}
    
    
	if (eff_libs >= 2) {  /* find connected nbrs to attack enemy nbrs */
		for (ptr = grlbp[gs]; ptr != EOL; ptr = link[ptr]) {
			sconn = solidconnect((sqr_t)list[ptr], tm, winsko);
			if (sconn != FALSE) {  /* solid connection not a ko */
				solid_conn = TRUE;
				for (gptr = nbgrp[list[ptr]][tm]; gptr != EOL; gptr = link[gptr]) {
					grp = (group_t)list[gptr];
					if (grp == gs) {
						continue;
					}
					for (ptr2 = grnbp[grp]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (inlist(list[ptr2],&nbatk))
							continue;
						if (mvp > NUMGEN/2)
							break;  /* don't generate too many moves! */
						if (sconn == TRUE) {
							if (canattack(gs,(group_t)list[ptr2],eff_libs,&nbeff_libs,ldrno)) {
								can_attack_nbr = TRUE;
								mvp = def_atk_nbr(gs, grp, (group_t)list[ptr2], mvp, g, eff_libs, maxefflibs, nbeff_libs, tm, ldrno, winsko);
								addlist(list[ptr2],&nbatk);
							}
							else if (grlibs[list[ptr2]] == 2 && attack2libs(gs,(group_t)list[ptr2],&mvp,tm)) {
								two_libs_attack = TRUE;
								addlist(list[ptr2],&nbatk);
							}
						}
						else if (grlibs[list[ptr2]] == 1) {  /* ko, only attack one liberty groups */
							mvp = def_atk_nbr(gs, grp, (group_t)list[ptr2], mvp, g, eff_libs, maxefflibs, 1, tm, ldrno, winsko);
						}
					}
				}
			}
		}
	}
	killist(&nbatk);

	if (ply <= 2) {
		mvp = jump_to_escape(gs,mvp);
	}

	mvp = play_next_to_group(gs, mvp, g, eff_libs, maxlibs, ldrno, winsko);

	if (mvp == 0) {  /* haven't found any moves yet, try for seki */
		for (ptr = grnbp[gs]; ptr != EOL; ptr = link[ptr]) {
		   common = comlist(grlbp[gs],grlbp[list[ptr]]);
		   if (common >= 1 && grlibs[list[ptr]] <= eff_libs + common) {
			   mvp = def_atk_nbr(gs, NOGROUP, (group_t)list[ptr], mvp, g, eff_libs, maxefflibs, grlibs[list[ptr]], tm, ldrno, winsko);
		   }
		}
	}

 	sortmoves(mvp, winsko, ply, maxbr, tm, "Def", g, gs, maxlibs);
}





/* generate moves from two stone wall.  rules:
 * at edge of board, just try hane if it connects.
 * if both sides open, try both jumps.
 * if one side open, try jumping on other.
 * if both sides open, hane if cannects, or attacks weak nbr
 */

int def_two_stone_wall(sqr_t l1, sqr_t l2, group_t gs, int mvptr, list_t *generated) {
	sqr_t hane, j;

	hane = l1+l1-l2;
	if (lnbn[l1] == 3 && (lnbf[hane][grcolor[gs]] != 0 ||
		lnbn[hane] == 3) && addlist(hane,generated))
		mvs[mvptr++] = hane;	
	hane = l2+l2-l1;
	if (lnbn[l2] == 3 && (lnbf[hane][grcolor[gs]] != 0 ||
		lnbn[hane] == 3) && addlist(hane,generated))
		mvs[mvptr++] = hane;	
	if (lnbn[l1] < 3 && lnbn[l2] < 3)
		return(mvptr);  /*hosed*/
	if (lnbn[l1] == 2 && board[l1+l1-l2] != NOGROUP && lnbn[l2] == 3) {
		j = jump(l1,gs);
		if (addlist(j,generated))
			mvs[mvptr++] = j;
		}
	else if (lnbn[l1] == 3 && lnbn[l2] == 2 && board[l2+l2-l1] != NOGROUP) {
		j = jump(l2,gs);
		if (addlist(j,generated))
			mvs[mvptr++] = j;
		}
	if (lnbn[l1] != 3 || lnbn[l2] != 3)
		return(mvptr);
	j = jump(l1,gs);
	if (addlist(j,generated))
		mvs[mvptr++] = j;
	j = jump(l2,gs);
	if (addlist(j,generated))
		mvs[mvptr++] = j;
	return(mvptr);
	}

/* find jumping move from liberty relative to g */

static sqr_t jump(sqr_t lib, group_t g) {
	int i,sn,ldtmp;
	i = fdir[lib];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		sn = lib + nbr[i];
		if (board[sn] == g)return(lib - nbr[i]);
		}
#ifdef CHECK
	outerror("ERROR in jump!\n");
#endif
	return(lib);
	}

#ifdef NEVER

/* generate defensive moves for two liberty group which cannot
 * attack a neighbor
 * rules:
 *	jump from two stone wall
 *	if both liberties have 3 libs of libs and liberties are adjacent
 *		try hane.
 *      potential eye is liberty surrounded by friendly stones
 *	if two potential eyes, pass
 *	if can't get more liberties, pass
 *	extend or connect for most liberties
 *      if a lib can be surrounded to make a potential eye, do it
 *	if liberty has two adjacent friendly stones and no enemies, hane to
 *		try to make an eye.
 */

static void deftwolibs(int ply, sqr_t starts, group_t g, int tm, group_t gs) {
	sqr_t s, libs[2];
	int mvptr,newlibs[2],i,j,ldtmp,hane,c,passok = TRUE;
	int pot_eye[2],twostonewall,can_connect,connlibs[2],can_atari;
	group_t cgroup;
	list_t generated = EOL,ptr,ptr2;
#ifdef CHECK
	char buf[80],buf2[10];
#endif
	g = g;
	starts = starts;
	c = grcolor[gs];
	mvptr = sply[ply];
	libs[0] = list[grlbp[gs]];		/* find liberties */
	libs[1] = list[link[grlbp[gs]]];

	can_connect = FALSE;
	twostonewall = g2abs(libs[0]-libs[1]) == 1 || 
		g2abs(libs[0]-libs[1]) == boardsize;

	pot_eye[0] = lnbf[libs[0]][1-tm] == 0 && lnbn[libs[0]] == 0;
	pot_eye[1] = lnbf[libs[1]][1-tm] == 0 && lnbn[libs[1]] == 0;
	can_atari = FALSE;

	for (i = 0; i < 2; ++i) {
		newlibs[i] = 0;  /* new open liberties gained here */
		connlibs[i] = 0; /* liberties from connecting here */
		cgroup = NOGROUP;
		j = fdir[libs[i]];
		for (ldtmp = ldir[j]; j < ldtmp; ++j) {
			s = libs[i]+nbr[j];
			if (grcolor[board[s]] == tm && board[s] != gs) {
				for (ptr = grnbp[board[s]]; ptr != EOL; ptr = link[ptr])
					if (grlibs[list[ptr]] == 1) {
						newlibs[i]++;
						}
				connlibs[i] = newlist(grlbp[gs],grlbp[board[s]]);
				if (connlibs[i] < 2)
					for (ptr = grlbp[board[s]]; ptr != EOL; ptr = link[ptr])
						if ((sqr_t)list[ptr] != libs[i] &&  /* new liberty */
							(lnbn[list[ptr]] <= 1 &&  /* needs approach move */
							lnbf[list[ptr]][1-c] == 0 || 
							link[nbgrp[list[ptr]][c]] != EOL)) /* threatens connection */
							connlibs[i] ++; 
				if (connlibs[i] && board[s] != cgroup) {
					can_connect = TRUE;
					cgroup = board[s];
					newlibs[i] += connlibs[i];
					if (lnbn[libs[i]] <= 1 && 
					   lnbf[libs[i]][1-c] == 0) {
					   	for (ptr = grnbp[cgroup]; ptr != EOL; ptr = link[ptr])
					   		if (grlibs[list[ptr]] == 1) {	 /* capture */
					   			mvs[mvptr++] = list[grlbp[list[ptr]]];
					   			}
						for (ptr = grlbp[cgroup]; ptr != EOL; ptr = link[ptr])
							if ((sqr_t)list[ptr] != libs[i] &&
								(lnbn[list[ptr]] > 1 || lnbf[list[ptr]][c] > 1) &&
								addlist(list[ptr],&generated))
								mvs[mvptr++] = list[ptr];  /* play from unbreakable connection */
					   }
					}
				}
			else if (board[s] == NOGROUP && s != libs[1-i]) {
				newlibs[i]++;
				if (lnbf[s][tm] != 0 && lnbn[s] <= 2 &&
				   lnbf[s][1-tm] == 0) {
					can_connect = TRUE;
					if (addlist(libs[i],&generated))
						mvs[mvptr++] = libs[i];
					}
				else if (lnbf[s][1-tm] == 0 && lnbn[s] == 1)
					newlibs[i]++;
				else if (lnbf[s][tm] != 0 && lnbn[s] <= 2)
					newlibs[i]++;  /* maybe can connect */
				}
			else if (grcolor[board[s]] == 1-tm && grlibs[board[s]] <= 2) {
				if (grlibs[board[s]] == 1 && !inlist(board[s],&grnbp[gs]))
					newlibs[i]++;
				can_atari = TRUE;
				if (grlibs[board[s]] == 2 && 
				   lnbn[libs[i]] == 1 && 
				   lnbf[libs[i]][1-tm] == 1 &&
				   inlist(list[nblbp[libs[i]]],&grlbp[board[s]]) &&
					addlist(list[nblbp[libs[i]]],&generated)) {
					mvs[mvptr++] = list[nblbp[libs[i]]];
					/* snapback */
					}
				}
			else if (grcolor[board[s]] == 1-tm && grlibs[board[s]] > 2)
				passok = FALSE;
			}
		newlibs[i]--;  /* since fill a liberty to play here */
		if (connlibs[i] && newlibs[i] && lnbf[libs[i]][1-tm] == 0)  /* make hanging connection */
			for (ptr = nblbp[libs[i]]; ptr != EOL; ptr = link[ptr])
				if (list[ptr] != libs[1-i] &&
					(lnbf[list[ptr]][tm] || lnbn[list[ptr]] >= 2))
				{
					mvs[mvptr++] = list[ptr];  /* make hanging connection */
				}	

		}



	if (twostonewall && !can_connect) {
		mvptr = def_two_stone_wall(libs[0],libs[1],gs,mvptr,&generated);
		}

/* handle plays in liberties */

	if (generated == EOL && newlibs[0] == newlibs[1] && pot_eye[0] == pot_eye[1] &&
		connlibs[0] == connlibs[1]) /* no reason to prefer one over the other */
		maxnply[ply+1] = numnn + (maxnply[ply]-numnn)/2;

	if (twostonewall && !can_connect &&     /* on edge of board */
	   edge[libs[0]] <= 1 && edge[libs[1]] <= 1) {
		hane = libs[0]+libs[0]-libs[1];
		if (lnbf[hane][c] != 0 ||
		   lnbn[hane] == 3)
			if (addlist(libs[0],&generated))
				mvs[mvptr++] = libs[0];	
		hane = libs[1]+libs[1]-libs[0];
		if (lnbf[hane][c] != 0 ||
		   lnbn[hane] == 3)
		   if (addlist(libs[1],&generated))
			mvs[mvptr++] = libs[1];	
		}
	else if (pot_eye[0] && pot_eye[1])  /* don't fill eyes */
		mvs[mvptr++] = PASS;

	else if (newlibs[0] == newlibs[1] && newlibs[0] > 0 || can_atari) {
		if (!pot_eye[0]) {
		   if (addlist(libs[0],&generated))
				mvs[mvptr++] = libs[0];
		   if (addlist(libs[1],&generated))
				mvs[mvptr++] = libs[1];
			}
		else if (!pot_eye[1]) {
		   if (addlist(libs[1],&generated))
				mvs[mvptr++] = libs[1];
		   if (addlist(libs[0],&generated))
				mvs[mvptr++] = libs[0];
			}
		}

	else if (newlibs[0] > 0 && newlibs[0] > newlibs[1]) {
		if (addlist(libs[0],&generated))
			mvs[mvptr++] = libs[0];
		if ((pot_eye[0] || newlibs[1] > 0 && 
			newlibs[0]-newlibs[1] == 1) &&
			addlist(libs[1],&generated))
			mvs[mvptr++] = libs[1];
		}
	else if (newlibs[1] > 0) {
		if (addlist(libs[1],&generated))
		mvs[mvptr++] = libs[1];
		if ((pot_eye[1]  || newlibs[0] > 0 &&
			newlibs[1]-newlibs[0] == 1) &&
			addlist(libs[0],&generated))
			mvs[mvptr++] = libs[0];
		}

	if (passok && (!pot_eye[0] && lnbn[libs[0]] == 0 && lnbn[libs[1]] == 0 ||
		twostonewall && lnbn[libs[0]] < 2 && lnbn[libs[1]] < 2)) {
		mvs[mvptr++] = PASS;
		}


	for (i = 0; i < 2; ++i) {
		if (edge[libs[i]] == 1 && lnbn[libs[i]] == 2)  /* hane up from edge */
			for (ptr = nblbp[libs[i]]; ptr != EOL; ptr = link[ptr])
				if (edge[list[ptr]] > 1 && (sqr_t)list[ptr] != libs[1-i] && 
					(lnbn[list[ptr]] >= 3 || lnbf[list[ptr]][tm] != 0))
					mvs[mvptr++] = list[ptr]; 
		if (lnbf[libs[i]][1-tm] != 0 || lnbn[libs[i]] != 1)continue; 
		mvs[mvptr++] = list[nblbp[libs[i]]];  /* make eye */
		for (ptr = nblbp[list[nblbp[libs[i]]]]; ptr != EOL; ptr = link[ptr])
			if (lnbf[list[ptr]][1-c] != 0 && lnbf[list[ptr]][c] != 0 && addlist(list[ptr],&generated))
				mvs[mvptr++] = list[ptr];
		}

	if (!twostonewall)for (i = 0; i < 2; ++i) /* connect with uncuttable shape */
		if (lnbn[libs[i]] == 2 && lnbf[libs[i]][1-tm] == 0 && !can_connect)
			for (ptr = nblbp[libs[i]]; ptr != EOL; ptr = link[ptr]) {
				if (lnbf[list[ptr]][tm] != 0) {
					if (addlist(list[ptr],&generated))
						mvs[mvptr++] = list[ptr];
					continue;
					}
				if (lnbn[list[ptr]] == 4) {
					if (addlist(list[ptr],&generated))
						mvs[mvptr++] = list[ptr];
					continue;
					}
				if (lnbn[list[ptr]] <= 1)continue;
				for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if ((sqr_t)list[ptr2] == libs[i])continue;
					if (lnbn[list[ptr2]] <= 2 && lnbf[list[ptr2]][1-tm] == 0) {
						if (addlist(list[ptr],&generated))
							mvs[mvptr++] = list[ptr];
						break;
						}
					}
				}
	
	for (i = 0; i < 2; ++i) { /* make connection not already generated */
		if (connlibs[i] > 1 &&
			addlist(libs[i],&generated))
			mvs[mvptr++] = libs[i];
		}
	eply[ply] = mvptr;
	for (i = sply[ply]; i < eply[ply]; ++i) {
		mvcolor[i] = (char)tm;
#ifdef CHECK
		if (debug == 2000+(unsigned int)g) {
			sprintf(buf,"def2 genning %s",ssqr(mvs[i],buf2));
			outerr(buf);
			}
#endif
		}
	killist(&generated);
	}
#endif


/* generate attack moves for two liberty groups 
 * generate moves for simple attacks of two liberty groups
 * if can't find simple attack, return false and let genatkmoves
 * generate the moves.
 * rules:
 *	try to geta the group if liberties are on diagonal and
 *		have two liberty liberties each and all nbr groups
 *		have more than two liberties and there are no
 *		connections to friendly groups
 *	try for loose ladder if libs on diagonal, all nbr groups
 *  		have more than 2 libs and no conns to friendly groups
 *	fill liberty which cuts connection to group with more liberties
 *	if there are more than 2 neighboring groups with 2 liberties
 *		then must fill a liberty
 *	if liberties are adjacent then filling lib will reduce nbn of
 *		other liberty
 *	fill liberty with highest nbn first
 *	if there are more than two nbr groups with 2 liberties
 *		then must not leave a liberty with nbn = 3
 *	Don't fill a liberty which will lead to immediate capture unless
 *		throw in works
 *	Throw in works if other liberty is adjacent to this liberty and
 *		move is a cut, or group being cut from has few liberties
 *              and other libs adjacent to this lib are not new.
 *	Throw in also works if move is cut and other liberty is not
		throw in
 *	If both liberties are protected and no throw in works, play next
 *		to a liberty
 */


int atktwolibs(int ply,sqr_t starts, group_t g, int tm, group_t gs, listval_t ldrno, int winsko) {
	sqr_t sn,sopen;
	group_t gn;
	list_t ptr;
	sqr_t libs[2];
	int cut[2],getaflag,i,ldtmp,libs_adjacent,j,getaval,tmp;
	int push[2],capture[2],throwin[2],illegal[2],oldmvp;
	int connect[2],pushfrom[2],val[2],c,num2libs = 0,num3libs = 0,libs_diagonal;
	int hole[2];  /* pushing this way goes into hole */
	int throwinok[2];  /* throwin here reduces a liberty */
	int friend2libs[2];	/* friendly nearby group with two liberties */

	tm = tm;
	g = g;
	starts = starts;
	c = grcolor[gs];	/* color of group being attacked */
	oldmvp = mvp;
	for (ptr = grnbp[gs]; ptr != EOL; ptr = link[ptr]) {
		gn = (group_t)list[ptr];
		if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[gn]]])) {
			adflist(mvs[grpieces[gn]],&grldr[ldrno]);
			}
		if (grlibs[gn] == 1)return(FALSE); /* too complicated */
		if (grlibs[gn] == 2)++num2libs;
		if (grlibs[gn] == 3)++num3libs;
		}

	libs[0] = list[grlbp[gs]];		/* find liberties */
	libs[1] = list[link[grlbp[gs]]];
	
	/* first make sure we don't have a snapback */
	if (issnapback(libs[0],libs[1],gs,c)) {
		mmov[mvp] = libs[0];
		mval[mvp] = 128;
		mvp++;
		return TRUE;
		}
	if (issnapback(libs[1],libs[0],gs,c)) {
		mmov[mvp] = libs[1];
		mval[mvp] = 128;
		mvp++;
		return TRUE;
		}

	if (lnbn[libs[0]] == 0 && lnbn[libs[1]] == 0 && num2libs)
		return(FALSE);
	if (link[nbgrp[libs[0]][c]] != EOL && link[nbgrp[libs[1]][c]] != EOL)
		return(FALSE);
	if (link[nbgrp[libs[0]][c]] != EOL && 
			lnbn[libs[0]] <= 1 && 
			lnbf[libs[0]][1-c] == 0 || 
		link[nbgrp[libs[1]][c]] != EOL && 
			lnbn[libs[1]] <= 1 && 
			lnbf[libs[1]][1-c] == 0)
		return FALSE;  /* already connected to another friendly group */
	cut[0] = cut[1] = FALSE;
	push[0] = push[1] = FALSE;
	connect[0] = connect[1] = FALSE;
	capture[0] = capture[1] = FALSE;
	throwin[0] = throwin[1] = FALSE;  /* indicates spot has one liberty */
	illegal[0] = illegal[1] = FALSE;
	pushfrom[0] = pushfrom[1] = NOGROUP;

	libs_diagonal = (signed)libs[1] - (signed)libs[0] == boardsize-1 || 
		(signed)libs[1] - (signed)libs[0] == boardsize+1;
	libs_adjacent = (signed)libs[1] - (signed)libs[0] == 1 || (signed)libs[1] - (signed)libs[0] == boardsize;


	if (libs_diagonal && num2libs == 0 && lnbn[libs[0]] == 2 && lnbn[libs[1]] == 2 &&
	   lnbf[libs[0]][c] == 1 && lnbf[libs[1]][c] == 1) {  /* try for geta */
		sn = NOSQUARE;
		if ((signed)libs[1]-(signed)libs[0] == boardsize-1) {
			if (board[libs[0]-1] == NOGROUP)
				sn = libs[0]-1;
			if (board[libs[1]+1] == NOGROUP)
				sn = libs[1]+1;
			}
		if ((signed)libs[1]-(signed)libs[0] == boardsize+1) {
			if (board[libs[0]+1] == NOGROUP)
				sn = libs[0]+1;
			if (board[libs[1]-1] == NOGROUP)
				sn = libs[1]-1;
			}
		if (sn != NOSQUARE && lnbf[sn][1-c] < 2) {
			mmov[mvp] = sn;
			mval[mvp++] = 20;  /* will get another 70 added later on */
#ifdef CHECK
			if (debug == 2000+(unsigned int)g)outerr("atktwolibs found perfect geta");
#endif
			}
		}


	if (ply > 3 && libs_diagonal && (lnbf[libs[0]][c] == 1 || lnbf[libs[1]][c] == 1)) {
		/* check for ladder */
		for (i = 0; i < 2; ++i)
			if (lnbn[libs[i]] == 3 && lnbn[libs[1-i]] == 2) {
				mmov[mvp] = libs[i];
				mval[mvp++] = 128;
#ifdef CHECK
	if (debug == 2000+(unsigned int)g)outerr("atktwolibs found ladder move\n");
#endif
				return(TRUE);
				}
		}
			

	/* find out if filling this liberty is a cut */
	/* find out if stone here pushes from friendly stone */

	for (i = 0; i < 2; ++i) {
		hole[i] = FALSE;
		friend2libs[i] = 0;
		j = fdir[libs[i]];
		for (ldtmp = ldir[j]; j < ldtmp; ++j) {
			sn = libs[i] + nbr[j];
			if (board[sn] == gs)continue;
			if (grcolor[board[sn]] == c) {
				cut[i] = TRUE;
				if (grlibs[board[sn]] == 1)
					capture[i] = TRUE;
				else if (lnbn[libs[i]] <= 2 &&  /* slapping tesuji */
					 grlibs[board[sn]] <= 5-lnbn[libs[i]] && 
					 lnbf[libs[1-i]][c] == 1 &&
					 inlist(libs[i],&nblbp[libs[1-i]]) &&
					 lnbn[libs[1-i]] == 3) {
					 for (ptr = nblbp[libs[1-i]]; ptr != EOL; ptr = link[ptr])
					 	if ((lnbn[list[ptr]] == 3 || lnbf[list[ptr]][1-c]) &&
					 		(sqr_t)list[ptr] != libs[i] &&
							inflist(board[sn],&nbgrp[list[ptr]][c])) {
							mmov[mvp] = list[ptr];
							mval[mvp++] = 105;
							}
					 }
				}
			else if (grcolor[board[sn]] == 1-c) {
				push[i] = TRUE;
				if (board[sn] != pushfrom[i] &&
				    pushfrom[i] != NOGROUP)connect[i] = TRUE;
				pushfrom[i] = board[sn];
				}
			else if (lnbn[sn] <= 2 && lnbf[sn][c] == 0)
				hole[i] = TRUE;
			else {
				for (ptr = nbgrp[sn][1-c]; ptr != EOL; ptr = link[ptr])
					if (grlibs[list[ptr]] == 2)
						friend2libs[i] = TRUE;
				}
			}
		}

#ifdef NEVER

	throwins are handled with cuts!
		

	if (libs_adjacent && lnbn[libs[0]] == 1 && lnbn[libs[1]] == 1) {
		/* throwins */
		if (lnbf[libs[0]][1-c] == 0 && lnbf[libs[1]][1-c] == 0) {
	   		if (cut[0]) {
				mmov[mvp] = libs[0];
				mval[mvp++] = 128;
				}
			if (cut[1]) {
				mmov[mvp] = libs[1];
				mval[mvp++] = 128;
				}
			return(TRUE);
			}  
		else if (lnbf[libs[0]][1-c] == 0 && cut[0]) {
				mmov[mvp] = libs[0];
				mval[mvp++] = 128;
			return(TRUE);
				}
		else if (lnbf[libs[1]][1-c] == 0 && cut[1]) {
				mmov[mvp] = libs[1];
				mval[mvp++] = 128;
			return(TRUE);
				}
		}

#endif
		

	/* find out if move is illegal or can be immediately captured */

	for (i = 0; i < 2; ++i) {
		throwinok[i] = FALSE;
		if (!push[i] && !capture[i] && lnbn[libs[i]] == 1) {
			throwin[i] = TRUE;	 /* is a single stone throwin */
			sopen = list[nblbp[libs[i]]];
			if (lnbn[sopen] == 1 && lnbf[sopen][c] == 0 &&
				iskopoint(libs[i], &tmp))
				throwinok[i] = TRUE;	/* throwin to start a ko */
		}
		if (!push[i] && !capture[i] && lnbn[libs[i]] == 1 && cut[i]) {
			if (!libs_adjacent) {
				sopen = list[nblbp[libs[i]]];
				if (lnbn[sopen] <= 2)throwinok[i] = TRUE;
				else for (ptr = nblbp[sopen]; ptr != EOL; ptr = link[ptr])
					if ((sqr_t)list[ptr] == libs[1-i]) {
							throwinok[i] = TRUE;
							break;
							}
				}
			else if (lnbn[libs[1-i]] == 1)throwinok[i] = TRUE;
			else if (lnbn[libs[1-i]] == 2) {
				for (ptr = nblbp[libs[1-i]]; ptr != EOL; ptr = link[ptr])
					if ((sqr_t)list[ptr] != libs[i] && 
						(lnbf[list[ptr]][c] != 0 || ply < 3))
						throwinok[i] = TRUE;
				}
			else if (ply < 3)
				throwinok[i] = TRUE;
			}
		if (!push[i] && !capture[i] && lnbn[libs[i]] == 0 ||
			kosquare == libs[i] && winsko == c)
			illegal[i] = TRUE;
		if (push[i] && !capture[i] && !connect[i] &&
			(grlibs[pushfrom[i]] + lnbn[libs[i]] == 1 ||
			 grlibs[pushfrom[i]] + lnbn[libs[i]] == 2 &&
			 grsize[pushfrom[i]] > 1))  /* allow 2 stone throwin sacrifice */ 
			illegal[i] = TRUE;
		}



	/* pick order for filling in the two liberties */

	/* make cutting move first (includes throw ins) */

	if (cut[0] && !illegal[0] && !throwin[0] || throwinok[0]) {
		mmov[mvp] = libs[0];
		if (throwin[0]) {
			if (lnbn[libs[0]] == 1 && lnbf[list[nblbp[libs[0]]]][c] == 0 &&
				lnbn[list[nblbp[libs[0]]]] == 1)  /* ko 5/03 fix bug */
				mval[mvp] = 16;
			else
				mval[mvp] = 64;
			}
		else 
			mval[mvp] = 128;
		mvp++;
		illegal[0] = TRUE;
		}
	if (cut[1] && !illegal[1] && !throwin[1] || throwinok[1]) {
		mmov[mvp] = libs[1];
		if (throwin[1]) {
			if (lnbn[libs[1]] == 1 && lnbf[list[nblbp[libs[1]]]][c] == 0 &&
				lnbn[list[nblbp[libs[1]]]] == 1)  /* ko */
				mval[mvp] = 16;
			else
				mval[mvp] = 64;
			}
		else
			mval[mvp] = 128;
		mvp++;
		illegal[1] = TRUE;
		}


	/* make move which takes away most liberties of liberties */
	/* if equal, best move pushes from friendly stone */
	/* if equal, move on side away from edge */
	/* if equal, push away from 2 liberto group */

	val[0] = val[1] = 0;
	for (i = 0; i < 2; ++i) {
		if (illegal[i])continue;
		val[i] += lnbn[libs[i]]*32;
		if (push[i] && grlibs[pushfrom[i]] < 3 && lnbn[libs[i]] > 1)val[i] += 32;
		if (push[i])val[i] += 8;
		if (hole[i])val[1-i] += 6;
		if (friend2libs[i])val[i] += 3;
		}
	/* lib on 2nd line must be more than hole or some cut on 3rd line don't work */
	if (edge[libs[1]] <= 3 && edge[libs[0]] > edge[libs[1]])
		val[0] += 4*(4-edge[libs[1]]);
	else if (edge[libs[0]] <= 3 && edge[libs[1]] > edge[libs[0]])
		val[1] += 4*(4-edge[libs[0]]);

	/* fill the liberty */
	if (!illegal[1] && !throwin[1]) {
		mmov[mvp] = libs[1];
		mval[mvp++] = val[1];
		illegal[1] = TRUE;
		}
	if (!illegal[0] && !throwin[0]) {
		mmov[mvp] = libs[0];
		mval[mvp++] = val[0];
		illegal[0] = TRUE;
		}


	/* move next to throwin spot */

	if (throwin[0]) {
		mmov[mvp] = list[nblbp[libs[0]]];
		mval[mvp++] = 32;
		}
	if (throwin[1]) {
		mmov[mvp] = list[nblbp[libs[1]]];
		mval[mvp++] = 32;
		}



   		/* try for geta or loose ladder */

	if (!cut[0] && !cut[1] && !libs_adjacent && num2libs <= 1 &&
	 	(lnbn[libs[0]] == 2 || lnbn[libs[1]] == 2)) {
		getaflag = TRUE;
		for (i = 0; i < 2; ++i)
			if (push[i] && grlibs[pushfrom[i]] < 3 &&
				!comlist(grlbp[pushfrom[i]],nblbp[libs[i]])) {
				getaflag = FALSE;
				break;	
			/* no geta if adjacent group to push-out point only 2 or 1 libs */
				}
		if (getaflag) {
			getaval = 90;
			if (num2libs)
				getaval -= 10 + (num2libs+num3libs-1)*30;
			if ((signed)libs[1]-(signed)libs[0] == boardsize-1) {
				if (board[libs[0]-1] == NOGROUP) {
					mmov[mvp] = libs[0]-1;
					mval[mvp++] = getaval;
					}
				if (board[libs[1]+1] == NOGROUP) {
					mmov[mvp] = libs[1]+1;
					mval[mvp++] = getaval;
					}
				}
			if ((signed)libs[1]-(signed)libs[0] == boardsize+1) {
				if (board[libs[0]+1] == NOGROUP) {
					mmov[mvp] = libs[0]+1;
					mval[mvp++] = getaval;
					}
				if (board[libs[1]-1] == NOGROUP) {
					mmov[mvp] = libs[1]-1;
					mval[mvp++] = getaval;
					}
				}
			}
		}
#ifdef CHECK
	if (mvp && debug == 2000+(unsigned int)g)outerr("from atktwolibs");
#endif
	return(mvp != oldmvp);  /* if generate no moves, return false */
	}

/* return moves to defend group g */
    
list_t getdefmoves(group_t g) {      
	list_t movelist = EOL;
	int i, efflibs, maxlibs;
	sply[0] = msptr - 1;
	eply[0] = msptr;
	sply[1] = eply[0];
	numnn = 0;
	maxnply[1] = maxnply[0] = 30;
	efflibs = getefflibs(g,4,NOGROUP,&maxlibs);
	gendefmoves(1,mvs[grpieces[g]],g,grcolor[g],NOGROUP,4,1-grcolor[g],2,efflibs,maxlibs);
	for (i = eply[1]-1; i >= sply[1]; --i) {
		adflist(mvs[i],&movelist);
		}
	return(movelist);
	}


/* return moves to attack group g, from best to worse */
    
list_t getatkmoves(group_t g) {      
	list_t movelist = EOL;
	int i;
	sply[0] = msptr - 1;
	eply[0] = msptr;
	sply[1] = eply[0];
	numnn = 0;
	maxnply[1] = maxnply[0] = 30;
	genatkmoves(1,mvs[grpieces[g]],g,1-grcolor[g],NOGROUP,4,grcolor[g],2);
	for (i = eply[1]-1; i >= sply[1]; --i) {
		adflist(mvs[i],&movelist);
		}
	return(movelist);
	}


void atkonelib(int ply, sqr_t starts, group_t g, int tm, group_t gs, listval_t ldrno) {
	mmov[mvp] = list[grlbp[gs]];
	mval[mvp++] = 100;
	}

/* generate moves to attack group at starts */

static void genatkmoves(int ply, sqr_t starts, group_t g, int tm, listval_t ldrno, int maxlibs, int winsko, int maxbr) {
	group_t gs;
	gs = board[starts];
	sply[ply] = eply[ply-1];
	mvp = 0;
	ASSERT(list[EOL] == 0xffff);

	if (grlibs[gs] == 1 && grsize[gs] > 1)	/* never true for lookahead, but genmoves called elsewhere also */
		atkonelib(ply, starts, g, tm, gs, ldrno);
	else if (grlibs[gs] == 2) {
		if (!atktwolibs(ply,starts,g,tm,gs,ldrno,winsko))
			genrestatk(ply,starts,g,tm,gs,ldrno,maxlibs,winsko);
		}
	else 
		genrestatk(ply,starts,g,tm,gs,ldrno,maxlibs,winsko);

   /* make move list */

	sortmoves(mvp,winsko,ply,maxbr,tm,"Atk",g,gs,maxlibs);
	}
                              
/* does a move at s by color capture a ko */                              
                              
static int iskocapture(sqr_t s, int color) {
	int koflag;
	list_t tptr;
	
	if (s == PASS)
		return FALSE;
	if (lnbn[s] == 0 && lnbf[s][color] == 0) {  /* capture ko */
		koflag = 0;
		for (tptr = nbgrp[s][1-color]; tptr != EOL; tptr = link[tptr]) {
			if (grlibs[list[tptr]] == 1) {
				if (grsize[list[tptr]] == 1)
					koflag++;
				else
					return FALSE;	 /* capture a group with 2 or more stones */
				}
			}
		if (koflag == 1)return(TRUE);
		}
	return FALSE;
	}

/* sort the generated moves by value */
	
static void sortmoves(int mvp, int winsko, int ply, int maxbr, int tm, char *name, group_t g, group_t gs, int maxlibs) {
	int j,k,mvall,best=0,num,koflag,diff=0,mvptr,bestval=0, bestk, bestprobval;
#ifdef MOVESORT
	int bestkprob;
#endif
	list_t gptr;
	int havepass = FALSE;	/* if have a pass, must generate it */
#ifdef G2DEBUGOUTPUT
	char buf[100];
#endif
#ifdef CHECK
	char tmpstr[10];
#endif
   
	g = g;
	name = name;
	mvptr = sply[ply];

#ifdef CHECK
	if (mvp > NUMGEN-1) {
		outerror("sortmove move list overflow\n");
		for (k = 0; k < mvp; ++k)
			outerror(ssqr(mmov[k],buf));
		outerror("\n");
	}
#endif

	best = -1000;
	bestprobval = -1;
	bestk = -1;
	for (k = 0; k < mvp; ++k) {	/* combine duplicates and close holes */
		if (mmov[k] < 0 || mmov[k] > PASS || board[mmov[k]] != NOGROUP) {
#ifdef G2DEBUGOUTPUT
			sprintf(buf, "Bad move in tactics move sort. k %d mmov[k] %d mval[k] %d, board %d\n", k, mmov[k], mval[k], board[mmov[k]]);
			outerror(buf);
#endif
			mval[k] = -1000;
			continue;
		}
		if (mmov[k] == NOSQUARE)
			continue;
		for (j = k+1; j < mvp; ++j)
			if (mmov[j] == mmov[k]) {
				if (mval[j] > mval[k])  /* combine by finding maximum value */
					mval[k] = mval[j];  /* evaluators are free to make full evaluations */
				mval[j] = -2000;
				mmov[j] = NOSQUARE;
		}
		if (mmov[k] == PASS)
			havepass = TRUE;
		if (winsko != tm && 
			(mmov[k] == kosquare ||
			 iskocapture(mmov[k],tm))) {
#ifdef CHECK
			if (debug == 2000 + (unsigned int)g || debug == 200) {
				outerr("not genning ill ko ");
				outerr(ssqr(mmov[k],tmpstr));
				outerr("\n");
				}
#endif
			mval[k] = -1000;
			continue;
		}
		if (mval[k] > best) {	/* best generated move */
			best = mval[k];
			bestk = k;
		}
#ifdef MOVESORT
		if (ply > 1) {
			mprob[k] = bestprob(mvs[sply[ply-1]], mmov[k]);
			if (mprob[k] > bestprobval) {
				bestprobval = mprob[k];
				bestkprob = k;
			}
		}
#endif
	}

#ifdef MOVESORT
	if (bestk != -1 && mprob[bestk] != -1 &&
		bestprobval == 100 && mprob[bestk] < 50)	/* move best better move to front */
		mval[bestkprob] = mval[bestk] + 10;
#endif

	mvall = mvp;
	if (ply > 3 && mvall > maxbr)
		mvall = maxbr;  /* maxbr limits branches after ply 3 */
	if (ply > 1 && mvall > maxbr+1)
		mvall = maxbr+1;  /* ply 2 and 3 get an extra move */
	if (ply == 1 && mvall > maxbr+2)
		mvall = maxbr+2;
          /* first ply gets 2 extra moves */
	for (k = 0; k < mvall; ++k) {
		best = -1000;
		num = 0;
		for (j = 0; j < mvp; ++j) {
			koflag = FALSE;  /* check to see if ko is first move */
			if (k == 0 && mvp > 1 && mmov[j] != PASS && lnbn[mmov[j]] == 0 &&
				lnbf[mmov[j]][tm] == 0 && mval[j] > 16) {
				for (gptr = nbgrp[mmov[j]][1-tm]; gptr != EOL; gptr = link[gptr]) {
			  		if (grsize[list[gptr]] == 1 && grlibs[list[gptr]] == 1)
					  	koflag++;
	  				if (grsize[list[gptr]] > 1 && grlibs[list[gptr]] == 1) {
				  		koflag = FALSE;
					  	break;
					}
	  			}
	  		}
			if (mval[j] > best && koflag != 1) {  /* don't allow ko for best move */
				best = mval[j];
				num = j;
			}
		}
		if (k == 0)bestval = mval[num];
		if (k == 1 && mval[num] > bestval)mval[num] = bestval;  /* if best was ko */
		if (k == 1)diff = bestval - mval[num];
		if (mval[num] < mintacval[playlevel] && k != 0 ||
			mval[num] <= -1000 ||
			mval[num] < 80 && 
			mval[num] < bestval-maxtacdiff[playlevel] && 
			mmov[num] != PASS ||
		    ply > maxbranchdepth[playlevel] && 
		    bestval > mval[num]+40 &&
		      (grlibs[gs] > 3 || 
			   mval[num] < 64) ||
			k > 2 && grlibs[gs] > 2 && bestval > (maxlibs+2) * 32 ) {
#ifdef CHECK
			if (debug == 2000 + (unsigned int)g) {
				sprintf(buf,"not genning val %d %s\n",mval[num],ssqr(mmov[num],tmpstr));
				outerr(buf);
				}
			mval[num] = -2000;
			continue;  /* so can see all of the ungenerated moves */
#endif
			break;
		}
#ifdef CHECK
		if (debug == 2000+(unsigned int)g || debug == 200) {
			sprintf(buf,"%s generating val %d %s\n",name,mval[num],ssqr(mmov[num],tmpstr));
			outerr(buf);
		}
#endif
		mval[num] = -2000;
		if (mmov[num] == PASS)
			havepass = FALSE;
		mvs[mvptr] = mmov[num];
		mvcolor[mvptr] = tm;
		++mvptr;
	}
#ifdef CHECK
	if (mvp > mvall && (debug == 2000+(unsigned int)g || debug == 200)) {
		outerr("Out of moves - not generating ");
		for (k = 0; k < mvp; ++k)
			if (mval[k] > -1000) {
				sprintf(buf,":%d %s",mval[k],ssqr(mmov[k],tmpstr));
				outerr(buf);
				}
		outerr("\n");
	}
#endif
	if (havepass) {  /* generate a pass if one was generated, or two eyed group may die */
		mvs[mvptr] = PASS;
		mvcolor[mvptr] = tm;
		++mvptr;
#ifdef CHECK
		if (debug == 2000+(unsigned int)g || debug == 200) {
			sprintf(buf,"%s generating a pass\n",name);
			outerr(buf);
		}
#endif
	}
	eply[ply] = mvptr;
	if (eply[ply] - sply[ply] > 1) {	  /* 11/96 changed ratios to give more nodes to first branch */
		if (maxnply[ply] - numnn >= 20 && (diff < 64 || maxnply[ply] > 30)) {
			if (diff <= bestval/4 && maxnply[ply]-numnn >= 30)
				maxnply[ply+1] = numnn + ((maxnply[ply]-numnn)*(ply+2))/(ply+4);  /* 1/2 of rest */
			else if (diff <= 32 && maxnply[ply]-numnn > 40)  /* 3/4 of rest */
				maxnply[ply+1] = numnn + ((maxnply[ply]-numnn)*(ply+3))/(ply+4);
			else if (diff <= 48 && maxnply[ply]-numnn > 20)  /* leave 20 */
				maxnply[ply+1] = maxnply[ply] - 5;
			else
				maxnply[ply+1] = maxnply[ply] - 3;
		}
	}
#ifdef CHECK
	if (debug == 2000+(unsigned int)g && eply[ply] == sply[ply]) {
		outerr("Generated no moves\n");
	}
#endif	
#ifdef NEVER
	if (debug == 2000+(unsigned int)g) {
		outerr("\n");
	}
#endif	
}

#ifdef CHECK
char tmpstr[10];
#endif
   

/* generate general attacking moves scores are 32 * number of liberties group
 * ends up with if I fill a different liberty and he plays here.
 * gs is group being attacked.  g is just for debug.
 */


static void genrestatk(int ply, sqr_t starts, group_t g, int tm, group_t gs, listval_t ldrno, int maxlibs, int winsko) {
	list_t lptr,lptr2,ptr,ptr2,ptr3,lptr3,nptr,nptr2,nptr3,gptr,ngptr,ngptr2;
	int flag,numextend = 0,numlibmoves,maxefflibs;
	list_t extendlist = EOL,cutgroups = EOL;
	sqr_t s,sn,sn2,s1,s2,sapproach,sopen;
	int i,j,ldtmp,libamt,sl,nl;
	group_t grl, grp;
	int val;
	int sumlibs;  /* number of liberties of the attacking stone played */
	int morelibs;
	int numlibs,nbflag,c,mvval,captureflag,libval,madethrowin,tryseki;
	int canatari,cancapture,canpush,best = NOSQUARE,max,defok,newlibs;
	int cantstop = FALSE,oldmvp, cancap;
	unsigned int throwinok;  /* ok to make 1 liberty group to throw in */
	list_t tmplist = EOL, liblist = EOL, newptr = EOL, sumptr = EOL;
	list_t conngroups = EOL;  /* groups connected to solidly */
	int cancut, connlibs;
	int makesnapback;
	ASSERT(list[EOL] == 0xffff);

	ply = ply;
	starts = starts;
	oldmvp = mvp;
	c = grcolor[gs];

   /* attack from a distance (commented out)
   
   if (ply <= 2)
      for (lptr = grlbp[gs]; lptr != EOL;
          lptr = link[lptr]) {
         s = list[lptr];
         if (edge[s] > 1 && lnbn[s] > 2 (need more stuff here)) {
            i = fdir[s];
            for (ldtmp = ldir[i]; i < ldtmp; ++i) {
               sn = s + nbr[i];
               if (board[sn] == NOGROUP) {
                  flag = TRUE;
                  j = fdir[sn];
                  for (ldtm2 = ldir[j]; j < ldtm2; ++j)
                     if (board[sn+nbr[j]] == gs)flag = FALSE;
                  if (flag) {
                     mmov[mvp] = sn;
                     mval[mvp] = 64;
                     ++mvp;
                     }
                  }
               }
            }
         }

   */
	/* attack from a distance, just to prevent bamboo joint */
	if (ply <= 2)
		for (lptr = grlbp[gs]; lptr != EOL; lptr = link[lptr]) {
			s = list[lptr];
			if (link[nbgrp[s][c]] == EOL)continue;  /* not a connection point */
            i = fdir[s];
            for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				sn = s + nbr[i];
				if (S_COLOR(sn) == c && S_GROUP(sn) != gs) {
					grl = S_GROUP(sn);
					if (grlibs[grl] + grlibs[gs] > 6)
						break;	/* have to cut, no break bamboo */
					for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
						if (list[ptr] + nbr[i] != s && 
							board[list[ptr]+nbr[i]] == NOGROUP &&
							inlist(list[ptr], &grlbp[gs]) && 
							inlist((listval_t)(list[ptr] + nbr[i]), &grlbp[grl])) {
							mmov[mvp] = list[ptr] + nbr[i];
							mval[mvp++] = 64;
							}
					}
				}
			}

	cpylist(grlbp[gs],&liblist);
	for (lptr = grlbp[gs]; lptr != EOL; lptr = link[lptr])
#ifdef NEVER
		if (lnbf[list[lptr]][1-c] == 0 && lnbn[list[lptr]] <= 1 ||
			lnbf[list[lptr]][1-c] == 1 && lnbn[list[lptr]] == 0 &&
			   grlibs[list[nbgrp[list[lptr]][1-c]]] <= 2) { /*connection*/
#endif
		if (solidconnect(list[lptr], c, winsko)) {
			for (ngptr = nbgrp[list[lptr]][c]; ngptr != EOL; ngptr = link[ngptr]) {
				grp = (group_t)list[ngptr];	
				if (grp != gs && grlibs[grp] + grlibs[gs] - 2 < maxlibs) {
			   		addlist(grp,&conngroups);
				   mrglist(grlbp[grp],&liblist);  /* add libs of connected group */
				   if (grlibs[grp] == 2) {
				   		for (lptr2 = grlbp[grp]; lptr2 != EOL; lptr2 = link[lptr2])
				   			if (list[lptr2] != list[lptr] && lnbn[list[lptr2]] <= 1 && lnbf[list[lptr2]][1-c] == 0) {
				   				for (ngptr2 = nbgrp[list[lptr2]][c]; ngptr2 != EOL; ngptr2 = link[ngptr2]) {
									if ((group_t)list[ngptr2] != grp &&
				   						grlibs[list[ngptr2]] == 2)
				   						mrglist(grlbp[list[ngptr2]],&liblist);
				   						/* libs of connected to connected group */
				   					}
				   				}
			   			}
			   		}
			   }
		   }

   /* play next to group chased (or closely connected), take away one liberty */
    madethrowin = FALSE;
    numlibmoves = cntlist(&liblist); /* liberties of this and connected groups */
	for (lptr = liblist; lptr != EOL; lptr = link[lptr]) {
		s = list[lptr];
		if (grlibs[gs] == 1 && s == list[grlbp[gs]] && snapback(gs, 1-grcolor[gs]))
			continue;	/* don't make snapback capture */
		sumlibs = lnbn[s];  /* number of friendly liberties end up with for the stone played */
		cpylist(nblbp[s],&sumptr);
		cpylist(liblist,&newptr);
		newlibs = numlibmoves;  /* number of total liberties enemy gets by playing here */
		connlibs = 0;
		cancut = FALSE;
		i = fdir[s];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = s + nbr[i];
			if (grcolor[board[sn]] == c) {  /* connection */
				connlibs += mrglist(grlbp[board[sn]],&newptr);
				if (grlibs[board[sn]] == 1)
					cancut = TRUE;  /* capture enemy to cut */
				}
			if (grcolor[board[sn]] == 1-c) {
				sumlibs += mrglist(grlbp[board[sn]],&sumptr);
				if (grlibs[board[sn]] + lnbn[s] >= 3)
					cancut = TRUE;  /* extend from friend to cut */
				}
	    	}
		if (lnbn[s] >= 2)
			cancut = TRUE;
	    sumlibs--;
	    killist(&sumptr);
		/* check if can really cut here, and not add so much if cut is captured */
		if (cancut)
			newlibs += connlibs;
		newlibs += newlist(newptr,nblbp[s]);
		newlibs -= 2;  /* number of liberties group would have if I fill a different liberty and he moves here. */

		mvval = newlibs * 32;
		if (cancut && grlibs[gs] >= 3 && sumlibs < grlibs[gs])
			mvval -= 32;	/* if end up with fewer liberties after cutting, might not be so good as it seems */
		if (grlibs[gs] == 1 && s == list[grlbp[gs]])
			mvval = 50;
		if (sumlibs > 1 && grlibs[gs] == 2 && (lnbn[list[lptr]] > 1 || lnbf[list[lptr]][1-c])) {  /* can atari here withot immediate capture */
			if (inlist(list[lptr],&grlbp[gs]))
				mvval += 32;  /* go for the atari first */
			else mvval -= 32; /* atari will force fill of two liberties */
			}
		if (grlibs[gs] == 2 && !inlist(s, &grlbp[gs]))
			mvval -= 32;	/* prefer to atari group than play at neighbor */
		throwinok = FALSE;
		canpush = FALSE;
		tryseki = captureflag = canatari = FALSE;
      
		if (lnbn[s] == 0) {
			for (nptr = nbgrp[s][1-tm]; nptr != EOL; nptr = link[nptr]) {
				if (grlibs[list[nptr]] == 1)captureflag++; /* capture enemy */
				if (grlibs[list[nptr]] == 2)canatari = TRUE;
				if (grlibs[gs] > 2 && grlibs[list[nptr]] <= grlibs[gs] &&
					comlist(grlbp[gs],grlbp[list[nptr]]) >= 2)
					tryseki = TRUE;
            	}

			/* see if need to connect before filling liberty */
			if (sumlibs == 1 && captureflag == FALSE) {
				if (newlibs > maxlibs && grlibs[gs] > 2 &&
					(grlibs[gs] > 2 || inlist(s,&grlbp[gs])))cantstop = TRUE;
				sapproach = NOSQUARE;
				for (nptr = nbgrp[s][tm]; nptr != EOL; nptr = link[nptr]) {
					if (grlibs[list[nptr]] == 2) {  /* need an approach move here */
						for (nptr2 = grnbp[list[nptr]]; nptr2 != EOL; nptr2 = link[nptr2]) {  /* see if can capture to get more liberties */
							if (grlibs[list[nptr2]] == 1) {
								mval[mvp] = (grlibs[gs]-1)*32 + grsize[list[nptr2]]*8;
								mmov[mvp++] = list[grlbp[list[nptr2]]];
								}
							}
						sn2 = list[grlbp[list[nptr]]];
						if (sn2 == s)
							sn2 = list[link[grlbp[list[nptr]]]];
						if (sn2 == sapproach)continue;
						if (lnbn[sn2] > 1) {
							mval[mvp] = (grlibs[gs]-1)*32;
							mmov[mvp++] = sn2;
							sapproach = sn2;
							}
						else if (lnbf[sn2][tm] > 1) {
							for (nptr2 = nbgrp[sn2][tm]; nptr2 != EOL; nptr2 = link[nptr2]) {
								if (list[nptr2] != list[nptr]) { /* group to connect to */
									if (grlibs[list[nptr2]] > 2 || cntlist(&nbgrp[sn2][tm]) > 2) {
										mval[mvp] = (grlibs[gs]-1)*32 + grlibs[list[nptr2]] * 8;   
										mmov[mvp++] = sn2;
										sapproach = sn2;
										}
									else {  /* get more liberties for group to connect to */
										for (nptr3 = grnbp[list[nptr2]]; nptr3 != EOL; nptr3 = link[nptr3]) {
											if (grlibs[list[nptr3]] == 1) {
												mval[mvp] = (grlibs[gs]-1)*16;
												mmov[mvp++] = list[grlbp[list[nptr3]]];
												}
											}
										if (grlibs[list[nptr2]] == 2 && lnbf[sn2][1-c]) {  /* 4/01 can atari nbr to force fill of liberty */
											for (nptr3 = nbgrp[sn2][1-c]; nptr3 != EOL; nptr3 = link[nptr3]) {
												if (grlibs[list[nptr3]] == 2 && inlist(s, &grlbp[list[nptr3]])) {
													mval[mvp] = (grlibs[gs]-1)*32 + grlibs[list[nptr2]] * 8 + 8;   
													mmov[mvp++] = sn2;
													sapproach = sn2;
													}
												}
											}
										}
									}
								}
							}
						else if (lnbn[sn2] == 1 && !inlist(list[nblbp[sn2]], &liblist) && lnbf[list[nblbp[sn2]]][tm]) {
							mval[mvp] = (grlibs[gs]-1)*32 - 8;
							mmov[mvp++] = sn2;
							sapproach = sn2;
							}
						else if (grsize[list[nptr]] <= 4)throwinok = TRUE;  /* only throwin to fill eyespace */
						}
	    			}
				}
	 

			if (!tryseki && !captureflag && !throwinok && (sumlibs <= 1 || sumlibs == 2 &&
				grlibs[gs] > 2 &&
				!canatari)) {
				if (newlibs > maxlibs && grlibs[gs] > 2 &&
					(grlibs[gs] > 3 || inlist(s,&grlbp[gs])))cantstop = TRUE;
#ifdef CHECK
				if (debug == 2000+(unsigned int)g || debug == 200) {
					outerr("not genning ill/bad liberty filling move ");
					outerr(ssqr(s,tmpstr));
					outerr("\n");
					}
#endif
				killist(&newptr);
				continue;
				}
			}  /* end of lnbn == 0 */

		else if (lnbn[s] == 1) {
			sopen = list[nblbp[s]];
			i = fdir[s];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				sn = s + nbr[i];
				if (grcolor[board[sn]] == c &&
					grlibs[board[sn]] == 2 && throwinok == FALSE &&
					(lnbn[sopen] <= 1 || 
					comlist(nblbp[sopen],newptr) == lnbn[sopen])) {
						throwinok = TRUE;
						mvval += 32;  /* try throwin early if atari */
					}
				else if (grcolor[board[sn]] == c &&
					grlibs[board[sn]] == 2 && throwinok == FALSE && 
					 lnbn[sopen] == 2 && edge[sopen] != 0 && lnbf[sopen][c] == 0 &&
					 board[sopen+sopen-s] == NOGROUP && lnbf[sopen+sopen-s][c] == 0 &&
					 (lnbn[sopen+sopen-s] > 1 || lnbf[sopen+sopen-s][1-c])) {  /* can make a ko to kill the liberty */
						throwinok = TRUE;
						mvval += 8;  /* try throwin early if atari, not much since is only ko */
						}
				else if (board[sn] != gs && grcolor[board[sn]] == c && grlibs[board[sn]] > 2 &&
					throwinok == FALSE && lnbn[sopen] == 1 &&
					comlist(grlbp[gs], grlbp[board[sn]]) == 1) {
					throwinok = TRUE;
					mvval += 16;  /* throwin is cut */
					}
				else if (grcolor[board[sn]] == c && grlibs[board[sn]] == 1)
					captureflag = TRUE;
				else if (grcolor[board[sn]] == 1-c) {
					if (grlibs[board[sn]] > 1)
						canpush = TRUE;
               		}
				}
			if (!canpush) {
#ifdef NEVER
				if (newlibs > maxlibs && grlibs[gs] > 2 && /* not for throwins */
					(grlibs[gs] > 2 || inlist(s,&grlbp[gs])))cantstop = TRUE;
#endif
				if (!inlist(list[nblbp[s]],&grlbp[gs]) && !lnbf[s][1-c]) {
					mval[mvp] = (grlibs[gs]-1) * 32;
					if (mval[mvp] < mvval/2)mval[mvp] += mvval/2;  /* this can't be all bad compared to throwin */
					mmov[mvp] = list[nblbp[s]];
					++mvp; /* move next to one lib spot */
					}
				for (nptr = nbgrp[s][1-c]; nptr != EOL; nptr = link[nptr])
					if (grlibs[list[nptr]] == 1 &&
						!inlist(list[nptr],&grnbp[gs])) { /* low lib neighbors handled later */
						for (nptr2 = grnbp[list[nptr]]; nptr2 != EOL; nptr2 = link[nptr2])
							if (grlibs[list[nptr2]] == 1) {
								mval[mvp] = mvval+8;
								mmov[mvp] = list[grlbp[list[nptr2]]];
								++mvp;  /* capture neighbor to get another liberty */
								}
						}
				if (!throwinok && !captureflag) { /* don't play here! */
					if (newlibs > maxlibs && grlibs[gs] > 2 &&
						(grlibs[gs] > 3 || inlist(s,&grlbp[gs])))cantstop = TRUE;
#ifdef CHECK
					if (debug == 2000+(unsigned int)g || debug == 200) {
						outerr("(2) not genning ill/bad liberty filling move ");
						outerr(ssqr(s,tmpstr));
						outerr("\n");
						}
#endif
					killist(&newptr);
					continue;
					}
				else madethrowin = TRUE;
				}
			}
		else   /* lnbn > 1 */
			mvval += 16;

		mmov[mvp] = s;
		mval[mvp] = mvval;
		if (edge[s] == 0)mval[mvp] -= 8;
		if (edge[s] == 1) {
			mval[mvp] -= 4;
			if (lnbn[s] == 2) {
				s1 = list[nblbp[s]];
				s2 = list[link[nblbp[s]]];
				if (lnbf[s1][1-c] != 0 && lnbf[s2][1-c] != 0)
					mval[mvp] += 16;
				}
			}
		if (edge[s] == 2)
			mval[mvp] -= 2;  /* so will force to edge */

		if (lnbf[s][1-c] != 0)mval[mvp] += 8;  /* play from friendly stone */
		flag = 0;
		i = fdir[s];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = s + nbr[i];
			grp = board[sn];
			if (grp == NOGROUP && 
				lnbf[sn][1-c] == 0 &&
				lnbf[sn][c] != 0) {  /* nearby friendly stone */
				mval[mvp] += 8;
				for (ptr = nbgrp[sn][c]; ptr != EOL; ptr = link[ptr]) {
					if (grlibs[list[ptr]] < 5) /* play from group with fewer liberties */
						mval[mvp] += 8-grlibs[list[ptr]]*2;
					}
				}
			if (grp != NOGROUP) {
				if (grcolor[grp] == tm) {  /* low lib count neighbor */
					sl = sumlibs;
					if (sl > grlibs[grp] + 2)
						sl = grlibs[grp] + 2;
					if (grlibs[grp] <= grlibs[gs] && sl > grlibs[grp]) {
						if (sl >= grlibs[gs])
							mval[mvp] += (grlibs[gs]+1-grlibs[grp])*8;
						mval[mvp] += (sl-grlibs[grp])*8;
						if (grlibs[grp] == 1) {
							mval[mvp] += (grsize[grp]-1)*32;
							for (ptr = grnbp[grp]; ptr != EOL; ptr = link[ptr])
								if ((group_t)list[ptr] != gs)
									mval[mvp] += grlibs[list[ptr]]*32;
							}
						}
					else if (sl > grlibs[grp] && grlibs[grp] < 5)
						mval[mvp] += (sl-grlibs[grp])*8;
					mval[mvp] -= 2 + grlibs[grp];
					}
				else if (grp != gs && !throwinok && !inlist(grp,&conngroups)) {	/* cut */
					if (grlibs[grp] == 2)
						mval[mvp] += 64;  /* cut is atari */
					else if (grlibs[grp] < grlibs[gs])
						mval[mvp] += 32;  /* cut attacks */
					if (grlibs[grp] == 3 || grlibs[grp] == 2)
						addlist(grp,&cutgroups);
					}
				}
				
			else if (!throwinok && grlibs[gs] > 2 && /* don't worry about long distance if can atari gs */
				lnbf[sn][c] != 0 && !inlist(sn,&grlbp[gs])) { /* long distance cut */
				if (cancut)
					mval[mvp] += 8;
				else {
					mval[mvp] += 16;
					if (lnbf[sn][1-c] == 0)mval[mvp] += 16;
					if (lnbf[sn][1-c] == 0 && lnbn[sn] <= 2)mval[mvp] += 32; /* cut */
					}
				}


			else if (lnbn[sn] == 4 || lnbn[sn] == 3 && lnbf[sn][c] == 0) {
				flag++;
				if (flag == 2) {
					if (addlist(s,&extendlist))
						numextend++;
					}
				}
			}  /* end of for loop */
			
		if (flag > 1)
			mval[mvp] += 8+(flag-2)*16;  /* can't stop from making more liberties if
                              plays here */
		++mvp;
		if (edge[s] == 1 && lnbn[s] == 2) {
			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
				if (lnbf[list[ptr]][1-c] != 0 && lnbn[list[ptr]] > 1) {
					mval[mvp] = mval[mvp-1]-16;
					mmov[mvp] = list[ptr];
					mvp++;
					}
			}
		killist(&newptr);
		}  /* end of fill a liberty */

    killist(&conngroups);
	if (cantstop && !madethrowin) {  /* don't generate any moves if none can work */
		mvp = oldmvp;
#ifdef CHECK
		if (debug == 2000+(unsigned int)g || debug == 200) {
			outerr("genatkmoves: no move can work\n");
			}
#endif
		killist(&extendlist);
		killist(&cutgroups);
		killist(&liblist);
		return;
		}

	/* fill liberty of adjacent group that he can cannect to */             
	for (ptr = cutgroups; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[gs] + grlibs[list[ptr]] - 2 > 4)continue;
		for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (inlist(list[ptr2],&liblist))continue;  /* already counted */
			if (lnbn[list[ptr2]] <= 1 && lnbf[list[ptr2]][1-c] == 0)
				continue;  /* no throwins here yet */
			if (link[nbgrp[list[ptr2]][c]] == EOL)continue;  /* looking for cut */     		
			mval[mvp] = (numlibmoves-2) * 32;
			mmov[mvp] = list[ptr2];
			for (ptr3 = nbgrp[list[ptr2]][c]; ptr3 != EOL; ptr3 = link[ptr3]) {
				if (list[ptr3] == list[ptr])continue;
				if (grlibs[list[ptr3]] == 2)mval[mvp] += 64;  /* atari */
				if (grlibs[list[ptr3]] == 3)mval[mvp] += 32;
				mvp++;
				break;
				}
				
			}
		}        
	killist(&cutgroups);
   	killist(&liblist);

	/* try for loose ladder */
	if (!cantstop && (grlibs[gs] == 2 || grlibs[gs] == 3) && numextend == 2 &&
		((signed)list[link[extendlist]]-(signed)list[extendlist] == boardsize+1 ||
		 (signed)list[link[extendlist]]-(signed)list[extendlist] == boardsize-1)) {
		andlist(nblbp[list[extendlist]],nblbp[list[link[extendlist]]],
			&tmplist);
		if (tmplist != EOL && edge[list[tmplist]] > 1) {
			mval[mvp] = 110;
			mmov[mvp] = list[tmplist];
			for (ptr = extendlist; ptr != EOL; ptr = link[ptr])
				if (lnbf[list[ptr]][1-c]) {
					mval[mvp] += 8;
					if (grlibs[list[nbgrp[list[ptr]][1-c]]] >= 4)
						mval[mvp] += 8;
					}
			mvp++;
			}
		killist(&tmplist);
		}
	killist(&extendlist);

	/* try for bigger snapback */
	if (grlibs[gs] == 1) {
		makesnapback = FALSE;
		s = list[grlbp[gs]];
		if (lnbn[s] == 1 && link[nbgrp[s][c]] == EOL && lnbf[s][1-c] == 1) {
			grl = (group_t)list[nbgrp[s][1-c]];	/* friendly group to be captured by snapback */
			sn = list[nblbp[s]];
			if (grlibs[grl] == 1 && grsize[grl] == 1 && lnbn[sn] > 1) {
				makesnapback = TRUE;
				for (ptr = grnbp[gs]; ptr != EOL; ptr = link[ptr]) {
					if (list[ptr] != grl && grlibs[list[ptr]] == 1)
						makesnapback = FALSE;	/* can make a different capture */
					}
				if (makesnapback) {
					mval[mvp] = 45;
					mmov[mvp] = sn;
					mvp++;
					}
				}
			}
		}

   /*  defend nbrs with fewer libs */
	libamt = grlibs[gs]-1;
	if (grlibs[gs] == 1 && lnbn[list[grlbp[gs]]] <= 1)
		libamt = 1;	/* 11/02 one liberty groupo can't get more liberties, so try to defend 1 liberty neighbors */
	if (!cantstop && libamt > 0) {
		for (lptr = grnbp[gs]; lptr != EOL; lptr = link[lptr]) {
			if (grlibs[list[lptr]] <= libamt && (grlibs[list[lptr]] == 1 ||
				getefflibs((group_t)list[lptr],libamt,ldrno,&maxefflibs) <= libamt)) {
				captureflag = FALSE;  /* can grl be captured */
				morelibs = FALSE;   /* can grl get more liberties */
				grl = (group_t)list[lptr];  /* found a group */
				val = 16 * grsize[grl];  /* value of defense of grl */
				if (val > 96)val = 96;
				if (grlibs[grl] == 1) {
					val = 32 * lnbn[list[grlbp[grl]]] + (grlibs[gs]-1)*32 - 32;
					val += 32*grsize[grl];
					for (ptr = grnbp[grl]; ptr != EOL; ptr = link[ptr]) /* connections */
						if (list[ptr] != gs)val += 32 * grlibs[list[ptr]];
					for (ptr = nblbp[list[grlbp[grl]]]; ptr != EOL; ptr = link[ptr])  /* more connections */
						if (lnbn[list[ptr]] <= 2 && lnbf[list[ptr]][1-c] == 0) {
							j = fdir[list[ptr]];
							for (ldtmp = ldir[j]; j < ldtmp; ++j) {
								sn = list[ptr]+nbr[j];
								if (board[sn] != NOGROUP && board[sn] != gs)
									val += 32 * (grlibs[board[sn]]-1);
								}
							}
					if (grsize[grl] == 1 && lnbn[list[grlbp[grl]]] <= 1 && val > 256)
						val = 256;  /* squeeze, so allow other moves */
					for (ptr = grnbp[grl]; ptr != EOL; ptr = link[ptr]) {
						if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]])) {
							adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
							}
						if (grlibs[list[ptr]] == 1 &&  /* capture one liberty neighbor */
							list[grlbp[list[ptr]]] != list[grlbp[grl]]) {
							mval[mvp] = val + 16 * (grsize[list[ptr]]-1);
							mmov[mvp++] = list[grlbp[list[ptr]]];
							}
						}
					if (winsko == tm && grsize[grl] == 1 && lnbf[list[grlbp[grl]]][1-grcolor[grl]] == 0 &&
						lnbn[list[grlbp[grl]]] == 1) {	/* make a ko */
						mval[mvp] = val;
						mmov[mvp++] = list[nblbp[list[grlbp[grl]]]];
						}
					if (inlist(list[grlbp[grl]],&grlbp[gs]))
						continue;  /* already handled by play in liberty */
					mval[mvp] = val;
					mmov[mvp++] = list[grlbp[grl]];
					continue;
#ifdef NEVER
					morelibs = lnbn[list[grlbp[grl]]] > 1 ||
						lnbf[list[grlbp[grl]]][1-c] > 1;
#endif
					}
				else if (grlibs[grl] == 2 && grlibs[gs] > 2) {
					canatari = FALSE;
					for (j = grlbp[grl]; j != EOL; j = link[j]) {
						if (!captureflag && (lnbn[list[j]] <= 1 || lnbn[list[j]] == 2 &&
							edge[list[j]] <= 1) && lnbf[list[j]][1-c] == 1) {
							val = 32 * grsize[grl] + (grlibs[gs]-1)*32 + 32;
							if (val > 96)val = 96;
							captureflag = TRUE;
							}
						if (lnbn[list[j]] > 1 || lnbf[list[j]][c] != 0)
							canatari = TRUE;
						if (lnbf[list[j]][grcolor[grl]] > 1 ||
							lnbn[list[j]] - comlist(nblbp[list[j]],grlbp[grl]) > 1)
							morelibs = TRUE;
						}
					if (!canatari)continue;  /* no point in defending! */
					else if (!captureflag)val += 16;
					}
				for (j = grnbp[grl]; j != EOL; j = link[j])
					if ((group_t)list[j] != gs) {	/* cutting stones */
						if (captureflag)
							val += grlibs[list[j]]*32;
						else if (grlibs[list[j]] < 5)
							val += 8 * grlibs[list[j]] - 8;
						else {
							val += 48;
							break;
							}
						if (val > 128) {
							val = 128;
							break;
							}
						}


				/* move in liberty of group to defend */
				/* only try best move, not all moves */
				flag = FALSE;
				defok = FALSE;
				mval[mvp] = 0;
				for (lptr2 = grlbp[grl]; lptr2 != EOL;
					lptr2 = link[lptr2]) { /* play next to grl */
					if (!captureflag &&  /* 5/02 if group can be captured, include this value anyway */
						inlist(list[lptr2],&grlbp[gs]))continue;  /* already handled by play in liberty */
					cancapture = FALSE; /* does move capture opponent */
					cpylist(grlbp[grl],&tmplist);
					numlibs = mrglist(nblbp[list[lptr2]],&tmplist)-1;
					/* number of new liberties from move */
					nbflag = FALSE;
					for (ptr = nbgrp[list[lptr2]][tm]; ptr != EOL; ptr = link[ptr])
						if (list[ptr] != grl) {
							numlibs += mrglist(grlbp[list[ptr]],&tmplist);
							}
						
					for (ptr = nbgrp[list[lptr2]][1-tm]; ptr != EOL; ptr = link[ptr]) {
						if (grlibs[list[ptr]] == 1) {
							cancapture = TRUE;
							numlibs += grsize[list[ptr]];
							for (ptr3 = grnbp[list[ptr]]; ptr3 != EOL; ptr3 = link[ptr3])
								if (list[ptr3] != grl)
									numlibs += mrglist(grlbp[list[ptr]],&tmplist);
							}
						else if (grlibs[list[ptr]] == 2 && numlibs > 0)  /* extra for atari */
							numlibs += 2;  /* crude approximation */
						nbflag = TRUE;
						}
#ifdef NEVER
					replaced by above code
					j = fdir[list[lptr2]];
					for (ldtmp = ldir[j]; j < ldtmp; ++j) {
						sn = list[lptr2]+nbr[j];
						if (grcolor[board[sn]] == tm) {
							if (board[sn] != grl) {
								numlibs += mrglist(grlbp[board[sn]],&tmplist);
								}
							}
						else if (board[sn] != NOGROUP) {
							if (grlibs[board[sn]] == 1) {
								cancapture = TRUE;
								numlibs += grsize[board[sn]];
								for (ptr3 = grnbp[board[sn]]; ptr3 != EOL; ptr3 = link[ptr3])
									if (list[ptr3] != grl)
										numlibs += mrglist(grlbp[board[sn]],&tmplist);
								}
							else if (grlibs[board[sn]] == 2)  /* extra for atari */
								numlibs += 2;  /* crude approximation */
							nbflag = TRUE;
							}
						}
#endif
					if (tmplist != EOL)killist(&tmplist);

	                   /* don't generate moves that opponent wouldn't make
			                     anyway */
					if (numlibs + grlibs[grl] <= 1 ||	/* 6/03 don't sac group - what bout throwins? */
						!cancapture && (lnbf[list[lptr2]][tm] == 0 && 
						lnbn[list[lptr2]] <= 1 || grlibs[grl] + numlibs <= 1) ) {
						if (grlibs[grl] == 1 && val >= (maxlibs+1)*32)
						/* cantstop = TRUE; liberty moves may be ok */
#ifdef CHECK
						if (debug == 2000+(unsigned int)g || debug == 200) {
							outerr("not genning bad rescuing move ");
							outerr(ssqr(list[lptr2],tmpstr));
							outerr("\n");
							if (cantstop)outerr("no move can work!\n");
							}
#endif
						continue;
						}
					libval = val + 8*numlibs + 16*nbflag;
					if (numlibs < 1 && val <= 96 || !morelibs)libval -= val; 
					if (libval > mval[mvp]) {
						mval[mvp] = libval;
						mmov[mvp] = list[lptr2];
						flag = TRUE;
						defok = grlibs[grl]+numlibs > libamt + 1 && grlibs[grl]+numlibs > 2;
						}
					}
				if (flag) {
					if (grlibs[grl] > 1 && lnbn[mmov[mvp]] <= 1 &&
						lnbf[mmov[mvp]][c] == 0 && mval[mvp] > 28)
						mval[mvp] = 28;
					++mvp;
					}


				if (!defok)  /* only need to capture if can't escape elsewhere */
				  	for (lptr2 = grnbp[grl]; lptr2 != EOL;
						lptr2 = link[lptr2]) {  /* capture nbr of nbr to defend nbr */
						if (grlibs[list[lptr2]] == 1) {
							numlibs = grsize[list[lptr2]]+lnbn[list[grlbp[list[lptr2]]]];
							for (ptr3 = grnbp[list[lptr2]]; ptr3 != EOL; ptr3 = link[ptr3])
								numlibs += newlist(grlbp[grl],grlbp[list[ptr3]]);
							if (numlibs > 5)
								numlibs = 5;
							mval[mvp] = val + 8*numlibs;
							mmov[mvp] = list[grlbp[list[lptr2]]];
							mvp++;
							}
						else if (grlibs[grl] >= 2 && grlibs[list[lptr2]] == 2) {
							max = -1;
							best = 0;
							for (lptr3 = grlbp[list[lptr2]]; lptr3 != EOL; lptr3 = link[lptr3]) {
								cancap = FALSE;
								nl = lnbn[list[lptr3]];  /* how many liberties does he get here */
								for (gptr = nbgrp[list[lptr3]][c]; gptr != EOL; gptr = link[gptr]) {
									if (list[gptr] != list[lptr2])
										nl += grlibs[list[gptr]] - 1;
									if (grlibs[list[gptr]] == 1)
										cancap = TRUE;
									}
								if (max >= 4 && nl >= 4) {
									max = 0;  /* two ways to escape */
									break;
									}
								if (nl >= 4 && !cancap &&
									(lnbn[list[lptr3]] == 0 ||
									lnbn[list[lptr3]] == 1 && lnbn[list[nblbp[list[lptr3]]]] > 1)) {
									max = 0;	/* can't cut the escape */
									break;
									}
								if (nl > max ||
									nl == max && 
									edge[list[lptr3]] > edge[best]) {
									max = nl;
									best = list[lptr3];
									}
								}
							if (max > 1) {
								mmov[mvp] = best;
								mval[mvp] = val + 16 + max;
								mvp++;
								}
							}
						}
				}
			}
		}
	
	if (cantstop && !madethrowin) {  /* don't generate any moves if none can work */
		mvp = oldmvp;
#ifdef CHECK
		if (debug == 2000+(unsigned int)g || debug == 200) {
			outerr("(2)genatkmoves: no move can work\n");
			}
#endif
		return;
		}
   }

/* find out if a group is alive or dead.
 *
 * group dies if it has one liberty and it is opponents move or
 * group has one liberty and cant get more
 * color is the color of the group being chased
 * winsko is color that wins all kos
 * nodesleft is the number of nodes remaining to be searched in this subtree
 *
 * group lives if it has more than maxlibs liberties or
 * search is more than maxply deep or
 * search is more than maxnodes wide or
 * group has maxlibs libs and can't be prevented from getting another or
 * 
 * return TRUE if this node is a leaf - found an answer, and result contains 1 or -1
 * on a FALSE return, efflibs and maxefflibs have the effective liberties of the group at starts
 */

static int livesordies(sqr_t starts, int maxlibs, int tm, int color, int ply, listval_t ldrno, int *efflibs, int *maxefflibs, group_t gs, int maxply, int *result, int winsko, int nodesleft) {
	group_t g;
	sqr_t s;
	int lbs, ecount, count, fcount, gcount, tcount, tmp, cancapture;
	list_t ptr2, ptr;
	int found;
#ifdef CHECK
	char buf[80];
#endif	

	gs = gs;
	g = board[starts];

	/* restrict small searches to small number of liberties */
	if (nodesleft < 5)
		maxlibs = 2;
	else if (nodesleft < 20 && maxlibs > 3)
		maxlibs = 3;
	if (g == NOGROUP) {
		*result = -1;  /* dead, must be my move... */
#ifdef CHECK
		if (debug == 2000+(unsigned int)gs) {
			outerr("dies-off board\n");
		}
#endif
		return(TRUE);
	}
/* NEED TO CHANGE SO SINGLE STONES THAT CAN SURVIVE SNAPBACK ARE NOT
   ASSUMED TO BE CAPTURED - but large, complex change... */
	if (grlibs[g] == 1 && color != tm &&
		!snapback(g, winsko) && /* 1/99 snapback stones can't be captured */
		(winsko != color || !iskopoint((sqr_t)list[grlbp[g]], &tmp))) {
/*		kosquare != (sqr_t)list[grlbp[g]])) { 10/97 replaced with iskopoint */
/*		 grsize[g] > 1 || lnbf[list[grlbp[g]]][1-color] != 0 ||
		 lnbn[list[grlbp[g]]] != 0) {   dies - adjust for ko and snapback (later)*/
		*result = 1;  /* dead */
#ifdef CHECK
		if (debug == 2000+(unsigned int)gs) {
			outerr("dies-captured\n");
		}
#endif
		return(TRUE);
	}
	/* if chased group wins kos, we want to be certain we kill it, so let it live here if we run out of nodes */
	if (ply > maxply || numnn >= maxnply[ply] && winsko == color || eply[ply-1] > MAXLADDER) {
		*result = -1;  /* lives */
		if (color == tm)*result = 1;
#ifdef CHECK
		if (debug == 2000+(unsigned int)g)
			outerr("Lives-too many ply or nodes\n");
#endif
		return(TRUE);
	}
	count = 0;	/* nbr groups with less libs (attackable) */
	tcount = 0;  /* nbr groups with <=2 liberties */
	fcount = 0;  /* nbr groups with 1 lib*/
	gcount = 0;  /* size of biggest nbr 1 lib group I can capture this move */
	if (tm == color) {
		for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr])
			if (grlibs[g] + lnbn[list[ptr]] - 1 > maxlibs &&
				grlibs[g] + newlist(grlbp[g],nblbp[list[ptr]]) - 1 > maxlibs) {
#ifdef CHECK
				if (debug == 2000+(unsigned int)gs) {
					outerr("lives can get enough liberties\n");
				}
#endif  
				addldrflag(list[ptr], ldrno);
				*result = 1;  /* lives */
				return(TRUE);
			}
	}
	if (tm == color) {
		for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {	/* enemy neighbor groups */
			if (grlibs[list[ptr]] <= grlibs[g]) {
				for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
					if (lnbn[list[ptr2]] > 1) {
						++count;
						if (grlibs[list[ptr]] <= 2)
							++tcount;
						break;
					}
			}
			if (grlibs[list[ptr]] == 1) {
				++fcount;
				if (grsize[list[ptr]] > gcount)gcount = grsize[list[ptr]];
			}
		}
	}
	else if (grlibs[g] > 1) {	/* and color != tm */
		for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {	/* enemy neighbor groups */
			if (grlibs[list[ptr]] == 1 &&  /* find enemy group that can't get away */
				lnbn[list[grlbp[list[ptr]]]] <= 1 &&	/* can't extend for more liberties */
				link[nbgrp[list[grlbp[list[ptr]]]][1-color]] == EOL) { /* no connection */
				found = FALSE;	/* found a one liberty neighbor, so can save the group */
				for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr2]]]])) {
						adflist(mvs[grpieces[list[ptr2]]],&grldr[ldrno]);
					}
					if (grlibs[list[ptr2]] == 1) {
						found = TRUE;
						break;
					}
				}
				if (!found) {
					++fcount;
					if (grsize[list[ptr]] > gcount)gcount = grsize[list[ptr]];
				}
			}
		}
	}

	s = list[grlbp[g]];					/* 1/99 added tm == color */
	if (grlibs[g] == 1 && fcount == 0 && tm == color) {   /* one lib group no one lib nbrs */
		addldrflag(s, ldrno);
		if (lnbn[s] < 2) {   /* cant get more libs */
			cancapture = FALSE;  /* does move capture stone(s)? */
			for (ptr = nbgrp[s][1-color]; ptr != EOL; ptr = link[ptr])
				if (grlibs[list[ptr]] == 1)
					cancapture = TRUE;
			if (!cancapture && lnbf[s][color] == 1 && edge[s] != 0) {  /* maybe ko in corner? */
				*result = -1;  /* dead */
#ifdef CHECK
				if (debug == 2000+(unsigned int)gs) {
					outerr("dies from no lnbn\n");
				}
#endif
				return(TRUE);
			}
		}
#ifdef NEVER
		// This is totally wrong!
		for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
			for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
				g2 = (group_t)list[ptr2];
				if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr2]]]])) {
					adflist(mvs[grpieces[list[ptr2]]],&grldr[ldrno]);
				}
				if (g2 != g && grlibs[g2] == 1 && grsize[g2] > grsize[g]) {
					*result = -1;  /* dead */
#ifdef CHECK
					if (debug == 2000+(unsigned int)gs) {
						outerr("dies of double atari\n");
					}
#endif
					return(TRUE);
				}
			}
		}
#endif
	}      


   if (grlibs[g] > maxlibs || /* has too many libs */
      tcount > 2 || /* too many nbrs can be ataried or captured */
      count > 3 || /* too many nbrs can be attacked */
      fcount > 2 || /* too many nbrs can be captured */
      gcount > 3 && grlibs[g] > 1/*+(tm!=color)*/ || /* since 4 stones captured is a big eye - 5 liberties 3/04 added tm != color for uner stones tesuji */
      gcount+grlibs[g]-1 > maxlibs && grlibs[g] > 1/*+(tm!=color)*/) { /* too big a nbr can be captured 5/02 sub one for capturing move */
         *result = -1;
         if (color == tm)*result = 1;
#ifdef CHECK
         if (debug == 2000+(unsigned int)gs) {
           sprintf(buf,"lives libs %d maxlibs %d\n",grlibs[g], maxlibs);
           outerr(buf);
           sprintf(buf,"fcount, gcount, count, tcount %d %d %d %d\n",
                   fcount,gcount,count,tcount);
           outerr(buf);
          }
#endif
         return(TRUE);  /* lives or dies, search complete here */
   }


   lbs = getefflibs(g,maxlibs,ldrno,maxefflibs);
   *efflibs = lbs;
   ecount = 0;	/* number of libs with 3 libs */
   if (lbs == maxlibs) {
      for (ptr = grlbp[board[starts]]; ptr != EOL; ptr = link[ptr]) {
         if (lnbn[list[ptr]] == 3 && 
			newlist(grlbp[board[starts]],nblbp[list[ptr]]) == 3)++ecount;
      }
   }

   if ( ecount > (tm != color) ||  /* can't stop from getting too many libs*/
      lbs > maxlibs) {  /* has too many libs */
         *result = -1;
         if (color == tm)*result = 1;
#ifdef CHECK
         if (debug == 2000+(unsigned int)gs) {
           sprintf(buf,"lives libs %d\n",lbs);
           outerr(buf);
           sprintf(buf,"ecount, fcount, gcount, count %d %d %d %d\n",
                   ecount,fcount,gcount,count);
           outerr(buf);
         }
#endif
         return(TRUE);  /* lives or dies, search complete here */
      }

   return(FALSE);
}


