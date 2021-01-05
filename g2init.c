/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include <stdlib.h>
#include <time.h>
#include "g2hd.h"
#include "g2tree.h"
#include "g2dist.h"
#include "learn.h"
#include "playout.h"
#include "uct.h"

/* initialization for the g2 go playing engine */

char *ident = "@(#) Copyright 1984-1997 David Fotland.  All rights reserved.";
char *revision = "@(#) Rev 10.2";
#ifdef DEMO
char *release = "Igo";
#else
char *release = "Many Faces of Go, Release 3";
#endif

static void initrtval(void);

unsigned int maxjosbytes;

extern sqr_t brddir[4];
extern int nbri[52],diagsi[52];

extern int linktypes[41];
extern int diffs4[7][3],diffs5[9][4],diffs6[4][5];
extern int diffs4i[7][3],diffs5i[9][4],diffs6i[4][5];


int nbri[52] = { 
   1,19,
   -1,19, 
   1,-19, 
   -1,-19,
   -1,1,19, 
   -19,19,1,
   -19,19,-1, 
   -1,1,-19,
   1,19,-1,-19, 
   -1,19,1,-19, 
   1,-19,-1,19, 
   -1,-19,1,19, 
   -1,1,-19,19, 
   -19,19,-1,1, 
   -19,19,1,-1, 
   -1,1,19,-19   };


int diagsi[52]  = {
	20, 0,
	18, 0,
	-18, 0,
	-20, 0,
	18, 20, 0,
	-18,20, 0,
	-20,18, 0,
	-20,-18, 0,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
};

extern void inittext(void);
extern void initjlib(void);
extern g2status initpatterns(void);

 
// one time initialization of the go engine. 
// return 0 for success, else an error code
g2status initmfgo(char *cwd, int max_memory) 
{
	int ok = G2OK;
	int patok;
	struct learn_stats ls;
	srand((unsigned int)time(NULL));
	po_init_all();
	uct_init_all();
	initjlib();
	if (!learn_init(cwd))	/* read the learned pattern database */
		ok = G2BADFILEOPEN;
	learn_get_stats(&ls);
	/* 1/2 MB init data, 150 KB tree, 200 KB trans table, 1 MB round up, 1 MB misc */
	max_memory -= 3 + ls.mem_used / (1024 * 1024);
	uct_set_memory(max_memory);
	showtac(FALSE);
	initvars(19, 50 * 6 + 25, 1, UCT_MAX_PLAYOUTS);
	fixplaylevel(2);
	patok = initpatterns();
	if (patok != G2OK)
		return patok;
	return ok;
}
 
void initkomi(int k)
{
	komi = k;
	uct_init_komi(komi / 50);
}
	       

/* initailize all constants that change when board size changes */

int oldbsize = -1;

void initbsizeconst(int size) 
{
	register int i;
	sqr_t s;
	int j, x, y, ixmin, iymin, xm, ym, mid;
	if (size < 7)
		size = 7;
	if (size > 19)
	   size = 19;
	if (size == oldbsize)
	   return;
	oldbsize = size;
	boardsize = size;
	boardsquare = boardsize * boardsize;
	if (boardsquare > NUMSQUARES - 1)
		boardsquare = NUMSQUARES - 1;  /* for IGO, where NUMSQUARES is 82 */
	firstsquare = 0;
	lastsquare = boardsquare;
	brddir[0] = -boardsize;
	brddir[3] = boardsize;
	for (i = 0; i < 41; ++i)
		linktypes[i] = 0;
	linktypes[1] = linktypes[boardsize] = 1;
	linktypes[boardsize-2] = linktypes[boardsize+2] =
		linktypes[boardsize*2+1] = linktypes[boardsize*2-1] = 2;
	for (i = 0; i < 7; ++i) {
		for (j = 0; j < 3; ++j) {
			if (diffs4i[i][j] > 10)
				diffs4[i][j] = diffs4i[i][j] - 19 + boardsize;
			else diffs4[i][j] = diffs4i[i][j];
		}
	}
	for (i = 0; i < 17; ++i) {
		for (j = 0; j < 4; ++j) {
			if (diffs5i[i][j] > 10)
				diffs5[i][j] = diffs5i[i][j] - 19 + boardsize;
			else diffs5[i][j] = diffs5i[i][j];
		}
	}
	for (i = 0; i < 4; ++i) {
		for (j = 0; j < 5; ++j) {
			if (diffs6i[i][j] > 10)
				diffs6[i][j] = diffs6i[i][j] - 19 + boardsize;
			else diffs6[i][j] = diffs6i[i][j];
		}
	}
	for (i = 0; i < 52; ++i) {
		if (nbri[i] == 19)
			nbr[i] = boardsize;
		else if (nbri[i] == -19)
			nbr[i] = -boardsize;
		else nbr[i] = nbri[i];
		if (diagsi[i] == -18)
			diags[i] = -boardsize+1;
		else if (diagsi[i] == -20)
			diags[i] = -boardsize-1;
		else if (diagsi[i] == 18)
			diags[i] = boardsize-1;
		else if (diagsi[i] == 20)
			diags[i] = boardsize+1;
	}
 
	edge[NOSQUARE] = edge2[NOSQUARE] = NOEDGE;
	edge[PASS] = edge2[PASS] = NOEDGE;
	for (s = 0; s < boardsquare; s++) {
		x = xval[s] = s%boardsize;
		y = yval[s] = s/boardsize;
		xm = x+1;
		if (boardsize - x < xm)
			xm = boardsize-x;
		ym = y+1;
		if (boardsize - y < ym)
			ym = boardsize-y;
		if (xm < ym) {
			edge[s] = xm;
			edge2[s] = ym;
		}
		else {
			edge[s] = ym;
			edge2[s] = xm;
		}
		if (x == 0) {
			fdir[s] = 11;
		}
		if (x == boardsize-1) {
			fdir[s] = 14;
		}
		if (y == 0) {
			fdir[s] = 8;
		}
		if (y == boardsize-1) {
			fdir[s] = 17;
		}
		if (edge[s] != 1) {     /* fdir and ldir for center */
			ixmin = x;
			iymin = y;
			mid = boardsize/2;
			if (boardsize-1-x < ixmin)
				ixmin = boardsize-1-x;
			if (boardsize-1-y < iymin)
				iymin = boardsize-1-y;
			if (ixmin == iymin) {    /* diagonal */
				if (x <= mid && y <= mid) {
					fdir[s] = 20;
				}
				if (x > mid && y <= mid) {
					fdir[s] = 24;
				}
				if (x <=mid && y > mid) {
					fdir[s] = 28;
				}
				if (x > mid && y > mid) {
					fdir[s] = 32;
				}
			}
			else if (iymin < ixmin) { /* top or bottom */
				if (y < mid) {
					fdir[s] = 48;
				}
				else {
					fdir[s] = 36;
				}
			}
			else {
				if (x < mid) {
					fdir[s] = 44;
				}
				else {
					fdir[s] = 40;
				}
			}
		}
	}
	edge[0] = 0;
	edge[boardsquare-1] = 0;
	edge[boardsize-1] = 0;
	edge[boardsquare-boardsize] = 0;
	edge2[0] = 0;
	edge2[boardsquare-1] = 0;
	edge2[boardsize-1] = 0;
	edge2[boardsquare-boardsize] = 0;
	fdir[0] = 0;
	fdir[boardsquare-1] = 6;
	fdir[boardsize-1] = 2;
	fdir[boardsquare-boardsize] = 4;
	initrtval();
}

# define MINJOSBOARD 17
extern int numcmoves[4];

#ifndef DEMO
extern char josx[4],josy[4];
#endif
	

list_t hcpoints;
extern int maxtreeused, maxlistused;

void initvars(int size, int komi, int max_threads, int max_playouts)
{	/* initialize variables for a new game */
	register int i;
	sqr_t s;
	int j, ldtmp;

	initbsizeconst(size);
	initboardbits(boardsquare);
	initbestresp();
	initkomi(komi);
	uct_init(boardsize, max_threads, max_playouts);

	clearfights();
	initree();
	initdistance();
	learn_init_game(boardsize);

	maxmove = NUMMOVES - NUMPLY;
	maxtreeused = 0;
	maxlistused = 0;
	msptr = 0;
	numnodes = nummoves = 0;
	firstsquare = 0;
	lastsquare = boardsquare;
	nextstrat = 0;
	tscr = 0;
	pscr = 0;
	passscr = 0;
	problemflag = FALSE;
	/*   pass_seq = EOL; */
	xmax = ymax = 0;
	xmin = ymin = boardsize-1;
	for (i = 0; i < EOL; i++) {
		link[i] = i+1;
	}
	link[EOL] = EOL;
	freelist = 0;
	link[NUMLIST-2] = EOL;
	list[EOL] = 0xFFFF;		/* biggest unsigned number */
	newpatfreelist = EOL;
	for (i = NUMNEWPATS-1; i > 0; --i)
		adflist((listval_t)i, &newpatfreelist);
	eyefreelist = EOL;
	for (i = NUMEYERECS-1; i > 0; --i)
		adflist((listval_t)i,&eyefreelist);
	/* keep eye zero out of list */
	for (i = 0; i < NUMEYERECS; i++) {
		eyetype[i] = NOEYE;
		eyeptr[i] = EOL;
		eyevital[i] = EOL;
		/*	eyethreat[i] = EOL; */
		eyeval[i] = 0;
		eyevalmax[i] = 0;
		eyepot[i] = 0;
		eyemin[i] = 0;
	}
	for (s = 0; s < NUMSQUARES; ++s) {
		scoreval[s] = BIGNUM;
		stratgoodreasons[s] = FALSE;
		stratreasons[s] = EOL;
		stratbestpat[s] = 0;
		for (i = 0; i < MAXSEARCHDEPTH; ++i) {
			stratguess[i][s] = 0;
		}
		eyerec[s] = 0;
		eyevitrec[s] = EOL;
		/*	  eyevitval[s][0] = eyevitval[s][1] = EYEVALNONE; */
		bestseq[s] = EOL;
	}
	bestseq[NOSQUARE] = EOL;
	bestseq[PASS] = EOL;
	initjflags();
	cnfreelist = EOL;
	for (i = NUMCONNS-1; i >= 0; --i)
		adflist((listval_t)i,&cnfreelist);
	for (i = 0; i < NUMMOVES; ++i) {
		lbply[i] = nbply[i] = EOL;
	}
	pots[0].pot_val = 0;
	pots[0].pot_max = 0;
	pots[0].pot_type = NOPOT;
	armyfreelist = EOL;
	for (i = NUMARMIES-2; i >= 0; --i)
		adflist((listval_t)i,&armyfreelist);
	for (i = NUMARMIES-1; i >= 0; --i) {
		armyvitalpoints[i] = EOL;
		armygroups[i] = EOL;
		armydeadgroups[i] = EOL;
		armypot[i] = EOL;
		armysize[i] = 0;
		armylibs[i] = 0;
		armylbp[i] = EOL;
		armynbp[i] = EOL;
		armyeyerecs[i] = EOL;
		armyeyes[i] = 0;
		armyeyepotential[i] = 0;
		armyeyespace[i] = 0;
		armyeyespacemax[i] = 0;
		armyminsemeyes[i] = 0;
		armymaxsemeyes[i] = 0;
		armybestpot[i] = 0;
		armybestmove[i] = NOSQUARE;
		armybestpot2[i] = 0;
		armybestmove2[i] = NOSQUARE;
		armysecond[i] = 0;
		armysecondmove[i] = NOSQUARE;
		armykillmove[i] = NOSQUARE;
		armykillmove2[i] = NOSQUARE;
		armykopot[i] = 0;
		armyrest[i] = 0;
		armypthreat[i] = 0;
		armynumrest[i] = 0;
		armyaji[i] = 0;
		for (j = 0; j < NUMRUN; ++j)armyrun[i][j] = EOL;
		armyrn_pot[i] = 0;
		armywk_pot[i] = 0;
	}
	charmy = EOL;
	chalive = EOL;
	lookldr = EOL;
	eyelist = EOL;
	for (i = 0; i < NUMMOVES; ++i) {
		mvs[i] = 0;
		mvconn[i] = mvcapt[i] = EOL;
		mvcolor[i] = 0;
		mvnext[i] = -1;
		kosave[i] = NOSQUARE;
	}
	pclsnext = 0;
	kosquare = NOSQUARE;
	for (s = firstsquare; s < lastsquare; s++) {
		lnbf[s][0] = lnbf[s][1] = 0;
		nbgrp[s][0] = nbgrp[s][1] = EOL;
		lnbn[s] = 4;
		nblbp[s] = EOL;
		ld[s] = NOLD;
		j = fdir[s];
		for (ldtmp = ldir[j]; j < ldtmp; ++j)
			addlist((sqr_t)(s+nbr[j]),&nblbp[s]);
		if (xval[s] == 0 || xval[s] == boardsize-1 ||
			yval[s] == 0 || yval[s] == boardsize-1)
			lnbn[s] = 3;
		board[s] = NOGROUP;
		newpatbrd[s] = EOL;
		groldalive[s] = 0;
		sqoldalprob[s] = 0;
		groldarmy[s] = NOARMY;
		grolddefv[s] = 0;
		groldrun[s] = 0;
		groldthreatened[s] = FALSE;
		ld[s] = NOLD;
		lgr[s] = NOGROUP;
		ldrflag[s] = EOL;
		terv[s] = 0;
		rterv[s][0] = rterv[s][1] = 0;
		runterv[s][0] = runterv[s][1] = 0;
		cnbrd[s] = EOL;
		lkbrd[s] = EOL;
		llbrd[s] = EOL;
		ollbrd[s] = EOL;
		ddbrd[s] = EOL;
		ltrgd[s] = FALSE;
		ltrcolor[s] = NOCOLOR;
		ltr1[s] = 0;
		for (j = 0; j < 4; ++j) {
			sqrbrd[s][j] = NOSQUARE;
			dstbrd[s][j] = 0;
		}
	}
	for (i = 0; i < NUMGROUPS+NUMCONNS+NUMEYERECS; ++i)
		grldr[i] = EOL;
	board[NOSQUARE] = NOGROUP;
	board[PASS] = NOGROUP;
	cnchgd = EOL;
	lnbn[0] = lnbn[boardsquare-1] = lnbn[boardsize-1] =
		lnbn[boardsquare-boardsize] = 2;
	lookaheadflag = FALSE;
	for (i = 0; i < NUMCONNS; i++) {
		cngr1[i] = NOGROUP;
		cngr2[i] = NOGROUP;
		cncnum[i] = 0;
		cnlknum[i] = 0;
		cnllnum[i] = 0;
		cnollnum[i] = 0;
		cnddnum[i] = 0;
		cnptr[i] = EOL;
		cnlkptr[i] = EOL;
		cnllptr[i] = EOL;
		cnollptr[i] = EOL;
		cnddptr[i] = EOL;
		cnpathptr[i] = EOL;
		cnprot[i] = CANT_CONNECT;
		cntype[i] = 0;
		cnshcent[i] = NOGROUP;
	}
	for (i = 0; i < NUMGROUPS; i++) {  /* init all groups, including NOGROUP */
		grcolor[i] = NOCOLOR;
		grlbp[i] = EOL;
		grnbp[i] = EOL;
		grcnp[i] = EOL;
		grlibs[i] = 0;
		gralive[i] = 0;
		gralprob[i] = 0;
		grthreatened[i] = FALSE;
		grlv[i] = FALSE;
		grsize[i] = 0;
		grpieces[i] = -1;
		grarmy[i] = NOARMY;
		grdeadarmy[i] = NOARMY;
		grdefval[i] = 0;
	}
	maxgr = maxpc = 0;
	numpris[0] = numpris[1] = 0;
	hcpoints = EOL;
	if (boardsize >= 13) {
		addlist((sqr_t)(3*boardsize+3),&hcpoints);
		addlist((sqr_t)(4*boardsize-4),&hcpoints);
		addlist((sqr_t)(boardsquare-3*boardsize-4),&hcpoints);
		addlist((sqr_t)(boardsquare-4*boardsize+3),&hcpoints);
	}
	if (boardsize >= 13 && boardsize%2 == 1) {
		addlist((sqr_t)(boardsquare/2),&hcpoints);
		addlist((sqr_t)(3*boardsize+boardsize/2),&hcpoints);
		addlist((sqr_t)(boardsquare/2-boardsize/2+3),&hcpoints);
		addlist((sqr_t)(boardsquare/2+boardsize/2-3),&hcpoints);
		addlist((sqr_t)(boardsquare-3*boardsize-boardsize/2-1),&hcpoints);
	}
	matchpatterns(0, boardsquare - 1, NOSQUARE, TRUE);	/* do the initial match on the empty board */
}
  
extern int pccplay;

/* update the playing level (1 to NUMLEVELS - 1).  MAXLEVEL has everything turned on.  UCTLEVEL is the UCT player.
 * return the old level
 * lower levels leave stuff out for speed.  Higher levels look at more
 * moves and do more reading.
 */

void fixplaylevel(int p) 
{
	if (p == playlevel) {
		return;
	}
	if ( p < 0) {
		p = 0;
	}
	if ( p >= NUMLEVELS) {
		p = NUMLEVELS - 1;
	}
	playlevel = p;
	initrtval();  /* set up proper distance for influence */
	life(2);
}


extern int rtval1[MAXDIST+1][DEAD+1];	/* for alive groups */
extern int rtval2[MAXDIST+1][DEAD+1];     /* for weak and unsettled groups */
extern int rtval3[MAXDIST+1][DEAD+1];     /* for weak and unsettled groups  - opposite color */

extern double rtalive1[NUMALIVE];	/* maximum value for rt for each aliveness */

extern double rtalive2[NUMALIVE];	/* maximum value for rt for each aliveness */
extern double rtalive3[NUMALIVE];	/* maximum value for rt for each aliveness */


/* influence falls off linearly with distance since circumference increases
 * linearly with radius.  Use 1.6 times 1/dist so area only needs to be
 * 50% surrounded to be secure
 */

/* old 1/n vales 
static float rtdist[] = {
0.f, 1.f, .5f, .3333333f, .25f, .2f, .1666666f, .142857f, .125f,
.11111111f, .1f, .090909f,
.0833333f, .076923f, .071428f };
*/


/* constant value multiplied by rtdist to get influence value.  20 makes liberty 1/4 point */
#define INITRTVAL 225

/* 3/01, switch to (2/3)^n for greater influence from walls */
/* at great distance (over 5), reduce value faster 12/ 07 reduce less to emphasize center more*/
static float rtdist[] = {  /* falloff in influence with distance */
0.f, 1.f, .66666f, .44444f, .30f, .2f, .13f, 
/* .0878f, .05853f, .039f, .026f, .0173f, .0115f, .0077f, .005f  old, 2/3 values */
   .0800f, .05000f, .030f, .02f, .008f, .005f, .003f, .002f 
};

static float rtdist3[] = {  /* falloff in distance for weak groups adding influence to enemy */
0.f, 1.f, .66f, .44f, .3f, .15f, .07f, .03f, .0f,
.0f, .0f,
.0f, .0f, .0f, .0f };

float rtfactor = 2.0f;  /* factor to multiply rtdist by KEEP THIS 2.0 to make debugging easier */
/* single strong stone radiates 100 to nearest point */

static int d1i[] = {
1,19,-1,-19 };

static int d2i[] = {
19,-1,-19,1 };

int d1[4],d2[4],d3[4]; /* (d3 is sum of d1 and d2) */

int maxsharedrt = (int)(INITRTVAL * .7);
extern int rtthreshold;

static void initrtval(void)
{
	int dist,alive,i;
	rtthreshold = (int)(rtdist[4] * rtfactor * INITRTVAL/4);
	for (alive = 0; alive <= DEAD; alive++) {
		for (dist = 1; dist <= MAXDIST; dist++) {
			if (dist > mdist[playlevel]) {
				rtval1[dist][alive] = 0;
				continue;
			}
			rtval1[dist][alive] = (int)(INITRTVAL * rtalive1[alive] * rtdist[dist] * rtfactor / 4 + .5); /* .5 for rounding */
			if (playlevel < 3 && rtval1[dist][alive] < INITRTVAL / 32 &&
			   rtval1[dist][alive] > -INITRTVAL / 32)rtval1[dist][alive] = 0;
		}
		for (dist = 1; dist <= MAXDIST; ++dist) {
			if (dist > mdist[playlevel]) {
				rtval2[dist][alive] = 0;
				rtval3[dist][alive] = 0;
				continue;
			}
			rtval2[dist][alive] = (int)(INITRTVAL * rtalive2[alive] * rtdist[dist] * rtfactor/4+.5);
			if (playlevel < 3 && rtval2[dist][alive] < INITRTVAL/32 &&
			   rtval2[dist][alive] > -INITRTVAL/32)rtval2[dist][alive] = 0;
			rtval3[dist][alive] = (int)(INITRTVAL * rtalive3[alive] * rtdist3[dist] * rtfactor/4+.5);
			if (playlevel < 3 && rtval3[dist][alive] < INITRTVAL/32 &&
			   rtval3[dist][alive] > -INITRTVAL/32)rtval3[dist][alive] = 0;
		}
	}
	for (i = 0; i < 4; ++i) {
		if (d1i[i] == 19)
			d1[i] = boardsize;
		else if (d1i[i] == -19)
			d1[i] = -boardsize;
		else 
			d1[i] = d1i[i];
		if (d2i[i] == 19)
			d2[i] = boardsize;
		else if (d2i[i] == -19)
			d2[i] = -boardsize;
		else 
			d2[i] = d2i[i];
		d3[i] = d1[i] + d2[i];
	}
}
