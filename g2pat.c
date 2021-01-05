/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* all code for pattern matching, generation, and updating */

# include "g2hd.h"
# include "g2rldef.h"
# define MAINPATTERN
# include "g2pat.h"
#if defined(CHECK) || defined(G2DEBUGOUTPUT) || defined(TEST)
# include <stdio.h>
#endif
#include <math.h>


# define _fastcall
static int moveispotconn(sqr_t s, army_t army);
static int patoffedge(int p, int o,sqr_t s);
static int badattsmovematch(sqr_t s, int o, int ptr, sqr_t mv, int color);
static int movematch(sqr_t s, int o, struct newpatstruct *np, sqr_t mv);
static int moveunmatch(sqr_t s, int o, struct newpatstruct *np, sqr_t mv);
static void addmovestype(sqr_t s, int o, int pat, int c);
static int attsmatch(int p, int o, sqr_t s, army_t *a1, army_t *a2, sqr_t *h1, sqr_t *h2, sqr_t *h3, listval_t ldrno);
/* static int patmatchtype(int pn, int *color); */
static int getcorner(sqr_t s, int o, int *xp, int *yp);
static void _fastcall patmatch(UINT16 h,int o,sqr_t s);
static int  _fastcall makebitmap(int x,int y,int o);
static void fireapat(int p,army_t a1,army_t a2,int h1,int h2,int h3, int color, int passval);


extern int maxpatvariations[21],urgalive[NUMALIVE];

/* values for pattern moves, in points lost if this move is not made: 0-9 not urgent, a-f urgent */
int pmvalue[16] = { 0,1,2,3,4,5,6,8,10,15,   5,8,10,12,15,20 };

UINT16 maxpatterns,maxpatmoves;
int nextfreemove = 0;  /* index of next free move */


union pat_union bmw,bmb,bme;  /* bit map to be matched against (from board) */

struct newpattern *pt = 0;

int nextpattern;  /* index of next free pattern */
struct pmove *pm = 0;

struct phash *ph = 0;

/* start of hash chains for different types of patterns */
UINT16 *hstarta = 0, *hstartc = 0, *hstarte = 0;  

UINT16 nexthash = 0;  /* index of next free hash */

struct newpatstruct newpat[NUMNEWPATS];

struct pfile *patfiles = 0;
                         
static int pat2conn[6] = {
CANT_CONNECT,
CAN_CONNECT,
SHARED_CONNECT,
KO_CONNECT,
AJI_CONNECT,
SOLID_CONNECT,
};                         
                         
int gettype(int pat)
{
	int ret;
	if (pat < 0)
		return CAN_CONNECT;  /* to prevent crash if pattern database not loaded */
	if ((pt[pat].wheretype & TYPEMASK) < PT_READCUT)
		ret = pt[pat].wheretype & TYPEMASK;
	else if ((pt[pat].wheretype & TYPEMASK) == PT_READCUT)
		ret = PT_CANCONN;
	else if ((pt[pat].wheretype & TYPEMASK) == PT_READSHR)
		ret = PT_SHARED;
	else if ((pt[pat].wheretype & TYPEMASK) == PT_READAJI)
		ret = PT_AJI;
	return pat2conn[ret];
}
	
#define MAXMM 40
sqr_t matchmoves[MAXMM];
int pnum[MAXMM];
int nextmm;

/* find all patterns of a particular type that match at a point
 * and return a list of moves, in priority order.
 * priority is determined first by the order of appearance of
 * patterns in the pattern file, and second by the move value within
 * a pattern.
 *
 * pf is the number of the pattern file to search
 * s is the square where the pattern should match,
 *  corresponding to the 4-4 point in the pattern.
 * c is the color to match, corresponding to the color of
 * the key stone(s), for example the stone at 4-4 for obvious patterns.
 * mcolor is the color whose moves we are looking for.
 *
 * define FASTPAT to make program a few percent faster, but it is
 * dangerous - not portable.  Test it carefully on your machine if
 * you set it.
 */
	
list_t matchtypemoves(int pf, sqr_t s, int c, int mcolor, int debug)
{
	list_t moves = EOL;
	int i,o,xp,yp,e,tmp;
	army_t a1, a2;
	unsigned int pat;
	sqr_t corner,h1,h2,h3;
#ifdef FASTPAT
	unsigned long *p,*b;
#endif
#ifdef G2DEBUGOUTPUT
	char buf[80];
	char bf2[10];
#endif
	nextmm = 0;
	if (!c)
		mcolor = 1-mcolor;
	for (o = 0; o < 8; o++) {	
		e = getcorner(s, o, &xp, &yp);
		corner = yp * boardsize + xp; /* note that corner may be off the board */
		if (o == 2 || o == 3 || o == 6 || o == 7) {
			tmp = xp;
			xp = yp;
			yp = tmp;
		}
		if (o > 3)
			yp = boardsize - 1 - yp;
		if (o & 1)
			xp = boardsize - 1 - xp;
		makebitmap(xp, yp, o);
#ifdef FASTPAT
		b = &bmw.lv[0];  /* not portable, but much faster */
#endif
 		for (pat = patfiles[pf].firstpat; pat < patfiles[pf].nextpat; pat++) {
			/* just match pattern directly here - 6 compares */
#ifdef FASTPAT
			p = &pt[pat].pw.lv[0];
			if (*(p+4) & *(b+4) ||
				*(p+5) & *(b+5))
				continue;
			if (c) {
				if (*(p+2) & *(b+2) ||
					*(p+3) & *(b+3))
					continue;
				if (*(p) & *(b) ||
					*(p+1) & *(b+1))
					continue;
			}
			else {
				if (*(p+2) & *(b) ||
					*(p+3) & *(b+1))
					continue;
				if (*(p) & *(b+2) ||
					*(p+1) & *(b+3))
					continue;
			}
#endif
#ifndef FASTPAT
			if ((pt[pat].pe.lv[0] & bme.lv[0]) ||
				(pt[pat].pe.lv[1] & bme.lv[1]))
				continue;
			if (c) {
				if ((pt[pat].pb.lv[0] & bmb.lv[0]) ||
					(pt[pat].pb.lv[1] & bmb.lv[1]))
					continue;
				if ((pt[pat].pw.lv[0] & bmw.lv[0]) ||
					(pt[pat].pw.lv[1] & bmw.lv[1]))
					continue;
			}
			else {
				if ((pt[pat].pb.lv[0] & bmw.lv[0]) ||
					(pt[pat].pb.lv[1] & bmw.lv[1]))
					continue;
				if ((pt[pat].pw.lv[0] & bmb.lv[0]) ||
					(pt[pat].pw.lv[1] & bmb.lv[1]))
					continue;
			}
#endif
			if ((pt[pat].wheretype & WHEREMASK) == P_CORNER && 
				(((pt[pat].size & 0xf0) >> 4) + edge[s] != 5 ||
				(pt[pat].size & 0xf0) + edge[s] != 5))
				continue;
			if ((pt[pat].wheretype & WHEREMASK) == P_EDGE &&
				(((pt[pat].size & 0xf0) >> 4) + edge[s] != 5 ||	/* edge is the right distance away from the key point */
				   s - e >= 0 && s + e < boardsquare && 
				 	((pt[pat].size & 0xf0) >> 4) + edge[s - e] != 6))	/* edge is the right direction */
				continue;
			
			if (attsmatch(pat, o, corner, &a1, &a2, &h1, &h2, &h3, NOGROUP)) {
				addmovestype(corner, o, pat, mcolor);
#ifdef G2DEBUGOUTPUT
				if (debug) {
					sprintf(buf, "matchtype for %d o=%d at %s matches pattern %d\n",
						pf, o, ssqr(s, bf2), pat);
					outerr(buf);
				}
#endif
			}
		}
	}
	for (i = 0; i < nextmm; ++i) {
		adflist(matchmoves[i], &moves);  /* best moves at end, go first */
	}
	return(moves);
}
        
extern int numopjtacnodes;
                                     
/* read one point jump with cutoffs.
 * c is color to move.
 * movc is color to look for in pattern move tree.
 * first ply is cutting move.  Return TRUE if cut works
 */
                                     
static int evalopj(int mptr, int o, int c, int movc, sqr_t corner, sqr_t connpoint, sqr_t h1, sqr_t h2, listval_t ldrno, int pat)
{
	int ptr2, retval = FALSE, can_cap,mlibs;
	sqr_t tmp;
	list_t common = EOL, ptr;
	sqr_t s;
	int lcount = numnodes;
#ifdef CHECK
	char buf[100];
#endif
	if (board[h1] == board[h2])
		return FALSE;   /* no cut if stones are in same group */
	for (ptr2 = mptr; ptr2 != -1; ptr2 = patmore(ptr2)) {
		if (PM_COLOR(ptr2) != movc)  /* find move under consideration in any pattern */
			continue;     
		s = patgetsqr(corner,o,pm[ptr2].xy);
		if (s < 0 || s >= boardsquare || board[s] != NOGROUP) {
#ifdef CHECK
			sprintf(buf,"Bad move in evalopj pattern %d at %d",pat,s);
			outerror(buf);
			turnoffcplay();
#endif
			continue;
		}
		mvs[msptr] = s;
		mvcolor[msptr] = c;
		upldrflags(msptr,ldrno);
		if (!lupdate(msptr)) {
			ldndate(msptr);
#ifdef NEVER
			if (debug) {
				sprintf(buf,"Suicide in one point jump patterns- pattern %d",pat);
				outerr(buf);
				}
#endif
			retval = FALSE;  /* move did not work */
			continue;
		}
		msptr++;
		if ((pm[ptr2].csfv) & MV_FINAL) {
				/* found an endpoint, evaluate */
			if (board[h1] == NOGROUP || board[h2] == NOGROUP)
				retval = TRUE;  /* captured group so cut must work! */
			else {
				andlist(grnbp[board[h1]],grnbp[board[h2]],&common);
				addlist(board[s],&common);  /* capture common neighboror last enemy stone played to connect */
#ifdef CHECK
				if (common == EOL)
					outerr("no common neighbors of h1, h2");
#endif					
				retval = TRUE;  /* cut works, unless a common group is captured */
				for (ptr = common; ptr != EOL; ptr = link[ptr]) {
					mlibs = conntaclibs[playlevel];
/*					if (list[ptr] == board[s] && edge[s] < 4 ||
						list[ptr] == board[connpoint] && edge[connpoint] < 4)
						mlibs++; */
					can_cap = iscaptured((group_t)list[ptr],conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],1-c,ldrno,&tmp,c);
					if (can_cap) {
						retval = FALSE;
						break;
					}
				}
				killist(&common);
			}
		}
		else
			retval = 1-evalopj(ptr2+1, o, 1-c, 1-movc, corner, connpoint, h1, h2, ldrno, pat);
		msptr--;
		ldndate(msptr);
		if (retval)
			break;  /* cutoff - move works so no need to try more */	
	}               
	numopjtacnodes += numnodes - lcount;	
	return retval;
}
 

/* reading for one point jump paterns.
 * c is color to move first (black if not color reversed match)
 * c is also TRUE if this is a color reversed match.
 * h1 and h2 are handles of points where stones to be connected are.
 * return TRUE if matches, for new best pattern.
 * for reading aji connection vs can_connect (READCUT/READAJI),
 * will return TRUE if pattern matches.
 */

int readopj(int pat, int o, int c, sqr_t corner, sqr_t connpoint, sqr_t h1, sqr_t h2, sqr_t h3, listval_t ldrno)
{
#ifdef G2DEBUG
	char buf[80];
#endif	
	int ret;
	if ((pt[pat].wheretype & TYPEMASK) < PT_READCUT)
		return TRUE;
#ifdef G2DEBUG		
	if (debug > 1) {
		sprintf(buf,"reading pattern %d, wheretype is %d",pat,(pt[pat].wheretype & TYPEMASK));
		outerr(buf);
	}
#endif		
	if (h1 == NOSQUARE || h2 == NOSQUARE) {
#ifdef G2DEBUG
		sprintf(buf,"Bad H1 or H2 in pat %d",pat);
		outerror(buf);
#endif
		return FALSE;
	}
	if (pt[pat].moves == 0xffff) {
#ifdef CHECK
		outerror("Empty move list in one point jump");
#endif		
		return FALSE;
	}
	ret = evalopj(pt[pat].moves, o, c, BLACKCOLOR,corner, connpoint, h1, h2, ldrno, pat);
	if ((pt[pat].wheretype & TYPEMASK) == PT_READAJI)
		ret = 1-ret;  /* READAJI matches if cut doesn't work */

	return ret;
}

/* find first of a particular type that match at a point
 * and return pattern number.  Error if no pattern matches.
 * orientation that matches is returned in o.
 *
 * pf is the number of the pattern file to search
 * s is the square where the pattern should match,
 *  corresponding to the 4-4 point in the pattern.
 * c is the color to match, corresponding to the color of
 * the key stone(s), for example the stone at 4-4 for obvious patterns.
 *
 * #define FASTPAT if you want faster pattern recognition,
 * but this fast code is not portable, and doesn;t work
 * on all machines.  It's only a few percent faster anyway
 */
	
unsigned int matchtypebest(int pf, sqr_t s, int c, listval_t ldrno, int *reto, conn_t cn, 
						   int (*readpat)(int, int, int, sqr_t, sqr_t, sqr_t, sqr_t, sqr_t, listval_t))
{
	unsigned int bestpat = BIGNUM;
	int besto; 
	int o,xp,yp,e,tmp;
	army_t a1, a2;
	unsigned int pat;
	sqr_t corner,h1,h2,h3;
#ifdef FASTPAT
	unsigned long *p,*b;
#endif
#ifdef TEST
	char buf[80],bf2[10];
#endif
	nextmm = 0;
	for (o = 0; o < 8; o++) {	
		e = getcorner(s, o, &xp, &yp);
		corner = yp*boardsize+xp; /* note that corner may be off the board */
		if (o == 2 || o == 3 || o == 6 || o == 7) {
			tmp = xp;
			xp = yp;
			yp = tmp;
		}
		if (o > 3)yp = boardsize-1-yp;
		if (o & 1)xp = boardsize-1-xp;
		makebitmap(xp,yp,o);
#ifdef FASTPAT
		b = &bmw.lv[0];  /* not portable, but much faster */
#endif
		for (pat = patfiles[pf].firstpat; 
			pat < patfiles[pf].nextpat && pat < bestpat; 
			pat++) {
				/* just match pattern directly here - 6 compares */
#ifdef FASTPAT
				p = &pt[pat].pw.lv[0];
				if (*(p+4) & *(b+4) ||
					*(p+5) & *(b+5))
					continue;
				if (c) {
					if (*(p+2) & *(b+2) ||
						*(p+3) & *(b+3))
						continue;
					if (*(p) & *(b) ||
						*(p+1) & *(b+1))
						continue;
				}
				else {
					if (*(p+2) & *(b) ||
						*(p+3) & *(b+1))
						continue;
					if (*(p) & *(b+2) ||
						*(p+1) & *(b+3))
						continue;
				}
#endif
#ifndef FASTPAT
				if ((pt[pat].pe.lv[0] & bme.lv[0]) ||
					(pt[pat].pe.lv[1] & bme.lv[1]))
					continue;
				if (c) {
					if ((pt[pat].pb.lv[0] & bmb.lv[0]) ||
						(pt[pat].pb.lv[1] & bmb.lv[1]))
						continue;
					if ((pt[pat].pw.lv[0] & bmw.lv[0]) ||
						(pt[pat].pw.lv[1] & bmw.lv[1]))
						continue;
				}
				else {
					if ((pt[pat].pb.lv[0] & bmw.lv[0]) ||
						(pt[pat].pb.lv[1] & bmw.lv[1]))
						continue;
					if ((pt[pat].pw.lv[0] & bmb.lv[0]) ||
						(pt[pat].pw.lv[1] & bmb.lv[1]))
						continue;
				}
#endif
				if ((pt[pat].wheretype & WHEREMASK) == P_CORNER && 
					(((pt[pat].size & 0xf0)>>4) + edge[s] != 5 ||
					(pt[pat].size & 0xf0) + edge[s] != 5))
					continue;
				if ((pt[pat].wheretype & WHEREMASK) == P_EDGE &&
					(((pt[pat].size & 0xf0)>>4) + edge[s] != 5 ||
					edge[s] > 1 && 
					((pt[pat].size & 0xf0)>>4) + edge[s+e] != 4))
					continue;

				if (attsmatch(pat,o,corner,&a1,&a2,&h1,&h2,&h3, ldrno) &&
					(*readpat)(pat,o,1-c,corner,s,h1,h2,h3,ldrno)) {
						bestpat = pat;
						besto = o;
						if ((pt[pat].wheretype & TYPEMASK) == PT_READSHR) {	/* shared, so have to set center */
							if (cngr1[cn] != board[h1] && cngr1[cn] != board[h2]) /* h1 and h2 are on the outer groups */
								cnshcent[cn] = cngr1[cn];
							else
								cnshcent[cn] = cngr2[cn];
						}
#ifdef TEST
						if (debug > 3) {
							sprintf(buf,"matchtype for %d o=%d at %s matches pattern %d\n",
								pf,o,ssqr(s,bf2),pat);
							outerr(buf);
						}
#endif
						break;  /* found first one - no more for this o */
				}
		}
	}
	*reto = besto;
	if (bestpat == BIGNUM) {
		bestpat = patfiles[pf].nextpat-1;
#ifdef CHECK
		outerror("No pattern match found");
#endif
	}		
	return bestpat;
}


/* add moves from a pattern match by type.
 * s is corner of pattern, o is orientation, pat is pattern number
 * color is 1 if match with colors reversed
 * c is color to move
 * return moves found in front of moves
 * sort moves in order by value
 */

static void addmovestype(sqr_t s, int o, int pat, int c)
{
	int mp,i,j;
	sqr_t sqr;
	for (mp = pt[pat].moves; mp != -1; mp = patmore(mp)) {
		if (PM_COLOR(mp) != c)continue;
		sqr = patgetsqr(s,o,pm[mp].xy);
		if (PM_VAL(mp) == 0) {  /* bad move */
			for (i = 0; i < nextmm; ++i)
				if (matchmoves[i] == sqr) {
					for (j = i; j < nextmm; ++j) {
						matchmoves[i] = matchmoves[i+1];
						pnum[i] = pnum[i+1];
					}
					nextmm--;
				}
		}
		else {
			for (i = 0; i < nextmm; ++i)  /* find place to insert */
				if (pnum[i] < pat*8-PM_VAL(mp))
					break;
			for (j = nextmm-1; j >= i; --j) {
				matchmoves[j+1] = matchmoves[j];  /* slow sort, but who cares */
				pnum[j+1] = pnum[j];
			}
			matchmoves[i] = sqr;
			pnum[i] = pat*8 - PM_VAL(mp);
			++nextmm;
		}
	}
}
	
/* for  special pattern pat, orientation o, with key point (4,4) at point s,
 * return the x and y values where the corner of the pattern is in corner,
 * return the offset of direction toward the edge.
 */
	
static int getcorner(sqr_t s, int o, int *xp, int *yp)
{
	int e = -1;  /* edge */
	int hx = 0 , hy = 0;  /* x and y handle on real board */
	switch(o) {
	case 0:
		hx = 3;
		hy = 3;
		e = -1;
		break;
	case 1:
		hx = -3;
		hy = 3;
		e = 1;
		break;
	case 2:
		hx = 3;
		hy = 3;
		e = -boardsize;
		break;
	case 3:
		hx = 3;
		hy = -3;
		e = boardsize;
		break;
	case 4:
		hx = 3;
		hy = -3;
		e = -1;
		break;
	case 5:
		hx = -3;
		hy = -3;
		e = 1;
		break;
	case 6:
		hx = -3;
		hy = 3;
		e = -boardsize;
		break;
	case 7:
		hx = -3;
		hy = -3;
		e = boardsize;
		break;
	}
	*xp = xval[s] - hx;
	*yp = yval[s] - hy;
	return(e);
}	
	
/* find shapes after making move at mptr 
 * the match is only by shapes. atributes are ignored, since
 * life can change from move to move while the pattern still matches
 */

void findmatch(int mptr, int up) 
{
	sqr_t s, sn, fsqr, lsqr;
	list_t ptr;
	int ptr2;  /* grpieces, not list */                                                                
	s = mvs[mptr];
	if (s == PASS) {
		return;
	}
	fsqr = lsqr = s;
	for (ptr = mvcapt[mptr]; ptr != EOL; ptr = link[ptr]) {
		for (ptr2 = grpieces[list[ptr]]; ptr2 != -1; ptr2 = mvnext[ptr2]) {
			sn = mvs[ptr2];
			if (xval[sn] < xval[fsqr]) {
				fsqr = yval[fsqr] * boardsize + xval[sn];
			}
			if (yval[sn] < yval[fsqr]) {
				fsqr = yval[sn] * boardsize + xval[fsqr];
			}
			if (xval[sn] > xval[lsqr]) {
				lsqr = yval[lsqr] * boardsize + xval[sn];
			}
			if (yval[sn] > yval[lsqr]) {
				lsqr = yval[sn] * boardsize + xval[lsqr];
			}
		}
	}
	matchpatterns(fsqr, lsqr, s, up);
}



/* find all pattern matches 
 * fsqr is upper left point and
 * lsqr is lower right point of bounding rectangle
 * where stones have been added or removed
 *
 * mv is the move that caused this area to be reevaluated
 */

int xso[] = { -7,0,-7,-7,-7,0,0,0 };
int xse[] = { 0,7,0,0,0,7,7,7 };
int yso[] = { -7,-7,-7,0,0,0,-7,0 };
int yse[] = { 0,0,0,7,7,7,0,7 };

/* Match patterns in rectangle from fsqr to lsqr.  mv is the move.
 * if 'up', then make move, else retract move.
 * if move is in pattern tree of pattern already matched, adjust
 * tree pointer, else delete pattern match.
 * only call with mv = NOSQUARE when matching whole board from scratch.
 * this will delete any existing pattern matches, and do all new ones.
 *
 * orientations: internal x, y on the real board are:
 *
 *              0  1  2  3  4  5  6  7
 * internal x  +x -x +y -y +x -x +y -y
 * internal y  +y +y +x +x -y -y -x -x
 *
 */

void matchpatterns(sqr_t fsqr, sqr_t lsqr, sqr_t mv, int up)
{
	sqr_t s;
	int o, h, x, y, xs, ys, xe, ye, ptr, xp, yp;
	listval_t tmp;
	if (maxpatterns == 0)
		return;

	for (o = 0; o < 8; ++o) {  /* each orientation TODO, continue if mv and all captures are outside of pattern size, bound on stones, not stones + 8 */
		xe = xval[lsqr] + xse[o];
		ye = yval[lsqr] + yse[o];
		xs = xval[fsqr] + xso[o];
		ys = yval[fsqr] + yso[o];
		if (xs < 0)
			xs = 0;
		if (ys < 0)
			ys = 0;
		if (xe >= boardsize)
			xe = boardsize - 1;
		if (ye >= boardsize)
			ye = boardsize - 1;
		for (x = xs; x <= xe; ++x) { /* walk the actual board */
			for (y = ys; y <= ye; ++y) {
				s = y * boardsize + x;
				ptr = newpatbrd[s]; /* check for move matches to remove (up and no more moves, or down), or adjust (up and move is in pattern move list) */
				while (ptr != EOL) { /* must be while due to dlflist */
					if (newpat[list[ptr]].orient == o) {
						if (!up && !moveunmatch(s, o, &newpat[list[ptr]], mv) ||	/* back a move, if at first move, delete */
							up && !movematch(s, o, &newpat[list[ptr]], mv)) {		/* make a move, if move not found, delete */
							tmp = list[ptr];
							ptr = link[ptr];
							dlflist(tmp, &newpatbrd[s]);
							adflist(tmp, &newpatfreelist);
						}
						else 
							ptr = link[ptr];
					}
					else 
						ptr = link[ptr];
				}
				xp = x;  /* x and y in pattern space */
				yp = y;
				if (o == 2 || o == 3 || o == 6 || o == 7) {
					xp = y;
					yp = x;
				}
				if (o > 3)
					yp = boardsize - 1 - yp;
				if (o & 1)
					xp = boardsize - 1 - xp;

				h = makebitmap(xp, yp, o);
				if (xp == 0 && yp == 0)
					patmatch(hstartc[h], o, s);
				if (xp == 0)
					patmatch(hstarte[h], o, s);
				patmatch(hstarta[h], o, s);
			}
		}
	}
}

/* move at mv is being made.  Check to see if it is next move for any currently
 * matching patterns.  If it is, and if it is a first move, check the
 * attributes.  If they don't match, delete the pattern match.
 */
	
void checkmoveatts(sqr_t mv, int c)
{
	listval_t tmp;
	list_t ptr;
	sqr_t s;
	int o;
	if (mv == PASS) {
		return;
	}
	for (s = 0; s < boardsquare; ++s) {
		ptr = newpatbrd[s];
		while (ptr != EOL) { /* must be while due to dlflist */
			if (newpat[list[ptr]].move != pt[newpat[list[ptr]].num].moves) {
				ptr = link[ptr];
				continue;  /* not at first ply */
			}
			o = newpat[list[ptr]].orient;
			if (badattsmovematch(s, o, list[ptr], mv, c)) {
				tmp = list[ptr];
				ptr = link[ptr];
				dlflist(tmp,&newpatbrd[s]);
				adflist(tmp,&newpatfreelist);
			} else  {
				ptr = link[ptr];
			}
		}
	}
}
	
/* pattern at s, with orientation o, at newpat[ptr].num.
 * Does mv match a move in the move list?  if so,
 * check that the attributes match.  If they don't,
 * return TRUE so pattern can be eliminated.
 */

static int badattsmovematch(sqr_t s, int o, int ptr, sqr_t mv, int color)
{
	int ptr2;  /* newpat ptr, not list ptr ! */
	army_t a1, a2;
	sqr_t h1, h2, h3;
	if (newpat[ptr].color)
		color = 1 - color;
	for (ptr2 = newpat[ptr].move; ptr2 != -1; ptr2 = patmore(ptr2)) {
		if (PM_COLOR(ptr2) == color && 
		   patgetsqr(s,o,pm[ptr2].xy) == mv &&
		   !PM_FINAL(ptr2)) {  /* found move match */
			if (!attsmatch(newpat[ptr].num, o, s, &a1, &a2, &h1, &h2, &h3, NOGROUP))
				return TRUE;
		}
	}
	return FALSE ;
}

/*
 * Take back a move in a pattern match at np.
 * if so, adjust the newpat move pointer and return TRUE
 * if no match, leave pointer and return TRUE, since move taken back is not part of this pattern
 * otherwise return FALSE so this pattern will be deleted
 */
static int moveunmatch(sqr_t s, int o, struct newpatstruct *np, sqr_t mv)
{
	int ptr2;	/* newpat ptr, not list ptr */
	int found = FALSE;
	int mptr;
	if (np->move == pt[np->num].moves)
		return FALSE;
	for (ptr2 = np->move - 1; ptr2 != -1; ptr2 = patprev(ptr2)) {
		mptr = ptr2;  /* first sibling */
		if (patgetsqr(s, o, pm[ptr2].xy) == mv) {
				found = TRUE;
		}
		if (ptr2 == pt[np->num].moves) {
			break;
		}
	}
	if (found) {
		np->move = mptr;	/* point at first sibling of parent */
	}
	return TRUE;
}

/*
 * See if s is the next move in an existing pattern match at np.
 * if so, adjust the newpat move pointer and return TRUE
 * otherwise several case
 * if the pattern is at the root, return false so it is deleted and rematched
 * if this stone is part of the pattern the mattern no longer matches and return false so it will be deleted
 * otherwise return true to keep it, since it still matches
 */
static int movematch(sqr_t s, int o, struct newpatstruct *np, sqr_t mv)
{
	int color, ptr2;
	color = S_COLOR(mv);
	if (color == NOCOLOR) {
		return FALSE;  /* self capture! */
	}
	if (np->color) {
		color = 1 - color;
	}
	for (ptr2 = np->move; ptr2 != -1; ptr2 = patmore(ptr2)) {
		if (PM_COLOR(ptr2) == color && 
			patgetsqr(s, o, pm[ptr2].xy) == mv &&
			!PM_FINAL(ptr2)) {
			np->move = ptr2 + 1;  /* next move */
			return TRUE;
		}
	}
	if (np->move == pt[np->num].moves)
		return FALSE;
	return FALSE;
}

/* walk list h and find patterns that match at s with orientation o */
	
static void
patmatch(UINT16 h, int o, sqr_t s)
{
	register UINT16 p;
	listval_t ptn, pn;

	for (p = h; p != 0xffff; p = ph[p].next) {
		pn = ph[p].pattnum;  

		if ((pt[pn].pw.lv[0] & bmw.lv[0] |
			pt[pn].pb.lv[0] & bmb.lv[0] |
			pt[pn].pe.lv[0] & bme.lv[0]) == 0L &&
			(pt[pn].pw.lv[1] & bmw.lv[1] |
			pt[pn].pe.lv[1] & bme.lv[1] |
			pt[pn].pb.lv[1] & bmb.lv[1]) == 0L) {

				/* found a match */
				if (!patoffedge(pn,o,s)) {
					ptn = gtflist(&newpatfreelist);
					if (ptn != G2ERROR) {
						adflist(ptn,&newpatbrd[s]);
						newpat[ptn].orient = o;
						newpat[ptn].color = 0;
						newpat[ptn].num = pn;
						newpat[ptn].msptr = msptr;
						newpat[ptn].move = pt[pn].moves;
						newpat[ptn].sqr = s;
					}
#ifdef TEST	
					else {
						clearerror();
						outerr("Out of patterns");
					}
#endif				
				}
		}
		if ((pt[pn].pw.lv[0] & bmb.lv[0] | 
			pt[pn].pb.lv[0] & bmw.lv[0] | 
			pt[pn].pe.lv[0] & bme.lv[0]) == 0L &&
			(pt[pn].pw.lv[1] & bmb.lv[1] | 
			pt[pn].pb.lv[1] & bmw.lv[1] | 
			pt[pn].pe.lv[1] & bme.lv[1]) == 0L) {
				/* found a color reversed match */
				if (!patoffedge(pn,o,s)) {
					ptn = gtflist(&newpatfreelist);
					if (ptn != G2ERROR) {
						adflist(ptn,&newpatbrd[s]);
						newpat[ptn].orient = o;
						newpat[ptn].color = 1;
						newpat[ptn].num = pn;
						newpat[ptn].msptr = msptr;
						newpat[ptn].move = pt[pn].moves;
						newpat[ptn].sqr = s;
					}
#ifdef TEST	
					else {
						clearerror();
						outerr("Out of patterns.");
					}
#endif				
				}
		}
	}
}

#ifdef NEVER
	
/* does pattern p match the current bitmap?
 * return TRUE for match.  retirn color = 0 if colors not reversed,
 * 1 if reversed
 */
	
static int patmatchtype(int pn, int *color) {

	if ((pt[pn].pw.lv[0] & bmw.lv[0] |
		pt[pn].pb.lv[0] & bmb.lv[0] |
		pt[pn].pe.lv[0] & bme.lv[0]) == 0L &&
   	     (pt[pn].pw.lv[1] & bmw.lv[1] |
		pt[pn].pe.lv[1] & bme.lv[1] |
		pt[pn].pb.lv[1] & bmb.lv[1]) == 0L) {

			/* found a match */
		*color = 0;
		return(TRUE);
		}
	if ((pt[pn].pw.lv[0] & bmb.lv[0] | 
		pt[pn].pb.lv[0] & bmw.lv[0] | 
		pt[pn].pe.lv[0] & bme.lv[0]) == 0L &&
	   (pt[pn].pw.lv[1] & bmb.lv[1] | 
	    pt[pn].pb.lv[1] & bmw.lv[1] | 
	    pt[pn].pe.lv[1] & bme.lv[1]) == 0L) {
		/* found a color reversed match */
		*color = 1;
		return(TRUE);
		}
	return(FALSE);
	}

#endif
	
/* does pattern p with orientation o match attributes at square s? */
/* return army number of referenced armies in a1 and a2 */
/* return square of handles in h1,h2,h3 */

static int attsmatch(int p, int o, sqr_t s, army_t *a1, army_t *a2, sqr_t *h1, sqr_t *h2, sqr_t *h3, listval_t ldrno)
{
	int i, sn, at, av, retval = TRUE, live;
	*a1 = *a2 = NOARMY;
	*h1 = *h2 = *h3 = NOSQUARE;
	/* if (patoffedge(p,o,s))return(FALSE); Not needed - types allowed off, main patmatch already checks */
	for (i = 0; i < NUMATTS; ++i) { /* add && retval == TRUE, for speed? */
		if ((pt[p].attsqr[i] & 0xff) == 0xff)
			break;
		sn = patgetsqr(s,o,pt[p].attsqr[i]);
		if (ldrno != NOGROUP && board[sn] != NOGROUP && 
			addlist(ldrno, &ldrflag[mvs[grpieces[board[sn]]]]))
			adflist(mvs[grpieces[board[sn]]],&grldr[ldrno]);
		at = pt[p].attval[i]  & 0xe0;
		av = pt[p].attval[i] & 0x1f;
		switch (at) {
		case AT_GTALIVE:
			live = S_ALIVE(sn) & 0x1f;  /* don't trim of 32 bit, for accuracy.  always do a life first, or only look alive == 25 */
			switch (av) {
			case AT_THICK:
				if (live <= VERY_ALIVE)
					retval = FALSE;
				break;
			case AT_ALIVE:
				if (live <= ALIVE)
					retval = FALSE;
				break;
			case AT_UNSETTLED:
				if (live <= UNSETTLED)
					retval = FALSE;
				break;
			case AT_WEAK:
				if (live <= WEAK)
					retval = FALSE;
				break;
			case AT_DEAD:
				if (live < WEAK)
					retval = FALSE;
				break;
			default:
				if (live <= av)
					retval = FALSE;
			}
			break;
		case AT_LTALIVE:
			live = S_ALIVE(sn) & 0x1f;
			switch (av) {
			case AT_THICK:
				if (live > VERY_ALIVE)
					retval = FALSE;
				break;
			case AT_ALIVE:
				if (live > VERY_ALIVE)
					retval = FALSE;
				break;
			case AT_UNSETTLED:
				if (live > ALIVE)
					retval = FALSE;
					break;
			case AT_WEAK:
				if (live > UNSETTLED)
					retval = FALSE;
				break;
			case AT_DEAD:
				if (live >= WEAK)
					retval = FALSE;
				break;
			default:
				if (live >= av)
					retval = FALSE;
			}
			break;
		case AT_EQALIVE:
			live = S_ALIVE(sn) & 0x1f;
			switch (av) {
			case AT_THICK:
				if (live > VERY_ALIVE)
					retval = FALSE;
				break;
			case AT_ALIVE:
				if (live > ALIVE)
					retval = FALSE;
				break;
			case AT_UNSETTLED:
				if (live <= ALIVE || live > UNSETTLED)
					retval = FALSE;
				break;
			case AT_WEAK:
				if (live <= UNSETTLED || live > WEAK)
					retval = FALSE;
				break;
			case AT_DEAD:
				if (live <= WEAK)
					retval = FALSE;
				break;
			default:
				if (S_ALIVE(sn) != av)
					retval = FALSE;
			}
			break;
		case AT_GTLIB:
			if (S_NUMLIBS(sn) <= av)
				retval = FALSE;
			break;
		case AT_LTLIB:
			if (S_NUMLIBS(sn) >= av)
				retval = FALSE;
			break;
		case AT_EQLIB:
			if (S_NUMLIBS(sn) != av)
				retval = FALSE;
			break;
		case AT_NONE:
			switch (av) {
			case AT_THREATENED:
				if (!S_THREATENED(sn))
					retval = FALSE;
				break;
			case AT_NOTTHREATENED:
				if (S_THREATENED(sn) == 2)
					retval = FALSE;
				break;
			case AT_STABLE:
				if (S_THREATENED(sn))
					retval = FALSE;
				break;
			case AT_G1:
				*a1 = S_ARMY(sn);
				break;
			case AT_G2:
				*a2 = S_ARMY(sn);
				break;
			case AT_H1:
				*h1 = sn;
				break;
			case AT_H2:
				*h2 = sn;
				break;
			case AT_H3:
				*h3 = sn;
				break;
			}
			break;
		}
	}
	return retval;
}

/* return TRUE if pattern p orientation o at s is off edge */

static int patoffedge(int p, int o,sqr_t s)
{
	int x,y;
	x = ((pt[p].size >> 4) & 0xf)-1;
	y = (pt[p].size & 0xf)-1;
	switch (o) {
		  case 0:
			  if (xval[s]+x >= boardsize || yval[s] + y >= boardsize)
				  return(TRUE);
			  break;
		  case 1:
			  if (xval[s]-x < 0 || yval[s] + y >= boardsize)
				  return(TRUE);
			  break;
		  case 2:
			  if (xval[s]+y >= boardsize || yval[s] + x >= boardsize)
				  return(TRUE);
			  break;
		  case 3:
			  if (xval[s]+y >= boardsize || yval[s] - x < 0)
				  return(TRUE);
			  break;
		  case 4:
			  if (xval[s]+x >= boardsize || yval[s] - y < 0)
				  return(TRUE);
			  break;
		  case 5:
			  if (xval[s]-x < 0 || yval[s] - y < 0)
				  return(TRUE);
			  break;
		  case 6:
			  if (xval[s]-y < 0 || yval[s] + x >= boardsize)
				  return(TRUE);
			  break;
		  case 7:
			  if (xval[s]-y < 0 || yval[s] - x < 0)
				  return(TRUE);
			  break;
	}
	return FALSE;
}

/* return square on board for xy offset from s for orientation o */

sqr_t patgetsqr(sqr_t s, int o, char xy)
{
	int x, y;
	x = (xy >> 4) & 0xf;
	y = xy & 0xf;
	switch (o) {
		  case 0:
			  return s + x + y * boardsize;
		  case 1:
			  return s - x + y * boardsize;
		  case 2:
			  return s + y + x * boardsize;
		  case 3:
			  return s + y - x * boardsize;
		  case 4:
			  return s + x - y * boardsize;
		  case 5:
			  return s - x - y * boardsize;
		  case 6:
			  return s - y + x * boardsize;
		  case 7:
			  return s - y - x * boardsize;
	}
	return 0;
}

/* put a bit map in bmw, bmb, bme at location x,y in pattern space
 * (pattern upper left corner),
 * orientation o.  return hash.
 */ 
 
 
 /* offset from edge of board bits to edge of patterns */
#define POFF 5 
 
# define MBIT(x) (((x)+POFF)&0x7)
# define MBYTE(x) (((x)+POFF)>>3)
		/* was +5 */

static int _fastcall
makebitmap(int x, int y, int o)
{
	int i, bt, shift;
	unsigned char *p;
	bt = MBYTE(x);
	shift = MBIT(x);
	if (o <= 3) {
		y += 7;
		p = (unsigned char *)&bdw[o][shift][bt][y];
		for (i = 0; i < 8; ++i)
			bmw.cv[i] = *p++;
		p = (unsigned char *)&bdb[o][shift][bt][y];
		for (i = 0; i < 8; ++i)
			bmb.cv[i] = *p++;
		p = (unsigned char *)&bde[o][shift][bt][y];
		for (i = 0; i < 8; ++i)
			bme.cv[i] = *p++;
	}
	else {
		o -= 4;
		y = boardsize+6-y;
		p = (unsigned char *)&bdw[o][shift][bt][y];
		for (i = 0; i < 8; ++i)
			bmw.cv[i] = *p--;
		p = (unsigned char *)&bdb[o][shift][bt][y];
		for (i = 0; i < 8; ++i)
			bmb.cv[i] = *p--;
		p = (unsigned char *)&bde[o][shift][bt][y];
		for (i = 0; i < 8; ++i)
			bme.cv[i] = *p--;
	}
	return BHASHFUNC(bme.cv);
}

/* set up the board bit maps to all empty - off the board set neither */

void initboardbits(int bs)
{
	int i,j,k,l;
	sqr_t s;
	for (i = 0; i < 4; ++i) { /* orientations */
		for (j = 0; j < 8; ++j) { /* shifts */
			for (k = 0; k < 4; ++k) {  /* byte number */
				for (l = 0; l < 32; ++l) { 
					bdw[i][j][k][l] = 0xff;
					bdb[i][j][k][l] = 0xff;
					bde[i][j][k][l] = 0xff;
				}
			}
		}
	}
	for (s = 0; s < bs; ++s)
		delbits(s,NOCOLOR);   /* set board to empty */
}

	
/* add a stone to the board bit maps */

void addbits(sqr_t s, int c) {
	int x,y,i;
	x = xval[s];
	y = yval[s];
	for (i = 0; i < 8; ++i) {
		if (x-i >= -POFF)
			bde[0][i][MBYTE(x-i)][y+7] &= ~cbitmask[MBIT(x-i)];
		if (boardsize-x-1-i >= -POFF)
			bde[1][i][MBYTE(boardsize-x-1-i)][y+7] &= ~cbitmask[MBIT(boardsize-x-1-i)];
		if (y-i >= -POFF)
			bde[2][i][MBYTE(y-i)][x+7] &= ~cbitmask[MBIT(y-i)];
		if (boardsize-y-1-i >= -POFF)
			bde[3][i][MBYTE(boardsize-y-1-i)][x+7] &= ~cbitmask[MBIT(boardsize-y-1-i)];
		if (c == WHITECOLOR) {
			if (x-i >= -POFF)
				bdw[0][i][MBYTE(x-i)][y+7] |= cbitmask[MBIT(x-i)];
			if (boardsize-x-1-i >= -POFF)
				bdw[1][i][MBYTE(boardsize-x-1-i)][y+7] |= cbitmask[MBIT(boardsize-x-1-i)];
			if (y-i >= -POFF)
				bdw[2][i][MBYTE(y-i)][x+7] |= cbitmask[MBIT(y-i)];
			if (boardsize-y-1-i >= -POFF)
				bdw[3][i][MBYTE(boardsize-y-1-i)][x+7] |= cbitmask[MBIT(boardsize-y-1-i)];
			}
		else {
			if (x-i >= -POFF)
				bdb[0][i][MBYTE(x-i)][y+7] |= cbitmask[MBIT(x-i)];
			if (boardsize-x-1-i >= -POFF)
				bdb[1][i][MBYTE(boardsize-x-1-i)][y+7] |= cbitmask[MBIT(boardsize-x-1-i)];
			if (y-i >= -POFF)
				bdb[2][i][MBYTE(y-i)][x+7] |= cbitmask[MBIT(y-i)];
			if (boardsize-y-1-i >= -POFF)
				bdb[3][i][MBYTE(boardsize-y-1-i)][x+7] |= cbitmask[MBIT(boardsize-y-1-i)];
			}
		}
	}

/* delete a stone from the board bit maps */


void delbits(sqr_t s, int c)
{
	int x, y, i;
	x = xval[s];
	y = yval[s];
	for (i = 0; i < 8; ++i) {
		if (x-i >= -POFF)
			bde[0][i][MBYTE(x-i)][y+7] |= cbitmask[MBIT(x-i)];
		if (boardsize - x - 1 - i >= -POFF)
			bde[1][i][MBYTE(boardsize - x - 1 - i)][y + 7] |= cbitmask[MBIT(boardsize - x - 1 - i)];
		if (y-i >= -POFF)
			bde[2][i][MBYTE(y - i)][x + 7] |= cbitmask[MBIT(y-i)];
		if (boardsize-y-1-i >= -POFF)
			bde[3][i][MBYTE(boardsize-y-1-i)][x+7] |= cbitmask[MBIT(boardsize-y-1-i)];
		if (c != BLACKCOLOR) {
			if (x-i >= -POFF)
				bdw[0][i][MBYTE(x-i)][y+7] &= ~cbitmask[MBIT(x-i)];
			if (boardsize-x-1-i >= -POFF)
				bdw[1][i][MBYTE(boardsize-x-1-i)][y+7] &= ~cbitmask[MBIT(boardsize-x-1-i)];
			if (y-i >= -POFF)
				bdw[2][i][MBYTE(y-i)][x+7] &= ~cbitmask[MBIT(y-i)];
			if (boardsize-y-1-i >= -POFF)
				bdw[3][i][MBYTE(boardsize-y-1-i)][x+7] &= ~cbitmask[MBIT(boardsize-y-1-i)];
		}
		if (c != WHITECOLOR) {
			if (x-i >= -POFF)
				bdb[0][i][MBYTE(x-i)][y+7] &= ~cbitmask[MBIT(x-i)];
			if (boardsize-x-1-i >= -POFF)
				bdb[1][i][MBYTE(boardsize-x-1-i)][y+7] &= ~cbitmask[MBIT(boardsize-x-1-i)];
			if (y-i >= -POFF)
				bdb[2][i][MBYTE(y-i)][x+7] &= ~cbitmask[MBIT(y-i)];
			if (boardsize-y-1-i >= -POFF)
				bdb[3][i][MBYTE(boardsize - y - 1 - i)][x + 7] &= ~cbitmask[MBIT(boardsize - y - 1 - i)];
		}
	}
}

/* color is color to move */
/* clr is color of move in original pattern we are interested in */
/* return list of moves matched */
	
list_t getpatmoves(int type, int color, int clr)
{
	army_t a1, a2;
	sqr_t h1, h2, h3;
	int p, mp;
	sqr_t s, sqr;
	list_t ptr, ret = EOL;

	for (s = 0; s < boardsquare; ++s) {
		for (ptr = newpatbrd[s]; ptr != EOL; ptr = link[ptr]) {
			if (attsmatch(newpat[list[ptr]].num, newpat[list[ptr]].orient, s, &a1, &a2, &h1, &h2, &h3, NOGROUP) ||
				pt[newpat[list[ptr]].num].moves != newpat[list[ptr]].move) {
					p = list[ptr];
					if (!newpat[p].color && color != clr)
						continue;
					if (newpat[p].color && color == clr)
						continue;
					if ((pt[newpat[p].num].wheretype & TYPEMASK) != type)
						continue;
					if (type == PT_CUT && (a1 == NOARMY || a2 == NOARMY ||
						a1 == a2 || A_ALIVE(a1) <= VERY_ALIVE &&
						A_ALIVE(a2) <= VERY_ALIVE))
						continue;
					if (type == PT_SURROUND && (color == newpat[p].color &&
						(h1 != NOSQUARE && S_ALIVE(h1) >= WEAK ||
						h2 != NOSQUARE && S_ALIVE(h2) >= WEAK) ||
						A_ALIVE(a1) == DEAD))
						continue;
					for (mp = newpat[p].move; mp != -1; mp = patmore(mp)) {
						if (PM_VALUE(mp) == 0)
							continue;  
						if (PM_COLOR(mp) != clr)
							continue;
						sqr = patgetsqr(newpat[p].sqr, newpat[p].orient,pm[mp].xy);
						addlist(sqr, &ret);
					}
			}
		}
	}
	return ret;
}



int numpatvariations;

/* fire strat rules for all the matching patterns for color to move
 */
void genpatmoves(int color, int passval)
{
	army_t a1, a2;
	sqr_t s, h1, h2, h3;
	list_t ptr;
	
	for (s = 0; s < boardsquare; ++s) {
		for (ptr = newpatbrd[s]; ptr != EOL; ptr = link[ptr]) {
			if (attsmatch(newpat[list[ptr]].num, newpat[list[ptr]].orient, s, &a1, &a2, &h1, &h2, &h3, NOGROUP) ||
				pt[newpat[list[ptr]].num].moves != newpat[list[ptr]].move && playlevel >= PATMOVELEVEL) {
				fireapat(list[ptr], a1, a2, h1, h2, h3, color, passval);
			}
		}
	}
}

/* make moves selected by newpat p 
 * fire rule with p as parameter
 * Pattern bonus is not added here since mutiple similar patterns would be
 * double counted.  Pattern bonus is added during lookahead.
 */

static void fireapat(int p, army_t a1, army_t a2, int h1, int h2, int h3, int mvcolor, int passval)
{
	int mp, color, scr = 0, val = 0, defv = 0, aval, cutval, guess;
	sqr_t sqr;
#ifdef CHECK
	char buf[80];
#endif
	h3 = h3;
	if (boardsize > 9 && (pt[newpat[p].num].size & 0xf) > playlevel + 3)
		return; /* small pats only at low levels */
	if (boardsize > 9 && ((pt[newpat[p].num].size >> 4) & 0xf) > playlevel + 3)
		return;
	if (newpat[p].color) {
		color = 1 - mvcolor;
	} else {
		color = mvcolor;
	}
	for (mp = newpat[p].move; mp != -1; mp = patmore(mp)) {
		if (PM_COLOR(mp) != color) {
			continue;
		}
		val = PM_VALUE(mp) * 50;
		if (val == 0)
			val = -1000;
		else if ((PM_VAL(mp)) >= 10) {	/* pattern says is urgent */
			val += 300;
		} else if (playlevel < PATVALLEVEL) {
			val = 100;		/* lower level don't know which moves are best */
		}
		sqr = patgetsqr(newpat[p].sqr, newpat[p].orient, pm[mp].xy);
		if (board[sqr] != NOGROUP) {
#ifdef CHECK
			sprintf(buf,"Bad move in pattern sequence, pat %d\n",newpat[p].num);
			outerror(buf);
			turnoffcplay();
#endif			
			continue;
		}
		/*			stratguess[sqr] += val; shouldn't be double counting this */
		switch(pt[newpat[p].num].wheretype & TYPEMASK) {
		case PT_NORMAL:
			if (problemflag != 1) {
				fire_strat_rule(sqr, PATMATCH, val, p, val / 2);
			}
			break;
		case PT_ENDGAME:
			fire_strat_rule(sqr, ENDPATMATCH, val, p, 50 + val / 2);
			break;
		case PT_CUT:
			if (playlevel == 0 && 
				((pt[newpat[p].num].size & 0xf) > 3 ||
				 (pt[newpat[p].num].size >> 4) & 0xf) > 3)
				 break;		/* no complex cuts at lowest level */
			  /* a1, a2 armies being cut/connected */
			  /* if h1,h2 - the cut also connects h1/h2 */
			  /* if h3, save/kill cutting stones */
			  if (newpat[p].move == pt[newpat[p].num].moves) {
				  if (a1 == NOARMY || a2 == NOARMY || a1 == a2)
					  break;
				  if (mvcolor == newpat[p].color) {  /* cut */
					  if (A_THREATENED(a1) == 2 || A_THREATENED(a2) == 2 || A_ALIVE(a1) == DEAD ||
						  A_ALIVE(a2) == DEAD)  /* rather capture than cut */
						  break;
					  guess = 0;
					  if (val > 0) {
						  scr = cut_val(a1, a2);
						  if (scr > val)  /* if no cut val for group, don't add value */
							  scr += val;
						  else
							  scr += scr;
						  if (h1 != NOSQUARE && h2 != NOSQUARE) {
							  guess = conn_val(S_ARMY(h1), S_ARMY(h2));
							  scr += guess;
						  }
						  if (safecaptarmy(a1, 1 - A_COLOR(a1) ,0))  
							  scr += safecaptval(a1, passval); 
						  if (safecaptarmy(a2, 1 - A_COLOR(a2), 0))
							  scr += safecaptval(a2, passval);
					  }
					  else 
						  scr = val;
					  fire_strat_rule(sqr, PATCUT, scr, p, guess);
				  }
				  else {  /* connect */
					  if (val < 0) {
						  scr = val;
					  }
					  else {
						  scr = conn_val(a1, a2);
						  /* connection between live and dead group */
						  if (A_ALIVE(a1) <= ALIVE &&
							  A_ALIVE(a2) > STRONG_SEMEAI &&
							  !urgalive[A_ALIVE(a2)] && armysize[a2] + armylibs[a2] > 3 ||
							  A_ALIVE(a2) <= ALIVE &&
							  A_ALIVE(a1) > STRONG_SEMEAI &&
							  !urgalive[A_ALIVE(a1)] && armysize[a1] + armylibs[a1] > 3)
							  scr += 300;
							  if (!moveispotconn(sqr, a1) &&
							  !moveispotconn(sqr,a2)) {
								  if (!A_THREATENED(a1) && 
									  (A_ALIVE(a2) <= ALIVE || armyeyespace[a2] >= 8 || armyrn_pot[a2] >= LIMP_RUN)) {
										  defv = grdefval[list[armygroups[a1]]];
										  if (defv > 0)
											  fire_strat_rule(sqr, PATCONNDEF, defv, A_KEYPOINT(a1), 0); 
								  }
								  if (!A_THREATENED(a2) && 
									  (A_ALIVE(a1) <= ALIVE || armyeyespace[a1] >= 8 || armyrn_pot[a1] >= LIMP_RUN)) {
										  defv = grdefval[list[armygroups[a2]]];
										  if ( defv > 0)
											  fire_strat_rule(sqr, PATCONNDEF, defv, A_KEYPOINT(a2), 0); 
								  }
						  }
					  }
					  if (h3 != NOSQUARE) {
						  scr += gratkval[G_ARMY(h3)];
						  fire_strat_rule(sqr, PATKILLCUTSTONES, scr + val, p, 0);
						  if (S_THREATENED(h3) == 2) {	/* just capture the group, pattern not worth much */
							  scr = 0;
							  if (val > 100)
								  val = 100;
						  }
					  }
					  else if (scr != 0)
						  fire_strat_rule(sqr, PATCONNECT, scr + val - 50, p, 0);  /* -50 since conn_val has whole value - no extra add - makes it conenct kos too much */
				  }
			  }
			  else {
				  if (val < 0) {
					  scr = val;
				  }
				  else scr = defv + val;
				  if (mvcolor == newpat[p].color)  /* cut */
					  fire_strat_rule(sqr, PATCUTSEQUENCE, scr, p, 0);
				  else
					  fire_strat_rule(sqr, PATSEQUENCE, scr, p, 0);
			  }
			  break;
		case PT_SURROUND:
			if (playlevel <= 1)
				break;
			if (mvcolor == newpat[p].color) {  /* surround */
				if (h1 != NOSQUARE && S_ALIVE(h1) >= WEAK && playlevel >= PATSURRLEVEL)
					break;
				if (h2 != NOSQUARE && S_ALIVE(h2) >= WEAK && playlevel >= PATSURRLEVEL)
					break;
				/* can't surround from weak groups */
				if (A_ALIVE(a1) == DEAD)
					break;  /* don't surround dead groups */
				if (val < 0) {
					scr = val;
				}
				else {
					scr = val;
					if (armyrn_pot[a1] < EASY_RUN &&
						A_ALIVE(a1) != UNSETTLED_DEAD  && A_ALIVE(a1) > VERY_ALIVE ) {
							aval = gratkval[list[armygroups[a1]]];
							if (armyrn_pot[a1] >= LIMP_RUN)
								aval = (aval * (EASY_RUN - armyrn_pot[a1])) / (EASY_RUN-LIMP_RUN);
							if (A_ALIVE(a1) <= UNSETTLED && armyrn_pot[a1] < 4)
								aval = (aval * armyrn_pot[a1]) / 4;
							scr += aval;
					}
					if (A_ALIVE(a1) >= WEAK && playlevel >= PATSURRLEVEL)
						scr /= 4;
					if (A_THREATENED(a1) && playlevel >= PATSURRLEVEL)
						scr /= 4;  /* don't surround threatened group, just catch it */
				}
				if (problemflag == 1)	/* not important for life/death problems */
					scr /= 4;
				if (edge[sqr] <= 2 && h2 != NOSQUARE)
					scr = 0;	/* running toward the edge of the board */
				fire_strat_rule(sqr, PATSURROUND, scr, p, 0);
			}
			else { /* escape */
				if (A_ALIVE(a1) == DEAD)
					break;  /* don't escape with dead group */

				if (h1 != NOSQUARE && S_ALIVE(h1) == DEAD || 
					h2 != NOSQUARE && S_ALIVE(h2) == DEAD)
					break;  /* can't be surrounded by dead groups */
				cutval = 0;
				if (val < 0) {
					scr = val;
				}
				else {
					scr = val;
					if (problemflag == 1)
						scr /= 3;	/* don't count for much when doing life and death */
					if ((h1 != NOSQUARE && S_ALIVE(h1) >= WEAK) &&
						(h2 == NOSQUARE || S_ALIVE(h2) >= WEAK))
						scr /= 2;  /* no need to run through dead stones */
					if (edge[sqr] >= 3 || h2 == NOSQUARE) {
						scr += grdefval[list[armygroups[a1]]];  /* don't jump between groups to edge to run! */
					}
					if (h1 != NOSQUARE && h2 != NOSQUARE &&
						S_ARMY(h1) != S_ARMY(h2)) {
							cutval = cut_val(S_ARMY(h1),S_ARMY(h2))/2;  /* if real cut, cutval added elsewhere */
					}
					if (A_ALIVE(a1) == SEMEAI || 
						A_ALIVE(a1) == STRONG_SEMEAI ||
						A_ALIVE(a1) == UNSETTLED_DEAD ||
						A_ALIVE(a1) == VERY_ALIVE) /* not so urgent to run away */
						scr /= 2;  /* fight back in this case */
				}
				if (edge[sqr] <= 2 && h2 != NOSQUARE)
					scr = 0;	/* running toward the edge of the board */
				fire_strat_rule(sqr, PATRUN, scr, p, 0);
				if (cutval > 0)
					fire_strat_rule(sqr, RUNCUTS, cutval, sqr, 0);
			}
			break;
		case PT_KILL:
			if (newpat[p].move == pt[newpat[p].num].moves) {
				if (mvcolor == newpat[p].color) {  /* kill */
					scr = gratkval[list[armygroups[a1]]];  /* careful about double counts */
					fire_strat_rule(sqr, PATKILL,val,p,0);
				}
				else { /* save */
					if (val < 0) {
						scr = val;
					}
					else if (A_ALIVE(a1) > ALIVE)
						scr = val + grdefval[list[armygroups[a1]]];
					fire_strat_rule(sqr, PATSAVE, scr, p, 0);
				}
			}
			else if (playlevel >= 3)
				fire_strat_rule(sqr, PATSEQUENCE, defv + val, p, 0);
			break;
		case PT_INVADE:
			if (playlevel == 0)
				break;
			if (newpat[p].move == pt[newpat[p].num].moves) {
				if (g2abs(terv[sqr]) > 35)
					val = val * (65-g2abs(terv[sqr]))/(65-35);	/* reduce value if territory solid */
				if (mvcolor == newpat[p].color && problemflag != 1)  /* invade (unless solving a problem) */
					fire_strat_rule(sqr, PATINVADE, val, p, val / 2);
				else if (problemflag != 1) { /* defend against invasion */
					if (val < 0) {       /* ignore for problem solving */
						scr = val;
					}
					else scr = defv + val;
					fire_strat_rule(sqr, PATDEFEND, scr, p, scr / 2);
				}
			}
			else if (playlevel >= 3 && boardsize >= 9) {
				if (val < 0) {
					scr = val;
				}
				else scr = defv+val;
				fire_strat_rule(sqr, PATINVSEQUENCE,scr,p,scr/2);
			}
			break;
		}
	}
}

/* return TRUE if move at s is potential connection eye making move for army */

static int moveispotconn(sqr_t s, army_t army)
{
	list_t tmplist;
	list_t ptr;
	for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
		if (pots[list[ptr]].pot_type != CONNECT)
			continue;
		tmplist = adpot(army, list[ptr]);
		if (inflist(s, &tmplist)) {
			killist(&tmplist);
			return TRUE;
		}
		killist(&tmplist);
	}
	return FALSE;
}
