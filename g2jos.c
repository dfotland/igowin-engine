/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2jos.h"
#include "g2hd.h"
#include "g2rldef.h"
#include "g2proto.h"
#ifdef G2DEBUGOUTPUT
#include <stdio.h>
#endif
#ifdef TEST
#include <string.h>
#endif
#include "g2jos.pr"
#define MINJOSBOARD 17
#define MAXJFLAG 3

#ifndef __STDC__
void jupdatec();
void getxyjlib();
#endif

extern unsigned char xymaptmp[62];
extern unsigned char jlib2tmp[50000];
extern unsigned int numjosbytestmp;

static unsigned char *jlib2, *xymap;
static unsigned int numjosbytes;
extern int numcmoves[4];

char josx[4], josy[4];  /* closest stone to corner each direction 
						  that is not a joseki move.  X and Y refer
						  to values in the joseki library, not values
						  on the board. */

static int savejcolor[NUMMOVES][4];
static int savejreflect[NUMMOVES][4];
static int savejptr[NUMMOVES][4];
static int savejflag[NUMMOVES][4];
static int savejosx[NUMMOVES][4];
static int savejosy[NUMMOVES][4];
static int savenumcmoves[NUMMOVES][4];

void initjlib(void)
{
	jlib2 = jlib2tmp;
	xymap = xymaptmp;
	numjosbytes = numjosbytestmp;
}

/*  format of joseki compressed library:
 *
 *  if first byte is 62, change color to move (a pass), and go to next byte
 *
 *  ffpppppp - f is flags, p is point (index into xymap array of point values)
 *  
 *  p == 63 (0x3f) adds a byte for real x and y
 *  ff0x63
 *  xxxxyyyy
 *
 *  ff - 0xc0 = extra flag byte (after xxxxyyyy byte if it exists), 
 *     - 0x40 no child
 *     - 0x80 has a sibling
 *     - 0x00
 *
 *  flag byte:
 *     - 0x10 - no child
 *     - 0x20 - next three bytes are index of first child
 */

/* initialize joseki flags. */

void initjflags(void)
{
	int i;
	for (i = 0; i < 4; ++i) {
		numcmoves[i] = 0;
	}
	for (i = 0; i < 4; ++i) {
		jflag[i] = 1;
		josx[i] = josy[i] = 19;
		if (boardsize < MINJOSBOARD) {
			jflag[i] = 9;
		}
		jptr2[i] = 0;
		jreflect[i] = 0;
		jcolor[i] = NOCOLOR;
	}
}


static int incorner(sqr_t s, int cn)
{
	int mid,x,y;
	mid = boardsize/2;
	x = xval[s];
	if (cn == 1 || cn == 3)
		x = boardsize-x-1;
	y = yval[s];
	if (cn > 1)
		y = boardsize-y-1;
	return x <= mid && y <= mid && x+y < mid+mid-3;
}

#ifdef G2DEBUGOUTPUT
void dumpjos(void)
{
	int i;
	char buf[80];
	for (i = 0; i < 4; ++i) {
		sprintf(buf,"Corner %d: jflag %d, josx %d, josy %d\n",
			i,jflag[i],(int)josx[i],(int)josy[i]);
		outerr(buf);
	}
}
#endif

/* jupdate updates jptr, jptr2, jreflect, jcolor for all corners when 
 * a move at s, color c is made.  update has already been called, so msptr-1
 * is the move that was made
 */

void jupdate(sqr_t s, int c)
{
	int corner, cn, in;
	/* save the joseki data */
	for (cn = 0; cn < 4; ++cn) {
		savejcolor[msptr-1][cn] = jcolor[cn];
		savejreflect[msptr-1][cn] = jreflect[cn];
		savejptr[msptr-1][cn] = jptr2[cn];
		savejflag[msptr-1][cn] = jflag[cn];
		savejosx[msptr-1][cn] = josx[cn];
		savejosy[msptr-1][cn] = josy[cn];
		savenumcmoves[msptr-1][cn] = numcmoves[cn];
	}
	if (s == PASS) {
		return;
	}
	for (corner = 0; corner < 4; ++corner) {
		in = incn(s, corner, 1);
		if (in) {
			numcmoves[corner]++;
			if (numcmoves[corner] == 1) {
				jcolor[corner] = S_COLOR(s);
			}
		}
		if (jflag[corner] <= MAXJFLAG) {
			jupdatec(s, c, corner);
		} else if (in) {
			jflag[corner]++;
		}
	}
}

void jdndate(void)
{
	int cn;
	for (cn = 0; cn < 4; ++cn) {
		jcolor[cn] = savejcolor[msptr][cn];
		jreflect[cn] = savejreflect[msptr][cn];
		jptr2[cn] = savejptr[msptr][cn];
		jflag[cn] = savejflag[msptr][cn];
		josx[cn] = savejosx[msptr][cn];
		josy[cn] = savejosy[msptr][cn];
		numcmoves[cn] = savenumcmoves[msptr][cn];
	}
}
	
void jupdatec(sqr_t s, int c, int corner)
{
	int x,y,xtmp,ytmp,tmp,found,lastcolor;
	unsigned int ptr,ptr2;
	x = xval[s] + 1;
	y = yval[s] + 1;
	if (corner > 1)
		y = boardsize+1-y;
	if (corner%2 == 1)
		x = boardsize+1-x;

	if (x > 15 || y > 15 || x+y > 18)
		return;

	if (jreflect[corner] == 1) {
		tmp = x;
		x = y;
		y = tmp;
	}

	if (jptr2[corner] == 0) {  /* first move in corner */
		lastcolor = 1-c;
		jcolor[corner] = c;
	}
	else {
		lastcolor = getlastcolor(corner);
		if (lastcolor == NOCOLOR)
			lastcolor = 1-c;
	}

	if (lastcolor == NOCOLOR) {
		if (jflag[corner] < 20)
			jflag[corner]++;
		return;
	}

	found = FALSE;
	for (ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)) {
		getxyjlib(ptr,&xtmp,&ytmp);
		if (xtmp == 0) {
			if (lastcolor != c)
				continue;
			ptr2 = j2next(ptr);
			getxyjlib(ptr2,&xtmp,&ytmp);
			if (xtmp == 0)
				continue;
			if (x == xtmp && y == ytmp) {
				jptr2[corner] = ptr2;
				if (jreflect[corner] == 0 && x != y)
					jreflect[corner] = 2;
				found = TRUE;
				break;
			}
			if (jreflect[corner] == 0 &&
			   x == ytmp && y == xtmp) {
				jptr2[corner] = ptr2;
				if (x != y)
					jreflect[corner] = 1;
				found = TRUE;
				break;
			}
			continue;
		}
				
		if (lastcolor == c)
			continue;
		if (x == xtmp && y == ytmp) {
			jptr2[corner] = ptr;
			if (jreflect[corner] == 0 && x != y)
				jreflect[corner] = 2;
			found = TRUE;
			break;
		}
		if (jreflect[corner] == 0 &&
		   x == ytmp && y == xtmp) {
			jptr2[corner] = ptr;
			if (x != y)
				jreflect[corner] = 1;
			found = TRUE;
			break;
		}
	}


	if (!found && incn(s,corner,1)) {
		jflag[corner]++;
		if (x > y && x < josx[corner])
			josx[corner] = x;
		if (y > x && y < josy[corner])
			josy[corner] = y;
	}
	else {
		if (getflag(jptr2[corner]) & 0x40)
			jreflect[corner] = 0;
	}
}

/* return first joseki move in corner */

int firstjos(int corner, int color)
{
	int x, y, lastcolor, type, xtmp, ytmp, tmp;
	unsigned int ptr, xyptr;
	lastcolor = getlastcolor(corner);
	if (lastcolor == NOCOLOR)
	lastcolor = 1-color;
	for (ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)) {
		xyptr = ptr;
		getxyjlib(xyptr, &x, &y);
		if (x == 0) {
			if (lastcolor != color)
				continue;
			xyptr = j2next(ptr);
			getxyjlib(xyptr, &x, &y);
			if (x == 0)
				continue;
		}
		else if (lastcolor == color)
			continue;
		x--;
		y--;
		if (x >= boardsize || y >= boardsize)
			continue;
		type = getflag(xyptr) & 0xf;
		if (type == BAD || type == IGNR)
			continue;
		if (jreflect[corner] <= 1) {
			tmp = x;
			x = y;
			y = tmp;
		}
		xtmp = x;
		ytmp = y;
		if (corner > 1)
			ytmp = boardsize - 1 - ytmp;
		if (corner%2 == 1)
			xtmp = boardsize - 1 - xtmp;
		return ytmp * boardsize + xtmp;
	}
	return(NOSQUARE);
}

/* return the joseki after sqr in corner for color */

int nextjos(sqr_t sqr, int corner, int color)
{
	int x, y, lastcolor, type, xtmp, ytmp, tmp;
	int found = FALSE;
	sqr_t s, firstj = NOSQUARE;
	unsigned int ptr, xyptr;
	lastcolor = getlastcolor(corner);
	if (lastcolor == NOCOLOR)
		lastcolor = 1-color;
	for (ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)) {
		xyptr = ptr;
		getxyjlib(xyptr, &x, &y);
		if (x == 0) {
			if (lastcolor != color)
				continue;
			xyptr = j2next(ptr);
			getxyjlib(xyptr,&x,&y);
			if (x == 0)
				continue;
		}
		else if (lastcolor == color)
			continue;
		x--;
		y--;
		if (x >= boardsize || y >= boardsize)
			continue;
		type = getflag(xyptr) & 0xf;
		if (type == BAD || type == IGNR)
			continue;
		if (jreflect[corner] <= 1) {
			tmp = x;
			x = y;
			y = tmp;
		}
		xtmp = x;
		ytmp = y;
		if (corner > 1)
			ytmp = boardsize - 1 - ytmp;
		if (corner%2 == 1)
			xtmp = boardsize - 1 - xtmp;
		s = ytmp * boardsize + xtmp;
		if (found)
			return s;
		if (firstj == NOSQUARE)
			firstj = s;
		if (s == sqr)
			found = TRUE;
		if (jreflect[corner] == 0 && x != y) {
			getxyjlib(xyptr, &x, &y);
			x--;
			y--;
			if (corner > 1)
				y = boardsize-1-y;
			if (corner%2 == 1)
				x = boardsize - 1 - x;
			s = y * boardsize + x;
			if (found)
				return s;
			if (s == sqr)found = TRUE;
		}
	}
	return firstj;
}

/* get the color of the last move in the corner */
/* there is a bug.  during retract, the moves are played back to
 * find the previous joseki pointer, but the stones are left on the
 * board.  If there is a later capture, getlastcolor may return
 * nocolor incorrectly.
 */

int getlastcolor(int corner)
{
	int xtmp,ytmp,tmp,c;
	unsigned int ptr;
	if (jptr2[corner] == 0)
		return NOCOLOR;
	getxyjlib(jptr2[corner], &xtmp, &ytmp);
		
	if (jreflect[corner] == 1) {
		tmp = xtmp;
		xtmp = ytmp;
		ytmp = tmp;
	}
	
	--xtmp; 
	--ytmp;
	if (corner > 1)
		ytmp = boardsize - ytmp - 1;
	if (corner == 1 || corner == 3)
		xtmp = boardsize - xtmp - 1;
	c = S_COLOR(xtmp + boardsize * ytmp);
	ptr = jptr2[corner];
	if ((jlib2[ptr] & 0xc0) == 0xc0) {
		tmp = ptr;
		if ((jlib2[ptr] & 0x3f) == 63)
			ptr++;
		if (jlib2[ptr+1] & 0x20) {
			if (jlib2[ptr+1] & 0x10)
				c = 1 - c;  /* color reversal */
			if (numjosbytes <= 32767 && (jlib2[ptr+2] & 0x80))
				c = 1 - c;  /* color reversal old format */
			if ((jlib2[tmp] & 0x3f) == 62)
				c = 1-c;
		}
	}
	return c;
}

unsigned int getflag(unsigned int ptr) 
{
	if ((jlib2[ptr] & 0xc0) != 0xc0)
		return 0;
	if ((jlib2[ptr] & 0x3f) == 63)
		return jlib2[ptr + 2];
	return jlib2[ptr + 1];
}

unsigned int getjostype(int corner)
{
	return getflag(jptr2[corner]) & 0xf;
}

/* return the x and y values for this move in j2lib.  return 0,0 for
 * tenuki range is 1-15 for x and y.  
 */

void getxyjlib(unsigned int ptr, int *x, int *y) 
{
	unsigned int tmp;
	tmp = jlib2[ptr] & 0x3f;
	if (tmp < 62) {
		*x = (xymap[tmp] >> 4) & 0xf;
		*y = xymap[tmp] & 0xf;
	}
	else if (tmp == 63) {
		*x = (jlib2[ptr + 1] >> 4) & 0xf;
		*y = jlib2[ptr + 1] & 0xf;
	}
	else {
		*x = 0; 
		*y = 0;
	}
}

/* return index of first response to ptr.  return 0
 * if there is none
 */

unsigned int j2next(unsigned int ptr) 
{
	unsigned int tmp;
	tmp = jlib2[ptr];
	if ((tmp & 0xc0) == 0x40)
		return 0;
	if ((tmp & 0x3f) == 63)
		ptr++;
	if ((tmp & 0xc0) != 0xc0)
		return ptr + 1;
	++ptr;
	tmp = jlib2[ptr];
	if (tmp & 0x20) {	/* have child pointer in node for recovergence */
		tmp = jlib2[ptr + 1] << 16;
		tmp |= ((unsigned int)jlib2[ptr + 2] & 0xff) << 8;
		tmp |= ((unsigned int)jlib2[ptr + 3] & 0xff);
#ifdef NEVER
		if (numjosbytes <= (unsigned int)32767)
			tmp &= 0x7fff;  /* for old format smalljos.dat */
#endif
		return tmp;
	}
	if (tmp & 0x10)
		return 0;
	++ptr;
	return ptr ;
}

/* return TRUE if this is last move in sequence.
 * update ptr to index of next move in line in array
 */

int j2skip(unsigned int *ptr) 
{
	unsigned int tmp;
	tmp = jlib2[*ptr];
	(*ptr)++;
	if ((tmp & 0x3f) == 63)
		(*ptr)++;
	if ((tmp & 0xc0) == 0 || (tmp & 0xc0) == 0x80)
		return FALSE;
	if ((tmp & 0xc0) == 0x40)
		return TRUE;
	
	tmp = jlib2[*ptr];
	(*ptr)++;
	if (tmp & 0x20) {
		(*ptr) += 3;
		return TRUE;
	}
	if (tmp & 0x10)
		return TRUE;
	return FALSE;
}

unsigned int j2more(unsigned int ptr) 
{
	int scount = 0;
	unsigned int tmp;
	if (jlib2[ptr] == 62)
		j2skip(&ptr); 
	if (!sibling(ptr))
		return 0;
	do {
		tmp = jlib2[ptr++];
		if ((tmp & 0x3f) == 63)
			ptr++;
		if ((tmp & 0xc0) == 0x80)
			++scount;
		else if ((tmp & 0xc0) == 0x40)
			--scount;
		else if ((tmp & 0xc0) == 0xc0) {
			tmp = jlib2[ptr++];
			if (tmp & 0x20) {
				ptr += 3;
				--scount;
			}
			else if (tmp & 0x10)
				--scount;
			if (tmp & 0x80)
				++scount;
		}
	} while(scount > 0);
	return ptr;
}

/* return TRUE if move at ptr has sibling (only called from j2more) */

int sibling(unsigned int ptr) 
{
	unsigned int tmp;
	if (jlib2[ptr] == 62)
		ptr++;
	tmp = jlib2[ptr];
	if (!(tmp & 0x80))
		return FALSE;
	if ((tmp & 0xc0) == 0x80)
		return TRUE;
	if ((tmp & 0x3f) == 63)
		ptr++;
	return jlib2[ptr+1] & 0x80;
}

/* joseki finds all joseki moves on board and fires the
 * appropriate strategic rule for that joseki
 * return number of joseki moves scored and evaluated.
 * can tenuki as alternative to Followup, shimari, kakari move.
 * alpha is best score found so far
 */

static int foundjoseki[5];  /* found a joseki or kakari or shimari move by corner */
static int foundjosfol[5];

void initjosreasons(void) {
	int i;
	for (i = 0; i < 4; ++i) {
		foundjoseki[i] = foundjosfol[i] = FALSE;
   	}
}

void joseki(int color)
{
	int corner,cn;
	sqr_t lasts;
	int inlastcorner = FALSE;		/* corner containing last move */
	int type,i;
	int cfoll[4];  /* number of followup moves in this corner */

	for (i = 0; i < 4; ++i) {
		cfoll[i] = 0;
		foundjoseki[i] = foundjosfol[i] = FALSE;
   	}
	
	if (problemflag == 1) {
		return;
	}
	if ((sqr_t)msptr > boardsquare/2) {
		return;
	}
	if (boardsize < MINJOSBOARD) {
		return;
	}
	
	if (msptr == 0) {
		cn = NOCORNER;
		lasts = NOSQUARE;
	} else {
		cn = which_corner(mvs[msptr-1]);
		lasts = mvs[msptr-1];
	}
	
	for (corner = 0; corner < 4; corner++) {  /* do corner of last move first */
		if (incn(lasts, corner, 1)) {
			type = getflag(jptr2[corner]) & 0xf;
			inlastcorner = type == PINC || type == NORM ||
				type == URGT || type == BAD || 
				type == FRKA || type == FOLL || 
				type == COMP || type == TRIK || 
				type == KAKA;
		}
		josekicorner(cn, inlastcorner, color);
	}
	return;
}

/* evaluate joseki moves in corner.  cn is corner of last move.
 * return number of moves evaluated.
 */

void josekicorner(int corner, int cflag, int color)
{
	int x, y, lastcolor, bsz, tmp, xtmp, ytmp;
	unsigned int ptr, xyptr;

	if (jflag[corner] > MAXJFLAG) {
		return;
	}
	if (jptr2[corner] == 0) {
		return;  /* let empty_corner make move */
	}
	lastcolor = getlastcolor(corner);
	if (lastcolor == NOCOLOR) {
		lastcolor = 1-color;
	}
	bsz = boardsize/2 + 2;
	for (ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)) {
		xyptr = ptr;
		getxyjlib(xyptr, &x, &y);
		if (x == 0) {
			if (lastcolor != color)continue;
			xyptr = j2next(ptr);
			getxyjlib(xyptr, &x, &y);
			if (x == 0)continue;
		} else if (lastcolor == color) {
			continue;
		}
		x--;
		y--;
		if (x > bsz || y > bsz) {
			continue;
		}
		if (jreflect[corner] <= 1) {
			tmp = x;
			x = y;
			y = tmp;
		}
		xtmp = x;
		ytmp = y;
	    if (ytmp < josy[corner]-1 && xtmp < josx[corner]-1) {
	    	/* close enough to corner to not interfere with moves already on board */
			if (corner > 1) {
				ytmp = boardsize-1-ytmp;
			}
			if (corner%2 == 1) {
				xtmp = boardsize-1-xtmp;
			}
			fire_joseki(xyptr, (sqr_t)(ytmp*boardsize+xtmp), jflag[corner], cflag, corner, color);
		}
		if (jreflect[corner] == 0 && x != y) {
			getxyjlib(xyptr, &x, &y);
			x--;y--;
		    if (y < josy[corner]-1 && x < josx[corner]-1) {
				if (corner > 1) {
					y = boardsize-1-y;
				}
				if (corner%2 == 1) {
					x = boardsize-1-x;
				}
				fire_joseki(xyptr, (sqr_t)(y*boardsize+x), jflag[corner], cflag, corner, color);
			}
		}
		/* only generate one joseki move */
		if (playlevel == JOSEKILEVEL)
			return;
	}
	return;
}

/* return color of first stone in corner */

static int jcornercolor(int corner)
{
	if (corner == -1) {
		return(NOCOLOR);
	}
	return jcolor[corner];
}

/* return TRUE (1) if evaluated a move 
 * color is the side to move. cflag TRUE if this corner of last move 
 * jflg 1 if in joseki lib
 */

static void fire_joseki(unsigned int ptr, sqr_t s, int jflg, int cflag, int corner, int color)
{
	int val, type, pval;
	int realjoseki = TRUE;
	int evmv = FALSE;
	int josval = 500;
	int notenukival = 500;
	if (playlevel < 4)
		notenukival = 2000;
	if (board[s] != NOGROUP) {
		return;
	}
	type = getflag(ptr) & 0xf;
	if (jflg != 1 && type != BAD && type != IGNR && board[s] == NOGROUP) {
	    realjoseki = FALSE;
		fire_strat_rule(s, MAYBE_JOSEKI, 0, NOSQUARE, 0);
		josval = cflag ? 300 : 150;
		if (playlevel < 3)
			return;
	}
	if (jflg != 1)
		cflag = FALSE;  /* don't foce to finish joseki if not truely joseki */
	if (type == IGNR)
		return;  /* ignore this one */
	if (type == FRFR)
		val = 400;  /* for strat_rule, not strat_score */
	else 
		val = 200;
	if (jflg == 1) {
		if (type == FOLL || type == PINC || type == KAKA || 
			type == FRKA || type == SHIM) {
			foundjosfol[corner] = TRUE; 
		} else if (type != BAD && type != TRIK) {
			foundjoseki[corner] = TRUE;
		}
	}

	switch(type) {
	  case PINC:
		pval = 0;
		if (jcornercolor(pincerstonefacing(corner)) == color) {
			fire_strat_rule(s, PINCER_EXTENSION, 0, corner, 0);
			pval += 100;
		} else if (edge2[s] == 7)
			pval = -100;  /* don't come close when enemy behind */
		fire_strat_rule(s, JOSEKI, josval + pval, corner, 0);
		if (cflag)
			fire_strat_rule(s, JOSEKI_NO_TENUKI, notenukival, corner, 0);
		break; 
	  case FRKA:
	  case KAKA:
		fire_strat_rule(s, JOSEKI, josval * 2 / 3, corner, 0); /* 3/02 - less important than joseki elsewhere */
		break;
	  case NORM:
		fire_strat_rule(s, JOSEKI, josval, corner, 0);
		if (cflag)
			fire_strat_rule(s, JOSEKI_NO_TENUKI, notenukival, corner, 0);
		break;
	  case URGT:
		if (jflg == 1) { 
			fire_strat_rule(s, URG_JOSEKI, josval + 200, corner, 0);
			evmv = TRUE;
			if (cflag) {
				fire_strat_rule(s, JOSEKI_NO_TENUKI, notenukival, corner, 0);
			}
		} else { 
			fire_strat_rule(s, URG_JOSEKI, josval, corner, 0);
		}
		break;
	  case COMP:
		if (ahead >= 2  || jflg != 1 || playlevel < 4)
			break;
		fire_strat_rule(s, COMP_JOSEKI, 100, corner, 0);  /* prefer simple joseki if available */
		break;
	  case BAD:
		if (jflg == 1)
			fire_strat_rule(s, BAD_JOSEKI, 0, corner, 0);
		break;
	  case SHIM:
		fire_strat_rule(s, SHIM_JOSEKI, 0, corner, 0);
		break;
	  case FOLL:
		fire_strat_rule(s, FOLL_JOSEKI, 150, corner, 0);
		break;
      case TRIK:
		if (ahead >= 2)
			break;
		fire_strat_rule(s, TRIK_JOSEKI, 0, corner, 0);
		break;
	}
	return;
}

/* is any point in this group in a corner which has a joseki move? */

int groupinjosekicorner(group_t g)
{
	int cn, mpt;
	for (cn = 0; cn < 4; ++cn) {
		if (foundjoseki[cn] || foundjosfol[cn]) {
			for (mpt = grpieces[g]; mpt != -1; mpt = mvnext[mpt]) {
				if (incorner(mvs[mpt], cn))
					return TRUE;
			}
		}
	}
	return FALSE;
}

/* return TRUE if s is a bad move because there are joseki in the corner, but 
 * this move is not joseki. 
 * if s is joseki, then return TRUE.            
 * if there are no joseki in any corner s is in, return TRUE
 * if s is not joseki, 
 *     if there are followup joseki only in corner, allow extensions
 *     if there are non-tenuki joseki in corner, return FALSE. 
 */

int notjoseki(sqr_t s) {
	int ptr, r, cn, joscorner = FALSE, josfolcorner = FALSE;
	if (boardsize < MINJOSBOARD)
		return FALSE;
	if (edge2[s] >= 9)
		return FALSE;
	for (cn = 0; cn < 4; ++cn) {
		if (incorner(s, cn)) {
			if (foundjoseki[cn])
				joscorner = TRUE;
			else if (foundjosfol[cn])
				josfolcorner = TRUE;
		}
	}
	if (!joscorner && !josfolcorner)
		return FALSE;
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
		r = strat[list[ptr]].reason;
		if (r == JOSEKI || r == TRIK_JOSEKI || 
		   r == COMP_JOSEKI || r == URG_JOSEKI || r == SHIM_JOSEKI ||
		   r == FOLL_JOSEKI && !joscorner)
			return FALSE;
		if (!joscorner &&   /* extensions allowed */
			(r == MAKE_SHIMARI ||
			 r == KAKARI ||
			 r == INVADE_HIGH || 
			 r == EXTEND_TO_ENEMY || 
			 r == INVADE_WITH_ROOM || 
			 r == BIG_MOVE))
			return(FALSE);
	}
	return TRUE;
}

char *jlibtypes[] = {
	"N",
	"U",
	"B",
	"C",
	"TF",
	"FF",
	"SH",
	"F",
	"TR",
	"I",
	"KA",
	"TT",
	"FK",
	"P",
	"HI",
};

/* is s a joseki move in corner? */    
    
int isjoseki(sqr_t s, int corner, int color)
{
	int x, y, tmp, num = 0;
	int flag = FALSE, josflag, lastcolor, tenuki;
	unsigned int ptr;
	tenuki = FALSE;
	lastcolor = getlastcolor(corner);
	if (lastcolor == NOCOLOR)
		lastcolor = 1-color;
	if (lastcolor == color)
		tenuki = TRUE;
	for (ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)) {
		num++;
		getxyjlib(ptr, &x, &y);
		josflag = getflag(ptr) & 0xf;
		if (tenuki) {
			if (x != 0)
				continue;
			getxyjlib(j2next(ptr), &x, &y);
			josflag = getflag(j2next(ptr)) & 0xf;
		}
		else if (x == 0)
			continue;
		if (josflag == IGNR)
			continue;
		x--;
		y--;
		if (jreflect[corner] <= 1) {
			tmp = x;
			x = y;
			y = tmp;
		}
		if (corner > 1)
			y = boardsize - 1 - y;
		if (corner%2 == 1)
			x = boardsize - 1 - x;
		if ((signed)s == y * boardsize + x)
			return TRUE;

		flag = TRUE;
	       
		getxyjlib(ptr, &x, &y);
		if (tenuki) {
			getxyjlib(j2next(ptr), &x, &y);
		}
		x--;
		y--;
		if (jreflect[corner] == 0 && x != y) {
			if (corner > 1)
				y = boardsize - 1 - y;
			if (corner%2 == 1)
				x = boardsize - 1 - x;
			if ((signed)s == y*boardsize+x)
				return TRUE;
		}
	}
	return FALSE;
}

/* find joseki move number num in corner or color, 
 * and return the index (0-360), or pass if there are not that many moves
 */

sqr_t getonejoseki(int corner, int color, int num)
{
	int x, y, tmp;
	int count = 0, josflag, lastcolor, tenuki;
	unsigned int ptr;
	char *p;
	
	p = getstring(GOPAGE, 54);
	tenuki = FALSE;
	lastcolor = getlastcolor(corner);
	if (lastcolor == NOCOLOR)
		lastcolor = 1 - color;
	if (lastcolor == color)
		tenuki = TRUE;
	for (ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)) {
		getxyjlib(ptr, &x, &y);
		josflag = getflag(ptr) & 0xf;
		if (tenuki) {
			if (x != 0)
				continue;
			getxyjlib(j2next(ptr), &x, &y);
			josflag = getflag(j2next(ptr)) & 0xf;
		}
		else if (x == 0)
			continue;
		x--;
		y--;
		if (jreflect[corner] <= 1) {
			tmp = x;
			x = y;
			y = tmp;
		}
		if (corner > 1)
			y = boardsize - 1 - y;
		if (corner%2 == 1)
			x = boardsize - 1 - x;
		if (josflag != BAD && josflag != FOLL && josflag != TRIK) {
			count++;
			if (count == num) {
				return y * boardsize + x;
			}
		}
		getxyjlib(ptr, &x, &y);
		if (tenuki) {
			getxyjlib(j2next(ptr), &x, &y);
		}
		x--;
		y--;
		if (jreflect[corner] == 0 && x != y) {
			if (corner > 1) {
				y = boardsize - 1 - y;
			}
			if (corner%2 == 1)
				x = boardsize - 1 - x;
			if (josflag != BAD && josflag != FOLL && josflag != TRIK) {
				count++;
				if (count == num) 
					return y * boardsize + x;
			}
		}
	}
	return PASS;
}

/* highlight all joseki moves in corner for color color
 * and return the number found (of J type)
 */
	
int getjoseki(int corner, int color, char marks[363])
{
	int x, y, tmp, i;
	int count = 0, josflag, lastcolor, tenuki;
	unsigned int ptr;
	char *p;
	
	p = getstring(GOPAGE, 54);
	for (i = 0; i < 363; ++i) {
		marks[i] = 0;
	}
	if (jflag[corner] != 1) {
		return 0;
	}
	tenuki = FALSE;
	lastcolor = getlastcolor(corner);
	if (lastcolor == NOCOLOR) {
		lastcolor = 1 - color;
	}
	if (lastcolor == color)
		tenuki = TRUE;
	for (ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)) {
		getxyjlib(ptr, &x, &y);
		josflag = getflag(ptr) & 0xf;
		if (tenuki) {
			if (x != 0)
				continue;
			getxyjlib(j2next(ptr), &x, &y);
			josflag = getflag(j2next(ptr)) & 0xf;
		}
		else if (x == 0)
			continue;
		x--;
		y--;
		if (jreflect[corner] <= 1) {
			tmp = x;
			x = y;
			y = tmp;
		}
		if (corner > 1) {
			y = boardsize - 1 - y;
		}
		if (corner%2 == 1) {
			x = boardsize - 1 - x;
		}
		outjos(x, y, josflag, p, marks);
		//if (josflag != BAD && josflag != FOLL && josflag != TRIK)
		count++;
	       
		getxyjlib(ptr, &x, &y);
		if (tenuki) {
			getxyjlib(j2next(ptr), &x, &y);
		}
		x--;
		y--;
		if (jreflect[corner] == 0 && x != y) {
			if (corner > 1)
				y = boardsize - 1 - y;
			if (corner%2 == 1)
				x = boardsize - 1 - x;
			outjos(x, y, josflag, p, marks);
			//if (josflag != BAD && josflag != FOLL && josflag != TRIK)
			count++;
		}
	}
	return count;
}

/* output a single joseki at x,y of type flag */

static void outjos(int x, int y, int flag, char *p, char marks[363])
{
	char opt;
	if (board[y * boardsize + x] != NOGROUP) {
		return;
	}
	if (flag == BAD) {
		opt = 'X';
	} else if (flag == FOLL) {
		opt = 'j';
	} else if (flag == TRIK) {
		opt = 'T';
	} else {
		opt = 'J';
	}
	if (flag != IGNR) {
		marks[y * boardsize + x] = opt;
	}
}
