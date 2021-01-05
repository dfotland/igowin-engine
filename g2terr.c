/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2hd.h"
#include "g2rldef.h"
#include "g2dist.h"
#ifdef G2DEBUGOUTPUT
#include <stdio.h>
#endif


#include "g2terr.pr"
void cntterr(list_t swaplist);
extern int obaval;
extern int atkv[NUMALIVE],kval[NUMALIVE],ctval[NUMALIVE],connval[NUMALIVE];
extern int sumeyes[41],semalive[NUMALIVE],urgalive[NUMALIVE],senteattack[NUMALIVE],defatkv[NUMALIVE];
extern int dirnm[],opdir[],msks[],ccl[3];
static int runout(army_t army, sqr_t s, sqr_t osqr, int c);

extern int rtthreshold,d1[4],d2[4],d3[4],maxsharedrt;
extern int cfac[3];
int dfac[19] = { 1,1,2,4,8,8,8,8,8,0 };

/* radiated territory.  Each stone radiates influence based on aliveness
 * of stone and taxicab distance from stone
 * value doesn't count if already counted in ltrscr or tscr
 */
                        /* one value for each distance and aliveness value */
int rtval1[MAXDIST + 1][DEAD + 1];	/* for running */
int rtval2[MAXDIST + 1][DEAD + 1];     /* for territory to friendly color, by aliveness */
int rtval3[MAXDIST + 1][DEAD + 1];     /* for territory by aliveness - for opposite color */

#define THFAC1 0.5
#define THFAC2 0.25

/* swaplist is list of groups to consider opposite aliveness */
/* radiate influence into runterv to terr if groups are surrounded */

void radiaterun(list_t swaplist) 
{
	group_t g;
	int ptr;   /* mvs ptr, not list ptr */
	int gral, i;
	double fac;
	for (i = 0; i < boardsquare; ++i) {
		runterv[i][0] = runterv[i][1] = 0;
	}
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])continue;
		gral = G_ALIVE(g);
		if (gral >= 32)continue;  /* doesn't have aliveness yet */
		if (rtval1[1][gral] == 0)continue;
		if (swaplist != EOL && inlist(g, &swaplist)) {
			if (gral <= UNSETTLED)
				gral = DEAD;
			else 
				gral = VERY_ALIVE;
		}
		if (grthreatened[g] == 2)	/* 3/01 can't give full value for running toward a threatened group */
			fac = THFAC2;
		else if (grthreatened[g] == 1)
			fac = THFAC1;
		else fac = 1.0;
		for (ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr]) {
			if (lnbn[mvs[ptr]] != 0)
				radiatepiece(runterv, mvs[ptr], gral, G_COLOR(g), rtval1, fac);
		}
	}
}

extern double rtalive2[];

/* radiate influence to find territory 
 * this is the only call to find the real territory
 * swaplist is a list of groups that need their strength
 * reversed (live to dead and vice-versa, for scoring)
 */

void radiateterr(list_t swaplist)
{ 
	group_t g;
	int ptr;  /* mvs ptr, not list ptr */
	int gral;
	int grprob, i;
	double fac, efac;
#ifdef CHECK
	char buf[80];
#endif		
	for (i = 0; i < boardsquare; ++i) {
		rterv[i][0] = rterv[i][1] = 0;
	}
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])
			continue;
		grprob = gralprob[g];
		gral = G_ALIVE(g);
#ifdef CHECK
		if (gral < HAS_TWO_EYES || gral > DEAD) {
			sprintf(buf,"radiateweak bad gral %d!\n",gral);
			outerror(buf);
		}
#endif
		/* reverse aliveness of group g, for scoring */
		if (swaplist != EOL && inlist(g, &swaplist)) {
			grprob = -grprob;
			if (gral <= UNSETTLED) {
				gral = VERY_WEAK;
				grprob = -50;
			} else { 
				gral = ALIVE;
				grprob = 50;
			}
		}
		
		fac = 1.0;
		if (rtalive2[gral] < 0.99) {  /* adjust with factor unless group is already alive */
			fac = (50. + grprob)/100.;  /* +50 -> 1.0, 0 -> 0.5, -50 -> 0.0 */
		}
		if (grprob > 0)
			efac = 0.0;
		else
			efac = -grprob/50.;    /* -50 -> 1.0, >= 0 -> 0 */

		for (ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr]) {
			if (lnbn[mvs[ptr]] != 0 && rtval2[1][gral] != 0)
				radiatepiece(rterv, mvs[ptr], gral, G_COLOR(g), rtval2, fac);  /* my color */
			if (grprob < 0 && lnbn[mvs[ptr]] != 0 && rtval3[1][gral] != 0)
				radiatepiece(rterv, mvs[ptr],gral, 1 - G_COLOR(g), rtval3, efac); /* for enemy */
		}
	}
	
	radiatescore();
	cntterr(swaplist);
}
	


/* sum up the score after correcting for edge effects for non-liberties
 * sum into rtscr;
 * set up rtsave[] with individual point values,
 * negative for black and positive for white.
 */
	
void radiatescore(void)
{
	int tval, sc;
	sqr_t i; 

	for (i = 0; i < boardsquare; ++i) {
		/* adjust for edge effects - assume stones reflected on
		 * other side of edge, so stones on edge get 2 value,
		 * adjust by 2/3 per unit distance
		 */
		if (edge[i] == 0) {
			rterv[i][0] *= 4;
			rterv[i][1] *= 4;
		}
		else if (edge[i] == 1) {
			if (edge2[i] == 2) {
				rterv[i][0] = rterv[i][0] * 2 + rterv[i][0] * 8 / 9;  /* 2 * ( 1 + 4/9) */
				rterv[i][1] = rterv[i][1] * 2 + rterv[i][1] * 8 / 9;
			}
			else if (edge2[i] == 3) {
				rterv[i][0] = rterv[i][0] * 2 + rterv[i][0] * 2 / 5;  /* 2 * (1 + 2 / 5) */
				rterv[i][1] = rterv[i][1] * 2 + rterv[i][1] * 2 / 5;
			}
			else { 
				rterv[i][1] += rterv[i][1];	/* 2x on edge since stone on other side same distance */
				rterv[i][0] += rterv[i][0];
			}
		}
		else if (edge[i] == 2) {
			if (edge2[i] == 2) {
				rterv[i][0] += rterv[i][0] * 8 / 9;
				rterv[i][1] += rterv[i][1] * 8 / 9;
			}
			else if (edge2[i] == 3) {
				rterv[i][0] += rterv[i][0] * 52 / 81;
				rterv[i][1] += rterv[i][1] * 52 / 81;
			}
			else {
				rterv[i][0] += rterv[i][0] * 4 / 9;  /* 2/3 * 2/3 */
				rterv[i][1] += rterv[i][1] * 4 / 9;
			}
		}
		else if (edge[i] == 3) {
			if (edge2[i] == 3) {
				rterv[i][0] += rterv[i][0] * 2 / 5;
				rterv[i][1] += rterv[i][1] * 2 / 5;
			}
			else {
				rterv[i][0] += rterv[i][0] / 5;  /* 16/81, close to 1/5 */
				rterv[i][1] += rterv[i][1] / 5;
			}
		}
		if (ld[i] != NOLD) {
			tscr -= terv[i];
			terv[i] = 0;
			continue;	/* liberties handled by cntterr - nonliberties here */
		}
		/* 5/03 reduce threshold to zero  - no! 3/04 - increase to rtthreshold, since otherwise dead stones kill territory */
		if (ltr1[i] && ltrgd[i] > 1 && rterv[i][0] > rtthreshold  && rterv[i][1] > rtthreshold) {
			tscr -= terv[i];
			terv[i] = 0;
			continue;	/* enemy can play here, so not territory */
		}


		/* figure out how much this point is worth */
		if (problemflag == 1 && rterv[i][0] < MAXRTVAL/2 && rterv[i][1] < MAXRTVAL/2)
			tval = 0;  /* only count secure territory when solving problems */

		/* either side could play here - just give small influence value */
		else if (rterv[i][1] >= rtthreshold && distance[i][1] <= 6 && 
			rterv[i][0] >= rtthreshold && distance[i][0] <= 6) {
			if (rterv[i][1] > 2*rterv[i][0] && distance[i][0] > 3)
				tval = (rterv[i][1]-2*rterv[i][0])/3;
			else if (rterv[i][0] > 2*rterv[i][1] && distance[i][1] > 3)
				tval = (-rterv[i][0]+2*rterv[i][1])/3;  /* 10/4/97 changee constant from 4 to 2 8/98 change divisor to 3*/
			else
				tval = 0;
		}
		else if (rterv[i][1] > 0 && rterv[i][1] < rtthreshold &&
			rterv[i][0] > 0 && rterv[i][0] < rtthreshold)tval = 0;
		else if (rterv[i][1] > MAXRTVAL)tval = MAXRTVAL - rterv[i][0];
		else if (rterv[i][0] > MAXRTVAL)tval = rterv[i][1] - MAXRTVAL;
		else if (rterv[i][1] > rterv[i][0]*2)
			tval = rterv[i][1]-2*rterv[i][0];
		else if (rterv[i][0] > rterv[i][1]*2)
			tval = -1*(rterv[i][0]-2*rterv[i][1]);
		else tval = 0;

		if (tval > MAXRTVAL)
			tval = MAXRTVAL;
		else if (tval < -MAXRTVAL)
			tval = -MAXRTVAL;

		sc = tval * 50 / MAXRTVAL;  /* CONSTANT is MAXRTVAL/50 */
		tscr -= terv[i];
		tscr += sc;
		terv[i] = sc;
	}
}



int xoff[] = { 1,0,-1,0 };
int xoff2[] = { 0,-1,0,1 };

#pragma warning( disable : 4244)

/* s is a point with a stone on it.  radiate influence from that stone */
/* fac is multiplied by rtval to adjust for thickness of group */

void radiatepiece(short rterv[NUMSQUARES][2], sqr_t s, int gral, int c, int rtval[12][26], double fac)
{
	sqr_t sn, sn2, point1[50], point2[50], *p1, *p2, *pt;
	int dir, dist, count1, count2, i, inc, rt;
	int j, ldtmp, connection;
	list_t ptr;
	j = fdir[s];
	for (ldtmp = ldir[j]; j < ldtmp; ++j) {
	
		/* radiate along lines */
		sn = s + nbr[j];
		inc = sn - s;
		dist = 1;
		rt = (int)(rtval[dist][gral] * fac);
		while (board[sn] == NOGROUP && rt > 0) {
			rterv[sn][c] += rt;
			if (edge[sn] < 2 && edge[sn - inc] > edge[sn])
				break;  /* stop at edge */
			if (lnbf[sn][1 - c] != 0 && (G_COLOR(lgr[sn]) != 1 - c || gralive[lgr[sn]] != DEAD) ||
				ltr1[sn] && ltrcolor[sn] == 1 - c) {  /* not dead group enemy liberty */
				if (dist > 2 || dist == 2 && ld[sn] > 3 && !S_NEUTRAL(sn)) {
					rterv[sn][c] -= rt / 2;
					break; /* can't get past here */
				}
				if (dist == 1 && lnbf[sn][1 - c] == 2 && lnbf[sn][c] == 1) {
					connection = FALSE;
					for (ptr = cnbrd[sn]; ptr != EOL; ptr = link[ptr])
						if (G_COLOR(cngr1[list[ptr]]) == 1-c &&
							cnprot[list[ptr]] >= AJI_CONNECT) {
							connection = TRUE;
							break;
						}
					if (connection)
						break;	/* can't push through solid connection */
				}
				if (dist > 1 || S_NEUTRAL(sn))
					dist += 1;  /* weaken */
			}
                           /* don't radiate thru walls */
			sn += inc;
			++dist;
			rt = (int)(rtval[dist][gral] * fac);
		}
	}

	/* do quadrants */

	j = fdir[s];
	for (ldtmp = ldiag[j]; j < ldtmp; ++j) {

		sn = s+diags[j];
		inc = sn-s;
		for (dir = 0; dir < 4; ++dir)
			if (d3[dir] == inc)
				break;

		if (board[s+d1[dir]] != NOGROUP && board[s+d2[dir]] != NOGROUP)
			continue;

		dist = 2;
		if (board[sn] != NOGROUP)
			continue;
		rterv[sn][c] += (int)(rtval[dist][gral] * fac);
		if (edge[sn] == 0)
			continue;
		if (edge[sn] == 1 && lnbf[sn][1-c] != 0 && gralive[lgr[sn]] != DEAD && lnbf[sn][c] == 0)
			continue;
		/* 3/01 connected enemy stones, don't go thru here */
		if (lnbf[sn][1-c] > 1 && !lnbf[sn][c] && 
			cnbrd[sn] != EOL && cnprot[list[cnbrd[sn]]] && cntype[list[cnbrd[sn]]] == CN_ONEPOINTJUMP)
			continue; 
		if (lnbf[sn][1-c] != 0 || ltr1[sn] && ltrcolor[sn] == 1-c)
			dist += 3; /* weaken */
		count1 = 1;
		point1[0] = sn;
		p1 = point1;
		p2 = point2;

		do {
			++dist;
			rt = (int)(rtval[dist][gral] * fac);
			if (rt <= 0)
				break;  /* no point in going on*/
			count2 = 0;
			
			/* do top of first piece */
			if (edge[p1[0]] > 1 || edge[p1[0] - d1[dir]] == 1) {
				sn = p1[0] + d1[dir];
				if (board[sn] == NOGROUP) {
					rterv[sn][c] += rt;
					if (edge[sn] > 0 && 
					   (ld[sn] == NOLD || ld[sn] < 4 && lnbf[sn][1-c] == 0) && 
					   (ltr1[sn] == 0 || (ltrgd[sn]&2) || ltrcolor[sn] != 1-c))  
						p2[count2++] = sn;	
				}	
			}

			/* do diags of middle */
			for (i = 1; i < count1; ++i) {
				sn = p1[i-1] + d2[dir];
				if (board[sn] == NOGROUP) {
					rterv[sn][c] += rt;
					if (edge[sn] > 0 && 
					   (ld[sn] == NOLD || ld[sn] < 4 && lnbf[sn][1-c] == 0) &&
					   (ltr1[sn] == 0 || (ltrgd[sn]&2) || ltrcolor[sn] != 1-c)) 
						p2[count2++] = sn;
				}
				sn2 = p1[i] + d1[dir];
				if (sn2 == sn)
					continue;
				if (board[sn2] == NOGROUP) {
					rterv[sn2][c] += rt;
					if (edge[sn2] > 0 &&
					   (ld[sn2] == NOLD || ld[sn2] < 4 && lnbf[sn2][1-c] == 0) &&
					   (ltr1[sn2] == 0 || (ltrgd[sn2]&2) || ltrcolor[sn2] != 1-c)) 
						p2[count2++] = sn2;
				}
			}

			/* do bottom of bottom piece */
			if (edge[p1[count1-1]] > 1 ||
				edge[p1[count1-1] - d2[dir]] == 1) {
				sn = p1[count1-1] + d2[dir];
				if (board[sn] == NOGROUP) {
					rterv[sn][c] += rt;
					if (edge[sn] > 0 && 
					   (ld[sn] == NOLD || ld[sn] < 4 && lnbf[sn][1-c] == 0) && 
					   (ltr1[sn] == 0 || (ltrgd[sn]&2) || ltrcolor[sn] != 1-c)) 
						p2[count2++] = sn;
				}
			}
			pt = p1;
			p1 = p2;
			p2 = pt;  /* swap pointers */			
			count1 = count2;
		} while(count2 != 0);
	}
}




/*
int clibs[] = { 0,-80, -50, -20, 05, 30,50, 70,080 };
*/
/*
int afac[] = {0,0,8,8,8,7,7,6,6,5 ,5,3,0,-2,-5,-12,-25,-35,-40,-40,
                 -45,-55,-65,-75,-190,-190,-190,-190 };
*/

/* set lgr to be the deadest nearby group */  
  
void fixlgr(void)
{
	sqr_t s, sn;
	int l, ldtmp;
	/* fix the lgr values */
	for (s = 0; s < boardsquare; ++s) {	
		if (ld[s] > 3 || S_NEUTRAL(s)) {   /* set lgr to deadest nearby group */
			l = fdir[s]; 
			for (ldtmp = ldir[l]; l != ldtmp; ++l) {
				sn = s + nbr[l];
				if (ld[sn] != 0)
					continue;

				if (G_THREATENED(lgr[sn]) && gralive[lgr[s]] != DEAD && !G_THREATENED(lgr[s]) ||
					(G_THREATENED(lgr[sn]) != 0) == (G_THREATENED(lgr[s]) != 0) &&
					G_ALIVE(lgr[sn]) > gralive[lgr[s]]) {
					lgr[s] = lgr[sn];
				}
			} 
		}
	}
}

/* evaluate the territory in liberties */
   
void cntterr(list_t swaplist) 
{
	sqr_t s;
	int sc;

	for (s = 0; s < boardsquare; ++s) {
		if (ld[s] != NOLD) {	/* is a liberty */
			sc = evallibsterr(s, swaplist);
			tscr -= terv[s];
			tscr += sc;
			terv[s] = sc;
		}
	}
}



/* figure out how much territory a liberty is worth.  50 = 1 point.
 * first find the deadest neighboring group.
 * keep track of if neighboring groups are equally dead.
 * if the liberty is edge territory, adjust for the aliveness and return.
 * if point is neutral, give to enemy if owning group dead.
 * if point completely surrounded and nbr group has 2 liberties and
 * the other liberty is neutral, then this is not a point.
 * if point is adjacent to neutral point, only worth 1/2 as much
 */


int evallibsterr(sqr_t s, list_t swaplist) {
	group_t g;
	sqr_t sn, sn2;
	int v,gral,c,c1,eflag,l,ldtmp,sc;
	int secneut = NOCOLOR;  /* color of side that has neutral point since can't fill due to shortage of liberties, or can be put into atari */
    int alprob;  /* lowest gralprob */
    int neutralnext;
	int sentepush[2];
    list_t ptr;
	g = lgr[s]; 
#ifdef CHECK
	if (!grlv[g]) {
		outerror("evallibsterr of nonexistant lgr[]");
		turnoffcplay();
	}
#endif
	v = ld[s];
	if (g == NOGROUP || v == 0) {  /* not an empty point */
		ltr1[s] = 0;
		ltrgd[s] = FALSE;
		return 0; 
	} 
	gral = G_ALIVE(g);
	alprob = gralprob[g];

	/* reverse aliveness of group g, for scoring */
	if (swaplist != EOL && inlist(g, &swaplist)) {
		alprob = -alprob;
		if (gral <= UNSETTLED)
			gral = VERY_WEAK;
		else 
			gral = ALIVE;
	}
#ifdef CHECK
	if (gral > DEAD || gral < 0) {
		outerror("Bad aliveness value in evallibsterr");
		turnoffcplay();
	}
#endif
	c = cfac[G_COLOR(g)]; 
	c1 = c;         /* color of deadest group */
	eflag = FALSE;  /* equally alive groups of opposite color */
	sentepush[0] = sentepush[1] = FALSE;
	if (ld[s] > 3 || S_NEUTRAL(s)) {   
		l = fdir[s]; 
		for (ldtmp = ldir[l]; l != ldtmp; ++l) { 
			sn = s + nbr[l];
			if (ld[sn] == NOLD) {   /* can push in sente since opponent will save territory */
				if (terv[sn] >= 25)
					sentepush[0] = TRUE;
				else if (terv[sn] <= -25)
					sentepush[1] = TRUE;
				continue;
			}

			if (board[sn] == NOGROUP && lgr[sn] != NOGROUP && 
				gralive[lgr[sn]] >= 18)  /* sente threat to connect */
				sentepush[G_COLOR(lgr[sn])] = TRUE; 
			/* found a neighboting stone */
			if (S_NUMLIBS(sn) == 2) {  /* can atari to take point */
				sn2 = list[grlbp[board[sn]]];
				if (sn2 == s)
					sn2 = list[link[grlbp[board[sn]]]];
				if (!S_NEUTRAL(sn2) || 
					G_ALIVE(list[nbgrp[sn2][1-S_COLOR(sn)]]) >= WEAK) {  /* 3/02 - changed to WEAK from DEAD */
					sentepush[1-S_COLOR(sn)] = TRUE;
				}
			}
			
			if (S_ALIVE(sn) == gral && S_COLOR(sn) 
				!= G_COLOR(lgr[s]))eflag = TRUE; 
		} 
	}

	if (S_NEUTRAL(s)) {  /* neutral territory */
		if (sentepush[0] && !sentepush[1])
			secneut = 0;
		else if (sentepush[1] && !sentepush[0])
			secneut = 1;
		if (gralprob[g] > 0) {
			sc = gralprob[g] * cfac[secneut];  /* approximation - wrong if gral is group that can't push in */
			return sc;
		}
		if (eflag)return 0;
		sc = gralprob[g] * c1;
		return sc;
    }

	if (alprob > 0 && lnbn[s] == 3 && 
		distance[s][1-G_COLOR(g)] <= 3 &&
		(ltr1[s] == 0 || ltrgd[s] != 0) &&
		S_ALIVE(closest[s][1-G_COLOR(g)]) <= UNSETTLED && 
		v <= 3 && rterv[s][1-G_COLOR(g)] >= rtthreshold)
		sc = 0; /* too close to enemy to be territory */
	else {
		if (v >= 2) {
			if (distance[s][1-G_COLOR(g)] > 2 && rterv[s][1-G_COLOR(g)] < MAXRTVAL/4 ||
				S_ALIVE(closest[s][1-G_COLOR(g)]) > UNSETTLED ||
				ltr1[s] != 0 && ltrgd[s] == 0) /* full value if ltrgd is zero */
				v = 8;  /* full value if no enemy stones nearby */
		}
		if (alprob < 0)  /* full value to enemy for weak groups - doesn't depend on neighborhood */
			v = 8;
		sc = (alprob * c1 * dfac[v]) / 8;
		if (alprob >= 0) {  /* reduce value if enemy nearby or not enough friendly influence */
			if (lnbn[s] == 3 && rterv[s][G_COLOR(g)] < MAXRTVAL) {
				sc = sc * rterv[s][G_COLOR(g)] / MAXRTVAL;
			}
		    neutralnext = 0;
			if (gral <= UNSETTLED)
				for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
	    			if (lnbf[list[ptr]][1-G_COLOR(g)] && G_ALIVE(lgr[list[ptr]]) <= UNSETTLED)
						neutralnext++;
			if (neutralnext == 0 &&
				distance[s][1 - G_COLOR(g)] == 3 &&
				S_ALIVE(closest[s][1-G_COLOR(g)]) <= UNSETTLED)
				sc = sc * 3 / 4;  /* opponent can take terr away with 2 moves */
			else if (neutralnext == 1 ||
				alprob > 0 && lnbn[s] == 3 && 
				distance[s][1-G_COLOR(g)] == 3 && 
				S_ALIVE(closest[s][1-G_COLOR(g)]) <= UNSETTLED && 
				ld[s] <= 3)
		   		sc /= 2;
			else if (neutralnext > 1)
       			sc = 0;
		}
	}
	if (sc > 50) {
		sc = 50;
#ifdef CHECK
		outerror("lib score error");
#endif
	}
	if (sc < -50) {
		sc = -50;
#ifdef CHECK
		outerror("lib score error");
#endif
	}
	return sc;
}
#ifdef G2DEBUGOUTPUT

static int colorfac[3] = { -1, 1, 0 };


extern int savepassval, savestartval, savebestval, debugnumlifecalls, numlifecalls, deadscr();
	

void outtterr(int tm, int handicap, int rules)
{
	sqr_t s;
	int total = 0, aji, t1 = 0, t2 = 0, t3 = 0, t4 = 0, t5 = 0, t6 = 0;
	int tot;
	char buf[200];
	int hscr = 0;
	int dead;
	int big;

	tot = getcurrenteval(tm, handicap, rules);
	if (rules == JAPANESE || rules == AMERICAN || rules == CHINESE)
		hscr = handicap * 50;
	unhighlight();
	sprintf(buf,"\nrtthreshold is %d, maxsharedrt is %d.\n\n", rtthreshold, maxsharedrt);
	outerr(buf);
	aji = groupaji(&big);
	dead = deadscr();

	outerr("\nEvaluation:\n");
	if (tot > 0)
		sprintf(buf,"To move wins by %5.1f points(%d currenteval).\n", tot / 50.f, tot);
	else
		sprintf(buf, "To move loses by %5.1f points(%d currenteval).\n", -tot / 50.f, tot);
	outerr(buf);
	sprintf(buf, "Passval %5.1f, startval %5.1f, moveval %5.1f, oba %5.1f\n%d moves, %d-%d evals, %d tac nodes\n",
		savepassval/50.f, savestartval/50.f, savebestval/50.f, obaval/50.f,
		nummoves+1, debugnumlifecalls, numlifecalls, numnodes); 
	outerr(buf);
	sprintf(buf,"piece terr %d, point terr %d, dead-inside %d, komi %d, pass terr %d, handicap %d, aji %d, big (attack) %d, oba %d\n",
		pscr, tscr, dead, komi, passscr, hscr, aji, big, obaval);
	outerr(buf);

	for (s = 0; s < boardsquare; ++s) {
		if (board[s] != NOGROUP) {
			sprintf(buf,"%d",gralprob[board[s]] * ccl[S_COLOR(s)]);
			outstone(s,buf);
			total += gralprob[board[s]] * ccl[S_COLOR(s)];
			t1 += gralprob[board[s]] * ccl[S_COLOR(s)];
		}
		else {
			sprintf(buf,"%d",terv[s]);
			outstone(s,buf);										 
			total += terv[s];
			t2 += terv[s];
			if (terv[s] > 0)
				t5 += terv[s];
			else
				t6 += terv[s];
		}
	}
	total += komi + passscr + hscr;
	sprintf(buf,"\nTotal point sum terr: %d(%d pieces, %d points (%d, %d), %d komi, %d pass, %d handicap)\n",
		total, t1, t2, t5, t6, komi, passscr, hscr);
	outerr(buf);
}
		
void outnewltrgood(sqr_t cursorpos, int tm) {
	sqr_t s;
	int tmp;
	char buf[10];
	if (board[cursorpos] != NOGROUP) {
		outerr("cursor must be on empty point!");
		return;
		}
	mvs[msptr] = cursorpos;
	mvcolor[msptr] = tm;
	lupdate(msptr);
	++msptr;
	for (s = 0; s < boardsquare; s++)
		if (ltr1[s] != 0) {
			sprintf(buf,"%d",getltrgd(s,FALSE,&tmp,&tmp));
			outstone(s,buf);
			}
	--msptr;
	ldndate(msptr);
	}

static char *lclrs[] = { "B", "W", "-" };

void outltrgood(void) {
	sqr_t s;
	char buf[50];
	int l1, lc, lg;
	for (s = 0; s < boardsquare; s++)
		if (ltr1[s] != 0) {
			lg = getltrgd(s, 0, &l1, &lc);
			if (lg != ltrgd[s]) {
				sprintf(buf, "%d != %d\r\n", lg, ltrgd[s]);
				outerr(buf);
			}
			sprintf(buf,"%d%s",ltrgd[s],lclrs[ltrcolor[s]]);
			outstone(s,buf);
			}
	}


void outtltrgood(sqr_t cursorpos) {
	char buf[10];
	int tmp;
	if (ltr1[cursorpos] != 0) {
		sprintf(buf,"%d", getltrgd(cursorpos, FALSE, &tmp, &tmp));
		outstone(cursorpos,buf);
		}
	}

void outltr1(void) {
	sqr_t s;
	char buf[10];
	for (s = 0; s < boardsquare; s++)
		if (ltr1[s] != 0) {
			sprintf(buf,"%d",ltr1[s]);
			outstone(s,buf);
			}
	}


void outrad(int c) {
	sqr_t s;
	char buf[20];
	for (s = 0; s < boardsquare; ++s) {
		if (board[s] == NOGROUP) {
			sprintf(buf,"%d",rterv[s][c]);
			outstone(s,buf);
			}
		}
	}

void outrunrad(int c) {
	sqr_t s;
	char buf[20];
	for (s = 0; s < boardsquare; ++s) {
		if (board[s] == NOGROUP) {
			sprintf(buf,"%d",runterv[s][c]);
			outstone(s,buf);
			}
		}
	}

#endif 

/* is s a cutting point? */
	
int iscut(sqr_t s) {
	army_t army = NOARMY,a;
	int i,ldtmp;
	
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		a = S_ARMY(s+nbr[i]);
		if (a != NOARMY && a != army) {
			if (army != NOARMY)return(TRUE);
			army = a;
		}	
	}
	return(FALSE);
}

/* s is a running point for army.  Surround army by filling running point */

list_t surroundpoints(army_t army, sqr_t s) {
	list_t ptr,slist = EOL;
	int c,i,ldtmp;
	sqr_t sn, cpoint;
	
	c = G_COLOR(list[armygroups[army]]);
	if (S_NEUTRAL(s)) {
		addlist(s,&slist);
		if (lnbn[s] == 2) {
			i = fdir[s];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				if (S_COLOR(s+nbr[i]) == 1-c)
				   addlist((sqr_t)(s-nbr[i]),&slist);
				}
			}
		}
	else if (ld[s] == 4 && lnbn[s] == 2 && !iscut(s)) {  /* play angle point */
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
			if (lnbf[list[ptr]][c] == 0 && 
			   edge[list[ptr]] >= edge[s] &&
			   lnbf[list[ptr]+list[ptr]-s][1-c] == 0 &&
			   S_COLOR(list[ptr]+list[ptr]-s) != 1-c)
			   addlist(list[ptr],&slist);
		}
	else if (lnbn[s] == 3) {
		if (ld[s] == 2 && grlibs[lgr[s]] < 4)
			addlist(s,&slist);  /* low on liberties, contact play */
		else {
			cpoint = NOSQUARE; /* capping play point */
			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
				if (board[s+s-list[ptr]] != NOGROUP) {
					cpoint = list[ptr];
					}
			if (lnbn[cpoint] == 4 || lnbf[cpoint][c])
				addlist(cpoint, &slist);
			else { /* cpoint must have unfriendly neighbor */
				for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
					sn = list[ptr];
					if (lnbn[sn] == 4)
						addlist(sn,&slist);
#ifdef NEVER
					if (lnbf[sn][c] == 0 && rterv[sn][1-c] > 0 && 
						(edge[sn] > edge[s] || edge[sn] > 3) &&
						board[s+s-sn] != NOGROUP) {
						addlist(sn,&slist);
		   				}
#endif
					}
				for (ptr = nblbp[cpoint]; ptr != EOL; ptr = link[ptr]) {
					if (lnbn[list[ptr]] == 4 && comlist(nblbp[list[ptr]], armylbp[army]))
						addlist(list[ptr], &slist);
					}
				if (slist == EOL)
					addlist(s, &slist);
				}
			}
		}
	return(slist);
	}


/* return a list of points where army can run to from point s 
 * type is the type of running point at s
 */

list_t runpoints(army_t army, sqr_t s, int type) {
	list_t rlist = EOL, ptr, ptr2, tmplist = EOL;
	int c, eyflag, numopen, i, ldtmp, num3, best;
	sqr_t ept = 0, sn;
	if (type >= 10 && type != 13 ||
		lnbn[s] == 3 && grlibs[lgr[s]] <= 2) {  /* can capture or atari here */
		adflist(s,&rlist);
		return(rlist);
		}
	c = G_COLOR(list[armygroups[army]]);
	if (lnbn[s] == 3) {
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
			if (lnbn[list[ptr]] == 4 && (edge[list[ptr]] > 3 || 
				edge[list[ptr]] > edge[s] ||
				type >= 7 && edge[list[ptr]] == edge[s])) {
				eyflag = FALSE;
				numopen = num3 = 0;
				for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if ((sqr_t)list[ptr2] == s)continue;
					if (lnbn[list[ptr2]] == 4)numopen++;
					if (lnbn[list[ptr2]] == 3)num3++;
					if (ld[list[ptr2]] >= 4 && ld[list[ptr2]] < 9 &&
						lnbf[list[ptr2]][c] != 0) {
						eyflag++;
						ept = list[ptr2];
						}
					}
				if (eyflag == 1 && edge[list[ptr]] > 1) {
					sn = list[ptr] + list[ptr] - ept;
					if (lnbn[sn] == 4)
						addlist(sn,&rlist);  /* dog face or knight jump */
					}
				if ((numopen > 0 || num3 == 3) && !eyflag)
					addlist(list[ptr],&rlist);
				if (board[s+s-list[ptr]] != NOGROUP && numopen == 3 &&
					armysize[army] < 3)  /* one pt jump spot */
					for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (lnbn[list[ptr2]] != 4)continue;
						sn = list[ptr2] + 2*s - 2*list[ptr];
						if (ld[sn] == 2 || S_NEUTRAL(sn))continue;
						addlist(list[ptr2],&rlist);	  /* knight or 2 pt jump */
						}
				}
			else if (S_COLOR(s+s-list[ptr]) == c && lnbn[list[ptr]] == 3 &&
				lnbf[list[ptr]][c] == 1 && /* run while making a good shape connection */
				(edge[list[ptr]] > 3 || edge[list[ptr]] > edge[s])) {
					for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (lnbn[list[ptr2]] == 4)
							addlist(list[ptr2],&rlist);
						}
				}
			else if (S_COLOR(s+s-list[ptr]) == c && lnbn[list[ptr]] == 3 &&
				lnbf[list[ptr]][1-c] == 1 && /* 3/01 attach to run */
				(edge[list[ptr]] > 3 || edge[list[ptr]] >= edge[s]) && board[list[ptr]+list[ptr]-s] == NOGROUP) {
						addlist(list[ptr],&rlist);
				}
			}
		}
	if (ld[s] >= 4) {
		i = fdir[s];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = s + nbr[i];
			if (grarmy[board[sn]] == army && lnbn[s-nbr[i]] == 4 &&
			   lnbn[s-nbr[i]-nbr[i]] == 4 && 
			   (edge[s-nbr[i]] > 3 || edge[s-nbr[i]] >= edge[s]))
				addlist((sqr_t)(s-nbr[i]-nbr[i]),&rlist);
			}
		}
	/* sort the list by the amount of enemy influence here */
	while(rlist != EOL) {
		best = -1000;
		for (ptr = rlist; ptr != EOL; ptr = link[ptr]) {
			if (rterv[list[ptr]][1-c]-rterv[list[ptr]][c] > best) {
				best = rterv[list[ptr]][1-c]-rterv[list[ptr]][c];
				sn = list[ptr];
				}
			}
		adflist(sn, &tmplist);
		dellist(sn, &rlist);
		}
	return(tmplist);
	}

/* does jump for army from s to sn2 end up near friendly stone?
 * or does jump end on a connection point? s is a liberty of army, 
 * sn is one pt jump, sn2 is 2 pt jump
 */

static int tosamearmy(army_t army, int c, sqr_t s, sqr_t sn, sqr_t sn2)
{
	list_t ptr, ptr2;
	sqr_t sn3, sn4;
#ifdef NEVER
	for (ptr = nbgrp[sn2][c]; ptr != EOL; ptr = link[ptr])
		if (G_ARMY(list[ptr]) == army)
			return TRUE;
#endif
	if (lnbf[sn2][c])
		return TRUE;	/* 3 pt jump or large night - handled by connections */
	for (ptr = nblbp[sn2]; ptr != EOL; ptr = link[ptr]) {
		if ((sqr_t)list[ptr] == sn)
			continue;
		for (ptr2 = nbgrp[list[ptr]][c]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (G_ARMY(list[ptr2]) == army)
				return TRUE;
		}
	}
	sn3 = sn2 + sn2 - sn;	/* 3 pt jump */
	if (board[sn3] != NOGROUP)
		return FALSE;
	for (ptr = nbgrp[sn3][c]; ptr != EOL; ptr = link[ptr])
		if (grarmy[list[ptr]] == army)
			return TRUE;
#ifdef NEVER
	12/07 - too strict - could be another friendly group
	if (lnbf[sn3][c])
		return TRUE;
#endif
	sn4 = sn3 + sn3 - sn2;
	if (board[sn4] != NOGROUP)
		return FALSE;
	for (ptr = nbgrp[sn4][c]; ptr != EOL; ptr = link[ptr])
		if (grarmy[list[ptr]] == army)
			return TRUE;
	return FALSE;
}
        
/* canrunhere looks at liberty s of army to see if group can run
 * away here.  Value returned is:
 * -1 can't run here
 * 0 - Wide open - strong friendly influence only, or unfriendly light 
 * 1 - Wide open - friendly influence only, or unfriendly very light
 * 2 - Wide open - friendly and unfriendly influence, more friendly 
                   and unfriendly light or running away from
 * 3 - Wide open - more unfriendly influence, but light or running away from
 * 4 - Wide open - more friendly influence, but unfriendly heavy or
 					running towards.
 * 5 - Wide open - more unfriendly influence and is increasing, or is heavy and decreasing.
 * 6 - Wide open - more unfriendly influence and is increasing and is heavy
 * 7 - Wide open - running along edge toward friend, and can extend here
 * 8 - Wide open - running along edge toward enemy, and can extend here 
 *    (7 and 8 are special, with low values to avoid double counting with extensions)
 *    (if there is no extension at this point, should be one of the others)
 * 9 (new) - Wide open (can jump) - but jumping close to own stones, so not running
 * 10 - neutral point can push, then jump sideways, or pushing on weak stone
 * 11 - neutral hole can push thru to open area
 * 12 - short of liberties - can only extend here, not jump
 * 13 - enemy stone blocking path or unfriendly influence heavy and increasing
 */


int canrunhere(army_t army, sqr_t s)
{
	int rflag, c, i, ldtmp, sum, canrun, oflag, osqr, runval;
	int sflag;	/* runing close to self */
	int runbest = MAXOPENRUN+1, jumpflag = FALSE, jump;
	int lib3cnt, opncnt;
	list_t ptr, ptr2, ptr3;
	sqr_t sn, sn2, sn3;
	c = G_COLOR(list[armygroups[army]]);
	if (edge[s] < 2 || edge[s] == 2 && lnbn[s] < 3)
		return(-1);
	if (S_NEUTRAL(s)) {  /* neutral, can push through to friendly influence? */
		canrun = FALSE;
		for (ptr = nbgrp[s][1-c]; ptr != EOL; ptr = link[ptr]) {
			if (grlibs[list[ptr]] <= 2 && 
				G_ALIVE(list[ptr]) != DEAD &&
				!G_THREATENED(list[ptr])) {
				for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (lnbn[list[ptr2]] == 4 && edge[list[ptr2]] > 3)
						canrun = TRUE;	/* can atari to push through hole */
				}
			}
		}
		if (lnbn[s] == 1 && lnbn[list[nblbp[s]]] == 4) {
			lib3cnt = 0;
			for (ptr = nbgrp[s][1-c]; ptr != EOL; ptr = link[ptr]) {
				if (grlibs[list[ptr]] == 3)
					lib3cnt++;
			}
			if (lib3cnt == 2) {
				opncnt = 0;
				for (ptr = nblbp[list[nblbp[s]]]; ptr != EOL; ptr = link[ptr]) {
					if (list[ptr] != s && lnbn[list[ptr]] >= 3)
						opncnt++;
				}
				if (opncnt == 3)
					canrun = TRUE;	/* can double atari after block */
			}
		}
		if (lnbn[s] == 2) {
			sum = 0;
			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
				sum += list[ptr] - s;
				if (S_COLOR(s + s - list[ptr]) != c) {
					if (lnbn[list[ptr]] == 4 &&
						(edge[list[ptr]] >= 4 || edge[list[ptr]] > edge[s]))
						jumpflag = TRUE;
					continue; /* straight out from stone */
				}
				sn = list[ptr] + list[ptr]-s;  /* one more point out */
				if (lnbn[list[ptr]] != 4)
					continue;  /* get to open area */
				if (runterv[list[ptr]][1 - c] >= MAXRTVAL)
					continue;  /* too much enemy infuence */
				
				if (edge[list[ptr]] < edge[s] && edge[s] < 5 ||
				    edge[list[ptr]] < 3)
					continue; /* must run toward center */
				if (runterv[list[ptr]][c] > rtval1[2][A_ALIVE(army)])
					canrun = TRUE;  /* friendly influence nearby */
				else if (lnbn[sn] == 4 && 
					runterv[sn][1-c] < 
						runterv[list[ptr]][1-c] || runterv[sn][1-c] < 200)
					canrun = TRUE;  /* run away from enemy */
			}
		}
		else
			sum = 1;
		if (sum != 0 && canrun) {
			if (jumpflag)
				return 10;
			return 11;
		}
		return -1;
	}
	if (lnbn[s] < 2)
		return -1;
	/* s is liberty with 3 adjacent empty points.  is it wide open? */
	rflag = FALSE;  /* can run at all? */
	oflag = FALSE;  /* is it wide open? */
	osqr = NOSQUARE; /* one point jump from stone */
	sflag = TRUE;	/* not to self */
	for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
		sn = list[ptr2];	/* empty one point jump point */
		if (edge[sn] <= 2)   /* no running on or too second line */
			continue;
		sn2 = sn + sn - s;  /* straight out from stone, 2 pt jump */
		if (edge[sn2] <= 1)
			continue;
		jump = edge[s] > 1 && board[sn2] == NOGROUP &&
			S_COLOR(s + s - sn) == c && S_ARMY(s + s - sn) == army;  /* sn is one pt jump from friendly stone */
		if (jump && lnbf[sn][1-c] == 1 && grsize[list[nbgrp[sn][1-c]]] == 1)
			sn3 = mvs[grpieces[list[nbgrp[sn][1-c]]]];  /* point with enemy stone */
		else
			sn3 = sn;
		if (jump && !tosamearmy(army, c, s, sn, sn2))
			sflag = FALSE;  /* can run, but not wide open - close to self 3/02 - reversed sense so no sflag if any diretion is good */ 
		if (jump &&  /* sn is one point jump straight out from stone */
			G_COLOR(lgr[sn]) == 1-c && /* and attaches to enemy stone */
			grsize[lgr[sn]] == 1 &&
			lnbn[sn] == 3 &&
			lnbn[s] == 3 &&
			lnbf[sn+sn-sn3][1-c] == 0 &&  /* point will extend to after he hane's */
			S_COLOR((sn+sn-sn3)+(sn+sn-sn3)-s) != 1-c &&
			(edge[sn] >= 4 || 
			 edge[sn] > edge[s] || 
			 edge[sn] == edge[s] && 
			 edge[s] >= 3)  &&
			board[sn2] == NOGROUP && lnbn[sn2] == 4) { 
			rflag = TRUE;   /* can attach to side of enemy stone */ 
			for (ptr3 = nblbp[sn]; ptr3 != EOL; ptr3 = link[ptr3]) {
				if (board[sn + sn - list[ptr3]] != NOGROUP &&
					lnbn[list[ptr3]] == 4) {
					sn3 = list[ptr3] + list[ptr3] - sn;   /* point we will end up extending to */
					if (lnbf[sn3][c] || lnbf[sn3][1-c] <= 1) {
						runval = runout(army, s, sn, c);
						if (runval != -1 && runval < runbest) {
							runbest = runval;
							oflag = TRUE;
						}
						break;
					}
				}
			}       	
		}
		else if (lnbn[sn] == 4 &&
				(edge[sn] >= 4 ||  /* anywhere in center */
			     edge[s] == 4 && edge[sn] == 3 && jump || /* down to edge */
			     edge[sn] >= edge[s]) &&   /* along edge or up */
			    (edge[sn] > 3 || jump) &&
				(!jump || lnbf[sn2][c] == 0)) { /* no run if jump into friendly stone - should just connect */
				/* open */
			for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr]) {
				if (lnbn[list[ptr]] == 4 &&
					(edge[list[ptr]] > 3 ||
					 edge[list[ptr]] == 3 && edge[s] == 4 ||
					 edge[list[ptr]] >= edge[s] && edge[list[ptr]] >= 2 && edge2[list[ptr]] > 2)) {
					rflag = TRUE;
					break;
				}
			}
			if (jump && board[sn2] == NOGROUP &&   /* TIGHTENUP */
				lnbn[sn2] == 3 && 
				lnbf[sn2][1-c] == 1 &&
				board[sn2+sn2-sn] == NOGROUP)
				rflag = TRUE; /* attach to enemy 2 points away */
			   
			if (jump &&  /* straight out from stone */
			   board[sn2] == NOGROUP &&
			   lnbf[sn][c] == 0 &&  /* no friendly stone here */
			   /*lnbf[sn2][c] == 0 &&  or here */
			   !inlist(sn2,&armylbp[army]) &&
			   !G_THREATENED(lgr[sn2]) &&
			   
			   (lnbf[sn2][1-c] <= 1 &&
			    S_COLOR(sn2+sn2-sn) != 1-c &&  /* no enemy stone blocking */
			    edge2[sn2] > 3 &&
			    (edge[sn2] >= edge[s] ||
			     edge[sn2] >= 2) ||
			    
			   lnbf[sn2][1-c] == 0 &&
			   lnbf[sn2][c] != 0 &&
			   edge[sn2] > 4)) {
				
				runval = runout(army, s, sn, c);
				if (runval != -1 && runval < runbest) {
					runbest = runval;
					oflag = TRUE;
				}
			}
		}
	}
	if (!rflag)
		return -1;
	if (!oflag)
		return 13;
	if (sflag)
		return 9;  /* must be wide open for 9 */
	if (A_NUMLIBS(army) == 1)
		return 12;
	if (lnbn[s] <= 3 && !S_NEUTRAL(s)) {
		i = fdir[s];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			if (S_COLOR(s+nbr[i]) == c) {
				if (grlibs[board[s+nbr[i]]] == 2 ||
					armylibs[grarmy[board[s+nbr[i]]]] == 3)
					return 12;
				break;
			}
		}
	}
	return runbest;
}

/* determine running ability straight out from stone */		
/* s is liberty.  osqr is point straight out from stone */
/* c is color of army, which is group that is running */
		
static int runout(army_t army, sqr_t s, sqr_t osqr, int c) {
	sqr_t fsqr; /* 3 points straight out from stone */ 
	sqr_t f2sqr;  /* 4 points out from stone */
	list_t ptr;
	int d1t,d2t,d3t;  /* this liberty's contribution to runterv by distance */
	int alongedge;
	int extendhere;
	fsqr = osqr + osqr - s;
	f2sqr = fsqr + fsqr - osqr;
	if (board[f2sqr] != NOGROUP)
		f2sqr = fsqr;
	alongedge = edge[s] <= 4 && edge[fsqr] <= edge[s]; /* reduced value unless no extension here */
	if (alongedge && lnbn[s] > 2) {	/* 4/01 see if there is an extension here */
		extendhere = FALSE;
		for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr])
			if (pots[list[ptr]].pot_type == EXTEND) {
				if (pots[list[ptr]].pot_where == s /* &&
					pots[list[ptr]].pot_val > 2 if can't extend, can't run either! */) {
					extendhere = TRUE;
					break;
					}
				if (inlist(pots[list[ptr]].pot_where, &nblbp[s])) {
					extendhere = TRUE;
					break;
					}
				}
		if (!extendhere)
			alongedge = FALSE;	/* no conflict with extension */
		}
	if (lnbf[fsqr][1-c] != 0 && ld[fsqr] > 2 && !S_NEUTRAL(fsqr) ||
		lnbf[f2sqr][1-c] != 0 && ld[f2sqr] > 2 && !S_NEUTRAL(f2sqr))
		return -1;    /* enemy wall in the way */
	d1t = rtval1[1][A_ALIVE(army)];
	d2t = rtval1[2][A_ALIVE(army)];
	d3t = rtval1[3][A_ALIVE(army)];
	if (lnbf[s][A_COLOR(army)] > 1) {  /* two stones contributing */
		d1t *= 2;
		d2t *= 2;
		d3t *= 2;
		}
	else if (ld[s] > 2 && !S_NEUTRAL(s)) {  /* another friendly stone nearby */
		d1t = (int)(d1t * 1.5);
		d2t = (int)(d2t * 1.66);
		d3t = (int)(d3t * 1.75);
		}
	for (ptr = nblbp[osqr]; ptr != EOL; ptr = link[ptr]) {
		if ((sqr_t)list[ptr] == fsqr || (sqr_t)list[ptr] == s)continue;
		if (lnbf[list[ptr]][c] != 0) {
			d2t += rtval1[2][A_ALIVE(army)];
			d3t += rtval1[3][A_ALIVE(army)];
			}
		}
	if (A_THREATENED(army) == 2) {
		d1t = (int)(d1t * THFAC2);
		d2t = (int)(d2t * THFAC2);
		d3t = (int)(d3t * THFAC2);
	}
	else if (A_THREATENED(army) == 1) {
		d1t = (int)(d1t * THFAC1);
		d2t = (int)(d2t * THFAC1);
		d3t = (int)(d3t * THFAC1);
	}
	d1t = runterv[s][c]-d1t;    /* subtract off own adjusted influence by distance */
	d2t = runterv[osqr][c]-d2t;
	d3t = runterv[fsqr][c]-d3t;
	if (d3t < 10 && (runterv[fsqr][1-c] >= MAXRTVAL || /* 8/99 was rtval/2 - missed good running points */
		runterv[osqr][1-c] >= MAXRTVAL))
		return(13);
	if (runterv[fsqr][1-c] >= MAXRTVAL && 
		runterv[fsqr][1-c] >= runterv[osqr][1-c]) {
		if (d3t < 10)return(-1);  /* no friends, heavy enemy */
		return(13);
		}
#ifdef NEVER
	if (lnbn[s] < 3) {  /* connection - too strict need some other way to handle peeps */
		for (ptr = cnbrd[s]; ptr != EOL; ptr = link[ptr])
			if (cntype[list[ptr]] == CN_HANE || 
				cntype[list[ptr]] == CN_THREAT ||
				cntype[list[ptr]] == CN_ONEPOINTJUMP)return(10);  /* opponent can peep so can't run well here */
		}
#endif
		
	if (runterv[fsqr][1-c] <= MAXRTVAL/8 && 
		runterv[osqr][1-c] <= MAXRTVAL/8 && edge[s] > 2) {
		if (d2t > MAXRTVAL/8 && d3t > MAXRTVAL/8 || 
			d3t >= d2t && d3t >= MAXRTVAL/8)return(alongedge?7:0);
		else return(alongedge?7:1);
		}
	else if (d2t >= runterv[osqr][1-c] && d3t >= runterv[fsqr][1-c]) {
		if ((runterv[s][1-c] > runterv[osqr][1-c] && runterv[osqr][1-c] < MAXRTVAL/2 ||
		 runterv[osqr][1-c] < MAXRTVAL/4) && edge[s] > 2)
			return(alongedge?7:2);
		return(alongedge?8:4);
		}
	else if ((   (runterv[fsqr][1-c] < runterv[osqr][1-c] && runterv[f2sqr][1-c] < runterv[osqr][1-c] || 
		         lnbf[fsqr][1-c] == 0 && runterv[f2sqr][1-c] < runterv[fsqr][1-c]) && 
		        (runterv[fsqr][1-c] < MAXRTVAL/2 &&
		         runterv[f2sqr][1-c] < MAXRTVAL/2) ||
		    d3t > 10 && runterv[fsqr][1-c] < MAXRTVAL/2 && runterv[f2sqr][1-c] < MAXRTVAL/2) && /* 4/01 wide open enough */
		   edge[s] > 2) {
			if (!alongedge)
				return 3;
			if (lnbf[osqr][1-c] != 0 || lnbf[fsqr][1-c] != 0 || d3t < 20)
				return 8;
			return 7;
		}
	else if (runterv[fsqr][1-c] - d3t < MAXRTVAL/2 ||
		(runterv[fsqr][1-c] < runterv[osqr][1-c] || 
		lnbf[fsqr][1-c] == 0 && runterv[f2sqr][1-c] < runterv[osqr][1-c]) && 
		runterv[fsqr][1-c] < MAXRTVAL)
		return(alongedge?8:5);
	else return(alongedge?8:6);
	}



