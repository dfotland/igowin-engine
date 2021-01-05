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
# include "g2pat.h"
#if defined(CHECK) || defined(TEST) || defined(G2DEBUGOUTPUT)
# include <stdio.h>
#endif


#ifdef __STDC__
# define _fastcall
static int evalpat(sqr_t s,int o,int ptr, int color, int handicap, int rules, list_t *bstseq, int maxvar, int depth, int move, int p, int str, int alpha, int beta, int bonus, int mcolor);
#endif


extern int maxpatvariations[21],urgalive[NUMALIVE],pmvalue[16];


extern struct newpattern *pt;

extern struct pmove *pm;

extern struct newpatstruct newpat[NUMNEWPATS];



/* get the squares for groups and handles for newpat[np] */
	
static void getpathandles(int str, sqr_t *g1, sqr_t *g2, sqr_t *h1, sqr_t *h2, sqr_t *h3)
{
	int i,p,o,at,av;
	sqr_t s, sn;
	*g1 = *g2 = *h1 = *h2 = *h3 = NOSQUARE;
	p = strat[str].pattern;
	s = strat[str].patsqr;
	o = strat[str].pato;
	for (i = 0; i < NUMATTS; ++i) {
		if ((pt[p].attsqr[i] & 0xff) == 0xff)
			break;
		sn = patgetsqr(s,o,pt[p].attsqr[i]);
		at = pt[p].attval[i]  & 0xe0;
		av = pt[p].attval[i] & 0x1f;
		if (at != AT_NONE)
			continue;
		switch (av) {
		      case AT_G1:
				*g1 = sn;
				break;
		      case AT_G2:
				*g2 = sn;
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
	}
}
	

/* is the reason in strat[str] satisfied at the curent board position?
 * str is the reason (index into strat)
 * move is index of the original move made being evaluated
 * c is the color of the move just made
 * if so, set goodrule in strat[str].
 * return TRUE if reason satisfied
 *
 * engame patterns must end with original move alive.
 * shape patterns always are good - worth evaluating at least
 * cut patterns must end with g1,g2 cut if pattern black moved first or
 *     g1,g2 connected if pattern white moved first.
 * invade patterns must end with pattern black invading group not dead if pattern
 * 		black moved first.  if pattern white moved first, last move must be not dead
 * if the pattern has a negative bonus (starts with bad move), then the
 *      rule always applies.
 */
	
int patgoodreason(int str, int move, int c)
{
	sqr_t s1,s2,h1,h2,h3;  /* squares for groups and handles */
	group_t g1,g2;
	list_t ptr, ptr2;
	int alive;
#ifdef TEST	
	char buf[10];
#endif	
	getpathandles(str, &s1, &s2, &h1, &h2, &h3);
	g1 = board[s1];
	g2 = board[s2];
	switch(strat[str].reason) {
		case PATINVSEQUENCE:
		case PATMATCH:		/* shape pattern */
		case PATSEQUENCE:  	/* complete sequence */
			strat[str].goodrule = TRUE;  /* once started, all moves are good */
			break;
		case PATDEFEND:		/* defend against invasion */
			strat[str].goodrule = S_ALIVE(mvs[move]) < WEAK;
			break;
		case ENDPATMATCH:   /* endgame */
			strat[str].goodrule = board[mvs[move]] != NOGROUP && S_ALIVE(mvs[move]) <= ALIVE;
			break;
		case PATCUTSEQUENCE:
		/* 10/96 - patcutsequence must also check if cut works, since
		may play first move of bad cutting sequence for another good reason,
		but don't want to finish the bad cutting sequence */
		case PATCUT:
			strat[str].goodrule = S_ARMY(s1) != S_ARMY(s2);
			if (board[mvs[move]] == NOGROUP ||
				S_THREATENED(mvs[move]) == 2 ||
				S_ALIVE(mvs[move]) >= WEAK)strat[str].goodrule = FALSE;
			for (ptr = armygroups[S_ARMY(s1)]; ptr != EOL; ptr = link[ptr])
				for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
					if (G_THREATENED(list[ptr2]) == 2 && 
						comlist(grnbp[list[ptr2]], armygroups[S_ARMY(s2)])) {
						strat[str].goodrule = FALSE;
				}
			/*	check for common threatened neighbor */
			break;
		case PATCONNECT:
		case PATKILLCUTSTONES:
			strat[str].goodrule = S_ARMY(s1) == S_ARMY(s2) &&
				gralive[g1] <= SEMEAI;
			break;
		case PATSURROUND:
			strat[str].goodrule = S_ALIVE(mvs[move]) < WEAK;
			if (S_THREATENED(mvs[move]))strat[str].goodrule = FALSE;
			break;
		case PATRUN:
			if (g1 == NOGROUP)
				alive = DEAD;
			else
				alive = G_ALIVE(g1);
			strat[str].goodrule = alive == RUNNING_FIGHT ||
							alive == UNSETTLED_LIMP ||
							alive <= ALIVE || 
							alive == SEMEAI ||
							alive == STRONG_SEMEAI;
			if (S_THREATENED(mvs[move]))strat[str].goodrule = FALSE;
			if (g1 == NOGROUP || G_ARMY(g1) != S_ARMY(mvs[move]))
				strat[str].goodrule = FALSE;  /* 10/17/97 running move must be connected to main group */
			break;
		case PATKILL:
			strat[str].goodrule = S_ALIVE(s1) > UNSETTLED;
			break;
		case PATSAVE:
			strat[str].goodrule = S_ALIVE(s1) <= ALIVE;
			break;
		case PATINVADE:
			strat[str].goodrule = S_ALIVE(mvs[move]) < WEAK_POTENTIAL &&
						grsize[board[mvs[move]]] == 1 ||
						S_ALIVE(mvs[move]) == WEAK_POTENTIAL &&
							armyeyespace[S_ARMY(mvs[move])] >= 8 ||
						S_ALIVE(mvs[move]) <= WEAK_LIMP && mvcolor[move] != c ||  /* 8/01 weak, but my move now */ 
						S_ALIVE(mvs[move]) <= RUNNING_FIGHT;	/* 8/01 good enough for an invasion */
			break;
#ifdef TEST		
		default:
			outerror("Unrecognized pattern rule in patgoodreason.\n");
			sprintf(buf,"%d",strat[str].reason);
			outerror(buf);
#endif			
		}
	if (strat[str].value < 0)
		return TRUE;  /* always look at consequences of bad moves */
	return(strat[str].goodrule);
}

/* return index of previous sibling of ptr, -1 if pointing at first sibling */

int patprev(int ptr)
{
	int scount = 0;
	if (!PM_FINAL(ptr - 1))
		return -1;
	do {
		--ptr;
		if ((pm[ptr].csfv) & MV_SIBLING)
			scount--;
		if ((pm[ptr].csfv) & MV_FINAL)
			scount++;
	} while(scount > 0);
	return ptr;
}

/* return index of next sibling of ptr, -1 if no sibling */

int patmore(int ptr)
{
	int scount = 0;
	if (!PM_SIBLING(ptr))
		return -1;
	do {
		if ((pm[ptr].csfv) & MV_SIBLING)
			++scount;
		if ((pm[ptr].csfv) & MV_FINAL)
			--scount;
		++ptr;
	} while(scount > 0);
	return ptr;
}

#ifdef G2DEBUGOUTPUT
void pmatches(void) {
	sqr_t s;
	list_t ptr;
	int p;
	char buf[200],tmp[10];
	clearerror();
	for (s = 0; s < boardsquare; ++s) {
		for (ptr = newpatbrd[s]; ptr != EOL; ptr = link[ptr]) {
			p = list[ptr];
			sprintf(buf,"%s: p=%d, c=%d, mv=%d, o=%d\r\n",ssqr(s,tmp),
				newpat[p].num,newpat[p].color,newpat[p].move,newpat[p].orient);
			outerr(buf);
			}
		}
	}
#endif

int getnewpatnum(int p)
{
	return(newpat[p].num);
}

sqr_t getnewpatsqr(int p)
{
	return(newpat[p].sqr);
}

short getnewpato(int p)
{
	return(newpat[p].orient);
}

short getnewpatcolor(int p)
{
	return(newpat[p].color);
}

short getnewpatmove(int p)
{
	return(newpat[p].move);
}

#ifdef CHECK

extern char listflag[NUMLIST];

int checkpatterns(int *npbcnt) {
	int retval = TRUE;
	sqr_t i;
	list_t ptr;
	char buf[100];
	
	for (i = firstsquare; i < lastsquare; ++i) {
		for (ptr = newpatbrd[i]; ptr != EOL; ptr = link[ptr]) {
			listflag[ptr] = TRUE;
			++(*npbcnt);
			if (newpat[list[ptr]].orient > 7) {
				sprintf(buf,"Bad pattern orientation %d, pattern %d at: ",
				       newpat[list[ptr]].orient,newpat[list[ptr]].num);
				outerror(buf);
				outerror(ssqr(i,buf));
				retval = FALSE;
				}
			}
		}
	return(retval);

	}
#endif
