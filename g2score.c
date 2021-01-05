/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

# include "g2hd.h"
# include "g2pat.h"
# include "g2tree.h"
# include "g2fcache.h"
/* # include "keys.h" */
# ifndef SMALLCODE
# include <stdio.h>
# endif
#ifdef G2DEBUGOUTPUT
# include <string.h>
#endif


#ifdef DEMO
extern int demolevel;
#endif
  
/* copyright 1984 david fotland */

#ifdef __STDC__
# include "g2score.pr"
#else
void osqr();
void outstone();
static void putcommand();
void putamove();
void displaychar();
void highlastmove();
#endif
  
extern int sumeyes[],sumpots[];
extern int ccl[3];
extern int semnbralive[NUMALIVE],semalive[NUMALIVE];


/* return TRUE if g is dead (for scoring purposes) */
/* if g is in swaplist, return the opposite */

int isdead(group_t g, list_t swaplist)
{
	if (gralive[g] == DEAD && deadinsidedead(g))
		return inlist(g, &swaplist);
	if (gralive[g] < WEAK_LIMP)
		return inlist(g, &swaplist);
	return !inlist(g, &swaplist);
}  

/* list of groups to reverse liveness during a score */
list_t swaplist = EOL;

/* return the score on the board (positive good for white) in points (not including 0.5 for komi).  
 * swap is point in group to switch aliveness of.
 * -1 clears the swap list, -2 leaves it unchanged
 * color is returned color of empty terr. (0,1 color, 2 unknown, 3 stone)
 * dead is returned
 * list of dead stones
 */

int getthescore(int rules, int handicap, int swap, int color[], int dead[])
{
	int tmp, sc = 0;
	list_t ptr;
	sqr_t i;

	fixplaylevel(MAXLEVEL);
	life(FALSE);
	if (swap == -1) {
		killist(&swaplist);
	}
	else if (swap != -2 && board[swap] != NOGROUP) {
		if (inlist(board[swap], &swaplist)) {
			for (ptr = armygroups[grarmy[board[swap]]]; ptr != EOL; ptr = link[ptr]) {
				dellist(list[ptr], &swaplist);
			}
		}
		else
			mrglist(armygroups[grarmy[board[swap]]], &swaplist);
	}
	radiaterun(swaplist); 
	radiateterr(swaplist); 
	for (i = firstsquare; i < lastsquare; i++) {
		color[i] = 2;
		dead[i] = FALSE;
		tmp = terr(rules, i, swaplist);     /* get whose territory */
		if (board[i] != NOGROUP) {
			color[i] = 3;
			if (isdead(board[i], swaplist)) {
				dead[i] = TRUE;
				if (rules == JAPANESE || rules == AMERICAN)
					sc += 2 * ccl[tmp];
				else
					sc += ccl[tmp];
			}
			else if (rules == GOE || rules == CHINESE || rules == CGOS)
				sc += ccl[tmp];
		}
		else {
			sc += ccl[tmp];
			color[i] = tmp;
		}
	}
	if (rules == JAPANESE || rules == AMERICAN)
		sc += numpris[1] - numpris[0];
	if (rules == CHINESE)
		sc += handicap;
	sc += komi / 50;
	return sc;
}
  
/* is there a dead group of color c on a diagonal from point s */

int deaddiag(sqr_t s, int c, list_t swaplist)
{
	int i, ldtmp;
	sqr_t sn;
	i = fdir[s];
	for (ldtmp = ldiag[i]; i < ldtmp; ++i) {  /* look at diagonals */
		sn = s + diags[i];
		if (board[sn] != NOGROUP && S_COLOR(sn) == c && 
			(S_ALIVE(sn) > UNSETTLED && !inlist(board[sn], &swaplist) ||
			 S_ALIVE(sn) <= UNSETTLED && inlist(board[sn], &swaplist)))
			return TRUE;
	}
	return FALSE;
}
  
/* figure out who owns this square, Black, White, or unknown.
if the group that controls this square is in swaplist, reverse the
result */

extern int rtthreshold;

#define SOLIDTERR 30

int terr(int rules, sqr_t s, list_t swaplist)
{
	group_t g,g2;
	int i, ldtmp, cflag, wl, bl, wseki, bseki;
	list_t ptr, ptr2;
	sqr_t sn;
	g = lgr[s];
	if (ld[s] == NOLD) {  /* open area away from stones */
		if (terv[s] == 0)
			return NOCOLOR;
		if (ltrgd[s] == 3 || ltrgd[s] == 7) {
			if (rterv[s][1] > 50 && rterv[s][0] > 50)
				return NOCOLOR;
			if (rterv[s][1] > rterv[s][0])
				return WHITECOLOR;
			else if (rterv[s][1] < rterv[s][0])
				return BLACKCOLOR;
		}
		if (terv[s] <= -SOLIDTERR)
			return BLACKCOLOR;  /* half point is good enough*/
		else if (terv[s] >= SOLIDTERR)
			return WHITECOLOR;
		else if (ltrgd[s] == 0 && ltr1[s] != 0) {
			if (rterv[s][1] > rterv[s][0])
				return WHITECOLOR;
			else if (rterv[s][1] < rterv[s][0])
				return BLACKCOLOR;
			else return NOCOLOR;
		}
		else if (ltrgd[s] == 2 || ltrgd[s] == 6) {
			if (rterv[s][0] == 0 && rterv[s][1] == 0)
				return NOCOLOR;
			if (rterv[s][0] <= 0)
				return WHITECOLOR;
			else if (rterv[s][1] <= 0)
				return BLACKCOLOR;
			else return NOCOLOR;
		}
		else return NOCOLOR;
	}
	else if (S_NEUTRAL(s)) {                  /* neutral square */
		wl = FALSE;  /* any live white */
		bl = FALSE;  /* any live black */
		wseki = bseki = FALSE;
		for (ptr = nbgrp[s][0]; ptr != EOL; ptr = link[ptr]) {
			if (!isdead((group_t)list[ptr], swaplist)) {
				bl = TRUE;
			}
			if (rules == JAPANESE && G_ALIVE((group_t)list[ptr]) == SEKI) {
				if (armynbp[grarmy[list[ptr]]] == EOL)
					getarmynbp(grarmy[list[ptr]]);
				for (ptr2 = armynbp[grarmy[list[ptr]]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (A_ALIVE(list[ptr2]) == SEKI)
						bseki = TRUE;
				}
			}
		}
		for (ptr = nbgrp[s][1]; ptr != EOL; ptr = link[ptr]) {
			if (!isdead((group_t)list[ptr], swaplist)) {
				wl = TRUE;
			}
			if (G_ALIVE((group_t)list[ptr]) == SEKI) {
				if (armynbp[grarmy[list[ptr]]] == EOL)
					getarmynbp(grarmy[list[ptr]]);
				for (ptr2 = armynbp[grarmy[list[ptr]]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (A_ALIVE(list[ptr2]) == SEKI)
						wseki = TRUE;
				}
			}
		}
		if (wl && !bl) {
			if (rules == JAPANESE && wseki)
				return NOCOLOR;  /* no points in seki */
			return WHITECOLOR;
		}
		if (!wl && bl) {
			if (rules == JAPANESE && bseki)
				return NOCOLOR;  /* no points in seki */
			return BLACKCOLOR;
		}
		
		if (rules == JAPANESE || rules == AMERICAN)
			return NOCOLOR;
		if (!wl && !bl)
			return NOCOLOR;
		wl = cntplyhere(s, 1, NOGROUP); /* YES if Black dies here */
		bl = cntplyhere(s, 0, NOGROUP); /* YES if White dies here */
		
		if (wl == YES && bl != YES)
			return WHITECOLOR;
		if (wl != YES && bl == YES)
			return BLACKCOLOR;
		
		return NOCOLOR;		
		}
	else { /* on stone or all nbr stones same color */
		if (isdead(g, swaplist))
			return 1 - grcolor[g];
		if (board[s] != NOGROUP) {  /* on a stone */
			if (rules == JAPANESE || rules == AMERICAN)
				return NOCOLOR;
			else 
				return grcolor[g];
		}
		if (rules == JAPANESE && G_ALIVE(g) == SEKI) {
			if (armynbp[grarmy[g]] == EOL)
				getarmynbp(grarmy[g]);
			for (ptr2 = armynbp[grarmy[g]]; ptr2 != EOL; ptr2 = link[ptr2])
				if (A_ALIVE(list[ptr2]) == SEKI)
					return NOCOLOR;  /* no points in seki */
		}
		for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
			for (ptr = nbgrp[list[ptr2]][1-grcolor[g]]; ptr != EOL; ptr = link[ptr]) {
				if (!isdead((group_t)list[ptr], swaplist))
					return NOCOLOR;  /* live enemy stone nearby */
			}
		}
#ifdef NEVER
		// OK to have threatened nearby stones at end of game as
		// long as people agree they are alive.
		for (ptr = nbgrp[s][grcolor[g]]; ptr != EOL; ptr = link[ptr])
			if (G_THREATENED(list[ptr]) == 2)
				return NOCOLOR;
#endif
		if (eyerec[s] && eyeval[eyerec[s]] >= 8 &&
			(eyetype[eyerec[s]] == ONEPOINTEYE ||
			 eyetype[eyerec[s]] == TWOPOINTEYE))
			return grcolor[g];  /* small eye must be point */
		if ((rules == JAPANESE || rules == AMERICAN) &&
			rterv[s][1-grcolor[g]] >= 50)
			return NOCOLOR;
		if ((rules == JAPANESE || rules == AMERICAN) && 
			lnbn[s] <= 1 && cnbrd[s] != EOL) {  /* must be connection here to make false eye */
			if (deaddiag(s, 1 - grcolor[g], swaplist))
				return grcolor[g];
			i = fdir[s];  /* false eye and group with no other connections */
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				sn = s + nbr[i];
				g2 = board[sn];
				if (g2 == NOGROUP) {
					continue;
				}
				if (grlibs[g2] > 3)
					continue;
				cflag = FALSE;
				for (ptr = grlbp[g2]; ptr != EOL; ptr = link[ptr]) {
					if ((sqr_t)list[ptr] == s)
						continue;
					if (eyerec[list[ptr]] && eyeval[eyerec[list[ptr]]] >= 8) {
						cflag = TRUE;
						break;
					}
					if (cnbrd[list[ptr]] != EOL &&
						grcolor[cngr1[list[cnbrd[list[ptr]]]]] == grcolor[g2] &&
						cnprot[list[cnbrd[list[ptr]]]] > CANT_CONNECT) {
							cflag = TRUE;
							break;
					}
					if (lkbrd[list[ptr]] != EOL &&
						grcolor[cngr1[list[lkbrd[list[ptr]]]]] == grcolor[g2] &&
						cnprot[list[lkbrd[list[ptr]]]] > CANT_CONNECT) {
							cflag = TRUE;
							break;
					}
					if (llbrd[list[ptr]] != EOL &&
						grcolor[cngr1[list[llbrd[list[ptr]]]]] == grcolor[g2] &&
						cnprot[list[llbrd[list[ptr]]]] > CANT_CONNECT) {
							cflag = TRUE;
							break;
					}
					if (lnbn[list[ptr]] == 3) {  /* can run? */
						cflag = TRUE;
						break;
					}
					if (lnbn[list[ptr]] > 0 && lnbn[list[nblbp[list[ptr]]]] > 2) { /* big eye */
						cflag = TRUE;
						break;
					}
					if ((gralive[lgr[list[ptr]]] == DEAD || grthreatened[lgr[list[ptr]]]) &&
						grcolor[lgr[list[ptr]]] == 1-grcolor[g2]) {
						cflag = TRUE;
						break;
					}
				}
				if (!cflag)
					return NOCOLOR;
			}
		}
		if ((rules == GOE || rules == CHINESE || rules == CGOS) && lnbn[s] <= 1) {
			if (lnbf[s][0] == 0)
				return WHITECOLOR;
			if (lnbf[s][1] == 0)
				return BLACKCOLOR;
		}
		if (ltr1[s] && ltrgd[s] == 0)
			return grcolor[g];
		if (ltr1[s] && (ltrgd[s] == 3 || ltrgd[s] == 7) &&
			rterv[s][1-grcolor[g]] > 50)
			return NOCOLOR;
		if (terv[s] < -SOLIDTERR)
			return BLACKCOLOR;
		else if (terv[s] > SOLIDTERR)
			return WHITECOLOR;
		else if (!ltr1[s] && rterv[s][1-grcolor[g]] < rtthreshold)
			return grcolor[g];
		else if (rterv[s][1-grcolor[g]] <= 20)
			return grcolor[g];  /* completely surrounded */
		return NOCOLOR;
	}
}


#ifdef G2DEBUGOUTPUT

extern int savepassval;

static char *phases[] = 
{
	"Opening",
	"Middle",
	"Endgame",
};

static char *ahds[] = 
{
	"way behind",
	"behind",
	"even",
	"ahead",
	"way ahead",
};

void outscores(void)
{
	sqr_t s;
	char buf[120];
	unhighlight();
	sprintf(buf," %s, %s, agressive %d(0-10).  Passval - %d(%d). * for urgent", 
		phases[phase], ahds[ahead], agressiveness, savepassval / 50,savepassval);
	outerr(buf);
	for (s = 0; s < boardsquare; ++s) {
		if (scoreval[s] != BIGNUM && scoreval[s] != SMALLNUM) {
			sprintf(buf,"%d", scoreval[s] / 50);
			outstone(s, buf);
		}
	}
}

#define S_URG 100000
#define S_EVAL 50000
#define S_UNUSED -100000

void outscoreorder(void)
{
	int move[NUMSQUARES], value[NUMSQUARES];	/* sorting array */
	int i, tmp, done;
	char buf[20];
	clearerror();
	unhighlight();
	outerr("Move sorting order from last lookahead. !=evaluated\n");
	for (i = 0; i < boardsquare; ++i) {
		move[i] = i;
		if (scoreval[i] != BIGNUM) {
			value[i] = scoreval[i] + S_EVAL;
		}
		else if (stratguess[0][i] != 0)
			value[i] = stratguess[0][i];
		else
			value[i] = -100000;
	}
	/* now bubble sort them */
	do {
		done = TRUE;
		for (i = 0; i < boardsquare-2; ++i) {
			if (value[i] < value[i+1]) {
				tmp = move[i];
				move[i] = move[i+1];
				move[i+1] = tmp;
				tmp = value[i];
				value[i] = value[i+1];
				value[i+1] = tmp;
				done = FALSE;
			}
		}
	} while(!done);
	for (i = 0; i < boardsquare; ++i) {
		if (value[i] == S_UNUSED)
			break;
		if (value[i] >= S_EVAL)
			sprintf(buf, "%d!", i + 1);
		else 
			sprintf(buf, "%d", i + 1);
		outstone((sqr_t)move[i], buf);
	}
}

void outlal(void)
{
	sqr_t s;
	char buf[10];
	for (s = 0; s < boardsquare; ++s) {
		if (board[s] != NOGROUP) {
			sprintf(buf, "%d", S_ALIVE(s));
			if (S_THREATENED(s) == 1)
				strcat(buf, "*");
			else if (S_THREATENED(s) == 2)
				strcat(buf, "!");
			outstone(s, buf);
		}
	}
}

void outalprob(void)
{
	sqr_t s;
	char buf[10];
	for (s = 0; s < boardsquare; ++s) {
		if (board[s] != NOGROUP) {
			sprintf(buf, "%d", gralprob[board[s]]);
			outstone(s ,buf);
		}
	}
}


void outlgr(void)
{
	sqr_t s;
	char buf[10];
	for (s = 0; s < boardsquare; ++s) {
		if (board[s] == NOGROUP) {
			sprintf(buf, "%d", lgr[s]);
			outstone(s, buf);
		}
	}
}

#endif  /* of ifdef G2DEBUGOUTPUT */

char *alives[NUMALIVE] = {
"New group",
"Unconditionally alive - has two eyes, or enough territory to make two eyes, or several ways to make second eye",
"Alive with barely 2 eyes in territory",
"Exactly two ways to make two eyes",
"Alive.  At least a seki",
"Alive since can win fight against adjacent group",
"Can live or run away",
"Can run easily, no eyes",
"Unsettled.  Can live in one move or maybe run away",
"Unsettled, but likely will win fight against adjacent group",
"Need to fill ko to live",
"Running fight between this group and adjacent group",
"Lives or dies in one move, surrounded",
"Unsettled, in fight with enemy group, one will likely die",
"Need to run away to live",
"Need to take ko and connect to live",
"Weak, almost surrounded",
"Unsettled, likely will lose fight against adjacent group",
"Surrounded, probably dead, but might get two eyes",
"Surrounded, probably dead, but has some eye potential",
"Dead, loses fight to adjacent group",
"Dead, surrounded, slight eye potential",
"Dead, surrounded, no eyes",
"bad value",
"Unused",
"Tactically captured, even if move first" };

#ifdef G2DEBUGOUTPUT
  
static char *etypes[] = {
	"NO EYE  ",
	"1 PT EYE",
  	"2 PT EYE",
	"DEAD EYE",
	"THRT EYE",
	"BIG EYE ",
	"LINE EYE",
	"4 PT BLK",
	"OPN LINE",
	"CORNER  ",
	"NEAR EDG",
	"VERY BIG",
};

char *cntpstr[] = {
"UNKN ",
"1Pt J",
"KNT  ",
"2PT J",
"HANE ",
"BBJ  ",
"DIAG ",
"MPL  ",
"DEAD ",
"THRT ",
"H KNT",
"3Pt J",
"LgKnt",
"H LKn",
"DDiag",
"CRNR ",
"MHKNT",
"ExLgK",
"H ELK",
"4Pt J",	
};

#endif


/* highlight group g */

int higroup(group_t g)
{
	int lptr,count;
	count = 0; 
	lptr = grpieces[g];
	while (lptr != -1) {
		count++;
		histone(mvs[lptr]); 
		lptr = mvnext[lptr];
	} 
	return count; 
}


extern int sumeyes[41],sumpots[41];
#ifdef G2DEBUGOUTPUT
static char buf2[10],buf3[10],buf4[10],buf5[10],buf6[10],buf7[10],buf8[10],buf9[10],buf10[10],buf11[10];
static char tmp[50];
#endif


static int outbestmove(group_t g)
{
	sqr_t bestmove, secondmove, bestmove2, bestkillmove, bestkillmove2;
	int rest, pthreat, numrest, kopot, weakpot, best, bestmax, secondbest, numbest;
	int bestval2, bestshared;
	army_t a;
	a = grarmy[g];
	best = bestpot(G_ARMY(g), &secondbest, &bestval2, &bestshared, &bestmove, &bestmove2, &bestkillmove, &bestkillmove2, &secondmove, &rest, &pthreat, &kopot, &weakpot, &numrest, &bestmax);
	numbest = 0;
	if (bestmove != PASS && bestmove != NOSQUARE) {
#ifdef NEVER	
		&& (!G_THREATENED(g) || grsavemove[g] != bestmove)) {
#endif		
		numbest++;
		outstone(bestmove, "1");
	}
	return numbest;
}

static void outsecondbestmove(group_t g) {
	sqr_t bestmove,secondmove,bestmove2,bestkillmove,bestkillmove2;
	int rest,pthreat,numrest,kopot,weakpot,best,bestmax,secondbest;
	int bestval2,bestshared;
	army_t a;
	a = grarmy[g];
	best = bestpot(G_ARMY(g),&secondbest,&bestval2,&bestshared,&bestmove,&bestmove2,&bestkillmove,&bestkillmove2,&secondmove,&rest,&pthreat,&kopot,&weakpot,&numrest,&bestmax);

	if (secondmove != PASS && secondmove != NOSQUARE && secondmove != bestmove &&
		(!G_THREATENED(g) || grsavemove[g] != secondmove)) {
		outstone(secondmove,"2");
		}

	}
	
	
static int markeyes(group_t g) {
	int count = 0;
	list_t ptr, ptr2;
	for (ptr = armyeyerecs[G_ARMY(g)]; ptr != EOL; ptr = link[ptr]) {
		if (eyeval[list[ptr]] < 8)continue;
		count++;
		for (ptr2 = eyeptr[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
			outstone(list[ptr2],getstring(GOPAGE,52));
		}
	return(count);
	}

static void markarmy(army_t army, char *str) {
	list_t ptr;
	int mp;
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr])
		for (mp = grpieces[list[ptr]]; mp != -1; mp = mvnext[mp])
			outstone(mvs[mp],str);
	}

/* mark neighbors of army that are cantidates for capture in a semeai */

static int markweaksemnbrs(army_t army) {
	list_t ptr, nblist;
	int nbflag;
	nblist = weaksemneighbors(army);
	nbflag = nblist != EOL;
	for (ptr = nblist; ptr != EOL; ptr = link[ptr]) {
		markarmy(grarmy[list[ptr]],"A");
		}
	killist(&nblist);
	return nbflag;
	}

#ifndef SMALLCODE

/* mark and explain group status for group g.  */

static void usergstat(sqr_t s, group_t g) {
	int numeyes;
	sqr_t s1;
	army_t army;
	list_t ptr;
	int nbflag,i;
	char buf[200], ms[361][NUMSUG+1];

	life(FALSE);
	army = G_ARMY(g);
	for ( s1 = 0; s1 < boardsquare; ++s1)
		ms[s1][0] = 0;
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr])
		markgroupstrength((group_t)list[ptr], ms);
	for (s1 = 0; s1 < boardsquare; ++s1)
		if (ms[s1][0] != 0)
			outstone(s1, ms[s1]);
 	switch (G_ALIVE(g)) {
	case VERY_ALIVE: /* also STRONG_MIAI and */
        outerr(getstring(GOPAGE,0));
	    numeyes = markeyes(g);
        if (numeyes >= 2) {
        	sprintf(buf,getstring(GOPAGE,1),getstring(GOPAGE,52));
        	outerr(buf);
        }
        else if (numeyes == 1) {
        	if (armyeyes[army] >= 16) {
        		sprintf(buf,getstring(GOPAGE,2),getstring(GOPAGE,52));
        		outerr(buf);
        	}
        	else {
        		if (outbestmove(g)) {
	        		sprintf(buf,getstring(GOPAGE,3),getstring(GOPAGE,52));
	        		outerr(buf);
	        	}
	        	else {
        			sprintf(buf,getstring(GOPAGE,4),getstring(GOPAGE,52));
        			outerr(buf);
        		}
        	}
        }
        else
        	outerr(getstring(GOPAGE,5));
		break;
	case DEAD:
		outerr(getstring(GOPAGE,6));
		break;
	case MUST_BE_DEAD:
		outerr(getstring(GOPAGE,7));
		break;
	case LOOKS_DEAD:
		outerr(getstring(GOPAGE,8));
		break;
	case LOSE_SEMEAI:
		nbflag = FALSE;
		if (armynbp[army] == EOL)getarmynbp(army);
		for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
			if (A_ALIVE(list[ptr]) == WINS_SEMEAI) {
				markarmy((army_t)list[ptr],"A");
				nbflag = TRUE;				
			}
		}
		if (nbflag)
			outerr(getstring(GOPAGE,9));
		else
			outerr(getstring(GOPAGE,10));
		break;
	case WEAK:
		outerr(getstring(GOPAGE,11));
		break;
	case WEAK_POTENTIAL:
		outerr(getstring(GOPAGE,12));
		break;
	case WEAK_SEMEAI:  
		if (markweaksemnbrs(army)) {
			outerr(getstring(GOPAGE,13));
			outerr(getstring(GOPAGE,14));
		}
		else
			outerr(getstring(GOPAGE,15));
		break;
	case WEAK_LIMP:
		for (i = 0; i < NUMRUN; ++i)
			if (armyrun[army][i] != EOL)break;
		if (i <= MAXOPENRUN)
			outerr(getstring(GOPAGE,16));
		else
			outerr(getstring(GOPAGE,17));
		break;
	case WEAK_KO:
		outerr(getstring(GOPAGE,18));
		break;
	case UNSETTLED_RUN:
		outerr(getstring(GOPAGE,19));
		break;
	case SEMEAI:
		if (markweaksemnbrs(army)) {
			outerr(getstring(GOPAGE,20));
			if (armyrn_pot[army] <= 1)
				outerr(getstring(GOPAGE,21));
			else
				outerr(getstring(GOPAGE,22));
		}
		else {
			outerr(getstring(GOPAGE,23));
			if (armyrn_pot[army] <= 1)
				outerr(getstring(GOPAGE,24));
			else
				outerr(getstring(GOPAGE,25));
		}
		break;
	case UNSETTLED_DEAD:
		outbestmove(g);
		outerr(getstring(GOPAGE,26));
		break;
	case RUNNING_FIGHT:
		if (armywk_pot[army])
			outerr(getstring(GOPAGE,27));
		else
			outerr(getstring(GOPAGE,28));
		break;
	case STRONG_KO:
		outerr(getstring(GOPAGE,29));
		break;
	case STRONG_SEMEAI:
		if (markweaksemnbrs(army)) {
			outerr(getstring(GOPAGE,30));
			outerr(getstring(GOPAGE,31));
			if (armyrn_pot[army] >= 3)
				outerr(getstring(GOPAGE,32));
		}
		else
			outerr(getstring(GOPAGE,33));
		break;
	case UNSETTLED_LIMP:
		outbestmove(g);
		outerr(getstring(GOPAGE,34));
		if (armyrunval(army) >= 2)
			outerr(getstring(GOPAGE,35));
		else
			outerr(getstring(GOPAGE,36));
		break;
	case RUN_NO_EYES:
	    numeyes = markeyes(g);
		outerr(getstring(GOPAGE,37));
		if (numeyes == 1) {
			sprintf(buf,getstring(GOPAGE,38),getstring(GOPAGE,52));
			outerr(buf);
		}
		break;
	case RUN_OR_LIVE:
		numeyes = markeyes(g);
		outbestmove(g);
		outerr(getstring(GOPAGE,39));
		if (numeyes == 1) {
			sprintf(buf,getstring(GOPAGE,40),getstring(GOPAGE,52));
			outerr(buf);
		}
		else
			outerr(getstring(GOPAGE,41));
		break;
	case WINS_SEMEAI:
		nbflag = FALSE;
		if (armynbp[army] == EOL)getarmynbp(army);
		for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
			if (A_ALIVE(list[ptr]) == LOSE_SEMEAI) {
				markarmy((army_t)list[ptr],"A");
				nbflag = TRUE;				
			}
		}
		if (nbflag)
			outerr(getstring(GOPAGE,42));
		else
			outerr(getstring(GOPAGE,43));
		break;
	case SEKI:
		if (armynbp[army] == EOL)getarmynbp(army);
		for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr])
			if (A_ALIVE(list[ptr]) == SEKI) {
				outerr(getstring(GOPAGE,44));
				markarmy((army_t)list[ptr],"A");
				return;
			}
		outerr(getstring(GOPAGE,45));
		break;
	case MIAI:
		numeyes = markeyes(g);
		outbestmove(g);
		outsecondbestmove(g);
		if (numeyes == 1) {
			sprintf(buf,getstring(GOPAGE,46),getstring(GOPAGE,52));
			outerr(buf);
		}
		else
			outerr(getstring(GOPAGE,47));
		break;
	case BARELY_ALIVE:
	    numeyes = markeyes(g);
	    if (numeyes == 1 && armyeyes[army] >= 16) {
			sprintf(buf,getstring(GOPAGE,48),getstring(GOPAGE,52));
			outerr(buf);
		}
		else if (numeyes == 0)
			outerr(getstring(GOPAGE,49));
		else
			outerr(getstring(GOPAGE,50));
		break;
	default:
		outerr(getstring(GOPAGE,50));
	}
}

#endif

#ifdef G2DEBUGOUTPUT
static char *ptps[NOPOT] = {
"Extend ",
"Vital  ",
"Threat ",
"Pthreat",
"Connect",
"Block  ",
"Dead   " };

extern char *protnam[10];

#endif

extern int resvalue[9];


#ifndef SMALLCODE

void
outgstat(sqr_t s)
{
#ifdef G2DEBUGOUTPUT
	army_t a, anew;
	list_t ptr;
	list_t tmplist;
	tree_t mt;
	int rn, max, cptr, i, j, typ, ptr2, min, type;
	sqr_t bestmove,secondmove,bestmove2,bestkillmove,bestkillmove2;
	int rest,pthreat,numrest,kopot,weakpot,best,bestmax,secondbest,numbest,maxlibs,efflibs;
	int bestval2,bestshared,mineyes,alval[2],eyes,maxeyes;
	int hemin, hemax, memin, memax;
	char *res;
	int prob;
#endif	
	group_t g;
	int tmp2;
	tmp2 = cntlist(&freelist);
	g = board[s];
	if (g == NOGROUP) {
		outerr("Not a group!\n");
		return;
	}
	if (!grlv[g]) {
		outerr("Group not active!");
		return;
	}
	fixplaylevel(MAXLEVEL);
	getobaval();
 	usergstat(s,g);
    if (!debug) {
    	return;
    }

#ifdef G2DEBUGOUTPUT
	best = bestpot(G_ARMY(g),&secondbest,&bestval2,&bestshared,&bestmove,&bestmove2,&bestkillmove,&bestkillmove2,&secondmove,&rest,&pthreat,&kopot,&weakpot, &numrest, &bestmax);
	a = G_ARMY(g);
	numbest = 0;
	numbest = 0;
	if (bestmove != PASS && bestmove != NOSQUARE &&
		(!G_THREATENED(g) || grsavemove[g] != bestmove)) {
		numbest++;
		outstone(bestmove,"1");
		}
	if (bestmove2 != PASS && bestmove2 != NOSQUARE && bestmove2 != bestmove &&
		(!G_THREATENED(g) || grsavemove[g] != bestmove2)) {
		numbest++;
		outstone(bestmove2,"1");
		}
	if (debug) {
		if (bestmove2 != PASS && bestmove2 != NOSQUARE &&
			(!G_THREATENED(g) || grsavemove[g] != bestmove2)) {
			outerr("Alternative best eye making move is at 1A.");
			outstone(bestmove2,"1A");
			}
		if (bestkillmove != PASS && bestkillmove != NOSQUARE &&
			(!G_THREATENED(g) || grsavemove[g] != bestkillmove)) {
			if (bestkillmove == bestmove) {
				outerr("Best killing move same as best living move (1K).");
				outstone(bestkillmove,"1K");
				}
			else if (bestkillmove == bestmove2) {
				outerr("Best killing move same as alternative living move (1AK).");
				outstone(bestkillmove,"1AK");
				}
			else {
		   		outerr("Best killing move is at K.");
		   		outstone(bestkillmove,"K");
				}
	   		}
		if (bestkillmove2 != PASS && bestkillmove2 != NOSQUARE &&
			(!G_THREATENED(g) || grsavemove[g] != bestkillmove2)) {
			if (bestkillmove2 == bestmove) {
				outerr("Alternative killing move same as best living move (1KA).");
				outstone(bestkillmove2,"1KA");
				}
			else if (bestkillmove2 == bestmove2) {
				outerr("Alternative killing move same as alternative living move (1AKA).");
				outstone(bestkillmove2,"1AKA");
				}
			else {
		   		outerr("Alternative best killing move is at KA.");
		   		outstone(bestkillmove2,"KA");
				}
	   		}
		outerr("8 is one eye.\n");
		sprintf(buf,"eyes %2d espc %2d-%d safe eyes %d poteyes %d bestpot %d[%d],%d[%d] to %d (%s[%s],%s[%s]), bestshared %d[%d],kill (%s[%s],%s[%s]),",armyeyes[a],
		   armyeyespace[a],armyeyespacemax[a],armysafeeyes[a],getpoteyes(a,&mineyes),best, armybestpot[a], bestval2, armybestpot2[a], armybestmax[a],
			ssqr(bestmove,buf2), ssqr(armybestmove[a],buf4),
			ssqr(bestmove2,buf6), ssqr(armybestmove2[a],buf7),
			bestshared,armybestshared[a],ssqr(bestkillmove,buf8), ssqr(armykillmove[a],buf9),
			ssqr(bestkillmove2,buf10), ssqr(armykillmove2[a],buf11));
		outerr(buf);
		sprintf(buf, "second %d[%d] (%s[%s]) rest %d[%d](%d) pthreat %d[%d] ko %d weak %d eyepotential %2d minsem %d maxsem %d.\n",
			secondbest, armysecond[a], ssqr(secondmove,buf3), ssqr(armysecondmove[a],buf5),rest, armyrest[a], 
			numrest, pthreat,armypthreat[a], kopot, weakpot, armyeyepotential[a], armyminsemeyes[a], armymaxsemeyes[a]);
		outerr(buf);
		}

	getalprob(grarmy[g], gralive[g], groldalprob[g], grsemval[g], alval);
	
	sprintf(buf, "%s(%d), gralprob (-50 to 50) %d/%d/%d (sem %d) (alval %d,%d) - was %d. seki %d\n",
		alives[gralive[g]], gralive[g], gralval[g][0], gralprob[g], gralval[g][1],
		grsemval[g], alval[0], alval[1], groldalprob[g], isseki(grarmy[g]));
	outerr(buf);
	sprintf(buf, "fight liveresult,prob %d,%d, killresult,prob %d,%d\n",
			resvalue[liveresult(g, g)],
			liveprob(g),
			resvalue[killresult(g, g)],
			killprob(g));
	outerr(buf);
	if (foughtalready((group_t)list[armygroups[grarmy[g]]], TRYTOLIVE, grcolor[g], &mt)) {
		res = treeres(mt, &prob);
		sprintf(buf, "Read to live result is %s, prob %d (treealprob %d)\n",res,prob,treealprob(mt,gralprob[g],TRUE));
		outerr(buf);
		}
	if (foughtalready((group_t)list[armygroups[grarmy[g]]], TRYTOKILL, 1-grcolor[g], &mt)) {
		res = treeres(mt, &prob);
		sprintf(buf, "Read to kill result is %s, prob %d (treealprob %d)\n",res,prob,treealprob(mt, gralprob[g],FALSE));
		outerr(buf);
		}
	sprintf(buf,"Group %3d alive %2d savealive: ",g,gralive[g]);
	outerr(buf);
	if (msptr > 0 && g != maxgr-1 && savevalid[mvcolor[msptr-1]])
		{
		sprintf(buf,"%d ",savegral[mvcolor[msptr-1]][g]);
		outerr(buf);
		}
	else
		outerr("XX ");
	if (msptr > 1 && g < maxgr-2 && savevalid[mvcolor[msptr-2]])
		{
		sprintf(buf,"%d ",savegral[mvcolor[msptr-2]][g]);
		outerr(buf);
		}
	else
		outerr("XX ");
		
	sprintf(buf,"atk %d[%d] def %d[%d] cut %d safecapt %d(%d) ahead-%d\n", 
		atk_val(a, gralval[list[armygroups[a]]][0],0), gratkval[list[armygroups[a]]],
		def_val(a,gralval[list[armygroups[a]]][1]), grdefval[list[armygroups[a]]],
		cut_stones_val(a),
		safecaptval(a,0), safecaptarmy(a,1-mvcolor[msptr-1],0), ahead); 
	outerr(buf);
	if (G_ALIVE(g) == DEAD) {
	   sprintf(buf,"eyes %d\n",
		   eyeval[eyerec[mvs[grpieces[g]]]]);
	   outerr(buf);
	   }
#endif

   if (G_THREATENED(g)) {
   	   if (grcapmove[g] != NOSQUARE && grcapmove[g] != PASS) {
		   outerr("Group can be captured by move at C.");
		   outstone(grcapmove[g],"C");
	   		}
	   if (grsavemove[g] != grcapmove[g] && grsavemove[g] != NOSQUARE &&
			grsavemove[g] != PASS && grsavemove[g] != kosquare) {
		   outerr("Group can be saved by move at S.");
		   outstone(grsavemove[g],"S");
		   }
#ifdef G2DEBUGOUTPUT
		if (debug) {
			sprintf(buf,"threat %d, grsave move is %s\n",G_THREATENED(g),
				ssqr(grsavemove[g],buf2));
			outerr(buf);
			}
	   if (debug) {
		   sprintf(buf,"eyes %d\n",
			   eyepot[eyerec[mvs[grpieces[g]]]]);
		   outerr(buf);
		   }
#endif
	   }
#ifdef G2DEBUGOUTPUT
   if (debug) {
	   for (ptr = armypot[a]; ptr != EOL; ptr = link[ptr]) {
		   sprintf(buf,"%s: %d-%d ",ptps[pots[list[ptr]].pot_type],
			   pots[list[ptr]].pot_val,
			   pots[list[ptr]].pot_max);
		   type = pots[list[ptr]].pot_type;
		   if (type == CONNECT || type == THREAT || type == POTTHREAT || type == POTDEAD) {
			   sprintf(tmp,"%d",pots[list[ptr]].pot_where);
			   strcat(buf,tmp);
			   }
		   else {
			   ssqr((sqr_t)(pots[list[ptr]].pot_where&EYEPOINTMASK),tmp);
			   strcat(buf,tmp);
			   if (pots[list[ptr]].pot_where&EYERMONLY)
				   strncat(buf, "r  ", 180);
			   if (pots[list[ptr]].pot_where&EYEADDONLY)
				   strncat(buf, "a  ", 180);
			   }
		   strcat(buf," Remove:");
		   tmplist = rmpot(a,list[ptr]);
		   for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2]) {
			   strcat(buf, " ");
			   ssqr(list[ptr2],tmp);
			   strcat(buf,tmp);
			   }
		   killist(&tmplist);
		   strcat(buf," Add:");
		   tmplist = adpot(a,list[ptr]);
		   for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2]) {
			   strcat(buf, " ");
			   ssqr(list[ptr2],tmp);
			   strcat(buf,tmp);
			   }
		   killist(&tmplist);
		   if (type == CONNECT) {
			   sprintf(tmp, " newlibs %d", newconnlibs(a, pots[list[ptr]].pot_where, &anew));
			   strcat(buf, tmp);
			}
		   outerr(buf);
		   outerr("\n");
		   }
	   
	   sprintf(buf,"run %d weak %d\n",
		   armyrn_pot[a],armywk_pot[a]);
	   outerr(buf);
	   sprintf(buf, "life/death reading vals: black %d, white %d\n", 
			fightatkdefval(a, g, 0),
			fightatkdefval(a, g, 0));
	   outerr(buf);
       efflibs = getefflibs(g,4,NOGROUP,&maxlibs);
	   sprintf(buf,"Tactical effective liberties %d-%d.\n",efflibs,maxlibs);
	   outerr(buf);
	   semeailibs(a,&min,&max,&typ,&hemin, &hemax, &memin, &memax);
	   sprintf(buf,"Semeai liberties min:%d typ:%d max:%d. %d-%d, %d-%d.\n",min,typ,max, hemin, hemax, memin, memax);
	   outerr(buf);
	   sprintf(buf,"armylibs %d vital pts: ",armylibs[a]);
	   for (ptr = armyvitalpoints[a]; ptr != EOL; ptr = link[ptr]) {
		   ssqr((sqr_t)(list[ptr]&EYEPOINTMASK),tmp);
		   strncat(buf,tmp,180);
		   }
	   outerr(buf);
	   sprintf(buf,"size %d army %d deadarmy %d.",grsize[g],a,grdeadarmy[g]); 
	   outerr(buf);
	   if (armydeadgroups[a] != EOL) {
		   sprintf(buf,"armydeadgroups: ");
		   for (ptr = armydeadgroups[a]; ptr != EOL; ptr = link[ptr]) {
			   sprintf(tmp,"%3d ",list[ptr]);
			   strncat(buf,tmp,180);
			   }
		   outerr(buf);
		   }
	   i = grnbp[g];
	   if (i == EOL) {
		   outerr("\nNo neighbors.");
		   }
	   else{
		   sprintf(buf,"\nneighbors: ");
		   while(i != EOL) {
			   sprintf(tmp,"%d ",list[i]); 
			   strncat(buf,tmp,180);
			   i = link[i]; 
			   }
		   outerr(buf);
		   } 
	   i = armynbp[a];
	   if (i == EOL) {
		   outerr("\nNo army neighbors.");
		   }
	   else{
		   sprintf(buf,"\narmy nbrs:\n");
		   while(i != EOL) {
			   sprintf(tmp,"%d ",list[i]); 
			   strncat(buf,tmp,180);
			   eyes = eyesifcapture((army_t)list[i], a, &maxeyes);
			   sprintf(tmp, "%d-%d eyes\n", eyes, maxeyes);
			   strncat(buf, tmp, 180);
			   i = link[i]; 
			   }
		   outerr(buf);
		   } 
	   if (armyeyerecs[a] == EOL)
		   outerr("\nGroup has no eye records.\n");
	   else {	   
		   sprintf(buf,"\neyerec eyetype  pot val min points\n");
		   outerr(buf);
		   }
	   for (ptr = armyeyerecs[a]; ptr != EOL; ptr = link[ptr]) {
		   rn = list[ptr];
		   sprintf(buf,"%3d %s %3d %3d-%d %3d ",rn,
			   etypes[eyetype[rn]],eyepot[rn],eyeval[rn],eyevalmax[rn],eyemin[rn]);
		   outerr(buf);
		   for (ptr2 = eyeptr[rn]; ptr2 != EOL; ptr2 = link[ptr2]) {
			   outerr(ssqr(list[ptr2],tmp));
			   }
		   outerr("\n");
		   }
#ifdef NEVER
	   for (gptr = armygroups[a]; gptr != EOL; gptr= link[gptr])
		   for (ptr = grnbp[list[gptr]]; ptr != EOL; ptr = link[ptr]) {
			   if (!G_THREATENED(list[ptr]))continue;
			   rn = eyerec[mvs[grpieces[list[ptr]]]];
			   sprintf(buf,"%3d    %s %3d %3d %3d ",rn,etypes[eyetype[rn]],eyepot[rn],eyeval[rn],eyemin[rn]);
			   outerr(buf);
			   for (ptr2 = eyeptr[rn]; ptr2 != EOL; ptr2 = link[ptr2]) {
				   outerr(ssqr(list[ptr2],tmp));
				   }
			   outerr("\n");
			   }
#endif
	   if (armyvitalpoints[a] == EOL)
		   outerr("Group has no vital points.");
	   else {
		   sprintf(buf,"Vital points: ");
		   for (ptr = armyvitalpoints[a]; ptr != EOL; ptr = link[ptr]) {
			   ssqr((sqr_t)(list[ptr]&EYEPOINTMASK),tmp);
			   strncat(buf,tmp,180);
			   if (list[ptr]&EYERMONLY)
				   strncat(buf, "r  ", 180);
			   if (list[ptr]&EYEADDONLY)
				   strncat(buf, "a  ", 180);
			   }
		   outerr(buf);
		   }
		if (grcnp[g] == EOL)
		   outerr("\nGroup has no connections.\n");
		else{
		   sprintf(buf,"\nconn. # prot type where\n");
		   outerr(buf);
		   cptr = grcnp[g]; 
		   while(cptr != EOL) {
			   i = list[cptr]; 
			   if (cngr1[i]  == g) 
				   sprintf(buf,"%3d/%d     %d %s  ", cngr2[i], i, conn_val(grarmy[cngr1[i]], grarmy[cngr2[i]]), 
					protnam[cnprot[i]]); 
			   if (cngr2[i] == g) 
				   sprintf(buf,"%3d/%d     %d %s  ", cngr1[i], i, conn_val(grarmy[cngr1[i]], grarmy[cngr2[i]]),
				    protnam[cnprot[i]]); 
			   if (cnprot[i] == SHARED_CONNECT) {
				   sprintf(tmp, "%d  ", cnshcent[i]);
				   strncat(buf, tmp, 180);
					}
			   strncat(buf,cntpstr[cntype[i]],180);
			   outerr(buf);
			   sprintf(buf,"\npath:               "); 
			   for (j = cnpathptr[i]; j != EOL; j = link[j]) {
				   ssqr(list[j],tmp);
				   strncat(buf,tmp,180);
				   }
			   outerr(buf);
			   sprintf(buf,"\n1:           %2d    ",cncnum[i]); 
			   for (j = cnptr[i]; j != EOL; j = link[j]) {
				   ssqr(list[j],tmp);
				   strncat(buf,tmp,180);
				   }
			   outerr(buf);
			   sprintf(buf,"\n2:           %2d    ",cnlknum[i]); 
			   for (j = cnlkptr[i]; j != EOL; j = link[j]) {
				   ssqr(list[j],tmp);
				   strncat(buf,tmp,180);
				   }
			   outerr(buf);
			   sprintf(buf,"\n3:           %2d    ",cnllnum[i]); 
			   for (j = cnllptr[i]; j != EOL; j = link[j]) {
				   ssqr(list[j],tmp);
				   strncat(buf,tmp,180);
				   }
			   outerr(buf);
			   sprintf(buf,"\n4:           %2d    ",cnollnum[i]); 
			   for (j = cnollptr[i]; j != EOL; j = link[j]) {
				   ssqr(list[j],tmp);
				   strncat(buf,tmp,180);
				   }
			   outerr(buf);
			   sprintf(buf,"\nD:           %2d    ",cnddnum[i]); 
			   for (j = cnddptr[i]; j != EOL; j = link[j]) {
				   ssqr(list[j],tmp);
				   strncat(buf,tmp,180);
				   }
			   outerr(buf);
			   outerr("\n");
			   cptr = link[cptr];
			   } 
		   }
	   outerr("runs: \n");
	   for (i = 0; i < NUMRUN; ++i)
		   for (ptr = armyrun[a][i]; ptr != EOL; ptr = link[ptr]) {
			   sprintf(buf," %d:",i);
			   ssqr(list[ptr],tmp);
			   strncat(buf,tmp,180);
			   strncat(buf, "@ ", 180);
			   tmplist = runpoints(a, list[ptr], i);
			   for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2]) {
					ssqr(list[ptr2], tmp);
					strncat(buf, tmp, 180);
				}
				strncat(buf, "\n", 180);
			   outerr(buf);
			   killist(&tmplist);
			   }
	   sprintf(buf,"conn run: %d\n", armycnrn_pot[a]);
	   outerr(buf);
	   }
#endif
   }
#endif /* smallcode */

