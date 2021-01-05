/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2hd.h"
#include "g2rldef.h"
#include "g2tree.h"
#include "g2fcache.h"
#if defined(TEST) || defined(G2DEBUGOUTPUT)
#include <stdio.h>
#endif


struct fightstruct {  /* record fights that have been read and no answer found */
	group_t group;  /* groups involved with fight */
	group_t nbgroup;  /* neighbor group was fighting with */
	tree_t moves;    /* move tree for fight */
	int nblibhash;  /* liberties of all neighbors hashed together */
	int fighthash;	/* hash of this gorup's properties */
	int use;       /* last use for LRU algorithm */
	unsigned int msptr;		/* msptr value when reading happened */
	char tm;     /* side to move */
	};

# define NUMFIGHTS 30	

static struct fightstruct fights[NUMFIGHTS];  /* record results of fights */

int countfights(void) {
	int cnt = 0, i;
	for (i = 0; i < NUMFIGHTS; ++i)
		cnt += counttree(fights[i].moves);
	return cnt;
	}
	
/* clear out the fights */

static int fightvalid = FALSE;

void clearfights() {
	int i;
	for (i = 0; i < NUMFIGHTS; ++i) {
		fights[i].group = fights[i].nbgroup = NOGROUP;
		if (fightvalid)
			freetree(fights[i].moves);
		fights[i].moves = NONE;
		fights[i].use = 0;
		}
	fightvalid = TRUE;
	}

void clearafight(group_t g)
{
	int i;
	for (i = 0; i < NUMFIGHTS; ++i) {
		if (fights[i].group != g)
			continue;
		fights[i].group = fights[i].nbgroup = NOGROUP;
		if (fightvalid)
			freetree(fights[i].moves);
		fights[i].moves = NONE;
		fights[i].use = 0;
		}
}

/* find the biggest group in the list of groups.  if a tie, go
 * for the group with the most liberties.  This is the group of
 * the set that we should make live or die
 */

group_t biggestarmygroup(list_t grouplist) {
	list_t ptr;
	group_t big = NOGROUP, g;
	int size = 0;
	int libs = 0;
	for (ptr = grouplist; ptr != EOL; ptr = link[ptr]) {
		g = board[mvs[grpieces[list[ptr]]]];  /* in case groups have merged */
		if (!grlv[g])
			continue;
		if (grsize[g] > size ||
			grsize[g] == size && grlibs[g] > libs) {
			size = grsize[g];
			big = g;
			libs = grlibs[g];
			}
		}
	return big;  
	}


/* hash all of the values that might invalidate a fight for group g 
 * for the group:
 * armylibs (1 to 30)
 * armyrnval (0 to 20)
 * armyeyespace (0 to 24)
 * aliveness (1 to 25) (ok, since fight result changes alprob, but not classification

 */

extern int zobhash[361][2];

static int fighthash(group_t g) {
	int hash = 0;
	int army = grarmy[g];
	hash ^= zobhash[G_ALIVE(g)][0];
	hash ^= zobhash[armysize[army]+30][0];
	hash ^= zobhash[armyeyespace[army]+60][0];
	hash ^= zobhash[armylibs[army]+90][0];
	hash ^= zobhash[armysecond[army]+120][0];
/*	hash ^= zobhash[armysecondmove[army]][0]; */

	hash ^= zobhash[A_NUMLIBS(army)][1];
	hash ^= zobhash[armyrn_pot[army]+30][1];
	hash ^= zobhash[armycnrn_pot[army]+60][1];
	hash ^= zobhash[armybestpot[army]+90][1];
/*	hash ^= zobhash[armybestmove[army]][1]; */
	return hash;
}

/* return a hash of all interesting values that could change the life of neighboring armies of g 
 */

static int hashlibs(group_t g) {
	army_t army;
	int hash = 0, ptr;
	
	/*hash = fighthash(g); */
	army = grarmy[g];
	if (armynbp[army] == EOL)getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		if (/*A_ALIVE(list[ptr]) >= MIAI && */ armylibs[list[ptr]] < 20) {
			hash ^= zobhash[armylibs[list[ptr]]+150][0];  /* any changes in nbr groups */
			hash ^= zobhash[armysize[list[ptr]]+150][1];
			hash ^= zobhash[A_ALIVE(list[ptr])+200][0]; /* can't depend on anything from life(), since fight result changes life */
			if (A_ALIVE(list[ptr]) > MIAI)
				hash ^= zobhash[armyrn_pot[list[ptr]]+200][1];
			}
		}
	return(hash);
	}


/* Return TRUE if already read
 * this battle.
 * and the result of last reading in *movetree
 */

int foughtalready(group_t g, group_t nbgroup, int tm, tree_t *movetree)
{
	int i;
	tree_t t;
	g = biggestarmygroup(armygroups[grarmy[g]]);
	for (i = 0; i < NUMFIGHTS; ++i) {
		if (fights[i].group != g || fights[i].nbgroup != nbgroup)
			continue;
		if (tm == fights[i].tm &&
			fighthash(g) == fights[i].fighthash &&
			hashlibs(g) == fights[i].nblibhash) {
			if (fights[i].moves != NONE) {
				for (t = tr[fights[i].moves].child; t != NONE; t = tr[t].sib) {
					if (board[tr[t].s] != NOGROUP) {	/* can't be a valid match if stone on move */
						return FALSE;
					}
				}
			}
			*movetree = fights[i].moves;
//			fights[i].use = msptr;        
			return TRUE;
		}
	}
	return FALSE;
}


	
/* mark a fight as read
 * fight between goodgroup and badgroup.  
 * if not a semeai, set badgroup to TRYTOKILL or TRYTOLIVE
 * tm to move first.
 * result is TRUE for a win. 
 */
	
void markfight(group_t goodgroup, group_t badgroup, int tm, tree_t t)
{
	int i,j,use = 1000;
	if (t == NONE)
		return;		/* don't record if there is no result */
	for (i = 0; i < NUMFIGHTS; ++i)
		if (fights[i].group == goodgroup && fights[i].nbgroup == badgroup)break;	/* reuse entry */
	if (i == NUMFIGHTS) {
		for (j = 0; j < NUMFIGHTS; ++j) {
			if (fights[j].moves == NONE) {  /* free entry */
				i = j;
				break;
			}
			if (fights[j].use < use) {
				use = fights[j].use;
				i = j;	/* find oldest fight */
			}
		}
	}
	if (fights[i].moves != t)	/* don't free it if it is just an update */
		freetree(fights[i].moves);
	fights[i].group = goodgroup;
	fights[i].nbgroup = badgroup;
	fights[i].nblibhash = hashlibs(goodgroup);
	fights[i].fighthash = fighthash(goodgroup);
	fights[i].tm = tm;
	fights[i].moves = t;
	fights[i].use = msptr;
	fights[i].msptr = msptr;
}

/* return list of moves that work for color tm against army, with best one first */

list_t fightmoves(army_t army, int tm) {
	list_t movelist = EOL;
	group_t g = biggestarmygroup(armygroups[army]);
	tree_t movetree, t;
	int res;
	int bval = -1000;
	sqr_t best;

	res = foughtalready(g, (group_t)(tm==grcolor[g]?TRYTOLIVE:TRYTOKILL), tm, &movetree);
	if (!res) {
		return EOL;
	}

	for (t = tr[movetree].child; t != NONE; t = tr[t].sib) {
		if (tr[t].s == NOSQUARE || tr[t].s == PASS) {
			continue;
		}
		if (TR_RESULT(t) == V_WIN) {
			addlist(tr[t].s, &movelist);
			if (tr[t].val > bval) {
				bval = tr[t].val;
				best = tr[t].s;
			}
		}
	}
	if (movelist != EOL) {
		dellist(best, &movelist);
		adflist(best, &movelist);
		return movelist;
	}
	for (t = tr[movetree].child; t != NONE; t = tr[t].sib) {
		if (tr[t].s == NOSQUARE || tr[t].s == PASS) {
			continue;
		}
		if (TR_RESULT(t) == V_WINLIKELY) {
			addlist(tr[t].s, &movelist);
			if (tr[t].val > bval) {
				bval = tr[t].val;
				best = tr[t].s;
			}
		}
	}
	if (movelist != EOL) {
		dellist(best, &movelist);
		adflist(best, &movelist);
	}
	return movelist;
}


/* check result of fight after move(s) are made to see if group
 * should be counted as alive or dead
 */
		
int checkliferesult(group_t g, int live) {
	int i,found = FALSE, lv;
	unsigned int mp;
	tree_t t, tmp;
	sqr_t s;
    g = biggestarmygroup(armygroups[grarmy[g]]);
	for (i = 0; i < NUMFIGHTS; ++i) {
		lv = FALSE;
		if (fights[i].group == NOGROUP || S_ARMY(mvs[grpieces[fights[i].group]]) != grarmy[g])
			continue;  /* check armies since may be looking at different group now */
		               /* check from board, since move made have eliminated g */
		if (fights[i].msptr >= msptr)
			continue;
		t = fights[i].moves;
		for (mp = fights[i].msptr; mp < msptr; ++mp) {
			s = mvs[mp];
			found = FALSE;
			for (tmp = tr[t].child; tmp != NONE; tmp = tr[tmp].sib)
				if (s == tr[tmp].s) {
					t = tmp;
					found = TRUE;
					break;
					}
			if (!found)break;
			lv = !lv;
			}
		if (found) {  /* found a fight and read down this line */
			if (lv && live && fights[i].nbgroup == TRYTOLIVE ||
				!lv && !live && fights[i].nbgroup == TRYTOKILL)
				return TR_RESULT(t);
			}
		}
	return V_UNKNOWNRES;
	}

#ifdef G2DEBUGOUTPUT

void dumpfights() {
	int i;
	group_t g;
	char buf[120], buf2[120];
	for (i = 0; i < NUMFIGHTS; ++i) {
		g = fights[i].group;
		if (g == NOGROUP)
			continue;
		if (hashlibs(g) == fights[i].nblibhash &&
			fighthash(g) == fights[i].fighthash &&
			fights[i].moves != NONE) {
			if (fights[i].nbgroup < NUMGROUPS)
				sprintf(buf,
					"fight %d, group %d, nbr %d, tm %d, %s\n",
					i, g, fights[i].nbgroup, fights[i].tm,
					treesummary(fights[i].moves, buf2));
			else
				sprintf(buf,
					"fight %d, group %d, nbr %s, tm %d, %s\n",
					i, g, 
					fights[i].nbgroup==TRYTOKILL?"Trytokill":
						(fights[i].nbgroup==TRYTOLIVE?"Trytolive":"???"), 
					fights[i].tm,
					treesummary(fights[i].moves, buf2));
			outerr(buf);
		}
	}
}

#endif
