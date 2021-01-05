/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* full board search for Many Faces of Go
 * it is a negamax lokahead, with no fixed depth
 * selective search is used with several move generators
 * generators include:
 *
 *  tenuki - just take the sente value
 *  local move
 *  joseki move
 *  pattern move
 *  big pass move
 *  save group, full board
 *  kill group full board
 *  life and death search move
 */
#include "g2hd.h"
#include "g2jos.h"
#include "g2proto.h"
#include "g2rldef.h"
#include "g2pat.h"
#include "learn.h" 
#include "g2tt.h"
#include "g2fcache.h"
#include <stdlib.h>
#include <stdio.h>

#ifdef WIN32
#include <windows.h>
#else
static int GetTickCount(void)
{
	return 1000 * time(NULL);
}
#endif


/* maximum search depth */
#define MAXSDEPTH 50
#define MAXSMOVES 10000

extern int obaval,savealive[NUMALIVE];
extern char maxscoredepth[21],maxscorebrdepth[21];

extern int maxpatvariations[21],urgalive[NUMALIVE],pmvalue[16];
extern struct newpattern *pt;
extern struct pmove *pm;
extern struct newpatstruct newpat[NUMNEWPATS];
extern int numlifecalls;
extern int semalive[];

static int firstsmove[MAXSDEPTH];
static int nextsmove[MAXSDEPTH];
static int firstpatmove[MAXSDEPTH];
static int nextpatmove[MAXSDEPTH];

static int timeout;		/* search time over - just return */

int logsearch = FALSE;		/* log the search to a file */
FILE *logfile;
#ifdef G2DEBUGOUTPUT
static int indent;
#define INDENT for (indent = 1; indent < ply; ++indent)fprintf(logfile, "  ");
static char *cnames[3] = { "Black", "White", "neither" };
#endif

/* types */
#define NUMTYPES 10
#define SJOSEKI 0
#define SOBVIOUS 1
/* save and kill from move generators, needs to be verified */
#define SSAVE 2
#define SKILL 3
#define SBIG 4
#define SPATTERN 5
#define SMOVE 6
/* save and kill from life reading, so should be trusted as accurate */
#define SFSAVE 7
#define SFKILL 8
#define SFUS 9

char typemark[] = "JOskBPMSKF";

/* stack to hold moves generated, by ply */
static struct newmoves {
	sqr_t s;
	sqr_t where;	/* for save or kill, the key stone in the group */
	int type;		/* which move generators made this move */
	int val;		/* for sorting, higher values first, in estimated points * 50 */
	int oldlife;	/* old aliveness of that group */
	char good;		/* the save or kill move worked */
} gnmv[MAXSMOVES];

static void initstacks(void)
{
	firstsmove[0] = nextsmove[0] = 0;
	firstpatmove[0] = nextpatmove[0] = 0;
}

static void addmove(sqr_t s, int ply, int type, sqr_t where, int oldlife, int val)
{
	int i;
	if (ply >= MAXSDEPTH)
		return;
	if (nextsmove[ply] >= MAXSMOVES) {
#ifdef G2DEBUGOUTPUT
		outerror("Out of space for moves in search");
#endif
		return;
	}
	if (s != PASS && (s < 0 || s > boardsquare || board[s] != NOGROUP || s == kosquare)) {
#ifdef G2DEBUGOUTPUT
		outerror("Search gen bad move\n");
#endif
		return;
	}
	for (i = firstsmove[ply]; i < nextsmove[ply]; ++i) {
		if (gnmv[i].s == s) {  	/* no duplicates, but combine values - keep maximum */
			if (val > gnmv[i].val) {
				gnmv[i].val = val;
				if (gnmv[i].type == SSAVE || gnmv[i].type == SKILL || gnmv[i].type == SFSAVE || gnmv[i].type == SFKILL) {
					gnmv[i].where = where;	/* get new location for higher value */
					gnmv[i].type = type;	/* replace type */
				}
			}
			return;
		}
	}
	gnmv[nextsmove[ply]].type = type;
	gnmv[nextsmove[ply]].s = s;
	gnmv[nextsmove[ply]].val = val;
	gnmv[nextsmove[ply]].where = where;
	gnmv[nextsmove[ply]].oldlife = oldlife;
	gnmv[nextsmove[ply]].good = FALSE;
	nextsmove[ply]++;
}

static int compare(const void* elem1, const void* elem2) {
	int v1 = ((struct newmoves *)elem1)->val;
	int v2 = ((struct newmoves *)elem2)->val;
	return v1 == v2?0:(v1 > v2?-1:1);
}

extern int cfac[3],maxjosvariations[21],maxjosbranches[21];
extern char josx[4], josy[4];

/* Here is the code for all of the full board move generators.
 */

/* values for sorting moves, approximately points times 50 */

#define URGJOSVAL 1000
#define JOSVAL 500

/* generate joseki moves in a corner 
 */
static int genjoscorner(int color, int ply, int corner, int tp) 
{
	int lastcolor;
	unsigned int ptr, xyptr;
	int x, y, tmp, type;
	int numbr = 0;  /* number of moves generated */
	int urg = FALSE;
	int order = 0;

	if (jptr2[corner] == 0)
		lastcolor = color;
	else
		lastcolor = getlastcolor(corner);
	if (lastcolor == NOCOLOR)
		lastcolor = color;
	for (ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)) {

		xyptr = ptr;
		getxyjlib(xyptr,&x,&y);
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
		x--;y--;
		type = getflag(xyptr) & 0xf;
		if (tp == NORM) {
			if (type != NORM && type != URGT)
				continue;
			if (type == NORM && urg)
				continue;
			if (type == URGT)
				urg = TRUE;
		} else if (type == TRIK || type == IGNR || type == NORM || type == URGT) {
			continue;
		} 
		if (jreflect[corner] <= 1) {
			tmp = x;
			x = y;
			y = tmp;
		}
		if (x < josx[corner]-1 && y < josy[corner]-1) {
			if (corner > 1)
				y = boardsize-1-y;
			if (corner%2 == 1)
				x = boardsize-1-x;
			addmove((sqr_t)(x + boardsize * y), ply, SJOSEKI, 0, 0, order + (type == URGT ? URGJOSVAL : JOSVAL));
			order--;
			numbr++;
		}
	}
	return numbr;
}

/* generate Q search joseki moves
 * gen one NORM/URG move for corner with last move joseki only
 * return TRUE if it generated a NORM or URG joseki move
 */

static int genjosekimoves(int color, float depth, int ply)
{
	int corner;
	for (corner = 0; corner < 4; ++corner) {
		if (jflag[corner] != 1)	/* must be in valid joseki */
			continue;
		if (msptr > 0 && incn(mvs[msptr-1], corner, -2))
			break;
	}
	if (corner == 4) {
		return FALSE;
	}
	return genjoscorner(color, ply, corner, NORM) != 0;
}

/* return TRUE if pattern moves are found
 * color is side to move
 * original mover can tenuki if any pattern has no moves in next ply
 * his opponent can tenuki at any time at the cost of the highest bonus value for the opponent
 * this is if any pattern has PM_FINAL, or there are no 
 * pattern moves found
 * finish any unfinished pattern sequences
 */
static int genpatternmoves(int mvcolor, int ply)
{
	sqr_t s, sqr;
	list_t ptr;
	int p, mp, val, color, bestval = 0, movecount = 0;

	return FALSE;
	for (s = 0; s < boardsquare; ++s) {
		for (ptr = newpatbrd[s]; ptr != EOL; ptr = link[ptr]) {
			p = list[ptr];
			if (pt[newpat[p].num].moves != newpat[p].move) {
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
						continue;
					if ((PM_VAL(mp)) >= 10) {	/* pattern says is urgent */
						val += 300;
					}
					if ((pt[newpat[p].num].wheretype & WHEREMASK) == P_CORNER)
						val += 100;	/* prefer to look at corner patterns first */
					else if ((pt[newpat[p].num].wheretype & WHEREMASK) == P_EDGE)
						val += 50;	/* then edge patterns */
					sqr = patgetsqr(newpat[p].sqr, newpat[p].orient, pm[mp].xy);
					if (board[sqr] != NOGROUP) {
						continue;
					}
					if (PM_VALUE(mp) > bestval)
						bestval = PM_VALUE(mp);
					addmove(sqr, ply, SPATTERN, 0, 0, val);
					movecount++;
				}	
			}
		}
	}
	return bestval > 4;
}

/* return the biggest group to attack or defend from
 * the rule matched reasons to play the original move.
 */
	
static group_t biggestrule(int move)
{
	list_t ptr;
	group_t g, bgroup = NOGROUP;
	int atk,bval=0, val;
#ifdef G2DEBUGOUTPUT
	char buf[100];
#endif
	if (mvs[move] == PASS)
		return NOGROUP;
	for (ptr = stratreasons[mvs[move]]; ptr != EOL; ptr = link[ptr]) {
		atk = rules[strat[list[ptr]].reason].attack;
		if ((atk&ATTACK) ||
			 (atk&DEFEND)) {
			 g = board[strat[list[ptr]].param];
			 if (g == NOGROUP)
				 continue;  /* already captured */
#ifdef G2DEBUGOUTPUT			 
			 if (!grlv[g]) {
			 	sprintf(buf,"bad group in biggestrule. g %d, rule %d\n",g,strat[list[ptr]].reason);
			 	outerr(buf);
		 	}
#endif			 
			 if (grlv[g] && (savealive[gralive[g]] ||
				grthreatened[g] && gralive[g] < WEAK)) {
				val = def_val(grarmy[g],gralval[g][1]);
				if (val > bval) {
					bval = val;
					bgroup = g;
				}
			}
		 }
	}
	return(bgroup);
}

/* find biggest armies to save or kill at search depth.  s is last move
 * move is msptr index of move being evaluated
 * first look at groups that are involved in current fight - from reasons
 * then just find biggest unsettled group on the board.
 */
	
static list_t biggestunsettled(sqr_t s, int move, int *mval, int havepattern)
{
	group_t g;
	int  max1 = SMALLNUM, max2 = SMALLNUM, val, islocal;
	army_t barmy1 = NOARMY, barmy2 = NOARMY, army, rulearmy = NOARMY, localarmy = NOARMY;
	list_t tmplist = EOL;
	list_t retlist = EOL;
	*mval = 0;
	g = biggestrule(move);
	if (g != NOGROUP && !havepattern) {
		adflist(grarmy[g], &retlist);
		rulearmy = grarmy[g];
	}
	localarmy = S_ARMY(s);
	if (localarmy != NOARMY && armynbp[localarmy] == EOL)
		getarmynbp(localarmy);

	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])
			continue;
		army = grarmy[g];
		if (army == rulearmy)
			continue;
		if (G_ALIVE(g) >= WEAK)
			continue;
		if (G_ALIVE(g) <= ALIVE)
			continue;
		islocal = army == localarmy || inlist(g, &grnbp[board[s]]);	/* 9/05 changed from neighbor army to neighbor group */
		if (havepattern && islocal)
			continue;	/* 7/03 if pattern, let it handle local isues */

		if (inlist(army,&tmplist))
			continue;
		addlist(army,&tmplist);

		val = def_val(army,gralval[list[armygroups[army]]][1]);
		if (val < obaval)
			continue;
		if (val > max1) {
			max2 = max1;
			barmy2 = barmy1;
			max1 = val;
			barmy1 = army;
		}
		else if (val > max2) {
			max2 = val;
			barmy2 = army;
		}
	}
	
#ifdef CHECK
	if (barmy1 != NOARMY && armygroups[barmy1] == EOL || barmy1 > NUMARMIES)
		outerr("bad barmy in biggesttosave\n");
#endif
	killist(&tmplist);
	*mval = max1;
	aeflist(barmy1, &retlist);
	if (max2 > max1/3)
		aeflist(barmy2, &retlist);
	return retlist;
}


/* find moves to save or kill groups 
 * color to move
 * ply moves
 * mptr is index of original move
 * havepattern if pattern moves were generated - don't include local save/kill moves - let pattern handle it
 */

static void gensavekillmoves(int color, int ply, int mptr, int havepattern)
{
	list_t moves = EOL, ptr, armylist, aptr;
	army_t army;
	int val, val2;
	int count = 0;
	sqr_t s = mvs[msptr-1];	/* last move */

	if (s != PASS && armynbp[S_ARMY(s)] == EOL)
		getarmynbp(S_ARMY(s));
	armylist = biggestunsettled(mvs[msptr-1], mptr, &val, havepattern);


	for (aptr = armylist; aptr != EOL; aptr = link[aptr]) {
		army = (army_t)list[aptr];
		if (army == NOARMY)
			continue;
#ifdef NEWSEARCH
		if (A_ALIVE(army) < MIAI)
			continue;
#endif
		if (color == A_COLOR(army)) {
			val = def_val((army_t)army, gralval[list[armygroups[army]]][1]);
		}
		else {
			val = atk_val((army_t)army, gralval[list[armygroups[army]]][0], 0);
		}
		/* any moves from fight reading? */
		moves = fightmoves(army, color);
		if (moves != EOL) {	   /* just the first/best one, since it will work */
			addmove(MV_MV(list[moves]), ply, 
				color == A_COLOR(army) ? SFSAVE : SFKILL, mvs[grpieces[list[armygroups[army]]]], A_ALIVE(army), val);
		}
		else {
			if (color == A_COLOR(army)) {
				moves = lifemoves((group_t)list[armygroups[army]]);
			}
			else {
				moves = genobviouskill(army);
			}
/*			savemoves = genobvioussave(savearmy); */
			count = 0;
			for (ptr = moves; ptr != EOL && count < 3; ptr = link[ptr]) { 
				if (MV_MV(list[ptr]) == PASS)
					continue;	/* no need for pass save moves since tenuki is always possible */
				if (MV_MV(list[ptr]) == kosquare) {
					/* TODO: generate a ko threat */
					continue;
				}
				val2 = val * (100 - MV_PROB(list[ptr]) * 2) / 100;	/* adjust for prob of working */
				addmove(MV_MV(list[ptr]), ply, 
					color == A_COLOR(army) ? SSAVE : SKILL, mvs[grpieces[list[armygroups[army]]]], A_ALIVE(army), val2);
				val = (val * 3) / 4;	/* adjust down for other moves */
				count++;
			}
		}
		killist(&moves);
	}
	killist(&armylist);
}

static void genobvcapture(army_t army, int ply)
{
	list_t ptr;
	list_t moves = EOL;
	int val;
	int val2;
	int count = 0;
	if (A_ALIVE(army) <= ALIVE && A_ALIVE(army) != MIAI)
		return;
	if (A_ALIVE(army) >= WEAK)
		return;
	val = atk_val((army_t)army, gralval[list[armygroups[army]]][0], 0);
	moves = killmoves((group_t)list[armygroups[army]]);
	for (ptr = moves; ptr != EOL && count < 5; ptr = link[ptr]) { 
		if (MV_MV(list[ptr]) == PASS)
			continue;	/* no need for pass save moves since tenuki is always possible */
		if (MV_MV(list[ptr]) == kosquare)
			continue;
		val2 = val * (100 - MV_PROB(list[ptr]) * 2) / 100;	/* adjust for prob of working */
		addmove(MV_MV(list[ptr]), ply, SKILL, mvs[grpieces[list[armygroups[army]]]], A_ALIVE(army), val2);
		val = (val * 2) / 3;	/* adjust down for other moves */
		count++;
	}
	killist(&moves);
}

static void genobvsave(army_t army, int ply)
{
	list_t ptr;
	list_t moves = EOL;
	int val;
	int val2;
	int count = 0;
	if (A_ALIVE(army) <= ALIVE)
		return;
	if (A_ALIVE(army) >= WEAK)
		return;
	val = def_val((army_t)army, gralval[list[armygroups[army]]][1]);
	moves = lifemoves((group_t)list[armygroups[army]]);
	for (ptr = moves; ptr != EOL && count < 5; ptr = link[ptr]) { 
		if (MV_MV(list[ptr]) == PASS)
			continue;	/* no need for pass save moves since tenuki is always possible */
		if (MV_MV(list[ptr]) == kosquare)
			continue;
		val2 = val * (100 - MV_PROB(list[ptr]) * 2) / 100;	/* adjust for prob of working */
		addmove(MV_MV(list[ptr]), ply, SSAVE, mvs[grpieces[list[armygroups[army]]]], A_ALIVE(army), val2);
		val = (val * 2) / 3;	/* adjust down for other moves */
		count++;
	}
	killist(&moves);
}

/* generate moves to capture or save threatened groups adjacent to
 * earlier moves that are in this search, including the move before the search.  
 * tm is color to move now
 * move is index of last move.
 */

static void genadjacent(int color, int move, int ply)
{
	int ptr;
	sqr_t s;
	army_t army;
	int p = ply;
	
	if (move <= 0)
		return;
	while(p >= 0 && move >= 0) {
		s = mvs[move];
		p--;
		move--;
		if (s == PASS || s == NOSQUARE)
			continue;
		if (board[s] == NOGROUP)
			continue;
		army = S_ARMY(s);
		if (armynbp[army] == EOL)
			getarmynbp(army);
		for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
			if (color == A_COLOR(list[ptr]))
				genobvsave(list[ptr], ply);
			else
				genobvcapture(list[ptr], ply);
		}
	}
}

/* generate moves to continue goals specified by goals (move index) */
/* move is index of last move */

static void genobvgoals(int move, int goals, int ply)
{
	list_t ptr;
	int rule;
	group_t g;
	if (mvcolor[move] == mvcolor[goals])
		return;
	for (ptr = stratreasons[mvs[goals]]; ptr != EOL; ptr = link[ptr]) {
		rule = strat[list[ptr]].reason;
		if (rules[rule].attack & PATTERNMV)
			continue;  /* patterns handled elsewhere */
		if (rules[rule].attack & ATTACK ||
		   rules[rule].attack & ATKVAL1 ||
		   rules[rule].attack & ATKVAL2) {
		   g = board[strat[list[ptr]].param];
		   	if (g == NOGROUP)
				continue;
			if (grthreatened[g] && grcapmove[g] != NOSQUARE && grcapmove[g] != kosquare) {  /* TODO: generate ko threats */
		   		addmove(grcapmove[g], ply, SOBVIOUS, 0, 0, 100);
			}
	   	}
    }
}

static void genobvlocalmoves(int color, int ply, float depth, int mptr, int havepattern)
{
	list_t ptr, tmplist = EOL;
	int move = msptr-1;
	sqr_t s = mvs[move];
	army_t army;
	int val, one_nbr;
	group_t g;

	if (s != PASS && board[s] != NOGROUP) {
		army = S_ARMY(s);
		genobvcapture(army, ply);								/* capture group at s */
		if (armynbp[army] != EOL)
			getarmynbp(army);
		for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr])
			genobvsave(list[ptr], ply);							/* save groups next to s */
	}
	genadjacent(color, move, ply);											/* adjacent captures and saves from earlier moves */
	/* ignore a ko threat and fill a ko */
	if (kosave[move] != NOSQUARE && board[kosave[move]] == NOGROUP) {
		g = NOGROUP;	/* the single stone in atari */
		for (ptr = nbgrp[kosave[move]][color]; ptr != EOL; ptr = link[ptr]) {
			if (grlibs[list[ptr]] == 1) {
				if (g == NOGROUP)
					g = (group_t)list[ptr];
				else {
					g = NOGROUP;
					break;			/* only one group in atari */
				}
			}
		}
		if (g != NOGROUP && grsize[g] == 1) {
			for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
				if (grlibs[list[ptr]] == 1) {
					addmove(list[grlbp[list[ptr]]], ply, SOBVIOUS, 0, 0, obaval + 50);
				}
			}
		}
		addmove(kosave[move], ply, SOBVIOUS, 0, 0, obaval);
	}

	if (!havepattern) {
		tmplist = matchtypemoves(OBVIOUSFILE, s, 1-color, color, FALSE);
		val = 300;
		for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
			addmove(list[ptr], ply, SOBVIOUS, 0, 0, val);
			val -= 50;
		}
		killist(&tmplist);
	}

    if (mptr != -1 && mvs[mptr] != PASS) {
    	genobvgoals(move, mptr, ply);
   	}

	if (move > 0) {
		tmplist = matchtypemoves(OBVIOUSFILE, mvs[move-1], color, color, FALSE);
		val = 300;
		for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
			addmove(list[ptr], ply, SOBVIOUS, 0, 0, val);
			val -= 50;
		}
		killist(&tmplist);
	}


	/* retake a ko */
	if (move > 1 && kosave[move-1] != NOSQUARE && board[kosave[move-1]] == NOGROUP)
		addmove(kosave[move-1], ply, SOBVIOUS, 0, 0, obaval);

	/* try an atari forcing move */
	if (S_ALIVE(s) < WEAK && grlibs[board[s]] == 2 && !S_THREATENED(s)) {
		one_nbr = FALSE;
		for (ptr = grnbp[board[s]]; ptr != EOL; ptr = link[ptr]) {
			if (grlibs[list[ptr]] == 1 || gralive[list[ptr]] == DEAD) {
				one_nbr = TRUE;
				break;
			}
		}
		if (!one_nbr) {
			for (ptr = grlbp[board[s]]; ptr != EOL; ptr = link[ptr]) {
				if ((lnbn[list[ptr]] > 1 || lnbf[list[ptr]][color] != 0 && lnbn[list[ptr]] == 1)) {
					addmove(list[ptr], ply, SOBVIOUS, 0, 0, obaval);
				}
			}
		}
	}
}

/* ideally all of the big moves for the opponent of the original side to move 
 * TODO: get the moves earlier from earlier get_pass_reasons()
 */

extern sqr_t oppmoves[NUMSQUARES];		/* opponent moves and values for lookahead */
extern int oppmoveval[NUMSQUARES];
/*extern int nextoppmove; */

int taxidistance(sqr_t s1, sqr_t s2)
{
	return abs(xval[s1] - xval[s2]) + abs(yval[s1] - yval[s2]);
}

/* generate moves, sort them. 
 * return TRUE if a tenuki is possible, and negamax will evaluate the score
 * only return FALSE if there are more moves in sequence to generate
 *
 * color is the side to move now
 * ply is the current absolute ply depth.  ply=0 is the move under consideration,
 *   so the first call will be ply=1, and is an opponent response
 * depth is remaining full width depth, negative for q search
 * mptr is the mvcolor/mvs index of the original move
 *
 * the original mover must finish the local sequence first (joseki or pattern)
 * then finish any local obvious moves or tenuki
 * then make big nonlocal moves or tenuki
 *
 * the opponent can tenuki for a very big move elsewhere at a cost
 *
 * TODO: moves from fuskeki library, moves from life and death search, 
 * TODO: much better local reading
 */

int havejoseki = FALSE;
int havepattern = FALSE;
extern int numfullgen, numqgen;
static int genmoves(int color, int ply, float depth, int mptr, int handicap, int rules, int tenukicolor)
{
	int cantenuki;
	sqr_t s;
	havejoseki = havepattern = FALSE;
	firstsmove[ply] = nextsmove[ply] = nextsmove[ply - 1];
	firstpatmove[ply] = nextpatmove[ply] = nextpatmove[ply - 1];
	if (ply >= MAXSEARCHDEPTH - 1)
		return TRUE;	/* done, generate no moves */
	life(FALSE);
	if (depth > 0) {
		numfullgen++;
		get_search_reasons(color, handicap, rules, depth, ply);
		for (s = 0; s < boardsquare; ++s) {
			if (stratguess[ply][s] && s != kosquare) {
				addmove(s, ply, SBIG, 0, 0, stratguess[ply][s]);
			}
		}
		return TRUE;
	}

	/* generate moves for quiescence search */
	numqgen++;
	havejoseki = genjosekimoves(color, depth, ply);
	/* joseki take precedence over patterns - don't generate pattern responses if have joseki instead, or first few moves */
	if (!havejoseki && msptr > 3)
		havepattern = genpatternmoves(color, ply);

	/* only allow other types of moves if no joseki or opponent has the move.  initiator of pattern must complete it */
	if (!havejoseki) { /* &&	/* always finish joseki first */
		/*(!havepattern || (ply&1))) { side starting pattern must stay in pattern if possible */

		life(FALSE);	/* need life eval before generating these moves */
		/* if (!havepattern)	 let pattern generate the local moves better. 2 ply local responses */
		genobvlocalmoves(color, ply, depth, mptr, havepattern);

		/* kill and save last since if also genned by obvious, don't ignore if doesn't meet goal */
		gensavekillmoves(color, ply, mptr, havepattern);
	}

	cantenuki = !havejoseki && (!havepattern || (ply & 1));

#ifdef NEWSEARCH
	if (cantenuki && tenukicolor == NOCOLOR) {
		addmove(PASS, ply, SBIG, 0, 0, 2 * obaval);
	}
#endif

	/* no tenuki from pattern or joseki. */
	return cantenuki || nextsmove[ply]-firstsmove[ply] == 0;
}

#ifdef G2DEBUGOUTPUT

/* dump the search moves and values, leaving out patterns and joseki */
void outsearchmoves(int tm) {
	int m;
	char tmp[20];
	if (msptr == 0)
		return;
	genmoves(tm, 1, 0, msptr-1, 0, JAPANESE, NOCOLOR);
	for (m = firstsmove[1]; m < nextsmove[1]; ++m) {
		sprintf(tmp, "%d%c", m-firstsmove[1]+1, typemark[gnmv[m].type]);
		outstone(gnmv[m].s, tmp);
		sprintf(tmp, "%d: %d\n", m-firstsmove[1]+1, gnmv[m].val);
		outerr(tmp);
	}
}

void outobvious(void)
{
	list_t patlist = EOL, tmplist = EOL, pat2l = EOL;
	char buf[30], buf2[30];
	int i;
	if (msptr == 0)return;
	life(FALSE);
	initstacks();
	firstsmove[1] = nextsmove[1] = 1;
	genobvlocalmoves(1-mvcolor[msptr-1], 1, 2., msptr-1, FALSE);
	qsort((void*)&gnmv[firstsmove[1]], nextsmove[1]-firstsmove[1], sizeof(struct newmoves), compare);
	if (msptr > 0)
		patlist = matchtypemoves(OBVIOUSFILE, mvs[msptr-1], mvcolor[msptr-1], 1-mvcolor[msptr-1], TRUE);
	if (msptr > 1)
		pat2l = matchtypemoves(OBVIOUSFILE, mvs[msptr-2], mvcolor[msptr-2], mvcolor[msptr-2], TRUE);
	if (msptr > 1 && mvs[msptr-1] == PASS) {
		tmplist = matchtypemoves(OBVIOUSFILE, mvs[msptr-2], 1-mvcolor[msptr-1], 1-mvcolor[msptr-1], TRUE);
		ecatlist(&tmplist, &patlist);
	}
	outerr("Order of new obvious answers.\n");
	outerr("* indicates from pattern match.\n");
	outerr("! for pattern match on 2nd last move.\n");
	for (i = firstsmove[1]; i < nextsmove[1]; i++) {
		ssqr(gnmv[i].s, buf2);
		if (inflist(gnmv[i].s, &patlist))
			sprintf(buf,"%d*: %3d %s\n", i, gnmv[i].val, buf2);
		else if (inflist(gnmv[i].s, &pat2l))
			sprintf(buf, "%d!: 3%d %s\n", i, gnmv[i].val, buf2);
		else
			sprintf(buf,"%d: %3d %s\n", i, gnmv[i].val, buf2);
		outerr(buf);
	}
	killist(&patlist);
	killist(&pat2l);
}
#endif

/* is there a reason that fired using defend or attack value.  if
 * so we will not count the defend or attack value in the pattern.
 * DEFVAL1 indicates that a nonpattern move has added def_val().
 * DEFVAL2 and !PATTERN is another possibility.  return the biggest one.
 */

int hasdef1(sqr_t s)
{
	int ptr;
	int val = 0;
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr])
		if (strat[list[ptr]].goodrule &&
			((rules[strat[list[ptr]].reason].attack & DEFVAL1) ||
			(rules[strat[list[ptr]].reason].attack & DEFVAL2) && 
			!(rules[strat[list[ptr]].reason].attack & PATTERNMV))) {  /* found a nonpattern rule with a def value in it */
				if (strat[list[ptr]].value > val)
					val = strat[list[ptr]].value;  /* get biggest one since that's the one that will be counted */
		}
		return(val);
}


int hasatk1(sqr_t s)
{ 
	int ptr;
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr])
		if (strat[list[ptr]].goodrule &&
			rules[strat[list[ptr]].reason].attack & ATKVAL1)
			return(strat[list[ptr]].value);
	return(FALSE);
}

static void unmakesearchmove(int ruleset)
{
	sqr_t s;
	takebackamove(ruleset, &s);
}

extern int randomval[362], obaval;

/* get current ealuation with color to move.  higher scores are better for color 
 * mptr is the index to the move at the first ply being evaluated by this search
 */
extern int numsearchevals;
static int searcheval(int color, int handicap, int ruleset, int mptr, int *strat, int bonus, int tenukicolor, int *tenukival)
{
	int sumst;
	numsearchevals++;
	life(FALSE);	/* need before sumstrat and score */
	if (mvs[mptr] != PASS) {	/* pass search doesn't have strategic values */
		sumst = sumstrat(mvs[mptr], mvcolor[mptr]) + bonus;
		if (sumst > stratvalue[mvs[mptr]])
			stratvalue[mvs[mptr]] = sumst;
		if (bonus > stratpatvalue[mvs[mptr]])
			stratpatvalue[mvs[mptr]] = bonus;
		*strat = sumst + randomval[mvs[mptr]];
	}
	else
		*strat = 0;
#ifdef CHECK
	if (*strat > 20000 || *strat < -20000) {
		outerror("strat too big");
	}
#endif		
/*	if (mvs[mptr] != PASS)
		*strat -= 15;   don't make moves worth less than 1/3 point */
	if (mvcolor[mptr] == 1-color)
		*strat = -*strat;  /* strat is always good for color of "mptr" */

	if (tenukicolor == NOCOLOR)	
		*tenukival = 0;			/* color is doing a tenuki so it gets the bonus */
	else if (color == tenukicolor)	/* side that tenukied gets 2*oba for their tenuki, since they made a big move */
		*tenukival = 2*obaval;	
	else
		*tenukival = -2*obaval;
	return getcurrenteval(color, handicap, ruleset) + *strat + *tenukival;
}

static int meetgoal(int m)
{
	int goal = gnmv[m].type;
	int where = gnmv[m].where;
	int oldlife = gnmv[m].oldlife;
	life(FALSE);
	/* save and kill need to be verified */
	if (goal == SSAVE) {
		return board[where] != NOGROUP &&
			(S_ALIVE(where) <= ALIVE 
#ifdef NEWSEARCH
				&& S_ALIVE(where) < oldlife 
#endif
				||
			gralval[board[where]][1] >= 0 || /* 7/01 he can't make me worse than unsettled */
			S_ALIVE(where) == UNSETTLED_LIMP && oldlife > S_ALIVE(where) ||
			semalive[oldlife] && (semalive[S_ALIVE(where)] ||
			S_ALIVE(where) == UNSETTLED_DEAD) &&
			oldlife >= S_ALIVE(where));
	}
	else if (goal == SKILL) {
		return board[where] == NOGROUP ||
			S_ALIVE(where) == WEAK_POTENTIAL && oldlife < S_ALIVE(where) ||
			S_ALIVE(where) == WEAK_POTENTIAL && gralval[S_GROUP(where)][0] < -UNSETTLEDALVAL ||
			S_ALIVE(where) >= WEAK ||
			oldlife <= UNSETTLED_DEAD && S_ALIVE(where) > UNSETTLED_DEAD ||
			S_ALIVE(where) == WEAK_SEMEAI && oldlife <= SEMEAI;	/* 7/03 - allow moves that almost win semeai */
	}
	return TRUE;
}

static int timecheck = 0;
extern int numsearchnodes;

/* color to move, 
 * handicap stones 
 * absolute ply (always 1 for first negamax call) 
	in moves made from root of search (do not change from absolute, since index into move save arrays ) 
 * depth is remaining depth in search, negative for quiescence search
 * tenukicolor is the color of the side that already tenukied with a pass move.  Only allow one tenuki
 * return value, higher is better for color
 */

static int negamax(int color, int handicap, int ruleset, int ply, float depth, int alpha, int beta, int tenukicolor, list_t *seq, int mptr, int patbonus, unsigned int stoptime)
{
	int score = SMALLNUM;	/* best score found so far */
	int m, mp, flag, val, tcolor, atari;
	float dp;
	int movescore, strat, tenukival;
	list_t tmpseq = EOL;	/* best move sequence */
	int leaf = TRUE;
	int tenukiok = TRUE;
	int goodsave = FALSE;
	int goodkill = FALSE;
	int mcount = 0, maxvars;
	int goodcount;
	int bestgen = 0;
	sqr_t mv, bestmove = PASS, s;
	struct newmoves mvtmp;
#ifdef G2DEBUGOUTPUT
	int hscore;
	int i;
	char buf[200], buf2[100];
#endif

	numsearchnodes++;

	/*
	 * check for time limit and abort search
	 */
	timecheck++;
	if (timeout || depth >= 0 && timecheck > 10 && GetTickCount() > stoptime) {
		timecheck = 0;
#ifdef G2DEBUGOUTPUT
		if (logsearch) { INDENT fprintf(logfile, "OUT OF TIME - search aborted\n"); }
#endif
		timeout = TRUE;
		return SMALLNUM;
	}

	/* 
	 * null move.  look for a cutoff udring main search only, when beta is reasonable
	 * evaluate null move as a tenuki for 2x obaval for higher likelihood of cutoff
	 */
	if (depth > 0 && beta < 30000 && tenukicolor == NOCOLOR && kosquare == NOSQUARE) {
#ifdef G2DEBUGOUTPUT
		if (logsearch) { INDENT fprintf(logfile, "Null move search\n"); }
#endif
		makeamove(PASS, color, ruleset, &atari, FALSE);
		movescore = -negamax(1-color, handicap, ruleset, ply+1, depth-2, -beta, -alpha, color, &tmpseq, mptr, patbonus, stoptime);
		takebackamove(ruleset, &s);
#ifdef G2DEBUGOUTPUT
		if (logsearch) {
			INDENT fprintf(logfile, "%d: NULL move has value %d %s\n", ply, movescore, movescore > alpha ? "NEW BEST" : "");
		}
#endif		
		if (movescore == -SMALLNUM) {
#ifdef G2DEBUGOUTPUT
			if (logsearch) {
				INDENT fprintf(logfile, "NUll move search returned -SMALLNUM, move rejected\n");
			}
#endif		
			movescore = SMALLNUM;
		}
#ifdef NEVER
		if (movescore > score) {
			if (*seq != EOL)
				killist(seq);
			*seq = tmpseq;
			tmpseq = EOL;
			adflist(PASS, seq);
			score = movescore;
		}
		else {
			killist(&tmpseq);
		}
#endif
		killist(&tmpseq);
		if (movescore > alpha) {
/*			alpha = movescore; */
			if (movescore >= beta) {
#ifdef G2DEBUGOUTPUT
				if (logsearch) { INDENT fprintf(logfile, "Null move cutoff %d >= %d\n", movescore, beta); }
#endif
				addentry(hashval[msptr][0]+color, LOWERBOUND, depth, movescore, PASS);
				return movescore;
			}
		}
	}

	/*
	 * generate moves
	 */
	tenukiok = genmoves(color, ply, depth, mptr, handicap, ruleset, tenukicolor);

	/* 
	 * sort the moves 
	 */
	qsort((void*)&gnmv[firstsmove[ply]], nextsmove[ply]-firstsmove[ply], sizeof(struct newmoves), compare);

	/* 
	 * Entend search depth if moves are forced. 
	 * singular move extension 
	 */
	if (depth > 0 && nextsmove[ply]-firstsmove[ply] == 1) {
		depth += 1;
#ifdef G2DEBUGOUTPUT
		if (logsearch) { INDENT fprintf(logfile, "One move generated - depth += 1\n"); }
#endif
	}
	else if (depth > 0 && nextsmove[ply]-firstsmove[ply] == 2) {
		depth += 0.5;
#ifdef G2DEBUGOUTPUT
		if (logsearch) { INDENT fprintf(logfile, "Two moves generated - depth += 0.5\n"); }
#endif
	}
	else if (depth > 0 && !tenukiok) {
		depth += 0.5;	/* extend search for forced local moves */
#ifdef G2DEBUGOUTPUT
		if (logsearch) { INDENT fprintf(logfile, "Forced local move - depth += 0.5\n"); }
#endif
	} else if (kosquare != NOSQUARE) {
		depth += 0.5;	/* extend for ko threat */
	}

	/*
	 * check if the hash table has a move to put first
	 */
	if (tt_lookup(hashval[msptr][0] + color, &flag, &dp, &val, &mv)) {
		if (mv != PASS) {
			for (m = firstsmove[ply] + 1; m < nextsmove[ply]; ++m) {
				if (gnmv[m].s == mv) {
					mvtmp = gnmv[m];		/* force hash move to front, keeping rest in order */
					for (mp = m - 1; mp >= firstsmove[ply] ; mp--) {
						gnmv[mp + 1] = gnmv[mp];
					}
					gnmv[firstsmove[ply]] = mvtmp;
#ifdef G2DEBUGOUTPUT
					if (logsearch) { INDENT fprintf(logfile, "Hash hit moves %s to first move\n", ssqr(mv, buf)); }
#endif
					break;
				}
			}
		}
#if G2DEBUGOUTPUT
		else {
			if (logsearch) { INDENT fprintf(logfile, "hash hit, move is pass %I64x\n", hashval[msptr][0] + color); }
		}
#endif
	}
#if G2DEBUGOUTPUT
	else {
		if (logsearch) { INDENT fprintf(logfile, "hash miss %I64x\n", hashval[msptr][0] + color); }
	}
#endif


#ifdef G2DEBUGOUTPUT
	if (logsearch) {
		INDENT
		fprintf(logfile, "%d:%.1f  ", ply, depth);
		if (firstsmove[ply] == nextsmove[ply])
			fprintf(logfile, "No %s moves generated havepattern %d.", cnames[color], havepattern);
		else {
			fprintf(logfile, "Gen: ");
		}
		if (tenukiok)
			fprintf(logfile, "can tenuki: ");
		for (i = firstsmove[ply]; i < nextsmove[ply]; ++i) {
			fprintf(logfile, "%s%d%c%s, ", ssqr(gnmv[i].s, buf), gnmv[i].val, typemark[gnmv[i].type],
			(gnmv[i].type == SSAVE || gnmv[i].type == SKILL || gnmv[i].type == SFSAVE || gnmv[i].type == SFKILL)?ssqr(gnmv[i].where, buf2):"");
		}
		fprintf(logfile, "\n");
	}
#endif


	/*
	 * evaluate in the quiescence search 
	 */
	if (depth <= 0 && tenukiok 
#ifdef NEWSEARCH
		&& tenukicolor != NOCOLOR
#endif
		) {

			/* score if I tenuki */
		if (!te_lookup(hashval[msptr][0]+color, mvs[mptr], &score)) {
			score = searcheval(color, handicap, ruleset, mptr, &strat, patbonus, tenukicolor, &tenukival);
			te_addentry(hashval[msptr][0] + color, score, mvs[mptr]);
#ifdef G2DEBUGOUTPUT
			if (logsearch) { INDENT fprintf(logfile, "%d: Eval leaf score %d (currenteval %d) (terr %d, aji %d, komi %d, patbonus %d, strat %d, bigscr %d, obaval %d, tenukival %d). exact hash %I64x added.\n", 
				ply, score, getcurrenteval(color, handicap, ruleset), pscr+tscr+dscr, ajiscr, komi, patbonus, strat, bigscr, obaval, tenukival, hashval[msptr][0]+color); }
#endif
		}
#ifdef G2DEBUGOUTPUT
		else {
			if (logsearch) {
				INDENT fprintf(logfile, "Evaluation hash hit score %d, hash %I64x\n", score, hashval[msptr][0]+color);
				hscore = searcheval(color, handicap, ruleset, mptr, &strat, patbonus, tenukicolor, &tenukival);
				strat = 0;
				tenukival = 0;
				if (score != hscore) {
					INDENT fprintf(logfile, "ERROR: eval score %d not equal to hash score %d", hscore, score);
				}
			}
		}
#endif
		if (score > alpha)
			alpha = score;
		if (score >= beta) {
#ifdef G2DEBUGOUTPUT
			if (logsearch) { INDENT fprintf(logfile, "%d: Eval cutoff score %d (terr %d, aji %d, komi %d, strat %d, obaval %d, tenukival %d) >= beta %d. hash %I64x lower bound added.\n", 
				ply, score, pscr+tscr+dscr, ajiscr, komi, strat, obaval, tenukival, beta, hashval[msptr][0]+color); }
#endif
			addentry(hashval[msptr][0]+color, LOWERBOUND, depth, score, PASS);
			return score;
		}
#ifdef G2DEBUGOUTPUT
		if (logsearch) { INDENT fprintf(logfile, "%d: score if %s tenuki is %d (scr %d, strat %d, obaval %d, tenukival %d)\n", ply, cnames[color], score, score, strat, obaval, tenukival);	}
#endif
	}

	/* 
	 * figure out how many moves to try
	 */
	mcount = 0;
	maxvars = maxvariations[playlevel];
	if (depth <= -4) {
		maxvars = 1;
	}
#ifdef NEWSEARCH
	else if (depth <= -2 && maxvars >= 4) {
		maxvars /= 4;
	}
#endif
	else if (depth <= 0 && maxvars >= 2) {
		maxvars /= 2;
	} else if (depth > 0 && playlevel >= MAXLEVEL) {
		maxvars += (int)depth;
	}
#ifdef G2DEBUGOUTPUT
	if (logsearch) {
		INDENT fprintf(logfile, "Trying max %d moves\n", maxvars);
	}
#endif

	/*
	 * try each move
	 */
	for (m = firstsmove[ply]; m < nextsmove[ply] && mcount < maxvars; ++m) {
		if (gnmv[m].val == SMALLNUM) {
#ifdef G2DEBUGOUTPUT
			if (logsearch) {
				INDENT fprintf(logfile, "stopping becasue out of good moves\n");
			}	
#endif		
			break;	/* out of good moves */
		}
#ifdef NEWSEARCH
		if (depth <= 0 && gnmv[m].s != PASS && 
				(gnmv[m].val < bestgen / 4 || gnmv[m].val < 2 * obaval || gnmv[m].val < 100)) {
		if (depth <= 2 && mcount > 1 && mcount >= maxvars / 2 && gnmv[m].val < bestgen / 4) {
#else
		if (depth <= 0 && gnmv[m].val < bestgen / 4) {		/* this from good 12/28 version */
#endif

#ifdef G2DEBUGOUTPUT
			if (logsearch) {
				INDENT fprintf(logfile, "stopping because move value too low\n");
			}	
#endif		
			break;
		}
#ifdef NEVER
		if (depth <= -2 && mcount > 1 && mcount >= maxvars / 2 && score < alpha) {
#ifdef G2DEBUGOUTPUT
			if (logsearch) {
				INDENT fprintf(logfile, "stopping because moves all worse than alpha\n");
			}	
#endif	
			break;
		}
#endif
		if ((gnmv[m].type == SSAVE || gnmv[m].type == SFSAVE) && goodsave) {
			goodcount = 0;
			for (mp = firstsmove[ply]; mp < m; mp ++) {
				if ((gnmv[mp].type == SSAVE || gnmv[mp].type == SFSAVE) && gnmv[mp].where == gnmv[m].where && gnmv[mp].good) {
					goodcount++;
				}
			}
			if (depth <= 0 && goodcount > 1) {	/* only one working move per group in Q search */
#ifdef G2DEBUGOUTPUT
				if (logsearch) {
					INDENT fprintf(logfile, "%d: skipping save move %s\n", ply, ssqr(gnmv[m].s, buf));
				}	
#endif		
				continue;	/* one good save move is enough */
			}
		}
		if ((gnmv[m].type == SKILL || gnmv[m].type == SFKILL) && goodkill) {
			goodcount = 0;
			for (mp = firstsmove[ply]; mp < m; mp ++) {
				if ((gnmv[mp].type == SKILL || gnmv[mp].type == SFKILL) && gnmv[mp].where == gnmv[m].where && gnmv[mp].good) {
					goodcount++;
				}
			}
			if (depth <= 0 && goodcount > 1) {
#ifdef G2DEBUGOUTPUT
				if (logsearch) {
					INDENT fprintf(logfile, "%d: skipping kill move %s\n", ply, ssqr(gnmv[m].s, buf));
				}
#endif		
				continue;	/* one good kill move is enough */
			}
		}
		if (makeamove(gnmv[m].s, color, ruleset, &atari, FALSE) != G2OK) {
#ifdef G2DEBUGOUTPUT
			if (logsearch) {
				INDENT fprintf(logfile, "%d: skipping illegal move %s\n", ply, ssqr(gnmv[m].s, buf));
			}
#endif		
			continue;	/* illegal move, wasn't made */
		}

		if (depth <= 0 && !meetgoal(m)) {
			takebackamove(ruleset, &s);
#ifdef G2DEBUGOUTPUT
			if (logsearch) {
				INDENT fprintf(logfile, "%d: didn't meet goal.  skipping move %s\n", ply, ssqr(gnmv[m].s, buf));
			}
#endif		
			continue;
		}
		leaf = FALSE;
#ifdef G2DEBUGOUTPUT
		if (logsearch) {
			INDENT fprintf(logfile, "%d:%.1f trying move %s %s.  alpha %d, beta %d\n", ply, depth, cnames[color], ssqr(gnmv[m].s, buf), alpha, beta);
		}
#endif	
		mcount++;
		if (gnmv[m].val > bestgen)
			bestgen = gnmv[m].val;
		tcolor = tenukicolor;
#ifdef NEWSEARCH
		if (tcolor == NOCOLOR && gnmv[m].s == PASS) {
			tcolor = color;
		}
#endif
		movescore = -negamax(1 - color, handicap, ruleset, ply + 1, depth - 1, -beta, -alpha, tcolor, &tmpseq, mptr, patbonus, stoptime);
		takebackamove(ruleset, &s);
		if (movescore == -SMALLNUM) {
#ifdef G2DEBUGOUTPUT
			if (logsearch) {
				INDENT fprintf(logfile, "got -SMALLNUM, move rejected\n");
			}
#endif		
			movescore = SMALLNUM;
		}
#ifdef G2DEBUGOUTPUT
		if (logsearch) {
			INDENT fprintf(logfile, "%d: move %s %s has value %d %s\n", ply, cnames[color], ssqr(gnmv[m].s, buf), movescore, movescore > alpha?"NEW BEST":"");
		}
#endif		
		if (movescore > score) {
			if (*seq != EOL)
				killist(seq);
			*seq = tmpseq;
			tmpseq = EOL;
			adflist(gnmv[m].s, seq);
			score = movescore;
		}
		else {
			killist(&tmpseq);
		}
		if (score > alpha) {
			alpha = score;
			bestmove = gnmv[m].s;
		}
		if (score >= beta) {
#ifdef G2DEBUGOUTPUT
			if (logsearch)  {
				INDENT fprintf(logfile, "%d: Return cutoff: score %d >= beta %d, %s hash %I64x lower bound added\n", 
					ply, score, beta, ssqr(gnmv[m].s, buf), hashval[msptr][0]+color);
			}
#endif		
			addentry(hashval[msptr][0]+color, LOWERBOUND, depth, score, gnmv[m].s);
			return score;
		}
		/* did I find a good move to save or kill? */
		if (movescore > SMALLNUM && (gnmv[m].type == SSAVE || gnmv[m].type == SFSAVE)) {
			goodsave = TRUE;
			gnmv[m].good = TRUE;
		}
		if (movescore > SMALLNUM && (gnmv[m].type == SKILL || gnmv[m].type == SFKILL)) {
			goodkill = TRUE;
			gnmv[m].good = TRUE;
		}
		if (depth < -maxscorebrdepth[playlevel])
			break;
	}
	if (mcount == 0) {
		score = searcheval(color, handicap, ruleset, mptr, &strat, patbonus, tenukicolor, &tenukival);
//#ifdef NEWSEARCH
		te_addentry(hashval[msptr][0] + color, score, mvs[mptr]);
//#endif
#ifdef G2DEBUGOUTPUT
		if (logsearch) {
			INDENT fprintf(logfile, "No moves found, return current score\n", score);
		}
#endif
	}
#ifdef G2DEBUGOUTPUT
	if (logsearch) {
		INDENT fprintf(logfile, "Add exact hash best move %s score %d hash %I64x\n", ssqr(bestmove, buf), score, hashval[msptr][0]+color);
	}
#endif
	addentry(hashval[msptr][0]+color, EXACT, depth, score, bestmove);
	return score;
}

#ifdef NEVER

static void findpatternreasons(sqr_t s, int color)
{
	list_t ptr;
	int ptr2;
	int patcolor;
	if (s == PASS)
		return;
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {  /* find each pattern match reason */
		if (!(rules[strat[list[ptr]].reason].attack & PATTERNMV))
			continue;
		if (!strat[list[ptr]].goodrule)
			continue;
		if (strat[list[ptr]].patcolor)
			patcolor = 1-color;  /* pattern matched colors reversed */
		else 
			patcolor = color;
		for (ptr2 = strat[list[ptr]].patmove; ptr2 != -1; ptr2 = patmore(ptr2)) {
			if (patgetsqr(strat[list[ptr]].patsqr, strat[list[ptr]].pato, pm[ptr2].xy) == s &&
				PM_COLOR(ptr2) == patcolor) {  /* find move under consideration in any pattern */
				addpatmove(strat[list[ptr]].patsqr, strat[list[ptr]].pato, strat[list[ptr]].patcolor, strat[list[ptr]].pattern, ptr2, 0);
			}
		}
	}
}

#endif

static int getbonus(sqr_t s, int c)
{
	list_t ptr;
	int patcolor;
	int ptr2;
	int bestpat = 0;
	int tbon;
	int move = msptr-1; 
	int defv;

	if (s == PASS)
		return 0;
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {  /* find each pattern match reason */
		if (!(rules[strat[list[ptr]].reason].attack & PATTERNMV)) {
			continue;
		}
		if (strat[list[ptr]].patcolor)
			patcolor = 1-c;  /* pattern matched colors reversed */
		else 
			patcolor = c;
		for (ptr2 = strat[list[ptr]].patmove; ptr2 != -1; ptr2 = patmore(ptr2)) {
			if (patgetsqr(strat[list[ptr]].patsqr, strat[list[ptr]].pato, pm[ptr2].xy) == s &&
				PM_COLOR(ptr2) == patcolor) {  /* find move under consideration in any pattern */
				if (PM_VALUE(ptr2) == 0) {  /* marked as bad move */
					return SMALLNUM;  /* if any pattern calls it bad it must be bad */
				}
				/* get best bonus for all patterns that work at one ply */
		 		if (patgoodreason(list[ptr], move, c)) {
					tbon = strat[list[ptr]].value;  
					if ((rules[strat[list[ptr]].reason].attack & DEFVAL2) &&
						(defv = hasdef1(mvs[move])) ||
						(rules[strat[list[ptr]].reason].attack & ATKVAL2) &&
						(defv = hasatk1(mvs[move])))
						tbon -= defv; /* leave off defensive value to avoid duplication with nonpattern defensive rules */
					if (tbon > bestpat)
						bestpat = tbon;
				}
			}
		}
	}
	return bestpat;
}

void fuslearnreasons(sqr_t s, int color) {
	int wwins, bwins, wval;
	struct learn_node *fuseki = NULL, *joseki[4], *side[4];
	
	if (s == PASS)
		return;
	learn_lookup(hashval[msptr], &fuseki, joseki, side);
	if (fuseki == NULL)
		return;

	bwins = fuseki->wins[0];
	wwins = fuseki->wins[1];

	if (playlevel >= FUSEKILEVEL && fuseki->strongest >= 10) {	/* professional position */
		fire_strat_rule(s, PRO_MOVE, 50 * 3, NOSQUARE, 0);
	}

	if (wwins != 0 || bwins != 0) {
		/* increase factor every 6 moves, 1/2 point per 10% win or loss */
		wval = (int)((1. + msptr / 6.) * 25. * 10. * ((double)wwins / (wwins + bwins) - 0.5));
		if (wval > 50 * 15)
			wval = 50 * 15;
		if (wval < -50 * 15)
			wval = -50 * 15;
		if (color == WHITECOLOR) {
			if (wwins > bwins)
				fire_strat_rule(s, WINNING_MOVE, wval, NOSQUARE, 0);
			else if (bwins > wwins)
				fire_strat_rule(s, LOSING_MOVE, wval, NOSQUARE, 0);
		}
		else {
			if (bwins > wwins)
				fire_strat_rule(s, WINNING_MOVE, -wval, NOSQUARE, 0);
			else if (wwins > bwins)
				fire_strat_rule(s, LOSING_MOVE, -wval, NOSQUARE, 0);
		}
	}
}

/* do full board lookahead sor a single move
 * replaces lookahead
 * follow patterns, joseki
 * read semeai
 * read life/death of surrounded groups
 * if rule suggested single move, just evaluate now.
 * obvious answers and obvious continuations
 * update bestseq[s] with the best sequence
 *
 * s is the move just made to do lookahead from
 * return the relative score of the move sequence (higher better for color
 *   making move s)
 * alpha is the best score so far for the side to move
 * color is color of move at s
 * depth is ply to search after move at s with any joseki/pattern followups
 * stoptime is time to stop, in GetTickCount()
 * reasons means we should do the one-ply reasons.
 * return goodmove as false if no goals matched.
 *
 * pass the index of the move just made to all search routines (mptr), so they can 
 * see the reasons for this move, and find the current move sequence.
 *
 * must do a full search on every move to try
 */


int search(sqr_t s, int alpha, int beta, int color, int handicap, int ruleset, float depth, unsigned int stoptime, int reasons)
{
	sqr_t stmp;
	int val, atari;
	int patbonus;			/* bonus to use for pattern search */
	list_t seq = EOL;		/* best possible sequence of moves */
	int mptr = msptr;		/* index of this move in mvs, mvcolor */
#ifdef G2DEBUGOUTPUT
	list_t ptr;
#endif

#ifdef G2DEBUGOUTPUT
	char buf[20];
	if (logsearch) {
		fprintf(logfile, "****  Search %s %s depth %.1f, alpha %d beta %d guess %d scoreval %d\n", 
			cnames[color], ssqr(s, buf), depth, alpha, beta, stratguess[0][s], scoreval[s]);
	}
#endif	

	numsearchnodes++;

	/* initialize the stacks */
	initstacks();

	/* make the move at ply 0 */
	if (makeamove(s, color, ruleset, &atari, FALSE) != G2OK) {
#ifdef G2DEBUGOUTPUT
		if (logsearch) fprintf(logfile, "SUICIDE or repetition: Final value returned is %d\n", SMALLNUM);
#endif
		fire_strat_rule(s, REPETITION, -10000, NOSQUARE, 0);
		return SMALLNUM;
	}

	life(FALSE);						/* 11/00 need accurate life/death values for stval, getbonus, etc */
	if (reasons) {
		fuslearnreasons(s, color);		/* use information from the fuseki learning database */
		stval(msptr - 1);					/* fill in strat[].goodrule and stratgoodreasons for one ply rules. */
										/* ignores pattern matches */
	}
	patbonus = getbonus(s, color);	/* get the bonus and set good reasons for patterns */
/*	findpatternreasons(s, color);	 pattern move tree to search */

	if (patbonus == SMALLNUM) {
#ifdef G2DEBUGOUTPUT
		if (logsearch) fprintf(logfile, "Pattern database rejects this move\n");
#endif
		takebackamove(ruleset, &stmp);
		killist(&bestseq[s]);
		adflist(s, &bestseq[s]);
		return SMALLNUM;
	}

	timeout = FALSE;
	val = -negamax(1 - color, handicap, ruleset, 1, depth, -beta, -alpha, NOCOLOR, &seq, mptr, patbonus, stoptime);
	takebackamove(ruleset, &stmp);
	life(FALSE);

	/* record best sequence of moves */
	killist(&bestseq[s]);
	bestseq[s] = seq;
	adflist(s, &bestseq[s]);

/*	*goodmove = TRUE;  5/20/03 can't do this or makes bad moves when no good rule prevents lookahead */
#ifdef G2DEBUGOUTPUT
	if (val > BIGNUM || val < SMALLNUM)
		addcomment("ERROR: search value out of range.\n");
	if (logsearch) {
		fprintf(logfile, "Final value returned is %d ", val);
		for (ptr = bestseq[s]; ptr != EOL; ptr = link[ptr])
			fprintf(logfile, "%s ", ssqr((sqr_t)list[ptr], buf));
		fprintf(logfile, "\n");
	}
#endif
	return val;
}

/*
 * get the value of the current position after a quiescence search, with color to move
 */
int getqsearchval(int color, int handicap, int ruleset)
{
	int val;
	float depth = 0.0;
	int alpha = SMALLNUM;
	int beta = BIGNUM;
	list_t seq = EOL;		/* best possible sequence of moves */
#ifdef G2DEBUGOUTPUT
	unsigned int starttime = GetTickCount();
#endif
	unsigned int stoptime = GetTickCount() + 5 * 1000;

#ifdef G2DEBUGOUTPUT
	list_t ptr;
	char buf[20];
	if (logsearch) {
		logfile = fopen("\\go\\wingovs\\searchlog.txt", "w");
		if (logfile == NULL) {
			logsearch = FALSE;
		}
		else
			fprintf(logfile, "Quiescence search, level %d.\n", playlevel);
	}
#endif

	life(FALSE);
	val = negamax(color, handicap, ruleset, 1, depth, -beta, -alpha, NOCOLOR, &seq, msptr, 0, stoptime);

#ifdef G2DEBUGOUTPUT
	if (logsearch) {
		fprintf(logfile, "Complete in %5.2f secs.  Total evals %d, Final value returned is %d.  ", 
			(GetTickCount() - starttime)/1000., numlifecalls, val);
		for (ptr = seq; ptr != EOL; ptr = link[ptr])
			fprintf(logfile, "%s ", ssqr((sqr_t)list[ptr], buf));
		fprintf(logfile, "\n");
		fclose(logfile);
		logfile = NULL;
	}
#endif
	killist(&seq);
	return val;
}

