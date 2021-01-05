/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* Here is the code for full board lookahead.
 *
 * Do lookahead based on reasons for moves, using patterns, or for
 * fights.
 */
 
#ifdef G2DEBUGOUTPUT
# include <stdio.h> 
#endif

#include <math.h>
# include "g2hd.h"
# include "g2rldef.h"
# include "g2pat.h" 
# include "g2tree.h"
# include "g2fcache.h"

# define SAVE 0
# define ANSWER 1
# define KILL 2
# define BIG 3

#ifdef G2DEBUG
char *goals[] = { "Save", "Answer", "Kill", "Big move"};
#endif

# include "g2look.pr"

extern int thalive[NUMALIVE],thalive2[NUMALIVE],pfac[NUMALIVE],semalive[NUMALIVE],sumeyes[41];
extern int livealive[NUMALIVE],kval[NUMALIVE],obaval,savealive[NUMALIVE];
extern char maxscoredepth[21],maxscorebrdepth[21];
#ifdef IGS
extern int igsmode;
#endif
extern int cfac[3],numlifecalls;


	
/* return biggest army to try to kill */


army_t biggesttokill(int c, int *mval) {
	group_t g;
	int biggest = SMALLNUM, val, tmp;
	army_t army, karmy = NOARMY;

	*mval = 0;
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])continue;
		if (grcolor[g] == c)continue;
		if (thalive2[G_ALIVE(g)] || G_THREATENED(g) && G_ALIVE(g) <= WEAK) {
			army = grarmy[g];
			tmp = (gralprob[g]-gralval[g][1]);
			val = tmp * (armysize[army] + armylibs[army]); 
			if (val > biggest) {
				biggest = val;
				karmy = grarmy[g];
				}
			}
		}
	*mval = biggest;
	return(karmy);
	}

/* find obvious moves that save army. get all moves that
 * add the most potential eyes
 */

list_t genobvioussave(army_t army) {
	list_t moves = EOL;

	moves = justlifemoves((group_t)list[armygroups[army]]);
	dlflist(kosquare,&moves);  /* need ko threat? */
	return(moves);
	}


/* find obvious moves that kill army. get all moves that
 * add the most potential eyes.  May return illegal ko move.
 */

list_t genobviouskill(army_t army) {
	list_t moves = EOL;

	moves = justkillmoves((group_t)list[armygroups[army]], 0);
	dlflist(kosquare,&moves);  /* need ko threat? */

	return(unflist(moves));
	}


/* return the sum of good strategic reasons for move in
 * current position ( which may be several ply down )
 * only add the best bonus with a def value added
 */

#define MAXRULES 100
struct {	/* valid rules for analysis */
	int val;	/* value of the rule */
	int attack;	/* attack value */
	int param;
	int done;	/* already added */
} vrs[MAXRULES];	/* valid rules */

int sumstrat(sqr_t s, int color)
{
	int strattotal = 0, rule, applied = FALSE, patreas = FALSE, i, j;
	int bestdef = 0, totdef = 0;  /* best defence value */
	int bestatk = 0, totatk = 0;  /* best attack value */
	int bestbig = 0;  /* best big value */
	int nextrule = 0;
	list_t ptr;

	if (stratreasons[s] == EOL)
		return(0);  /* for joseki */

	/* see if any rules applied */

	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
		rule = strat[list[ptr]].reason;
		if (rules[rule].attack & PATTERNMV) {
			patreas = TRUE;  /* there is a pattern reason for this move... */
			break;
		}
	}

	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
		rule = strat[list[ptr]].reason;
		if (rules[rule].attack & PATTERNMV) {
			applied = TRUE;
			continue;  /* patterns handled elsewhere */
		}
		if (patreas && (rules[rule].attack & PATBETTER))
			continue;  /* pattern is more accurate */
		if (rule_applied(rule, s, color, strat[list[ptr]].param)) {
			applied = TRUE;
			vrs[nextrule].val = strat[list[ptr]].value + rules[rule].value;
			vrs[nextrule].attack = rules[rule].attack;
			vrs[nextrule].param = strat[list[ptr]].param;
			vrs[nextrule].done = FALSE;
			if (nextrule < MAXRULES-1)
				nextrule++;
		}
	}
	/* remove duplicates, keeping the best values 3/02 */
	for (i = 0; i < nextrule; ++i) {
		if (vrs[i].done)
			continue;
		if ((vrs[i].attack & DEFVAL1) ||
			(vrs[i].attack & DEFVAL2)) {
				bestdef = vrs[i].val;
				for (j = i+1; j < nextrule; ++j) {
					if ((vrs[j].attack & DEFVAL1 ||
						vrs[j].attack & DEFVAL2) &&
						vrs[j].param == vrs[i].param) { /* duplicate rule for same reason */
							if (vrs[j].val > bestdef)
								bestdef = vrs[j].val;
							vrs[j].done = TRUE;
					}
				}
				totdef += bestdef;
		}
		else if ((vrs[i].attack & ATKVAL1) ||
			(vrs[i].attack & ATKVAL2)) {
				bestatk = vrs[i].val;
				for (j = i+1; j < nextrule; ++j) {
					if ((vrs[j].attack & ATKVAL1 ||
						vrs[j].attack & ATKVAL2) &&
						vrs[j].param == vrs[i].param) { /* duplicate rule for same reason */
							if (vrs[j].val > bestatk)
								bestatk = vrs[j].val;
							vrs[j].done = TRUE;
					}
				}
				totatk += bestatk;
		}
		else if (vrs[i].attack & BIGMOVE) {
			if (vrs[i].val > bestbig)
				bestbig = vrs[i].val;
		}
		else
			strattotal += vrs[i].val;
	}
	strattotal += totdef + totatk + bestbig;
	if (!stratgoodreasons[s] && strattotal == 0 && !applied) {
		fire_strat_rule(s, NO_RULE_APPLIES, 0, 0, 0);
#ifdef G2DEBUGOUTPUT
		if (debug)
			outerr("no rule applies\n");
#endif
		return(-5000);
	}
#ifdef G2DEBUGOUTPUT
	if (strattotal < -20000 || strattotal > 20000)
		outerr("Bad strattotal\n");
#endif
	return strattotal;
}


/* strategic value of this move at s
 * s is square just moved onto
 * fill in stratgoodreasons[s] and strat[].goodrule for one ply rules.
 * can fire some additional rules
 */

void stval(int move)
{
    int g, g2, ptr, ptr2, s1, s2, c, sente_val, rule, val, flag;
    sqr_t s;
#ifdef CHECK
	char buf[100];
#endif

    s = mvs[move];
   	g = board[s];

#ifdef CHECK
	if (s >= boardsquare) {
		sprintf(buf,"stval called s = %d!\n",s);
		outerror(buf);
	}
   	if (g == NOGROUP) {
    	outerror("stval g is NOGROUP!!\n");
    	outerror(ssqr(s,buf));
    	outerror("\n");
	}
#endif

	/* block expansion along edge */

	if (G_ALIVE(g) <= VERY_ALIVE && ( edge[s] == 3 || edge[s] == 2) &&
		fdir[s] > 32) {
		c = grcolor[g];
		s1 = s + nbr[fdir[s]];		/* left */
		s2 = s + nbr[fdir[s] + 1];	/* right */
		if (grcolor[board[s2]] == 1 - c && groldalive[s2] && groldalive[s2] <= WEAK_POTENTIAL &&
			ltr1[s1] != 0 && !ltrgd[s1]) {
			val = 100;
			if (groldalive[s2] >= UNSETTLED)
				val = 50;
			fire_strat_rule(s,BLOCK_FROM_TERR,val,NOSQUARE,0);
		}

		if (grcolor[board[s1]] == 1 - c && groldalive[s1] && groldalive[s1] <= WEAK_POTENTIAL &&
			ltr1[s2] != 0 && !ltrgd[s2]) {
			val = 100;
			if (groldalive[s2] >= UNSETTLED)
				val = 50;
			fire_strat_rule(s, BLOCK_FROM_TERR, val, NOSQUARE, 0);
		}
	      /* block out of good territory */
	}

	/* see if a group evaluation is wrong after reading */
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
		rule = strat[list[ptr]].reason;
		if (rule == MAKE_GROUP_DIE && S_ALIVE(strat[list[ptr]].param) < WEAK_POTENTIAL) {
			flag = FALSE;
			for (ptr2 = stratreasons[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (strat[list[ptr2]].reason == KILL_WORKS && strat[list[ptr2]].param == strat[list[ptr]].param) {
					flag = TRUE;	/* avoid duplicates */
					break;
				}
			}
			if (!flag) {
				g2 = S_GROUP(strat[list[ptr]].param);
				val = (grsize[g2] + grlibs[g2]) * (50 + gralprob[g2]);
				fire_strat_rule(s, KILL_WORKS, val * 3 / 4, strat[list[ptr]].param, 0);
			}
		}
	}
	/* see if a group evaluation is wrong after reading. unexpected good result - make sure it gets counted */
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
		rule = strat[list[ptr]].reason;
		if (rule == MAKE_GROUP_LIVE && S_ALIVE(strat[list[ptr]].param) > ALIVE) {
			flag = FALSE;
			for (ptr2 = stratreasons[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (strat[list[ptr2]].reason == LIVE_WORKS && strat[list[ptr2]].param == strat[list[ptr]].param) {
					flag = TRUE;	/* avoid duplicates */
					break;
				}
			}
			if (!flag) {
				g2 = S_GROUP(strat[list[ptr]].param);
				val = (grsize[g2] + grlibs[g2]) * (50 - gralprob[g2]);
				/* 3/02 *3/4 since prefer moves that match evaluation */
				fire_strat_rule(s, LIVE_WORKS, val * 3 / 4, strat[list[ptr]].param, 0);
			}
		}
	}
	/* see if any rules applied */

	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
		rule = strat[list[ptr]].reason;
		if (rules[rule].attack & PATTERNMV)
			continue;  /* patterns handled elsewhere */
		if (rule_applied(rule, mvs[move], mvcolor[move], strat[list[ptr]].param)) {
			strat[list[ptr]].goodrule = TRUE;
			if (rules[rule].value >= 0) {
				stratgoodreasons[s] = TRUE;
			}
		}
	}
	
	if (stratgoodreasons[s])
		check_killed(s);	/* killed anything by accident? */

	sente_val = 0;
}



/* bad move return true if any rules fired had negative values */

int badmove(sqr_t s)
{
	int ptr;
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
		if (rules[strat[list[ptr]].reason].value < 0)
			return(TRUE);
	}
	return(FALSE);
}

   	
/* see if the last move at s saved friendly or killed any enemy groups
 * (changed their aliveness from unsettled to weak)
 * value for pieces weakened or killed is already included in
 * pfac.  Add extra for liberties of group killed.  If attacking group
 * is not strong, reduce the value of the attack
 */
void check_killed(sqr_t s)
{
	group_t g;
	int val;
	list_t armylist=EOL, oldarmylist=EOL, savelist = EOL, reasons, ptr, ptr2;
	int saverule, killrule;
	int saveval = 0;
	if (badmove(s))
		return;
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])
			continue;
		if (grcolor[g] != grcolor[board[s]] &&
			G_ALIVE(g) > WEAK_SEMEAI &&
			groldalive[mvs[grpieces[g]]] <= UNSETTLED &&
			groldalive[mvs[grpieces[g]]] > VERY_ALIVE &&
			(groldalive[mvs[grpieces[g]]] > ALIVE || ahead <= 2))
			addlist(grarmy[g],&armylist);
		if (grcolor[g] == grcolor[board[s]] &&
			G_ALIVE(g) <= ALIVE &&
			groldalive[mvs[grpieces[g]]] > ALIVE) {
			addlist(g,&savelist);
		}
	}
	reasons = stratreasons[s];
	for (ptr = savelist; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[ptr];
		val = grolddefv[mvs[grpieces[g]]] * gralprob[g] / 50;	/* 1/03 reduce with gralprob */
		if (val < 0)
			val = 0;
		if (S_ALIVE(s) > ALIVE)
			val /= 2;
		saverule = FALSE;  /* already rule for saving this group? */
		saveval = 0;
		/* use reasons so don't get mixed up as add new reasons to front */
		for (ptr2 = reasons; ptr2 != EOL; ptr2 = link[ptr2]) {
			if ((rules[strat[list[ptr2]].reason].attack & (DEFEND | DEFVAL1 | DEFVAL2)) &&
				((rules[strat[list[ptr2]].reason].attack & PATTERNMV) ||
				 groldarmy[strat[list[ptr2]].param] == groldarmy[mvs[grpieces[g]]])) {
				saverule = TRUE;
				if (strat[list[ptr2]].value > saveval)
					saveval = strat[list[ptr2]].value;
			}
		}
		if (!saverule || val > saveval) {	/* 1/03 bring up to saveval */
			if (inlist(groldarmy[mvs[grpieces[g]]], &oldarmylist))
				continue;
			addlist(groldarmy[mvs[grpieces[g]]], &oldarmylist);
			/* no G_KEYPOINT here since is after the move is made */
			fire_strat_rule(s, SAVE_UNSETTLED_GROUP, val - saveval, mvs[grpieces[g]], 0);
			strat[list[stratreasons[s]]].goodrule = TRUE;
		}
	}
	killist(&oldarmylist);
	for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[armygroups[list[ptr]]];
		val = kval[G_ALIVE(g)] * (armylibs[list[ptr]] + armysize[list[ptr]]);
		if (val > 300)
			val = 300;
		if (S_ALIVE(s) > ALIVE)
			val /= 2;
		killrule = FALSE;  /* already rule for killing this group? */
		/* use reasons so don't get mixed up as add new reasons to front */
		for (ptr2 = reasons; ptr2 != EOL; ptr2 = link[ptr2]) {
			if ((rules[strat[list[ptr2]].reason].attack & (ATKVAL1 | ATKVAL2)) &&
				groldarmy[strat[list[ptr2]].param] == groldarmy[mvs[grpieces[g]]]) {
				killrule = TRUE;
				break;
			}
			if (strat[list[ptr2]].reason == CANT_KILL && 
				groldarmy[strat[list[ptr2]].param] == groldarmy[mvs[grpieces[g]]]) {
				killrule = TRUE;	/* can't count killing since reading says this move doesn't work */
				break;
			}
		}
		if (!killrule) {
			if (inlist(groldarmy[mvs[grpieces[g]]], &oldarmylist))
				continue;
			addlist(groldarmy[mvs[grpieces[g]]], &oldarmylist);
			if (G_ALIVE(g) == DEAD) {
			    fire_strat_rule(s, KILL_UNSETTLED_GROUP, gratkval[g], G_KEYPOINT(g), 0);
				strat[list[stratreasons[s]]].goodrule = TRUE;
			}
			else if (!groldthreatened[mvs[grpieces[g]]]) { /* should have killed it, so no bonus */
			    fire_strat_rule(s, THRT_UNSETTLED_GROUP, val, G_KEYPOINT(g), 0);
				strat[list[stratreasons[s]]].goodrule = TRUE;
			}
		}
	}
	killist(&armylist);
	killist(&savelist);
	killist(&oldarmylist);
}

	
/* return TRUE if rule rl applied to move made at s 
 * rule applied if group made by move is at least as alive as rules[].weakest
 * (unless DEFEND, in which case, group at param must be that alive)
 * AND
 * if rules[].attack & ATTACK, the group at param must have gotten weaker
 * if rules[].attack & DEFEND, the group at param must have gotten stronger
 * if rules[].attack & PATTERN, param is the newpat index for this pattern match
 * if rules[].attack & RUN, param is armyrn_pot and must not have gotten less
 *   or group is alive
 */

int rule_applied(int rl, sqr_t s, int c, int param)
{
	int g;
#ifdef CHECK
	char buf[100];
#endif	

	g = board[s];

#ifdef CHECK	
	if (!(rules[rl].attack & PATTERNMV) && !(rules[rl].attack & RUN) &&
		(s < 0 || s > NOSQUARE) ) {
		sprintf(buf,"rule applied error! rule %d s %d\n",rl,s);
		outerror(buf);
	}
#endif		

	if (rules[rl].attack & PATTERNMV) 
		return FALSE;
	if (!(rules[rl].attack & DEFEND)) {
		if (rules[rl].weakest != DEAD &&
			(c != S_COLOR(s) || G_THREATENED(g) && G_ALIVE(g) > STRONG_KO))
			return FALSE;
		else if (G_ALIVE(g) > rules[rl].weakest)
			return FALSE;
	}
	if ((rules[rl].attack & ATTACK) && 
		param != NOSQUARE && board[param] != NOGROUP && c == S_COLOR(s)) {
		if (checkliferesult(board[param],FALSE) >= V_WINKO)
			return TRUE;
	   	if (groldalive[param] > S_ALIVE(param) ||
			groldalive[param] == S_ALIVE(param) &&
			(S_ALIVE(param) <= ALIVE || gralprob[S_GROUP(param)] < sqoldalprob[param]-20 ||
			 groldrun[param] <= armyrn_pot[S_ARMY(param)] ))
			 return FALSE;
   	}

	if ((rules[rl].attack & DEFEND) && 
		param != NOSQUARE && board[param] != NOGROUP) {
		if (checkliferesult(board[param],TRUE) >= V_WINKO)
			return TRUE;
	   	if (groldalive[param] < S_ALIVE(param))
			return FALSE;
	   	if (groldalive[param] == S_ALIVE(param) && 
			!groldthreatened[param] && 
			S_ALIVE(param) > MIAI && /* 10/96 - defend works if group alive */
			groldrun[param] >= armyrn_pot[S_ARMY(param)]) /* 10/96 defend works if better running room */
	   		return FALSE;
		if (S_THREATENED(param) && rules[rl].weakest != DEAD)
			return FALSE;
		if (S_ALIVE(param) > rules[rl].weakest)
			return FALSE;
	}
	if ((rules[rl].attack & RUN) && 
		(armyrn_pot[G_ARMY(g)] < groldrun[param]  ||
		 armyrn_pot[G_ARMY(g)] == groldrun[param] && groldrun[param] < 8) && 
		gralive[g] > UNSETTLED_LIMP)
		return FALSE;
	if (rl == LIVE_WORKS && S_ALIVE(param) <= ALIVE)
		return FALSE;	/* it lives now, so don't need this bonus */
	if (rl == KILL_WORKS && S_ALIVE(param) >= WEAK_POTENTIAL)
		return FALSE;
	return TRUE;
}


/* generate moves to continue goals specified by goals (move index) */
/* move is index of last move */

list_t obviousgoals(int move, int goals)
{
	list_t ret = EOL,ptr;
	int rule;
	group_t g;
	if (goals == -1 || mvs[goals] == PASS)return(EOL);
	if (mvcolor[move] == mvcolor[goals])return(EOL);
	for (ptr = stratreasons[mvs[goals]]; ptr != EOL; ptr = link[ptr]) {
		rule = strat[list[ptr]].reason;
		if (rules[rule].attack & PATTERNMV)continue;  /* patterns handled elsewhere */
		if (rules[rule].attack & ATTACK ||
		   rules[rule].attack & ATKVAL1 ||
		   rules[rule].attack & ATKVAL2) {
		   g = board[strat[list[ptr]].param];
		   	if (g == NOGROUP)continue;
		   	if (grthreatened[g] && grcapmove[g] != NOSQUARE)
		   		adflist(grcapmove[g],&ret);
	   	}
    }
	return(ret);
}

	
/* last move index is move, what are obvious responses? (best to worst)
 * depth is the remaing full search depth, or negative if in quiescence search
 * ply is the depth of the search since the last actual move
 * goals is index of move whose goals we will continue.
 * if havepattern, have a move from a bigger pattern, so ignore obvious patterns
 * 0) if last move was pass, capture threatened neighbor of next to last move.
 * and return if any above found
 * 1) capture last move if it is tac threatened  (obviouscapture)
 * and return if deep enough
 * 2) save tac threatened group adjacent to last move (obvioussave)
 *    stop if any moves.
 * and return if threatened group is big
 * 2.5) save nearby group that got much weaker
 * 3) capture last move if it is unsettled
      stop if any moves.
 * 4) Capture threatened group adjacent to second to last move (obviousadjacentcap)
 * return if find capturing move
 * 4A) check goals for original move made.  If a goal worked to
 *  attack or defend a group and that group is unsettled now, kill it
 *  or save it.
 * return if any moves generated to this point
 * 5) attack last group if alive is miai or unsettled_limp
 * 6) capture group adjacent to move before last
 * 7) play a move from an obvious pattern match
 * 8) retake ko from 2 or 4 moves ago
 * 9) atari last move played
 * don't add a stone to a dead group
 * return a list of moves in order from presumed best to presumed worst
 */

list_t genobviousmoves(float depth, int ply, int goals, int havepattern)
{
	sqr_t s;
	army_t army;
	int c, size, num;
	group_t best, g;
	list_t movelist = EOL, tmplist = EOL, tmplist2 = EOL, tmplist3 = EOL;
	int ptr,one_nbr,biggroup = FALSE,weaknbr = FALSE;
	int move = msptr-1;
#ifdef CHECK
	char buf[10];
#endif
	s = mvs[move];
	c = 1-mvcolor[move];
	if (s == PASS) {
		movelist = obviousadjacentcap(move, ply); /* capture group adjacent to move before last */
		if (move > 0 && mvs[move-1] != PASS) {
			tmplist = matchtypemoves(OBVIOUSFILE,mvs[move-1],c,c, FALSE);
			ecatlist(&tmplist,&movelist);
		}
		return unflist(movelist);
	}
	if (board[s] == NOGROUP) {
#ifdef CHECK
		outerror("genobviousmoves for empty point ");
		outerror(ssqr(s,buf));
#endif
		return(movelist);
	}

	/* kill last move is tactivally threatened */
	if (S_THREATENED(s) && S_ALIVE(s) < WEAK)
		obviouscapture(board[s],&movelist);

/*	if (-depth > maxscoredepth[playlevel])return(unflist(movelist)); 8/05 - need main search to be superset of q search or unstable */
	/* only do these if in Q search */

	/* save adjacent threatened group, in order of size if more than one */
	cpylist(grnbp[board[s]],&tmplist);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		if (G_ALIVE(list[ptr]) > ALIVE)
			weaknbr = TRUE;
		if (G_THREATENED(list[ptr]) && gralive[list[ptr]] < WEAK) {
			addlist(list[ptr], &tmplist3);
			if (grsize[list[ptr]] > 1 && G_THREATENED(list[ptr]) == 2)biggroup = TRUE;
		}
	}
	killist(&tmplist);
	num = 0;
	while(tmplist3 != EOL) {
		size = 0;
		for (ptr = tmplist3; ptr != EOL; ptr = link[ptr]) {
			if (grsize[list[ptr]]+grlibs[list[ptr]] > size) {
				size = grsize[list[ptr]]+grlibs[list[ptr]];
				best = (group_t)list[ptr];
			}
		}
		dlflist(best, &tmplist3);
		obvioussave(best, &tmplist2);
		mrflist(&tmplist2, &movelist, num);
	}


	/* ignore a ko threat and fill a ko */
	if (kosave[move] != NOSQUARE && board[kosave[move]] == NOGROUP) {
		g = NOGROUP;	/* the single stone in atari */
		for (ptr = nbgrp[kosave[move]][c]; ptr != EOL; ptr = link[ptr]) {
			if (grlibs[list[ptr]] == 1) {
				if (g == NOGROUP)
					g = (group_t)list[ptr];
				else
					g = NOGROUP;
			}
		}
		if (g != NOGROUP)
			for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr])
				if (grlibs[list[ptr]] == 1)
					aeflist(list[grlbp[list[ptr]]], &movelist);
		aeflist(kosave[move], &movelist);
	}
	if (movelist != EOL && biggroup)return(unflist(movelist));  /* if can save, return */

	if (board[s] != NOGROUP) {
		army = S_ARMY(s);
		if (armynbp[army] == EOL)
			getarmynbp(army);
		for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
			if (A_ALIVE(list[ptr]) > ALIVE)
				weaknbr = TRUE;
		}
	}

	/* kill group including last move */
	if (!S_THREATENED(s) && S_ALIVE(s) == UNSETTLED_DEAD) {
		tmplist = justkillmoves(board[s], 20);
		dlflist(kosquare,&tmplist);
		if (tmplist != EOL) {  /* just two moves */
			aeflist(list[tmplist],&movelist);
			if (!weaknbr)
				if (link[tmplist] != EOL)aeflist(list[link[tmplist]],&movelist);
			killist(&tmplist);
			if (!weaknbr)  /* just kill it */
				return(unflist(movelist));
		}
	}

/*	if (movelist != EOL)return(unflist(movelist));   if groups to save, don't try
						 anything else */
						 
	tmplist = obviousadjacentcap(move, ply);  /* capture group adjacent to move before last */
	ecatlist(&tmplist,&movelist);
/*	if (movelist != EOL)return(movelist);  */
    
    if (goals != -1) {
    	tmplist = obviousgoals(move, goals);
    	ecatlist(&tmplist, &movelist);
   	}
 
	if (!havepattern) {
		tmplist = matchtypemoves(OBVIOUSFILE, s, 1-c, c, FALSE);
		ecatlist(&tmplist,&movelist);
	}

	if (move > 0 && mvs[move-1] != PASS) {
		tmplist = matchtypemoves(OBVIOUSFILE, mvs[move-1], c, c, FALSE);
		ecatlist(&tmplist,&movelist);
	}

	/* retake a ko */
	if (move > 1 && kosave[move-1] != NOSQUARE && board[kosave[move-1]] == NOGROUP)
		aeflist(kosave[move-1], &movelist);

	if (!S_THREATENED(s) && 
			(S_ALIVE(s) == UNSETTLED_LIMP || S_ALIVE(s) == RUNNING_FIGHT) && 
			armyrn_pot[S_ARMY(s)] <= OBVIOUS_RUN) {
		tmplist = justkillmoves(board[s], 20);
		dlflist(kosquare,&tmplist);
		if (tmplist != EOL) {  /* just two moves */
			aeflist(list[tmplist],&movelist);
			if (!weaknbr)
				if (link[tmplist] != EOL)aeflist(list[link[tmplist]],&movelist);
			killist(&tmplist);
		}
	}
	if (S_ALIVE(s) == MIAI && armyrn_pot[S_ARMY(s)] <= 4) {
		obviousattack(s,&tmplist);
		ecatlist(&tmplist,&movelist);
	}
	
	/* try an atari forcing move */
	if (S_ALIVE(s) < WEAK && grlibs[board[s]] == 2 && !S_THREATENED(s)) {
		one_nbr = FALSE;
		for (ptr = grnbp[board[s]]; ptr != EOL; ptr = link[ptr])
			if (grlibs[list[ptr]] == 1 || gralive[list[ptr]] == DEAD) {
				one_nbr = TRUE;
				break;
			}
		if (!one_nbr)
			for (ptr = grlbp[board[s]]; ptr != EOL; ptr = link[ptr])
				if ((lnbn[list[ptr]] > 1 || lnbf[list[ptr]][c] != 0 && lnbn[list[ptr]] == 1) &&
						!inflist(list[ptr],&movelist))
					aeflist(list[ptr],&movelist);
	}
	return(unflist(movelist));
}

/* generate move list to capture or save threatened groups adjacent to
 * earlier moves that are in this search, including the move before the search.  
 * move is index of last move.
 */

list_t obviousadjacentcap(int move, int ply) {
	list_t movelist = EOL;
	list_t armylist = EOL;
	int color, ptr, smallest, size;
	sqr_t s;
	army_t barmy = NOARMY;
	
	if (move <= 0)return(EOL);
	while(ply >= 0 && move >= 0) {
		s = mvs[move];
		color = mvcolor[move];
		ply--;
		move--;
		if (s == PASS || s == NOSQUARE)continue;
		if (board[s] == NOGROUP)continue;
		for (ptr = grnbp[board[s]]; ptr != EOL; ptr = link[ptr]) {
			if (G_THREATENED(list[ptr]) && G_ALIVE(list[ptr]) < WEAK ||
				G_ALIVE(list[ptr]) == UNSETTLED_DEAD)
				if ((1 + armysize[G_ARMY(list[ptr])] + armylibs[G_ARMY(list[ptr])])*100 > obaval ||
					S_ALIVE(s) > ALIVE && eyerec[mvs[grpieces[list[ptr]]]] &&
					eyepot[eyerec[mvs[grpieces[list[ptr]]]]] > 0)
				/* only capture groups that are big enough to be interesting */
					addlist(G_ARMY(list[ptr]),&armylist);
		}
		while(armylist != EOL) {  /* sort to get biggest groups in front of movelist */
			smallest = 1000;
			for (ptr = armylist; ptr != EOL; ptr = link[ptr]) { /* find smallest */
				size = armysize[list[ptr]] + armylibs[list[ptr]];
				if (G_THREATENED(list[armygroups[list[ptr]]]) == 2)
					size += 5;
				if (size < smallest) {
					smallest = size;
					barmy = (army_t)list[ptr];
				}
			}
			dellist(barmy,&armylist);
			for (ptr = armygroups[barmy]; ptr != EOL; ptr = link[ptr]) {
				if (color == grcolor[list[ptr]])
					obvioussave((group_t)list[ptr],&movelist);
				else
					obviouscapture((group_t)list[ptr],&movelist);
			}
		}
	}
	return(unflist(movelist));
}

	
/* generate obvious attacking moves for miai group to try to kill it
 * or force it to live
 */

void obviousattack(sqr_t s, list_t *movelist)
{
	list_t tmplist = EOL, tmplist2 = EOL, ptr;
	int count = 0;

	tmplist2 = justkillmoves(board[s], 5);  /* 7/01 5 to avoid the moves that obviously don't work */
	dlflist(kosquare,&tmplist2);
	for (ptr = tmplist2; ptr != EOL && count < 2; ptr = link[ptr]) {
		adflist(list[ptr], &tmplist);
		++count;
	}
	killist(&tmplist2);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] != PASS)
			adflist(list[ptr], movelist);
	}
	killist(&tmplist);
}

/* generate obvious moves which save g (which is threatened or unsettled) */

void obvioussave(group_t g, list_t *movelist)
{
	list_t tmplist = EOL;
#ifdef CHECK
	char buf[80];
#endif	
	if (!G_THREATENED(g)) {
		if (G_ALIVE(g) == UNSETTLED_DEAD) {
			if (armybestmove[G_ARMY(g)] != NOSQUARE &&
				!illegalko(armybestmove[G_ARMY(g)], msptr, AMERICAN))
				adflist(armybestmove[G_ARMY(g)],movelist);
		}
#ifdef CHECK
		else { 
			sprintf(buf,"obvioussave: group not threatened or unsettled - %d!\n",G_ALIVE(g));
			outerror(buf);
		}
#endif
		return;
	}
	rmthreat(grarmy[g], &tmplist);
	catlist(&tmplist, movelist);
	return;
}

/* generate obvious moves to capture g (which is threatened or unsettled) */

void obviouscapture(group_t g, list_t *movelist)
{
	list_t ptr;
#ifdef CHECK
	char buf[80];
#endif	
	if (!G_THREATENED(g)) {
		if (G_ALIVE(g) == UNSETTLED_DEAD) {
			if (armykillmove[G_ARMY(g)] != NOSQUARE &&
				!illegalko(armykillmove[G_ARMY(g)], msptr,AMERICAN))
				adflist(armykillmove[G_ARMY(g)], movelist);
		}
#ifdef CHECK
		else { 
			sprintf(buf,"obviouscapture: group not threatened or unsettled - %d!\n",G_ALIVE(g));
			outerror(buf);
		}
#endif
		return;
	}
	for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr])
		if (G_ALIVE(list[ptr]) == DEAD && grsize[list[ptr]] <= 3)
			return;  /* snapback or similar */
	if (grcapmove[g] != NOSQUARE && 
		!illegalko(grcapmove[g], msptr, AMERICAN))
		adflist(grcapmove[g], movelist);
}

