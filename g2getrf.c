/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* g2getr routines that are called back from children */

#if defined(G2DEBUGOUTPUT) || defined(CHECK)
#include <stdio.h>
#endif

# include "g2hd.h"
# include "g2rldef.h"

extern int fire_ply;

/* return which corner a square is in */
int which_corner(sqr_t s)
{
	int cn = 0;
	if (s == PASS || s == NOSQUARE)return(NOCORNER);
	if (xval[s] * 2 > boardsize)cn += 1;
	if (yval[s] * 2 > boardsize)cn += 2;
	return(cn);
}

int olddefvalue(sqr_t s)
{
	list_t ptr;
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
		if (rules[strat[list[ptr]].reason].attack & DEFVAL1 ||
			 rules[strat[list[ptr]].reason].attack & DEFVAL2)
			return strat[list[ptr]].value;
	}
	return 0;
}
   	
/* has rule fired for square s?
 */

int rule_fired(sqr_t s, int rule)
{
	list_t ptr;
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr])
		if (strat[list[ptr]].reason == rule)
			return TRUE;
	return FALSE;
}


/* add a rule to a square.  Rule fired for square s with
 * additional value val.  param is a value passed to rule_applied() to
 * check if rule satisfied
 * only count best guess for pattern rule.
 * guess is additional guess value
 */

void fire_strat_rule(sqr_t s, int rule, int val, int param, int guess)
{
#ifdef G2DEBUGOUTPUT
	char ebuf[80];
#endif
	list_t ptr;
	int ptr2, guessval;
	if (s >= boardsquare) {
#ifdef G2DEBUGOUTPUT
		sprintf(ebuf,"fire_strat_rule, rule %d, s is %d!\n", rule, s);
		outerror(ebuf);
		turnoffcplay();
#endif
		return;
	}

	/* do guess value first since that's all we need after the first ply */
	guessval = val + rules[rule].value + rules[rule].guess + guess;
	if (rules[rule].attack & PATTERNMV) {  /* only count best pattern guess for guess */
		if (guessval > stratbestpat[s]) {
			stratguess[fire_ply][s] -= stratbestpat[s];	/* this one is bigger, so count it */
			stratbestpat[s] = guessval;
		} else {
			guessval = 0;
		}
	}
	stratguess[fire_ply][s] += guessval;
	if (fire_ply > 0) {
		return;
	}

	if (nextstrat >= NUMSTRATS) {
#ifdef G2DEBUGOUTPUT
		sprintf(ebuf,"Out of strategy records, rule %d!!!\n",rule);
		outerr(ebuf);
#endif
		return;
	}
	strat[nextstrat].reason = rule;
	strat[nextstrat].value = val;
	strat[nextstrat].goodrule = FALSE;
	if ((rules[rule].attack & DEFVAL1) && board[param] != NOGROUP) {
		param = mvs[grpieces[list[armygroups[S_ARMY(param)]]]];
		/* make sure that parameter point is the same to prevent double count */
	}
	strat[nextstrat].param = param;

	if (rules[rule].attack & DEFVAL1) {	/* save the defense value for group */
		for (ptr = armygroups[S_ARMY(param)]; ptr != EOL; ptr = link[ptr])
			for (ptr2 = grpieces[list[ptr]]; ptr2 != -1; ptr2 = mvnext[ptr2])
				if (val > grolddefv[mvs[ptr2]])
					grolddefv[mvs[ptr2]] = val;
	}
	
	if (rules[rule].attack & PATTERNMV) {
		strat[nextstrat].pattern = getnewpatnum(param);
		strat[nextstrat].patsqr = getnewpatsqr(param);
		strat[nextstrat].pato = getnewpato(param);
		strat[nextstrat].patcolor = getnewpatcolor(param);
		strat[nextstrat].patmove = getnewpatmove(param);
	} else {
		strat[nextstrat].pattern = 0;
	}
	strat[nextstrat].guess = guessval;  /* only for debug display */
	adflist(nextstrat, &stratreasons[s]);
	nextstrat++;
}
