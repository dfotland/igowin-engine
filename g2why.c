/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2hd.h"
#include "g2proto.h"
#include "g2rldef.h"
#include "g2look.pr"
#ifdef UCT
#include "uct.h"
#endif
#include <stdio.h>
#include <string.h>

#define NUMANUM 20

/* TRUE if move was considered */

int considr(sqr_t s) {
        return(scoreval[s] != BIGNUM);
        }
void markagroup(group_t g, char *c) {
	int ptr;
	for (ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr])
			outstone(mvs[ptr],c);
	}
	
#ifndef SMALLCODE

extern int randomval[];
        
void
printreasons(sqr_t s, int c)
{
	list_t ptr, ptr2, tmplist=EOL;
	int flag, oflag = 0, newreas, i;
	sqr_t s2;
	group_t mgroup;
	char buf[200],buf2[200],*pt,tmp[40];
	int armynums[NUMANUM];
#ifdef G2DEBUGOUTPUT
	int p;
#endif	
/*	unhighlight(); */
	for (s2 = 0; s2 < boardsquare; ++s2) {
		if (considr(s2)) {
			histone(s2);
			oflag++;
		}
	}
	for (i = 0; i < NUMANUM; ++i)
		armynums[i] = -1;
	if (s == PASS) {
		outerr(getstring(25,74));
		outerr("\n");
		return;
	}
#ifdef UCT
	if (last_search_level == UCTLEVEL) {
		sprintf(buf, "After %d pseudorandom games, this move had the highest winning percentage.\n", uct_get_playout_count());
		outerr(buf);
		if (stratreasons[s] == EOL)
			return;
	}
#endif
	ptr = stratreasons[s];
	if (ptr == EOL) {
		outerr(getstring(25,75));
		outerr("\n");
	}
	else {
		for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
			if (list[tmplist] != list[ptr])
				adflist(list[ptr],&tmplist);
		}
#ifdef G2DEBUGOUTPUT
		if (debug) {
			if (!considr(s))
				outerr("Move not considered.\n");	
			sprintf(buf,"Guess is %d (with %d random).  obaval %d, sumstrat %d, stratvalue %d, savepat %d ",
				stratguess[0][s], randomval[s], obaval, sumstrat(s, c), stratvalue[s], stratpatvalue[s]);
			outerr(buf);
			sprintf(buf,"Move value %d (%d points).\n",scoreval[s],scoreval[s]/50);
			outerr(buf);
		}
#endif			
		flag = FALSE;
		for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
			newreas = TRUE;
			if (!debug && /* eliminate duplicates */
				!(rules[strat[list[ptr]].reason].attack&(ATTACK|DEFEND)))
				for (ptr2 = tmplist; ptr2 != ptr; ptr2 = link[ptr2])
					if (strat[list[ptr2]].reason == strat[list[ptr]].reason)
						newreas = FALSE;
			if (!newreas)
				continue;
			if (strat[list[ptr]].goodrule || debug || last_search_level == UCTLEVEL) {
				strcpy(buf,getstring(WHYPAGE,strat[list[ptr]].reason));
				if (rules[strat[list[ptr]].reason].attack&(ATTACK|DEFEND|ATKVAL1|DEFVAL1|NAMEGROUP)) {
					mgroup = S_GROUP(strat[list[ptr]].param);
					for (i = 0; i < NUMANUM-1; ++i)
						if (armynums[i] == mgroup || armynums[i] == -1)break;
					armynums[i] = mgroup;
					pt = (char *)memchr(buf,'%',strlen(buf));
					if (pt != NULL)
						*pt = 'A'+i;
					sprintf(tmp,"%c",'A'+i);
					markagroup(mgroup,tmp);
				}
				if (!debug) {
					outerr(buf);
					outerr("\n");
				}
				else {
#ifdef G2DEBUGOUTPUT					
					p = strat[list[ptr]].param;
					if (rules[strat[list[ptr]].reason].attack & PATTERNMV)
						p = strat[list[ptr]].pattern;
					sprintf(buf2,"%d %d %d+%d G%d[%d] %d - %s",
						strat[list[ptr]].goodrule,
						strat[list[ptr]].reason,
						strat[list[ptr]].value,
						rules[strat[list[ptr]].reason].value,
						strat[list[ptr]].guess,
						rules[strat[list[ptr]].reason].guess,
						p,

						buf);
#endif						
					outerr(buf2);
					outerr("\n");
				}
				flag = TRUE;
			}
		}	
		if (!flag)
			outerr(getstring(25,76));	
	}
	killist(&tmplist);	
	if (oflag > 1) {
		outerr(getstring(27,50));
		outerr("\n");
	}

}
#endif /* SMALLCODE */

