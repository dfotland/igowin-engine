/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* play_safe getr code */

# include "g2hd.h"
# include "g2rldef.h"
# include "g2getr.pr"

	/* generate best move to stop running */
	
static sqr_t stoprunmove(army_t army, sqr_t s) {
	list_t ptr;
	int c,open = -1,val,numfriends = 0,numenemy = 0;
	sqr_t best;
	
	if (S_NEUTRAL(s))return(s);
	best = s;
	c = A_COLOR(army);
	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
		val = lnbn[list[ptr]]*2;
		if (edge[list[ptr]] >= 3 &&
			S_COLOR(s+s-list[ptr]) == c)
				val++;
		if (val > open) {
			best = list[ptr];
			open = val;
			}
		if (lnbf[list[ptr]][c] == 0 && lnbf[list[ptr]][1-c] != 0)
			numfriends++;
		if (lnbf[list[ptr]][c] != 0)
			numenemy++;
		}
	if (numenemy == 0 && numfriends && ld[s] == 2)
		return(s);
	return(best); 
	}


/* play safe and capture army */
	
static void safe_capture(army_t army, int passval) {
	list_t ptr, gptr, lptr, ptr3, movelist, tmplist = EOL, t2 = EOL;
	int val, i, urg, extra; 
	sqr_t move;
	
	urg = FALSE;
	val = safecaptval(army, passval);
	if (urg)val += 300;
	if (A_ALIVE(army) == DEAD) {
		movelist = getatkmoves((group_t)list[armygroups[army]]);
		extra = 30;	/* to give more value to better moves */
		for (ptr = movelist; ptr != EOL; ptr = link[ptr]) {
			fire_strat_rule(list[ptr], SAFE_CAPTURE, val+extra+20*lnbn[list[ptr]], NOSQUARE, 0);
			extra -= 10;
		}
		killist(&movelist);
		return;
	}
	if (G_THREATENED(list[armygroups[army]])) {
		for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
			t2 = th_cap_moves((group_t)list[ptr]);
			for (ptr3 = t2; ptr3 != EOL; ptr3 = link[ptr3])
				addlist(list[ptr3], &tmplist);  /* since t2 is not sorted */
			killist(&t2);
		}
		for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
			/* safecaptval already included in capture threatened group atack value */
			fire_strat_rule(list[ptr], CAPTURE_SAFE, 0, NOSQUARE, 0);
		}
		killist(&tmplist);
		return;
	}
	if (armyrn_pot[army] == 0 && armyeyepotential[army] == 0) {
		for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
			fire_strat_rule(list[ptr], SAFE_CAPTURE, val+25*lnbn[list[ptr]], NOSQUARE, 0);
			if (A_NUMLIBS(army) > 1 && lnbn[list[ptr]] == 0) {
				for (gptr = nbgrp[list[ptr]][1-A_COLOR(army)]; gptr != EOL; gptr = link[gptr]) {
					if (grlibs[list[gptr]] > 2) continue;
					for (lptr = grlbp[list[gptr]]; lptr != EOL; lptr = link[lptr]) {
						if (list[lptr] == list[ptr]) continue;
						fire_strat_rule(list[lptr], SAFE_CAPTURE, val+25, NOSQUARE, 0);
					}
				}
			}
		}
		if (armynbp[army] == EOL)getarmynbp(army);		
		for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr])
			if (armylibs[list[ptr]] == 1 && !inlist(list[armylbp[list[ptr]]], &armylbp[army]))
				fire_strat_rule(list[armylbp[list[ptr]]], SAFE_CAPTURE, val+25*armysize[list[ptr]], NOSQUARE,0);
		return;				
	}
	if (armyrn_pot[army] != 0) {
		for (i = 0; i < NUMRUN; ++i) {
			if (armyrun[army][i] == EOL)continue;
			for (ptr = armyrun[army][i]; ptr != EOL; ptr = link[ptr]) {
				move = stoprunmove(army,list[ptr]);
				fire_strat_rule(move, SAFE_CAPTURE, val+50, NOSQUARE, 0);
			}
			break;
		}
		return;
	}
	movelist = justkillmoves((group_t)list[armygroups[army]], 10);
	for (ptr = movelist; ptr != EOL; ptr = link[ptr])
		if ((sqr_t)list[ptr] != PASS && (sqr_t)list[ptr] != NOSQUARE && (sqr_t)list[ptr] != kosquare)
			fire_strat_rule(list[ptr], SAFE_CAPTURE, val+50, NOSQUARE, 0);
	killist(&movelist);
}

/* play safe and make weak conections stronger */

static void safe_connect(int color) {
	int cn, val;
	group_t g1, g2;
	for (cn = 0; cn < NUMCONNS; ++cn) {
		if (cncnum[cn] == 0 && cnlknum[cn] == 0 && cnllnum[cn] == 0 && cnollnum[cn] == 0)
			continue;
		g1 = cngr1[cn];
		g2 = cngr2[cn];
		if (G_COLOR(g1) != color)continue;
		if (cntype[cn] == CN_THREAT) {
			val = (grsize[g1] + grsize[g2]) * 10;
			if (cnptr[cn] != EOL)
				fire_strat_rule(list[cnptr[cn]], SAFE_CONNECT, val, NOSQUARE, 0);
			}
		}
	}


/* play safe when you are ahead. color is side to move 
 * saving safe group is in savegroup()
 */

void play_safe(int color, int handicap, int passval) {
	group_t g;
	list_t ptr,caplist = EOL;

	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])continue;

        if (safecaptarmy(grarmy[g],color,handicap))
        	addlist(grarmy[g],&caplist);
	}

	for (ptr = caplist; ptr != EOL; ptr = link[ptr])
		safe_capture((army_t)list[ptr], passval);

	safe_connect(color);

	killist(&caplist);

	}
