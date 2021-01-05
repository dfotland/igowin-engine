/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2hd.h"
#include "g2rldef.h"
#include "g2getr.pr"
#include "learn.h"

//int foundjoseki[5];  /* found a joseki or kakari or shimari move by corner */
//int foundjosfol[5];

int fire_ply;		/* depth for reasons and guess values */

void finish_reasons(void)
{
	fire_ply = 0;
}

void initreasons(int ply, int passval)
{
	sqr_t s;
	fire_ply = ply;
	setdefvals();
	setatkvals(passval);
	initjosreasons();
	for (s = firstsquare; s < lastsquare; ++s) {
		stratguess[ply][s] = 0;
		stratbestpat[s] = 0;
	}
	if (ply > 0) {
		return;
	}
	nextstrat = 0;
	for (s = firstsquare; s < lastsquare; ++s) {
		stratgoodreasons[s] = FALSE;
		scoreval[s] = BIGNUM;
		killist(&stratreasons[s]);
		if (board[s] != NOGROUP) {
			groldalive[s] = S_ALIVE(s);
			sqoldalprob[s] = gralprob[S_GROUP(s)];
			groldthreatened[s] = S_THREATENED(s);
			groldrun[s] = armyrn_pot[S_ARMY(s)];
			groldarmy[s] = S_ARMY(s);
			grolddefv[s] = 0;
		}
		else {
			groldalive[s] = 0;
			sqoldalprob[s] = 0;
			groldthreatened[s] = FALSE;
			groldrun[s] = 0;
			groldarmy[s] = NOARMY;
			grolddefv[s] = 0;
		}
	}
}

/*
 * get counts for children in fuseki library and prefer moves that are more popular.
 */
void fuseki(int color)
{
	struct learn_mv lm[361];
	int strongest, wins[3];
	int i, baseval = 0, stval, val;
	int mostpop = 1;
	int game;
	int count;
	
	if (msptr == 0)
		return;

	count = getfuseki(color, lm, &strongest, wins, &game);

	for (i = 0; i < count; ++i) {
		if (lm[i].count > mostpop) {
			mostpop = lm[i].count;
		}
	}

	if (playlevel < FUSEKILEVEL)
		baseval = 50;
	else if (mostpop > 100)
		baseval = 6 * 50;
	else if (mostpop > 10)
		baseval = 4 * 50;
	else if (mostpop > 2)
		baseval = 2 * 50;
	else if (mostpop == 2)
		baseval = 1;
	else
		baseval = 0;

	if (playlevel < FUSEKILEVEL)
		stval = 2;
	else if (strongest > 10)	/* professional */
		stval = 5;
	else if (strongest > 0)  /* dan */
		stval = 3;
	else
		stval = 1;

	for (i = 0; i < count; ++i) {
		if (lm[i].count < mostpop / 20) {
			continue;
		}
		if (lm[i].count <= 3 && wins[color] == 0) {
			continue;	/* don't play moves that lost before */
		}
		val = baseval + stval * 50 * lm[i].count / mostpop;
		if (rule_fired(lm[i].s, PLAY_IN_EMPTY_CORNER))
			val /= 2;
		fire_strat_rule(lm[i].s, FUSEKI_LIB, val, 0, 0);
	}
}
  
/* get all the reasons for moves by color and attach them to squares using 
 * stratreasons[].  return 0 if there are no dame moves and a poass is allowed
 */

int get_reasons_for_moves(int passval, int color, int handicap, int rules, int randomize)
{
	int numdame = 1;
	life(FALSE);
	initreasons(0, passval);
	if (problemflag != SOLVEPROBLEM) {
		if (empty_corner(color, randomize, handicap)) {
			return 1;
		}
	}

	killgroup(passval, color);				/* attack enemy group */
									/* before r4t so savegroup gets benefit of analysis */
	savegroup(passval, color, 0);				/* save weak group */
	cut_connect(color);			/* cut or connect */
	genpatmoves(color, passval);                /* look for patterns to suggest moves */
									/* after attack or def moves */
	if (problemflag != SOLVEPROBLEM) {
		fuseki(color);				/* moves from fuseki library */
		if (playlevel >= SHIMARILEVEL)
			shimari_kakari(color);	/* play shimari and kakari */
		if (playlevel >= JOSEKILEVEL)
			joseki(color);       	/* play joseki moves (unless already got by shimari_kakari) */
		extend(color);			/* extend along edge */
		center(color);			/* play in center */
		if (playlevel >= SAFELEVEL && ahead > 2 && ((signed)msptr < boardsquare || mvs[msptr-1] != PASS))
			play_safe(color,handicap,passval);       /* ahead, so play it safe */
										/* no need to play safe if he just passed */
		if (playlevel >= SQUIRMLEVEL && ahead < 2 && phase != FUSEKI)
			squirm(passval, color);               /* behind, so try to stage upset */
		}
	block_move(color);			/* block when enemy stones diagonal */
	if (playlevel >= KOLEVEL && kosquare != NOSQUARE)		/* make ko threat */
		kothreat(color);
#ifndef DEMO      
	if (phase != FUSEKI || problemflag == SOLVEPROBLEM || problemflag == SOLVEFULLPROBLEM || playlevel < 4)
#endif   
		numdame = fill_dame(color);				/* fill dame in endgame */
	if (rules == CGOS && phase == ENDGAME)
		numdame += remove_dead_stones(color); /* take dead stones off board */
	obvious_answer(0);
	return numdame;
}

/*
 * get reasons for moves during search for color to move
 * positive depth is main search.  negative or zero depth is quiescence
 */
void get_search_reasons(int color, int handicap, int rules, double depth, int ply)
{
	int passval;
	passval = getcurrenteval(color, handicap, rules);   /* score after pass */
	initreasons(ply, passval);
	if (empty_corner(color, FALSE, handicap)) {
		return;
	}
	killgroup(passval, color);				/* attack enemy group */
									/* before r4t so savegroup gets benefit of analysis */
	savegroup(passval, color, ply);				/* save weak group */
	cut_connect(color);			/* cut or connect */
	genpatmoves(color, passval);                /* look for patterns to suggest moves */
	fuseki(color);
	shimari_kakari(color);	/* play shimari and kakari */
	joseki(color);       	/* play joseki moves */
	extend(color);			/* extend along edge */
	center(color);			/* play in center */
	obvious_answer(ply);		/* have to have obvious answer or get early passes */
	if (playlevel >= KOLEVEL && kosquare != NOSQUARE)		/* make ko threat */
		kothreat(color);
	fill_dame(color);				/* fill dame in endgame */
	finish_reasons();
	return;
}



