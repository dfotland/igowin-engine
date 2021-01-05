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
#include "g2tt.h"
#include "uct.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32
#include <windows.h>
#else
static int GetTickCount(void)
{
	return 1000 * time(NULL);
}
#endif

extern int rand(void);

#ifdef WIN32
extern int timecontrol;
#endif
extern int maxiterations[NUMLEVELS], maxlifecalls[NUMLEVELS], numcmoves[4];
extern int senteattack[NUMALIVE];
extern char maxscoredepth[21];
#ifdef TEST
int testmode = FALSE;
#endif

#ifdef G2DEBUGOUTPUT
int savedebugalive[NUMSQUARES];
int savedebugbest[NUMSQUARES];
int savedebugrun[NUMSQUARES];
extern FILE *logfile;
extern int logsearch;
#endif

int cfac[3] = { -1,1,0 };

int numlifecalls;  /* number of life/death evaluations */
int numsearchnodes;	/* negamax calls */
int numsearchevals;
int numfullgen;		/* move generation */
int numqgen;
int numtacnodes;	/* number of tactical moves made */
int numgrtacnodes;	/* tactical nodes in group evaluation */
int nummiaitacnodes;
int numtvtacnodes;
int numcntacnodes;
int numeyetacnodes;
int numlifetacnodes;
int numopjtacnodes;
int numfixgrtacnodes;
int debugnumlifecalls;   /* life calls for real */
int totstablenodes = 0;
int randomval[363];  /* random value to add in getscore() */
int maxlistused = 0;
int maxtreeused = 0;

extern int senteattack[NUMALIVE],attackable[NUMALIVE];


/* Return bonus for sente attacks on groups that will live, or other aji 
 * bonus should be less than the actual amount that can be gained.
 * value of answering must be greater than 2*oba, since the options are:
 * attack, defend, oba, or
 * attack, 2*oba, capture, oba
 * no bonus for attacking threatened groups, since they can't defend.
 * return in big the value of the biggest group that can be saved or killed 
 */

int groupaji(int *big)
{
	list_t tmplist=EOL;
	group_t g;
	int val = 0, size;
	int bigval;
	army_t army;
	if (problemflag == SOLVEPROBLEM)
		return 0;
	*big = 0;
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])continue;
		army = G_ARMY(g);
		if (!addlist(army, &tmplist))continue;

		armyaji[grarmy[g]] = 0;
		/* can a group that will live be attacked, gaining some points in sente when it defends itself? */
		if (attackable[G_ALIVE(g)] && !G_THREATENED(g)) { 
			size = armysize[army] * 100 + armylibs[army] * 50;
			if (size > obaval*3)	/* full value */
				armyaji[army] = -senteattack[A_ALIVE(army)] * cfac[grcolor[g]];
			else if (size > obaval)	/* partial value, zero at obaval to full value at obaval*3  */
				armyaji[army] = -senteattack[A_ALIVE(army)] * cfac[grcolor[g]] *
									(size-obaval)/(obaval*2);
		}
		/* barely alive group needs a defensive move eventually */
		else if (G_ALIVE(g) >= BARELY_ALIVE && G_ALIVE(g) <= ALIVE) {
			armyaji[army] = -50 * cfac[grcolor[g]];
		}
		else {
			/* less than full value to allow reading to correct mistakes */
			bigval = (armysize[army] + armylibs[army]) * (gralval[g][0] - gralval[g][1]) / 4;
			if (bigval > *big)
				*big = bigval;
		}
		val += armyaji[army];
	}
	killist(&tmplist);
	return(val);
}


/* see if we have any tactically threatened, unsettled,
 * or weak groups on the board 
 * Penalty for unsettled and weak groups is reducing value of
 * stone to -40.  Liberty values are not reduced.
 * except that the side to move gets his biggest new unsettled group saved
 * (gets 40 added to his score for each piece and liberty)
 * if unsettled group is also tactically threatened, add another 50 points
 * per liberty since will lose those points too containing it.
 * c is color to move
 * if we have tactically captured groups inside very weak groups, they are 
 * actually alive so add back two points per stone and liberty
 * if the total weak and threatened groups is small, use obaval.
 */

/* adjust for dead groups inside other dead groups positive good for white */
int deadscr(void)
{
	int dval = 0;
	list_t ptr;
	group_t g;
	for (g = 0; g < maxgr; g++) {
		if (!grlv[g])continue;
		if (G_ALIVE(g) == DEAD && deadinsidedead(g)) {
			dval += grsize[g] * (50-gralprob[g])*cfac[grcolor[g]];
			for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr])
				dval += (50*cfac[grcolor[g]]-terv[list[ptr]]);
		}
	}
	return dval;
}

/* 
 * best estimate of the final score, higher better for color (side to move), in 50ths of a point 
 */

int getcurrenteval(int color, int handicap, int rules)
{
	int scrtmp;
	life(FALSE);					/* update the group strengths and territory - REQUIRED! */
	dscr = deadscr();				/* adjust for dead stones inside dead stones */
	ajiscr = groupaji(&bigscr);
	scrtmp = pscr + tscr + dscr + ajiscr + komi + passscr; /* piece value */
	if (rules == CHINESE)
		scrtmp += 50 * handicap;	/* chinese rules don't give a point for a handicap stone - GOE counts points for handicaps */
	if (handicap  && (rules == JAPANESE || rules == AMERICAN))
		scrtmp += 50 * (handicap - 1);	/* american and japanese rules don't give a point for a handicap stone (but off by 1 since white moves first and last) */
	if (color == BLACKCOLOR)
		scrtmp = -scrtmp;
	if (bigscr > 2 * obaval)		/* make a big attack/capture/save first */
		scrtmp += bigscr - 2 * obaval;
	scrtmp += obaval;		/* side to move gets 1/2 of the score for big move */
	return scrtmp;
}

/* don't look at bad moves */

int isbadmove(sqr_t s)
{
/* skip illegal moves */

	if (illegalko(s,msptr,AMERICAN)) {
		fire_strat_rule(s, ILLEGAL_KO, BAD_MOVE,NOSQUARE, 0);
		return(TRUE);
	}
	if (board[s] != NOGROUP) {
		fire_strat_rule(s, ILLEGAL_STONE, BAD_MOVE, NOSQUARE, 0);
		return TRUE;
	}

/* skip bad moves */

/* skip nonjoseki moves if found joseki move in corner */
	if (notjoseki(s)) {
		fire_strat_rule(s, NOT_JOSEKI, BAD_MOVE, NOSQUARE, 0);
		return TRUE;
	}
			
	return FALSE;
}

sqr_t movestotry[NUMSQUARES];
sqr_t oppmoves[NUMSQUARES];		/* opponent moves and values for lookahead */
int oppmoveval[NUMSQUARES];
/*int nextoppmove = 0; */
int nextmvtotry;  /* total number of moves suggested */

int sortguess(const void *e1, const void *e2) {
	int g1 = stratguess[0][*(sqr_t*)e1];
	int g2 = stratguess[0][*(sqr_t*)e2];
	if (g1 > g2)return -1;
	if (g1 < g2)return 1;
	return 0;
}

int sortscore(const void *e1, const void *e2) {
	int g1 = scoreval[*(sqr_t*)e1];
	int g2 = scoreval[*(sqr_t*)e2];
	if (g1 > g2)return -1;
	if (g1 < g2)return 1;
	return 0;
}

int resign_limit[NUMLEVELS] = 
{
	40, 40, 35, 30, 25, 20, 20
};

/*
 * return true if color should resign now
 */
int should_resign(int color, int rules)
{
	int score, passval;
	sqr_t s;
	int best = 0;
	int limit = resign_limit[playlevel];		/* how far behind to resign */

	if ((int)msptr < boardsquare / 2) {
		return FALSE;
	}

	if (mvs[msptr-1] == PASS) {
		return FALSE;
	}
	if (boardsize < 13) 
		limit = limit * 2 / 3;
	score = getcurrenteval(color, 0, rules);
	if (score > -50 * limit) {
		return FALSE;
	}

	getobaval();
	passval = strategy(color, 0, rules); 
	get_reasons_for_moves(passval, color, 0, rules, FALSE);

	for (s = 0; s < boardsquare; ++s) {
		if (stratguess[0][s] > best) {
			best = stratguess[0][s];
		}
	}
	return score + best * 3 < -50 * limit;
}
	
/* return first move to sort */

static int getmovestotry(int randomize, int allowpass, int color, point_t tryfirst, int skip_count, point_t *skip_points)
{
	sqr_t s;
	int first = 0;
	int skip;
	int i;

	nextmvtotry = 0;
	if (tryfirst != PASS && tryfirst != NOSQUARE) {
		movestotry[nextmvtotry++] = tryfirst;
		first = 1;
	}
	if (allowpass) {
		movestotry[nextmvtotry++] = PASS;
	}
	for (s = 0; s < boardsquare; ++s) {
		if (stratreasons[s] == EOL || s == tryfirst) {
			continue;
		}
		if (board[s] != NOGROUP) {
#ifdef G2DEBUGOUTPUT
			outerror("move reason for nonempty point");
#endif
			continue;
		}
		skip = FALSE;
		for (i = 0; i < skip_count; ++i) {
			if (s == skip_points[i]) {
				skip = TRUE;
				break;
			}
		}
		if (skip) {
			continue;
		}

        stratguess[0][s] += randomval[s];
		if (stratguess[0][s] >= 0 && stratreasons[s] != EOL && !isbadmove(s)) {  /* not tried yet 7/03 - don't try negative values */
			movestotry[nextmvtotry++] = s;
		}
	}
	return first;
}

/* by handicap, 0 to 9 */
int erunb[10] ={  
	EASY_RUN, EASY_RUN, EASY_RUN, EASY_RUN + 1, EASY_RUN + 2, EASY_RUN + 3, EASY_RUN + 3, EASY_RUN + 3, EASY_RUN + 3, EASY_RUN + 3,
};

int erunw[10] = { 
	EASY_RUN, EASY_RUN - 1, EASY_RUN - 2, EASY_RUN - 3, EASY_RUN - 4, EASY_RUN - 5, EASY_RUN - 5, EASY_RUN - 5, EASY_RUN - 5, EASY_RUN - 5, 
};

/* set the defensive values for each group 
 * this is a value based on the strengthes and size of the group
 * a value based on the groups susceptibility to being surrunded
 * measures in some sense the number of points the enemy gets by attacking this group
 * and will be added as a bonus to moves that defend this group
 */
void setdefvals()
{
	army_t a;
	int def;
	list_t ptr;
	for (a = 0; a < NUMARMIES; ++a) {
		if (armygroups[a] == EOL) {
			continue;
		}
		def = def_val(a, gralval[list[armygroups[a]]][1])/2;  /* 1/07 reading can figure this out so reduce bonus */
		if (playlevel < ATKDEFLEVEL) {
			def /= 10;
		}
		for (ptr = armygroups[a]; ptr != EOL; ptr = link[ptr]) {
			grdefval[list[ptr]] = def;
		}
	}	
}


/* set the attack values for each group 
 * this is a value based on the strengthes and size of the group
 * a value based on the groups susceptibility to being surorunded
 */
void setatkvals(int passval)
{
	army_t a;
	int atk;
	list_t ptr;
	for (a = 0; a < NUMARMIES; ++a) {
		if (armygroups[a] == EOL)
			continue;
		atk = atk_val(a, gralval[list[armygroups[a]]][0], passval)/2;  /* 1/07 - reading can find this more accuarately, so divide bonus by 2 */
		if (playlevel < ATKDEFLEVEL)
			atk /= 10;
		for (ptr = armygroups[a]; ptr != EOL; ptr = link[ptr]) {
			gratkval[list[ptr]] = atk;
		}
	}	
}

/* strategy figures out strategic concepts that wil be used later in
 * evaluating moves.
 * it sets ahead to show if side to move is ahead or behind
 * it sets phase showing which pase the game is in.
 * color is side to move
 * it sets aggressiveness (0-10) (5 is neutral, 0 is defensive, 10 is agressive)
 * based on handicap and how far ahead
 * returns current score
 * THERE"S SOME STRANGE CODE HERE!
 */
int strategy(int color, int handicap, int rules)
{
	int i,val,h;
	group_t g;
	phase = -1;

	val = getcurrenteval(color, handicap, rules);

	h = handicap;
	if (h > 9)
		h = 9;
	easyrun[0] = erunb[h];
	easyrun[1] = erunw[h];
	agressiveness = 5;
	if (color == WHITECOLOR)
		agressiveness = 5 + h / 2;
	else
		agressiveness = 5 - h / 2;
	agressiveness -= val / 1000;  /* adjust by 1 for each 20 points */
	if (agressiveness < 0)
		agressiveness = 0;
	if (agressiveness > 10)
		agressiveness = 10;
	
	for (i = 0; i < 4; ++i) {
		if (jflag[i] == 1 || numcmoves[i] <= 2 && boardsize >= 13 ||
			numcmoves[i] <= 1) {
			phase = FUSEKI;
			break;
		}
	}

	if (phase == -1 && (sqr_t)msptr > boardsquare/3) {
		phase = ENDGAME;
		for (g = 0; g < maxgr; ++g) {
			if (!grlv[g])
				continue;
			if (G_ALIVE(g) > ALIVE && G_ALIVE(g) < WEAK_SEMEAI &&	/* 11/08 change from WEAK to WEAK_SEMEAI since sometimes semeai misevaluated */
			   grsize[g] + grlibs[g] > 5) {
				phase = MIDDLE;
				break;
			}
		}
	}
	if (phase == -1)
		phase = MIDDLE;

	ahead = 2;  /* even score */
	if (val > 2000)
		ahead = 4;  /* way ahead */
	else if (val > 1000)
		ahead = 3;  /* ahead */
	else if (val < -1000 && val < -1000)
		ahead = 0;  /* way behind */
	else if (val < -500 && val < -300)
		ahead = 1;  /* behind */
	else if (phase == ENDGAME && val < -obaval)
		ahead = 1;
	if (boardsize == 9) {
		if (val > 1000)
			ahead = 4;
		if (val > 500)
			ahead = 3;
		if (val < -500)
			ahead = 0;
	}
	setdefvals();				/* now with accurate reading results, can set atk and def values */
	setatkvals(val);
	return val;
}

int compmoveinprogress = FALSE;

/* 
 * search for the top num_moves moves
 */
g2status compmove_multi(int random, int color, int handicap, int rules, int *num_moves, point_t *points, int *values, unsigned int time_left, int level)
{
	int i;
	int stat;
	int num = *num_moves;
	*num_moves = 0;
	for (i = 0; i < num; ++i) {
		stat = compmove(random, color, handicap, rules, &points[i], PASS, time_left, num, level, FALSE, i, points, FALSE, 1, FALSE); 
		values[i] = scoreval[points[i]];
		if (stat != G2OK && stat != G2RESIGN) {
			return stat;
		}
		(*num_moves)++;
	}
	return G2OK;
}

/* compmove generates a computer move for color.  returns G2OK if it found
 * a move.  G2FAIL if there was an internal error and no move was generated
 * G2NOROOM if there is not enough data space to safely make a move
 * The move returned is NOPOINT to indicate a resignation
 * randomize causes the computer to randomize it's moves
 * move is returned in *retmove.
 *
 * time_left is in milliseconds
 * stones_left is 0 for full game, or the number of stones to the next time control (in overtime)
 *
 * tryfirst will be sorted to the front of the moves to try.  Set it
 * to PASS to avoid the sort.  Use this to analyze a particular move
 * since it will be tried first, it will get an accurate score.  Can
 * look at score[] or use why to get analysis.
 *
 */
int savepassval;  /* for debug output only */
int savebestval;  /* for debug output only */
int savestartval;  /* for debug out, territory val before move */

/* penalty for nonurgent move, when there is already an urgent move found */
#define NOTURGVAL (-250)

extern int montecarlo;
int last_search_level;

#define MAXPVDEPTH 50
#ifdef SHOW_LOOKAHEAD

extern void sendlookahead(sqr_t *, int);

#endif

/* amount of random variation per level as a function of obaval */
extern double randomness[NUMLEVELS];

g2status compmove(int randomize, int color, int handicap, int rules, sqr_t *retmove, sqr_t tryfirst, unsigned int time_left, unsigned int stones_left, int level, int showlookahead, int skip_count, sqr_t *skip_points, int using_mpi, int mpi_ranks_per_group, int allow_resign)
{
#ifdef G2DEBUGOUTPUT	
	char buf[500];
	char buf2[500];
	double secs;
	char buf3[10];
	int lifenodes;
	int badmove;
	int oldpscr;
	int listused, treeused;
#endif
	list_t ptr;
	sqr_t s;
	group_t g;
	int scr, goodmove;
	unsigned int starttime, endtime;	// time in ms
	int nm;
	int maxlc;  /* maximum number of life calls total */
	int moves;  /* good moves (good reason, better than pass) tried so far */
	int alpha;  /* best score for side to move */
	int passval;  /* value of a pass - higher is better for side which is passing */
	sqr_t bestsqr = PASS;  /* square of best move from complete iteration */
	sqr_t newbestsqr = PASS;  /* square of best move from current iteration */
	int besturgscore;
	int beta;
	int iter;
	int first = 0;
	int startlifecalls;
	unsigned int used;	/* time used */
	int timeout = FALSE;
	unsigned int stoptime = 0;
	int allowpass;
	int newscoreval[NUMSQUARES];	/* incomplete scoreval during an iteration */
	int newbest;	/* is there a new best move this iteration */
	int randlimit;
	int stopearly;
	int stones;
	float win_rate;
#ifdef SHOW_LOOKAHEAD
	sqr_t pv[MAXPVDEPTH];  /* pv being displayed */
	int next_pv = 0;
#endif

	unsigned int target_time, max_time;	/* time for this move, in ms */

	if (level < MAXLEVEL) {
		randomize = TRUE;
	}

	last_search_level = level;
	if (level == UCTLEVEL || montecarlo) {
		*retmove = uct_compmove(color, handicap, rules, time_left, stones_left, showlookahead, using_mpi, mpi_ranks_per_group, &win_rate);
		if (allow_resign && 
			(win_rate < 0.05 || win_rate < 0.25 && should_resign(color, rules))) {
			return G2RESIGN;
		}
		return G2OK;
	}

	if (problemflag)
		randomize = FALSE;
	if (noroomleft()) {
		*retmove = PASS;
		compmoveinprogress = FALSE;
		return G2NOROOM;
	}
#ifdef CHECK
	if (!dscheck(FALSE)) {  /* check data structures for consistency */
		turnoffcplay();
		*retmove = PASS;
		return G2FAIL;
	}
#endif

	/* figure out the time limits and levels */
	if (stones_left > 0) {
		/* in overtime */
		target_time = (time_left * 4 / 5) / stones_left;
		max_time = target_time * 5 / 4;
	} else {
		stones = (boardsize * boardsize * 3 / 4 - (int)msptr) / 2;   /* approx number of my moves left in the game - enough for 270 move 19x19 game (135 for me) */
		if (stones < boardsize * boardsize / 4) {
			stones = boardsize * boardsize / 4;				/* minimum 72 moves left for me */
		}
		target_time = time_left / stones;
		if (boardsize > 9 && msptr < 16) {						/* play first few moves faster on big boards */
			target_time  = target_time * (msptr + 2) / (16 + 2);
		}
		max_time = target_time * 2;
	}
	if (level == MAXLEVEL && target_time < 5000) {
		level--;
	}
	if (level >= MAXLEVEL - 1 && target_time < 3000) {
		level--;
	}
	if (level > 0 && target_time < 1500) {
		level--;
	}
	if (level > 1 && target_time < 750) {
		level = 1;
	}

	starttime = GetTickCount();
	fixplaylevel(level);
	life(FALSE);
	getobaval();
	initbestresp();
	compmoveinprogress = TRUE;
	stopearly = FALSE;

	/* set up randomness */
	for (s = 0; s < 361; ++s) {
		randomval[s] = 0;
	}
	randomval[PASS] = 0;

	if (randomize && 
		((sqr_t)msptr < boardsquare / 8 || level < MAXLEVEL && (sqr_t)msptr < boardsquare / 2)) {
		randlimit = (int)(obaval * randomness[playlevel]);
		for (s = 0; s < 361; ++s) {
			randomval[s] = rand() % randlimit;  /* set random value for evaluating this move (used in getscore) */
		}
	}
	
	if (thinking()) {
		*retmove = PASS;
		return G2OK;
	}
	
	tt_init();
	if (thinking()) {
		*retmove = PASS;
		return G2OK;
	}
	
	for (s = 0; s < boardsquare; s++) {
		if (bestseq[s] != EOL)
			killist(&bestseq[s]);
		stratvalue[s] = -BIGNUM;
		stratpatvalue[s] = -BIGNUM;
	}
	killist(&bestseq[NOSQUARE]);

#ifdef G2DEBUGOUTPUT
	oldpscr = pscr;
	for (s = firstsquare; s < lastsquare; ++s) {
		if (board[s] != NOGROUP) {
			savedebugalive[s] = S_ALIVE(s);
			savedebugbest[s] = armybestpot[S_ARMY(s)];
			savedebugrun[s] = armyrn_pot[S_ARMY(s)];
		}
		else {
			savedebugalive[s] = 0;
			savedebugbest[s] = 0;
		}
	}
#endif

	numlifecalls = 0;  /* no random for pass and get_reasons_ */
	numsearchnodes = 0;
	numsearchevals = 0;
	numfullgen = 0;
	numqgen = 0;
	numtacnodes = 0;
	numgrtacnodes = 0;
	nummiaitacnodes = 0;
	numtvtacnodes = 0;
	numcntacnodes = 0;
	numeyetacnodes = 0;
	numlifetacnodes = 0;
	numopjtacnodes = 0;
	debugnumlifecalls = 0;
	nummoves = 0;  /* how many moves did we look at */
	numnodes = 0;  /* number of tactical nodes looked at */

#ifdef NEVER
 	if (msptr != 0) {	 /* set the last alive value not incremental so works right after game restore. */
 		move = mvs[msptr-1];	/* to restore */
 		c = mvcolor[msptr-1];
 		dndate();
 		life(FALSE);	/* back up one move */
 		for (s = firstsquare; s < lastsquare; ++s)
 			if (board[s] != NOGROUP)
 				grlastalive[s] = S_ALIVE(s);
 			else
 				grlastalive[s] = 0;
 		update(move, c, TRUE); 
 		life(FALSE);
 	} 
#endif

#ifdef G2DEBUGOUTPUT
	if (logsearch) {
		logfile = fopen("\\go\\wingovs\\searchlog.txt", "w");
		if (logfile == NULL) {
			logsearch = FALSE;
		}
		else
			fprintf(logfile, "Start a new full board search, level %d, %d-%d milliseconds.  %d evals so far.\n", playlevel, target_time, max_time, numlifecalls);
	}
	sprintf(buf, "Level %d, %d-%d milliseconds.\n", playlevel, target_time, max_time);
	outerr(buf);
#endif
	if (playlevel > READLIFELEVEL) {
		lifereading(color, starttime + target_time / 8, maxlifecalls[playlevel] / 8);	/* do the life and death reading before strategy and passeval since results affect evaluation */
	}
	
	if (thinking()) {
		*retmove = PASS;
		return G2OK;
	}

#ifdef G2DEBUGOUTPUT
	lifenodes = numlifecalls;
	secs = (GetTickCount()-starttime)/1000.;
	life(FALSE);
	badmove = FALSE;
	if (pscr != oldpscr) {
		sprintf(buf, "ERROR move %d: life reading changed pscr %d to %d\r\n", msptr+1, oldpscr, pscr);
		outerror(buf);
		addcomment(buf);
		badmove = TRUE;
	}
	for (s = firstsquare; s < lastsquare; ++s) {
		if (board[s] != NOGROUP && 
			(S_ALIVE(s) != savedebugalive[s] || armybestpot[S_ARMY(s)] != savedebugbest[s] ||
			 armyrn_pot[S_ARMY(s)] != savedebugrun[s])) {
			sprintf(buf, "ERROR move %d: pscr %d-%d Alive changed by life reading at %s from %d to %d, best %d to %d, run %d to %d\n",
				msptr+1, pscr, oldpscr, ssqr(s, buf2), savedebugalive[s], S_ALIVE(s), savedebugbest[s], armybestpot[S_ARMY(s)], savedebugrun[s], armyrn_pot[S_ARMY(s)]);
			outerror(buf);
			addcomment(buf);
			badmove = TRUE;
		}
	}
#ifdef CHECK
	if (badmove) {
		*retmove = PASS;
		compmoveinprogress = FALSE;
		if (logsearch && logfile != NULL) {
			fprintf(logfile, "Error aborts search: %s\n", buf);
			fclose(logfile);
			logfile = NULL;
		}
		return G2OK;
	}
#endif
	sprintf(buf, "Search %d-%d milliseconds (%d max evals), %5.1f secs in life reading level %d, life reading %d life()\n", 
		target_time, max_time, maxlifecalls[playlevel], secs, playlevel, numlifecalls);
	addcomment(buf);
	if (logsearch)
		fprintf(logfile, buf);
	sprintf(buf, "%5.1f secs in life reading\n", secs);
	outerr(buf);
#endif

	endtime = GetTickCount() + target_time / 8;	/* get at least this much time for pass search, even if life search goes long */
	if (starttime + target_time / 4 > endtime)
		endtime = starttime + target_time / 4;	/* use any time unused by life search */
/*	12/07 - no! savepassval = passval = getpassscore(color, handicap, rules, endtime, maxlifecalls[playlevel]/2, &passok, starttime); */


	allowpass = (int)msptr > boardsize * boardsize / 2 && !fill_dame(color) || msptr > 0 && mvs[msptr - 1] == PASS;
	if (rules == CGOS) {
		for (g = 0; g < maxgr; ++g) {
			if (!grlv[g] || grcolor[g] == color || gralive[g] < 18)
				continue;
			allowpass = FALSE;
			break;
		}
	}
	
	update(PASS, color, TRUE);
   	passval = -getcurrenteval(1 - color, handicap, rules);	/* only find moves that gain more than the assumed obaval */
	dndate();

	if (thinking())
		stopearly = TRUE;
    savestartval = getcurrenteval(color, handicap, rules);  /* score from point of view of after color to move so comparable with passval, etc */

	strategy(color, handicap, rules); 
	get_reasons_for_moves(passval, color, handicap, rules, randomize);  /* must have passval before call this */
	first = getmovestotry(randomize, allowpass, color, tryfirst, skip_count, skip_points);  /* sort moves that don't already have scores */
	
	if (thinking()) {
		*retmove = PASS;
		return G2OK;
	}
	
	bestsqr = PASS;
	besturgscore = SMALLNUM;
	mvcolor[msptr] = color;
	mvs[msptr] = NOPOINT;	/* default move to see if any changes between iterations */
	maxlc = numlifecalls + maxlifecalls[playlevel] / 2; /* allow additional life calls even if all used up in get_reasons */
	if (maxlc < maxlifecalls[playlevel])
		maxlc = maxlifecalls[playlevel];

	for (nm = 0; nm < nextmvtotry; ++nm) {  /* evaluate moves that don't already have scores */
		scoreval[movestotry[nm]] = SMALLNUM;
		newscoreval[movestotry[nm]] = SMALLNUM;
	}

	stoptime = GetTickCount() + max_time;

	for (iter = 1; iter <= maxiterations[playlevel] && !timeout && (iter == 1 || !stopearly); ++iter) {

		beta = BIGNUM;
		alpha = SMALLNUM;		/* TODO: change this for tighter bound and research if pass best move */
		newbestsqr = PASS;		/* best move found in this iteration */
		moves = 0;
		startlifecalls = numlifecalls;
		if (iter == 1) {
			qsort(&movestotry[first], nextmvtotry - first, sizeof(sqr_t), sortguess);	/* sort by guess value */
		} else {
			qsort(&movestotry[first], nextmvtotry - first, sizeof(sqr_t), sortscore);	/* sort by result of the previous iteration */
		}

#ifdef G2DEBUGOUTPUT
		if (logsearch) {
			fprintf(logfile, "\nStart iteration %d, %d moves generated\n      ", iter, nextmvtotry);
			for (nm = 0; nm < nextmvtotry; ++nm) {  /* evaluate moves that don't already have scores */
				fprintf(logfile, "%s:%d ", ssqr(movestotry[nm], buf2), scoreval[movestotry[nm]]);
			}
			fprintf(logfile, "\n\n");
		}
#endif
		for (nm = 0; nm < nextmvtotry && !timeout; ++nm) {  /* evaluate moves that don't already have scores */
			s = movestotry[nm];
			if (iter > 1 && scoreval[s] == SMALLNUM) {
#ifdef G2DEBUGOUTPUT
				if (logsearch)fprintf(logfile, "No more good moves, iteration over.\n", moves);
#endif
				continue;	/* no more good moves */
			}

			if (iter == 1 && moves == maxmoves[playlevel] / 2) {
				stoptime = starttime + max_time;		/* have enough search now to enable emergency stop */
			}

			if (moves >= maxmoves[playlevel] * 2) {
#ifdef G2DEBUGOUTPUT
				if (logsearch)fprintf(logfile, "Iteration 1 selected maximum %d moves to try.\n", moves);
#endif
				break;
			}

			if (iter == 1 && moves >= maxmoves[playlevel] && newbestsqr != PASS &&	/* go past maxmoves is have enough time */
				(numlifecalls - startlifecalls > maxlc / maxiterations[playlevel] ||	/* used enough life calls in this iteration */
				 GetTickCount() + starttime > target_time / 2)) {					/* used half of target time */
#ifdef G2DEBUGOUTPUT
				if (logsearch)fprintf(logfile, "Iteration 1 selected %d moves to try.\n", moves);
#endif
				break;  /* tried enough moves.  First iteration itentifies all moves for later examination */
			}

			scr = search(s, alpha, beta, color, handicap, rules, (float)(iter - 1), stoptime, iter == 1 && s != PASS);  /* value of move at s,color */
			if (rule_fired(s, REPETITION)) {
#ifdef G2DEBUGOUTPUT
				if (logsearch)fprintf(logfile, "repeated position ignored.\n", moves);
#endif
				continue;
			}
			if (scr != -SMALLNUM && scr > alpha && beta == alpha + 1) {
#ifdef G2DEBUGOUTPUT
				if (logsearch) fprintf(logfile, "Research scr %d > alpha %d\n", scr, alpha);
#endif
				scr = search(s, alpha, BIGNUM, color, handicap, rules, (float)(iter-1), stoptime, FALSE);  /* research for PVS */
			}

			if (scr == -SMALLNUM)
				scr = SMALLNUM;

			if (thinking()) {
#ifdef G2DEBUGOUTPUT
				if (logsearch) fprintf(logfile, "Engine says to stop thinking\n");
#endif
				break;
			}

			used = GetTickCount() - starttime;
			if (used > max_time && iter > 1 || 
				moves >= maxmoves[playlevel] && used > target_time) {
				timeout = TRUE;
#ifdef G2DEBUGOUTPUT
				if (logsearch) fprintf(logfile, "Out of time used %d target %d max (msec) %d moves %d. iteration aborted\n",
					used, target_time, max_time, moves);
#endif
				break;
			}

			goodmove = FALSE; 
			for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
				if (strat[list[ptr]].goodrule)
				goodmove = TRUE;
			}

			if (!goodmove && s != PASS) {	/* must have a good 1-ply reason unless it's a pass */
				newscoreval[s] = SMALLNUM;
#ifdef G2DEBUGOUTPUT
				if (logsearch) fprintf(logfile, "No good reason.  Ignore this move.\n");
#endif
				continue;
			}
			newscoreval[s] = scr;
			if (scr != -SMALLNUM && scr > alpha) {	/* just take the best move */
				alpha = scr;
				newbestsqr = movestotry[nm];
				beta = alpha + 1;		/* once have a good move, narrow range for PVS search */
#ifdef G2DEBUGOUTPUT
				sprintf(buf, "New best %d (strat %d)%s ", alpha, stratvalue[movestotry[nm]], ssqr(movestotry[nm], buf2));
				for (ptr = bestseq[movestotry[nm]]; ptr != EOL; ptr = link[ptr])
					strcat(buf, ssqr((sqr_t)list[ptr], buf2));
				strcat(buf, "\n");
				addcomment(buf);
				if (logsearch) fprintf(logfile, buf);
#endif
#ifdef SHOW_LOOKAHEAD
				if (showlookahead & LOOKAHEADMOVES) {
					next_pv = 0;
					for (ptr = bestseq[movestotry[nm]]; ptr != EOL; ptr = link[ptr]) {
						pv[next_pv++] = (sqr_t)list[ptr];
					}
					sendlookahead(pv, next_pv);
				}
#endif

			}
			if (scr != SMALLNUM)
				++moves;
#ifdef G2DEBUGOUTPUT
			life(FALSE);
			for (s = firstsquare; s < lastsquare; ++s) {
				if (board[s] != NOGROUP && 
					(S_ALIVE(s) != savedebugalive[s] || armybestpot[S_ARMY(s)] != savedebugbest[s] ||
					 armyrn_pot[S_ARMY(s)] != savedebugrun[s]) ) {
					sprintf(buf, "ERROR move %d: Alive during main search of move %s at %s from %d to %d, best %d to %d, run %d to %d\n",
						msptr + 1, ssqr(movestotry[nm], buf3), ssqr(s, buf2), savedebugalive[s], S_ALIVE(s), savedebugbest[s], armybestpot[S_ARMY(s)], savedebugrun[s], armyrn_pot[S_ARMY(s)]);
					outerr(buf);
					addcomment(buf);
#ifdef CHECK
					timeout = TRUE;
#endif
				}
			}
#endif
		}  /* end of moves loop for this iteration */

//		if (!thinking() && (!timeout || iter == 1)) {
		if ((!thinking() && !timeout)) {
			bestsqr = newbestsqr;
			for (nm = 0; nm < nextmvtotry; ++nm)  /* update scoreval from a good iteration */
				scoreval[movestotry[nm]] = newscoreval[movestotry[nm]];

		}
		stoptime = starttime + max_time;
#ifdef G2DEBUGOUTPUT
		sprintf(buf, "Iteration %d complete %d moves in %5.2f secs.  Total evals %d, search life() %d, \nFinal value is %d for %s: ", 
			iter, moves, (GetTickCount()-starttime)/1000., numlifecalls, numlifecalls-lifenodes, alpha, ssqr(bestsqr, buf2));
		for (ptr = bestseq[bestsqr]; ptr != EOL; ptr = link[ptr])
			strcat(buf, ssqr((sqr_t)list[ptr], buf2));
		strcat(buf, "\n");
		addcomment(buf);
		if (logsearch) fprintf(logfile, buf); 
#endif
		newbest = bestsqr != mvs[msptr];
		mvs[msptr] = bestsqr;  /* set up best move found after a full iteration */
		*retmove = bestsqr;
		savebestval = alpha;

		if (numlifecalls + (numlifecalls - startlifecalls) > maxlc || thinking()) {
#ifdef G2DEBUGOUTPUT
			if (logsearch)
				fprintf(logfile, "Out of evals for another iteration, calls %d, used %d, max %d\n", numlifecalls, numlifecalls - startlifecalls, maxlc);
#endif
			break;
		}
		used = GetTickCount() - starttime;
		if (used > max_time || 
			used > target_time * 4 / 5 ||
			!newbest && used > target_time / 2) {
#ifdef G2DEBUGOUTPUT
			if (logsearch)
				fprintf(logfile, "Out of time for another iteration, new best %d, used %d, target %d, max %d msec\n", 
					newbest, used, target_time, max_time);
#endif
			break;
		}
	}

#ifdef SHOW_LOOKAHEAD

	/* remove any lookahead */
	if (showlookahead & LOOKAHEADMOVES) {
		sendlookahead(pv, 0);
	}
#endif
	
#ifdef G2DEBUGOUTPUT
	listused = numlistused();
	treeused = usedtreenodes();
	if (treeused > maxtreeused)
		maxtreeused = treeused;;
	if (listused > maxlistused)
		maxlistused = listused;
	sprintf(buf,"Pass val %4.1f, best val %4.1f\r\n",passval/50.,alpha/50.);
	addcomment(buf);
	secs = (GetTickCount()-starttime)/1000.;
	sprintf(buf,"%5.1f secs. life() %d (%3.0f/s), searchevals %d, Nodes %d (%3.0f/s), full gen %d, Q gen %d, Moves %d(%3.0f/s), tree nodes %d/%d, list %d/%d, tac nodes %d (%3.0f/s), evalopj %d (%3.0f%%), life %d (%3.0f%%) [ fixgralive %d (%3.0f%%) miai %d (%3.0f%%) tvpot %d (%3.0f%%), group %d (%3.0f%%), conn %d (%3.0f%%), eye %d (%3.0f%%) ]\r\n", 
		secs, numlifecalls, numlifecalls/secs, numsearchevals,
		numsearchnodes, numsearchnodes/secs, numfullgen, numqgen,
		nummoves, nummoves/secs, treeused, maxtreeused, listused, maxlistused, 
		numnodes, numnodes/secs, 
		numopjtacnodes, 100.*numopjtacnodes/numnodes, 
		numlifetacnodes, 100.*numlifetacnodes/numnodes, 
		numfixgrtacnodes, 100.*numfixgrtacnodes/numnodes, 
		nummiaitacnodes, 100.*nummiaitacnodes/numnodes, 
		numtvtacnodes, 100.*numtvtacnodes/numnodes, 
		numgrtacnodes, 100.*numgrtacnodes/numnodes, 
		numcntacnodes, 100.*numcntacnodes/numnodes, 
		numeyetacnodes, 100.*numeyetacnodes/numnodes);
	addcomment(buf);
#endif

#ifdef G2DEBUGOUTPUT
	if (logsearch) {
		fprintf(logfile, "Complete in %5.2f secs.  Total evals %d, search evals %d, Final value returned is %d for %s: ", 
			(GetTickCount()-starttime)/1000., numlifecalls, numlifecalls-lifenodes, alpha, ssqr(bestsqr, buf));
		for (ptr = bestseq[bestsqr]; ptr != EOL; ptr = link[ptr])
			fprintf(logfile, "%s ", ssqr((sqr_t)list[ptr], buf));
		fprintf(logfile, "\n");
		fclose(logfile);
		logfile = NULL;
	}
#endif
#ifdef G2DEBUGOUTPUT
	life(FALSE);
	for (s = firstsquare; s < lastsquare; ++s) {
		if (board[s] != NOGROUP && S_ALIVE(s) != savedebugalive[s]) {
			sprintf(buf, "ERROR move %d: Alive value changed by main search at %s from %d to %d\n",
				msptr+1, ssqr(s, buf2), savedebugalive[s], S_ALIVE(s));
			outerr(buf);
			addcomment(buf);
		}
	}
#endif
	
	compmoveinprogress = FALSE;
#ifdef CHECK
	if (!dscheck(FALSE)) {  /* check data structures for consistency */
		turnoffcplay();
		*retmove = PASS;
		return G2FAIL;
	}
#endif
	if (allow_resign && alpha < -15 * 50 && should_resign(color, rules)) {
		return G2RESIGN;
	}
	return G2OK;  /* generated a move */
}
  

