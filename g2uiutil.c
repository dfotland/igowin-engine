/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */
	
#include "g2hd.h"
#include "g2pat.h"
#include "g2dist.h"
//#include "g2fuslrn.h"
#include "g2fcache.h"
#include "g2terr.pr" 
#ifdef UCT
#include "uct.h"
#endif
#include <stdio.h>
#ifdef TEST
#include <math.h>
#endif
#ifndef SMALLCODE
#include <string.h>
#endif

extern void testtv_pot(army_t army, sqr_t s);
extern void outsearchmoves(int tm);
extern void outobvious(void);
extern int getqsearchval(int color, int handicap, int ruleset);

extern int maxlifecalls[NUMLEVELS];
extern char maxfightdepth[NUMLEVELS];
extern int savealive[NUMALIVE]; 

int montecarlo = FALSE;

#ifndef SMALLCODE

char *ssqr(sqr_t s, char *str) {
	char c;
	int line;
#ifdef CHECK
	char buf[30];
#endif	
	if (s == PASS) {
		strcpy(str,"pass ");
		return(str);
		}
	if (s == NOSQUARE) {
		strcpy(str,"NO  ");
		return(str);
		}
	if (s >= boardsquare) {
#ifdef CHECK
		sprintf(buf,"ssqr, s is %d! ",s);
		outerr(buf);
#endif		
		strcpy(str,"BAD ");
		return(str);
		}
	c = s%boardsize + 'a';
	if (c > 'h')++c;
	str[0] = c;
	line = boardsize-s/boardsize;
	if (line < 10) {
		str[1] = '0'+line;
		str[2] = ' ';
		str[3] = 0;
		}
	else {
		str[1] = '1';
		str[2] = '0'+line-10;
		str[3] = ' ';
		str[4] = 0;
		}
	return(str);
	}

/* output the expected best sequence of moves */

void outbestseq(sqr_t cursorpos) {
	int ptr,i = 0;
	sqr_t start;
	char buf[10];                
	if (msptr == 0 || last_search_level == UCTLEVEL)
		return;
	if (bestseq[cursorpos] != EOL)start = cursorpos;
	else start = mvs[msptr-1];
	if (bestseq[start] == EOL) {
		outerr("Best response is to play elsewhere.\n");
		return;
		}
//	outstone(start,"0");
	i = 1;
	for (ptr = bestseq[start]; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] == PASS) {
			++i;
			continue;
		}
		sprintf(buf,"%d",i);
		outstone(list[ptr],buf);
		++i;
		}
	outerr("Expected move sequence numbered.\n");
	}



#endif /* smallcode */

#ifdef CHECK

void psqr(sqr_t s) {
	char tmp[10];
	ssqr(s,tmp);
	outerr(tmp);
	}

	
#endif

#ifdef G2DEBUGOUTPUT

extern void dumpnewlibs(sqr_t s);
extern void dumpjos(void);
extern void	outdeadshape(sqr_t cursorpos);
extern int rtthreshold;

#ifdef NEVER

char *convmove(sqr_t s, sqr_t start, int xinc, int yinc) {
	static char buf[10];
	int xv,yv,tmp;
	xv = abs(xval[s]-xval[start])+1;
	yv = abs(yval[s]-yval[start])+1;
	if (abs(yinc) == 1) {
		tmp = yv;
		yv = xv;
		xv = tmp;
		}
	sprintf(buf,"%d%d",xv,yv);
	return(buf);
	}
	
char oclr[] = { 'B', 'W' };

	
/* dump a pattern to the end of file fname */
	
void dumpapat(int xl, int xr, int yu, int yd, sqr_t s,int c, char *fname, char *str) {
	int tmp,i,j,loc;
	sqr_t start,point;  /* starting point */
	int xinc,yinc,xsize,ysize;
	char buf[80];
	FILE *pat;
	xsize = abs(xr-xl)+1;
	ysize = abs(yd-yu)+1;
	pat = fopen(fname,"a");
	if (pat == NULL) {
		sprintf(buf,"Can't open %s\n",fname);
		outerr(buf);
		return;
		}
	start = yu*boardsize+xl;
	xinc = 1;
	if (xl > xr)
		xinc = -1;
	yinc = boardsize;
	if (yu > yd)
		yinc = -boardsize;
	loc = 2;  /* anywhere */
	if (xl == 0 || xl == boardsize-1) {
		loc = 1;  /* edge */
		if (yu == 0 || yu == boardsize-1)
			loc = 0;  /* corner */
		}
	else if (yu == 0) {  /* top edge - reflect */
		loc = 1;
		xinc = boardsize;
		if (xr > xl)
			yinc = 1;
		else
			yinc = -1;
		tmp = xsize;
		xsize = ysize;
		ysize = tmp;
		}
	else if (yu == boardsize-1) {  /* bottom edge - reflect */
		loc = 1;
		xinc = -boardsize;
		if (xr > xl)
			yinc = 1;
		else
			yinc = -1;
		tmp = xsize;
		xsize = ysize;
		ysize = tmp;
		}
	for (i = 0; i < ysize; ++i) {
		for (j = 0; j < xsize; ++j) {
			point = start + xinc*j+yinc*i;
			if (board[point] == NOGROUP)
				fprintf(pat,"e");
			else if (S_COLOR(point) == 0)
				fprintf(pat,"B");
			else fprintf(pat,"W");
			}
		if (i == 0)fprintf(pat," %s",str);
		fprintf(pat,"\n");
		}
	if (loc == 0)
		fprintf(pat,"C");
	else if (loc == 1)
		fprintf(pat,"E");
	else fprintf(pat,"A");
	fprintf(pat,"N\n%s%c1.\n",convmove(s,start,xinc,yinc),oclr[c]);
	fclose(pat);
	}


void fileapat(int tm) {
	sqr_t cursorpos;
	int xl,xr,yu,yd;
	clearerror();
	outerr("Click on reference corner of pattern\n");
	outerr("On edge or corner for edge/corner patterns\n");
	cursorpos = waitclick();
	xl = xval[cursorpos];
	yu = yval[cursorpos];
	clearerror();
	outerr("Click on diagonal corner\n");
	cursorpos = waitclick();
   	xr = xval[cursorpos];
	yd = yval[cursorpos];
	clearerror();
	outerr("Click on move\n");
	cursorpos = waitclick();
	dumpapat(xl,xr,yu,yd,cursorpos,tm,"local.pat","Cut from game record");
	outerr("Done\n");
	}	

#endif /* TEST */


void checkknight(sqr_t cursorpos, int tm) {
	list_t ptr;
	int type;
	for (ptr = lkbrd[cursorpos]; ptr != EOL; ptr = link[ptr]) {
		if (cntype[list[ptr]] == CN_KNIGHTSMOVE)
			cntwolinks(cngr1[list[ptr]], cngr2[list[ptr]], grcolor[cngr1[list[ptr]]], 
				list[ptr], list[cnlkptr[list[ptr]]],
				list[link[cnlkptr[list[ptr]]]], (listval_t)(NUMGROUPS+list[ptr]), &type, TRUE);
		}
	}

void checkldr(sqr_t cursorpos, int tm, int winsko, int normal) {
	int olddebug,val,size;
	sqr_t move;
	group_t g;
	char buf[80],buf2[10];
	g = S_GROUP(cursorpos);
	if (g == NOGROUP)return;
	olddebug = debug;
	
	debug = 2000+g;
	if (normal) {
		size = cancapsize[playlevel];
		if (tm == grcolor[g])	/* see if captured if it moves first (DEAD group) */
			size *= 2;
		}
	else
		size = conncapsize[playlevel];
	if (normal)
		val = iscaptured(g,80,size,taclibs[playlevel],mvmost[playlevel],tm,NOGROUP,&move,winsko);
	else
		val = iscaptured(g,conncapdepth[playlevel],size,eyetaclibs[playlevel],connmost[playlevel],tm,NOGROUP,&move,winsko);
	sprintf(buf,"return is %d, move is %s\n",val,ssqr(move,buf2));
	outerr(buf);
	debug = olddebug;
	}



static void updatereasons(int tm, int handicap) {
	sqr_t s;
	char buf[20];              
	life(FALSE);
	get_reasons_for_moves(SMALLNUM,tm,handicap,JAPANESE,FALSE);
	unhighlight();
	outerr("Guess values for move sorting");
	for (s = 0; s < boardsquare; ++s) {
		if (stratreasons[s] == EOL)
			continue;
		sprintf(buf,"%d",stratguess[0][s]/50);
		outstone(s,buf);
		}
	}

extern void outqsearchgen();

void oldobvious(void)
{
	list_t obvlist = EOL, patlist = EOL, tmplist = EOL, ptr, pat2l = EOL;
	char buf[20], buf2[20];
	int i = 1;
	if (msptr == 0)return;
	life(FALSE);
	obvlist = genobviousmoves(0., 1, msptr-1, FALSE);
	if (msptr > 0)
		patlist = matchtypemoves(OBVIOUSFILE,mvs[msptr-1],mvcolor[msptr-1],1-mvcolor[msptr-1], TRUE);
	if (msptr > 1)
		pat2l = matchtypemoves(OBVIOUSFILE,mvs[msptr-2],mvcolor[msptr-2],mvcolor[msptr-2], TRUE);
	if (msptr > 1 && mvs[msptr-1] == PASS) {
		tmplist = matchtypemoves(OBVIOUSFILE,mvs[msptr-2],1-mvcolor[msptr-1],1-mvcolor[msptr-1], TRUE);
		ecatlist(&tmplist, &patlist);
	}
	outerr("Order of old obvious answers.\n");
	outerr("* indicates from pattern match.\n");
	outerr("! for pattern match on 2nd last move.\n");
	for (ptr = obvlist; ptr != EOL; ptr = link[ptr]) {
		ssqr(list[ptr], buf2);
		if (inflist(list[ptr], &patlist))
			sprintf(buf,"%d*: %s\n", i, buf2);
		else if (inflist(list[ptr], &pat2l))
			sprintf(buf, "%d!: %s\n", i, buf2);
		else
			sprintf(buf,"%d: %s\n", i, buf2);
		outerr(buf);
		++i;
	}
	killist(&obvlist);
	killist(&patlist);
	killist(&pat2l);
}

void outnbgrp(sqr_t s) {
	list_t ptr;
	int c;
	for (c = 0; c < 2; c++)
		for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr])
			if (c == 0)
				outstone(mvs[grpieces[list[ptr]]],"B");
			else
				outstone(mvs[grpieces[list[ptr]]],"W");
	}


void spechilist(list_t head) {
	int ptr;
	for (ptr = head; ptr != EOL; ptr = link[ptr])
		hispecstone(list[ptr]);
	}

void outpassseq(int tm) {
	int i;
	list_t ptr;
	char buf[10];
	if (bestseq[PASS] == EOL) {
		outerr("best pass equence is tenuki");
		return;
		}
	i = 1;
	for (ptr = bestseq[PASS]; ptr != EOL; ptr = link[ptr]) {
		sprintf(buf,"P%d",i);
		outstone(list[ptr],buf);
		++i;
		}
	clearerror();
	outerr("Pass sequence.\n");
	}


void outlivehere(int tm) {
	sqr_t s;
	unhighlight();
	for (s = 0; s < boardsquare; ++s)
		if (board[s] == NOGROUP &&
			livestonehere(s, tm, ltrgd[s], ld[s], NOGROUP, 3))
			outstone(s, "X");
	}

void outguess() {
	sqr_t i;
	char buf[10];
	unhighlight();
	for (i = 0; i < boardsquare; ++i) {
		if (stratguess[0][i] != 0) {
			sprintf(buf, "%d", stratguess[0][i]/50);
			outstone(i, buf);
		}
	}
}

void outnewguess(int tm, int handicap, int rules) {
	int passval;
	getobaval();
	passval = strategy(tm, handicap, rules); /* 1/05 removed pass value, less acurate, but strategy needs to be before get_reasons in getpassscore */
	get_reasons_for_moves(passval, tm, handicap, rules, FALSE);
	outguess();
}

void outstrat() {
	sqr_t i;
	char buf[10];
	unhighlight();
	for (i = 0; i < boardsquare; ++i) {
		if (stratvalue[i] != -BIGNUM) {
			sprintf(buf, "%d", stratvalue[i]/50);
			outstone(i, buf);
		}
	}
}

void outpatstrat() {
	sqr_t i;
	char buf[10];
	unhighlight();
	for (i = 0; i < boardsquare; ++i) {
		if (stratpatvalue[i] != -BIGNUM) {
			sprintf(buf, "%d", stratpatvalue[i]/50);
			outstone(i, buf);
		}
	}
}


void dumpaji(int tm) {
	sqr_t s;
	char buf[10];
	for (s = 0; s < boardsquare; ++s) {
		if (board[s] != NOGROUP) {
			sprintf(buf, "%d", armyaji[S_ARMY(s)]);
			outstone(s, buf);
		}
	}
}


void dumpfight(sqr_t cursorpos, int tm) {
	group_t g1;
	sqr_t cp2;
	tree_t mt;
	g1 = S_GROUP(cursorpos);
	g1 = biggestarmygroup(armygroups[grarmy[g1]]);
	higroup(g1);
	clearerror();
	outerr("Friendly group marked\n");
	outerr("Click on enemy group for semeai or same group for life/death\n");
	cp2 = waitclick();
	unhighlight();
	if (board[cp2] == board[cursorpos]) {
		if (!foughtalready(g1, TRYTOLIVE, tm, &mt) &&
			!foughtalready(g1, TRYTOKILL, tm, &mt)) {
			outerr("No cached fight");
			return;
			}
		}
	else if (!foughtalready(g1, board[cp2], tm, &mt)) {
		outerr("No cached fight here");
		return;
	}
	dumptree(mt, tm);
}
void outeyeindex(void)
{
	sqr_t i;
	char buf[20];
	for (i = 0; i < boardsize*boardsize; ++i) {
		if (eyerec[i] != 0) {
			sprintf(buf, "%d", eyerec[i]);
			outstone(i, buf);
		}
	}
}

extern void dumpfights(void), outlearnstats(void);
extern int numlifecalls, debugnumlifecalls, savebestval, savestartval, logsearch;

static char *probtype[] = 
{
	"Problems off\n",
	"Local Problem\n",
	"Full board Problem\n",
	"NRT life/death Problem\n",

};

void debugcommand(char c, sqr_t cursorpos, int tm, int handicap, int rules, int level)
{
	int tmp, sc;
	char buf[200];
	army_t army;
	sqr_t cp2;
	fixplaylevel(level);
	switch(c) {
		case '\001':
			debug++;
			outerr("Debug on\n");
			break;
		case 'P':
			if (problemflag == SOLVELDPROBLEM) {
				outerr("Problem solving mode off.\n");
				problemflag = FALSE;
				}
			else {
				problemflag++;
				outerr(probtype[problemflag]);
				}
			break;
		}
	if (!debug)
		return;
	switch(c) {
		case '?':
			outerr(" * - move tried\n");
			outerr(" A... pattern lookahead\n");
			outerr("a - aliveness\n");
			outerr("A - life(2), then show aliveness\n");
			outerr("B,W - black and white radiated territory\n");
			outerr("b - Best sequence (see m)\n");
			outerr("C - show connection\n");
#ifdef CHECK			
			outerr("c - iscaptured 1-tm wins ko (unconditional threatened)\n");
#endif			
			outerr("d - toggle debug\n");
			outerr("D - check data structures\n");
			outerr("E - show eye\n");
			outerr("e - show getcount result at cursor\n");
			outerr("f - fight cache contents\n");
			outerr("F - nonincremental good territory\n");
			outerr("g - log full board search to searchlog.txt\n");
			outerr("G - good territory\n");
			outerr("h - howmuchvital for army at point\n");
			outerr("H - live stone Here for each color\n");
			outerr("i - isseki output dead shape stats\n");
			outerr("j - joseki stats\n");
			outerr("J - group aji values\n");
			outerr("K - show reading for connection - knight's move\n");
			outerr("l - show location of all eye points\n");
			outerr("L - show ladder lists\n");
			outerr("M - monte carlo go toggle.\n");
			outerr("m - move sequence for pass\n");
			outerr("n - show new liberties for moves in liberty of groups \n");
			outerr("N - show groups in nbgrp lists\n");
			outerr("O - obvious responses to last move\n");
			outerr("o - game database statistics\n");
			outerr("P - problem flag\n");
			outerr("p - pattern matches\n");
			outerr("q - generate moves for iscaptured\n");
			outerr("Q - do a Q search.  save in log if log search.\n");
			outerr("r - uct prior values without mfgo\n");
			outerr("R - uct prior values with mfgo (was canrunhere)\n");
			outerr("S - scores\n");
			outerr("s - semeai moves (or life/death moves)\n");
			outerr("t - iscaptured tm wins ko (dead or possibly threatened)\n");
			outerr("T - territory values\n");
			outerr("u - guess values\n");
			outerr("U - get_reasons, then out guess values\n");
			outerr("v - vitality - probability of living\n");
			outerr("V - generate move for playout\n");
			outerr("w - Why - calculate reasons for this position\n");
			outerr("W - White radiated territory.\n");
			outerr("x - Distance from B stones.\n");
			outerr("X - Distance from W stones.\n");
			outerr("Y - output eye, with evaluate eye first\n");
			outerr("y - show eyelist - points to reevaluate eyes\n");
			outerr("z - dump the move tree for the fight reading here\n");
			outerr("1 - ltr1 terr value\n");
			outerr("0 - neither moves\n");
			outerr("2 - White running radiation\n");
			outerr("3 - Black running radiation\n");
			outerr("4 - show move sort order (1 best), *=urgent, !=evaluated, rest are just suggested\n");
			outerr("5 - connection reading\n");
			outerr("6 - show strategic values\n");
			outerr("7 - show strategiv pattern values\n");
			outerr("8 - territory at this liberty\n");
			outerr("9 - show search.c moves not in joseki or patterns\n");
			break;
#ifdef CHECK
	        case 'D':
			dscheck(TRUE);
			break;
#endif
			case 'g':
			logsearch = 1-logsearch;
			if (logsearch) {
				outerr("Search logging is ON");
				remove("searchlog.txt");
			}
			else
				outerr("Search logging is OFF");
			break;
			case 'O':
			outobvious();
			oldobvious();
			break;
			case 'o':
				outlearnstats();
				break;
			  case 'N':
			outnbgrp(cursorpos);
			break;
		      case 'r':
			uct_show_prior(handicap, tm, rules, 0);
			break;
			  case 'R':
			uct_show_prior(handicap, tm, rules, 1);
//				  if (board[cursorpos] != NOGROUP || lgr[cursorpos] == NOGROUP) {
//					  outerr("cursor must be on a liberty");
//					  break;
//				  }
//					canrunhere(grarmy[lgr[cursorpos]], cursorpos);
			break;
		      case 'E':
			outeyerec(cursorpos);
				break;
			  case 'Y':
			  outneweyerec(cursorpos);
			  break;
		      case 'e':
			outgetcount(cursorpos);
			getltrgd(cursorpos,TRUE, &tmp, &tmp);
			break;
		  case 'y':
			  outeyelist();
			  break;
		  case 'f':
			  	dumpfights();
				break;
		  case 'h':
			  if (board[cursorpos] == NOGROUP) {
				  outerr("Cursor must be on a group\n");
				  break;
				}
				army = S_ARMY(cursorpos);
				outerr("Click on point to evaluate\n");
				cp2 = waitclick();
				testtv_pot(army, cp2);
				  break;
		  case 'H':
				  outlivehere(tm);
				  break;
		case 'j': 
				dumpjos();
				break;
		case 'J': 
				dumpaji(tm);
				break;
        case 'L':
			if (board[cursorpos] == NOGROUP) {
				outerr("Cursor must be on group\n");
				break;
				}
			spechilist(grldr[board[cursorpos]]);
			break;
        case 'n': 
        	dumpnewlibs(cursorpos);
        	break;
		case '0':
			turnoffcplay();
			outerr("Computer neither\n");
			break;
		case 'b':
			outbestseq(cursorpos);
			break;
		case 'm':
			outpassseq(tm);
			break;
		case 'M':
			montecarlo = !montecarlo;
			if (montecarlo) {
				outerr("Monte carlo go");
			} else {
				outerr("Normal go");
			}
			break;
		case 's':
			debugsemmoves(cursorpos, tm);
			break;
		case 'S':
			outscores();
			break;
		case '4':
			outscoreorder();
			break;
		case 'A':
			life(2);
			outlal();
			break;
		case 'v':
			life(FALSE);
			outalprob();
			break;
		case 'V':
			po_test_generate(handicap, tm);
			break;
		case 'i':
			if (board[cursorpos] != NOGROUP)
				outdeadshape(cursorpos);
			break;
		case 'a':
			outlal();
			break;
#ifdef CHECK			
		case 'c':
			clearerror();
			checkldr(cursorpos, tm, 1-tm, TRUE);
			break;
		case 't':
			clearerror();
			checkldr(cursorpos, tm, tm, TRUE);
			break;
		case '5':
			clearerror();
			checkldr(cursorpos, tm, 1-tm, FALSE);
			break;
		case 'K':
			clearerror();
			checkknight(cursorpos, tm);
			break;
#endif
		case 'x':
			outdist(0);
			break;
		case 'X':
			outdist(1);
			break;
		case 'G':
			outltrgood();
			break;
		case 'F': 
			outtltrgood(cursorpos);
			break;
		case '1':
			outltr1();
			break;
		case 'W':
			outrad(1);
			break;
		case 'B':
			outrad(0);
			break;
		case '2':
			outrunrad(1);
			break;
		case '3':
			outrunrad(0);
			break;
		case 'T':
			outtterr(tm, handicap, rules);
			break;
		case 'd':
			{
				if (debug == 0) {
					outerr("Debug on\n");
					debug = 3;
				}
				else {
					outerr("Debug off\n");
					debug = 0;
				}
			}
			break;
		case 'p':
			pmatches();
			break;
		      case 'C':
			outconnections(cursorpos);
			break;
		case 'q':
			outerr("tm wins ko\n");
			dumpgenmoves(cursorpos, tm, tm);
			outerr("1 - tm wins ko\n");
			dumpgenmoves(cursorpos, tm, 1-tm);
			break;
		case 'Q':
			{
			tmp = getqsearchval(tm, 0, JAPANESE);
			sprintf(buf, "Q search returns %d\n", tmp);
			outerr(buf);
			break;
			}
		case 'w':
			updatereasons(tm, handicap);
			break;
		case 'z':
			dumpfight(cursorpos, tm);
			break;
		case 'u':
			outguess();
			break;
		case 'U':
			outnewguess(tm, handicap, rules);
			break;
		case '6':
			outstrat();
			break;
		case '7':
			outpatstrat();
			break;
		case '8':
			sc = evallibsterr(cursorpos, EOL);
			sprintf(buf, "terr: %d\n", sc);
			outerr(buf);
			break;
		case '9':
			outsearchmoves(tm);
			break;
		case 'l':
			outeyeindex();
			break;
		}	/* end switch */
}

#endif	/* g2debugoutput */

#ifndef SMALLCODE

extern int solvingproblem;
void solveproblem(sqr_t s, int tm, int level, int maxnodes, int maxdepth)
{
	tree_t mtree;
	if (board[s] == NOGROUP)
	{
		outerr("No stone at point selected\n");
		return;
	}
	if (level > NUMLEVELS - 1)
		level = NUMLEVELS - 1;
	fixplaylevel(level);
	life(FALSE);
	clearafight(board[s]);	/* do a new analysis for this group */
	solvingproblem = TRUE;
	if (S_COLOR(s) == tm)
	{
		mtree = canmakelife(board[s], maxnodes, maxdepth);
	}
	else
	{
		mtree = canbekilled(board[s], maxnodes, maxdepth);
	}
	solvingproblem = FALSE;
	dumptree(mtree, tm);
}

#endif


#ifdef NEVER
char *ssqr(sqr_t s, char *str) {
	char c, buf[30];
	if (s == PASS) {
		strcpy(str,"pass");
		return(str);
		}
	if (s == NOSQUARE) {
		strcpy(str,"NO  ");
		return(str);
		}
	if (s >= boardsquare) {
		sprintf(buf,"ssqr, s is %d! ",s);
		outerr(buf);
		strcpy(str,"BAD ");
		return(str);
		}
	c = s%boardsize + 'a';
	if (c > 'h')++c;
	sprintf(str,"%c%d ",c,boardsize-s/boardsize);
	return(str);
	}
#endif

void markgroupstrength(group_t g, char ms[361][NUMSUG+1]) {
	char buf[3],*gstr;
	int i;
	gstr = getstring(GOPAGE,73);
	buf[0] = *(gstr+G_ALIVE(g));
#ifdef NEVER	
	if (G_THREATENED(g)) {
		buf[1] = '*';
		buf[2] = 0;
		}
	else 
#endif	
#ifdef NEVER
	for (i = 0; i < 361; ++i)
		ms[i][0] = 0;
#endif		
	for (i = grpieces[g]; i != -1; i = mvnext[i]) {
		ms[mvs[i]][0] = buf[0];
		ms[mvs[i]][1] = 0;
		}
	}

void getgroupstrength(char ms[361][NUMSUG+1]) {
	group_t g;
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])continue;
		markgroupstrength(g,ms);
		}
	}
	
/* mark moves of a particulr type.  return TRUE if marks made. */	

static void getpatmovelist(int type, int color, char b, int clr, char marks[])
{
	list_t tmp, ptr;
	tmp = getpatmoves(type, color, clr);
	for (ptr = tmp; ptr != EOL; ptr = link[ptr])
		marks[list[ptr]] = b;
	killist(&tmp);
}

                            
static void  setmarks(char marks[], char ms[][NUMSUG+1])
{
	sqr_t i;
	int j;
	for (i = 0; i < boardsquare; ++i) {
		if (marks[i] == 0)
			continue;
		if (marks[i] == ms[i][0])
			continue;  /* no duplicate letters */
		for (j = NUMSUG-1; j > 0; --j)
			ms[i][j] = ms[i][j-1];
		ms[i][0] = marks[i];
		ms[i][NUMSUG] = 0;
		marks[i] = 0;
	}
}  

extern char *getstr(int str, char *buf);

/* return fuseki marks for all moves from this position for color */
static void getfusekimarks(int color, char *marks) 
{
	int wins[3], strongest, i;
	struct learn_mv lm[361];
	int mostpop = 1, total = 0, best, bestlm;
	char buf[1000], mark = 'A', wbuf[100], bbuf[100];
	struct learn_game gm;
	int game;
	int count = getfuseki(color, lm, &strongest, wins, &game);
	getlearngame(game, &gm);

	for (i = 0; i < 363; ++i) {
		marks[i] = 0;
	}

	if (count == 0) {
		return;
	}

	getstr(gm.strength[0], wbuf);
	getstr(gm.strength[1], bbuf);
	sprintf(buf, "White: %s %s\nBlack: %s %s\nEvent: %s: %d/%d/%d\n\n",
		gm.names[0], wbuf, gm.names[1], bbuf, gm.event, gm.month, gm.day, gm.year);
	outerr(buf);
#ifdef G2DEBUGOUTPUT
	sprintf(buf, "File: %s\n", gm.filename);
	outerr(buf);
#endif

	if (wins[0] + wins[1] + wins[2] == 1) {
		sprintf(buf, "Seen once.  %s wins %3.0f%%.\r\n",
			color ? "White" : "Black",
			100. * wins[color]/(wins[0] + wins[1] + wins[2])
			);
	} else {
		sprintf(buf, "Seen %d times.  %s wins %3.0f%%.\r\n", 
			wins[0] + wins[1] + wins[2],
			color ? "White" : "Black",
			100. * wins[color]/(wins[0] + wins[1] + wins[2])
			);
	}
	outerr(buf);
	for (i = 0; i < count; ++i) {
		total += lm[i].count;
		if (lm[i].count > mostpop) {
			mostpop = lm[i].count;
		}
	}

	while(1) {
		best = 0;
		for (i = 0; i < count; ++i) {
			if (lm[i].count > best) {
				best = lm[i].count;
				bestlm = i;
			}
		}
		if (!best) {
			break;
		}
		if (best >= mostpop / 40 && marks[lm[bestlm].s] == 0) {
			marks[lm[bestlm].s] = mark;
			sprintf(buf, "%c: %d%%\r\n", mark, 
				100 * best / (wins[0] + wins[1] + wins[2]));
			outerr(buf);
			if (mark == 'Z') {
				mark = 'a';
			} else {
				mark++;
			}
		} else if (best >= mostpop / 100 && marks[lm[bestlm].s] == 0) {
			marks[lm[bestlm].s] = '*';
		}

		lm[bestlm].count = 0;
	}
}
                            
void getsuggestions(int flag, int color, char ms[361][NUMSUG+1])
{ 
	int i,brun,mvptr,tlibs;
	group_t g;
	army_t army;
	list_t tmp = EOL, ptr, uns = EOL, ptr2, tmplist;
	char marks[363], *p, tmpc[10];

	for (i = 0; i < 361; ++i) {
		marks[i] = 0;
		ms[i][0] = 0;
	}
	
	life(FALSE);

	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])continue;
		if (G_ALIVE(g) > ALIVE && G_ALIVE(g) < WEAK || (G_THREATENED(g) && G_ALIVE(g) < WEAK))
			addlist(grarmy[g],&uns);
	}
		
	if (flag & SHOWGROUPS) {
		getgroupstrength(ms); /* must be first since clears ms */
	}
	
	if (flag & SHOWLIBERTIES) {
		for (g = 0; g < maxgr; ++g) {
			if (!grlv[g])continue;
			tlibs = grlibs[g];
			p = tmpc;
			if (tlibs >= 100) {
				*p = '0' + tlibs/100;
				tlibs = tlibs % 100;
				p++;
				if (tlibs < 10)
					*p++ = '0';
			}
			if (tlibs >= 10) {
				*p = '0' + tlibs/10;
				tlibs = tlibs % 10;
				p++;
			}
			*p++ = '0' + tlibs;
			*p = 0;
			for (mvptr = grpieces[g]; mvptr != -1; mvptr = mvnext[mvptr]) {
				i = mvs[mvptr];
				if (strlen(ms[i]) + strlen(tmpc) > NUMSUG)
					continue;
				strcat(ms[i], tmpc);
			}
		}
	}
	
	p = getstring(GOPAGE,54);
	if (flag & SHOWENDGAME) {
		getpatmovelist(PT_ENDGAME, color, *p, 0, marks);
		getpatmovelist(PT_ENDGAME, color, *p, 1, marks);
		setmarks(marks,ms);
	}

	if (flag & SHOWSHAPE) {
		getpatmovelist(PT_NORMAL, color, *(p+1), 0, marks);
		getpatmovelist(PT_NORMAL, color, *(p+1), 1, marks);
		setmarks(marks,ms);
	}

	if ((flag & SHOWOBVIOUS) && msptr > 0) {
		tmp = genobviousmoves(1., 0, -1, FALSE);
		for (ptr = tmp; ptr != EOL; ptr = link[ptr]) {
			marks[list[ptr]] = *(p+2);
		}
		killist(&tmp);	
		setmarks(marks,ms);
	} 

	if (flag & SHOWINVADE) {
		getpatmovelist(PT_INVADE, color, *(p+3), 0, marks);
		setmarks(marks,ms);
	}

	if (flag & SHOWDEFEND) {
		getpatmovelist(PT_INVADE, color, *(p+4), 1, marks);
		setmarks(marks,ms);
	}
		
	if (flag & SHOWSURROUND) {
		getpatmovelist(PT_SURROUND, color, *(p+5), 0, marks);
		for (ptr = uns; ptr != EOL; ptr = link[ptr]) {
			if (A_COLOR(list[ptr]) == color)continue;
			army = (army_t)list[ptr];
			brun = NUMRUN-1;
			for (i = 0; i < NUMRUN; ++i)  /* find best running value */
				if (armyrun[army][i] != EOL) {
					brun = i;
					break;
				}

			for (ptr = armyrun[army][brun]; ptr != EOL; ptr = link[ptr]) {
				tmplist = surroundpoints(army,list[ptr]);
				for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2])
					marks[list[ptr2]] = *(p+5);
				killist(&tmplist);
			}
		}
		setmarks(marks,ms);
	}

	if (flag & SHOWRUN) {
		getpatmovelist(PT_SURROUND, color, *(p+6), 1, marks);
		for (ptr = uns; ptr != EOL; ptr = link[ptr]) {
			if (A_COLOR(list[ptr]) != color)continue;
			army = (army_t)list[ptr];
			brun = NUMRUN-1;
			for (i = 0; i < NUMRUN; ++i)  /* find best running value */
				if (armyrun[army][i] != EOL) {
					brun = i;
					break;
				}

			if (brun <= MAXEASYRUN)  /* get the running moves */
				for (ptr = armyrun[army][brun]; ptr != EOL; ptr = link[ptr]) {
					tmplist = runpoints(army,list[ptr],brun);
					for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2])
						marks[list[ptr2]] = *(p+6);
					killist(&tmplist);
				}
		}
		setmarks(marks,ms);
	}

	if (flag & SHOWCUT) {
		getpatmovelist(PT_CUT, color, *(p+7), 0, marks);
		getpatmovelist(PT_CUT, color, *(p+7), 1, marks);
		setmarks(marks,ms);
	}

	if (flag & SHOWKILL) {
		for (ptr = uns; ptr != EOL; ptr = link[ptr])
			if (A_COLOR(list[ptr]) != color) {
				tmp = genobviouskill((army_t)list[ptr]);
				for (ptr2 = tmp; ptr2 != EOL; ptr2 = link[ptr2]) {
					marks[list[ptr2]] = *(p+8);
				}
				killist(&tmp);
			}
		setmarks(marks,ms);
	}
		
	if (flag & SHOWLIVE) {
		for (ptr = uns; ptr != EOL; ptr = link[ptr])
			if (A_COLOR(list[ptr]) == color) {
				tmp = genobvioussave((army_t)list[ptr]);
				for (ptr2 = tmp; ptr2 != EOL; ptr2 = link[ptr2]) {
					marks[list[ptr2]] = *(p+9);
				}
				killist(&tmp);
			}
		setmarks(marks,ms);
	}
		
	if (boardsize > 13 && (flag & SHOWJOSEKI)) {
		for (i = 0; i < 4; ++i) {
			getjoseki(i,color,marks);
			setmarks(marks,ms);
		}
	}
	if (flag & SHOWFUSEKI) {
		getfusekimarks(color, marks);
		setmarks(marks, ms);
	}

	if (flag & SHOWJOSEKIGAME) {
		getgamejoseki(color, marks);
		setmarks(marks, ms);
	}

	killist(&uns);
}

void getprisoners(int *b, int *w) {
	*b = numpris[0];
	*w = numpris[1];
	}

void showtac(int f)
{
	showtactics = f;
}

army_t point2army(sqr_t s) {
	return(S_ARMY(s));
	}                    

group_t point2group(sqr_t s) {
	return(S_GROUP(s));
	}	

void group2points(group_t g, sqr_t points[]) {
	int i = 0;
	int ptr;
	for (ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr])
		points[i++] = mvs[ptr];
	points[i] = NOPOINT;
	}

void army2groups(army_t a,int groups[]) {
	int i = 0;
	list_t ptr;
	for (ptr = armygroups[a]; ptr != EOL; ptr = link[ptr])
		groups[i++] = list[ptr];
	groups[i] = -1;
	}

void getboard(char brd[]) {
	sqr_t s;
	for (s = 0; s < boardsquare; ++s) {
		brd[s] = S_COLOR(s);
		}
	}
