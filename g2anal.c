/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2hd.h"
#include <stdio.h>
extern int savepassval;

void outalscore(int c, char *s, int scr)
{
	char buf[100];
	outerr(s);
	if (c == BLACKCOLOR)
		scr = -scr;
	if (scr > 0)
		sprintf(buf, "White wins by %d\n", (scr+25)/50);
	else
		sprintf(buf, "Black wins by %d\n", -(scr-25)/50);
	outerr(buf);
}

void outanalysis(sqr_t s, sqr_t retmove, int c, int handicap, int rules)
{
	int pval;

	outerr("Analysis for last move played:\n");
	if (stratreasons[s] != EOL) {
		printreasons(s, c);
		outbestseq(s);
		outerr("\n");
	}
	if (s == retmove) {
		outerr("I like this move best.\n");
	}
	else {
		if (stratreasons[s] == EOL)
			outerr("I would never consider this move.\n");
		if (retmove == PASS)
			outerr("My favorite move here is to pass.\n");
		else {
			outerr("My favorite move in this position is at A.\n");
			outstone(retmove, "A");
		}
	}
	outalscore(c, "Score before move: ", getcurrenteval(c, handicap, rules));

	pval = savepassval;  /* positive is better for side that just moved */
	outalscore(c, "Opponent could have reduced score to: ", pval);
	update(s, c, FALSE);
		
	outalscore(1-c, "After this move, the score is ", getcurrenteval(1-c, handicap, rules));

	dndate();
	if (s != retmove) {
		update(retmove, c, FALSE);
		outalscore(1-c, "After my favorite, the score is ", getcurrenteval(1-c, handicap, rules));
		dndate();
	}
}

/* analyze the current position */
void analyzepos(int rules, int handicap)
{
	int c;
	int atari;
	int stat;
	sqr_t s, retmove;
	if (msptr == 0) {
		outerr("No move on the board to analyze.\n");
		return;
	}
	c = mvcolor[msptr - 1];
	takebackamove(rules, &s); 

	stat = compmove(FALSE, c, handicap, rules, &retmove, s, 10000, 1, MAXLEVEL, FALSE, 0, NULL, FALSE, 1, FALSE);
	if (stat == G2RESIGN) {
		outerr("Computer recommends resignation.\n");
	} else if (stat != G2OK) {
		outerr("Error in computer move, can't analyze.\n");
	} else {
		outanalysis(s, retmove, c, handicap, rules);
	}
	makeamove(s, c, rules, &atari, TRUE);   /* put the old move back */
}
