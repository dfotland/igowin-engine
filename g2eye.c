/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */
#include "g2hd.h"
#include "g2dist.h"

#ifdef G2DEBUGOUTPUT
#include <stdio.h>
#include <string.h>
#endif


# define MIN(x,y) (((x) < (y)) ? x : y)

/* can't use ld[] or lgr[] here since not updated in howmuchvital */
#define ld ERROR
#undef S_NEUTRAL
#define S_NEUTRAL2(s) (lnbf[s][0] && lnbf[s][1])
/* #define lgr ERROR */ 

static int inbigeye(sqr_t s, int c, army_t army);
static int cankillwithatari(sqr_t s, int c,sqr_t *s2, listval_t ldrno, int libs);
static int countends(eye_t rn, int c, list_t plist, list_t liblist, int *fours, int *threes, int *falseeyes, int *emptyends, sqr_t *keypoint, listval_t ldrno, int fe_enable, int libs);
static int eyecntplyhere(sqr_t s, int c, listval_t ldrno, int libs);
static void expandeye(eye_t rn, sqr_t s, int *c, army_t army);
static void eyesforshape(eye_t rn, int c, listval_t ldrno, int full, int libs);
static int can_be_eye(sqr_t s, group_t g);
static void evaleyenearedge(eye_t rn, sqr_t s, sqr_t end1, sqr_t end2, int c, listval_t ldrno, int full, int libs);
static void evalverybigeye(eye_t rn, sqr_t s, army_t army, int c, listval_t ldrno, int full, int libs);
static void evalopenlineeye(sqr_t s, int c, eye_t rn, listval_t ldrno, int full, int libs);
static void findeyelist(void);
static void evalonepteye(sqr_t s, int c, eye_t rn, listval_t ldrno, int full, int libs);
static void evalcornereyes(void);
static void deallocate_eye(eye_t rn);
static void evaldeadgroupeye(sqr_t s, group_t g, int c, eye_t rn, listval_t ldrno, int libs);
static void evalthgroupeye(sqr_t s, group_t g, int c,eye_t rn, listval_t ldrno, int libs);
static void evaloneptdeadeye(sqr_t s, group_t g, int c, eye_t rn, listval_t ldrno, int libs);
static void evalmanyeyespots(eye_t rn, sqr_t sopen, int c, int numopenspots, int numenemyspots, listval_t ldrno, sqr_t openspot, int libs);
static void eval2pointeye(sqr_t s, sqr_t sopen, int c,eye_t rn, listval_t ldrno, int full, int libs);
static void evallineeye(sqr_t s, sqr_t sopen, sqr_t start, int c,eye_t rn, listval_t ldrno, int full, int libs);
static void evalbigeye(sqr_t s, sqr_t sopen, sqr_t start, int c,eye_t rn, listval_t ldrno, int full, int libs);
int getldval(sqr_t s);
static int getcount(sqr_t s, int c, eye_t rn, int vital,listval_t ldrno,int libs);
  
int c8[] = { 1,1,1,4,1,4,1,1,4,4,8,8 };  
int c7[] = { 1,1,1,1,1,1,1,1,1,1,4,4 };
int cpot[] = { 0,0,0,0,0,0,6,6,6,6,8,8 };  
int opt1[] = { 0,0,4,8,4,8,3,3,8,8,8,8};  /* eyepot (8 - can get eye in one move, 4 can get eye in two moves,3 ko */
int opt2[] = { 0,0,0,0,0,0,0,0,5,5,8,8};  /* eyeval (5 2/3 eye - win ko to make eye 1/01) */
int opt2max[] = { 0,0,0,0,0,0,0,0,8,8,8,8};  /* eyeval (5 2/3 eye - win ko to make eye 1/01) */
int opt3[] = { 0,0,0,0,0,0,0,0,0,0,0,8};  /* eyemin */
int opt4[] = { 0,0,0,0,0,0,1,2,3,4,0,0};  /* eyeko */
  
void dumpeyerec(eye_t rn);

/* utility functions for neighbor states of a group or point 
 */
static int anydeadnbgrp(sqr_t s, int c)
{
	list_t ptr;
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
		if (gralive[list[ptr]] == DEAD)
			return TRUE;
	}
	return FALSE;
}

static int onelibgrnbp(group_t g)
{
	int ptr;
	for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr])
		if (grlibs[list[ptr]] == 1)
			return(TRUE);
	return(FALSE);
}

/* does s have a nbr of color c that can be captured */      
static int onelibnbgrp(sqr_t s, int c) {
	list_t ptr;
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr])
		if (grlibs[list[ptr]] == 1 &&
			(grsize[list[ptr]] == 1 || lnbn[s]))
			return TRUE;
	return FALSE;
}
 
static int onelibnbgrp2(sqr_t s, int c) {
	list_t ptr;
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr])
		if (grlibs[list[ptr]] == 1 )
			return TRUE;
	return FALSE;
}
void fixli(void)
{
	list_t ptr, ptr2, tmplist = EOL;
	eye_t rn;
	listval_t ldrno;
	sqr_t s;
	group_t g;
#ifdef G2DEBUGOUTPUT
	char buf[80], buf2[20];
	unsigned int i;
#endif

	findeyelist();

	for (ptr = eyelist; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		ASSERT(s >= 0 && s <= boardsquare);
#ifdef TEST
		if (s >= boardsquare) {
			outerror("Bad val in eyelist\n");
			turnoffcplay();
		}
#endif
		if (eyerec[s] != 0) {
			deallocate_eye(eyerec[s]);
		}
	}

	/* deallocate corner eyes */
	if (eyetype[eyerec[0]] == CORNEREYE)deallocate_eye(eyerec[0]);
	if (eyetype[eyerec[boardsize-1]] == CORNEREYE)
		deallocate_eye(eyerec[boardsize-1]);
	if (eyetype[eyerec[boardsquare-boardsize]] == CORNEREYE)
		deallocate_eye(eyerec[boardsquare-boardsize]);
	if (eyetype[eyerec[boardsquare-1]] == CORNEREYE)
		deallocate_eye(eyerec[boardsquare-1]);
  
	for (ptr = eyelist; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		g = board[s];

		/* is this spot part of an eye? */
		if (eyerec[s] != 0)
			continue;  /* already did this eye */
		if (g != NOGROUP && eyerec[mvs[grpieces[g]]] != 0)
			continue;
                                   /* already did this eye */
		if (!can_be_eye(s, g))
			continue;
		rn = (eye_t)gtflist(&eyefreelist);

		if (rn == G2ERROR) {
#ifdef CHECK      	
			outerror("Out of eye records");
#endif	   
			return;
		}

		ldrno = NUMGROUPS+NUMCONNS+rn;
		evaleye(s, rn, ldrno, TRUE, eyetaclibs[playlevel]);
		if (eyeptr[rn] == EOL) {	/* no points in the eye */
			delete_eye(rn);
			continue;
		}
		if (eyetype[rn] == UNKNOWNEYE) {
#ifdef G2DEBUGOUTPUT
			outerror("Evaleye returns unknown");
#endif
			delete_eye(rn);
			continue;
		}
		for (ptr2 = eyeptr[rn]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (eyerec[list[ptr2]] != 0) {
#ifdef G2DEBUGOUTPUT
				/* turnoffcplay(); */
				ssqr(list[ptr2], buf2);
				sprintf(buf, "ERROR eyerec allocation move %d old %d, new %d at %s.  Recent moves:\n", 
					msptr, eyerec[list[ptr2]], rn, buf2);
				outerror(buf);
				for (i = msptr >= 8?msptr-8:0; i < msptr; ++i) {
					ssqr(mvs[i], buf2);
					outerror(buf2);
				}
				histone(list[ptr2]);
				outeyerec(list[ptr2]);
				dumpeyerec(rn);
#endif
				dellist(list[ptr2], &eyeptr[eyerec[list[ptr2]]]);
				if (eyeptr[eyerec[list[ptr2]]] == EOL)
					deallocate_eye(eyerec[list[ptr2]]);
			}
			eyerec[list[ptr2]] = rn;
		}
      
		for (ptr2 = eyevital[rn]; ptr2 != EOL; ptr2 = link[ptr2]) {
			/*  eyevitval[board[list[ptr2]&EYEPOINTMASK]][eyecolor[rn]] = EYEVALNONE;  clear the cache */

			/* if a vital point has a stone on it, ignore it */
			if ((board[list[ptr2]&EYEPOINTMASK]) != NOGROUP) {
#ifdef G2DEBUGOUTPUT
				outerror("Nonempty vital point at ");
				outerror(ssqr((sqr_t)(list[ptr2]&EYEPOINTMASK), buf));
#endif
				continue;
			}

			/* combine duplicates with different flags set */
			if ((list[ptr2]&(~EYEPOINTMASK))) { /* restrictions */
				if (inlist((listval_t)(list[ptr2]&EYEPOINTMASK), &eyevital[rn]))
					continue;	/* already have this point, without restrictions */
				if (inlistm((listval_t)(list[ptr2]&EYEPOINTMASK), &tmplist, EYEPOINTMASK)) {
					dellistm((listval_t)(list[ptr2]&EYEPOINTMASK), &tmplist, EYEPOINTMASK);
					addlist((listval_t)(list[ptr2]&EYEPOINTMASK), &tmplist);   /* both bits set, no restrictions */
				}
				else 
					addlist(list[ptr2], &tmplist);
			}
			else 
				addlist(list[ptr2], &tmplist);
		}
		killist(&eyevital[rn]);
		eyevital[rn] = tmplist; /* new eyevital with duplicates combined */
		tmplist = EOL;

		/* for every eyevital, set eyerec */
		for (ptr2 = eyevital[rn]; ptr2 != EOL; ptr2 = link[ptr2]) {
			adflist((listval_t)(rn|(list[ptr2]&EYEFLAGMASK)),
     			&eyevitrec[list[ptr2]&EYEPOINTMASK]);  /* transfer flags and init no value */
			if (addlist(ldrno,&ldrflag[list[ptr2]&EYEPOINTMASK]))
				adflist((sqr_t)(list[ptr2]&EYEPOINTMASK), &grldr[ldrno]);
		}
	}
	evalcornereyes();
	killist(&eyelist);
}


/* evaluate the eye at s.  return its values in eyerecord number rn 
 * modifies the eye record structures at rn only.
 * note that eyevital returned here has EYEADDNLY and EYERMONLY flags set.
 * if full, can do a full eval based on all data structures.  otherwise,
 * position just had an lupdate from howmuchvital, and higher level data structures
 * are not valid
 */

void evaleye(sqr_t s, eye_t rn, listval_t ldrno, int full, int libs)
{
	sqr_t sopen;
	int c = NOCOLOR,bigeye;
	group_t dead = NOGROUP;
	list_t ptr;
#ifdef G2DEBUGOUTPUT
	char buf[10], buf2[120];
#endif
#ifdef CHECK
	if (s > boardsquare) {
		outerror("bad square in evaleye!\n");
	}
#endif
	eyetype[rn] = UNKNOWNEYE;
	/* check if can be eye at all */
	if (!can_be_eye(s, board[s])) {
		eyemin[rn] = eyeval[rn] = eyepot[rn] = eyevalmax[rn] = eyeko[rn] = 0;
		return;
	}

	eyevalmax[rn] = 0;	/* for default if not set by an eye evaluator */
	eyeko[rn] = 0;

	if (board[s] != NOGROUP)  /* stone here, group eye */
		c = 1-grcolor[board[s]];
	else {
		for (ptr = nbgrp[s][1]; ptr != EOL; ptr = link[ptr]) {
			c = 1;
			if (G_ALIVE(list[ptr]) == DEAD) {
				dead = (group_t)list[ptr];
				c = 0;
				break;
			}
		}
		for (ptr = nbgrp[s][0]; ptr != EOL; ptr = link[ptr]) {
			c = 0;
			if (G_ALIVE(list[ptr]) == DEAD) {
				dead = (group_t)list[ptr];
				c = 1;	/* if two dead groups share liberties, give point to black dead group */
				break;
			}
		}
	}
	if (c == NOCOLOR)
		c = WHITECOLOR;
	eyecolor[rn] = c;

	if (dead != NOGROUP) { /* in liberty of dead group */
		evalverybigeye(rn, mvs[grpieces[dead]], NOARMY, c, ldrno, full, libs);
	}
	else if (board[s] != NOGROUP) {
		if (gralive[board[s]] == DEAD) {
			evalverybigeye(rn, s, NOARMY, c, ldrno, full, libs);  /* 9/99 */
		}
		else {
			evalthgroupeye(s, board[s], c, rn, ldrno, libs);  /* threatened group */
		}
	}

	else if (lnbn[s] == 0)
		evalonepteye(s, c, rn, ldrno, full, libs);

	else if (lnbn[s] == 1) {
	 	sopen = list[nblbp[s]];
		if (lnbn[sopen] == 1 && lnbf[sopen][1-c] == 0)	/* two point eye */
			eval2pointeye(s, sopen, c, rn, ldrno, full, libs);

		else if (lnbn[sopen] == 2 && lnbf[sopen][1-c] == 0)  /* line eye */
			evallineeye(s, sopen, s, c, rn, ldrno, full, libs);
		else 		/* more than 2 point eye */
			evalbigeye(s, sopen, s, c, rn, ldrno, full, libs);
	}
	else if (lnbn[s] == 2 && lnbf[s][c] != 0)
		evalopenlineeye(s, c, rn, ldrno, full, libs);
	else {
		bigeye = FALSE;
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
			if (lnbn[list[ptr]] == 1 && lnbf[list[ptr]][1-c] == 0) {
				evalbigeye(list[ptr], s, s, c, rn, ldrno, full, libs);
				bigeye = TRUE;
				break;
			}
		}
		if (!bigeye && nbgrp[s][c] != EOL && inbigeye(s, c, G_ARMY(list[nbgrp[s][c]])))
			evalverybigeye(rn, s, G_ARMY(list[nbgrp[s][c]]), c, ldrno, full, libs);
#ifdef G2DEBUGOUTPUT 
		else if (!bigeye) {
			sprintf(buf2,"eye eval fell thru! at %s - %d\n",ssqr(s,buf),board[s]);
			outerror(buf2);
			turnoffcplay();
		}
#endif
	}
	if (eyevalmax[rn] < eyeval[rn])
		eyevalmax[rn] = eyeval[rn];
#ifdef G2DEBUGOUTPUT
	if (eyeval[rn] > eyepot[rn] || eyemin[rn] > eyeval[rn] || eyeval[rn] > eyevalmax[rn]) {
		outerror("ERROR: evaleye bad value\n");
		dumpeyerec(rn);
	}
#endif
}

/*
 * find value of eye at edge of board that has many points in it.
 * army is NOARMY If owning army is unknown, or MULTIARMY if multiple armies adjacent (don't expand it)
 * c is color of group making eye, or NOCOLOR if unknown
 * start expanding eye from s, which is a point with ltr1 > 0 and ltrgd == 0
 * all points in this eye must be associated with the same army
 */     
static void evalverybigeye(eye_t rn, sqr_t s, army_t army, int c, listval_t ldrno, int full, int libs) {
	group_t dead = NOGROUP;
	int deadeye = FALSE;
	list_t ptr, ptr2;
#ifdef CHECK
	char buf[20];
	unsigned int i;
#endif
	/* a dead group - find army. all neighbors and liberties must be in the same army */
	if (S_GROUP(s) != NOGROUP && S_ALIVE(s) == DEAD) {
		army = NOARMY;
		/* look at neighbors */
		for (ptr = grnbp[board[s]]; ptr != EOL; ptr = link[ptr]) {
			if (army == NOARMY)
				army = G_ARMY(list[ptr]);
			else if (G_ARMY(list[ptr]) != army && G_ARMY(list[ptr]) != NOARMY) { /* noarmy for howmuchvital */
				army = MULTIARMY;
				break;
			}
		}
		/* look at liberties */
		if (army != MULTIARMY) {
			for (ptr = grlbp[board[s]]; ptr != EOL; ptr = link[ptr]) {
				for (ptr2 = nbgrp[list[ptr]][c]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (army == NOARMY)
						army = G_ARMY(list[ptr2]);
					else if (G_ARMY(list[ptr2]) != army && G_ARMY(list[ptr2]) != NOARMY) {
						army = MULTIARMY;
					}
				}
				if (army == MULTIARMY)break;
			}
		}
		if (army == MULTIARMY) {	/* can't expand it */
			evaldeadgroupeye(s, board[s], c, rn, ldrno, libs);
			return;
		}
	}
#ifdef CHECK
	if (army != MULTIARMY && !inbigeye(s, c, army)) {
		outerror("evalverybigeye point not in bigeye ");
		outerror(ssqr(s, buf));
		for (i = msptr >= 8?msptr-8:0; i < msptr; ++i) {
			ssqr(mvs[i], buf);
			outerr(buf);
		}
	}
#endif
	addlist(s,&eyeptr[rn]);
	
	if (board[s] != NOGROUP || army != MULTIARMY)	/* don't expand cutting point */
		expandeye(rn, s, &c, army);

	/* if only one dead group and liberties, let deadshape evaluate it - more accurate */
	for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
		if (board[list[ptr]] != NOGROUP) {
			if (dead != NOGROUP && board[list[ptr]] != dead) {
				deadeye = FALSE;
				break;
			}
			dead = board[list[ptr]];
			deadeye = TRUE;  /* exactly one dead group */
		}
	}
	if (deadeye)
		for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
			if (board[list[ptr]] == NOGROUP &&
				!inlist(list[ptr], &grlbp[dead])) {
				deadeye = FALSE;  /* part of eye is not liberty of dead group */
				break;
			}
		}
	if (deadeye) {
		killist(&eyeptr[rn]);
		evaldeadgroupeye(mvs[grpieces[dead]], dead, c, rn, ldrno, libs);
		return;
	}
	eyesforshape(rn, c, ldrno, full, libs);
	eyetype[rn] = VERYBIGEYE;
}

char minp[] = { 0,0,0,0,0,8,8, 8, 8, 8, 8, 8,16,16,20 };
char valp[] = { 0,0,0,0,0,8,8, 8, 8, 8,16,16,20,20,24 };
char potp[] = { 4,4,8,8,8,8,8,16,16,16,20,20,24,24,24 };

#ifdef NEVER

/* eye value for a shape */	

static void eyeshapeval(list_t tmplist, int *emin, int *eval, int *epot) {
	int perim, size, ends, points, canblock,sum;
	list_t perlist, ptr;
	int i,ldtmp;
	sqr_t sn;
	
	perim = 0;
	size = 0;
	ends = 0;
	points = 0;
	perlist = EOL;
	canblock = FALSE;		
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		++size;
		if (edge[list[ptr]] == 0)perim += 2;
		else if (edge[list[ptr]] == 1)perim += 1;
		i = fdir[list[ptr]];
		sum = 0; /* number of neighbors also in eye */
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = list[ptr]+nbr[i];
			if (!inlist(sn,&tmplist)) {
				if (addlist(sn,&perlist)) {
					++perim;
					if (board[sn] == NOGROUP && comlist(nblbp[sn],tmplist) == 2)
						canblock = TRUE;  /* can make block shape */
					}
				}
			else ++sum;
			}
		if (sum == 1)++ends;
		if (sum == 0)++points;
		}
	if (perim > 14)perim = 14;
	*emin = minp[perim];
	*eval = valp[perim];
	*epot = potp[perim];

	/* handle exceptions - 8 for square, L, snake 4, r pentomino */

	if (perim == 6 && points >= 2) { /* split into two diagonal point eyes */
		*epot = 16;
		*emin = 0;
		}

	if (perim == 7 && points >= 2) { /* split into two in line point eyes */
		*epot = 16;
		*emin = 0;
		}
		
	if (perim == 8 && points == 3) {  /* split into 3 one pt eyes */
		*eval = 16;
		}
	
	if (perim == 7 && size == 3 && canblock) {  /* bent 3 can make square */
		*epot = 8;
		}

	if (perim == 8) {
		if (ends == 0) {  /* 4 in square */
				*epot = 8;
			}
		else if (size == 4 && ends == 2 && !canblock)  /* snake 4 */
			*eval = 16;
		}
	else if (perim == 9) {
		if (size == 4 && !canblock)  /* L */
			*eval = 16;
		else if (ends == 3)  /* r pentomino */
			*eval = 16;
		}
	
	if ((cntlist(&tmplist) > 5) && *epot == 8)
		*epot = 12;  /* can make two eyes with two moves */

		
	killist(&perlist);
	}

#endif            
            

/* can color c place a live stone at point s.
 * s is a point next to an eye.  If c can put a live stone here
 * s must be an empty point
 * then that point is not part of a surrounded eye.
 */

int livestonehere(sqr_t s, int c, int lgd, int tld, listval_t ldrno, int libs) { 
	list_t ptr;
	int onecount = 0;
	ASSERT(board[s] == NOGROUP);
	for (ptr = nbgrp[s][1-c]; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[list[ptr]] == 1) {
			return TRUE;
		}
	}
	if (anydeadnbgrp(s, c)) {
		return FALSE;
	}
	if (lnbf[s][0] && lnbf[s][1] || 
		lgd == 3 ||
		lgd == 5 ||
		lgd == 7 ||
		lgd == 11 ||
		lgd == 2 && lnbf[s][c] == 0 ||
		lgd == 4 && tld == NOLD || 
		/* ltr1[s] == 0 && tld == NOLD && edge[s] <= 4 || makes bad ones on 4th line with no stone near */
		lnbf[s][c] != 0)
		return TRUE;
	if (lnbn[s] >= 3) {
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
			if (lnbf[list[ptr]][c] && !anydeadnbgrp(s, c)) {
				if (lnbf[list[ptr]][1-c] == 0)
					return TRUE;  /* no enemy peep here */
				onecount++;
			}
		}
	}
	if (onecount >= 2)
		return TRUE;
#ifdef NEVER
	if (distance[s][c] <= 3 && distance[s][1-c] > 5)
		return TRUE;
#endif
	if (/*distance[s][c] <= 4 && */ lnbn[s] <= 2 && NO == eyecntplyhere(s, 1-c, ldrno, libs))
		return TRUE;
	return FALSE;
}

/* what is the perimeter of this list, up to 14 */
static int perimeter(list_t eyelist) {
	int perim = 0, i, ldtmp;
	sqr_t sn;
	list_t ptr, perlist = EOL;
	for (ptr = eyelist; ptr != EOL; ptr = link[ptr]) {
		if (edge[list[ptr]] == 0)perim += 2;
		else if (edge[list[ptr]] == 1)perim += 1;
		i = fdir[list[ptr]];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = list[ptr]+nbr[i];
			if (!inlist(sn,&eyelist)) {
				if (addlist(sn,&perlist)) {
					++perim;
				}
			}
		}
	}
	killist(&perlist);
	if (perim > 14)
		perim = 14;
	return perim;
}

                       
/* look at points in rn and evaluate eye values from shape 
 * old - shape must be made of empty points!
 * shape can contain dead enemy stones, but not threatened enemy stones
 * c is color of group making eye 
 * classify by size, number of bumps, and perimiter 
 * if perimeter is more than one army, not an eye, but keep potential 
 * value of this kind of eye depends on cutting points, so
 * must update armies and connections to believe result.
 */

static int trial = 1;	/* increment each try */
static int tried[NUMSQUARES];	/* set to trial if this one tried already */
static int result[NUMSQUARES];  /* result of a trial */ 
	
static void eyesforshape(eye_t rn, int c, listval_t ldrno, int full, int libs) {
	list_t minlist = EOL;		/* minimum set of eye points for eyemin, eyeval */
	list_t potlist = EOL;		/* biggest possible eyespace if c moves first */

	list_t perlist = EOL;
	list_t ptr, ptr2, cutlist = EOL, ptr3, ptr4, nblist = EOL;
	int perim, potperim; 
	int i, ldtmp, sum = 0, ends = 0, size, potsize, count, three, points = 0, goodpoints = 0;
	int canblock = FALSE;  /* true if empty spot inside triangle of points */
	sqr_t sn, sn2, sold; 
	sqr_t bestvit = NOSQUARE, bestvit2 = NOSQUARE;  /* best and second best internal vital points */
	sqr_t threepoint;
	int bestsum = 0, bestsum2 = 0, lgd, tltr1, tmp, tltrc;
	int cutpoints = 0;	/* internal cutting points */
	int minpot = 0,minval = 0;
	int expand = 0;  /* number of expansion points */
	int expandable = FALSE;  /* can the eye be expanded? no internal vital point */
	int eyenbrs, tld, threepointval;
	int deadvital = 0;   /* number of dead stones on major vital points */
	int deadcenter = 0;	/* number of dead stones on points with at least two eyepoints adjacent */
	int deadinside;  /* number of dead stones inside this eye */
	int fullperim;  /* perimeter is all on stones */
	int atariinside = FALSE;	/* there is an atari inside the shape */
	int threatperim = FALSE;	/* threatened friendly stones on perimeter */
	int liveresult;
	group_t g;

	trial++;	/* new trial, old result invalid */
	cpylist(eyeptr[rn],&minlist);
	cpylist(eyeptr[rn],&potlist);
	if (cntlist(&minlist) < 18) {
 		for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {  /* trim bad ends */
			sum = lnbf[list[ptr]][1-c]; /* number of adjacent empty points and dead enemy stones in eye */
			three = FALSE;	/* enemy can force a move here, so not part of minimal eye shape  */
			expandable = FALSE;
			cpylist(nblbp[list[ptr]], &nblist);	/* since nblbp list changes inside loop */
			for (ptr2 = nblist; ptr2 != EOL; ptr2 = link[ptr2]) {
				sn = list[ptr2];	/* adjacent empty point - see if enemy can play here */
				/* 5/02 tighter hanging connection vital points */
				if (lnbf[sn][c] && lnbn[sn] > 1 && lnbn[sn] < 4 && lnbf[sn][1-c] == 0) { /* hanging conn eye point */
					for (ptr4 = nblbp[sn]; ptr4 != EOL; ptr4 = link[ptr4]) {
						if (lnbn[list[ptr4]] > 2)
							addlist(list[ptr4],&eyevital[rn]);
					}
				}
				if (lnbf[sn][1-c] != 0 && lnbn[sn] > 1) { /* play near enemy stone 12/02... */
					for (ptr4 = nblbp[sn]; ptr4 != EOL; ptr4 = link[ptr4]) {
						if (lnbn[list[ptr4]] > 2 && inlist(list[ptr4], &eyeptr[rn]))  
							addlist(list[ptr4],&eyevital[rn]);
					}
				}
				/* cutting points are places where enemy can play */
				if (full) {
					for (ptr3 = cnbrd[sn]; ptr3 != EOL; ptr3 = link[ptr3]) {
 						if (grarmy[cngr1[list[ptr3]]] != grarmy[cngr2[list[ptr3]]] && 
							grcolor[cngr1[list[ptr3]]] == c &&
 							!G_THREATENED(cngr1[list[ptr3]]) &&
 							!G_THREATENED(cngr2[list[ptr3]])) {
	 						dellist(sn, &minlist);
 							dellist(sn, &potlist);
	 						addlist(sn, &eyevital[rn]);
							cutpoints++;
 							for (ptr4 = nblbp[sn]; ptr4 != EOL; ptr4 = link[ptr4]) {
 								dellist(list[ptr4],&minlist);
 								if (lnbf[sn][1-c] == 0 &&
 									lnbn[sn] > 1)
 									addlist(sn,&eyevital[rn]);
 							}
						}
					}
				}
				if (full && cnbrd[sn] == EOL) {  /* remove cut points from eye list */
					for (ptr3 = lkbrd[sn]; ptr3 != EOL; ptr3 = link[ptr3]) {
 						if (cncnum[list[ptr3]] == 0 &&
 							grarmy[cngr1[list[ptr3]]] != grarmy[cngr2[list[ptr3]]] && 
							grcolor[cngr1[list[ptr3]]] == c &&
 							!G_THREATENED(cngr1[list[ptr3]]) &&
 							!G_THREATENED(cngr2[list[ptr3]])) {
			 				if (cntype[list[ptr3]] == CN_TWOPOINTJUMP) {
								addlist(sn, &eyevital[rn]);
 								dellist(sn, &minlist);
	 							dellist(sn, &potlist);
 								for (ptr4 = nblbp[sn]; ptr4 != EOL; ptr4 = link[ptr4])
 									dellist(list[ptr4],&minlist);
 								}
							else if (cntype[list[ptr3]] == CN_KNIGHTSMOVE) {
								knightcutpoints(list[ptr3], &cutlist);  /* find cutting points */
								dellist(sn,&minlist);
							}
 						}
					}
				}
				/* done with cutting pionts */
				tld = getldval(sn);
				lgd = getltrgd(sn,FALSE,&tltr1, &tltrc);
				if (lgd || tltr1 == 0 && tld == NOLD) {
					expandable = TRUE;
					if (lnbf[list[ptr]][c] > 1 && 
						lnbf[list[ptr]][1-c] == 0 &&
						(edge[sn] > 1 || edge[list[ptr]] > 1 && lnbn[list[ptr]] <= 2)) { /* added 9/95 all ow on edge from daviesld problem 36 */
						addlist(sn,&eyevital[rn]);
					}
				}
				if (inlist(sn,&eyeptr[rn]))
					++sum;
				if (tried[sn] == trial)
					liveresult = result[sn];
				else {
					liveresult = livestonehere(sn, 1-c, lgd, tld, ldrno, libs);  /* 10/05 add else for speed */
					tried[sn] = trial;
					result[sn] = liveresult;
				}
				if (liveresult) {  /* 10/05 add else for speed */
					three++;  /* number of adjacent points where enemy can play */
					threepoint = sn;
					threepointval = lgd;
					if (lgd & 2) { /* 2 or 3 or  6 find highest blocking point */
						for (ptr3 = nblbp[sn]; ptr3 != EOL; ptr3 = link[ptr3]) {
							if ((getltrgd(list[ptr3], FALSE,&tmp, &tmp) & 2) &&
								edge[list[ptr3]] > edge[sn]) {
								sold = list[ptr3];
								for (sn2 = list[ptr3]; 
									edge[sn2] > 0 &&
										board[sn2] == NOGROUP &&
										(getltrgd(sn2,FALSE, &tmp, &tmp)&2) && 
										lnbf[sn2][1-c] == 0; 
									sn2 = sn2 + list[ptr3]-sn)
									sold = sn2;
								if (board[sn2] == NOGROUP &&
									lnbf[sn2][1-c] != 0)
									addlist(sn2,&eyevital[rn]);
								if (!(getltrgd(sn2,FALSE,&tmp, &tmp)&2)) {
									addlist(sold, &eyevital[rn]);
									}
								for (ptr4 = nblbp[sold]; ptr4 != EOL; ptr4 = link[ptr4])
									if (edge[list[ptr4]] == edge[sold] && 
										lnbn[list[ptr4]] == 4)
										addlist(list[ptr4], &eyevital[rn]);
							}	
						}
					}
					if (lgd == 2 && edge[sn] == edge[list[ptr]]) {
						if (lnbf[list[ptr]][c]) { /* extend eye along the line above */
							for (ptr3 = nblbp[sn]; ptr3 != EOL; ptr3 = link[ptr3])
								if (edge[list[ptr3]] >= edge[sn]) {
									if (S_COLOR(list[ptr3]+sn-list[ptr]) != 1-c)
										addlist(list[ptr3],&eyevital[rn]);
									else
										addlist(sn,&eyevital[rn]);
								}
						}
#ifdef NEVER							
						else
							addlist(sn,&eyevital[rn]);  /* causes horrible vital points on edge */
#endif							
					}
					if ((lgd == 3 || lgd == 7)) {
						if (lnbf[sn][c] != 0) {
							for (ptr3 = nblbp[sn]; ptr3 != EOL; ptr3 = link[ptr3])
								if (lnbn[list[ptr3]] == 3 && lnbf[list[ptr3]][1-c] != 0)
									addlist(list[ptr3],&eyevital[rn]);
						}
						if (board[list[ptr]] == NOGROUP && lnbf[list[ptr]][1-c] == 0 &&
							lnbn[list[ptr]] >= 3)
							addlist(list[ptr], &eyevital[rn]);
					}
					if (lnbf[sn][1-c] && gralive[lgr[sn]] != DEAD ||
						tltr1 != 0 && lnbf[sn][c] != 0 ||
						lgd >= 3 && lnbn[sn] == 4) {
						addlist(sn,&eyevital[rn]);
						for (ptr3 = nblbp[sn]; ptr3 != EOL; ptr3 = link[ptr3])
							if (edge[list[ptr3]] == edge[sn] && edge[sn] > 1 &&
								lnbn[list[ptr3]] == 4)  /* jump out one line on edge */
								addlist(list[ptr3], &eyevital[rn]);
					}
				}
			}
			killist(&nblist);
			if (expandable)expand++;  /* can expand eye here */
			if (three) {  /* spot next to ltrgd 3, opponent can force move here */
#ifdef NEVER
				if (board[list[ptr]] == NOGROUP && sum*4 > bestsum && sum > 1) {
					bestvit = list[ptr];
					bestsum = sum*4;
				}
#endif
				dellist(list[ptr],&minlist);
				if (edge[list[ptr]] == 1 && lnbn[list[ptr]] == 3 &&
					(three > 1 || (threepointval & 3) == 2)) {	/* trim back one point on edge for hane */
					for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
						if (edge[list[ptr2]] <= 1)
							dellist(list[ptr2], &minlist);
				}
				continue;
			}
			if (board[list[ptr]] == NOGROUP) {
				for (ptr2 = nbgrp[list[ptr]][1-c]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (grthreatened[list[ptr2]]) {	/* 4/01 point next to threatened group must be filled eventually */
						three = TRUE;
					}
					if (gralive[list[ptr2]] == DEAD && !inlist(mvs[grpieces[list[ptr2]]], &eyeptr[rn]))
						three = TRUE;	/* dead group is a different eye that can extend to this spot ld1e:42 */
				}
				if (three) {
					dellist(list[ptr], &minlist);
					continue;
				}
			}
			lgd = getltrgd(list[ptr],FALSE,&tltr1, &tltrc);
 			if (board[list[ptr]] == NOGROUP &&
				(tltr1 != 0 && tltrc == c && lgd > 1 ||
				 /* lnbf[list[ptr]][1-c] != 0 ||  8/99 can't happen unless stone is dead */ 
				tltr1 == 0 && lnbn[list[ptr]] > 3 && distance[list[ptr]][1-c] < BIGEYEMINDIST)) {
 				dellist(list[ptr],&minlist);
 				continue;  /* not surrounded point */
 			}
			if (sum <= 1 /* && board[list[ptr]] == NOGROUP*/ ) {  /* found bump next to empty point */
				if (lnbn[list[ptr]] >= 3)  /* don't add spurious vital points */
					count = 3;  /* opponent can peep at eye */
				else
					count = getcount(list[ptr],c,rn,TRUE,ldrno,libs);
				if (lnbn[list[ptr]] + lnbf[list[ptr]][1-c] == 1) {  /* closed end */
					if (minpot < opt1[count])
						minpot = opt1[count];
					if (count <= 5) {  /* he can force to connect */
						dellist(list[ptr],&minlist);
						dellist(list[ptr],&potlist);
					}                             
					else 
						minval = 8;
					if (count == 0) {   /* can throw in */
						if (board[list[ptr]] == NOGROUP)
							addlist(list[ptr],&eyevital[rn]);
						for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
							if (inlist(list[ptr2],&eyeptr[rn])) {
								dellist(list[ptr2],&minlist); 
							}
					}
				}
				else {
					if (count < 11)
						dellist(list[ptr],&minlist);  /* open end - can peep */
					if (count == 0) {  /* can throw in */
						if (lnbn[list[ptr]] < 4 && board[list[ptr]] == NOGROUP)
							addlist(list[ptr],&eyevital[rn]);
						for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
							if (inlist(list[ptr2],&eyeptr[rn])) {
								dellist(list[ptr2],&minlist);  /* can throw in */
							}
					}
				}
			}
		}
	}
	/* minlist is the minimum set of eyepoints.  find eye value */
	
			
	size = cntlist(&minlist);
	potsize = cntlist(&potlist);
	ends = 0;
	perim = 0;
	points = 0;	 /* single point eyes that may be false */
	goodpoints = 0;  /* single point good eyes */
	deadinside = 0;
	fullperim = TRUE;  /* all perimeter is on stones */
	perlist = EOL;
	canblock = FALSE;		
	for (ptr = minlist; ptr != EOL; ptr = link[ptr]) {
		if (board[list[ptr]] == NOGROUP) {
			if (edge[list[ptr]] == 0)
				mrglist(nblbp[list[ptr]], &eyevital[rn]);	/* 5/01 always add 1-2 points */
			for (ptr2 = nbgrp[list[ptr]][c]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr2]]]]))
					adflist(mvs[grpieces[list[ptr2]]],&grldr[ldrno]);
				if (grlibs[list[ptr2]] == 2 && lnbn[list[ptr]] > 1)
					atariinside = TRUE;
				if (grlibs[list[ptr2]] <= 2)  /* internal atari or capture */
					if (board[list[ptr]] == NOGROUP)
						addlist(list[ptr], &eyevital[rn]);
					else if (grlibs[board[list[ptr]]] == 1)
						addlist(list[grlbp[board[list[ptr]]]], &eyevital[rn]);
				}
			}
		else
			deadinside++;
		sum = 0; /* number of neighbors also in eye */
		eyenbrs = 0;  /* neighbors in eye with lnbn == 2 */
		if (edge[list[ptr]] == 0)perim += 2;
		else if (edge[list[ptr]] == 1)perim += 1;
		i = fdir[list[ptr]];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = list[ptr]+nbr[i];
			if (lnbn[sn] == 2)
				++eyenbrs;
			if (!inlist(sn,&minlist)) {
				if (addlist(sn,&perlist)) {
					perim++;
					if (board[sn] == NOGROUP)
						fullperim = FALSE;
					else if (S_NUMLIBS(sn) == 1) {
						threatperim = TRUE;
						addlist(list[grlbp[board[sn]]], &eyevital[rn]);
						}
					if (board[sn] == NOGROUP && comlist(nblbp[sn],minlist) == 2)
						canblock = TRUE;  /* can make block shape */
					}
				}
			else {
				++sum;
				}
			}
		if (eyenbrs >= 2) { /* too good to pass up shape */
			if (board[list[ptr]] == NOGROUP)
				addlist(list[ptr],&eyevital[rn]);  /* internal vital point */
			else
				deadvital++;
			}
		if (sum >= 2 && board[list[ptr]] != NOGROUP)
			++deadcenter;
		if (sum*4+eyenbrs > bestsum && sum > 1) {
			bestsum2 = bestsum;
			bestvit2 = bestvit;
			bestsum = sum*4+eyenbrs;
			bestvit = list[ptr];
			}
		else if (sum*4+eyenbrs > bestsum2 && sum > 1) {
			bestsum2 = sum*4+eyenbrs;
			bestvit2 = list[ptr];
			}
		if (board[list[ptr]] == NOGROUP && edge[list[ptr]] == 1 && edge2[list[ptr]] == 2 && sum >= 1) {
			addlist(list[ptr],&eyevital[rn]);  /* internal vital point at 1-2 point */
			}
		if (sum == 1)++ends;
		if (sum == 0 && lnbf[list[ptr]][1-c] == 0 && lnbn[list[ptr]] == 0) {  /* completely surrounded points */
			if (getcount(list[ptr],c,rn,TRUE,ldrno,libs) < 10)
				++points;
			else
				++goodpoints;
			}
		}

	if (perim > 14)
		perim = 14;
	if (size == 1) {	/* can be reduced to a single point */
		count = getcount(list[minlist],c,rn,TRUE,ldrno,libs);
		eyepot[rn] = opt1[count];
		eyeval[rn] = opt2[count];
		eyemin[rn] = opt3[count];
		if (board[list[minlist]] == NOGROUP && lnbf[list[minlist]][1-c] == 1 &&
			grsize[list[nbgrp[list[minlist]][1-c]]] == 1 &&
			grlibs[list[nbgrp[list[minlist]][1-c]]] <= 2) {	/* 2/03 can sacrifice two stones to remove this eye */
			eyevalmax[rn] = eyeval[rn];  /* perhaps eye can be killed by playing at adjacent point */
			eyeval[rn] = 0;
			eyemin[rn] = 0;
		}
		if (board[list[minlist]] == NOGROUP && lnbn[list[minlist]] == 1) {
			sn = list[nblbp[list[minlist]]];
			if (lnbf[sn][1-c] == 1) {
				g = list[nbgrp[sn][1-c]];
				if (gralive[g] == DEAD && grsize[g] <= 2 && !inlist(mvs[grpieces[g]], &eyeptr[rn])) {
					eyevalmax[rn] = eyeval[rn];  /* perhaps eye can be killed by playing at adjacent point */
					eyeval[rn] = 0;
					eyemin[rn] = 0;
				}
			}
		}
		if (!fullperim)
			eyemin[rn] = 0;	/* two moves can destroy eye */
	}
	else {
		eyemin[rn] = minp[perim];
		eyeval[rn] = valp[perim];
		eyepot[rn] = potp[perim];
		if (perim < 14 && atariinside)
			eyemin[rn] = 8;
		if (perim < 14 && atariinside && eyeval[rn] > 16)
			eyeval[rn] = 16;
	}
	if (!fullperim && potsize > 1) {
		potperim = perimeter(potlist);
		if (eyepot[rn] < valp[potperim])
			eyepot[rn] = valp[potperim];				/* correct for eyes that shrink a lot */
	}

	if (eyepot[rn] < minpot)
		eyepot[rn] = minpot;
	if (eyevalmax[rn] < minval)
		eyevalmax[rn] = minval;

	/* handle exceptions - 8 for square, L, snake 4, r pentomino */

	if (points > 1 && size < 5 && perim > 9) {
		eyeval[rn] = 12; /* points may be false - don't count two eyes */
		eyevalmax[rn] = 16;
	}

	if (size == 3 && goodpoints >= 1) {
		eyevalmax[rn] = 16;	/* single point and 2 point separated eyes */
	}
	else if (size == 3 && deadinside == 2 && board[bestvit] == NOGROUP && fullperim && !threatperim) {  /* 3 can't connect */
		eyeval[rn] = 16;
	}

	if (perim == 6 && goodpoints >= 2) { /* split into two diagonal point eyes */
		eyepot[rn] = 16;
		eyemin[rn] = 0;
	}

	if (perim == 7 && points >= 2) { /* split into two in line point eyes */
		eyepot[rn] = 16;
		eyemin[rn] = 0;
	}
		
	if (perim == 8 && points == 3) {  /* split into 3 one pt eyes */
		eyeval[rn] = 16;
	}
	
	if (perim == 7 && size == 3 && canblock) {  /* bent 3 can make square */
		eyepot[rn] = 8;
	}

	if (perim == 8) {
		if (ends == 0) {  /* 4 in square */
			if (cntlist(&eyeptr[rn]) > 4)
				eyepot[rn] = 12;
			else 
				eyepot[rn] = 8;
		}
		else if (size == 4 && ends == 2 && !canblock && 
			board[bestvit] == NOGROUP && board[bestvit2] == NOGROUP) {  /* snake 4 */
			if (atariinside) {
				eyeval[rn] = 8;
				eyevalmax[rn] = 16;
			}
			else
				eyeval[rn] = 16;
		}
	}
	else if (perim == 9) {
		if (size == 4 && ends == 2 && !canblock &&
			board[bestvit] == NOGROUP && board[bestvit2] == NOGROUP)  /* L */
			eyeval[rn] = 16;
		else if (ends == 3) {
			if (board[bestvit] == NOGROUP && board[bestvit2] == NOGROUP)  /* r pentomino */
				eyeval[rn] = 16;
			else if (board[bestvit] != NOGROUP && board[bestvit2] != NOGROUP)
				eyepot[rn] = 8;
		}
		else if (size == 5 && board[bestvit] != NOGROUP)
			eyepot[rn] = 8;	/* bulky 5 with stone already on the vital point */
		else if (size == 5 && !deadvital && fullperim && deadinside == 4)
			eyeval[rn] = 16;  /* bulky 5, almost full, can't play on vital */
		else if (size == 4 && !deadvital && fullperim && deadinside == 3)
			eyeval[rn] = 16;
	}
	else if (perim == 10 && size == 6) {  /* block of 6 */
		if (S_COLOR(bestvit) == 1-c && S_COLOR(bestvit2) == 1-c) {
			eyeval[rn] = 8;
			eyepot[rn] = 8;
		}
		else if (S_COLOR(bestvit) == 1-c || S_COLOR(bestvit2) == 1-c) {
			eyeval[rn] = 8;
			eyepot[rn] = 16;
		}
		else {
			eyeval[rn] = 12;  /* usually only one eye */
			eyevalmax[rn] = 16;
		}
	}
	else if (perim == 10 && size == 5 && ends > 2) {  /* pyramid with one extra point, not a U shaped line of 5 */
		if (board[bestvit] != NOGROUP && board[bestvit2] != NOGROUP) {
			eyeval[rn] = 8;
			eyepot[rn] = 8;
		}
		else if ( board[bestvit] != NOGROUP || board[bestvit2] != NOGROUP) {
			eyeval[rn] = 8;
			if (board[bestvit] != NOGROUP)
				eyepot[rn] = 16;
		}
	}
	else if (perim == 10 && size == 4 && deadcenter) {  /* line of 4 with dead stone in middle */
		eyeval[rn] = 8;
		if (deadcenter == 1)
			eyepot[rn] = 16;
		else
			eyepot[rn] = 8;
	}
	else if (perim == 11 && size == 7 && cntlist(&eyeptr[rn]) < 10) {  /* blocky 7 */
		eyeval[rn] = 12;  /* usually only on eye for this shape */
		eyevalmax[rn] = 16;
	}
	else if (perim == 11 && size == 6 && deadinside >= 2) {
		if (bestvit != NOSQUARE) {
			if (S_COLOR(bestvit) == NOCOLOR && lnbf[bestvit][c] == 0 &&
				lnbf[bestvit][1-c] >= 2) {
				if (lnbf[bestvit][1-c] == 3 && deadinside == 3) {
					eyepot[rn] = 8;
					eyeval[rn] = 8;
				}
				else if (lnbf[bestvit][1-c] == 3) {
					eyepot[rn] = 16;
					eyeval[rn] = 8;
				}
				else if (lnbf[bestvit][1-c] == 2 && deadinside == 2) {
					eyepot[rn] = 16;
					eyeval[rn] = 8;
				}
			}
			else if (S_COLOR(bestvit) == 1-c) { /* already stone on vital point to leave bulky 5 */
				eyemin[rn] = 8;
				eyeval[rn] = 8;
			}
		}
	}
	else if (perim == 11 && size == 5 && (deadinside == 2 || deadinside == 3) && deadcenter == 2 &&
		board[bestvit] == NOGROUP && edge[bestvit] == 0) {  /* bent-4 - 5 in corner with 2 dead stones inside */
		eyeval[rn] = eyemin[rn] = 8;
		if (lnbn[bestvit] == 0) {  /* 5/02 corner has 2 enemy stones adjacent */
			eyepot[rn] = 8;
			killist(&eyevital[rn]);  /* 5/02 - no vital points here */
		}
		else if (lnbn[bestvit] == 1 && lnbn[list[nblbp[bestvit]]] == 1 &&
			lnbf[list[nblbp[bestvit]]][1-c] == 0) {  /* ko */
			eyepot[rn] = 16;
			eyeko[rn] = 1;
		}
		else {
			eyepot[rn] = 16;
		}
	}
	else if (perim == 11 && size == 5 && deadinside == 1 && deadcenter == 1 &&
		board[bestvit] == NOGROUP && edge[bestvit] == 0 && lnbn[bestvit] == 1 &&
		lnbf[bestvit][1-c] == 1 && lnbn[list[nblbp[bestvit]]] == 2) {  /* bent-4 - 5 in corner with 1 dead stone inside */
		eyeval[rn] = eyemin[rn] = 8;
		eyepot[rn] = 16;
		addlist(list[nblbp[bestvit]], &eyevital[rn]);
	}
	
	if ((cntlist(&minlist) > 5 || cntlist(&eyeptr[rn]) > 5) && eyepot[rn] == 8)
		eyepot[rn] = 12;  /* can make two eyes with two moves */

	if (bestvit != NOSQUARE && bestsum >= 8 && board[bestvit] == NOGROUP &&
		expand < 4 &&  /* no internal vital if can expand eye */
		(eyepot[rn] > eyeval[rn] && eyeval[rn] < 24 ||
		 eyepot[rn] > 0 && cutpoints))
		addlist(bestvit,&eyevital[rn]);  /* internal vital point */

#ifdef NEVER
	5/02 - these special cases are already handled better above
	/* extra value for moving first */		
	eyeshapeval(potlist,&emin,&eval,&epot);
	if (eval > eyepot[rn])
		eyepot[rn] = eval;
	if (epot > eyepot[rn])
		eyepot[rn] += (epot-eyepot[rn])/2;
#endif	

	for (ptr = perlist; ptr != EOL; ptr = link[ptr])
		if (board[list[ptr]] == NOGROUP &&
			!inlist(list[ptr],&eyeptr[rn]) &&
			edge[list[ptr]] > 4 &&
			lnbn[list[ptr]] == 4) {
			addlist(list[ptr],&eyevital[rn]);
		}
		
	killist(&perlist);
	killist(&minlist);
	killist(&potlist);

	if (eyemin[rn] < 16 && eyevital[rn] == EOL ||
		eyeval[rn] < 24 && eyepot[rn] > eyeval[rn] ||
		eyepot[rn] < 16 && cntlist(&eyeptr[rn]) > size+1) {  /* 5/02 include vital points if eye is bigger than it appears */
		for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
			if (lnbn[list[ptr]] == 1)
				addlist(list[nblbp[list[ptr]]],&eyevital[rn]);
#ifdef NEVER				
			if (lnbn[list[ptr]] == 4 && size > 2)
				addlist(list[ptr],&eyevital[rn]);
#endif				
		}
	}
	if (cutpoints) {  /* he can cut and remove the entire eye! */
		eyevalmax[rn] = eyeval[rn];
		if (eyeval[rn] >= 16) {
			eyeval[rn] = 12;
			eyemin[rn] = 8;
		}
		if (eyepot[rn] > 0) {
			for (ptr = cutlist; ptr != EOL; ptr = link[ptr])
				addlist(list[ptr],&eyevital[rn]);
		}
	}
	killist(&cutlist);
}

extern int dirnm[52], opdir[4];

/* return the ltrgd value for this point, and set ltr1 nonzero if this point is between edge and a liberty 
 and ltrc to color of territory */

int getltrgd(sqr_t s, int dump, int *ltr1, int *ltrc) {
	int dir;  /* toward center */
	int diralt, dir2alt;
	int dir2;  /* along edge */
	int udir;  /* along each edge */
	int udiralt;
	int tltr = -1, tltr2 = -1;
	int cleartoedge;
	int dist;
	sqr_t ttpt;	/* three-three point */
	sqr_t s2, s3;
#ifdef G2DEBUGOUTPUT
	char buf[80];
#endif
	*ltr1 = 0;
	*ltrc = NOCOLOR;
	if (edge[s] > 4)
		return 0;  /* always good in center... */
	if (board[s] != NOGROUP)
		return 0;	/* to preventproblems... */
	if (edge[s] != edge2[s]) {
		dir = nbr[fdir[s]+2]; /* toward center */
		dir2 = nbr[fdir[s]+1];  /* along edge */
		udir = dirnm[fdir[s]+1];
		if (edge2[s+nbr[fdir[s]]] > edge2[s]) {
			diralt = nbr[fdir[s]];  /* alternate toward center dir */
		}
		else {
			diralt = nbr[fdir[s]+1];
		}
		dir2alt = nbr[fdir[s]+2];
		udiralt = dirnm[fdir[s]+2];
	}
	else { /* on diagonal */
		dir = nbr[fdir[s]+1]; /* toward center (first one) */
		dir2 = nbr[fdir[s]];  /* along edge */
		udir = dirnm[fdir[s]];
		diralt = nbr[fdir[s]];  /* alternate toward center dir */
		dir2alt = nbr[fdir[s]+1];
		udiralt = dirnm[fdir[s]+1];
	}

	s2 = s;
	while (edge[s2] < 4 && /* stop with s2 on 4th line max */
		(edge2[s] >= 4 || edge2[s2] < 4) && 
		lnbf[s2][0] == 0 && lnbf[s2][1] == 0)
		s2 = s2 + dir;

	if (lnbf[s2][0] != 0 && lnbf[s2][1] == 0)
		*ltrc = 0;
	else if (lnbf[s2][0] == 0 && lnbf[s2][1] != 0)
		*ltrc = 1;
		
	cleartoedge = TRUE;
	s3 = s2;
	dist = 1;
	for (;;) {
		if (board[s3] != NOGROUP) {
			cleartoedge = FALSE;
			break;
		}
		if (edge[s3] == 0 || edge[s3] == 1 && edge[s3+dir] > 1)
			break;
		s3 -= dir;
		dist++;
	}
		
	if (cleartoedge && (lnbf[s2][0] != 0 || lnbf[s2][1] != 0)) {
		tltr = findltrgd(s2, dir, dir2, udir, opdir[udir], dist);  /* out from near edge */
		if (s2 == s && (tltr&31) != 1 && (tltr&32) && !S_NEUTRAL2(s))
			tltr |= 3;
		tltr &= 31;
		if (S_NEUTRAL2(s) && gralive[lgr[s]] != DEAD)
			tltr |= 2;
		if (lnbn[s] == 4 || edge[s] == 1 && lnbn[s] == 3 || edge[s] == 0 && lnbn[s] == 2)
			*ltr1 = 9;
		else if (edge[s] == 0)
			*ltr1 = 1;
		else
			*ltr1 = edge[s];
#ifdef G2DEBUGOUTPUT 
		if (dump) {
			sprintf(buf, "1st getltrgd, at %d (%d), dir %d, dir2 %d, udir %d, udir2 %d, findltrgd %d\n",s,s2,dir,dir2,udir,opdir[udir],tltr);
			outerr(buf);
		}
#endif
		}
	if (edge2[s] <= 4) {  /* in corner, must look both ways */
		s2 = s;
		while (edge[s2] < 4 && /* stop with s2 on 4th line max */
			(edge2[s] > 4 || edge2[s2] < 4) && 
			lnbf[s2][0] == 0 && lnbf[s2][1] == 0)
			s2 = s2 + diralt;

		if (lnbf[s2][0] != 0 && lnbf[s2][1] == 0)
			*ltrc = 0;
		else if (lnbf[s2][0] == 0 && lnbf[s2][1] != 0)
			*ltrc = 1;

		cleartoedge = TRUE;
		s3 = s2;
		dist = 1;
		for (;;) {
			if (board[s3] != NOGROUP) {
				cleartoedge = FALSE;
				break;
			}
			if (edge[s3] == 0 || edge[s3] == 1 && edge[s3+diralt] > 1)
				break;
			s3 -= diralt;
			dist++;
		}
			
		if (cleartoedge && (lnbf[s2][0] != 0 || lnbf[s2][1] != 0)) { /* foud stone other way */
			tltr2 = findltrgd(s2, diralt, dir2alt, udiralt, opdir[udiralt], dist);  /* out from second nearest edge */
			if (s2 == s && (tltr2&31) != 1 && (tltr2&32) && !S_NEUTRAL2(s))
				tltr2 |= 3;
			tltr2 &= 31;
			if (S_NEUTRAL2(s) && gralive[lgr[s]] != DEAD)
				tltr |= 2;
			if (lnbn[s] == 4 || edge[s] == 1 && lnbn[s] == 3 || edge[s] == 0 && lnbn[s] == 2)
				*ltr1 = 9;
			else if (edge[s] == 0)
				*ltr1 = 1;
			else
				*ltr1 = edge[s];
#ifdef G2DEBUGOUTPUT 
			if (dump) {
				sprintf(buf, "2nd getltrgd, at %d (%d), dir %d, dir2 %d, udir %d, udir2 %d, findltrgd %d\n",s,s2,diralt,dir2alt,udiralt,opdir[udiralt],tltr2);
				outerr(buf);
			}
#endif
			if (tltr == -1)
				tltr = tltr2;
			else if (tltr > 1 || tltr2 > 1)
				tltr = tltr | tltr2;
			else if (tltr == 1 && tltr2 != 0 || tltr != 0 && tltr2 == 1)
				tltr = 1;
			else
				tltr = 0;
		}
	}
	if (*ltrc == NOCOLOR && edge[s] == 0 && *ltr1 == 0) {  /* 5/02 corner point matches the 2-2 point */
		s2 = s + diags[fdir[s]];
		tltr = getltrgd(s2, dump, ltr1, ltrc);
	}
	if (tltr == -1) {
		if (edge2[s] <= 2) {	/* fix up corner, 1-2, and 2-2 points */
			if (edge[s] == 0)
				ttpt = s + 2*dir + 2*dir2;
			else if (edge[s] == 2)
				ttpt = s + dir + dir2;
			else 
				ttpt = s + 2*dir + diralt;
			if (board[ttpt] == NOGROUP && (lnbf[ttpt][0] && !lnbf[ttpt][1] || lnbf[ttpt][1] && !lnbf[ttpt][0])) {
				tltr = getltrgd(ttpt, dump, ltr1, ltrc);	/* use value from 3-3 point */
			}
			else {
				tltr = 0;
			}
		}
		else 
			tltr = 0;
	}
	return tltr;
}

/* return the LD value for a point s.  Since howmuchvital uses lupdate,
 * the ld[] data structure is not updated always in g2eye.  This gives the
 * correct value;
 */

int getldval(sqr_t s) {
	int tld,c;
	list_t ptr;
	if (board[s] != NOGROUP)
		return NOLD;
	if (lnbf[s][0] == 0 && lnbf[s][1] == 0)
		return NOLD;
	if (lnbf[s][0] != 0 && lnbf[s][1] != 0)
		return NEUTRALLD;
	c = lnbf[s][0] == 0;
	tld = lnbf[s][c] * 2;
	if (edge[s] == 1)
		tld += 2;
	else if (edge[s] == 0)
		tld += 4;
	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
		if (lnbf[list[ptr]][c] != 0 && lnbf[list[ptr]][1-c] == 0)
			tld++;
	return tld;	
	}
    
/* TRUE if this point is part of a very big eye for color c 
 * big eyes are large surrounded areas, and may include dead stones
 */    
    
static int inbigeye(sqr_t s, int c, army_t army)
{
	int tld, tltrgd, tltr1,tltrc;
	int deadcount, livecount;
	list_t ptr, ptr2;
	/* part of enemy dead group.  A single dead group will be a dead eye instead */
	if (board[s] != NOGROUP) {
		if (grcolor[board[s]] == 1-c &&
			gralive[board[s]] == DEAD) {
			if (army == NOARMY)
				return TRUE;
			for (ptr = grnbp[board[s]]; ptr != EOL; ptr = link[ptr]) {
				if (G_ARMY(list[ptr]) != army && G_ARMY(list[ptr]) != NOARMY)
					return FALSE;
			}
			return TRUE;
		}
		return FALSE;
	}
	/* not next to friendly dead group or friendly group of another army */
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
		if (gralive[list[ptr]] == DEAD) {
			return FALSE;  /* next to friendly stone */
		}
		/* all points must belong to army (or NOARMY for howmuchvital lookahead) */
		if (army != NOARMY && G_ARMY(list[ptr]) != army && G_ARMY(list[ptr]) != NOARMY)
			return FALSE;
	}
	/* point next to enemy dead group, and not next to other enemy group and next to friendly group */
	if (lnbf[s][1-c] != 0) {
		deadcount = livecount = 0;
		/* 9/99 next to other enemy groups OK as long as two or more dead ones */
		/* 8/00 one enemy dead group is OK since liberties of dead stones are included */
		for (ptr = nbgrp[s][1-c]; ptr != EOL; ptr = link[ptr]) {
			if (gralive[list[ptr]] != DEAD)
				livecount++;
			else {
				deadcount++;
				if (army != NOARMY) {
					for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (G_ARMY(list[ptr2]) != army && G_ARMY(list[ptr2]) != NOARMY) {
							deadcount--;
							break;
						}
					}
				}
			}
		}
		if (livecount != 0 && deadcount == 1) /* 1/02 need to merge dead groups, even if live stone there */
			return FALSE;	/* 6/01 can't be an eye if it is next to a live enemy stone */
		if (deadcount >= 1)
			return TRUE; /* lnbf[s][c] != 0;  /* enemy neighbors are dead stones, must have a friendly stone */
		if (deadcount == 0)  /* livecount must be nonzero, or dead group has bad neighbor army */
			return FALSE;
	}
	tld = getldval(s);
	tltrgd = getltrgd(s,FALSE,&tltr1,&tltrc);
	/* 5/02 ld1e problem 180, ltrgd == 3 on inside is still potential eye space */
	/* can't use cut as part of this since it is not updated for vital point evaluation */
	/* even if enemy can play here it is still part of potential eye */
	if (tltr1 != 0 && (tltrgd&3) == 3 && lnbn[s] > 2 && distance[s][1-c] <= 3)
		return FALSE;  /* enemy can play here */
	if (tltr1 != 0 && tltrgd == 1 && lnbn[s] > 2 && distance[s][1-c] < 10)
		return FALSE;  /* 5/02 enemy can play next to here - outside point */
#ifdef NEVER
	2/01 need eye here when can complete eye on 2nd line with enemy stone nearby, graded2l, prob 34
	if (tltr1 != 0 && (tltrgd&4) && tld <= 4)
		return FALSE;	/* 11/00 */
#endif
	if (lnbn[s] > 2) { /* 11/00 in case tv_pot plays a stone to make connection, but wasn't connection before */
		/* need to be > 2 since line eyes include cutting points and big eyes must be superset of line eyes */
		for (ptr2 = cnbrd[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (grarmy[cngr1[list[ptr2]]] != grarmy[cngr2[list[ptr2]]] && 
				!G_THREATENED(cngr1[list[ptr2]]) &&
				!G_THREATENED(cngr2[list[ptr2]]))
				return FALSE;	/* cutting points not part of eyespace */
		}
	}

	if (lnbf[s][c] == 0 && closest[s][c] != NOSQUARE && army != NOARMY && S_ARMY(closest[s][c]) != army)
		return FALSE;	/* eye only for one army - check far away points*/
	return(distance[s][c] <= 2 && distance[s][1-c] >= BIGEYEDIST ||  /* 8/06 10 to 6. change eyelist stuff in g2dist if this line changes */
		/*distance[s][c] == 40 && distance[s][1-c] == 40 || 8/00 eyerec allocation problem when two nearby dead groups of opposite color /* was under captured stones in howmuchvital */
		tltr1 != 0 && tltrc == c && tltrgd == 0 ||
		edge[s] == 0 && tltr1 != 0 && tltrgd == 1 ||  /* continue into corner point */
		tld >= 5 && tld <= 8 ||  /* 8/97 changed 5 to 4 for fost round 1 game big group */
		tld == 4 && edge[s] > 1 || /*|| ltrgd[s] == 0) || 11/00 redundant - ltrgd only valid if ltr1 nonzero */ /* 9/99 graded1c problem 47 G13 move */
	    lnbn[s] <= 2 && tld != 0 &&  /* not after an lupdate capture */
	       (edge[s] == 2 || edge[s] == 0 && S_COLOR(s+diags[fdir[s]]) == c));
}

/* expand big eye to include all points inbigeye()
 * and and point in an adjacent dead group
 * points not next to a stone must be part of the same army
 */

static void expandeye(eye_t rn, sqr_t s, int *c, army_t army)
{ 
	list_t ptr, ptr3;
	int ptr2, deadok, multi;
	army_t a;
	if (*c == NOCOLOR && board[s] != NOGROUP) {
#ifdef G2DEBUGOUTPUT
		if (gralive[board[s]] != DEAD)
			outerror("nondead group in expandeye");
#endif
		*c = 1-grcolor[board[s]];
	}
	else if (*c == NOCOLOR && (!lnbf[s][0] || !lnbf[s][1])) {
		for (ptr = nbgrp[s][0]; ptr != EOL; ptr = link[ptr])
			if (gralive[list[ptr]] == DEAD)
				*c = 1;
		for (ptr = nbgrp[s][1]; ptr != EOL; ptr = link[ptr])
			if (gralive[list[ptr]] == DEAD)
				*c = 0;
		if (*c == NOCOLOR)
			*c = lnbf[s][1] != 0;
	}
	/* expand into neighboring empty points */
	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
		if (inlist(list[ptr], &eyeptr[rn]))continue;  /* already counted */
		multi = FALSE;
		a = army;
		for (ptr2 = nbgrp[list[ptr]][*c]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (a == NOARMY)
				a = G_ARMY(list[ptr2]);
			else if (G_ARMY(list[ptr2]) != a && G_ARMY(list[ptr2]) != NOARMY) {
				multi = TRUE;
				break;
			}
		}
		if (!multi && inbigeye(list[ptr], *c, army)) {
			addlist(list[ptr], &eyeptr[rn]);
			expandeye(rn, list[ptr], c, army);
		}
	}
	/* expand onto neighboring dead enemy groups */
	if (board[s] == NOGROUP) {
		for (ptr = nbgrp[s][1-*c]; ptr != EOL; ptr = link[ptr]) {
			if (gralive[list[ptr]] == DEAD && !inlist(mvs[grpieces[list[ptr]]], &eyeptr[rn])) {
				deadok = TRUE;
				for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					for (ptr3 = nbgrp[list[ptr2]][*c]; ptr3 != EOL; ptr3 = link[ptr3]) {
						if (army == NOARMY)
							army = G_ARMY(list[ptr3]);
						else if (G_ARMY(list[ptr3]) != army && G_ARMY(list[ptr3]) != NOARMY) {
							deadok = FALSE;
							break;
						}
					}
				}
				if (deadok) {
					for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (army == NOARMY)
							army = G_ARMY(list[ptr2]);
						else if (G_ARMY(list[ptr2]) != army && G_ARMY(list[ptr2]) != NOARMY) {
							deadok = FALSE;
							break;
						}
					}
				}
				if (deadok) {
					for (ptr2 = grpieces[list[ptr]]; ptr2 != -1; ptr2 = mvnext[ptr2]) {
						if (addlist(mvs[ptr2], &eyeptr[rn]))
							expandeye(rn, mvs[ptr2], c, army);
					}
				}
			}
		}
	}
	/* expand through the whole dead group */
	else { 
		for (ptr2 = grpieces[board[s]]; ptr2 != -1; ptr2 = mvnext[ptr2]) {
			if (addlist(mvs[ptr2], &eyeptr[rn]))
				expandeye(rn, mvs[ptr2], c, army);
		}
	}
}

/*
 * find value of eye on 2nd line with two stones adjacent (above and to side - s1 and s2)
 *
 *   s6 s5 s1 s3
 *   s4 cp  s s2
 *         ep
 */
      
static void evaleyenearedge(eye_t rn, sqr_t s, sqr_t end1, sqr_t end2, int c, listval_t ldrno, int full, int libs) {
	sqr_t ep,cp,s1,s2,s3,s4,s5,s6;
	int i,ldtmp,dir;
	army_t army;

	if (edge[end1] < edge[end2]) {
		ep = end1;  /* edge point */
		cp = end2;  /* center point */
	}
	else {
		ep = end2;
		cp = end1;
	}
	s1 = s + s - ep;  /* stone on 3rd line */
	if (edge[ep] == 0) {
		s2 = s3 = s1;
	}
	else {
		s2 = s + s - cp;  /* stone on 2nd line */
		s3 = s + s - ep + s - cp;  /* diagonal point */
	}
		
	/* give priority to big eye */
		
	if (inbigeye(cp, c, NOARMY) ||
		inbigeye(ep, c, NOARMY)) {
		army = S_ARMY(s1);
		if (S_ARMY(s2) != army)
			army = MULTIARMY;
		evalverybigeye(rn, s, army, c, ldrno, full, libs);
		return;
	}

	if (edge[cp] == 2)s4 = cp + cp - s;
	else s4 = cp;
    if (addlist(ldrno, &ldrflag[s4]))
		adflist(s4, &grldr[ldrno]);
	s5 = cp + s - ep;
    s6 = s4 + s - ep;
    
	eyetype[rn] = NEAREDGEEYE;
	eyeval[rn] = 0;
	eyemin[rn] = 0;
	eyepot[rn] = 0;
	addlist(s, &eyeptr[rn]);
	if (lnbf[ep][1-c] == 0 && !inbigeye(ep, 1-c, NOARMY)) {  /* 7/01 can't add it it if it is part of a big eye for the opponent */
			addlist(ep, &eyeptr[rn]);  /* added to avoid eyerec allocation errors with very big eye starting right of ep */
	}
	if (lnbf[ep][1-c] == 1 && S_COLOR(ep + s2 - s) == 1-c) {
		addlist(ep, &eyevital[rn]);
		if (grlibs[board[ep+s2-s]] <= 2 &&  /* block at ep to make eye */
			lnbf[cp][1-c] == 0)
			eyepot[rn] = 8;
	}
	if (ltrgd[cp] & 4) {
		i = fdir[cp];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			dir = dirnm[i];
			if (sqrbrd[cp][dir] != NOSQUARE &&
				S_COLOR(sqrbrd[cp][dir]) == 1-c)
					addlist((sqr_t)(cp + dstbrd[cp][dir] * nbr[i]), &eyevital[rn]);
		}
	}
	if (lnbf[ep][1-c] == 0) {
		/* addlist(cp,&eyeptr[rn]); causes error message */
		eyepot[rn] = 8;
		if (S_NUMLIBS(s1) > 2 && S_NUMLIBS(s2) > 2 && lnbf[cp][1-c] == 0 &&
			(ltrgd[s]&4) == 0 &&  /* not undercut */
			(lnbf[s4][1-c] == 0 && board[s4] == NOGROUP && S_COLOR(s5) == c ||
			  lnbf[s4][1-c] == 0 && board[s4] == NOGROUP &&
				lnbf[s5][1-c] == 0 && lnbf[s4][c] != 0 ||
			  S_COLOR(s4) == c && S_NUMLIBS(s4) > 3 ||
			  edge[cp] == 1 || edge2[cp] == 2)) {
				eyeval[rn] = 8;
				addlist(cp, &eyevital[rn]);
		}
		else if (S_NUMLIBS(s2) > 2) {
			if (S_COLOR(s5) == c && board[s4] == NOGROUP)
				addlist(s4, &eyevital[rn]);
			else
				addlist(cp, &eyevital[rn]);
			if (board[s5] == NOGROUP &&
				board[s4] == NOGROUP &&
				lnbn[s4] == 4)
				addlist(s4, &eyevital[rn]);
			if (lnbn[ep] == 3 && lnbf[cp][1-c] == 0 &&
				(S_COLOR(s5) == c || board[s5] == NOGROUP && S_COLOR(s6) == c))
				addlist(ep, &eyevital[rn]);
		}
		else { 
			if (S_NUMLIBS(s2) == 2)
				addlist(cp,&eyevital[rn]);
			addlist(s, &eyevital[rn]);
			if (lnbf[cp][1-c] == 0)
				addlist(ep, &eyevital[rn]);
		}
	}
}

	
/* evaluate line eyes that are open at both ends 
 * start with point s, which must be adjacent to a friendly stone.  c is friendly color 
 */

int evlol[] = { 0,0,0,0,0,8,8,8,16,16 };

static void evalopenlineeye(sqr_t s, int c, eye_t rn, listval_t ldrno, int full, int libs) {
	int length, pot, min;
	list_t points = EOL; 
	     /* length is number of points.  Points are places with two
              * liberties and only friendly neighbors
              */
	sqr_t end1 ,end2;  /* ends are points after last point */
	sqr_t v1, v2;      /* v1,v2 are last points */
	sqr_t sn, corner = NOSQUARE;
	int atari_inside = FALSE, cap_inside = FALSE;
	list_t ptr;
	int dead = FALSE;
	army_t army = NOARMY;	/* army for point s, or NOARMY if ther is more than one army neighbor */
	army_t army1, army2;	/* neighboring armies of points before end1 and end2, if single army */
	if (nbgrp[s][c] == EOL) {
#ifdef G2DEBUGOUTPUT
		outerror("evalopenlineeye not next to friendly stone!");
#endif
		return;
	}
	
	addlist(s,&points);
	length = 1;

	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
		if (army == NOARMY)
			army = G_ARMY(list[ptr]);
		else if (G_ARMY(list[ptr]) != army && G_ARMY(list[ptr]) != NOARMY) {
			army = MULTIARMY;
			break;
		}
	}
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
		if (addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]]))
			adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
		if (grlibs[list[ptr]] == 1 || G_THREATENED(list[ptr])) {
			cap_inside = TRUE;
			if (grcapmove[list[ptr]] != NOSQUARE && grcapmove[list[ptr]] != PASS)
				addlist(grcapmove[list[ptr]],&eyevital[rn]);
			if (grsavemove[list[ptr]] != NOSQUARE && grsavemove[list[ptr]] != PASS)
				addlist(grsavemove[list[ptr]],&eyevital[rn]);
		}
		else if (grlibs[list[ptr]] == 2) {
			atari_inside = TRUE;
		}
	}
	army1 = army2 = army;

	end1 = list[nblbp[s]];
	end2 = list[link[nblbp[s]]];
	v1 = v2 = s;
	while(!dead && lnbn[end1] <= 2 && lnbf[end1][1-c] == 0) {
		if (edge[end1] == 0 && lnbn[end1] == 2 && board[end1+diags[fdir[end1]]] == NOGROUP)
			break;  /* only count corner if stone on 2-2 point */
		dead = FALSE;
		for (ptr = nbgrp[end1][c]; ptr != EOL; ptr = link[ptr]) {
			if (G_ARMY(list[ptr]) != NOARMY)
				army1 = G_ARMY(list[ptr]);
			if (addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]]))
				adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
			if (G_ALIVE(list[ptr]) == DEAD) {
				dead = TRUE;
			}
			else if (grlibs[list[ptr]] == 1 || G_THREATENED(list[ptr])) {
				cap_inside = TRUE;
				if (grcapmove[list[ptr]] != NOSQUARE && grcapmove[list[ptr]] != PASS)
					addlist(grcapmove[list[ptr]],&eyevital[rn]);
				if (grsavemove[list[ptr]] != NOSQUARE && grsavemove[list[ptr]] != PASS)
					addlist(grsavemove[list[ptr]],&eyevital[rn]);
			}
			else if (grlibs[list[ptr]] == 2) {
				atari_inside = TRUE;
			}
		}
		if (dead)
			break;	/* no eye next to dead friendly group */
		if (lnbn[end1] == 1) {
			killist(&points);
			killist(&eyevital[rn]);
			evallineeye(end1, list[nblbp[end1]], s, c, rn, ldrno, full, libs);
			return;  /* end not open */
		}
		addlist(end1,&points);
		++length;
		v1 = end1;
		if (edge[end1] == 0)corner = end1;
		if (end1 == end2)break;
		sn = list[nblbp[end1]];
		if (inlist(sn,&points))
			sn = list[link[nblbp[end1]]];
		end1 = sn;
	}
	dead = FALSE;
	if (end1 != end2) {  /* in case of loops */
  	    while(!dead && lnbn[end2] <= 2 && lnbf[end2][1-c] == 0) {
			if (edge[end2] == 0 && lnbn[end2] == 2 && board[end2+diags[fdir[end2]]] == NOGROUP)
				break;  /* only count corner if stone on 2-2 point */
			dead = FALSE;
			for (ptr = nbgrp[end2][c]; ptr != EOL; ptr = link[ptr]) {
				if (G_ARMY(list[ptr]) != NOARMY)
					army2 = G_ARMY(list[ptr]);
				if (addlist(ldrno, &ldrflag[mvs[grpieces[list[ptr]]]]))
					adflist(mvs[grpieces[list[ptr]]], &grldr[ldrno]);

				if (G_ALIVE(list[ptr]) == DEAD) {
					dead = TRUE;
				}
				else if (grlibs[list[ptr]] == 2) {
					atari_inside = TRUE;
				}
				else if (grlibs[list[ptr]] == 1 || G_THREATENED(list[ptr])) {
					cap_inside = TRUE;
					if (grcapmove[list[ptr]] != NOSQUARE && grcapmove[list[ptr]] != PASS)
						addlist(grcapmove[list[ptr]], &eyevital[rn]);
					if (grsavemove[list[ptr]] != NOSQUARE && grsavemove[list[ptr]] != PASS)
						addlist(grsavemove[list[ptr]], &eyevital[rn]);
				}
			}
			if (dead)
				break;	/* no eye next to dead friendly group */
			if (lnbn[end2] == 1) {
				killist(&points);
				killist(&eyevital[rn]);
				evallineeye(end2, list[nblbp[end2]], s, c, rn, ldrno, full, libs);
				return;  /* end not open */
			}
			addlist(end2,&points);
			++length;
			v2 = end2;
			if (edge[end2] == 0)corner = end2;
			sn = list[nblbp[end2]];
			if (inlist(sn,&points))
				sn = list[link[nblbp[end2]]];
			end2 = sn;
		}
	}

	if (length == 1 &&
		edge[s] == 2 && s+s-end1 != end2 &&
		(edge[end1] == 1 || edge[end2] == 1)) {
		killist(&points);
		killist(&eyevital[rn]);
		evaleyenearedge(rn, s, end1, end2, c, ldrno, full, libs);
#ifdef G2DEBUGOUTPUT
		if (eyetype[rn] == UNKNOWNEYE)
			outerr("evaleyenearedge returned unknown\n");
#endif
		return;
	}

/* if isolated line eye eval here.  if might extend to bigger eye, call
 * evalverybigeye
 */
			
	if (!(end1 == end2 && lnbn[end1] == 2) && army1 != NOARMY && inbigeye(end1, c, army1) && inbigeye(v1, c, army1) ||
		army2 != NOARMY && inbigeye(end2, c, army2) && inbigeye(v2, c, army2)) {
		killist(&points);
		killist(&eyevital[rn]);
		evalverybigeye(rn, s, army, c, ldrno, full, libs);
#ifdef G2DEBUGOUTPUT
		if (eyetype[rn] == UNKNOWNEYE)
			outerr("evalverybigeye returned unknown\n");
#endif
		return;
	}
#ifdef NEVER
	if (army2 != NOARMY && inbigeye(end2, c, army2) && inbigeye(v2, c, army2)) {
		killist(&points);
		killist(&eyevital[rn]);
		evalverybigeye(rn, v2, G_ARMY(list[nbgrp[s][c]]), c, ldrno, full, libs);
#ifdef G2DEBUGOUTPUT
		if (eyetype[rn] == UNKNOWNEYE)
			outerr("evalverybigeye 2 returned unknown\n");
#endif
		return;
	}
#endif
	eyeptr[rn] = points;

	if (length == 1) {
		if (board[end1+end2-s] == NOGROUP && 
		   end1+end2-s-s != 0 && 
		     (lnbn[end1] <= 3 || lnbn[end2] <= 3) && 
		   lnbf[end1][1-c] == 0 && 
		   lnbf[end2][1-c] == 0) {
			/* corner eye in middle of board */
			eyetype[rn] = OPENLINEEYE;
			eyeval[rn] = 0;
			eyepot[rn] = 8;
			eyemin[rn] = 0;
			sn = end1+end2-s;
			if (lnbf[sn][c] == 2 && lnbn[sn] == 2)
				eyepot[rn] = 4;  /* so don't double count two
						    facing each other */
			addlist(sn,&eyevital[rn]);
			return;
		}
		if (end1+end2-s != s &&
		   board[end1+end2-s] != NOGROUP && 
		   S_COLOR(end1+end2-s) == c &&
		   (getldval(end1) >= 3 || getldval(end2) >= 3)) {
			eyetype[rn] = OPENLINEEYE;
			eyeval[rn] = 0;
			eyemin[rn] = 0;
		    if ((!lnbf[end1][0] || !lnbf[end1][1]) &&
		   		(!lnbf[end2][0] || !lnbf[end2][1]))
				eyepot[rn] = 8;  /* corner eye with diagonal taken */
			else
				eyepot[rn] = 4;
			addlist(end1,&eyevital[rn]);
			addlist(end2,&eyevital[rn]);
			return;
		}
		if (end1+end2-s == s) { /* straight line eye */
			addlist(end1,&eyevital[rn]);
			addlist(end2,&eyevital[rn]);
		}
	}

		
	if (length == 2 && (end1 - end2 == 1 || end1 - end2 == -1 || end1 - end2 == boardsize || end1 - end2 == -boardsize)) {
		if (lnbf[end1][1-c] == 0 && lnbf[end2][1-c] == 0) {
			if (v1+v1-end1 != v2 && (edge[v1] == 2 || edge[v2] == 0)) {
				evaleyenearedge(rn, v1, end1, v2, c, ldrno, full, libs);
#ifdef G2DEBUGOUTPUT
				if (eyetype[rn] == UNKNOWNEYE)
					outerr("evaleyenearedge 2 returned unknown\n");
#endif
				return;
			}
			else if (v2+v2-end2 != v1 && (edge[v2] == 2 || edge[v1] == 0)) {
				evaleyenearedge(rn, v2, end2, v1, c, ldrno, full, libs);
#ifdef G2DEBUGOUTPUT
				if (eyetype[rn] == UNKNOWNEYE)
					outerr("evaleyenearedge 3 returned unknown\n");
#endif
				return;
			}
			eyetype[rn] = OPENLINEEYE;  /* partial square */
			eyeval[rn] = 0;
			eyepot[rn] = 8;
			eyemin[rn] = 0;
			if (lnbf[end1][c] != 0 && lnbf[end1][1-c] == 0 &&
				lnbf[end2][c] != 0 && lnbf[end2][1-c] == 0)
				eyeval[rn] = 8;
			else {
				addlist(end1,&eyevital[rn]);
				addlist(end2,&eyevital[rn]);
			}
			return;
		}
	}

	if (length == 3 && end1 == end2) {  /* almost 4 in square */
		eyetype[rn] = OPENLINEEYE;
		eyeval[rn] = 0;
		if (!atari_inside && !cap_inside && (lnbf[end1][c] > 0 || edge[end1] <= 1) && lnbf[end1][1-c] == 0)
			eyeval[rn] = 8;
		eyepot[rn] = 8;
		eyemin[rn] = 0;
		addlist(end1,&eyevital[rn]);
		return;
	}

	if (end1 == end2 && length >= 4) {  /* block 4 eye or larger loop eye */
		eyetype[rn] = FOURPTBLOCKEYE;
		eyepot[rn] = 8;
		if (cap_inside) {
			eyeval[rn] = 0;
			eyemin[rn] = 0;
		}
		else {
			eyeval[rn] = 8;
			eyemin[rn] = 8;
		}
		return;
	}


	if (playlevel > 2) {
	    /* this code is needed, but is too slow.  use following
    	   approximation at low levels */
	
		if (!inlist(end1,&points) && getcount(v1,c,rn,FALSE,ldrno,libs) >= 10) {
			/* end1 can only be in points if there is a loop (handled above) */
			++length;
			addlist(v1, &eyevital[rn]);	/* sometimes enemy can move here */
			v1 = end1; /* can make eye if play v1, so play out one */
		}
		if (!inlist(end2,&points) && getcount(v2,c,rn,FALSE,ldrno,libs) >= 10) {
			/* end2 can never be in points */
			++length;
			addlist(v2, &eyevital[rn]);	/* sometimes enemy can move here */
			v2 = end2;  /* can make eye if play v2, so play out one */
		}
	}
	else {

		if (lnbf[end1][1-c] == 0 || 
			grcolor[lgr[end1]] == 1-c && gralive[lgr[end1]] == DEAD) {
			++length;
			/* addlist(v1, &eyevital[rn]);	 sometimes enemy can move here */
			v1 = end1;
		}

		if (lnbf[end2][1-c] == 0 || grcolor[lgr[end2]] == 1-c && gralive[lgr[end2]] == DEAD) {
			++length;
			/* addlist(v2, &eyevital[rn]);	 sometimes enemy can move here */
			v2 = end2;
		}
	}
		
	if (v1 != end1 && edge[v1] == 1 && ltrgd[v1] == 0)
		++length;
	if (v2 != end2 && edge[v2] == 1 && ltrgd[v2] == 0)
		++length;

	pot = length+1;  /* don't subtract atari and cap for pot, since making eye can make them go away */
	if (v1 != end1 && lnbf[end1][c] || v2 != end2 && lnbf[end2][c])
		pot++;
	if (pot > 8)
		pot = 8;
	if (atari_inside)length--;
	if (cap_inside)length -= 3;
	if (length > 8)length = 8;
	min = length-1;
	if (corner != NOSQUARE) {
		if (length < 8 || 
			edge2[end1] != 7 && edge2[end2] != 7)  /* 4/01 length 8 one edge long, can't force bent 4 */
			length--; 
		for (ptr = nblbp[corner]; ptr != EOL; ptr = link[ptr])
			addlist(list[ptr], &eyevital[rn]);	/* add 1-2 points - ko possibility? */
	}
	if (min < 0)
		min = 0;
	if (length < 0)
		length = 0;
	if (pot < 0)
		pot = 0;
	eyetype[rn] = OPENLINEEYE;
	eyeval[rn] = evlol[length];
	eyepot[rn] = evlol[pot];
	eyemin[rn] = evlol[min];
	if (lnbn[end1] >= 3)
		addlist(end1, &eyevital[rn]);
	for (ptr = nblbp[end1]; ptr != EOL; ptr = link[ptr]) {
		if (lnbf[list[ptr]][1-c] == 0 && 
			(lnbn[list[ptr]] <= 2 || edge[list[ptr]] == 2) && 
			!inlist(list[ptr],&points)) {
			addlist(end1,&eyevital[rn]);  /* potential extra eyes */
		}
		if (lnbf[end1][1-c] == 0 && lnbn[end1] <= 3 &&
			lnbf[list[ptr]][c] &&
			(lnbf[list[ptr]][1-c] || lnbn[list[ptr]] > 2))
			addlist(list[ptr],&eyevital[rn]);
	}
	if (lnbn[end2] >= 3)
		addlist(end2, &eyevital[rn]);
	for (ptr = nblbp[end2]; ptr != EOL; ptr = link[ptr]) {
		if (lnbf[list[ptr]][1-c] == 0 &&
			(lnbn[list[ptr]] <= 2 || edge[list[ptr]] == 2) && 
			!inlist(list[ptr],&points)) {
			addlist(end2,&eyevital[rn]);  /* potential extra eyes */
		}
		if (lnbf[end2][1-c] == 0 && lnbn[end2] <= 3 &&
			lnbf[list[ptr]][c] &&
			(lnbf[list[ptr]][1-c] || lnbn[list[ptr]] > 2))
			addlist(list[ptr],&eyevital[rn]);
	}
	if (eyemin[rn] != eyeval[rn] || eyepot[rn] != eyeval[rn] || length == 6 || length == 3 ||
		length > 3 && corner != NOSQUARE) {
		if (corner != NOSQUARE && !inlist(v1,&nblbp[corner]) && !inlist(v2,&nblbp[corner]))
			addlist(corner,&eyevital[rn]);
		if (edge[v1] == 1 && lnbn[v1] == 2)
			for (ptr = nblbp[end1]; ptr != EOL; ptr = link[ptr])
				if (edge[list[ptr]] == 2)
					v1 = list[ptr];
		if (edge[v1] != 2 || lnbf[end1][1-c] || lnbn[v1] > 2 || lnbf[v1][1-c] != 0)
			addlist(v1,&eyevital[rn]);

		if (edge[v2] == 1 && lnbn[v2] == 2)
			for (ptr = nblbp[end2]; ptr != EOL; ptr = link[ptr])
				if (edge[list[ptr]] == 2)
					v2 = list[ptr];
		
		if (edge[v2] != 2 || lnbf[end2][1-c] || lnbn[v2] > 2 || lnbf[v2][1-c] != 0)
			addlist(v2,&eyevital[rn]);
	}
}

/* find all of the eyelist entries for this move.
 * these are potentially changed eyes.  they must be points with
 * 3 or 4 friendly neighbors. (or 2 neighbors on the second line or with ld = 5)
 */


static void findeyelist() { /* big changes 5/18/94 */
	sqr_t s, sn, sn2;
	int i,j,k,ldtmp,ldtm2;
	list_t ptr,ptr2;
	list_t elist = EOL;  /* current eyes (before recent moves) */
	list_t neweyelist = EOL;  /* points not eyes that can be eyes */
	for (ptr = eyelist; ptr != EOL; ptr = link[ptr]) {
		if (eyerec[list[ptr]] != 0)
			addlist(eyerec[list[ptr]],&elist);
		else if (can_be_eye(list[ptr], board[list[ptr]]))
			addlist(list[ptr],&neweyelist);
		for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (!eyerec[list[ptr2]] && 
				can_be_eye(list[ptr2], board[list[ptr2]]))
				addlist(list[ptr2],&neweyelist);
			else if (eyerec[list[ptr2]] && 
				eyerec[list[ptr2]] != eyerec[list[ptr]])
				addlist(eyerec[list[ptr2]],&elist);
			}
		}
    killist(&eyelist);
    eyelist = neweyelist;
    neweyelist = EOL;
	for (i = 0; i < pclsnext; ++i) { 
		s = pcls[i];
		j = fdir[s];
		for (ldtmp = ldiag[j]; j < ldtmp; ++j)
			if (eyerec[s+diags[j]] != 0)
				addlist(eyerec[s+diags[j]],&elist);
		
		if (eyerec[s] != 0)
			addlist(eyerec[s],&elist);
		else if (can_be_eye(s, board[s])) {
			addlist(s,&eyelist);
			if (board[s] == NOGROUP)
				for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
					if (eyerec[list[ptr]] != 0 &&
						eyerec[list[ptr]] != eyerec[s])
						addlist(eyerec[list[ptr]],&elist);
			}
		j = fdir[s];
		for (ldtmp = ldir[j]; j != ldtmp; ++j) {
			sn = s + nbr[j]; 
			if (eyerec[sn] != 0)
				addlist(eyerec[sn],&elist);
			else if (can_be_eye(sn, board[sn])) {
					addlist(sn,&eyelist);
					for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr])
						if (eyerec[list[ptr]] != 0 &&
							eyerec[list[ptr]] != eyerec[sn])
							addlist(eyerec[list[ptr]],&elist);
					}
			if (board[sn] != NOGROUP)continue;  /* not empty */
			k = fdir[sn];
			for (ldtm2 = ldir[k]; k != ldtm2; ++k) { 
				sn2 = sn + nbr[k];
				if (sn2 == s)continue;
				if (eyerec[sn2] != 0)
					addlist(eyerec[sn2],&elist);
				else if (can_be_eye(sn2, board[sn2])) {
					addlist(sn2,&eyelist);
					for (ptr = nblbp[sn2]; ptr != EOL; ptr = link[ptr])
						if (eyerec[list[ptr]] != 0 &&
							eyerec[list[ptr]] != eyerec[sn2])
							addlist(eyerec[list[ptr]],&elist);
					}
				} 
			}
		}
	for (ptr = elist; ptr != EOL; ptr = link[ptr])
		for (ptr2 = eyeptr[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
			adflist(list[ptr2],&eyelist);
	killist(&elist);
	}





/* return TRUE if this spot can be part of an eye
 * must be a liberty of a live stone, not neutral, and inbigeye()
 * or a dead or threatened stone
 */

static int can_be_eye(sqr_t s, group_t g) {
	list_t ptr;
	int c = NOCOLOR, dead = FALSE;

	if (board[s] == NOGROUP) {
		for (ptr = nbgrp[s][0]; ptr != EOL; ptr = link[ptr]) {
			c = 0;
			if (G_ALIVE(list[ptr]) == DEAD)
				dead = TRUE;
		}
		for (ptr = nbgrp[s][1]; ptr != EOL; ptr = link[ptr]) {
			c = 1;
			if (G_ALIVE(list[ptr]) == DEAD)
				dead = TRUE;
		}
	}

	if (dead)
		return FALSE;	/* liberties of dead groups are not eyes, but can be included in very big eyes later */

	if (g == NOGROUP && lnbf[s][0] && lnbf[s][1])
		return FALSE;  /* neutral point */

	if (g == NOGROUP && c != NOCOLOR &&
		lnbf[s][c] != 0 && 
		inbigeye(s, c, NOARMY))
		 	return TRUE;

	if (g != NOGROUP && (gralive[g] == DEAD || G_THREATENED(g)))
		return TRUE;

	if (g == NOGROUP && 
		(!lnbf[s][0] || !lnbf[s][1]) &&
		(lnbf[s][0] != 0 || lnbf[s][1] != 0)) {
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
			if (lnbn[list[ptr]] == 1 && lnbf[list[ptr]][1-c] == 0) {
				dead = FALSE;
				for (ptr = nbgrp[list[ptr]][c]; ptr != EOL; ptr = link[ptr])
					if (G_ALIVE(list[ptr]) == DEAD)
						dead = TRUE;
				if (!dead)
					return TRUE;  /* sopen for big eye */
			}
	}
	return FALSE;
}


/* evaluate a one point eye at s for color c. */

static void evalonepteye(sqr_t s, int c, eye_t rn, listval_t ldrno, int full, int libs)
{
	int count,i,ldtmp;
	list_t ptr, ptr2;
	sqr_t sn;
	list_t tmplist = EOL, nblist = EOL;
	eyetype[rn] = ONEPOINTEYE;
	addlist(s,&eyeptr[rn]);
	count = getcount(s,c,rn,TRUE,ldrno,libs);
	eyepot[rn] = opt1[count];
	eyeval[rn] = opt2[count];
	eyevalmax[rn] = opt2max[count];
	eyemin[rn] = opt3[count];
	eyeko[rn] = opt4[count];
	if (eyeval[rn] >= 8) {  /* no threatened neighbors (grthreatened not valid if not full) */
		cpylist(nbgrp[s][c], &nblist);	/* since reading inside loop changes this list */
		for (ptr = nblist; ptr != EOL; ptr = link[ptr]) {
			if (full && G_THREATENED(list[ptr]) && 
				grsavemove[list[ptr]] != NOSQUARE &&
				grsavemove[list[ptr]] != PASS &&
				board[grsavemove[list[ptr]]] == NOGROUP) {
				if (G_THREATENED(list[ptr]) == 2) /* fully threatened, 9/99 */
					eyeval[rn] = 0;     /* he can kill the eye with a capture */
				else {
					eyeval[rn] = 4;
					eyeko[rn] = 4;
				}
				eyemin[rn] = 0;
				tmplist = th_cap_moves((group_t)list[ptr]);	/* 7/01 get a better list of moves that work */
				for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2])
					if (list[ptr2] != PASS /* && list[ptr2] != s */)
						addlist((listval_t)(list[ptr2]|EYERMONLY), &eyevital[rn]);
				killist(&tmplist);
				tmplist = getdefmoves((group_t)list[ptr]);
				for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2])
					if (list[ptr2] != PASS && list[ptr2] != s &&
						!canbecaptured(list[ptr2], c, mvs[grpieces[list[ptr]]], 1-c, 1-c, vitalcapdepth[playlevel], vitalcapsize[playlevel], libs, ldrno))
						addlist((listval_t)(list[ptr2]|EYEADDONLY), &eyevital[rn]);
				killist(&tmplist);
				break;
			}
			if (grlibs[list[ptr]] == 1) {
				eyeval[rn] = 0;	/* might not be threatened due to lookahead */
				eyemin[rn] = 0;
				addlist((listval_t)(list[grlbp[list[ptr]]]|EYERMONLY), &eyevital[rn]);
			}
		}
		killist(&nblist);
	}
	else if (eyepot[rn] >= 8) {
		i = fdir[s];
		for (ldtmp = ldiag[i]; i < ldtmp; ++i) {  /* look at diagonals */
			sn = s + diags[i];
			if (board[sn] == NOGROUP && lnbf[sn][1-c] == 0 && 
				lnbn[sn] == 2 &&
				getcount(sn, c, rn, FALSE, ldrno, libs) >= 5) {
				eyepot[rn] += 4; 
			}
		}
	}
}


int ed1[] = { 1,1,-1,-1};
int ed2[] = { 1,-1,1,-1};

static void evalcornereyes() {
	sqr_t s, dir1, dir2;
	eye_t rn;
	int cn,rf,lf,c;
	for (cn = 0; cn < 4; cn ++) {
		s = 0;
		rf = lf = FALSE;
		if (cn == 1)s = boardsize-1;
		if (cn == 2)s = boardsquare - boardsize;
		if (cn == 3)s = boardsquare - 1;
		dir1 = ed1[cn]*boardsize;
		dir2 = ed2[cn];
		if (eyerec[s] != 0 || eyerec[s+dir1] != 0 ||
		   eyerec[s+dir2] != 0)continue;  /* already is an eye */
		if (board[s] != NOGROUP)continue;
		if (board[s+dir1+dir2] != NOGROUP) {
		   c = grcolor[board[s+dir1+dir2]];
		   if (getldval((sqr_t)(s+dir1)) >= 4 && board[s+dir2] == NOGROUP ||
		      getldval((sqr_t)(s+dir2)) >= 4 && board[s+dir1] == NOGROUP) {
			   rn = (eye_t)gtflist(&eyefreelist);
			   if (rn == G2ERROR) {
#ifdef CHECK			   
				   outerror("Out of eye records");
#endif				   
				   return;
				   }
			   eyerec[s] = rn;
			   eyecolor[rn] = c;
			   addlist(s,&eyeptr[rn]);
			   if (getldval((sqr_t)(s+dir1)) >= 4) {
				   eyerec[s+dir1] = rn;
				   addlist((sqr_t)(s+dir1),&eyeptr[rn]);
				   if (addlist((sqr_t)(s+dir2),&eyevital[rn])) {
					   adflist((listval_t)(rn),&eyevitrec[s+dir2]);
						}
				   if (board[s+dir1+dir1] == NOGROUP &&
				      (lnbf[s+dir1+dir1][1-c] == 0  ||
				       lnbn[s+dir1+dir1] == 1) &&
				      (grcolor[board[s+dir1+dir1+dir2]] == c ||
				       board[s+dir1+dir1+dir2] == NOGROUP &&
				       lnbf[s+dir1+dir1+dir2][1-c] == 0))
					   rf = TRUE;
				   }
			   if (getldval((sqr_t)(s+dir2)) >= 4) {
				   eyerec[s+dir2] = rn;
				   addlist((sqr_t)(s+dir2),&eyeptr[rn]);
				   if (addlist((sqr_t)(s+dir1),&eyevital[rn])) {
					   adflist((listval_t)(rn),&eyevitrec[s+dir1]);
					}
				   if (board[s+dir2+dir2] == NOGROUP &&
				      (lnbf[s+dir2+dir2][1-c] == 0  ||
				       lnbn[s+dir1+dir1] == 1) &&
				      (grcolor[board[s+dir2+dir2+dir1]] == c ||
				       board[s+dir2+dir2+dir1] == NOGROUP &&
				       lnbf[s+dir2+dir2+dir1][1-c] == 0))
					   lf = TRUE;
				   }
			   eyetype[rn] = CORNEREYE;
			   eyepot[rn] = 8;
			   eyeval[rn] = 0;
			   eyevalmax[rn] = 0;
			   if (rf && lf) {
				   eyeval[rn] = 8;
				   }
			   else {
				   if (addlist((sqr_t)(s+dir1),&eyevital[rn])) {
						adflist((listval_t)(rn),&eyevitrec[s+dir1]);
						}
				   if (addlist((sqr_t)(s+dir2),&eyevital[rn])) {
						adflist((listval_t)(rn),&eyevitrec[s+dir2]);
						}
				   }
			   eyemin[rn] = 0;
			   }
		   }
		else if (board[s+dir1] == NOGROUP && board[s+dir2] == NOGROUP &&
			getldval((sqr_t)(s+dir1+dir2)) >= 4 && getldval((sqr_t)(s+dir1+dir2)) <= 6 &&
			eyerec[s+dir1+dir2] == 0) {

			c = lnbf[s+dir1+dir2][1] != 0;
			rn = (eye_t)gtflist(&eyefreelist);
			if (rn == G2ERROR) {
#ifdef CHECK			   
				outerror("Out of eye records");
#endif				   
				return;
				}
			eyecolor[rn] = c;
			eyetype[rn] = CORNEREYE;
			eyepot[rn] = 8;
			eyeval[rn] = 0;
			eyevalmax[rn] = 0;
			eyemin[rn] = 0;
			addlist(s,&eyeptr[rn]);
			eyerec[s] = rn;
			addlist((sqr_t)(s+dir1),&eyeptr[rn]);
			eyerec[s+dir1] = rn;
			addlist((sqr_t)(s+dir2),&eyeptr[rn]);
			eyerec[s+dir2] = rn;
			addlist((sqr_t)(s+dir1+dir2),&eyeptr[rn]);
			eyerec[s+dir1+dir2] = rn;
			if (addlist((sqr_t)(s+dir1),&eyevital[rn])) {
				adflist((listval_t)(rn),&eyevitrec[s+dir1]);
				}
			if (addlist((sqr_t)(s+dir2),&eyevital[rn])) {
				adflist((listval_t)(rn),&eyevitrec[s+dir2]);
				}
			}
		}
	return;
	}


/* delete_eye deletes an eye record without touching other data structures.
 */

void delete_eye(eye_t rn)
{
	if (grldr[NUMGROUPS+NUMCONNS+rn] != EOL)
		kill_ldrflags((listval_t)(NUMGROUPS+NUMCONNS+rn));
	killist(&eyeptr[rn]);
	killist(&eyevital[rn]);
	eyetype[rn] = NOEYE;
	adflist(rn,&eyefreelist);
	eyeval[rn] = eyepot[rn] = eyemin[rn] = 0;
}

/* deallocate_eye gets rid of an eye record.  All eye records for eyes
 * that changed are deallocated at the start of fixli and reconstructed later
 */

static void deallocate_eye(eye_t rn) {
	int ptr;
#ifdef G2DEBUGOUTPUT
	char buf[10];
#endif
	if (grldr[NUMGROUPS+NUMCONNS+rn] != EOL)
		kill_ldrflags((listval_t)(NUMGROUPS+NUMCONNS+rn));
	for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
		eyerec[list[ptr]] = 0;
	}
	killist(&eyeptr[rn]);
#ifdef G2DEBUGOUTPUT
	for (ptr = eyevital[rn]; ptr != EOL; ptr = link[ptr]) {
		if (!inlistm(rn,&eyevitrec[list[ptr]&EYEPOINTMASK],EYEPOINTMASK)) {
			outeyerec(rn);
			turnoffcplay();
			outerror("Eyevital not in eyevitrec in deallocate!\n");
			outerror(ssqr((sqr_t)(list[ptr]&EYEPOINTMASK), buf));
		}
	}
#endif			
	for (ptr = eyevital[rn]; ptr != EOL; ptr = link[ptr]) {
		/*		eyevitval[list[ptr]&EYEPOINTMASK][eyecolor[rn]] = EYEVALNONE;   cache value no good anymore */
		dellistm(rn,&eyevitrec[list[ptr]&EYEPOINTMASK],EYEPOINTMASK);
	}
	killist(&eyevital[rn]);
	eyetype[rn] = NOEYE;
	adflist(rn,&eyefreelist);
	eyeval[rn] = eyepot[rn] = eyemin[rn] = 0;
	eyeko[rn] = 0;
}


static int onecapture(sqr_t s) {
	int i,ldtmp,sum = 0;
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		if (S_NUMLIBS(s+nbr[i]) == 1)++sum;
	}
	return(sum == 1);
}	
    


/* examines the diagonals of an eye and returns a count
 * 11 eye can never be taken away (3 or 4 corners controlled)
 * 10 0 opponent corner, 2 controlled (cant stop from making an eye)
 *      (adds two vital points), or 3 controlled, 1 opponent, and 2 move threat to remove eye
 * 9 ko for eye 1 opponent corner, 2 controlled, 1 ko - he needs threat	(kotype 4)
 *       or     0 opponent corner, 2 controlled, 1 ko (with his stone on it)- can live or take the ko
 * 8 ko for eye 1 opponent corner, 2 controlled, 1 ko - he can capture (kotype 3)
 * 7 ko for eye 1 opponent corner, 2 controlled, 1 ko - I can capture (kotype 2)
 * 6 ko for eye 1 opponent corner, 2 controlled, 1 ko - I need threat (kotype 1)
 * 5 0 opponent corner, 1 controlled (get eye if move first)
 *	(adds three vital points)
 * 4 0 opponent corner, 0 controlled (get eye with two moves in a row)
 *      (adds four vital points)
 * 3 1 opponent corner, 2 controlled (get eye if you move first)
 *     (adds vital point)
 * 2 1 opponent corner, 1 controlled (get eye with two moves in a row)
 *     (adds 2 vital points)
 * 1 1 opponent corner, 0 controlled (cant get eye)
 * 0 can never be an eye (2 opponent corners)
 *
 * These numbers correspond to diags that are definately controlled
 *   (assumes other diags can be played in by either color)
 *
 * rn is the eye record to add vital points to.  ONLY use rn for this purpose!
 * c is the color of the side making the eye
 * if (vital) then add vital points to the eye record
 * ldrno is the real ladder number for this eye!
 */
sqr_t vtp[40]; /* vital points for this eye, no check for overflow, could have EYEADDONLY or EYERMONLY included */

static int getcount(sqr_t s, int c, eye_t rn, int vital, listval_t ldrno, int libs)
{
	sqr_t sn, sn2, sn3, sv;
	list_t ptr, ptr2, ptr3, cntlist;
	int count, haveatari, gcount, bcount, deadgroup, cancap, capture, tmvs;
	int vtc = 0, i, ldtmp, flag, j, ldtm2, numkos = 0;
	int kotype=1;  /* type of ko
				   * 1 - he controls - I need to make threat, then recapture for control
				   * 2 - I can capture for control
				   * 3 - He can capture for control
				   * 4 - he needs to make a threat and capture to get control
				   */
	list_t ataripoints = EOL;  /* point where opponent can atari to force eye to be filled */
	int canatkgood = FALSE;	/* can a good point be removed with two moves? */

	count = 0;
	haveatari = 0;  /* number of enemy corners stones in atari */
	cntlist = EOL;
	gcount = 0;
	bcount = 0;
	if (edge[s] == 1) {
		gcount = 1;
		bcount = 1;
	}
	else if (edge[s] == 0) {
 		gcount = 2;
 		bcount = 1;
	}
	/* find ataripoints */
	if (board[s] == NOGROUP) {
		for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
			if (addlist(ldrno, &ldrflag[mvs[grpieces[list[ptr]]]]))
				adflist(mvs[grpieces[list[ptr]]], &grldr[ldrno]);
			if (grlibs[list[ptr]] == 2) {
				capture = FALSE;
				for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
					if (grlibs[list[ptr2]] == 1) {
						capture = TRUE;
						break;
					}
				if (!capture)
					for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
						if ((sqr_t)list[ptr2] != s)
							addlist(list[ptr2], &ataripoints);
			}
		}
	}
	i = fdir[s];
	for (ldtmp = ldiag[i]; i < ldtmp; ++i) {  /* look at diagonals */
		sn = s + diags[i];
		if (board[sn] != NOGROUP) { /* stone on diagonal point */
			if (addlist(ldrno,&ldrflag[sn]))
				adflist(sn,&grldr[ldrno]);
			deadgroup = gralive[board[sn]] == DEAD ||  /* captured */
				S_THREATENED(sn) &&	grcapmove[board[sn]] != NOSQUARE &&
				gralive[board[sn]] != 1 &&  /* not new group */
				grcolor[board[grcapmove[board[sn]]]] == 1-grcolor[board[sn]];  /* this is from howmuchvital and group was captured */
			if (grcolor[board[sn]] == c) {  /* friendly controlled */
				if (deadgroup)++bcount;
				else ++gcount;
			}
			else if (deadgroup)++gcount;
			else { /* enemy stone here */
				if (S_THREATENED(sn) || grlibs[board[sn]] == 1 &&
					!snapback(board[sn], 1-S_COLOR(sn))) {
					if ((sqr_t)list[grlbp[board[sn]]] == kosquare) {
						numkos++;
						kotype = 1;  /* can't capture */
						vtp[vtc++] = kosquare;
					}
					else if (grlibs[board[sn]] == 1 && 
						grsize[board[sn]] == 1 &&
				    	lnbn[list[grlbp[board[sn]]]] == 0 &&
				    	lnbf[list[grlbp[board[sn]]]][c] == 0 &&
				    	onecapture(list[grlbp[board[sn]]])) {
						numkos++;
						kotype = 2; /* can capture */
						vtp[vtc++] = list[grlbp[board[sn]]];
					}
					else {
						if (addlist(ldrno,&ldrflag[sn]))
							adflist(sn,&grldr[ldrno]);
						haveatari++;
						if (S_THREATENED(sn) && grcapmove[board[sn]] != NOSQUARE &&
							grcapmove[board[sn]] != PASS && board[grcapmove[board[sn]]] == NOGROUP) {
							/*!inlist(grcapmove[board[sn]],&grlbp[board[sn]])) {  */
							if (grcapmove[board[sn]] == grsavemove[board[sn]] ||
								grsavemove[board[sn]] == NOSQUARE || grsavemove[board[sn]] == PASS ||
								board[grsavemove[board[sn]]] != NOGROUP)
								vtp[vtc++] = grcapmove[board[sn]];
							else {
								vtp[vtc++] = grcapmove[board[sn]]|EYEADDONLY;  /* 3/01 split add and rm */
								vtp[vtc++] = grsavemove[board[sn]]|EYERMONLY;
							}
						}
						else /* 3/01 removed this from threatened */
							for (ptr = grlbp[board[sn]]; ptr != EOL; ptr = link[ptr]) {
								vtp[vtc++] = list[ptr];
								if (vtc > 18)break;
							}
						++bcount;
					}
				}
				else
					++bcount;
			}
		}
		else if (sn == kosquare) {
			numkos++;
			kotype = 4;
			vtp[vtc++] = sn;
		}
		else if (lnbn[sn] > 2) { /* enemy can play here without selfatari */
			flag = 0;
			for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr]) {
				if (lnbn[list[ptr]] == 4 || 
					lnbn[list[ptr]] == 3 && getldval(list[ptr]) <= 4 && edge[list[ptr]] > 2) {
					flag++;
				}
			}
			if (flag <= 1)
				addlist(sn,&cntlist);
			else
				vtp[vtc++] = sn;
			continue;
		}
		else if (lnbn[sn] < 2 && lnbf[sn][1-c] == 0 &&	/* capture enemy stone here, (but could be snapback or ko throwin) */
			lnbf[sn][c] != 0) {	/* no enemy, some friendly, at most 1 empty point adjacent */
			if (addlist(ldrno,&ldrflag[sn]))
				adflist(sn,&grldr[ldrno]);
			if (lnbn[sn] == 1) {
				sn2 = list[nblbp[sn]];
				if (lnbf[sn2][1-c] || lnbn[sn2] == 4) {
					canatkgood = TRUE;	/* can play two moves and remove this good point */
					if (vital)
						addlist((listval_t)(sn2|EYERMONLY),&eyevital[rn]);
				}
			}
			flag = FALSE;	/* can enemy play here with ko or snapback or capture? */
			j = fdir[sn];
			for (ldtm2 = ldir[j]; j != ldtm2; ++j) {
				sn2 = sn + nbr[j];
				if (board[sn2] == NOGROUP) {
					if (lnbn[sn2] != 1 || lnbf[sn2][c] != 0)
						continue;
					cancap = FALSE;
					for (ptr = nbgrp[sn2][1-c]; ptr != EOL; ptr = link[ptr]) {
						if (grlibs[list[ptr]] == 1) {
							cancap = TRUE;
							break;
						}
					}
					if (cancap)
						continue;	/* after throwin, would capture two enemy groups */
					numkos++;
					kotype = 4;  /* he can throw in and make ko */
					vtp[vtc++] = sn;
					flag = TRUE;
					continue;
				}
				else if (grlibs[board[sn2]] == 1) {
					if (grsize[board[sn2]] == 1 &&
						lnbn[sn] == 0 && onecapture(sn)) {
							numkos++;
							kotype = 3;
							vtp[vtc++] = sn;
							flag = TRUE;
							for (ptr = grnbp[board[sn2]]; ptr != EOL; ptr = link[ptr]) {
								if (grlibs[list[ptr]] == 1)
									vtp[vtc++] = list[grlbp[list[ptr]]];
							}
							break;
					}
					adflist(sn,&cntlist);
					flag = TRUE;
					break;
				}
				else if (grlibs[board[sn2]] == 2 && grsize[board[sn2]] == 1) {
					sn3 = list[grlbp[board[sn2]]];
					if (sn3 == sn)
						sn3 = list[link[grlbp[board[sn2]]]];
					if (lnbf[sn3][1-c] || lnbn[sn3] >= 2 || onelibnbgrp(sn3, c)) {  /* he can atari */
						vtp[vtc++] = sn;
						if (vital)
							addlist((listval_t)(sn3|EYERMONLY), &eyevital[rn]);
					}
				}
			}
			if (!flag)
				adflist(sn, &cntlist);
		}
		else { 
			if (addlist(ldrno, &ldrflag[sn]))
				adflist(sn, &grldr[ldrno]);
			adflist(sn, &cntlist);
		}
	}
	if (bcount < 2+haveatari && gcount < 3) {
		ptr2 = cntlist;
		while (ptr2 != EOL && gcount/* + haveatari */< 3) {
			if (eyecntplyhere(list[ptr2], c, ldrno, libs) == YES)
				++gcount;
			else
				vtp[vtc++] = list[ptr2];
			ptr2 = link[ptr2];
		}
/*		if (bcount+gcount < 4 && haveatari && !numkos)++gcount; 
		leave this out so one threatened and one open makes 10 rather than 11 */
	}
	killist(&cntlist);

	if (haveatari > 1)
		bcount--;	/* must capture one of them */
	if (gcount == 4)
		count = 11;
	else if (gcount == 3) {
		if (canatkgood && bcount)
			count = 10;
		else
			count = 11;
	}
	else if (gcount == 2 && bcount == 2 && haveatari)count = 3;
	else if (gcount == 2 && bcount == 1 && haveatari)count = 6;  /* added when took out above... */
	else if (bcount >= 2)count = 0;
	else count = 4 + gcount - 3 * bcount;
	if (count == 6) {
		if (numkos > 1 || numkos == 1 && kotype < 3)
			count = 9;
		else
			count = 10;
	}
	if (count == 5 && numkos == 2)
		count = 10;
	else if ((count == 3 || count == 5) && numkos == 1)
   		count = 5 + kotype;
	if (vital && (count >= 2 || haveatari) && count <= 10) {
		for (i = 0; i < vtc; ++i) {
			addlist(vtp[i], &eyevital[rn]);
			sv = vtp[i]&EYEPOINTMASK;
			if (board[sv] == NOGROUP && lnbn[sv] == 0 && lnbf[sv][1-c] == 1 &&
				grlibs[list[nbgrp[sv][1-c]]] == 3) {	/* prevent push in by filling a liberty */
				for (ptr = grlbp[list[nbgrp[sv][1 - c]]]; ptr != EOL; ptr = link[ptr]) {
					addlist(list[ptr], &eyevital[rn]);  /* 5/99 */
				}
			}
			if ((!lnbf[sv][0] || !lnbf[sv][1]) && lnbn[sv] == 2) {
				for (ptr = nblbp[sv]; ptr != EOL; ptr = link[ptr]) {
					if (lnbf[list[ptr]][0] && lnbf[list[ptr]][1] || lnbn[list[ptr]] > 2)
						addlist(list[ptr], &eyevital[rn]);  /* hanging vital point */
				}
			}
		}
	}
	/* see if an atari can force the eye to be filled */
	if (gcount < 3 && count > 5 && ataripoints != EOL) {
   		for (ptr = ataripoints; ptr != EOL; ptr = link[ptr]) {
			if (inlist(list[ptr], &nblbp[s]))
				continue; /* atari is inside eye */
			mvs[msptr] = list[ptr];
			mvcolor[msptr] = 1 - c;
			flag = lupdate(msptr);
			upldrflags(msptr, ldrno);
			++msptr;
			for (tmvs = grpieces[board[list[ptr]]]; tmvs != -1; tmvs = mvnext[tmvs]) {
				sn = mvs[tmvs];
				if (addlist(ldrno, &ldrflag[sn]))
					adflist(sn, &grldr[ldrno]);
			}
			if (flag) {
				if (S_NUMLIBS(list[ptr]) > 1) {
					for (ptr2 = grnbp[board[list[ptr]]]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (G_NUMLIBS(list[ptr2]) == 1 && inlist(s, &grlbp[list[ptr2]])) {
							if (vital)
		   						addlist((listval_t)(list[ptr]|EYERMONLY), &eyevital[rn]);
   							count = 5; /* opponent can atari from ouside to force eye filled */
							if (vital && S_NUMLIBS(list[ptr]) == 2) {
								for (ptr3 = grlbp[board[list[ptr]]]; ptr3 != EOL; ptr3 = link[ptr3]) {
									if (lnbn[list[ptr3]] > 1 || lnbf[list[ptr3]][c] != 0)
					   					addlist((listval_t)(list[ptr3]|EYEADDONLY), &eyevital[rn]);
								}
							}
							break;
						}
					}
				}
			}
			--msptr;
			ldndate(msptr);

		}
	}
   	if (ataripoints != EOL)
   		killist (&ataripoints);
	return count;
}  

/* return YES if color 1-c can't play here
 * NO if can play here
 * MAYBE if unsure
 * libs is maximum liberties to allow in reading
 */

static int eyecntplyhere(sqr_t s, int c, listval_t ldrno, int libs) {
	int flag,count;
	sqr_t tmp;
	list_t ptr;
	group_t g;
	
	if (board[s] != NOGROUP) {
#ifdef G2DEBUGOUTPUT
		outerror("eyecntplyhere nonempty point!");
#endif
		return YES;
	}
	if (gralive[lgr[s]] == DEAD && grcolor[lgr[s]] == 1-c)return(YES);
	if (lnbn[s] == 4)return(NO);
	if (lnbn[s] == 3) {
		flag = 0;
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
			if (lnbn[list[ptr]] == 4 || 
				lnbn[list[ptr]] == 3 && getldval(list[ptr]) <= 4 && edge[list[ptr]] > 2)flag++;
		if (flag > 1)return(NO);  /* two ways to escape */
	}
	if (s == kosquare)return(MAYBE);
	if (getldval(s) >= 4 && getldval(s) <= 8 && ltr1[s] > 0 && ltrgd[s] == 0)return(YES);  /* inside eyespace */
	mvs[msptr] = s;
	mvcolor[msptr] = 1-c;
	flag = lupdate(msptr);
	upldrflags(msptr,ldrno);
	++msptr;
	if (flag) {
		g = board[s];
		if (mvnext[grpieces[g]] != -1) {
			if (addlist(ldrno,&ldrflag[mvs[mvnext[grpieces[g]]]]))
				adflist(mvs[mvnext[grpieces[g]]],&grldr[ldrno]);
		}
		if (grlibs[g] == 1) {
			if ((sqr_t)list[grlbp[g]] == kosquare)
				flag = 2;
			else if (snapback(g, TRUE))
				flag = TRUE;
			else
				flag = FALSE;     /* can capture */
		}
		else if (grlibs[g] <= libs) {
			count = 0;
			if (libs < 3)
				for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr])
					if (lnbn[list[ptr]] > 2)count++;
			if (count <= 1)
				flag = !iscaptured(g, eyecapdepth[playlevel], eyecapsize[playlevel], libs, eyemost[playlevel], c, ldrno, &tmp, 1-c);
		}
	}
	--msptr;
	ldndate(msptr);
	if (flag == 2)return(MAYBE);
	else if (flag)return(NO);
	else return(YES);
}

int diffs4[7][3]; /* size 4 shapes */
int diffs4i[7][3] =
{
{  1,18, 1 },  /* square */
{  1, 1,18 },  /* pyramid */
{ 18, 1,19 },  /* pyramid */ 
{ 18, 1, 1 },  /* pyramid */
{ 19, 1,18 },  /* pyramid */
{  1, 1, 1 },  /* line */
{ 19,19,19 },
}; 

int diffs5[17][4];	/* size 5 shapes */
int diffs5i[17][4] =
	{
	{  1, 1,17, 1 },  /* 8 bulky 5 shapes */
	{  1,18, 1,19 },
	{  1,17, 1, 1 },
	{ 19, 1,18, 1 },
	{  1,18, 1, 1 },
	{  1,18, 1,18 },
	{  1, 1,18, 1 },
	{ 18, 1,18, 1 }, 
	{ 18, 1, 1,18 }, /* plus */
	{ 18, 1, 1,19 }, /* 8 r shapes */
	{ 18, 1, 1,17 },
	{ 19, 1,17, 1 },
	{ 18, 1,19, 1 },
	{ 19, 1, 1,18 },
	{ 17, 1, 1,18 },
	{  1,17, 1,19 },
	{  1,19, 1,18 },
	 };  

int diffs6[4][5];
int diffs6i[4][5] =
	{
	{ 18, 1, 1,17, 1 },
	{  1,18, 1, 1,18 },
	{  1,17, 1, 1,18 },
	{ 18, 1, 1,18, 1 } };

/* do all neighbors of g share both of its liberties */
static int allshrlibs(group_t g, eye_t rn, list_t liblist)
{
	list_t ptr, ptr2;
	list_t groups = EOL;  /* neighboring enemy groups with common liberties */
	list_t libs = EOL;
	int count = 0;
	int comlibs;
	sqr_t s;
	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr])
	{
		if (cntlist(&nbgrp[list[ptr]][1-grcolor[g]]) == 2)
		{
			andlist(grlbp[list[nbgrp[list[ptr]][1-grcolor[g]]]],
						   grlbp[list[link[nbgrp[list[ptr]][1-grcolor[g]]]]],
						   &libs);
			for (ptr2 = libs; ptr2 != EOL; ptr2 = link[ptr2])
				if (!inlist(list[ptr2], &liblist))
					addlist(list[ptr2], &eyevital[rn]);
			killist(&libs);
		}
	}
	for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr])
	{
		if (comlist(grlbp[list[ptr]], liblist) != 2)
		{
			count++;
			addlist(list[ptr], &groups);
		}
	}
	if (count == 2)
	{
		comlibs = comlist(grlbp[list[groups]], grlbp[list[link[groups]]]);
		if (comlibs >= 2)
			count = 0;	/* groups connected around outside */
		else if (comlibs == 1) {
			andlist(grlbp[list[groups]], grlbp[list[link[groups]]], &libs);
			s = list[libs];
			if (solidconnect(s, 1-grcolor[g], grcolor[g]) == TRUE) /* not a ko */
				count = 0;
			else
				addlist(s, &eyevital[rn]);
			killist(&libs);
		}

	}
	killist(&groups);
	if (count)
		return FALSE;
	return TRUE;
}

/* deadshape takes a group and eye record number and sets the eyemin,
 * val, max, and vital points for the shape of the group.  the group is
 * between 3 and 6 stones inclusive.  liblist has libs and second order libs
 * of g.
 */

void deadshape(group_t g, eye_t rn, list_t liblist, listval_t ldrno, int libs)
{
	int size, count, lsize, numlibs;
	sqr_t keypoint = NOSQUARE, s, last, sn, cornerpoint;
	int minsize;
	list_t ptr, ptr2;
	int i, j ,diffs[5], ldtmp, ends, threes, fours, falseeyes, emptyends;
	int corner = FALSE, c;
#ifdef CHECK
	char buf[80];
#endif	
	eyeval[rn] = eyepot[rn] = eyemin[rn] = 8;  /* default */
	numlibs = cntlist(&liblist);
	size = grsize[g];
	lsize = size + numlibs;  /* stones plus level 1,2 liberties */
	if (lsize >= 8)
		eyepot[rn] = 16;
	c = grcolor[g];

	minsize = lsize;
	for (ptr = liblist; ptr != EOL; ptr = link[ptr]) {
		if (comlist(nblbp[list[ptr]], liblist) != lnbn[list[ptr]]) {	/* some 3rd order liberties not included */
			minsize -= 2;	/* he can force the eye smaller */
			addlist(list[ptr], &eyevital[rn]);
			for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (!inlist(list[ptr2], &liblist))
					addlist(list[ptr2], &eyevital[rn]);
			}
		}
		for (ptr2 = cnbrd[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (grcolor[cngr1[list[ptr2]]] == 1-c && cncnum[list[ptr2]] == 1) {
				minsize--;
				if (cncnum[list[ptr2]] == 1 && 
					lnbn[list[ptr]] == 1 &&
					inlist(list[nblbp[list[ptr]]], &liblist))
					minsize--;  /* can throwin to reduce size by 2 */
				break;
			}
		}
	}
#ifdef CHECK
	if (size < 3 || size > 6) {
		sprintf(buf, "deadshape called with bad size %d!\n",size);
		outerror(buf);
	}
#endif
	i = 0;
	for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
		if (board[list[ptr]] != NOGROUP)
			break;
	}
	last = list[ptr];	/* first point with a stone on it */
	if (edge[list[ptr]] == 0) {
		corner = TRUE;
		cornerpoint = list[ptr];
	}
	if (ptr != EOL) {
		for (ptr = link[ptr]; ptr != EOL; ptr = link[ptr]) { 
			if (board[list[ptr]] == NOGROUP)
				continue;   /* 8/99 to make old code work with libs included */
			ASSERT(i < 5);
			diffs[i] = list[ptr] - last;
			if (edge[list[ptr]] == 0) {
				corner = TRUE;
				cornerpoint = list[ptr];
			}
			++i;
			last = list[ptr];
		}
	}
	if (size == 3) {
		ends = countends(rn, 1 - grcolor[g], eyeptr[rn], liblist, &fours, &threes, &falseeyes, &emptyends, &keypoint, ldrno, TRUE, libs);
		eyepot[rn] = eyeval[rn] = eyemin[rn] = 8;
		if (lsize > 8 || minsize > 7) {
			eyeval[rn] = eyevalmax[rn] = eyepot[rn] = 16;
		}
		else if (lsize > 6)
			eyepot[rn] = 16;
		else if (lsize == 6) {
			eyepot[rn] = 16;
			eyevalmax[rn] = 16;
			eyeval[rn] = 8;
			if (ends == 0) {  /* 6 in box */
				eyeval[rn] = 8; /* vital point? */
				eyevalmax[rn] = 8;
				for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
					if (board[list[ptr]] != NOGROUP)
						continue;
					if (lnbn[list[ptr]] == 2 && lnbf[list[ptr]][grcolor[g]] == 1)
						addlist(list[ptr], &eyevital[rn]);
					else if (lnbn[list[ptr]] == 1 && lnbf[list[ptr]][grcolor[g]] == 2)
						addlist(list[ptr], &eyevital[rn]);
					else
						eyepot[rn] = 8;
				}
			}
			else if (ends == 1 && emptyends == 1) {  /* can force bulky 5 */
				eyevalmax[rn] = 8;
				if (board[keypoint] != NOGROUP && minsize < 6)
					eyepot[rn] = 8;
				if (lnbn[keypoint] == 0)
					eyepot[rn] = 8;	/* 3 single stones around key point */
				for (ptr = nblbp[keypoint]; ptr != EOL; ptr = link[ptr]) {
					addlist(list[ptr], &eyevital[rn]);
					if (minsize > 5)
						eyepot[rn] = 16;
				}
			}
			else if (emptyends == 2) {  /* can force bulky 5 */
				eyeval[rn] = 8;
				eyepot[rn] = 8;
				for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
					if (board[list[ptr]] != NOGROUP)
						continue;
					if (lnbf[list[ptr]][grcolor[g]] == 2) {
						addlist(list[ptr], &eyevital[rn]);
						eyepot[rn] = 16;
					}
				}
			}
			else if (fours) {
				eyeval[rn] = eyevalmax[rn] = eyepot[rn] = 8;  /* rabitty six */
			}
		}
		else if (lsize == 5) {
			if (ends == 1) {  /* bulky 5 shape */
				if (board[keypoint] == NOGROUP || size < 3) {
					eyepot[rn] = 16;
					addlist(keypoint, &eyevital[rn]);
				}
			}
			else if (ends == 2 &&  /* STRAIGHT LINE */
				!corner) { /* not bent 4 in corner */
				eyepot[rn] = 16;
				if (!falseeyes && 
					(numlibs == 1 || 
					size > grsize[g] ||	 /* two groups already */
					allshrlibs(g, rn, liblist)))
					eyeval[rn] = 16; /* worth two eyes as a seki */
			}
			else if (ends == 3) {   /* r pentomino */
				if (size == 3 && lnbn[keypoint] == 2)
					eyeval[rn] = eyepot[rn] = 16;
				else if (lnbn[keypoint] == 1 && lnbn[list[nblbp[keypoint]]] == 2) {
					eyepot[rn] = 16;
					addlist(list[nblbp[keypoint]], &eyevital[rn]);
				}
			}
			else if (ends == 4) {	 /* plus shape */
				eyepot[rn] = eyeval[rn] = 8;
			}
		}
#ifdef NEVER
		2/01, only one group now, and in any case this code is wrong, since eyeptr includes liberties
		else if (grsize[board[list[eyeptr[rn]]]] != size) {  /* two groups */
				eyepot[rn] = 16;
				if (lsize == 4)	/* can't connect */
					eyevalmax[rn] = 16;
		}
#endif
		else eyepot[rn] = 8;
		return;
	}
	if (size == 4) {
		eyeval[rn] = eyepot[rn] = eyemin[rn] = 8;  /* default */
		for (j = 0; j < 3; ++j) {
			if (diffs4[0][j] != diffs[j])
				break;
			else if (j == 2) {
				if (lsize > 6)
					eyepot[rn] = 16;
				return;	/* 4 in square */
			}
		}
		for (i = 1; i < 5; ++i) {
			for (j = 0; j < 3; ++j)
				if (diffs4[i][j] != diffs[j])
					break;
				else if (j == 2) {
					if (lsize > 6)
						eyepot[rn] = 16;
					else if (lsize == 6 && grlibs[g] == 1)
						eyepot[rn] = 16;
					else if (lsize == 6 && grlibs[g] == 2) {
						for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
							if (lnbf[list[ptr]][grcolor[g]] == 2)
								return;   /* can make bulky 5 + 1 liberty */
							s = list[ptr];
							i = fdir[s];
							for (ldtmp = ldir[i]; i < ldtmp; ++i) {
								if (S_COLOR(s+nbr[i]) == grcolor[g] && lnbf[s+nbr[i]][grcolor[g]] == 3)
									return;  /* can make pyramid */
							}
						}
						eyepot[rn] = 16;
						eyeval[rn] = 16;  /* can't make dead shape */
					}
					return;  /* 4 in pyramid */
			}
		}
		eyeval[rn] = eyemin[rn] = eyepot[rn] = 16; /* 4 in line */
		for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
			i = fdir[list[ptr]];
			count = 0;
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				sn = list[ptr]+nbr[i];
				if (board[sn] == g) {
					++count;
				}
#ifdef NEVER
				else {
					eyemin[rn] = eyeval[rn] = 8;	/* can throw in to reduce to 2 or 3 in a row */
					eyevalmax[rn] = 16;
					break;
				}
#endif
			}
			if (count == 2 && grlibs[g] > 1) {
				eyemin[rn] = eyeval[rn] = 8;
				addlist(list[ptr],&eyevital[rn]);
				return;  /* can make bulky 5 */
			}
		}
		for (i = 5; i < 7; ++i)
			for (j = 0; j < 3; ++j)
				if (diffs4[i][j] != diffs[j])break;
				else if (j == 2)return;  /* 4 in straight line */
		if (corner && lsize == 5 && lnbf[cornerpoint][c] == 2) {	/* bent 4 */
			eyemin[rn] = eyeval[rn] = 8;
			addlist(list[grlbp[g]], &eyevital[rn]);
		}
		return;  
	}
	if (size == 5 && grsize[g] == 5) {
		for (i = 0; i < 17; ++i)
			for (j = 0; j < 4; ++j)
				if (diffs5[i][j] != diffs[j])break;
				else if (j == 3 && i < 9) {  /* bulky 5 or plus dead group */
					if (lsize > 7) {
						eyevalmax[rn] = 16;   /* might be 2 eyes */
						eyepot[rn] = 16;
					}
					if (lsize == 7) {  /* might make rabbity 6 */
						eyepot[rn] = 16;
						eyeval[rn] = 16;	/* if can't make rabbity 6, is two eyes as a seki */
						if (grlibs[g] == 1)
							return;
						for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
							s = list[ptr];
							i = fdir[s];
							for (ldtmp = ldir[i]; i < ldtmp; ++i) {
								if (S_COLOR(s+nbr[i]) == grcolor[g] && lnbf[s+nbr[i]][grcolor[g]] == 3) {
									eyepot[rn] = 8;
									eyeval[rn] = 8;
									return;  /* can make rabbity 6 */
								}
							}
						}
						if (minsize <= 6) {
							eyeval[rn] = 8;	/* must connect to reduce to bulky-5 and one liberty */
							eyepot[rn] = 8;
						}
					}
					return;  /* bulky 5 or plus */
				}
				else if (j == 3 && i >= 9) {	/* r shape 2 eyes unless can make rabitty 6 */
					if (lsize == 7) {
						for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
							if (board[list[ptr]] == NOGROUP)continue;
							if (lnbf[list[ptr]][grcolor[g]] == 3 && lnbn[list[ptr]] == 1) {  /* rabbity 6 */
								eyeval[rn] = eyemin[rn] = 8;
								eyepot[rn] = 16;
								addlist(list[nblbp[list[ptr]]], &eyevital[rn]);
								return;
							}
						}
					}
					eyeval[rn] = eyevalmax[rn] = eyemin[rn] = eyepot[rn] = 16;
					return;
				}
		eyeval[rn] = eyevalmax[rn] = eyemin[rn] = eyepot[rn] = 16;
		return;  /* other 5 shapes */
	}
	if (size == 6 && grsize[g] == 6) {
		for (i = 0; i < 4; ++i)
			for (j = 0; j < 5; ++j)
				if (diffs6[i][j] != diffs[j])break;
				else if (j == 4) {
					if (lsize > 7)
						eyepot[rn] = 16;
					return;  /* rabbity 6 */
			}
		eyeval[rn] = eyevalmax[rn] = eyemin[rn] = eyepot[rn] = 16;
		return;  /* other 6 shapes */
	}
	/* after here all shapes have more than one dead group */
	eyemin[rn] = eyevalmax[rn] = eyeval[rn] = eyepot[rn] = 16;  /* most 4-6 shapes are two eyes */
	if (numlibs == 1)  /* can't connect */
		return;

	if (size == 6)  /* 6 stones in two groups must be bigger than rabitty 6 */
		return;
		
	if (minsize >= 7)
		return;
	
	/* size of dead stones is 4 to 6, and there are more than one group */
	if (size == 4) {
		for (ptr = liblist; ptr != EOL; ptr = link[ptr]) {
			if (lnbf[list[ptr]][c] == 4) {
				eyemin[rn] = eyeval[rn] = eyevalmax[rn] = eyepot[rn] = 8;  /* 4 single stones in diamond */
				return;
			}
			if (lnbf[list[ptr]][c] == 3) {
				eyemin[rn] = eyeval[rn] = 8;
				return;	/* can make bulky 5 or rabitty 6 */
			}
		}
		return;
	}
	/* size == 5 */
}

/* find shape features for group eye rn.  plist is the stones in the group
 * liblist is the 1st and second order liberties
 * return the number of ends (points in plist or liblist that
 * stick out (have only one neighbor in plist or liblist)
 * falseeyes are ends that are false eyes.
 * fours are internal eye points with 4 neighbors in eye
 * emptyends are ends that have no stone on them
 * keypoint is the point where the eye has 3 or 4 adjacent points
 * if fe, coutn false eyes, add eyevital and grldr.
 */
   
static int countends(eye_t rn, int c, list_t plist, list_t liblist, int *fours, int *threes, int *falseeyes, int *emptyends, sqr_t *keypoint, listval_t ldrno, int fe, int libs) {
	int i,ldtmp,cnt,ends = 0,count;
	list_t ptr,tmplist = EOL;
	*falseeyes = 0;
	*fours = 0;
	*threes = 0;
	*emptyends = 0;
	*keypoint = NOSQUARE;
	cpylist(plist,&tmplist);
	mrglist(liblist,&tmplist);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		cnt = 0;  /* number of adjacent points in the eye */
		i = fdir[list[ptr]];
		for (ldtmp = ldir[i]; i < ldtmp; ++i)
			if (inlist((sqr_t)(list[ptr]+nbr[i]),&tmplist))
				++cnt;
		if (cnt == 1) {
			ends++;
			if (board[list[ptr]] == NOGROUP)++(*emptyends);
			/* false eyes reduce size, but only if must connect here */
			if (fe && (board[list[ptr]] != NOGROUP || 
				link[nbgrp[list[ptr]][c]] != EOL)) {
				count = getcount(list[ptr], c, rn, TRUE, ldrno, libs);
				if (count < 10) {
					++(*falseeyes);
					if (board[list[ptr]] == NOGROUP)
						addlist(list[ptr], &eyevital[rn]);
					}
				}
			}
		if (cnt == 3) {
			(*threes)++;
			*keypoint = list[ptr];
			}
		if (cnt == 4) {
			(*fours)++;
			*keypoint = list[ptr];
			}
		}
	killist(&tmplist);
	return(ends);
	}

/* is s a connection point for c that will have to be filled as nakeade shape is completed? */
/* army a is making the nakade, so if the other side of the connection is also adjacent to a,
 * there is no force to fill it */

static int connectpoint(army_t a, sqr_t s, int c) {
	list_t ptr;
	sqr_t s2;
	int cn;
	if (cnbrd[s] == EOL)
		return FALSE;
	for (ptr = cnbrd[s]; ptr != EOL; ptr = link[ptr]) {
		cn = list[ptr];
		if (grcolor[cngr1[cn]] == c) {
			if (cncnum[cn] == 2) {
				s2 = list[cnptr[cn]];
				if (s2 == s)
					s2 = list[link[cnptr[cn]]];
				if (inlist(s2, &armylbp[a]))
					return FALSE;  /* no force to connect */
				}
			return TRUE;
			}
		}
	return FALSE;
	}

/* can the stones in army a make a dead shape (0 or 1 eye only) 
 * can't make nakade if capturing them makes a false eye real
 */

int canmakenakade(army_t a)
{
	int size, c, ends, threes, fours, falseeyes, emptyends, p;
	int stoneonkeypoint;
	group_t g;
	sqr_t keypoint, s;
	list_t ptr, ptr2, tlist = EOL, tlist2 = EOL, plist = EOL;
	int corner = FALSE;	/* for bent 4 in the corner */
	int j, ldtm2, false_eye = FALSE;	/* is there a false eye associated with this group */

	size = A_SIZE(a);
	c = A_COLOR(a);
	cpylist(armylbp[a],&tlist);	 /* tlist has liberties */
	for (ptr = armygroups[a]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[ptr];
		for (p = grpieces[g]; p != -1; p = mvnext[p]) {
			addlist(mvs[p], &plist);
			if (A_ALIVE(a) == DEAD)
				continue;
			j = fdir[mvs[p]];
			for (ldtm2 = ldiag[j]; j < ldtm2; ++j) {
				s = mvs[p] + diags[j];
				/* does capturing these stones make a false eye into a real eye? */
				if (board[s] == NOGROUP && lnbn[s] == 0 && lnbf[s][c] == 0) {
					false_eye = TRUE;
				}
			}
		}
	}
	if (false_eye) {
		killist(&plist);
		killist(&tlist);
		return FALSE;
	}
	for (ptr = armylbp[a]; ptr != EOL; ptr = link[ptr]) {
		if (connectpoint(a, list[ptr], 1 - c))
			continue;
		size++;
		for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (!connectpoint(a, list[ptr2], 1 - c) && 
				addlist(list[ptr2], &tlist) && 
				lnbf[list[ptr2]][c] == 0)
				size++;
		}
	}   /* tlist has 1st and second order libs */
	cpylist(tlist, &tlist2);
	for (ptr = tlist; ptr != EOL; ptr = link[ptr]) {
		for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (closest[list[ptr2]][c] != NOSQUARE && 
				S_ARMY(closest[list[ptr2]][c]) != a) {
				killist(&tlist);
				killist(&tlist2);
				killist(&plist);
				return TRUE;	/* BUG! TODO: remove this code and put it in isseki().  not surrounded yet 2/2/99, don't allow a seki */
			}
			else if (!connectpoint(a, list[ptr2], 1 - c) && 	 /* assume will have to fill interior connection points */
				addlist(list[ptr2], &tlist2) && 
				lnbf[list[ptr2]][c] == 0)
				size++;
		}
	}  /* tlist2 has first, second, third order liberties that are not connections */
	for (ptr = tlist2; ptr != EOL; ptr = link[ptr]) {
		if (edge[list[ptr]] == 0 && (lnbn[list[ptr]] || lnbf[list[ptr]][c] > 1))
			corner = TRUE;
	}
	for (ptr = plist; ptr != EOL; ptr = link[ptr]) {
		if (edge[list[ptr]] == 0)
			corner = TRUE;
	}
	ends = countends(0, c, plist, tlist2, &fours, &threes, &falseeyes, &emptyends, &keypoint, 0, FALSE, eyetaclibs[playlevel]);

	stoneonkeypoint = inlist(keypoint, &plist);
	killist(&tlist);
	killist(&tlist2);
	killist(&plist);
	if (size <= 4)
		return TRUE;	/* can always make dead shape with 4 or less points */
	if (threes == 0 && fours == 0 && !corner)  /* line of 4 or more is always 2 eyes, unless bent 4 in corner */
			return FALSE;
	if (size == 6 && threes == 1 && fours == 0 && ends == 1 && emptyends == 1) {  /* one flavor of partial bulky 5 */
		return TRUE;
	}
	if (size == 6 && fours == 1 && threes == 0 && ends == 2) {  /* rabit six shape */
		return TRUE;
	}
	if (size == 5 && ends == 1 && emptyends == 0 && stoneonkeypoint &&
		link[armygroups[a]] != EOL)
		return FALSE;	/* bulky 5 with 3 stones in diagonal - can only make line 4 */
	if (size == 5 && ends == 3 && threes == 1 && fours == 0 && keypoint != NOSQUARE) {	/* T shape */
		for (ptr = armygroups[a]; ptr != EOL; ptr = link[ptr]) {
			g = (group_t)list[ptr];
			for (p = grpieces[g]; p != -1; p = mvnext[p]) {
				if (mvs[p] != keypoint && mvs[p] - keypoint != 1 && mvs[p] - keypoint != -1 && mvs[p] - keypoint != boardsize && mvs[p] - keypoint != -boardsize)
					return FALSE;	/* stone at the end of the T */
			}
		}
	}
	if (size == 7 && fours != 1)
		return FALSE;	/* can't make rabbity 6 without exactly one four */
	if (size == 7 && fours == 1 && !stoneonkeypoint)
		return FALSE;   /* must occupy the key point */
	if (size == 6 && fours == 0 && threes <= 1)
		return FALSE;
	if (size >= 8)
		return FALSE;
	return TRUE;
}

/* evalthgroupeye evaluates the eye made of a group that is threatened.
 * make the capturing move and call evaldeadgroupeye if has more than
 * one libery, or call evaleye recursively if it was captured.
 * if full, all data structures are valid
 * can not make a recursive call after an lupdate that can call evalverybigeye
 */
	
static void evalthgroupeye(sqr_t s, group_t g, int c, eye_t rn, listval_t ldrno, int libs) {
	eye_t rn2;
	int moveflag = FALSE, size, tmp;
	int mptr;  /* mvs ptr, not list ptr */
	list_t ptr, tmplist = EOL;
	int inlib = FALSE;
	int flag;
	
#ifdef CHECK
	if (!G_THREATENED(g)) {
		outerror("evalthgroupeye - group not dead or threatened");
		turnoffcplay();
	}
#endif		
	size = grsize[g];

	if (grcapmove[g] != NOSQUARE &&  /* make killing move */
		grcapmove[g] != PASS && board[grcapmove[g]] == NOGROUP) {
		/* grcapmove[g] != kosquare) { 9/97 too pessimistic - davies 18 */
		inlib = inlist(grcapmove[g], &grlbp[g]);
		mvcolor[msptr] = 1-grcolor[g];
		mvs[msptr] = grcapmove[g];
		if (!lupdate(msptr)) {  /* lupdate so doesn't destroy cnprot, etc */
			 ldndate(msptr);
		}
		else {
			upldrflags(msptr, ldrno);
			++msptr;
			moveflag = TRUE;
		}
	}

	eyevalmax[rn] = 0;
	eyeval[rn] = 0;  /* since group can live in one move */
	eyemin[rn] = 0;

	if (board[s] != NOGROUP) {  /* stones still on board */
		evaldeadgroupeye(s, g, c, rn, ldrno, libs);
		/*  4/99 - sometimes eye making moves kill stone also 4/00 - too many bad vital points */
		killist(&eyevital[rn]);	/* only capturing move is vital point */
		/* any point in this eye should be marked.  it might be bigger */
		if (ldrno != NOGROUP) {
			for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
				if (addlist(ldrno, &ldrflag[list[ptr]]))
					adflist(list[ptr], &grldr[ldrno]);
			}
		}
		killist(&eyeptr[rn]);
		for (mptr = grpieces[g]; mptr != -1; mptr = mvnext[mptr])
			addlist(mvs[mptr], &eyeptr[rn]);
		if (eyevalmax[rn] < eyeval[rn])
			eyevalmax[rn] = eyeval[rn];
	   	tmp = eyevalmax[rn];	/* 12/06 use eyevalmax, not eyeval for bigger pots */
		if (eyeko[rn] == 0) {
		   	if (eyepot[rn] <= 16)
		   		tmp += (eyepot[rn] - eyevalmax[rn])/2;  /* adjust for extra move made */
	   		else if (eyevalmax[rn] < 16)
	   			tmp += (16 - eyevalmax[rn])/2;  /* adjust for extra move made */
		}

		if (tmp == 8 && grsize[g] >= 4 && grlibs[g] > 1 && inlib)
			tmp += 4;	/* might be able to make a seki */
		if (tmp != 0 && tmp < 4)
			tmp = 4;
	   	eyepot[rn] = tmp;
		eyeval[rn] = 0;  /* since group can live in one move */
		eyevalmax[rn] = 0;
		eyemin[rn] = 0;
	}
	else if (size > 6) {  /* capture 7 or more stones */
		eyepot[rn] = 16;
		for (mptr = grpieces[g]; mptr != -1; mptr = mvnext[mptr])
			addlist(mvs[mptr], &eyeptr[rn]);
	}
	else {  /* check shapes left over - may be more than one eye */
		eyeval[rn] = 0;
		rn2 = (eye_t)gtflist(&eyefreelist);
		for (mptr = grpieces[g]; mptr != -1; mptr = mvnext[mptr]) {
			if (!inlist(mvs[mptr],&eyeptr[rn])) {
				if (can_be_eye(mvs[mptr], NOGROUP)) {
					evaleye(mvs[mptr], rn2, ldrno ,FALSE, libs);  /* can't be full since just did lupdate */
					eyepot[rn] += eyevalmax[rn2];
					if (eyeko[rn2] == 0) {
						if (eyepot[rn2] <= 16)
							eyepot[rn] += (eyepot[rn2]-eyevalmax[rn2])/2;
						else if (eyeval[rn2] < 16)
							eyepot[rn] += (16-eyevalmax[rn2])/2;
					}
					else if (eyepot[rn] < 3) {
						eyepot[rn] = 3;
						eyeko[rn] = eyeko[rn2];
					}
					mrglist(eyeptr[rn2], &eyeptr[rn]);
					mrglist(eyevital[rn2], &tmplist);
					killist(&eyeptr[rn2]);
					killist(&eyevital[rn2]);
					eyeval[rn2] = eyepot[rn2] = eyemin[rn2] = 0;
					eyetype[rn2] = NOEYE;
    	  		}
    		  	else addlist(mvs[mptr], &eyeptr[rn]);
    		}
     	}
      	adflist(rn2,&eyefreelist);
    }
		
	if (moveflag) {
	   	--msptr;
   		ldndate(msptr);
		if (mvs[msptr] != grsavemove[g] && 
			eyepot[rn] < 16)  /* might play here even though it doesn;t live to make nakade shape */
	   		addlist((listval_t)(mvs[msptr]|EYEADDONLY),&eyevital[rn]);
	   	else
	   		addlist(mvs[msptr],&eyevital[rn]);
   	}

	for (ptr = tmplist; ptr != EOL; ptr = link[ptr])
		if (board[list[ptr]] == NOGROUP)
			addlist(list[ptr], &eyevital[rn]);
	killist(&tmplist);
	/* threatened eye may have added vital points on top of stones that
	   were captured before the eye evaluation.  remove them here. */
	cpylist(eyevital[rn], &tmplist);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr])
		if (board[list[ptr]&EYEPOINTMASK] != NOGROUP)
			dellist(list[ptr], &eyevital[rn]);
	killist(&tmplist);
	if (grlibs[g] == 2) {
		for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
			if (!inlistm(list[ptr], &eyevital[rn], EYEPOINTMASK))
				addlist((listval_t)(list[ptr]|EYEADDONLY), &eyevital[rn]);  /* 9/99 for ld1e problem 180 */
			if (lnbn[list[ptr]] == 1 && lnbf[list[ptr]][1-grcolor[g]] == 2)
				addlist((listval_t)(list[nblbp[list[ptr]]]|EYEADDONLY), &eyevital[rn]);  /* try to make a bigger eye */
		}
	}
	/* 1/04 more vital points */
	tmplist = getdefmoves(g);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		if (lnbn[list[ptr]] == 0) {	/* don't include illegal moves */
			mvcolor[msptr] = grcolor[g];
			mvs[msptr] = list[ptr];
			flag = lupdate(msptr);
			ldndate(msptr);
			if (!flag)
				continue;
		}
		if (list[ptr] != PASS)
			addlist((listval_t)(list[ptr]|EYERMONLY), &eyevital[rn]);
	}
	killist(&tmplist);
	tmplist = getatkmoves(g);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] != PASS)
			addlist((listval_t)(list[ptr]|EYEADDONLY), &eyevital[rn]);
	}
	killist(&tmplist);

	if (G_THREATENED(g) && 
   		grsavemove[g] != NOSQUARE && 
   		grsavemove[g] != PASS && !inlistm(grsavemove[g], &eyevital[rn], EYEPOINTMASK))
   		addlist((listval_t)(grsavemove[g]|EYERMONLY), &eyevital[rn]);
	eyetype[rn] = THRTEYE;
}


/* evaldeadgroupeye figures out the eyes for the single dead group g
 * bigger collections of nearby dead groups are handled by verybigeye, so do not expand the eye!
 * the eye includes the stones in g and its liberties (8/00)
 * s is the spot for the ladder number. c is the color of the surrounding 
 * stones (color of the eye), not the dead group 
 * if dead group is one stone, treat like eye surrounded on 3 sides.
 * assume group is dead, even if aliveness doesn't say so.
 */

static void evaldeadgroupeye(sqr_t s, group_t g, int c, eye_t rn, listval_t ldrno, int libs)
{
	sqr_t s1,s2;
	int e1,e2,size,atari,mptr;
	list_t liblist=EOL, glist = EOL, ptr;
	cpylist(grlbp[g], &liblist);
	for (mptr = grpieces[g]; mptr != -1; mptr = mvnext[mptr]) {
		addlist(mvs[mptr],&eyeptr[rn]);
	}

	eyetype[rn] = DEADEYE;
	eyevalmax[rn] = 0;
                        
	size = grsize[g];

	if (size == 1)
      	evaloneptdeadeye(s,g,c,rn,ldrno,libs);

	else if (size == 2) {
      	s1 = mvs[grpieces[g]];
      	s2 = mvs[mvnext[grpieces[g]]];
		e1 = e2 = 8;
		if (lnbn[s1] < 2)
			e1 = c8[getcount(s1, c, rn, FALSE, ldrno, libs)];
		if (lnbn[s2] < 2)
			e2 = c8[getcount(s2, c, rn, FALSE, ldrno, libs)];
      	if (e1 + e2 == 16) {
      		if (grsize[g] == 1 && board[s1] != NOGROUP && board[s2] != NOGROUP) {  /* two one stone groups */
				andlist(grnbp[board[s1]], grnbp[board[s2]], &glist);
				atari = FALSE;
				for (ptr = glist; ptr != EOL; ptr = link[ptr])
					if (grlibs[list[ptr]] == 1)
						atari = TRUE;
				if (link[liblist] == EOL && !atari) {  /* one liberty, can't connect */
					if (lnbf[list[liblist]][1-c] == 2)  /* exactly 2 dead stones next to liberty */
      					eyepot[rn] = eyeval[rn] = eyevalmax[rn] = eyemin[rn] = 16;
					else {
						eyepot[rn] = eyevalmax[rn] = 16;
						eyeval[rn] = eyemin[rn] = 8;
					}
				}
      			else {
      				eyepot[rn] = 16;
      				eyevalmax[rn] = 8;
      				eyeval[rn] = 8;
					eyemin[rn] = 8;
   				}
				killist(&glist);
   			}
      		else 
				eyepot[rn] = eyevalmax[rn] = eyeval[rn] = eyemin[rn] = 8;
		}
		else if (e1 + e2 == 12) {
			eyepot[rn] = 8;
			if (grsize[g] == 1) {
				eyeval[rn] = 8;
				eyevalmax[rn] = 8;
			}
			else {
				eyeval[rn] = 4;
				eyevalmax[rn] = 8;
			}
			eyemin[rn] = 4;
			if (e1 == 4)
				getcount(s1, c, rn, TRUE, ldrno, libs);  /* fix vital point */
			else 
				getcount(s2, c, rn, TRUE, ldrno, libs);
		}
		if (eyeval[rn] == 8 && cntlist(&liblist)+grsize[g] > 4)
			eyepot[rn] = 12;  /* could be big eye or seki */
	}
	else if (size < 7) {
        deadshape(g, rn, liblist, ldrno, libs);
	}
	else{
		eyepot[rn] = eyeval[rn] = eyemin[rn] = 16;
	}
	if (eyepot[rn] != eyeval[rn] && eyevital[rn] == EOL)
		mrglist(grlbp[g], &eyevital[rn]);
	else {
		for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
			if (link[nbgrp[list[ptr]][G_COLOR(g)]] != EOL) {
				addlist(list[ptr], &eyevital[rn]);	/* can connect to more stones and maybe make bigger nakade */
			}
		}
	}
	killist(&liblist);
	if (eyevalmax[rn] < eyeval[rn])
		eyevalmax[rn] = eyeval[rn];
}


int othr1[] = {0,0,0,4,0,4,0,0,4,4,8,8}; /* eyepot (can't move first after threat) */
int othr2[] = {0,0,0,0,0,0,0,0,0,0,8,8}; /* eyeval */
int othr3[] = {0,0,0,0,0,0,0,0,0,0,0,8}; /* eyemin */

int odead1[] = {0,0,4,8,4,8,3,3,8,8,8,8}; /* eyepot */
int odead2[] = {0,0,0,0,0,0,0,0,5,5,8,8}; /* eyeval */
int odead3[] = {0,0,0,0,0,0,0,0,0,0,0,8}; /* eyemin */

/* evaluate the eye made by a single dead stone at s
 * g is the dead group (or threatened group after killing move made )
 * c is the color of the side that gets the eye.
 */
static void evaloneptdeadeye(sqr_t s, group_t g, int c, eye_t rn, listval_t ldrno, int libs) {
	sqr_t sopen, openspot = 0, sn;
	int numeyespots, j, size, max;
	int numopenspots, ldtm2, count, count2, flag, numenemyspots;
	list_t tmplist = EOL, ptr, ptr2;
	sqr_t sn2;

	if (grlibs[g] == 1 && !G_THREATENED(g)) {
		sopen = list[grlbp[g]];
		numeyespots = 0;
		numopenspots = 0;
		numenemyspots = 0;
		j = fdir[sopen];
		for (ldtm2 = ldir[j]; j != ldtm2; ++j) {
			sn2 = sopen + nbr[j];
			if (board[sn2] != NOGROUP && 
				grcolor[board[sn2]] == grcolor[g] && 
				grsize[board[sn2]] == 1 && 
				grlibs[board[sn2]] == 1) {
					if (sn2 == s || gralive[board[sn2]] == DEAD)
						addlist(sn2, &eyeptr[rn]);
					else {
						if (addlist(ldrno, &ldrflag[sn2]))
							adflist(sn2, &grldr[ldrno]);
					}
					numeyespots++;
			}
			else if (grcolor[board[sn2]] == 1-c)++numenemyspots;
			else if (board[sn2] == NOGROUP) {
				++numopenspots;
				openspot = sn2;
			}
		}
#ifdef CHECK
		if (numeyespots == 0) {
			outerror("no eyespots! in evaloneptdeadeye\n");
			turnoffcplay();
		}
#endif
		if (numeyespots > 1 && eyetype[rn] == DEADEYE) {
			/* only allow many eyespots for dead groups, not threatened groups */
			evalmanyeyespots(rn, sopen, c, numopenspots, numenemyspots, ldrno, openspot, libs);
			mvcolor[msptr] = 1-c;
			mvs[msptr] = sopen;
			flag = lupdate(msptr);
			upldrflags(msptr,ldrno);
			if (!flag || board[sopen] == NOGROUP)  /* japanese or chinese rules */
				eyeval[rn] = eyepot[rn];  /* opponent can't take eye */
			ldndate(msptr);
		}
		else {	/* only one or zero eyespot one liberty */
			count = getcount(s, c, rn, TRUE, ldrno, libs);
			eyeval[rn] = odead2[count];
			eyemin[rn] = odead3[count];

			/* check for extend, sacrifice, throwin at sopen */			
			if (eyeval[rn] >= 8 && 
				(lnbn[sopen] == 1 || grlibs[g] > 1 || onelibnbgrp2(sopen, c))) {
					count = getcount(sopen, c, rn, FALSE, ldrno, libs);
					if (count == 0) {
						eyeval[rn] = 0;
						eyemin[rn] = 0;
					}
			}

			mvcolor[msptr] = c;  /* make the eye and seee what you can get */
			mvs[msptr] = sopen;
			flag = lupdate(msptr);
			upldrflags(msptr, ldrno);
			if (flag) {
				++msptr;
				count = getcount(s, c, rn, FALSE, ldrno, libs);
				--msptr;
			}
			else count = 0;

			eyepot[rn] = odead2[count];
			if (eyepot[rn] < eyeval[rn]) {
				eyeval[rn] = eyepot[rn];
			}
			addlist(sopen, &eyevital[rn]);
			ldndate(msptr);
		}
	}

	else {	/* one stone group with more than one liberty  or threatened */
		count = getcount(s,c,rn,TRUE /*gralive[g] == DEAD 4/99 need all vital points :)*/,ldrno,libs);
		/* 8/12/97 don't add these vital points to threatened group */
		/* some of these vital points could end up on stones after the capturing move is taken back */
		eyepot[rn] = odead1[count];
		eyeval[rn] = odead2[count];
		eyemin[rn] = odead3[count];
		if (eyeval[rn] == 8) {
			cpylist(grlbp[g],&tmplist);  /* because getcount can change grlbp! */
			for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
				if (count <= 10 && comlistm(nblbp[list[ptr]], eyevital[rn], EYEPOINTMASK)) { /* enemy can take one vital point in sente */
					eyeval[rn] = 0;
				}
				count2 = getcount(list[ptr],c,rn,FALSE,ldrno,libs);
				if (count2 <= 10) {	/* 7/03 - better? dead eye extend for false eye? */
					addlist(list[ptr], &eyevital[rn]);
					if (lnbn[list[ptr]] == 1) {
						sn = list[nblbp[list[ptr]]];
						for (ptr2 = nblbp[sn]; ptr2 != EOL; ptr2 = link[ptr2]) {
							if (list[ptr2] == list[ptr])continue;
							if (lnbf[list[ptr2]][1-c] && G_ALIVE(list[nbgrp[list[ptr2]][1-c]]) != DEAD) {
								eyemin[rn] = 0;
								if (eyepot[rn] > eyeval[rn] && eyeval[rn] >= odead2[count2])
									eyepot[rn] = eyeval[rn];
								if (eyeval[rn] > odead2[count2])
									eyeval[rn] = odead2[count2];
							}
						}
					}
				}
				if (lnbf[list[ptr]][1-c] == 1 && 
					(lnbn[list[ptr]] == 0 ||
					lnbn[list[ptr]] == 1 && lnbf[list[nblbp[list[ptr]]]][1-c])) {  /* 5/99 */
						if (odead1[count2] == 0) {  /* can push in to make eye false */
							if (eyeval[rn] == 0 && grlibs[g] > 2) {
								eyepot[rn] = 0;  /* two ways to kill eye */
								break;
							}
							addlist(list[ptr],&eyevital[rn]);
							eyeval[rn] = 0;
							eyemin[rn] = 0;
						}
						else if (odead1[count2] == 3) {	/* ko, but he can take it after push in , so change to 5 */
							eyeval[rn] = 5;
							eyemin[rn] = 0;
							eyeko[rn] = 4;
						}
						if (odead2[count2] == 0)
							eyemin[rn] = 0;
				}						
			}
			killist(&tmplist);
		}
		if (eyeval[rn] == 8 && !G_THREATENED(g)) {  /* check for potential for two eyes */
			size = 1; 
			size += mrglist(grlbp[g],&tmplist);
			max = 0;
			for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
				for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (!can_be_eye(list[ptr2], board[list[ptr2]]) &&
						addlist(list[ptr2],&tmplist)) {
							size++;
					}
				}
				/*size += mrglist(nblbp[list[ptr]],&tmplist);  */
				if (lnbn[list[ptr]] > max)max = lnbn[list[ptr]];
			}
			if (size >= 6 || size-grlibs[g]-1 > 1) {
				eyepot[rn] = 16;
				if (max > 0)
					for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr])
						if (lnbn[list[ptr]] == max)
							addlist(list[ptr],&eyevital[rn]);
			} 
			killist(&tmplist);
		}
		for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr])
			if (lnbn[list[ptr]] == 1 && 
				(lnbf[list[nblbp[list[ptr]]]][1-c] ||
				lnbn[list[nblbp[list[ptr]]]] > 1))
				addlist(list[nblbp[list[ptr]]], &eyevital[rn]);  /* expand eye or peep */
	} 
}

/* eval an eye with several points surrounded on 3 sides opening into
 * a common point.  Sopen is the common point.  rn is the eye record
 * number.  c is color of side with eye.  One or more of the points
 * could be a single stone group.  numopenspots is the number of
 * spots next to sopen which are empty (and not surrounded on 3 sides)
 * numenemyspots is the number of spots next to sopen that have
 * uncaptured enemy stones on them.
 * openspot is one of the open spots.
 *
 * eyeptr[rn] is a list of the eyespots.
 */


static void evalmanyeyespots(eye_t rn, sqr_t sopen, int c, int numopenspots, int numenemyspots, listval_t ldrno, sqr_t openspot, int libs) {
	int eye_pot, eye_val, count,opp_can_fill,flag,numeyespots= 0,numeyes= 0;
	list_t ptr, ptr2;
	sqr_t sn2, noteyespot = NOSQUARE,tmp;
	/* first see what happens if opponent moves in sopen */

	if (numopenspots == 0 && numenemyspots > 1) {  /* may be shortage of liberties */
		mvcolor[msptr] = 1-c;
		mvs[msptr] = sopen;
		opp_can_fill = lupdate(msptr); /* check for opponent shortage of liberties */
		upldrflags(msptr,ldrno);
		ldndate(msptr);
		}
	else opp_can_fill = TRUE;
	if (opp_can_fill) {
		addlist(sopen,&eyevital[rn]);
		}
	/* now see how many eyes we can make */

	mvcolor[msptr] = c;
	mvs[msptr] = sopen;
	flag = lupdate(msptr);
	upldrflags(msptr,ldrno);
        ++msptr;
	if (!flag || S_NUMLIBS(sopen) == 1) {  /* can't play in sopen */
		eye_pot = 8;
		eyeval[rn] = 8;
		eyemin[rn] = 8;
		numeyes = 1;
		numeyespots = 1;
		}
	else {
		eye_pot = 0;
		for (ptr2 = eyeptr[rn]; ptr2 != EOL; ptr2 = link[ptr2]) {
		   sn2 = list[ptr2];
		   numeyespots++;
		   count = getcount(sn2,c,rn,FALSE,ldrno,libs);
		   eye_pot += cpot[count];
	   		if (cpot[count] == 8)numeyes++;
		   else noteyespot = sn2;
		   }
		}
	--msptr;
	ldndate(msptr);

	eyepot[rn] = eye_pot;
	eyeval[rn] = 0;
	eyemin[rn] = 0;

    if (!opp_can_fill) {
		eyeval[rn] = eye_pot;
		eyemin[rn] = eye_pot;
		}

	else if (numenemyspots + numopenspots + numeyespots - numeyes == 0) { /* opponent can't stop one eye */
		if (eye_pot >= 8) {
			eyeval[rn] = 8;
			eyemin[rn] = 8;
			}
		}
	else if (!numenemyspots && numopenspots + numeyespots - numeyes == 1) { /* opponent can't stop one eye */
		if (eye_pot >= 8 && (noteyespot == NOSQUARE || board[noteyespot] == NOGROUP)) {
			eyeval[rn] = 8;
			eyemin[rn] = 0;
			if (numopenspots == 1) {
				if (lnbf[openspot][1-c] == 0) {
					eyemin[rn] = 8;
					for (ptr = nblbp[openspot]; ptr != EOL; ptr = link[ptr]) {
						if ((sqr_t)list[ptr] != sopen && lnbf[list[ptr]][1-c] > 0)
							eyemin[rn] = 0;	/* can push into sopen with two moves in a row */
						for (ptr2 = nbgrp[openspot][c]; ptr2 != EOL; ptr2 = link[ptr2]) {
							if (grlibs[list[ptr2]] == 2 && grsize[list[ptr2]] == 1) {  /* opponent can make a ko */
									eyeval[rn] = 0;
									eyemin[rn] = 0;
									eyeko[rn] = 1;	/* I need a threat to make any eye here */
								}
							}
						}
					}
				else {	/* see if enemy can push into openspot and make eyes false */
					mvs[msptr] = openspot;
					mvcolor[msptr] = 1-c;
					if (lupdate(msptr)) {
						++msptr;
						mvs[msptr] = sopen;
						mvcolor[msptr] = c;
						if (lupdate(msptr)) {
							++msptr;
							eye_val = 0;
							for (ptr2 = eyeptr[rn]; ptr2 != EOL; ptr2 = link[ptr2]) {
								sn2 = list[ptr2];
								count = getcount(sn2,c,rn,FALSE,ldrno,libs);
								eye_val += opt2[count];
								}
							if (eye_val < eyeval[rn])
								eyeval[rn] = eye_val;
							--msptr;
							}
						ldndate(msptr);
						--msptr;
						}
					ldndate(msptr);
					}
				}
			}
		}
	/* now see if opponent can play in sopen without being captured */
	mvs[msptr] = sopen;
	mvcolor[msptr] = 1-c;
	lupdate(msptr);
	upldrflags(msptr,ldrno);
	++msptr;
	if (!iscaptured(board[sopen],eyecapdepth[playlevel],eyecapsize[playlevel],eyemost[playlevel]+1,taclibs[playlevel],c,
				ldrno,&tmp,S_COLOR(sopen))) {
		eyeval[rn] = 0; /* opponent can kill eye */
		eyemin[rn] = 0;
		}
	--msptr;
	ldndate(msptr);
	if (eyeval[rn] > eyemin[rn])
		eyemin[rn] = eyeval[rn];
	}

/* 0,0 = two corners controlled by opponent
 * 1,4 = one opponent, one open
 * 2,8 = both open
 * 3,12 = one or both controlled by self
 */

int ind1[] = { 0,0,0,1,0,0,0,0,1,1,2,3};
int ind2[] = { 0,0,0,4,0,0,0,0,4,4,8,12};	

int tpt1[] = {  0,0,8,8,
		0,8,8,8,
		8,8,8,8,
		8,8,8,8 };

int tpt2[] = {  0,0,0,0,
		0,0,8,4,
		0,8,8,8,
		0,4,8,8 };

int tpt3[] = {  0,0,0,0,
		0,0,0,0,
		0,0,8,8,
		0,0,8,8 };

/* evaluate 2 point completely surrounded eye at s and sopen */


static void eval2pointeye(sqr_t s, sqr_t sopen, int c, eye_t rn, listval_t ldrno, int full, int libs) {
	sqr_t sn;
	list_t tmplist,ptr,ptr2;
	group_t gnbr;
	int count1,count2,index;
	group_t thrnbr = NOGROUP;  /* enclosing group is threatened */
	                        
	eyetype[rn] = TWOPOINTEYE;
	addlist(s,&eyeptr[rn]);
	addlist(sopen,&eyeptr[rn]);
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr])
		if (G_THREATENED(list[ptr]))
			thrnbr = (group_t)list[ptr];
		else if (gralive[list[ptr]] == DEAD) {
			eyepot[rn] = eyeval[rn] = eyemin[rn] = 0;  /* no possible eye here if stone captured */
			return;
			}
	for (ptr = nbgrp[sopen][c]; ptr != EOL; ptr = link[ptr])
		if (G_THREATENED(list[ptr]))
			thrnbr = (group_t)list[ptr];
		else if (gralive[list[ptr]] == DEAD) {
			eyepot[rn] = eyeval[rn] = eyemin[rn] = 0;  /* no possible eye here if stone captured */
			return;
			}
	count1 = getcount(s,c,rn,FALSE,ldrno,libs);
	count2 = getcount(sopen,c,rn,FALSE,ldrno,libs);
	index = ind1[count1] + ind2[count2];
	eyepot[rn] = tpt1[index];
	eyeval[rn] = tpt2[index];
	eyemin[rn] = tpt3[index];
	if (ind1[count1] == 0 && (ind2[count2] == 12 || ind2[count2] == 8)) {
		addlist(s,&eyevital[rn]);
		}
	if ((ind1[count1] == 2 || ind1[count1] == 3) && ind2[count2] == 0) {
		addlist(sopen,&eyevital[rn]);
		}
	if (eyeval[rn] == 4) {	/* need more reading */
		eyeval[rn] = 8;
		sn = sopen;
		if (ind1[count2] == 3) {
			sn = s;
			}
		mvs[msptr] = sn;
		mvcolor[msptr] = 1-c;
		lupdate(msptr);
		upldrflags(msptr,ldrno);
		++msptr;
				/* put down throw in stone */

		tmplist = EOL;
		cpylist(grnbp[board[sn]],&tmplist);
		for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2]) {
			gnbr = (group_t)list[ptr2];
			if (grlibs[gnbr] == 1) {   /* throwin is atari? */
				if (list[grlbp[gnbr]] == list[grlbp[board[sn]]]) {
					eyeval[rn] = 0;
					addlist(sn,&eyevital[rn]);  
            			break;
            		}
				else {
					mvs[msptr] = list[grlbp[gnbr]];
					mvcolor[msptr] = c;
					lupdate(msptr);
					upldrflags(msptr,ldrno);
					if (grlibs[board[mvs[msptr]]] == 1) {
						eyeval[rn] = 0;
						addlist(sn,&eyevital[rn]);
						}
					ldndate(msptr);
					} 
				}
			}
		killist(&tmplist);
		--msptr;
		ldndate(msptr);
		}
	/* redo getcount to set the vital points */
	if (eyeval[rn] == 0 && eyepot[rn] == 8) {
		count1 = getcount(s,c,rn,TRUE,ldrno,libs);
		count2 = getcount(sopen,c,rn,TRUE,ldrno,libs);
		}
	if (eyeval[rn] == 4)
		eyeval[rn] = 0;
	if (eyeval[rn] == 8 && thrnbr != NOGROUP) {
		eyevalmax[rn] = 8;
		if (full || grsavemove[thrnbr] == NOSQUARE || grsavemove[thrnbr] == PASS ||
			board[grsavemove[thrnbr]] == NOGROUP)
			eyeval[rn] = 4;
		eyemin[rn] = 0;
		if (grsavemove[thrnbr] != NOSQUARE && grsavemove[thrnbr] != PASS && 
			board[grsavemove[thrnbr]] == NOGROUP)
			addlist(grsavemove[thrnbr],&eyevital[rn]);
		}
	}


/* values for line eyes closed at both ends by number of spaces in line */

int linepot[] = { 0, 8, 8,12,16, 16, 16 };  /* I moves first */
int lineval[] = { 0, 8, 8, 8,16, 16, 16 };  /* he moves first */
int linemin[] = { 0, 0, 8, 8,16, 16, 16 };  /* he gets two moves */

/* evaluate linear eyes without enemy stones. 2 space eye closed
 * at both ends is treated as a special case elsewhere.
 * s is closed end of line
 * sopen is the adjacent empty point
 * start is the original evaluation point
 * c is color of group surrounding eye.
 * full if all data structures valid
 */

static void evallineeye(sqr_t s, sqr_t sopen, sqr_t start, int c, eye_t rn, listval_t ldrno, int full, int libs) {
	int length, cn, vlength, potlength, minlength, cn2;
	sqr_t corner = NOSQUARE;
	int atari_inside = 0;	/* a point inside the eye, not in an end, where opponent can atari */
	int snap;
	int cap_inside = FALSE, mincaplength=1000, maxcaplength = 0, caplength; /* enemy can capture some of my stones */
	int newpoint;
	int thr_inside = FALSE;
	list_t ataripoints = EOL, ptr, ptr2;
	sqr_t sn,sold,vpoint;
	int maxextra = 0, maxlength;
	int dead;
	int nearend = TRUE;  /* capture splitting eye, near end (s, sopen) is longer */
	army_t army;   /* army of open end of line */
	army_t sarmy;

   	eyetype[rn] = LINEEYE;

	sn = s;	/* find end of line */
	sold = s;
	length = 0;
	do {
		dead = FALSE;
		for (ptr = nbgrp[sn][c]; ptr != EOL; ptr = link[ptr]) {
			if (addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]]))
				adflist(mvs[grpieces[list[ptr]]], &grldr[ldrno]);
			if (mvnext[grpieces[list[ptr]]] != -1 &&
			   addlist(ldrno,&ldrflag[mvs[mvnext[grpieces[list[ptr]]]]]))
				adflist(mvs[mvnext[grpieces[list[ptr]]]], &grldr[ldrno]);
			if (G_ALIVE(list[ptr]) == DEAD) {
				dead = TRUE;	/* NO EYE NEXT TO DEAD GROUP */
				break;
			}
			else if (G_THREATENED(list[ptr]) &&
				grsavemove[list[ptr]] != NOSQUARE &&
				grsavemove[list[ptr]] != PASS &&
				S_GROUP(grsavemove[list[ptr]]) == NOGROUP) {
				cap_inside = TRUE;
				thr_inside = TRUE;
				if (length < mincaplength)
					mincaplength = length;  /* when he captures here will have to block at next point */
				if (length > maxcaplength)
					maxcaplength = length;
				if (grcapmove[list[ptr]] != NOSQUARE && grcapmove[list[ptr]] != PASS)
					addlist(grcapmove[list[ptr]], &eyevital[rn]);
				if (grsavemove[list[ptr]] != NOSQUARE && grsavemove[list[ptr]] != PASS)
					addlist(grsavemove[list[ptr]], &eyevital[rn]);
			}
			else if (grlibs[list[ptr]] == 2) {
				if (lnbn[sn] > 1) {  /* ends are handled by getcount later */
					atari_inside++;
				}
				addlist(sn,&ataripoints);
				snap = FALSE;
				for (ptr2 = nblbp[sn]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (inlist(list[ptr2], &grlbp[list[ptr]]) && 
						lnbn[list[ptr2]] == 1) {
						snap = TRUE;
						break;
					}
				}
				if (snap)
					for (ptr2 = nblbp[sn]; ptr2 != EOL; ptr2 = link[ptr2])
						if (lnbn[list[ptr2]] >= 2) {
							addlist(list[ptr2], &eyevital[rn]);
							break;
						}
			}
			else if (grlibs[list[ptr]] == 1) {
				cap_inside = TRUE;
				addlist(list[grlbp[list[ptr]]], &eyevital[rn]);
			}
		}
		if (dead)
			break;	/* this point not in eye since dead group next to it */
		++length;
		addlist(sn,&eyeptr[rn]);
		if (edge[sn] == 0)corner = sn;
		if (lnbn[sn] == 2 && nbgrp[sn][c] != EOL &&
			(link[nbgrp[sn][c]] != EOL || edge[sn] == 1))
			addlist(sn, &eyevital[rn]);  /* sometime internal cut/throwins work */
		newpoint = FALSE;
		for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr]) {
			if ((sqr_t)list[ptr] != sold) {
				sold = sn;
				sn = list[ptr];
				newpoint = TRUE;
				break;
			}
		}
	} while(newpoint && lnbn[sn] <= 2 && lnbf[sn][1-c] == 0);

#ifdef CHECK
	if (length == 0)
		outerror("Length 0 in evallineeye!\n");
#endif

	/* points from s to sold inclusive are included in eye and length */

	if (lnbn[sn] == 1 && lnbf[sn][1-c] == 0) {  /* closed off both ends */
		 /* length is empty sqrs in line */
		 /* s, sopen is one end, sold, sn is the other */
		 /* measure full length */

		vpoint = sopen;
		vlength = length;
		potlength = length;
		minlength = length;
		if (cap_inside) {
			/*if (mincaplength == 0 || 5/02 ld1e prob 151 - eye move and save move might be the same
				maxcaplength == length-1)
				potlength--; */
			caplength = length;		/* use mincaplength and maxcaplength to find the actual caplength */
			if (mincaplength-1 > length - maxcaplength-2) {
				caplength = mincaplength-1;
				nearend = TRUE;
			}
			else {
				caplength = length - maxcaplength-2;
				nearend = FALSE;
			}
			if (!full && thr_inside)
				maxextra += length-caplength;
			length = caplength;
			minlength = caplength-1;
			/* potlength--; 7/98 move to unthreaten group may be eye making also */
		}
		if (atari_inside) { /* subtract one or two depending on where is atari*/
			if (!inlist(sopen, &ataripoints) && !inlist(sold, &ataripoints) &&
				!inlist(s, &ataripoints) && !inlist(sn, &ataripoints))
				++atari_inside;
			length -= atari_inside;
			minlength -= atari_inside+2;  /* he can capture with two moves */
			mrglist(ataripoints, &eyevital[rn]);
		}
		killist(&ataripoints);
		if (length < 8) {  /* for performance */
			cn = getcount(sn, c, rn, TRUE, ldrno, libs);
			if (cn < 10)	/* 1/03 independent of cap_inside */
				potlength--;
			if (!cap_inside || !nearend) {
				if (cn == 3 || cn == 8) {
					length--;
					minlength--;
				}
				else if (cn < 10) {
					length -= 2;
					minlength -= 2;
					addlist(sn, &eyevital[rn]);
				}
			}
			cn2 = getcount(s, c, rn, TRUE, ldrno, libs);
			if (cn2 < 10)
				potlength--;
			if (!cap_inside || nearend) {
				if (cn2 == 3 || cn2 == 8) {
					length--;
					minlength--;
					vpoint = sold;
				}
				else if (cn2 < 10) {
					length -= 2;
					minlength -= 2;
					addlist(s, &eyevital[rn]);
				}
			}

			if (cn < 10 && cn2 < 10)potlength--;
			if (potlength == 3 && vlength == 3 && !cap_inside) {
				addlist(sopen, &eyevital[rn]);
				potlength++;
			}
			if ((length == 4 || length == 5) && corner != NOSQUARE && lnbn[corner] > 1) {  /* bent 4 in corner */
				for (ptr = nblbp[corner]; ptr != EOL; ptr = link[ptr])
					if (lnbn[list[ptr]] == 2) {
						addlist(list[ptr], &eyevital[rn]);
					}
				if (length == 4) {
					length--;
					minlength--;
				}
			}
		}
		maxlength = length + maxextra;
		if (length < 0)length = 0;
		if (length > 6)length = 6;
		if (potlength < length)
			potlength = length;
		if (potlength < 0)potlength = 0;
		if (potlength > 6)potlength = 6;
		if (minlength < 0)minlength = 0;
		if (minlength > 6)minlength = 6;
		if (maxlength > 6)maxlength = 6;
		if (maxlength < 0)maxlength = 0;
		eyeval[rn] = lineval[length];
		eyevalmax[rn] = lineval[maxlength];
		eyepot[rn] = lineval[potlength];
		eyemin[rn] = linemin[minlength];
#ifdef G2DEBUGOUTPUT
		if (eyeval[rn] < eyemin[rn] || eyeval[rn] > eyepot[rn])
			outerror("Bad line eye");
#endif
		if (length == 3 && vpoint != sopen)addlist(vpoint, &eyevital[rn]);

		if (eyeval[rn] != eyepot[rn] && corner != NOSQUARE && length > 3) {
			addlist(corner, &eyevital[rn]);
		}
	}
	
	else {  /* open ended line. s to sold in line.  sn is past end */
		/* for open ended line, potlength is best potential line length */
		/* length is best length if enemy moves first */

		army = NOARMY;
		for (ptr = nbgrp[sn][c]; ptr != EOL; ptr = link[ptr]) {
			if (G_ARMY(list[ptr]) != NOARMY) {
				army = G_ARMY(list[ptr]);
				break;
			}
		}
		/* make it a very big eye if sn and sold are in big eye */
		if (inbigeye(sn, c, army) && inbigeye(sold, c, army)) {
			killist(&eyeptr[rn]);
			killist(&ataripoints);
			killist(&eyevital[rn]);
			sarmy = NOARMY;
			for (ptr = nbgrp[start][c]; ptr != EOL; ptr = link[ptr]) {
				if (sarmy == NOARMY) {
					sarmy = G_ARMY(list[ptr]);
				}
				else if (G_ARMY(list[ptr]) != sarmy && G_ARMY(list[ptr]) != NOARMY) {
					sarmy = MULTIARMY;
					break;
				}
			}
			
			evalverybigeye(rn, start, sarmy, c, ldrno, full, libs);
			return;
		}
		potlength = length;
		minlength = length;
		if (cap_inside && mincaplength-1 < length) {
			if (!full && thr_inside)		/* might not actually be threatned after all */
				maxextra += length - mincaplength+1;
			else if (potlength > 1)
				potlength--;
			length = mincaplength - 1;  /* TODO: completely wrong if stone at near end is threatened */
			minlength = mincaplength - 2;
		}
		if (atari_inside) {
			if (!inlist(sopen,&ataripoints)) {
				++atari_inside;
				potlength--;  /* can't make eyes and remove atari at same time */
			}
			length -= atari_inside;
			minlength -= atari_inside;
			mrglist(ataripoints, &eyevital[rn]);
		}
		killist(&ataripoints);

		/* check closed end for throwin etc. */

		cn = getcount(s, c, rn, TRUE, ldrno, libs);
		if (cn == 10 && inlist(sn, &eyevital[rn])) { /* 5/02 peep at end is also threatens endpoint */
			length--;
			minlength--;
		}
		if (cn == 3 || cn == 5) {
			if (comlistm(eyevital[rn], eyeptr[rn], EYEPOINTMASK)) {
				length -= 2;  /* opponent can play inside eye without being captured */
				minlength -= 2;
			}
			length--;
			if (potlength > 1)
				potlength--;
			minlength--;
		}
		else if (cn < 10) {
			length -= 2;
			potlength -= 2;
			minlength -= 2;
			addlist(s,&eyevital[rn]);
			if (edge[list[nblbp[s]]] == 0 && 
			   grlibs[board[s+s-list[nblbp[s]]]] == 1)length--;
		}
		else
			addlist(sopen, &eyevital[rn]);  /* make point eye at end of line */

		/* check open end for throw in, etc */

		if (lnbf[sn][1-c] != 0 &&
			lnbf[sn][c] == 0 &&
			gralive[lgr[sn]] != DEAD) {  /* see if have potential for full length */
			mvs[msptr] = sn;
			mvcolor[msptr] = c;
			if (lupdate(msptr)) {
				msptr++;
				if (grlibs[board[sn]] > 1 &&
					getcount(sold, c, rn, FALSE, ldrno, libs) < 10) {
					potlength--;	/* can't make full length - have to close end */
					addlist(sold, &eyevital[rn]);
				}
				else
					addlist(sn, &eyevital[rn]);	/* can make full length of eye */
				--msptr;
			}
			else {
				addlist(sn, &eyevital[rn]);	/* can make full length of eye */
			}
			ldndate(msptr);
		}
		
		if (!cap_inside || !full) {	/* if true (full) inside capture, no need to look at open end */
			if (lnbf[sn][1-c] != 0 && 
				lnbf[sn][c] == 0  
				/*gralive[lgr[sn]] != DEAD */ ||  /* dead group hane can make nakade, davies problem 36 */
			
			    lnbn[sn] == 1 && lnbf[sn][c] == 1 &&
				(grlibs[list[nbgrp[sn][c]]] <= 2 ||
			     grlibs[list[nbgrp[sn][c]]] == 3 && 
				 edge[sn] > 1 &&
			     grcolor[board[sn+sn-sold]] == 1-c)) { 
				              /* enemy can hane or throw in */
				if (lnbf[sn][1-c] == 1 && edge[sn] > 1 &&
				   grcolor[board[sn+sn-sold]] == 1-c ||
				   lnbf[sn][1-c] == 1 && grlibs[list[nbgrp[sn][1-c]]] == 1) {
					length--;  /* can't throw in */
					minlength--;
				}
				else {
					length -= 2;
					minlength -= 2;
					maxextra++;		/* in case we can capture the hane stone */
					if (corner != NOSQUARE && edge2[sold] == 2 && edge[sold] == 1) {
						length--;  /* since can't block in corner square */
						minlength--;
					}
					else if (length == 1 && lnbf[sopen][c] && link[nbgrp[sopen][c]] == EOL &&
						grlibs[list[nbgrp[sopen][c]]] == 3) {
						length--;	/* shortage of liberties daviesld problem 30 */
						minlength--;
					}
				}
			}
			else if (lnbf[sn][1-c] && lnbf[sn][c] && edge[sold] > 1 &&
				lnbn[sold] == 2 && board[sold+sold-sn] != NOGROUP &&
				getcount(sold, c, rn, FALSE, ldrno, libs) == 0) { /* throw in for false eye */
				length -= 3;
				minlength -= 3;
				addlist(sold,&eyevital[rn]);
				addlist(sn,&eyevital[rn]);
			} 
			else if (edge[sn] == 1 && edge[sold] == 1 && lnbf[sn][c] == 0 &&
			   lnbf[sn+sn-sold][c] == 0 && lnbf[sn+sn-sold][1-c] != 0 &&
				gralive[lgr[sn+sn-sold]] != DEAD) {
				length -= 2; /* enemy can throw in on edge */
				minlength -= 2;
				addlist(sold, &eyevital[rn]);
			}
			else if (lnbf[sn][1-c] != 0 && (gralive[lgr[sn]] != DEAD ||
				grcolor[lgr[sn]] != 1-c)) {
				length--;  /* enemy can peep at end */
				minlength -= 2;
				addlist(sn, &eyevital[rn]);
			}
			else if (lnbn[sn] == 1 && lnbf[sn][1-c] >= 1 && lnbf[sn][c] != 0 &&
				(gralive[lgr[sn]] != DEAD || grcolor[lgr[sn]] != 1-c)) { 
				addlist(sn, &eyevital[rn]);
				length--;  /*enemy can push in */
				minlength -= 2;
			}
			else if (lnbf[sn][1-c] == 0 && 
				eyecntplyhere(sn, c, ldrno, libs) == NO) {  /* enemy can peep at end */
				if (edge[sn] > 1)
					addlist(sn, &eyevital[rn]);
				length--;
				minlength -= 2;
			}
			if (lnbf[sn][1-c] == 0) {
				for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr]) {
					if (lnbf[list[ptr]][1-c] != 0) {
						addlist(list[ptr], &eyevital[rn]);
						if (lnbf[list[ptr]][1-c] == 1 && 
							grlibs[list[nbgrp[list[ptr]][1-c]]] == 2)  /* look for jump-peep */
							for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
								if (comlist(nblbp[list[ptr2]], grlbp[list[nbgrp[list[ptr]][1-c]]]) == 2)
									addlist(list[ptr2], &eyevital[rn]);
					}
					else if (edge[sn] == 1 && edge[list[ptr]] > 1)
						addlist(list[ptr], &eyevital[rn]);
				}
			}
		}
/*		if (G_THREATENED(lgr[sold]))length--;
		if (G_THREATENED(lgr[s]))length--; */

		/* check for special shapes */
		for (ptr = nbgrp[sold][c]; ptr != EOL; ptr = link[ptr])
			if (grlibs[list[ptr]] <= 3)
				minlength--;	/* atari after 2 moves */
		
			if (edge[sold] == 0) {  /* special property of the corner */
				for (ptr = nblbp[sold]; ptr != EOL; ptr = link[ptr]) {
					if ((sqr_t)list[ptr] != sn) {
						addlist(list[ptr],&eyevital[rn]);	
						break;
					}
				}
			}

		if (length == 3) {
			addlist(list[nblbp[s]], &eyevital[rn]);
		}
					
		if (edge[s] == 1 && edge2[s] < 4 && length == 4 && corner != NOSQUARE) {  /* for bent 4 */
			for (ptr = nblbp[corner]; ptr != EOL; ptr = link[ptr])
				if (lnbn[list[ptr]] == 2) {
					addlist(list[ptr], &eyevital[rn]);
				}
			length--;
			potlength--;
			minlength--;
		}
		if (sold == corner) {
			length--; /* since can't block in corner */
			minlength--;
		}

		maxlength = length+maxextra;
		if (length > 6)length = 6;
		if (length < 0)length = 0;
		if (potlength > 6)potlength = 6;
		if (potlength < 0)potlength = 0;
		if (minlength < 0)minlength = 0;
		if (minlength > 6)minlength = 6;
		if (maxlength > 6)maxlength = 6;
		if (maxlength < 0)maxlength = 0;
		eyepot[rn] = linepot[potlength];
		eyeval[rn] = lineval[length];
		eyevalmax[rn] = lineval[maxlength];
		eyemin[rn] = linemin[minlength];
#ifdef G2DEBUGOUTPUT
		if (eyeval[rn] < eyemin[rn] || eyeval[rn] > eyepot[rn])
			outerror("Bad line eye");
#endif
		if (edge[sn] > 1 || lnbf[sn][1-c] == 0) {
			for (ptr = nblbp[sn]; ptr != EOL; ptr = link[ptr]) {
				if (lnbf[list[ptr]][1-c] == 0 && lnbn[list[ptr]] <= 2) {
					addlist(sn,&eyevital[rn]);  /* potential extra eyes */
					break;
				}
			}
		}
		if (eyeval[rn] != eyepot[rn] || eyemin[rn] != eyeval[rn]) {
			if (corner != NOSQUARE && corner != s) {
				addlist(corner,&eyevital[rn]);
			}
			addlist(sold,&eyevital[rn]);
		}
	}
}

int onesp1[] = { 0,0,2,4,2,6,6,6,8,8,8,8 }; /* potential for one eyespot */

/* figure out how much a corner opening into a big area is worth */

static void evalbigeye(sqr_t s, sqr_t sopen, sqr_t start, int c, eye_t rn, listval_t ldrno, int full, int libs)
{
	int j, ldtm2, flag, count, numeyespots, verybig = FALSE, dead;
	int tmpli, numopenspots, numenemyspots, qflag, deadnbr;
	sqr_t openspot = 0, blockpoint;
    sqr_t sn2;
    list_t ptr;
	army_t army = NOARMY;	/* friendly army next to sopen point */
	int havearmy = TRUE;	/* no army or one army around sopen, so very nig eye is possible */
	army_t sarmy;
#ifdef CHECK
	char buf[100], buf2[20];
	unsigned int i;
#endif
     
   	eyetype[rn] = BIGEYE;
   	addlist(s, &eyeptr[rn]);

	for (ptr = nbgrp[sopen][c]; ptr != EOL; ptr = link[ptr]) {
		if (army == NOARMY) {
			army = G_ARMY(list[ptr]);
		}
		else if (G_ARMY(list[ptr]) != army) {
			army = NOARMY;
			havearmy = FALSE;
			break;
		}
	}

	/* find other eyespots sharing same sopen */
	/* eyespot is surrounded on 3 sides by friendly stones that are not dead */

	numeyespots = numopenspots = numenemyspots = 0;
	j = fdir[sopen];
	for (ldtm2 = ldir[j]; j != ldtm2; ++j) {
		sn2 = sopen + nbr[j];
		deadnbr = FALSE;  /* adjacent dead friendly group */
		if (board[sn2] == NOGROUP)
			for (ptr = nbgrp[sn2][c]; ptr != EOL; ptr = link[ptr])
				if (gralive[list[ptr]] == DEAD)
					deadnbr = TRUE;
		if (lnbn[sn2] == 1 && board[sn2] == NOGROUP && !deadnbr &&
			lnbf[sn2][1-c] == 0 && lnbf[sn2][c] != 0) {
			addlist(sn2, &eyeptr[rn]);
			numeyespots++;
		}
		else if (grcolor[board[sn2]] == 1-c) {
			if (gralive[board[sn2]] == DEAD && havearmy)
				verybig = TRUE;
			++numenemyspots;
		}
		else if (board[sn2] == NOGROUP) {
			if (havearmy && inbigeye(sn2, c, army))
				 verybig = TRUE;  /* could be part of very big eye */
			if (lnbf[sn2][1-c])
				addlist(sn2, &eyevital[rn]);
			++numopenspots;
			openspot = sn2;
		}
	}

	if ((verybig || numeyespots == 0) && inbigeye(sopen, c, army)) {

		killist(&eyeptr[rn]);
		killist(&eyevital[rn]);
		sarmy = NOARMY;
		for (ptr = nbgrp[start][c]; ptr != EOL; ptr = link[ptr]) {
			if (sarmy == NOARMY) {
				sarmy = G_ARMY(list[ptr]);
			}
			else if (G_ARMY(list[ptr]) != sarmy && G_ARMY(list[ptr]) != NOARMY) {
				sarmy = MULTIARMY;
				break;
			}
		}
		evalverybigeye(rn, start, sarmy, c, ldrno, full, libs);
		return;
	}

#ifdef CHECK
	if (numeyespots == 0) {
		sprintf(buf, "no eyespots! rn=%d evalbigeye at %s\n", rn, ssqr(s, buf2));
		outerror(buf);
		for (i = msptr >= 8?msptr-8:0; i < msptr; ++i) {
			ssqr(mvs[i], buf2);
			outerr(buf2);
		}
	}
#endif

	if (numeyespots > 1) {
		evalmanyeyespots(rn, sopen, c, numopenspots, numenemyspots, ldrno, openspot, libs);
		if (lnbf[sopen][1-c] == 0 && lnbf[sopen][c] != 0) {
			dead = FALSE;	/* 6/01 can't be eye if next to friendly dead geoup */
			for (ptr = nbgrp[sopen][c]; ptr != EOL; ptr = link[ptr]) {
				if (G_ALIVE(list[ptr]) == DEAD) {
					dead = TRUE;
					break;
				}
			}
			if (!dead)
				addlist(sopen, &eyeptr[rn]);
		}
   	}
	else {	/* ony one or zero eyespot */
		/* put down stone to make eye and see if it is eye */
		if (lnbf[sopen][1-c] == 0 && lnbf[sopen][c] != 0) {
			dead = FALSE;
			for (ptr = nbgrp[sopen][c]; ptr != EOL; ptr = link[ptr])
				if (G_ALIVE(list[ptr]) == DEAD) {
					dead = TRUE;
					break;
				}
			if (!dead)
				addlist(sopen, &eyeptr[rn]);
		}
		mvs[msptr] = sopen;
		mvcolor[msptr] = c;
		flag = lupdate(msptr);
		upldrflags(msptr, ldrno);
		++msptr;
		count = getcount(s, c, rn, FALSE, ldrno, libs);
		eyepot[rn] = onesp1[count];
		--msptr;
		ldndate(msptr);

		if (eyepot[rn] >= 4) {
			if (lnbn[sopen] == 3 && 
				(edge[sopen] == 1  || edge[sopen] > 1 && board[sopen+sopen-s] == NOGROUP) && 
				lnbf[sopen][1-c] == 0) {
				for (ptr = nblbp[sopen]; ptr != EOL; ptr = link[ptr])
					if ((sqr_t)list[ptr] != sopen + sopen - s && (sqr_t)list[ptr] != s) {
						addlist(list[ptr], &eyevital[rn]);
						break;
					}
			 	if (ptr != EOL && edge[list[ptr]] > 1 &&
			 		board[list[ptr] + sopen - s] == NOGROUP &&
			 		lnbn[list[ptr] + sopen - s] >= 3)
			 		addlist((sqr_t)(list[ptr]+sopen-s), &eyevital[rn]);
/*				if (ptr == EOL || edge[s] > 1) 9/97 always want to look at this move */
					addlist(sopen, &eyevital[rn]);
			}
			else {
				addlist(sopen, &eyevital[rn]);
				if (count == 3)
					getcount(s, c, rn, TRUE, ldrno,libs);	/* can play the vital point first, so include it */
			}
		}
		else if (eyepot[rn] > 0)
			addlist(sopen, &eyevital[rn]);

		qflag = eyepot[rn] >= 4 && lnbf[sopen][1-c] == 0 &&
			lnbn[sopen] < 4;

		if (qflag) {
			mvs[msptr] = sopen;
			mvcolor[msptr] = 1-c;   /* put down enemy stone */
			flag = lupdate(msptr);
			upldrflags(msptr, ldrno);
			++msptr;
			if (iscaptured(board[sopen], eyecapdepth[playlevel], eyecapsize[playlevel], eyemost[playlevel], taclibs[playlevel], c,
				ldrno, &blockpoint, NOCOLOR) && blockpoint != NOSQUARE && blockpoint != PASS) {	/* if can capture it, see if it makes an eye */
				mvcolor[msptr] = c;
				mvs[msptr] = blockpoint;
				flag = lupdate(msptr);
				upldrflags(msptr, ldrno);
				++msptr;
				tmpli = c7[getcount(sopen, c, rn, FALSE, ldrno, libs)];
				if (tmpli > 1) {
					if (eyepot[rn] == 8)
						eyeval[rn] = 8;
					else {
						eyepot[rn] = 8;
					}
				}
				--msptr;
				ldndate(msptr);
			}
			--msptr;
			ldndate(msptr);
		}

		if (eyeval[rn] == 8) {  /* check if eye can be killed by putting group in atari */
			if (cankillwithatari(s, c, &sn2, ldrno, libs)) {
				eyeval[rn] = 0;
				addlist(sn2, &eyevital[rn]);
			}
			else { /* check if hane can kill eye */
				count = getcount(s, c, rn, TRUE, ldrno, libs);
				if (count <= 3)
					eyeval[rn] = 0;
			}
		}
/*#ifdef NEVER			*/
		else if (eyepot[rn] == 8) {  /* add any extra vital points */
			getcount(s, c, rn, TRUE, ldrno, libs);
		}
/*#endif		*/
	}
	if (lnbf[sopen][c] == 1) {
		for (ptr = nblbp[sopen]; ptr != EOL; ptr = link[ptr]) {
			if (lnbn[list[ptr]] > 1 || lnbf[list[ptr]][c])
				addlist(list[ptr], &eyevital[rn]);	/* try to make eye bigger */
		}
	}
}

/* return TRUE if can kill eye at s by atari on nbring group */
/* c is color of group with eye */

static int cankillwithatari(sqr_t s, int c,sqr_t *s2, listval_t ldrno, int libs)
{
	int i, ldtmp;
	sqr_t sn, sn2;
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		sn = s + nbr[i];
		if (board[sn] != NOGROUP && grlibs[board[sn]] == 2) {
			sn2 = list[grlbp[board[sn]]];
			if (sn2 == s)sn2 = list[link[grlbp[board[sn]]]];
			if (lnbn[sn2] > 1 || lnbf[sn2][1-c] != 0 && !onelibgrnbp(board[sn]) &&
				!eyecntplyhere(sn2, c, ldrno, libs)) {
				*s2 = sn2;
				return(TRUE);
			}
		}
	}
	return(FALSE);
}


#ifdef G2DEBUGOUTPUT

void outeyelist(void)
{
	list_t ptr, ptr2;
	int count;
	char buf[6];
	outerr("Number shows how many times point appears in eyelist.");
	for (ptr = eyelist; ptr != EOL; ptr = link[ptr]) {
		count = 0;
		for (ptr2 = eyelist; ptr2 != EOL; ptr2 = link[ptr2])
			if (list[ptr2] == list[ptr])
				count++;
		sprintf(buf, "%d", count);
		outstone(list[ptr], buf);
	}
}
	
int outgetcount(sqr_t s)
{
	eye_t rn;
	int count, c;
	char buf[120];
	int libs = eyetaclibs[playlevel];
	rn = (eye_t)gtflist(&eyefreelist);
	if (rn == G2ERROR) {
		outerr("Out of eye records.\n");
		return(0);
	}
	if (board[s] != NOGROUP)c = 1-S_COLOR(s);
	else if (lnbf[s][0] && lnbf[s][1]) {
		outerr("can't figure out color\n");
		count = getcount(s, 0, rn, TRUE, (listval_t)(NUMGROUPS+NUMCONNS+rn), libs);
		sprintf(buf, "color %d, Count %d\n", 0, count);
		outerr(buf);
		count = getcount(s, 1, rn, TRUE, (listval_t)(NUMGROUPS+NUMCONNS+rn), libs);
		sprintf(buf, "color %d, Count %d\n", 1, count);
		outerr(buf);
		return(0);
	}
	else 
		c = lnbf[s][1] != 0;
	count = getcount(s, c, rn, TRUE, (listval_t)(NUMGROUPS+NUMCONNS+rn), libs);
	sprintf(buf, "color %d, Count %d\n", c, count);
	outerr(buf);
	delete_eye(rn);
	return 1;
}

char *eytype[] = {
	"None",
	"One point",
	"Two point",
	"Dead",
	"Threatened",
	"Big eye",
	"Line eye",
	"4 point block",
	"Open line eye",
	"Corner eye",
	"Near edge",
	"Very big",
	"Unknown",
	};
	
void dumpeyerec(eye_t rn) {	
	list_t ptr;
	sqr_t s;
	int min,val,pot,ko,count;
	char buf[120], buf2[200];
	count = 1;
	spechilist(grldr[NUMGROUPS+NUMCONNS+rn]);
	for (ptr = eyevital[rn]; ptr != EOL; ptr = link[ptr]) {
		s = (sqr_t)(list[ptr]&EYEPOINTMASK);
		sprintf(buf, "v%d", count);
		if (inlist(s, &eyeptr[rn]))
			strcat(buf, "o");
		outstone(s, buf);
		sprintf(buf, "v%d %s", count, ssqr(s, buf2));
		if (list[ptr] & EYEADDONLY) {
			outerr(buf);
			outerr(" only makes eye.\n");
		}
		if (list[ptr] & EYERMONLY) {
			outerr(buf);
			outerr(" only removes eye.\n");
		}
		count++;
	}
	min = eyemin[rn];
	val = eyeval[rn];
	pot = eyepot[rn];
	ko = eyeko[rn];
	sprintf(buf, "eyerec %d: type %s, min %d, eyes %d-%d, pot %d, ko %d, color %d\neye points: ",rn,eytype[eyetype[rn]],
		min, val, eyevalmax[rn], pot, ko, eyecolor[rn]);
	outerr(buf);
	for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
		if (!inlistm(list[ptr], &eyevital[rn], EYEPOINTMASK))
			outstone(list[ptr], "o");
		ssqr(list[ptr], buf);
		outerr(buf);
	}
	outerr("\n");
	outerr("Reevaluate points marked\n");
	for (ptr = grldr[NUMGROUPS+NUMCONNS+rn]; ptr != EOL; ptr = link[ptr]) {
		histone(list[ptr]);
		ssqr(list[ptr], buf);
		outerr(buf);
		if (!inlist((listval_t)(NUMGROUPS+NUMCONNS+rn), &ldrflag[list[ptr]])) {
			sprintf(buf2, "Error, %s ldrflag missing this eye\n", buf);
			outerr(buf);
		}
	}
	outerr("\n");
}	


void outeyerec(sqr_t s) {
	if (eyerec[s] == 0) {
		outerr("No eye at cursor\n");
		}
	else		
		dumpeyerec(eyerec[s]);
	}

	
void outneweyerec(sqr_t s) {
	eye_t rn;
	int count=1;
	int val, max;
	list_t ptr, ptr2, eyepoints=EOL;
	char buf[100], buf2[20];
	army_t army;
	
	rn = (eye_t)gtflist(&eyefreelist);
	if (rn == G2ERROR) {
#ifdef CHECK				
		outerror("Out of eye records");
#endif				
		return;
		}
	if (!can_be_eye(s, board[s])) {
		outerror("Can't start eye here (altho this is part of an eye)");
		return;
		}
	evaleye(s, rn, (listval_t)(NUMGROUPS+NUMCONNS+rn),TRUE, eyetaclibs[playlevel]);
	                
	dumpeyerec(rn);
	army = 0;
	if (board[s] != NOGROUP) {
		if (grnbp[board[s]] != EOL)
			army = grarmy[list[grnbp[board[s]]]];
		else 
			for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (nbgrp[list[ptr2]][1-grcolor[board[s]]] != EOL) {
					army = grarmy[list[nbgrp[list[ptr2]][1-grcolor[board[s]]]]];
					break;
				}
			}
	}
	else {
		if (nbgrp[s][0] != EOL)
			army = grarmy[list[nbgrp[s][0]]];
		else if (nbgrp[s][1] != EOL)
			army = grarmy[list[nbgrp[s][1]]];
	}

	for (ptr2 = armyeyerecs[army]; ptr2 != EOL; ptr2 = link[ptr2])
			if (eyetype[list[ptr2]] != THRTEYE &&
			   eyetype[list[ptr2]] != DEADEYE &&
			   eyeval[list[ptr2]] >= 8)  /* list of all points in eyes already */
				mrglist(eyeptr[list[ptr2]],&eyepoints);
	for (ptr = eyevital[rn]; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr]&EYERMONLY)continue;
		sprintf(buf,"V%d", count);
		outstone((sqr_t)(list[ptr]&EYEPOINTMASK), buf);
		val = howmuchvital((sqr_t)(list[ptr]&EYEPOINTMASK), army, eyecolor[rn], eyepoints, &max);
		sprintf(buf,"V%d %s potential: %d-%d\n", count, ssqr((sqr_t)(list[ptr]&EYEPOINTMASK), buf2),
			val, max);
		outerr(buf);
		count++;
		}
	delete_eye(rn);
	killist(&eyepoints);
	}
	

#endif
