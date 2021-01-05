/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

# include "g2hd.h"
#ifndef SMALLCODE
# include <string.h> 
#endif
# define B -1
# define W 1
# define NEUT 0

static void chckcorner(int cs, int d1, int d2);
static void chckside(sqr_t start, sqr_t stop, sqr_t dir, sqr_t dir2, int udir, int udir2);
void chckeyelist(sqr_t ptr2);
int canundercut(sqr_t sg, int dir, int dir2, sqr_t sn, int dist);

static unsigned char oldltr1[NUMSQUARES],oldltrgd[NUMSQUARES];

/* lgr is not valid yet, since life and terr not evaluated yet */
  
void upltr(void)     /* fix the ltr data structure */
{
	sqr_t s;
	int x,y;
	memcpy(oldltr1, ltr1, NUMSQUARES);    /* save old values */
	memcpy(oldltrgd, ltrgd, NUMSQUARES);                                                              
	ymin -= 3; xmin -= 3; ymax += 3; xmax += 3;
	if (ymin < 5)
		ymin = 0;
	if (xmin < 5)
		xmin = 0;
	if (ymax > boardsize - 6)
		ymax = boardsize - 1;
	if (xmax > boardsize - 6)
		xmax = boardsize - 1;
  
	if (xmax < 4)
		xmax = 4;
	if (xmin > boardsize - 5)
		xmin = boardsize - 5;
	if (ymax < 4)
		ymax = 4;
	if (ymin > boardsize - 5)
		ymin = boardsize - 5;
  

	/* clear out old values */
	for (x = xmin; x <= xmax; ++x) {
		for (y = ymin; y <= ymax; ++y) {
			s = y * boardsize + x;
			if (edge[s] > 4)
				continue;
			ltr1[s] = 0;
			ltrgd[s] = FALSE;
			ltrcolor[s] = NOCOLOR;
        }
	}
	if (ymin == 0)
		chckside(xmin, xmax, boardsize, (sqr_t)1, RIGHT,LEFT);
	if (ymax == boardsize - 1)
		chckside((sqr_t)(xmin + boardsquare - boardsize), (sqr_t)(xmax + boardsquare - boardsize),
               (sqr_t)-boardsize, (sqr_t)1, RIGHT, LEFT);
	if (xmin == 0)
		chckside((sqr_t)(ymin * boardsize), (sqr_t)(ymax * boardsize), (sqr_t)1, boardsize, DOWN,UP);
	if (xmax == boardsize - 1)
		chckside((sqr_t)(ymin * boardsize + boardsize - 1), (sqr_t)(ymax * boardsize + boardsize - 1),
              (sqr_t)-1, boardsize, DOWN, UP);

	if (xmin == 0 && ymin == 0)   /* corner */
		chckcorner(0, 1, boardsize);
	if (xmax == boardsize - 1 && ymin == 0)   /* corner */
		chckcorner(boardsize - 1, -1, boardsize);
	if (xmin == 0 && ymax == boardsize-1)   /* corner */
		chckcorner(boardsquare-boardsize, 1, -boardsize);
	if (xmax == boardsize - 1 && ymax == boardsize - 1)   /* corner */
		chckcorner(boardsquare - 1, -1, -boardsize);
	for (x = xmin; x <= xmax; ++x) {
		for (y = ymin; y <= ymax; ++y) {
			s = y * boardsize + x;
			if (ltrgd[s] != oldltrgd[s])
				chckeyelist(s);
		}
	}
	xmin = ymin = 18;
	xmax = ymax = 0;
}

/* adjust corner territory.  cs is corner square.  d1 and d2 are directions */

static void chckcorner(int cs, int d1, int d2) {
	char n = 0;
	int tmp;
	char c;
	tmp = cs + (d1+d2)*2;  /* 3-3 point */
	if (ltr1[tmp] > 3 && /* must be uninterrupted to edge both ways */
		ld[tmp] > 1 && ld[tmp] != NOLD) {
		c = G_COLOR(lgr[tmp]);
		if (ltrgd[tmp] == 4 && ltrgd[tmp-d1] == 4 && ltrgd[tmp-d2] == 4)
	   		ltrgd[tmp] = 8;
		if (ltr1[cs+d1+d2] == 0 && 
		   S_GROUP(cs+d1+d2) == NOGROUP) {
			++n;
			ltr1[cs+d1+d2] = 9;
			ltrgd[cs+d1+d2] = ltrgd[cs+d1+d1+d2] | ltrgd[cs+d1+d2+d2] | ltrgd[tmp];
			if (ltrgd[cs+d1+d2] == 0)
				ltrcolor[cs+d1+d2] = c;
			}
		if (ltr1[cs+d2] == 0 && 
		   S_GROUP(cs+d2) == NOGROUP) {
			++n;
			ltr1[cs+d2] = 9;
			ltrgd[cs+d2] = ltrgd[cs+d2+d2] | ltrgd[cs+d2+d1] | ltrgd[tmp];
			if (ltrgd[cs+d2] == 0)
				ltrcolor[cs+d2] = c;
			}
		if (ltr1[cs+d1] == 0 &&
		   S_GROUP(cs+d1) == NOGROUP) {
			++n;
			ltr1[cs+d1] = 9;
			ltrgd[cs+d1] = ltrgd[cs+d1+d1] | ltrgd[cs+d1+d2] | ltrgd[tmp];
			if (ltrgd[cs+d1] == 0)
				ltrcolor[cs+d1] = c;
			}
		if (ltr1[cs] == 0 && 
		   S_GROUP(cs) == NOGROUP) {
			++n;
			ltr1[cs] = 9;
			ltrgd[cs] = ltrgd[cs+d1] | ltrgd[cs+d2] | ltrgd[tmp];
			if (ltrgd[cs] == 0)
				ltrcolor[cs] = c;
			}
		ltr1[tmp] += n;
		}
	else if (ltr1[cs+d1] != 0 && ltr1[cs+d2] != 0) {
		if (ltr1[cs] == 0 && S_GROUP(cs) == NOGROUP) {  /* fill in corner square */
			if (ld[cs+d1+2*d2] > 1 && ld[cs+d1+2*d2] != NOLD) {
				ltr1[cs] = 9;
				ltrgd[cs] = ltrgd[cs+d1] | ltrgd[cs+d2];
				if (ltrgd[cs] == 0)
					ltrcolor[cs] = G_COLOR(lgr[cs+d1+2*d2]);
				ltr1[cs+d1+2*d2] ++;
				}
			else if (ld[cs+2*d1+d2] > 1 && ld[cs+2*d1+d2] != NOLD) {
				ltr1[cs] = 9;
				ltrgd[cs] = ltrgd[cs+d1] | ltrgd[cs+d2];
				if (ltrgd[cs] == 0)
					ltrcolor[cs] = G_COLOR(lgr[cs+2*d1+d2]);
				ltr1[cs+2*d1+d2] ++;
				}
			}
		}
	}
	
/* return TRUE if this point is at the top of territory for color c, enemy can put stone here
 * - will get
 * ltrgd equal to 3, but not extended toward edge of board
 * dir towards center, dir2 along edge
 */
	
static int topofterr(sqr_t s, int dir, int c, group_t g)
{
	if (lnbf[s][1-c] == 0 && board[s+dir] == NOGROUP &&
		lnbf[s+dir][1-c] != 0 && !S_NEUTRAL(s+dir) && G_COLOR(lgr[s+dir]) == 1-c &&
		gralive[lgr[s+dir]] != DEAD && gralive[g] != DEAD ||
		G_THREATENED(g) == 2) {	/* 2/02 unconditional only makes a 3.  */
		return(TRUE);
	}
	return(FALSE);
}

/* return TRUE if territory is undercuttable with hane or similar move */
/* TRUE means that enemy (1-c) can place a live stone on s or otherwise */
/* force c to play in points next to s.  */
/* includes if can be ataried */
/* dir is towards center, dir2 is along edge, c is color of territory */

int edgeofterr(sqr_t s, int dir, int dir2, int c) {
	int sn,ptr;
	if (lnbf[s][1-c] == 0) {  /* check for cut */
		for (ptr = cnbrd[s]; ptr != EOL; ptr = link[ptr])  /* cut hane or one pt jump */
			if (cnprot[list[ptr]] <= THRT_CONNECT && G_ARMY(cngr1[list[ptr]]) != G_ARMY(cngr2[list[ptr]]))
				return(TRUE); /* he can cut (and put a stone here) */ 
		if (cnbrd[s] == EOL) {
			for (ptr = lkbrd[s]; ptr != EOL; ptr = link[ptr]) {  /* cut 2 pt jump */
				if (cncnum[list[ptr]] == 0 && cntype[list[ptr]] == CN_TWOPOINTJUMP && 
					cnprot[list[ptr]] <= THRT_CONNECT && G_ARMY(cngr1[list[ptr]]) != G_ARMY(cngr2[list[ptr]]))
					return(TRUE); /* he can cut a 2 point jump here 7/01 took out allowing knights move, since puts 3 at wrong point */
				}
#ifdef NEVER
TODO: knight move cuts, so that eye vital points will find knights move connection
			if (lnbf[s][c] == 1 && lnbf[s][1-c] == 0)
				for (ptr = nblbp[s]; ptr != EOL: ptr = link[ptr]) {  /* cut simple knights move */
					if (lnbf[list[ptr]][c] == 1) {

						}
					}
#endif
			}
		}
#ifdef NEVER  /* this makes possible eyes go to zero, pretty bad! */
	if (!S_NEUTRAL(s)) {  /* check for cut */
		for (ptr = cnbrd[s]; ptr != EOL; ptr = link[ptr])
			if (cnprot[list[ptr]] < AJI_CONNECT)
				return(TRUE); /* he can cut */
		if (cnbrd[s] == EOL)
			for (ptr = lkbrd[s]; ptr != EOL; ptr = link[ptr])
				if (cncnum[list[ptr]] == 0 && cnprot[list[ptr]] < AJI_CONNECT)
					return(TRUE); /* he can cut */
		}
#endif		
	sn = s + dir2 + dir;  /* diagonally up and out */
/*	if (edge[sn] <= edge[s])return(FALSE); not in corner, towards edge */
	if (board[s+dir2] != NOGROUP) {  /* stone next to empty point */
		if (S_COLOR(s+dir2) == 1-c &&   /* dead enemy */
		   gralive[board[s+dir2]] == DEAD)return(FALSE);
		if (grlibs[board[s+dir2]] == 1)return(TRUE); /* can be captured */
		if (grsize[board[s+dir2]] == 1 &&
		   grlibs[board[s+dir2]] == 2 &&
		   edge[s+dir2] > 1) {
			for (ptr = grlbp[board[s+dir2]]; ptr != EOL; ptr = link[ptr])
				if ((sqr_t)list[ptr] != s && lnbn[list[ptr]] > 1)return(TRUE);
			}
		if (grlibs[board[s+dir2]] <= 3 && lnbn[s] == 3 &&
			board[s+dir] == NOGROUP && board[sn] != NOGROUP &&
			S_COLOR(sn) == 1-c && gralive[board[sn]] != DEAD)
			return TRUE;
		if (edge[s] > 1 && S_COLOR(s-dir+dir2) == 1-c &&
			gralive[board[s-dir+dir2]] != DEAD &&
				(edge[s-dir] <= 2 || grlibs[board[s-dir+dir2]] > 2))
					return(TRUE);  /* enemy can hane up from edge */
		}
	else {  /* open spot to side of terr */
		if (board[sn] == NOGROUP) {  /* empty spot at diagonal */
			if (edge[s] == 1 && edge[sn] > 1 && 
			   S_COLOR(sn+dir2) == 1-c &&
			   gralive[board[sn+dir2]] != DEAD)
			   return(TRUE); /* 2 pts away on 2nd line */
			if (edge[s] == 1 && edge[sn] > 1 && 
				board[s+dir] == NOGROUP &&
				S_COLOR(sn+dir) == 1-c &&
				gralive[board[sn+dir]] != DEAD)
				return(TRUE);  /* knight above on 3rd line */
			if (edge[sn] > 2 && 
				board[s+dir] == NOGROUP &&
				board[sn-dir+dir2] == NOGROUP &&
				board[sn-dir-dir] == NOGROUP) {
				if (lnbf[s+dir][1-c] && !lnbf[s+dir][c])
					return(TRUE);  /* enemy above */
				if (S_COLOR(sn+dir2) != c &&
				   S_COLOR(sn+dir) == 1-c && 
				   S_ALIVE(sn+dir) != DEAD)return(TRUE); /* over one, up 2 */
				if (gralive[board[sn+dir2]] != DEAD &&
				   S_COLOR(sn+dir2) == 1-c)
					return(TRUE);  /* over 2, up one */
				if (edge[sn+dir2] > 1 && board[sn-dir+dir2+dir2] == NOGROUP &&
				   board[sn+dir2] == NOGROUP &&
				   lnbf[s+dir2][c] == 0 &&
				   gralive[board[sn+dir2+dir2]] != DEAD &&
				   S_COLOR(sn+dir2+dir2) == 1-c)
					return(TRUE);
				}
			}
		else {  /* stone up and out */
			if (gralive[board[sn]] != DEAD && S_COLOR(sn) == 1-c &&
				lnbf[s+dir2][c] == 0 && lnbn[s] > 1)
				return(TRUE);  /* enemy one line over and up */
	/*		if (edge[sn] > 1 && edge[s] > 1 && grcolor[board[sn]] == c &&
			   S_GROUP(sn-dir+dir2) == NOGROUP && 
			   grcolor[board[sn+dir2]] == 1-c &&
			   gralive[board[sn+dir2]] != DEAD)
				return(TRUE);  enemy two lines over and up */
			}
		}
	return(FALSE);  
	}



/* do territory for a side.
 * dir is direction towards the center as offset.  dir2
 * is direction along edge as offset.  udir and udir2 are directions
 * along edge as indeces.  dir2 and udir are the same direction.
 * s must be in a liberty of a stone
 * dist is the distance from the edge (1 is on edge)
 */  
int findltrgd(sqr_t s, int dir, int dir2, int udir, int udir2, int dist)
{
	int goodflag = 0, stone_above, between_stones;
	group_t g;	/* adjacent dead or threatened group */
	int c;		/* color of friendly group next to s */
	list_t ptr;
	int deadnbr = FALSE;

	if (nbgrp[s][0] != EOL)
		c = 0;
	else
		c = 1;
	g = (group_t)list[nbgrp[s][c]];
	c = G_COLOR(g);
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
		if (G_ALIVE(list[ptr]) == DEAD) {
			c = 1-c;	/* 6/03 dead groups count as live groups for the other side */
			g = (group_t)(list[ptr]);
			deadnbr = TRUE;
			break;
		}
		if (G_THREATENED(list[ptr]) == 2)
			g = (group_t)list[ptr];
	}
	if (!deadnbr) {
		for (ptr = nbgrp[s][1-c]; ptr != EOL; ptr = link[ptr]) {
			if (G_ALIVE(list[ptr]) == DEAD) {
				deadnbr = TRUE;
				g = (group_t)(list[ptr]);
				break;
			}
		}
	}
	stone_above = S_GROUP(s+dir) != NOGROUP || 
					(lnbf[s][1-c] == 0 &&
					 lnbf[s][c] >= 2 &&
					 lnbf[s+dir][c] != 0);
	between_stones =
		dist > 1 && edge[s] == 1 ||  /* between stone and edge */
		edge[s] == 1 &&
		   (board[s+dir2] == NOGROUP && lnbf[s+dir2][c] && !lnbf[s+dir2][1-c] ||
		    board[s-dir2] == NOGROUP && lnbf[s-dir2][c] && !lnbf[s-dir2][1-c] ) ||
		edge[s] > 1 &&
		   ((S_GROUP(s-dir2) != NOGROUP && 
		      (lnbf[s+dir2][1-c] == 0 && lnbf[s+dir2][c] != 0 &&
		          (board[s+dir2+dir] != NOGROUP ||  /* knight move and 2 point jump */
		           lnbf[s+dir2+dir][1-c] == 0 ||
		           edge2[s+dir2] > 1 && 
		           S_COLOR(s+dir2+dir2) == c && 
		           S_NUMLIBS(s+dir2+dir2) >= 4) ||
			   edge2[s] <= 4 && 
			   	   (edge2[s+dir2] < edge2[s] || /* near corner */
			       edge[s+dir2] < edge[s]) ||
			   edge[s+dir2] > 1 &&
			       lnbf[s+dir2][1-c] == 0 && 
			       lnbf[s+dir2+dir][1-c] == 0 &&
				   (lnbf[s+dir2+dir][c] != 0 ||
				       lnbf[s+dir2+dir2][1-c] == 0 &&
				       lnbf[s+dir2+dir2][c] != 0))) ||

			(S_GROUP(s+dir2) != NOGROUP && 
			   (lnbf[s-dir2][1-c] == 0 && lnbf[s-dir2][c] != 0 &&
		          (board[s-dir2+dir] != NOGROUP || 
		           lnbf[s-dir2+dir][1-c] == 0 ||
		           edge2[s-dir2] > 1 && 
		           S_COLOR(s-dir2-dir2) == c && 
		           S_NUMLIBS(s-dir2-dir2) >= 4) ||
			    edge2[s] <= 4 && 
			       (edge2[s-dir2] < edge2[s] || /* near corner */
			       edge[s-dir2] < edge[s]) ||
			    edge[s-dir2] > 1 &&
			       lnbf[s-dir2][1-c] == 0 && 
			       lnbf[s-dir2+dir][1-c] == 0 &&
				   (lnbf[s-dir2+dir][c] != 0 ||
				       lnbf[s-dir2-dir2][1-c] == 0 &&
				       lnbf[s-dir2-dir2][c] != 0))) ||
    				S_GROUP(s-dir2) != NOGROUP && S_GROUP(s+dir2) != NOGROUP);
	if (lnbf[s][1-c] != 0) {
		if (!deadnbr) /* right, but (caused bad verybigeye expansion) if dead, only one point */
			if (lnbn[s] != 1 ||	/* all the way to edge unless: 1 empty points next to */
				lnbf[s][c] < 2 ||  /* 2 or more friendly stones that are connected */
				link[nbgrp[s][c]] != EOL && grarmy[list[nbgrp[s][c]]] != grarmy[list[link[nbgrp[s][c]]]] ||
				edge[list[nblbp[s]]] >= edge[s] ||
				S_COLOR(s + s - list[nblbp[s]]) != 1-c)  /* single enemy stone is away from edge */
			goodflag = 2;	/* 2 all the way to the edge */
	}
	else if ((edge[s] != 0 && dist == 1 && stone_above || edge[s] > 1) &&
		edgeofterr(s,dir,dir2,c))goodflag = 3;
	else if ((edge[s] != 0 && dist == 1 && stone_above || edge[s] > 1) &&
		edgeofterr(s,dir,-dir2,c))goodflag = 3;
	else if (!stone_above && !between_stones) {
		goodflag = 1;
	}
/*	else if (edge[s] == 4 && ld[s] < 3)goodflag = 1; */
	if (lnbf[s][1-c] == 0)  /* so c is valid */
		goodflag += undercut(s,-dir,dir2,c,udir,udir2);
	if (topofterr(s,dir,c,g))
		goodflag |= 32;  /* 3 but only for top spot */
	return goodflag;
}

/* do territory for a side.  start, stop, run along the edge
 * of the side.  dir is direction towards the center as offset.  dir2
 * is direction along edge as offset.  udir and udir2 are directions
 * along edge as indeces.  dir2 and udir are the same direction.
 */  
static void chckside(sqr_t start, sqr_t stop, sqr_t dir, sqr_t dir2, int udir, int udir2)
{
	int c, gral, ii, ptr;
	sqr_t s, x;
	group_t g;
	sqr_t ptr2;
	int touched[10], dist, last;
	int n;	/* number of nonliberty points to add when we find a liberty */
	char goodflag;
     
	for (x = start; x <= stop; x += dir2) {	/* walk along edge */
		goodflag = 0;
		n = 0;
		ptr = 0;
		s = x - dir;
		last = s;
		for (dist = 1; dist <= 4; dist++) {	/* walk up from edge */
			s += dir;
			if (S_GROUP(s) != NOGROUP)
				break;  /* ran into stone */
			if (ld[s] != NOLD) {	/* found a liberty*/
				g = lgr[s];
				c = G_COLOR(g);
				
				if (gralive[g] == DEAD) {
					c = 1-c;
				}
				goodflag = findltrgd(s, dir, dir2, udir, udir2, dist);
					
				gral = gralive[g] & 31;
				if (ltr1[s] == 0) { /* don't double count this spot */
					++n;
				}
				for (ii = 0; ii < ptr; ii++) {
					ltr1[touched[ii]] = 9;
				}
				for (ptr2 = last + dir; ptr2 != s; ptr2 += dir) {  /* add good to all points not liberties */
					if (ld[ptr2] == NOLD) {
 						ltrgd[ptr2] |= (goodflag & 31);
						if (ltrgd[ptr2] == 0)  /* 2/01 have to make ltrgd==0 same color as surrounding stones for big eyes */
							ltrcolor[ptr2] = c; /*G_COLOR(g); 6/03 try putting it back the other way */
						else if ((ltrgd[ptr2]&12) == 0 && (ltrgd[ptr2]&2) != 2)
							ltrcolor[ptr2] = c;
						else
							ltrcolor[ptr2] = NOCOLOR;
						chckeyelist(ptr2); 
					}
				}
				if (!S_NEUTRAL(s) && (goodflag & 31) != 1 && (goodflag & 32)) {
					goodflag |= 3;
					ltrcolor[s] = c;
				}
				goodflag &= 31;
				if (S_NEUTRAL(s) && gralive[lgr[s]] != DEAD) {
					goodflag |= 2;  /* neutral point itself always gets a 2 */
				}
				last = s;
				ptr = 0;
				if (ltr1[s] == 0) {
					ltrgd[s] = goodflag;  /* first time */
					if (goodflag == 3)
						ltrcolor[s] = c;
				}
				else if (ltrgd[s] == 1 && goodflag == 0) {
					ltrgd[s] = 0;
				}
				else if (ltrgd[s] != 0 || goodflag != 1) {
					ltrgd[s] |= goodflag;
					ltrcolor[s] = NOCOLOR;
				}
				ltr1[s] += n;
				if (ltrgd[s] == 0) {  
					ltrcolor[s] = c;
				}
				chckeyelist(s);  
				n = 0;
			}
			if (ltr1[s] == 0 && edge2[s] > dist) {  /* not counted, and on this edge */
				++n;
				touched[ptr++] = s;
			}
		}
	}
}
  

void chckeyelist(sqr_t s)
{
	list_t ptr, ptr3, elist = EOL;
	if (ltrgd[s] != oldltrgd[s]) {  /* changed 3 */
		if (ld[s] != NOLD)
			adflist(s, &eyelist);
		for (ptr3 = nblbp[s]; ptr3 != EOL; ptr3 = link[ptr3]) {
			if (eyerec[list[ptr3]] != 0) {
			   addlist(eyerec[list[ptr3]], &elist);
			}
			else {
				for (ptr = ldrflag[list[ptr3]]; ptr != EOL; ptr = link[ptr]) {
					if (list[ptr] >= NUMGROUPS + NUMCONNS) {
						if (eyeptr[list[ptr] - NUMGROUPS - NUMCONNS] != EOL) {
							addlist((listval_t)(list[ptr] - NUMGROUPS - NUMCONNS), &elist);
						}
					}
				}
			}
		}
	}
	if (ltrgd[s] != 0 && eyerec[s] != 0 && 
		eyetype[eyerec[s]] == VERYBIGEYE) {
		addlist(eyerec[s], &elist); /* new eye here */
	}
	if (ltrgd[s] == 0 && ltr1[s] != 0 && (eyerec[s] == 0 ||
				eyetype[eyerec[s]] != VERYBIGEYE)) {
		adflist(s, &eyelist);  /* needs reeval */
		for (ptr3 = nblbp[s]; ptr3 != EOL; ptr3 = link[ptr3]) {
			if (eyerec[list[ptr3]] != 0)
				addlist(eyerec[list[ptr3]], &elist);
			/* 6/01 - liberty of dead group will be bridge for verybigeye, but not included in dead group eye */
			for (ptr = nbgrp[list[ptr3]][0]; ptr != EOL; ptr = link[ptr]) {
				if (G_ALIVE(list[ptr]) == DEAD)
					adflist(mvs[grpieces[list[ptr]]], &eyelist);
			}
			for (ptr = nbgrp[list[ptr3]][1]; ptr != EOL; ptr = link[ptr]) {
				if (G_ALIVE(list[ptr]) == DEAD)
					adflist(mvs[grpieces[list[ptr]]], &eyelist);
			}
		}
	}
	for (ptr = elist; ptr != EOL; ptr = link[ptr])
		adflist(list[eyeptr[list[ptr]]], &eyelist);
	killist(&elist);
}

/* return distance (0-3) to nearest stone to using offset dir and the point
 * matches incremental dstbrd and sqrbrd
 */

sqr_t getdst(sqr_t s, int dir, int *dist) {
	*dist = 0;
	if (edge[s] <= 1) {
		if (s + dir < 0 || s+dir > boardsquare)
			return NOSQUARE;
		if (edge[s] == 1 && edge[s+dir] <= 1 && edge[s+dir+dir] > edge[s+dir])	/* wrapped */
			return NOSQUARE;
		if (edge[s] == 0 && edge[s+dir+dir] != 1)
			return NOSQUARE;
	}
	for (*dist = 0; *dist < 4; ++*dist) {
		s += dir;
		if (board[s] != NOGROUP)
			return s;
		if (edge[s] == 0 || edge[s] == 1 && edge[s-dir] > 1) {
			*dist = 0;
			return NOSQUARE;
		}
	}
	*dist = 0;
	return NOSQUARE;
}

/* return undercut value if square s is undercut
 * 4 if undercut one direction and 8 if undercut two ways
 * c is the color of this square
 * dir is direction to the
 * edge of the board (amount to add).  
 * dir2 is direction along edge of board to left (facing edge)
 * Undercut is if any squares between
 * this square and the edge of the board inclusive have a live 
 * enemy stone within 4 (2 for threatened)
 * lines along the edge.  Or if any squares have a dead or threatened 
 * friendly stone
 * within 2 lines along the edge.
 * udir and udir2 are directions along edge.
 */

int undercut(sqr_t s, int dir, int dir2, int c, int udir,int udir2)
{
	sqr_t sn, sqr;
	int lcount = 0,rcount=0,g,uc,cval,dist;
	int friendly = 0;  /* how many friendly liberties have been traversed */
	if (lnbf[s][c] != 0 && lnbf[s][1-c] == 0 &&
		G_THREATENED(lgr[s]) == 2)
		return 0;	/* threatened groups can't control points to edge */
	sn = s-dir;
	do {
		sn += dir;
		if (board[sn] != NOGROUP)
			break;  /* stone toward edge of board - sholdn't happen, but... */
		if (lnbf[sn][c] != 0 && lnbf[sn][1-c] == 0)
			++friendly;
		sqr = getdst(sn, dir2, &dist);
#ifdef NEVER
		g = board[sqrbrd[sn][udir]];
		dist = dstbrd[sn][udir];
#endif
		g = board[sqr];
		uc = G_COLOR(g);
		/* 4/00 can't have threatened groups undercut at a distance.  kills big eyes too much */
		if (gralive[g] != DEAD && G_THREATENED(g) != 2 && 
			uc == 1-c && canundercut(sqrbrd[sn][udir],dir,dir2,sn,dist)) {
			lcount++;
		}
		if ((gralive[g] == DEAD || G_THREATENED(g) == 2) && uc == c &&
			dist < 1)
			lcount++;
		sqr = getdst(sn, -dir2, &dist);
#ifdef NEVER
		g = board[sqrbrd[sn][udir2]];
		dist = dstbrd[sn][udir2];
#endif
		g = board[sqr];
		uc = G_COLOR(g);
		if (gralive[g] != DEAD && G_THREATENED(g) != 2 &&
			uc == 1-c && canundercut(sqrbrd[sn][udir2],dir,-dir2,sn,dist)) {
			rcount++;
		}
		if ((gralive[g] == DEAD || G_THREATENED(g) == 2) && uc == c &&
			dist < 1)
			rcount++;
		if (lcount == 0 && rcount == 0 && friendly >= 2)
			break;  /* don't look down past one friendly liberties */
	}while(edge[sn] != 0 && (edge[sn] != 1 || edge[sn-dir] != 2));
	cval = 0;
	if (rcount)cval += 4;
	if (lcount)cval += 4;
	return(cval);
}

/* return TRUE if the stone at sg can undercut square sn 
 * at distance dist (number of empty points between sg and sn).
 * dir is direction toward edge of board  
 * dir2 is amount to add to sn to go towards sg
 */

int canundercut(sqr_t sg, int dir, int dir2, sqr_t sn, int dist) {
	int ptr;
	if (gralive[board[sg]] == DEAD)return(FALSE);
	if (dist < 1)return(TRUE);
	if (grsize[board[sg]] == 1 &&
		grlibs[board[sg]] == 2 && lnbn[sg-dir2] == 1 && 
		lnbf[sg-dir2][S_COLOR(sg)] == 1 &&
		dist >= 1)return(FALSE);
	        /* can give atari when he pushes */
	if (dist < 2)return(TRUE);
	if (S_THREATENED(sg))return(FALSE);
	if (ld[sn+dir2] >= 4 && ld[sn+dir2] != NOLD && !S_NEUTRAL(sn+dir2))
		return(FALSE); /* one pt jump can't get thru */ 
	if (ld[sn+dir2] == 3 && S_COLOR(sn+dir2+dir2+dir) == 1-S_COLOR(sg) &&
		S_NUMLIBS(sn+dir2+dir2+dir) >= 4)
		return(FALSE);  /* knight move block */
	if (lnbf[(signed)sg-dir2-dir2][S_COLOR(sg)] == 0 && 
	   lnbn[(signed)sg-dir2] == 1 && lnbf[(signed)sg-dir2][S_COLOR(sg)] == 1) {
		if (lnbn[sg-dir2-dir2] == 2)return(FALSE);
		/* can block when he pushes */
		for (ptr = nblbp[(signed)sg-dir2-dir2]; ptr != EOL; ptr = link[ptr])
			if (ld[list[ptr]] >= 4 && ld[list[ptr]] <= 8)
				return(FALSE);
		/* can block when he pushes */
	}
	if (lnbf[(signed)sg-dir2-dir2][S_COLOR(sg)] == 0 && 
		lnbn[(signed)sg-dir2-dir2] == 2)return(FALSE);
	        /* can atari when he jumps in (or wedges or hanes) */
	if (dist < 3)return(TRUE);
	if (lnbf[(signed)sg-dir2-dir2-dir2][S_COLOR(sg)] == 0 &&
	   lnbn[(signed)sg-dir2] == 1 && lnbf[(signed)sg-dir2][S_COLOR(sg)] == 1 &&
		lnbn[(signed)sg-dir2-dir2-dir2] == 2)return(FALSE);
	if (lnbf[(signed)sg-dir2-dir2-dir2][S_COLOR(sg)] == 0 && 
		lnbn[(signed)sg-dir2-dir2-dir2] == 2)return(FALSE);
	        /* can atari when he jumps in (or wedges or hanes) */
	if (lnbf[(signed)sg-dir2-dir2-dir2][S_COLOR(sg)] == 0 && 
		lnbf[(signed)sg-dir2-dir2-dir2][1-S_COLOR(sg)] != 0 && /* 3/02 too far away */ 
		lnbf[(signed)sg-dir2-dir2][S_COLOR(sg)] == 0)return(FALSE);
	return(TRUE);
}
