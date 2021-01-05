/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* all the special corner rules */

# include "g2hd.h"
# include "g2rldef.h"
# include "g2getr.pr"

extern int rand(void);

extern int numcmoves[4];
//extern int foundjosfol[5];

/* corner pointed at */
  
static int point_corner[2][4] = {
		{ 1,0,3,2 },
		{ 2,3,0,1 }
			};
			
/* return color of first stone in corner */

static int cornercolor(int corner) {
	if (corner == -1)return(NOCOLOR);
	return(jcolor[corner]);
	}

/* color is the color to move */

int empty_corner(int color,int randomize,int handicap)
{
	int corner,numempties,firstcorner;
	sqr_t s;

	if (problemflag == 1) {
		return FALSE;
	}
	if (msptr == 0) {	
		if (boardsize <= 7) {
			s = (boardsize / 2) * boardsize + boardsize / 2;  /* center point for small boards */
			fire_strat_rule(s, CENTER_FOR_7_LINE, 1200, s, 0);
			return TRUE;
		} else if (boardsize == 9) {
			s = (boardsize / 2) * boardsize + boardsize / 2;  /* center point for small boards */
			fire_strat_rule(s, CENTER_FOR_7_LINE, 1200, s, 0);
			fire_strat_rule(s + 1, CENTER_FOR_7_LINE, 500, s, 0);
			fire_strat_rule(s - 1, CENTER_FOR_7_LINE, 500, s, 0);
			fire_strat_rule(s + boardsize, CENTER_FOR_7_LINE, 500, s, 0);
			fire_strat_rule(s - boardsize, CENTER_FOR_7_LINE, 500, s, 0);
			return TRUE;
		}
	}


	numempties = 0;
	for (corner = 0; corner < 4; ++corner) {
		if (emptycorner(corner)) {	/* found mt corner */
			++numempties;
		}
	}
	if (numempties == 0) {
		return FALSE;
	}

/* rules: play in an empty corner
 *	play on 3-3, 5-3, 5-4, 3-4, or 4-4 point only
 *	make 3-4 point plays spiral (don't face 3-4 points at each other)
 *	play in corner adjacent to 4-4 point stone if >1 empty corners
 *	play in corner opponent is facing on 3-4 point
 *	play 3-4 point facing empty corner
 * 	don't play on 3-4 point facing opponent
 *		unless there are 2 empty corners 
 *	don't face two 3-4 points at same empty corner
 */

	if (randomize) {
		firstcorner = rand()%4;
	} else {
		firstcorner = 1;
	}
   for (corner = 0; corner < 4; ++corner) {
	   if (numempties == 4 && corner != firstcorner) {
   			continue;
	   }
	   if (emptycorner(corner)) {	/* found mt corner */
		   threethreecorner(corner,numempties,color);
		   threefourcorner(corner,numempties, color);
		   if (boardsize <= 13) {
			   continue;
		   }
		   fourfourcorner(corner,numempties, color);
		   fivethreecorner(corner,numempties);
		   fivefourcorner(corner,numempties,handicap);
		}
	}
	return TRUE;
}

static void  fivethreecorner(int corner,int numempties) {
	sqr_t s;
	int dir, val;
	for (dir = 0; dir < 2; ++dir) {
		s = whatpoint(corner,5,3,dir);	/* move under consideration */
		val = 300;
		if (numempties == 4)val += 100;
		fire_strat_rule(s, PLAY_IN_EMPTY_CORNER, val, s, 0);
		}
	}

static void  fivefourcorner(int corner,int numempties,int handicap) {
	sqr_t s;
	int dir;
	for (dir = 0; dir < 2; ++dir) {
		s = whatpoint(corner,5,4,dir);	/* move under consideration */
		fire_strat_rule(s,PLAY_IN_EMPTY_CORNER,250, s, 0);
		}
	}
	
static void threethreecorner(int corner,int numempties,int color) {
	sqr_t s;
	int val = 300;
	s = whatpoint(corner,3,3,0);
	if (boardsize <= 13)val += 50;
	if (boardsize == 9)val += 75;
	if (numempties == 3 || numempties == 1)val += 25;
	fire_strat_rule(s,PLAY_IN_EMPTY_CORNER,val, s, 0);
	if (stonefacing(point_corner[0][corner]) == corner &&
	   cornercolor(point_corner[0][corner]) == 1-color) {
		fire_strat_rule(s,PLAY_IN_CORNER_POINT,50, s, 0);
		}
	if (stonefacing(point_corner[1][corner]) == corner &&
	   cornercolor(point_corner[1][corner]) == 1-color) {
		fire_strat_rule(s,PLAY_IN_CORNER_POINT,50,s,0);
		}
	}


static void  threefourcorner(int corner, int numempties, int color) {
	sqr_t s;
	int dir,val;
	val = 300;
	if (boardsize == 9 && color == 0)val = 1000;
	for (dir = 0; dir < 2; ++dir) {
		s = whatpoint(corner,3,4,dir);	/* move under consideration */
		fire_strat_rule(s,PLAY_IN_EMPTY_CORNER,val,s,0);

		if (numempties > 1 && color == BLACKCOLOR && spiral(corner,dir, color)) {   /* make 3-4 points spiral */
			fire_strat_rule(s,MAKE_34_POINTS_SPIRAL,140,s,0);
			}

		if (numempties > 1 && (stoneon44point(point_corner[0][corner]) ||
				      stoneon44point(point_corner[1][corner]))) { 
				/* corner adjacent to 4-4 point stone */
			fire_strat_rule(s,PLAY_NEXT_TO_44_POINT_STONE,50,s,0);
			}

		if (stonefacing(point_corner[0][corner]) == corner &&
		   cornercolor(point_corner[0][corner]) == 1-color) {
			fire_strat_rule(s,PLAY_IN_CORNER_POINT,100,s,0);
			}
		
		if (stonefacing(point_corner[1][corner]) == corner &&
		   cornercolor(point_corner[1][corner]) == 1-color) {
			fire_strat_rule(s,PLAY_IN_CORNER_POINT,100,s,0);
			}
		
		if (emptycorner(point_corner[dir][corner]) && boardsize > 13) {
			fire_strat_rule(s,POINT_AT_EMPTY,100,s,0);
			/* point 3-4 point at empty corner */
			}
		
		if (stoneon44point(point_corner[dir][corner]) && 
		   cornercolor(point_corner[dir][corner]) == color) {
			fire_strat_rule(s,POINT_AT_FRIENDLY_44,150,s,0);
			}
		
		if (cornercolor(point_corner[0][corner]) != 1-color && 
		   cornercolor(point_corner[1][corner]) != 1-color) {
			fire_strat_rule(s,PLAY_44_2_EMPTY,75,s,0);
			}
			
		if (cornercolor(point_corner[dir][corner]) == 1-color) { 
			/* 3-4 facing opponent */
			if (numempties != 2) {
				fire_strat_rule(s,DONT_POINT_AT_OPP,-100,s,0);
				}
			
			}
		}
	}


static void fourfourcorner(int corner, int numempties, int color) {
	sqr_t s;
	s = whatpoint(corner,4,4,0);
	fire_strat_rule(s, PLAY_IN_EMPTY_CORNER,250,s,0);
	if ((stoneon44point(point_corner[0][corner]) ||
			      stoneon44point(point_corner[1][corner]))) { 
		/* corner adjacent to 4-4 point stone */
		fire_strat_rule(s,PLAY_NEXT_TO_44_POINT_STONE,100,s,0);
		}
	if (stoneon44point(point_corner[0][corner]) && 
	   cornercolor(point_corner[0][corner]) == color ||
	   stoneon44point(point_corner[1][corner]) && 
	   cornercolor(point_corner[1][corner]) == color) {
		fire_strat_rule(s,NI_REN_SEI,50,s,0);
		}
	if (cornercolor(point_corner[0][corner]) != 1-color && 
	   cornercolor(point_corner[1][corner]) != 1-color) {
		fire_strat_rule(s,PLAY_44_2_EMPTY,125,s,0);
		}
	if (stonefacing(point_corner[0][corner]) == corner &&
	   cornercolor(point_corner[0][corner]) == 1-color) {
		fire_strat_rule(s,PLAY_IN_CORNER_POINT,75,s,0);
		}
	if (stonefacing(point_corner[1][corner]) == corner &&
	   cornercolor(point_corner[1][corner]) == 1-color) {
		fire_strat_rule(s,PLAY_IN_CORNER_POINT,75,s,0);
		}
	}




/* the following routines deal with corners of the board.  corners are
 * numbered :
 *               0   1
 *
 *               2   3
 */

/* return true if stone on 4-4 point in corner and no other stones */

static int stoneon44point(int corner) {
	return(numcmoves[corner] == 1 && board[whatpoint(corner,4,4,0)] != NOGROUP);
	}

/* return true if stone on 3-4 point in corner and no other stones */

static int stoneon34point(int corner) {
	return(numcmoves[corner] == 1 &&
		(board[whatpoint(corner,3,4,0)] != NOGROUP ||
		 board[whatpoint(corner,3,4,1)] != NOGROUP));
	}

/* return TRUE if stone on 5-4 or 5-3 point in corner and no other stones */

static int stoneonhighpoint(int corner) {
	return(numcmoves[corner] == 1 &&
		(board[whatpoint(corner,5,4,0)] != NOGROUP ||
		 board[whatpoint(corner,5,4,1)] != NOGROUP ||
		 board[whatpoint(corner,5,3,0)] != NOGROUP ||
		 board[whatpoint(corner,5,3,1)] != NOGROUP ));
	}




/* return corner number stone on 3-4 point is facing in corner */

static int stonefacing(int corner) {
	int sqr;
	if (!stoneon34point(corner))return(-1);
	sqr = whatpoint(corner,3,4,0);
	if (board[sqr] != NOGROUP)return(point_corner[0][corner]);
	else return(point_corner[1][corner]);
	}

int pincerstonefacing(int corner) {
	int sqr;
	sqr = whatpoint(corner,3,4,0);
	if (board[sqr] != NOGROUP)return(point_corner[0][corner]);
	else return(point_corner[1][corner]);
	}

/* return square of high approach to 3-4 point in corner */

static int highpoint(int corner) {
	int sqr;
	if (!stoneon34point(corner))return(-1);
	sqr = whatpoint(corner,3,4,0);
	if (board[sqr] != NOGROUP) {
		return(whatpoint(corner,5,4,0));
		}
	return(whatpoint(corner,5,4,1));
	}


/* return square of low approach to 3-4 point in corner */

static int lowpoint(int corner) {
	int sqr;
	if (!stoneon34point(corner))return(-1);
	sqr = whatpoint(corner,3,4,0);
	if (board[sqr] != NOGROUP) {
		return(whatpoint(corner,5,3,0));
		}
	return(whatpoint(corner,3,5,0));
	}

/* true if 3-4 in corner facing dir makes spiral */

static int spiral(int corner, int dir, int color) {
	int cn;

	cn = point_corner[dir][corner];
	if (stoneon34point(cn) && cornercolor(cn) == color && 
		stonefacing(cn) != corner)
		return(TRUE);

	cn = point_corner[1-dir][corner];
	if (cornercolor(cn) == color && stonefacing(cn) == corner)
		return(TRUE);

	return(FALSE);
	}

/* true if corner is empty */

static int emptycorner(int corner)
{
	return numcmoves[corner] == 0;
}

/* return the coordinate of the x,y point in corner with orientation dir */
	
static int whatpoint(int corner, int x, int y, int dir) {
	int tmp;
	if (dir) {
		tmp = x;
		x = y;
		y = tmp;
		}
	x--;
	y--;
    if (corner > 1)
         y = boardsize-1-y;
    if (corner == 1 || corner == 3)
         x = boardsize-1-x;
	return(y*boardsize+x);
	}
	

void try33invasion(int c)
{
	int corner;
	sqr_t s33, s44, s43, s34, s22;
	for (corner = 0; corner < 4; ++corner) {
		s33 = whatpoint(corner, 3, 3, 0);
		s44 = whatpoint(corner, 4, 4, 0);
		s43 = whatpoint(corner, 3, 4, 0);
		s34 = whatpoint(corner, 3, 4, 1);
		s22 = whatpoint(corner, 2, 2, 0);
		if ( S_COLOR(s44) == 1-c &&
			board[s33] == NOGROUP &&
			lnbn[s33] == 4 &&
			lnbn[s34] == 3 &&
			lnbn[s43] == 3 &&
			lnbn[s22] == 4) {
			fire_strat_rule(s33, TRY_33_INV, 0, NOSQUARE, 0);
		}
	}
}


void  shimari_kakari(int color) {
	int numshimari[3],num34points[3],corner,numhighpoints[3];
/* rules:
 *	don't let opponent make 2 shimari
 *	make shimari 
 *	make kakari
 */
	if (problemflag == 1)return;
	if (boardsize < 13)return;
	numshimari[0] = numshimari[1] = 0;
	num34points[0] = num34points[1] = 0;
	numhighpoints[0] = numhighpoints[1] = 0;
	for (corner = 0; corner < 4; corner++) {
		if (is_shimari(corner))
			numshimari[cornercolor(corner)]++; 
		if (stoneon34point(corner))num34points[cornercolor(corner)]++;
		if (stoneonhighpoint(corner))numhighpoints[cornercolor(corner)]++; 
		}


	if (numshimari[1-color] == 1 && (num34points[1-color] >= 1 || numhighpoints[1-color] >= 1))
		makekakari(TRUE, color);


	else if (num34points[color] >= 1) {
		makeshimari(color);
		if (num34points[1-color] >= 1 || numhighpoints[1-color] >= 1)
			makekakari(FALSE, color);
		return;
		}

	else if (num34points[1-color] >= 1 || numhighpoints[1-color] >= 1)
		makekakari(FALSE, color);
	}


/* true if corner has lone shimari in it */

static int is_shimari(int corner) {
	int c;
#ifdef DEMO
	return(FALSE);
#else
	if (numcmoves[corner] != 2)return(FALSE);
	c = S_COLOR(whatpoint(corner,3,4,0));
	if (c != NOCOLOR &&
		(S_COLOR(whatpoint(corner,5,3,0)) == c ||
		 S_COLOR(whatpoint(corner,5,4,0)) == c ||
		 S_COLOR(whatpoint(corner,6,3,0)) == c))
		 return(TRUE);
	c = S_COLOR(whatpoint(corner,3,4,1));
	if (c != NOCOLOR &&
		(S_COLOR(whatpoint(corner,5,3,1)) == c ||
		 S_COLOR(whatpoint(corner,5,4,1)) == c ||
 		 S_COLOR(whatpoint(corner,6,3,1)) == c))
		 return(TRUE);
	c = S_COLOR(whatpoint(corner,3,3,0));
	if (c != NOCOLOR &&
		(S_COLOR(whatpoint(corner,5,4,0)) == c ||
		 S_COLOR(whatpoint(corner,5,4,1)) == c))
		 return TRUE;
	return(FALSE);
#endif
	}

#define SKSIZE 31

#define KAKVAL (5*50)

/* make the best kakari. */

static void makekakari(int prevent2shimari, int color) {
	int corner,cn,count;
	sqr_t stmp;
	list_t ptr;

/* Joseki will show the possible kakari moves.
 * extensions will find low kakaris from friendly stones.
 * rules:
 *	prevent 2 shimaris
 *	if facing corner is 4-4 point use high kakari
 *	if facing corner has enemy shimari use high kakari
 *	else use low shimari
 *      prefer kakari from friendly corner
 */
	for (corner = 0; corner < 4; ++corner) {
		if (cornercolor(corner) == 1-color && stoneonhighpoint(corner)) {
			stmp = whatpoint(corner,3,4,0);
			count = 0;
			for (ptr = nblbp[stmp]; ptr != EOL; ptr = link[ptr])
				if (lnbn[list[ptr]] < 4)count++;
			if (count > 1 || lnbn[stmp] < 4)
				stmp = whatpoint(corner,3,4,1);
			fire_strat_rule(stmp,KAKARI,KAKVAL,0,0);
//			foundjosfol[corner] = TRUE;
			if (prevent2shimari) {
				fire_strat_rule(stmp,PREVENT_2_SHIMARI,800+600,0,0);
				}
			stmp = whatpoint(corner,3,3,0);
			fire_strat_rule(stmp,KAKARI,KAKVAL,0,0);
//			foundjosfol[corner] = TRUE;
			if (prevent2shimari) {
				fire_strat_rule(stmp,PREVENT_2_SHIMARI,800+600,0,0);
				}
			stmp = whatpoint(corner,4,4,0);
			if (lnbn[stmp] != 4)continue;  /* 5-4 opening */
			
			}
		else if (cornercolor(corner) == 1-color && stoneon34point(corner)) {
			/* found possible kakari */
			stmp = highpoint(corner);
			fire_strat_rule(stmp,KAKARI,KAKVAL,0,0);
//			foundjosfol[corner] = TRUE;
			if (prevent2shimari) {
				fire_strat_rule(stmp,PREVENT_2_SHIMARI,0,800+600,0);
				}
			cn = stonefacing(corner);
			if (stoneon44point(cn) || is_shimari(cn) && 
				cornercolor(cn) == 1-color) {
				/* high kakari */
				stmp = highpoint(corner);
				fire_strat_rule(stmp,SHIMARI_OR_44_POINT,100,0,0);
				}
			stmp = lowpoint(corner);
			fire_strat_rule(stmp,KAKARI,KAKVAL,0,0);
//			foundjosfol[corner] = TRUE;
			if (prevent2shimari) {
				fire_strat_rule(stmp,PREVENT_2_SHIMARI,800+600,0,0);
				}
			}
		}
	}

/* make the best shimari.  return TRUE if urgent */

static void makeshimari(int color) {
	int corner;

/* rules:
 *	best shimari is on 3-4 point facing enemy corner
 *	make small knights shimari
 */
	for (corner = 0; corner < 4; ++corner) {
		if (cornercolor(corner) == color && stoneon34point(corner)) {
//			foundjosfol[corner] = TRUE;
			}
		}
	}

