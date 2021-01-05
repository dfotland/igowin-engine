/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#ifdef TEST
# include <stdio.h>
#endif

# include "g2hd.h"
# include "g2rldef.h"
# include "g2getr.pr"
# include "g2fight.h"

#define SHIMARI_VAL 250

extern int numcmoves[4];
extern int numlifecalls,maxlifecalls[21];
extern int sumpots[41],maxsemdiff[21];
extern int dirnm[52],opdir[],msks[];
extern int sumeyes[41];

/* s is the edge of extension area.  longincr points into area.
 * shortincr points toward center.  s is on 3 line.  Ther is a stone on this column.
 * Return TRUE if
 * there is a shimari here
 */

static int edge_shimari(sqr_t s, int longincr, int shortincr) {
	if (board[s] == NOGROUP) {
		if (board[s+2*shortincr] != NOGROUP &&
			board[s-longincr] != NOGROUP && edge2[s] == 4)
			return TRUE;	/* shimari on 3-3 point */
		return(FALSE);
		}
	if (edge2[s] == 3 &&
		grcolor[lgr[s+2*shortincr]] == grcolor[board[s]])
		return TRUE;
	if (edge2[s] != 4)return(FALSE);
	if (grcolor[lgr[s+2*shortincr-longincr]] == grcolor[board[s]] &&
		board[s+shortincr] == NOGROUP &&
		board[s+2*shortincr] == NOGROUP &&
		board[s-shortincr] == NOGROUP)return(TRUE); /* knight shimari */
	if (grcolor[lgr[s+2*shortincr]] == grcolor[board[s]] &&
		board[s+shortincr] == NOGROUP &&
		board[s+2*shortincr-longincr] == NOGROUP &&
		board[s-shortincr] == NOGROUP)return(TRUE);  /*one point jump */
	return(FALSE);
	}


/* figure out the end points for an extension along an edge
 * startpoint is point on 3 line at end of edge
 * longincr is the amount to add to move along the 3 line
 * shortincr is the amount to add to get to the 4 line
 */

static void extedge(sqr_t startpoint, sqr_t longincr, sqr_t shortincr, int color) {
   sqr_t s1,s2,s3,s4,s5;  /* follow 1,2,3,4,5 line */
   int i,state;
   sqr_t startext=0; /* point on 1st line where stone is */
   sqr_t endext; /* point on 1st line where stone is */
   int shimari; /* number of shimaris facing along this edge */

   state = 0;
   shimari = 0;
   s3 = startpoint;		/* s3 follows 3 line */
   s4 = startpoint + shortincr; /* s4 follows 4 line */
   s2 = startpoint - shortincr; /* s2 follows 2 line */
   s1 = startpoint - 2*shortincr; /* s1 follows 1 line */
   s5 = startpoint + 2*shortincr; /* s5 follows 5 line */
   for (i = 0; i < boardsize-4; ++i) {
      if (state == 0) { /* looking for first stones */
         if (board[s1] != NOGROUP || board[s2] != NOGROUP || 
            board[s3] != NOGROUP || 
            board[s4] != NOGROUP || board[s5] != NOGROUP)
           state = 1;
         }
      else if (state == 1) { /* looking for empty area */
         if (board[s1] == NOGROUP && board[s2] == NOGROUP && 
            board[s3] == NOGROUP && 
            board[s4] == NOGROUP && board[s5] == NOGROUP) {
            state = 2;
			if (edge_shimari((sqr_t)(s3-longincr),longincr,shortincr))++shimari;
            startext = s1 - longincr;
            }
         }
      else if (state == 2) { /* looking for stones */
         if (board[s1] != NOGROUP || board[s2] != NOGROUP || 
            board[s3] != NOGROUP || 
            board[s4] != NOGROUP || board[s5] != NOGROUP) {
            state = 1;
	    if (edge_shimari(s3,-longincr,shortincr))++shimari;
            endext = s1;

            findbestextension(startext,endext,longincr,shortincr,shimari, color);
			shimari = 0;
            }
         }
      s1 += longincr;
      s2 += longincr;
      s3 += longincr;  
      s4 += longincr;  
      s5 += longincr;  
      }
   }

/* distance 5-6 is nice balance */
static int bmval[] = { 
	0, 0, 0, 75, 100, 175, 200, 175, 200, 200, 225, 225,
	200, 200, 175, 175, 175, 175, 150, 150, 150, 150, 150 
};  /* less so make shimari instead */

/* big jump at 9 since can extend both directions */
static int inval[] = { 
	0, 0, 0, 25, 75, 100, 125, 150, 200, 300, 325, 350, 350,
	350, 350, 350, 350, 350, 350, 350, 350, 350, 350 
};


/* return TRUE if urgent to extend group g */

static int urgextend(group_t g, sqr_t s) {
	if (G_ALIVE(g) <= ALIVE)return(FALSE);
	if (armybestpot[grarmy[g]] >= 8)
		return(FALSE);  /* prefer to make eyes */
	if (armysize[grarmy[g]] > 2)return(FALSE);
	if (grlibs[g] < 4)return(FALSE);
	if (jflag[which_corner(s)] == 1)return(FALSE);  /* let joseki handle it */
	if (grnbp[g] == EOL)return(FALSE); /* must be in contact with enemy */
	return(TRUE);
	}

/* make a two point extension from startext to strengthen a group */

static void extend_strengthen(sqr_t startext, sqr_t endext, sqr_t longincr, sqr_t shortincr, sqr_t spoint, sqr_t epoint, int starttwo, int dist) {
	sqr_t s3, s4;
	group_t startgr,endgr;
	int defval, urg, defs;

	endgr = board[epoint];
	startgr = board[spoint];

	s3 = startext+3*longincr+2*shortincr;
	if (grcolor[endgr] == grcolor[startgr])s3 += shortincr;
	if (dist == 3) {
		s3 -= longincr;
	}
	s4 = s3;
	if (dist >= 5 && starttwo) {
		s4 += longincr;
	}
	defval = grdefval[startgr];
	if (s4 == s3)
		defval += 150;
	if (groupinjosekicorner(startgr))
		defval /= 4;
	urg = urgextend(startgr,endext);
	if (urg)defval += 300;
	if (grcolor[endgr] != grcolor[startgr] && dist > 3)defval += 150;
	    /* since opponent can get 6 points in sente by extending */
	if (grlibs[startgr] < 4)defval = 0;
	if (G_ALIVE(startgr) > UNSETTLED) {
		fire_strat_rule(s3,EXTEND_WEAK_GROUP,defval,S_KEYPOINT(spoint),0);
		if (edge[s3] == 4)
			fire_strat_rule((sqr_t)(s3-shortincr),EXTEND_WEAK_GROUP,defval,S_KEYPOINT(spoint),0);
		if (s4 != s3) {
			fire_strat_rule(s4,EXTEND_WEAK_GROUP,defval+200,S_KEYPOINT(spoint),0);
			if (edge[s4] == 4)
				fire_strat_rule((sqr_t)(s4-shortincr),EXTEND_WEAK_GROUP,defval+200,S_KEYPOINT(spoint),0);
		}
	}
	else if (G_ALIVE(startgr) > ALIVE) {
		fire_strat_rule(s3,EXTEND_UNSETTLED_GROUP,
			defval,S_KEYPOINT(spoint),0);
		if (edge[s3] == 4)
			fire_strat_rule((sqr_t)(s3-shortincr),EXTEND_UNSETTLED_GROUP,
				defval,S_KEYPOINT(spoint),0);
		if (s4 != s3) {
			fire_strat_rule(s4,EXTEND_UNSETTLED_GROUP,
				defval+200,S_KEYPOINT(spoint),0);
			if (edge[s4] == 4)
				fire_strat_rule((sqr_t)(s4-shortincr),EXTEND_UNSETTLED_GROUP,
					defval+200,S_KEYPOINT(spoint),0);
		}
	}
	else if (G_ALIVE(startgr) >= MIAI) {
		if (armyeyespace[grarmy[startgr]] >= 8)
			defs = defval/2;
		else
			defs = defval;	/* make a base 6/03 */
		fire_strat_rule(s3,EXTEND_STRENGTHEN_GROUP,
			defs,S_KEYPOINT(spoint),0);
		if (edge[s3] == 4)
			fire_strat_rule((sqr_t)(s3-shortincr),EXTEND_STRENGTHEN_GROUP,
				defs,S_KEYPOINT(spoint),0);
		if (s4 != s3) {
			fire_strat_rule(s4,EXTEND_STRENGTHEN_GROUP,
				defs+100,S_KEYPOINT(spoint),0);
			if (edge[s4] == 4)
				fire_strat_rule((sqr_t)(s4-shortincr),EXTEND_STRENGTHEN_GROUP,
					defs+100,S_KEYPOINT(spoint),0);
		}
	}
}



/* figure out the best extension point between startext and endext
 * which are points on the first line where there are stones 
 * on the 1,2,3,4 or 5 line.
 */


static void findbestextension(sqr_t startext, sqr_t endext, sqr_t longincr, sqr_t shortincr, int shimari, int color) {
	sqr_t s, s3;
	int i,val,urg,urg2;
	int startclr,endclr; /* start and end stone colors */
	int dist;
	int starttwo,endtwo; /* is a wall on end */
	int startalive,endalive;
	group_t startgr=NOGROUP,endgr=NOGROUP;
	sqr_t spoint=0,epoint=0; /* highest point with stone on it */
	int sline=3,eline=3;  /* highest line with stone */
	int slow=3,elow=3;    /* lowest line with stone on it */
	list_t invadepoints = EOL;
	int cankakari;

	s = startext;
	startclr = 3;
	starttwo = FALSE;
	slow = elow = 5;
	for (i = 1; i < 6; ++i) {   /* look up and find the stone's color */
		if (board[s] != NOGROUP) {
			sline = i;
			spoint = s;
			if (slow == 5)slow = i;
			startgr = board[s];
			if (startclr == 3) {
				startclr = grcolor[startgr];
				if (G_ALIVE(startgr) >= WEAK_POTENTIAL)
					startclr = 1-startclr;
				}
 			else if (startclr != grcolor[startgr])startclr = NOCOLOR;
			else if (startclr != NOCOLOR)starttwo = TRUE;
			}
		s += shortincr;
		}

	s = endext;
	endclr = 3;
	endtwo = FALSE;
	for (i = 1; i < 6; ++i) {
		if (board[s] != NOGROUP) {
			epoint = s;
			eline = i;
			endgr = board[s];
			if (elow == 5)elow = i;
			if (endclr == 3) {
				endclr = grcolor[endgr];
				if (G_ALIVE(endgr) >= WEAK_POTENTIAL)
					endclr = 1-endclr;
				}
			else if (endclr != grcolor[endgr])endclr = NOCOLOR;
			else if (endclr != NOCOLOR)endtwo = TRUE;
			}
		s += shortincr;
		}


        /* 2 point extensions to stabilize group */

  	dist = (endext-startext)/longincr - 1;
	if (dist >= 3 && endclr == color && grarmy[startgr] != grarmy[endgr]) {
		extend_strengthen(endext, startext, (sqr_t)-longincr, shortincr, epoint, spoint, endtwo, dist);
	}
	if (dist >= 3 && startclr == color && grarmy[startgr] != grarmy[endgr]) {
		extend_strengthen(startext, endext, longincr, shortincr, spoint, epoint, starttwo, dist);
	}


	startalive = G_ALIVE(startgr);
	endalive = G_ALIVE(endgr);
	if (startclr == color && endclr == 1-color)
	   	ext_to_enemy(startext, endext, longincr, shortincr, spoint, epoint, sline, eline, shimari);
	else if (startclr == 1-color && endclr == color)
	   	ext_to_enemy(endext, startext, (sqr_t)-longincr, shortincr, epoint, spoint, eline, sline, shimari);

/******************************************
 * INVASIONS
 ******************************************/


	else if (startclr == 1-color && endclr == 1-color) {  /* invasion */
		urg = FALSE;	/* 4/99 no more urgent invasions */
#ifndef DEMO	

		cankakari = dist >= 7 &&
			(edge2[startext] == 3 && sline == 4 ||
			 edge2[endext] == 3 && eline == 4);
		if (dist >= 7 && !cankakari) {
		   /* can extend both ways and no kakari */
			s3 = startext + (1+dist/2)*longincr + 2*shortincr;
			if (dist%2 == 0 && endalive < startalive) {
				s3 -= longincr;
				}
			if (addlist(s3,&invadepoints)) {
				if (dist%2 == 0) {
					fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,0,NOSQUARE,0);
					}
				fire_strat_rule(s3,INVADE_WITH_ROOM,inval[dist] + 150, NOSQUARE,0);
				if (shimari)fire_strat_rule(s3,SHIM_INVADE,200,NOSQUARE,0);
				}
			}

		if (dist >= 4) {  /* 3 space invasions handled by patterns */
                        /* attacking invasions */
			val = gratkval[startgr] + 
				inval[dist] + 
				cut_val(G_ARMY(startgr),G_ARMY(endgr))*4/dist;
			urg2 = urg;
			if (urg || urg2)
				val += 300;
			s3 = startext+2*longincr+2*shortincr;
			if (rterv[s3][color] > 0)val += 50+rterv[s3][color]*2;

			if ((startalive > MIAI || armylibs[grarmy[startgr]] <= 4 && startalive == MIAI) &&  /* weak start group */ 
				sline < 5 && grcolor[startgr] != color) {
				if (addlist(s3,&invadepoints))
					fire_strat_rule(s3,INVADE_ATTACK,
						val,G_KEYPOINT(startgr),0);
				if (sline == 3 && dist >= 6) {
					if (addlist((sqr_t)(s3+longincr),&invadepoints))
						fire_strat_rule((sqr_t)(s3+longincr),INVADE_ATTACK,
							val,G_KEYPOINT(startgr),0);
					if (addlist((sqr_t)(s3+longincr+shortincr),&invadepoints))
						fire_strat_rule((sqr_t)(s3+longincr+shortincr),INVADE_ATTACK,
							val,G_KEYPOINT(startgr),0);
					}
				if (sline == 4 && dist >= 6) {
					if (addlist((sqr_t)(s3+longincr+shortincr),&invadepoints))
						fire_strat_rule((sqr_t)(s3+longincr+shortincr),INVADE_ATTACK,
							val-100,G_KEYPOINT(startgr),0);
					if (addlist((sqr_t)(s3+shortincr),&invadepoints))
						fire_strat_rule((sqr_t)(s3+shortincr),INVADE_ATTACK,
							val-100,G_KEYPOINT(startgr),0);
					}
				}
			else if (sline == 4 && grcolor[startgr] != color && 
				ld[startext+2*shortincr] != 0 &&
				ld[startext+2*shortincr] < 4 &&
				!rule_fired(s3, KAKARI) &&  /* don't double count with kakari */
				!rule_fired(s3, PATINVADE) &&
				addlist(s3,&invadepoints)) {
				if (eline < 4 && G_ALIVE(startgr) <= STRONG_MIAI && 
					G_ALIVE(endgr) <= STRONG_MIAI && dist < 6)
					val = 0;  /* dangerous invasion */
				fire_strat_rule(s3,INVADE_UNDER_4,val,G_KEYPOINT(startgr),0);
				}

			val = gratkval[endgr] + 
				inval[dist] + 
				cut_val(G_ARMY(startgr),G_ARMY(endgr))*4/dist;
			urg2 = urg;
			s3 = endext-2*longincr+2*shortincr;
			if (rterv[s3][color] > 0)val += 50+rterv[s3][color]*2;

			if ((endalive > MIAI || armylibs[grarmy[endgr]] <= 4 && endalive == MIAI) && 
				eline < 5 && grcolor[endgr] != color) {
				if (addlist(s3,&invadepoints))
					fire_strat_rule(s3,INVADE_ATTACK,
						val,G_KEYPOINT(endgr),0);
				if (eline == 3 && dist >= 6) {
					if (addlist((sqr_t)(s3-longincr),&invadepoints))
						fire_strat_rule((sqr_t)(s3-longincr),INVADE_ATTACK,
							val,G_KEYPOINT(endgr),0);
					if (addlist((sqr_t)(s3-longincr+shortincr),&invadepoints))
						fire_strat_rule((sqr_t)(s3-longincr+shortincr),INVADE_ATTACK,
							val,G_KEYPOINT(endgr),0);
					}
				if (eline == 4 && dist >= 6) {
					if (addlist((sqr_t)(s3-longincr+shortincr),&invadepoints))
						fire_strat_rule((sqr_t)(s3-longincr+shortincr),INVADE_ATTACK,
							val-100,G_KEYPOINT(endgr),0);
					if (addlist((sqr_t)(s3+shortincr),&invadepoints))
						fire_strat_rule((sqr_t)(s3+shortincr),INVADE_ATTACK,
							val-100,G_KEYPOINT(endgr),0);
					}
				}
			else if (eline == 4 && grcolor[endgr] != color &&
				ld[endext+2*shortincr] != 0 &&
				ld[endext+2*shortincr] < 4 && 
				!rule_fired(s3, KAKARI) &&
				!rule_fired(s3, PATINVADE) &&
				addlist(s3,&invadepoints)) {
				if (eline < 4 && G_ALIVE(startgr) <= STRONG_MIAI && 
					G_ALIVE(endgr) <= STRONG_MIAI && dist < 6)
					val = 0;  /* dangerous invasion */
				fire_strat_rule(s3,INVADE_UNDER_4,val,G_KEYPOINT(endgr),0);
				}
			}
#endif
		if (dist >= 4 && !cankakari) { /* high invasions at halfway point */
			s3 = startext + (1+dist/2)*longincr+3*shortincr;
	   		val = cut_val(G_ARMY(startgr),G_ARMY(endgr))/2 + inval[dist];
	   		if (G_ALIVE(endgr) > G_ALIVE(startgr)) {
	   			val += gratkval[endgr];
	   			urg2 = urg;
	   			}
	   		else {
	   			val += gratkval[startgr];
	   			urg2 = urg;
	   			}
			if (rterv[s3][color] > 0)val += 100;
			if (addlist(s3,&invadepoints))
				fire_strat_rule(s3,INVADE_HIGH,val,NOSQUARE,0);
			if (dist%2 == 0 && endalive < startalive) {
				s3 -= longincr;
				if (addlist(s3,&invadepoints))
					fire_strat_rule(s3,INVADE_HIGH,val,NOSQUARE,0);
				}
			if (dist%2 == 0) {
				fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,50,NOSQUARE,0);
				}
			}
		if (dist >= 4 && (armysize[grarmy[endgr]] == 1 && 
			G_ALIVE(endgr) > ALIVE && 
			grlibs[endgr] == 4 ||
			armysize[grarmy[startgr]] == 1 &&
			G_ALIVE(startgr) > ALIVE && 
			grlibs[startgr] == 4)) {  /* low invade behind single stone */
			s3 = startext + (1+dist/2)*longincr+2*shortincr;
			val = gratkval[startgr] + 
				gratkval[endgr] +
				cut_val(G_ARMY(startgr),G_ARMY(endgr)) + inval[dist];
			urg2 = urg;
			if (rterv[s3][color] <= 0)val = 0;
			if (dist%2 == 0 && endalive < startalive) {
				s3 -= longincr;
				}
			if (dist == 5 && endalive < startalive) {
				s3 -= longincr;
				fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,0,NOSQUARE,0);
				}
			if (dist == 5 && endalive > startalive) {
				s3 += longincr;
				fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,0,NOSQUARE,0);
				}
			if (dist%2 == 0) {
				fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,0,NOSQUARE,0);
				}
			if (addlist(s3,&invadepoints))
				fire_strat_rule(s3,INVADE_BEHIND_WEAK,val,G_KEYPOINT(startgr),0);
			}
			
		/* additional invasions when behind */	
		
		if (ahead < 2 && dist > 2) {
			s3 = startext + (1+dist/2)*longincr+2*shortincr;
			if (dist%2 == 0 && endalive < startalive) {
				s3 -= longincr;
				}
			val = 0;
			if (rterv[s3][color] > 0)val += inval[dist];
			if (rterv[s3][1-color] < MAXRTVAL && addlist(s3,&invadepoints))
				fire_strat_rule(s3,INVADE_BEHIND,val,NOSQUARE,0);
			if (rterv[s3+shortincr][1-color] < MAXRTVAL && addlist((sqr_t)(s3+shortincr),&invadepoints))
				fire_strat_rule((sqr_t)(s3+shortincr),INVADE_BEHIND,val,NOSQUARE,0);
			
			}
		else if (ahead < 2 && dist == 2 && (eline == 4 || sline == 4)) {
			s3 = startext + longincr + 2*shortincr;
			if (lnbf[s3][1-color] != 0)
				s3 = endext - longincr + 2 * shortincr;
			val = 0;
			if (rterv[s3][color] > 0)val = inval[dist];
			if (rterv[s3][1-color] < MAXRTVAL && lnbf[s3][1-color] == 0 && addlist(s3,&invadepoints))
				fire_strat_rule(s3,INVADE_BEHIND,val,NOSQUARE,0);
			}
        killist(&invadepoints);
		}

/***********************************************************
 * BIG MOVE BETWEEN FRIENDLY STONES
 ***********************************************************/
 
	else /*if (startclr == color && endclr == color) 10/97 could be NOCOLOR on ends */{
		/* big move on side between friendly stones */
		dist = (endext-startext)/longincr - 1;
		if (dist >= 5 && sline == 4 && edge[spoint] == 3 && grsize[startgr] == 1 &&
			grlibs[startgr] == 4) {
			if (lnbn[spoint+2*longincr] == 4 &&
					!rule_fired((sqr_t)(spoint+2*longincr-shortincr), MAKE_SHIMARI))  /* don't fire if already scored */
				fire_strat_rule((sqr_t)(spoint + 2*longincr - shortincr),MAKE_SHIMARI,SHIMARI_VAL,NOSQUARE,0);
			if (lnbn[spoint+2*longincr] == 4 && edge[epoint] == 3 &&
					!rule_fired((sqr_t)(spoint+2*longincr), MAKE_SHIMARI))  /* don't fire if already scored */
				fire_strat_rule((sqr_t)(spoint + 2*longincr),MAKE_SHIMARI,SHIMARI_VAL,NOSQUARE,0);
			}
		if (dist >= 5 && eline == 4 && edge[epoint] == 3 && grsize[endgr] == 1 &&
			grlibs[endgr] == 4 && lnbn[epoint-2*longincr] == 4) {
			if (!rule_fired((sqr_t)(epoint-2*longincr-shortincr), MAKE_SHIMARI))  /* don't fire if already scored */
				fire_strat_rule((sqr_t)(epoint - 2*longincr - shortincr),MAKE_SHIMARI,SHIMARI_VAL,NOSQUARE,0);
			if (edge[spoint] == 3 && !rule_fired((sqr_t)(epoint-2*longincr), MAKE_SHIMARI))  /* don't fire if already scored */
				fire_strat_rule((sqr_t)(epoint - 2*longincr),MAKE_SHIMARI,SHIMARI_VAL,NOSQUARE,0);
			}
		if (dist >= 3) {
			s3 = startext + (1+dist/2)*longincr + 2*shortincr;
			if (dist%2 == 0 && dist != 12) {
				if (endalive < startalive)
					s3 -= longincr;
				}
			val = bmval[dist];
			if (msptr > 100)
				val /= 2;
			if (msptr > 200)
				val /= 2;
			if (sline == 3 && eline == 3) {  /* 3 line both ends */
				s3 += shortincr;
				fire_strat_rule(s3,PLAY_HIGH_FROM_LOW,0,NOSQUARE,0);
				if (dist%2 == 0 && endalive < startalive && dist != 12) {
					fire_strat_rule(s3,PLAY_NEAR_WEAKER_GROUP,0,NOSQUARE,0);
					}
				fire_strat_rule(s3, BIG_MOVE, val, NOSQUARE, val);
				if (dist >= 5) {  /* allow extra play near weaker side */
					if (endalive < startalive)
						s3 -= longincr;
					else
						s3 += longincr;
					fire_strat_rule(s3, PLAY_HIGH_FROM_LOW, 0, NOSQUARE, 0);
					fire_strat_rule(s3, BIG_MOVE, val, NOSQUARE, val);
					}
				}
			else {
				if (endalive > ALIVE || startalive > ALIVE)
					fire_strat_rule(s3, BIG_MOVE, val, NOSQUARE, val);
				fire_strat_rule((sqr_t)(s3+shortincr), BIG_MOVE, val, NOSQUARE, val);
				if (edge[spoint] == 4 && edge[epoint] == 4 && dist == 11 && slow == 4 && elow == 4)
					fire_strat_rule((sqr_t)(s3+shortincr),MAKE_SAN_REN_SEI,0,NOSQUARE,0);
				if (slow == 4 && elow == 4 && dist == 12) {
					if (edge[spoint] == 3)s3 -= longincr;
					fire_strat_rule((sqr_t)(s3+shortincr),CHINESE_STYLE,0,NOSQUARE,0);
					fire_strat_rule(s3,CHINESE_STYLE,0,NOSQUARE,0);
					}
				}
			}
		}
	}

/*
 * does the move at ext prevent a double wing formation around the
 * stone at enemy, enemy is color.
 * if so, fire a rule to encourage this move
 */

 static void preventdoublewing(sqr_t ext, sqr_t enemy, int dist) {
	int x, y, color, val;
	sqr_t s;
	if (edge2[enemy] > 5)
		return;	/* not a corner stone */
	if (dist < 5)
		return;
	if (board[enemy] == NOGROUP)
		return;
	if (rule_fired(ext, JOSEKI_NO_TENUKI))
		return;	/* not if part of a joseki already */
	color = S_COLOR(enemy);
	x = xval[ext];
	y = yval[ext];
	if (xval[enemy] >= boardsize/2)
		x = boardsize-x-1;
	if (yval[enemy] >= boardsize/2)
		y = boardsize-y-1;
	if (x > y) {
		x = 2;
		y = 7;
		}
	else{
		y = 2;
		x = 7;
		}
	if (xval[enemy] >= boardsize/2)
		x = boardsize-x-1;
	if (yval[enemy] >= boardsize/2)
		y = boardsize-y-1;
	s = y*boardsize+x;
	val = 200;
	if (dist < 9)
		val = 150;
	if (board[s] != NOGROUP && S_COLOR(s) == color ||
		rterv[s][color] > MAXRTVAL/3 && rterv[s][1-color] < MAXRTVAL/3)
		fire_strat_rule(ext, PREVENT_2_WING, val, NOSQUARE,  0); 
	}

/* figure out extension to enemy.  start is our color, end is enemy 
 * startext and endext are points on first line and empty space
 * in between.  startext is friendly side.  spoint and epoint are the
 * highest point with a stone on them.  shimari is number of shimaris at
 * ends of open area.
 * if the end is a 3-4 point, and dist > 3, make a shimari
 */

static int toenemy[2][19] = { 
{0,0,0,75,125,175,225,250,250,275,250,250,225,
 200,200,200,200,200,200}, /* from low point */
{0,0,0,75,150,200,250,225,225,300,300,300,300,
 300,300,300,300,300,300 } };  /* from high point */


/* avoid extending near thickness, based on the territory value */

double thickfactor(sqr_t s, int color) {
	double res;
	int tval;
	tval = terv[s];	/* make it positive for end color */
	if (color == BLACKCOLOR)
		tval = -tval;
	tval -= 15;
	if (tval < 0)
		res = 1.0;
	else if (tval > 40-15)
		res = 0.0;
	else
		res = (40-15-tval)/(40.-15);
	return res;
}

static void  ext_to_enemy(sqr_t startext, sqr_t endext, sqr_t longincr, sqr_t shortincr, sqr_t spoint, sqr_t epoint, int sline, int eline, int shimari) {
	sqr_t s3, s4;
	group_t startgr,endgr;
	int dist,atkval,val,urg,highstart = sline >= 4;
	
	endgr = board[epoint];
	startgr = board[spoint];
	s3 = NOSQUARE;
	dist = (endext-startext)/longincr - 1;

	/* no room */
	if (dist < 3)return;

	/* prefer to make a shimari than a long extension */
	if (sline == 4 && edge[spoint] == 3 && grsize[startgr] == 1 &&
	        grlibs[startgr] == 4 && lnbn[spoint+2*longincr] == 4 &&
				!rule_fired((sqr_t)(spoint+2*longincr-shortincr), MAKE_SHIMARI)) {  /* don't fire if already scored */
        fire_strat_rule((sqr_t)(spoint + 2*longincr - shortincr),MAKE_SHIMARI,SHIMARI_VAL,NOSQUARE,0);
		fire_strat_rule((sqr_t)(spoint + 2*longincr - shortincr),STONE_FACING,6*50,NOSQUARE,  0);
		return;
		}

	/* prevent enemy from approaching weak point */
	if (sline == 4 &&
		board[spoint-shortincr] == NOGROUP && lnbf[spoint-shortincr][G_COLOR(startgr)] == 1 &&
		lnbf[spoint-shortincr][1-G_COLOR(startgr)] == 0 &&
		G_ALIVE(startgr) >= MIAI &&   /* 9/97 was RUN_OR_LIVE */
		G_ALIVE(startgr) <= UNSETTLED &&  /* 9/97 was RUN_OR_LIVE */ 
		dist >= 3 && G_ALIVE(endgr) <= RUN_OR_LIVE) {
		fire_strat_rule((sqr_t)(spoint + 2*longincr - shortincr),COVER_WEAKNESS,200,NOSQUARE,0);
		fire_strat_rule((sqr_t)(spoint + 2*longincr),COVER_WEAKNESS,175,NOSQUARE,0);
		if (dist >= 5) {
			fire_strat_rule((sqr_t)(spoint + 3*longincr - shortincr),COVER_WEAKNESS,250,NOSQUARE,300);
			fire_strat_rule((sqr_t)(spoint + 3*longincr),COVER_WEAKNESS,175,NOSQUARE,200);
			}
		}
	atkval = gratkval[board[epoint]];
	if (grsize[board[epoint]] == 1 && grlibs[board[epoint]] < 4)
		atkval = 0;  /* don't extend towards contact fight */
	if (G_ALIVE(startgr) <= RUN_OR_LIVE) {	/* extend from strength */
	   	if (eline == 4 && board[endext+2*shortincr] == NOGROUP &&
			lnbf[endext+2*shortincr][grcolor[startgr]] == 0) {  /* to 4th line stone */
	      	s3 = endext - 2*longincr + 2*shortincr;
			if (G_ALIVE(board[epoint]) > ALIVE) {
				val = toenemy[highstart][dist] + 100 + atkval;
				if (armysize[grarmy[endgr]] == 1)
					val += 50;
				fire_strat_rule(s3,EXTEND_TO_4POINT,val,
					S_KEYPOINT(endext+3*shortincr),toenemy[highstart][dist]);
				preventdoublewing(s3, epoint, dist); 
				if (armysize[grarmy[endgr]] == 1) {
					fire_strat_rule((sqr_t)(s3+shortincr),EXTEND_TO_ENEMY,
						toenemy[highstart][dist] + 100,(sqr_t)(endext+3*shortincr),toenemy[highstart][dist]);
					preventdoublewing((sqr_t)(s3+shortincr), epoint, dist); 
					}
				}
			else {	/* extending to strong group */
				val = atkval + toenemy[highstart][dist] + 100;
				if (armysize[grarmy[endgr]] == 1)
					val += 50;
				val = (int)(val * thickfactor(s3, grcolor[endgr]));
				fire_strat_rule(s3,EXTEND_TO_4POINT,val,S_KEYPOINT(endext+3*shortincr),toenemy[highstart][dist]);
				preventdoublewing(s3, epoint, dist); 
				if (grsize[endgr] == 1) {	/* high approach */
					val = atkval+toenemy[highstart][dist]-50;
					val = (int)(val * thickfactor(s3, grcolor[endgr]));
					fire_strat_rule((sqr_t)(s3+shortincr),EXTEND_TO_ENEMY,
						val,(sqr_t)(endext+3*shortincr),toenemy[highstart][dist]);
					preventdoublewing((sqr_t)(s3+shortincr), epoint, dist); 
					}
				if (dist >= 9) {
					fire_strat_rule((sqr_t)(s3-3*longincr),EXTEND_TO_ENEMY,atkval + toenemy[highstart][dist],endext+3*shortincr, toenemy[highstart][dist]);
					preventdoublewing((sqr_t)(s3-3*longincr), epoint, dist); 
					fire_strat_rule((sqr_t)(s3-4*longincr),EXTEND_TO_ENEMY,atkval + toenemy[highstart][dist],endext+3*shortincr, toenemy[highstart][dist]);
					preventdoublewing((sqr_t)(s3-4*longincr), epoint, dist); 
					}
				}
#ifdef NEVER
			9/97 have patterns for this one now.
			if ((edge2[s3] == 5 || edge2[s3] == 6) && edge[epoint] == 3)
				fire_strat_rule(s3,EXTENSION_IS_KAKARI,100,NOSQUARE,0);
#endif
	      	}
		else if (G_ALIVE(endgr) <= ALIVE) {  /* to strength, 3rd line stone */
			val = toenemy[highstart][dist];
			if (G_ALIVE(startgr) > MIAI || armysize[G_ARMY(startgr)] == 1)
				val += 100;
			urg = FALSE; /*dist >= 7; */
			if (grsize[endgr] == 1 && (grcnp[endgr] == EOL || dist <= 5) &&
				edge2[epoint] >= 4) {
				s3 = endext - 2*longincr + 2*shortincr;
				if (armysize[grarmy[endgr]] == 1 && edge2[epoint] >= 6)
					val += 150;   /* invasion behind is possible, so it's bigger */
				if (edge2[epoint] <= 5 && dist > 5)
					val /= 2;   /* don't extend so close to stone in corner */
				val = (int)(val * thickfactor(s3, grcolor[endgr]));
				fire_strat_rule(s3,EXTEND_TO_ENEMY,val,NOSQUARE, toenemy[highstart][dist]);
				preventdoublewing(s3, epoint, dist); 
				if (shimari == 1)
					fire_strat_rule(s3,SHIMARI_EXTEND,toenemy[highstart][dist]/3,NOSQUARE,toenemy[highstart][dist]/3);
				if (dist >= 5) {
					fire_strat_rule((sqr_t)(s3-longincr),EXTEND_TO_ENEMY,val,NOSQUARE, val);
					preventdoublewing((sqr_t)(s3-longincr), epoint, dist); 
					if (shimari == 1)  /* extra for extending in front of a shimari */
						fire_strat_rule((sqr_t)(s3-longincr),SHIMARI_EXTEND,toenemy[highstart][dist]/3,NOSQUARE,toenemy[highstart][dist]/3);
					}
				if (dist >= 7) {
					fire_strat_rule((sqr_t)(s3-longincr*(dist/2)+longincr),EXTEND_TO_ENEMY,toenemy[highstart][dist]*3/4,NOSQUARE,toenemy[highstart][dist]*3/4);
					preventdoublewing((sqr_t)(s3-longincr*(dist/2)+longincr), epoint, dist); 
					if (sline == 4) {
						fire_strat_rule((sqr_t)(s3-longincr*(dist/2)+longincr+shortincr),EXTEND_TO_ENEMY,toenemy[highstart][dist]*3/4,NOSQUARE,toenemy[highstart][dist]*3/4);
						preventdoublewing((sqr_t)(s3-longincr*(dist/2)+longincr+shortincr), epoint, dist); 
						}
					}
				if (dist >= 10) {
					fire_strat_rule((sqr_t)(s3-longincr*(dist/2)+longincr+longincr),EXTEND_TO_ENEMY,toenemy[highstart][dist]*3/4,NOSQUARE,toenemy[highstart][dist]*3/4);
					preventdoublewing((sqr_t)(s3-longincr*(dist/2)+longincr+longincr), epoint, dist); 
					}
				}
			else {
				s4 = NOSQUARE;
				if ((sline == 4 || G_ALIVE(startgr) >= MIAI) && dist > 5) {
					s4 = startext + 3*longincr + 2*shortincr;  /* just cover the weak point */
					fire_strat_rule(s4,EXTEND_TO_ENEMY,val,NOSQUARE,val);
					preventdoublewing(s4, epoint, dist); 
					}
		   		s3 = startext + (1+dist/2)*longincr + 2*shortincr;
		   		if (dist > 5 && G_ALIVE(startgr) > VERY_ALIVE)
		   			s3 -= longincr;  /* conservative if weak */
				if (s3 != s4) {
					fire_strat_rule(s3,EXTEND_TO_ENEMY,val,NOSQUARE,val);
					preventdoublewing(s3, epoint, dist); 
					if (shimari == 1)
						fire_strat_rule(s3,SHIMARI_EXTEND,toenemy[highstart][dist]/3,NOSQUARE,toenemy[highstart][dist]/3);
					if (sline == 4 && dist > 5) {
						fire_strat_rule((sqr_t)(s3+shortincr),EXTEND_TO_ENEMY,val,NOSQUARE,val);
						preventdoublewing((sqr_t)(s3+shortincr), epoint, dist); 
						if (shimari == 1)
							fire_strat_rule((sqr_t)(s3+shortincr),SHIMARI_EXTEND,toenemy[highstart][dist]/3,NOSQUARE,toenemy[highstart][dist]/3);
						}
					}
				}
			if (shimari == 2)
				fire_strat_rule(s3,TWO_SHIMARI_EXTEND,300+toenemy[highstart][dist]/2,NOSQUARE,toenemy[highstart][dist]/2);
	      	}
		else if (G_ALIVE(endgr) <= UNSETTLED) {
			s3 = endext-2*longincr+2*shortincr;
			if (dist > 6 && ld[s3+longincr] > 2 && ld[s3]+longincr != NOLD)
				s3 -= longincr;
			fire_strat_rule(s3,EXTEND_ATTACK_UNSETTLED,toenemy[highstart][dist] +
				atkval,G_KEYPOINT(endgr),toenemy[highstart][dist]);
			}
		else if (G_ALIVE(endgr) <= WEAK) {
			s3 = endext-2*longincr+2*shortincr;
			fire_strat_rule(s3,EXTEND_AND_ATTACK,toenemy[highstart][dist] +
				atkval,G_KEYPOINT(endgr),toenemy[highstart][dist]);
			}
		}
	else if (G_ALIVE(startgr) <= UNSETTLED && dist > 2) {	/* extend from weakness */
/*		defval = def_val(grarmy[startgr]); */
		if (dist >= 5) {
			s3 = startext + 3*longincr + 2*shortincr;
			}
		else {
			s3 = startext + 2*longincr + 2*shortincr;
			}
		if (shimari) {
			fire_strat_rule(s3,SHIMARI_EXTEND,toenemy[highstart][dist]/3,NOSQUARE,toenemy[highstart][dist]/3);
			}
		if (dist < 7 && G_ALIVE(endgr) > ALIVE) {
			if (G_ALIVE(endgr) <= UNSETTLED) {
				fire_strat_rule(s3,EXTEND_ATTACK_UNSETTLED,toenemy[highstart][dist] +
					atkval,G_KEYPOINT(endgr),toenemy[highstart][dist]);
				}
			else if (G_ALIVE(endgr) <= WEAK) {
				fire_strat_rule(s3,EXTEND_AND_ATTACK,toenemy[highstart][dist] +
					atkval,G_KEYPOINT(endgr),toenemy[highstart][dist]);
				}
			}
		}
	if (shimari && s3 != NOSQUARE && edge_shimari(startext,longincr,shortincr))
		fire_strat_rule(s3,EXTEND_FROM_SHIM+150,0,NOSQUARE,0);

	}


/* look for extensions and invasions on the 3 and 4 lines */

void extend(int color) {

   extedge((sqr_t)(boardsize*2+2),1,boardsize, color);
   extedge((sqr_t)(boardsize*3-3),boardsize,-1, color);
   extedge((sqr_t)(boardsize*2+2),boardsize,1, color);
   extedge((sqr_t)(boardsize*(boardsize-3)+2),1,(sqr_t)(-boardsize), color);
}


/* determine value for saving a threatened group (g) and find all
 * of the moves that save it.
 * Value of saving group is two points per stone in group.
 *   plus one point for every liberty after the first.
 * If group is cutting stones there is additional value.
 * If neighbors of group are unsettled there is additional value.
 * Possible saving moves are capturing neighboring groups with one liberty,
 * extending in own liberties, or capturing neighboring threatened group
 * include the group saving move from iscaptured also.
 * include the bestpot move also if make an eye
 */
 /* NOTE THAT IT IS NOW POSSIBLE THAT A THREATENED ARMY IS MORE THAN ONE GROUP */
 /* call once per threatened army only */

extern int obaval;	
static void save_th_army(army_t army, int color)
{
	list_t ptr, ptr2, tmplist = EOL, savemovelist = EOL;
	sqr_t s;
	int val, cutval;
	int iseye, weaknbr = FALSE, urg2;
	group_t g2, g = (group_t)list[armygroups[army]];
	
	val = grdefval[list[armygroups[army]]];
	if (armynbp[army] == EOL)
		getarmynbp(army);
	if (armysize[army] == 1 && armylibs[army] == 1 && ld[list[grlbp[g]]] == 8)
		val = 35;	/* ko connect 4/3 point (chinese rules - 4 points difference in score for 3 moves difference) */
                     /* eval will already give it 2 points - too much */
	cutval = cut_stones_val(army);	/* see if cutting stones */
	iseye = FALSE;
	iseye = eyepot[eyerec[mvs[grpieces[g]]]] != 0;

#if 0
	/* very old code - replace it with better getdefmoves */
	cpylist(armynbp[army],&tmplist);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		if (A_ALIVE(list[ptr]) > ALIVE) {
			weaknbr = TRUE;
		}
		if (A_THREATENED(list[ptr])) {
			for (ptr2 = armygroups[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
				t2 = th_cap_moves((group_t)list[ptr2]); /* capture neighbor */
				for (ptr3 = t2; ptr3 != EOL; ptr3 = link[ptr3])
					addlist(list[ptr3], &savemovelist);  /* since t2 is not sorted */
				killist(&t2);
			}
		}
		else if (armylibs[list[ptr]] <= armylibs[army]) {
			cpylist(armylbp[list[ptr]], &tmp2list);
			for (ptr2 = tmp2list; ptr2 != EOL; ptr2 = link[ptr2]) {
				s = list[ptr2];
				if (!canbecaptured(s, color, mvs[grpieces[g]], 1-color, NOCOLOR, 80, cancapsize[playlevel], taclibs[playlevel], NOGROUP)) {
					addlist(s,&savemovelist);
				}
			}
			killist(&tmp2list);
		}
	}
	killist(&tmplist);


	cpylist(armylbp[army], &tmplist);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (!canbecaptured(s, color, mvs[grpieces[g]], 1 - color, NOCOLOR, 80, cancapsize[playlevel], taclibs[playlevel],NOGROUP)) {
			addlist(s, &savemovelist);
		}
		cpylist(nblbp[s], &tmp2list);
		for (ptr2 = tmp2list; ptr2 != EOL; ptr2 = link[ptr2]) {
			sn = list[ptr2];
			if (!canbecaptured(sn, color, mvs[grpieces[g]], 1 - color, NOCOLOR, 80, cancapsize[playlevel], taclibs[playlevel], NOGROUP))
				addlist(sn, &savemovelist);
		}
		killist(&tmp2list);
		if (lnbn[s] < 2 && lnbf[s][1-color] == 0) {
			i = fdir[s];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				if (ld[s+nbr[i]] == 0 && board[s+nbr[i]] != g) {
					cpylist(grlbp[board[s+nbr[i]]], &tmp2list);
					for (ptr2 = tmp2list; ptr2 != EOL; ptr2 = link[ptr2]) {
						sn = list[ptr2];
						if (!canbecaptured(sn, color, mvs[grpieces[g]], 1 - color, NOCOLOR, 80, cancapsize[playlevel], taclibs[playlevel], NOGROUP))
							addlist(sn, &savemovelist);
					}
					killist(&tmp2list);
				}
			}
		}
	}
#endif

	/* special case for double atari at a ladder */
	if (armysize[army] == 1 && grlibs[g] == 2 && grthreatened[g] == 1) {
		for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
			s = list[ptr];
			fire_strat_rule(s, SAVE_TH_GROUP, val, G_KEYPOINT(g), 0);
			if (lnbf[s][1 - grcolor[g]] == 0) { 
				for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
					s = list[ptr2];
					if (inlist(s, &grlbp[g]))
						continue;
					fire_strat_rule(s, SAVE_TH_GROUP, val, G_KEYPOINT(g), 0);
				}
			}
		}
		for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
			if (grlibs[list[ptr]] == 1) {
				s = list[grlbp[list[ptr]]];
				if (!inlist(s, &grlbp[g])) {
					fire_strat_rule(s, SAVE_TH_GROUP, val, G_KEYPOINT(g), 0);
				}
			}
			if (grthreatened[list[ptr]] == 2) {
				s = grcapmove[list[ptr]];
				if (s != NOSQUARE) {
					fire_strat_rule(s, SAVE_TH_GROUP, val, G_KEYPOINT(g), 0);
				}
			}
		}
		return;
	}

	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		if (A_ALIVE(list[ptr]) > ALIVE) {
			weaknbr = TRUE;
		}
	}
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g2 = list[ptr];
		tmplist = getdefmoves(g2);
		for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2]) {
			s = list[ptr2];
			if (board[s] != NOGROUP || s == grsavemove[g2] || s == PASS || s == NOSQUARE)
				continue;
			if (!canbecaptured(s, grcolor[g2], mvs[grpieces[g2]], 1 - grcolor[g2], NOCOLOR, 80, conncapsize[playlevel], conntaclibs[playlevel], NOGROUP)) {
				addlist(s, &savemovelist);
			}
		}
		killist(&tmplist);
		if (grsavemove[g2] != NOSQUARE && grsavemove[g2] != PASS)
			addlist(grsavemove[g2], &savemovelist);
	}
	if (armybestpot[army] >= 8 && armybestmove[army] != NOSQUARE)
		addlist(armybestmove[army], &savemovelist);
	if (armybestpot2[army] >= 8 && armybestmove2[army] != NOSQUARE)
		addlist(armybestmove2[army], &savemovelist);

	for (ptr = savemovelist; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		fire_strat_rule(s, SAVE_TH_GROUP, val, G_KEYPOINT(g), 0);
		urg2 = FALSE;
		if (cutval != 0) {
			if (cutval > obaval * 3)
				urg2 = TRUE;
			fire_strat_rule(s, SAVE_CUTTING_STONES, cutval + urg2 * 100, G_KEYPOINT(g), 0);
		}
		if (iseye && weaknbr)
			fire_strat_rule(s, SAVE_GROUP_ATTACK, 50, NOSQUARE, 0);
	}
	killist(&tmplist);
	killist(&savemovelist);
}	

void savegroup(int passval, int color, int ply)
{
	group_t g;
	list_t armylist = EOL, ptr;
	int result;
	army_t army;
	tree_t movetree;
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])continue;
		if (grcolor[g] != color)continue;
		if (G_ALIVE(g) > WEAK)continue;
		if (G_ALIVE(g) == VERY_ALIVE)continue;
		if (groupinjosekicorner(g))continue;  /* let joseki library handle it */
		addlist(grarmy[g], &armylist);
	}
	for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
		army = (army_t)list[ptr];
		result = readliferesult((group_t)list[armygroups[army]], TRUE, &movetree);
		if (result > V_UNKNOWNRES) {
			fireliverules((group_t)list[armygroups[army]], movetree);
		} else if (A_ALIVE(army) > ALIVE && A_ALIVE(army) <= UNSETTLED) {
			save_unsettled_army(army, color, passval);
		} else if (A_ALIVE(army) > UNSETTLED) {
			save_weak_army(army, passval, color);  /* do weak groups last in case not enough time to read */
		}
		if (problemflag != SOLVEPROBLEM && A_ALIVE(army) > VERY_ALIVE &&  /* 1/99 aded problemflag here */
			A_ALIVE(army) <= ALIVE) {
			save_safe(army, color, passval, ply);
		}
	}
	killist(&armylist);
}

extern int apflag;
/* save alive but bad aji groups. extra when opponent attacks them */

static void save_safe(army_t army, int color, int passval, int ply)
{
	list_t slist, ptr, ptr2, ptr3, tmplist2 = EOL;
	int def = 0,bpotv = 0, val;
	army_t opparmy;
	
	slist = EOL;  /* allow moves that save two groups at once */
	def = grdefval[list[armygroups[army]]];
	opparmy = NOARMY;
	if (msptr > 0 && mvs[msptr-1] != PASS)
		opparmy = grarmy[board[mvs[msptr-1]]];

	if (armynbp[army] == EOL)
		getarmynbp(army);
	if (inlist(opparmy, &armynbp[army]))
		def += 100;  /* bonus for responding*/
	if (passval < -20)
		return;  /* 3/20 no play safe when behind */
	if (passval < 30 * 50)  /* more conservative if less than 30 points ahead */
    	def = (int)(def * ((float)passval + 50 * 50) / (100 * 50));

	/* find best potential eye */
	for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
		if (pots[list[ptr]].pot_type == UNDERCUT ||
			pots[list[ptr]].pot_type == EXTEND) {
			val = sumeyes[pots[list[ptr]].pot_val];
		} else { 
			val = (pots[list[ptr]].pot_val + pots[list[ptr]].pot_max) / 2;
		}
		if (val > bpotv)
			bpotv = val;
	}
	if (bpotv < 4)
		bpotv = 4;

	for (ptr2 = armypot[army]; ptr2 != EOL; ptr2 = link[ptr2]) {
	/*	if (pots[list[ptr2]].pot_val == 0)continue; */

		tmplist2 = adpot(army, list[ptr2]);
		for (ptr3 = tmplist2; ptr3 != EOL; ptr3 = link[ptr3]) {
			if ((sqr_t)list[ptr2] == kosquare)
				continue;  /* need to make ko threat */
			if (pots[list[ptr2]].pot_type == UNDERCUT ||
				pots[list[ptr2]].pot_type == EXTEND) {
				val = sumeyes[pots[list[ptr2]].pot_val];
			} else { 
				val = (pots[list[ptr2]].pot_val + pots[list[ptr2]].pot_max) / 2;
			}
			if (val < 4)
				val = 4;
			addlist(list[ptr3], &slist);
			/* 3/02 value depends on amount of eye space gained */
			fire_strat_rule(list[ptr3], SAFE_STRENGTHEN_GROUP, def * val / bpotv, A_KEYPOINT(army), 0);
		}
		killist(&tmplist2);
	}
	
	make_eye_shape(army, def, FALSE, &slist, color);
	runaway(army, &slist, passval, def);

	killist(&slist);
}


/* find moves that make eye shape for army */
/* return TRUE if found one.  Note is called from two places */

static int make_eye_shape(army_t army, int val, int urg, list_t *slist, int color) {
	list_t ptr,ptr2,ptr3;
	sqr_t s,sn;
	int numpot,flag = FALSE;
	list_t eslist = EOL;

	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (lnbn[s] != 2 || ld[s] < 4 || ld[s] > 5)continue;
		for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
			sn = list[ptr2];
		
			numpot = 0;
			for (ptr3 = nblbp[sn]; ptr3 != EOL; ptr3 = link[ptr3]) {
				if (lnbn[list[ptr3]] == 2 && ld[list[ptr3]] >= 2 &&
				   ld[list[ptr3]] <= 5 &&
				   (sqr_t)list[ptr3] != s &&
				   grcolor[lgr[list[ptr3]]] == color)
					numpot += ld[list[ptr3]];
				else if (edge[sn] == 2 && edge[list[ptr3]] == 1 &&
					lnbf[list[ptr3]][1-color] == 0)
					numpot += 5;
				}
			if ((lnbf[sn][1-color] == 0 || numpot > 0) &&
			   addlist(sn,&eslist)) {
				if (addlist(sn,slist))
					fire_strat_rule(sn,MAKE_EYE_SHAPE,val+10*numpot+urg*300,A_KEYPOINT(army),0);
				else
					fire_strat_rule(sn,MAKE_EYE_SHAPE,10*numpot+urg*300,A_KEYPOINT(army),0);
					
				flag = TRUE;
				}
			}
		}
	killist(&eslist);
	return(flag);
	}



/* find moves which might save an unsettled group
 * any liberty of group
 * any liberty of liberty
 * any liberty of neighboring group
 * make a partial eye
 * defend territory
 * connect to another group is handled by cut_connect
 * any vital point
 * if not allowurg, don't make move urgent, since can't read how to save it
 */


static void save_unsettled_army(army_t army, int color, int passval) {
	sqr_t s,sg;
	list_t ptr,ptr2,tmplist = EOL, slist = EOL;
	int defval,val,c;
	int defpot,best,extra,bestpot;
	                                 

	if (A_THREATENED(army) && 
		grsavemove[list[armygroups[army]]] != NOSQUARE) {
		save_th_army(army,color);	/* both save threatened and get eyes here */
		if (armyeyepotential[army] == 0) 
			return;
		}

	bestpot = armybestpot[army];
	if (bestpot < 2)bestpot = 2;
	sg = A_KEYPOINT(army);
	c = grcolor[list[armygroups[army]]];
	defval = grdefval[list[armygroups[army]]];
	if (A_THREATENED(army))defval = 0; /* was no-or ko save value too low. don't double count since already added to save group */
	defpot = defval/2 + 100;
	if (defpot > 300)defpot = 300;
     
	val = armyeyespace[army] + armybestpot[army];
	if (val > 16)val = 16;  /* value by how close come to living */
	val = ((defval/4) * val) / 4;
	if (armybestmove[army] != NOSQUARE && addlist(armybestmove[army],&slist))
		fire_strat_rule(armybestmove[army],EYEPOT_SAVE_UNSETTLED,val,sg,50);
	val = armyeyespace[army] + armybestpot2[army];
	if (val > 16)
		val = 16;
	val = ((defval/4) * val) / 4;
	if (armybestmove2[army] != NOSQUARE && addlist(armybestmove2[army],&slist))
		fire_strat_rule(armybestmove2[army],EYEPOT_SAVE_UNSETTLED,val,sg,25);
	val = armyeyespace[army] + armysecond[army];
	if (val > 16)
		val = 16;
	val = ((defval/4) * val) / 4;
	if (armysecondmove[army] != NOSQUARE && addlist(armysecondmove[army],&slist))
		fire_strat_rule(armysecondmove[army],EYEPOT_SAVE_UNSETTLED,val,sg,15);

	for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
		tmplist = adpot(army,list[ptr]);
		for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2]) {
			if ((sqr_t)list[ptr2] == kosquare)continue;  /* need ko threat */
			if (addlist(list[ptr2],&slist)) {
				val = pots[list[ptr]].pot_val;
				if (pots[list[ptr]].pot_type == EXTEND ||
					pots[list[ptr]].pot_type == UNDERCUT)
					val = sumeyes[val] + 1;
				extra = val * 5;
    			val = armyeyespace[army] + val;
    			if (val > 16)val = 16;
    			val = ((defval/4) * val) / 4;
				fire_strat_rule(list[ptr2],EYEPOT_SAVE_UNSETTLED,val,sg,
					extra);
				}
			}
		killist(&tmplist);
		}


	cpylist(armylbp[army],&tmplist);
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (lnbf[s][1-c] == 0)
			mrglist(nblbp[s],&tmplist);
		}
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (lnbf[s][1-c] == 1 && lnbn[s] >= 1 && 
		   !G_THREATENED(lgr[s])) {
		   	for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2])
		   		if (lnbf[list[ptr2]][1-c] == 0 && 
		   			S_COLOR(s+s-list[ptr2]) == 1-c &&
		   			addlist(s,&slist)) {
					fire_strat_rule(s,DEFEND_EYE_SPACE,defpot,sg,0);
					break;
					}
			}
		}
	killist(&tmplist);


	save_capture_nbr(army,&slist,defval);
	make_eye_shape(army,0,FALSE,&slist, color);
	best = armybestpot[army];  /*(army,&tmp,&tmp2,&tmp3,&tmp4,&tmp5,&tmp6,&tmp7,&tmp8,&tmp9,&tmp10); */
	if (best + armyeyespace[army] < 16)
		runaway(army,&slist,passval,defval);

	killist(&slist);
	}


static int rnval[NUMRUN] = { 150,100,100,100,50,50,50,50,50,50,50,0,0,0 };


/* fire rules for running away with this army */

void runaway( army_t army,list_t *rlist, int passval, int defval) {
	list_t ptr;
	int val, i, erun;
	sqr_t s;

	erun = easyrun[A_COLOR(army)];
	
	if (armyrn_pot[army] >= 16)return;
	if (armylibs[army] == 2)return;
	if (armyrn_pot[army] >= erun)
		val = 0;
	else if (A_THREATENED(army))val = 0;
	else {
		val = defval;
/*		if (val > BIGNUM/erun)val = BIGNUM/erun;  to prevent overflow */
/*		val = (val * (erun - armyrn_pot[army]))/erun; in def_val now */
		if (A_ALIVE(army) == RUNNING_FIGHT) {
			if (armynbp[army] == EOL)
				getarmynbp(army);
			for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr])
				if (A_ALIVE(list[ptr]) == RUNNING_FIGHT) {
					val += gratkval[list[ptr]];  
						/* since attack as well as defend prefer running to living */
					}
			}
		}
	if (armysize[army] == 1 && armylibs[army] < 4)
		val = 0;  /* single stone in contact fight */
	for (i = 0; i < NUMRUN; ++i)
		for (ptr = armyrun[army][i]; ptr != EOL; ptr = link[ptr]) {
			s = list[ptr];
			runhere(army,val+rnval[i],s,rlist,i);
			}
	killist(rlist);
	}



/* this must be rewritten to call runpoints!!! */
	
static void runhere(army_t army, int val, sqr_t s, list_t *rlist, int type) {
	list_t ptr,ptr2;
	sqr_t sn,ept=0;
	int i,ldtmp,eyflag,numopen,c,run;
	int keypoint = A_KEYPOINT(army);
	
	if (type == RUNEXTENDFRIEND || type == RUNEXTENDENEMY)
		return;	/* 7/02 can extend here, so shouldn't generate running moves */
	run = armyrn_pot[army]; /* since is type RUN, param must be run_pot, not keypoinT */
	if (type == NEUTRALRUN && S_NEUTRAL(s) || type > NEUTRALRUN && type != JUMPENEMY) {
		if (addlist(s,rlist)) {
			fire_strat_rule(s,RUN_AWAY,val,keypoint,0);
			}
		return;
		}
	c = grcolor[list[armygroups[army]]];
	if (lnbn[s] == 3) {
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
			if (lnbn[list[ptr]] == 4 && 
		   	   (edge[list[ptr]] > 3 || edge[list[ptr]] > edge[s])) {
				eyflag = FALSE;
				numopen = 0;
				for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (lnbn[list[ptr2]] == 4)numopen++;
					if (ld[list[ptr2]] >= 4 && ld[list[ptr2]] < 9 &&
				   		lnbf[list[ptr2]][c] != 0) {
						eyflag++;
						ept = list[ptr2];
				   		}
					}
				if (eyflag == 1 && edge[list[ptr]] > 1) {
					sn = list[ptr] + list[ptr] - ept;
					if (lnbn[sn] == 4 && addlist(sn,rlist)) {  /* dog face or knight jump */
					   if (rterv[sn][c] > rterv[s][c])
						   fire_strat_rule(sn,TOWARD_STRENGTH,0,NOSQUARE,0);
					   fire_strat_rule(sn,RUN_AWAY,val+100,keypoint,0);
						}
					}
				if (numopen > 0 && !eyflag && addlist(list[ptr],rlist)) {
					if (rterv[list[ptr]][c] > rterv[s][c])
					   fire_strat_rule(list[ptr],TOWARD_STRENGTH,0,NOSQUARE,0);
					if (board[s+s-list[ptr]] != NOGROUP)  /* one point jump is better */
						fire_strat_rule(list[ptr],RUN_AWAY,val+50,keypoint,0);
					else
						fire_strat_rule(list[ptr],RUN_AWAY,val,keypoint,0);
					}
			   if (board[s+s-list[ptr]] != NOGROUP && numopen == 3 &&
			      armysize[army] < 3)  /* one pt jump spot */
				   for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					   if (lnbn[list[ptr2]] != 4)continue;
					   if ((sqr_t)list[ptr2] == s)continue;
					   if (!addlist(list[ptr2],rlist))continue;
					   sn = list[ptr2] + 2*s - 2*list[ptr];
					   if (board[sn] != NOGROUP ||
					   		ld[sn] == 2 || S_NEUTRAL(sn))continue;
					   if (rterv[list[ptr2]][c] > rterv[s][c])
						   fire_strat_rule(list[ptr2],TOWARD_STRENGTH,0,NOSQUARE,0);
					   fire_strat_rule(list[ptr2],RUN_AWAY,val+50,keypoint,0);
					   }
			   }
			else if (S_COLOR(s+s-list[ptr]) == c && lnbn[list[ptr]] == 3 &&
				lnbf[list[ptr]][c] == 1 && (edge[list[ptr]] > 3 || edge[list[ptr]] > edge[s])) {
					for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (lnbn[list[ptr2]] == 4 && addlist(list[ptr2],rlist))
						   fire_strat_rule(list[ptr2],RUN_AWAY,val,keypoint,0);
						}
					}
		   }
	   }
	if (ld[s] >= 4) {
		i = fdir[s];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = s + nbr[i];
			if (grarmy[board[sn]] == army && lnbn[s-nbr[i]] == 4 &&
			   lnbn[s-nbr[i]-nbr[i]] == 4 && addlist((sqr_t)(s-nbr[i]-nbr[i]),rlist))
				   fire_strat_rule((sqr_t)(s-nbr[i]-nbr[i]),RUN_AWAY,val+150,keypoint,0);
			}
		}
	}

/* find moves to look at which might save a weak group
 */

static void save_weak_army(army_t army, int passval, int color)
{
	list_t ptr, tmplist = EOL, t2list = EOL;
	group_t g = NOGROUP;
	int def, size = 0;

#ifdef CHECK
	if (armygroups[army] == EOL) {
		outerror("Bad army in save_weak_army");
		turnoffcplay();
		return;
	}
#endif

	if (playlevel > 2 && armyrn_pot[army] <= 1 && numlifecalls <= maxlifecalls[playlevel]/4) {
		for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr])
			if ((int)grsize[list[ptr]] > size) {
				g = (group_t)list[ptr];
				size = grsize[g];
			}
		if (trytolive(g))  /* changes army! */
			return;
		army = grarmy[g];  /* new army value */
	}
	
	if (A_THREATENED(army) && 
		grsavemove[list[armygroups[army]]] != NOSQUARE) {
		save_th_army(army,color);
		if (armyeyepotential[army] == 0)
			return;
	}

	def = grdefval[list[armygroups[army]]];

	if (A_THREATENED(army))
		def = 0;

	runaway(army,&t2list,passval,def);	/* try to run away */

/*	make_eye_shape(army,0,FALSE,&t2list, color); */

	tmplist = lifesetmoves(armygroups[army]);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		if (MV_MV(list[ptr]) != PASS)
			fire_strat_rule(MV_MV(list[ptr]), TRY_SAVE_WEAK, def*(50-MV_PROB(list[ptr]))/50, 
				A_KEYPOINT(army), 4*(50-MV_PROB(list[ptr]))); 
	}
	killist(&tmplist);

	killist(&t2list);
}

/* return the numbe rof moves generated */
int remove_dead_stones(int color)
{
	list_t ptr, gptr, lptr;
	group_t g;
	int count = 0;
        
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g] || grcolor[g] == color || G_ALIVE(g) <= UNSETTLED_DEAD)
			continue;
		for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
			/* liberty */
			fire_strat_rule(list[ptr], REMOVE_DEAD, 0, NOSQUARE, 0);
			count++;
			if (grlibs[g] > 1 && lnbn[list[ptr]] == 1 && lnbf[list[ptr]][1-grcolor[g]] == 0) {
				/* approach safe liberty */
				fire_strat_rule(list[nblbp[list[ptr]]], REMOVE_DEAD, 0, NOSQUARE, 0);
				count++;
			}
			if (grlibs[g] > 1 && lnbn[list[ptr]] == 0) {
				for (gptr = nbgrp[list[ptr]][1-grcolor[g]]; gptr != EOL; gptr = link[gptr]) {
					if (grlibs[list[gptr]] > 2) continue;
					for (lptr = grlbp[list[gptr]]; lptr != EOL; lptr = link[lptr]) {
						if (list[lptr] == list[ptr]) continue;
						fire_strat_rule(list[lptr], REMOVE_DEAD, 0, NOSQUARE, 0);
						count++;
					}
				}
			}
		}
	}
	return count;
}


/* attack weak enemy groups and kill threatened enemy groups
 * attacking by saving a nbr group is handled elsewhere
 */


void killgroup(int passval, int color) {
	group_t g;
	list_t ptr, ptr2;
	int alive;
	sqr_t s;
	army_t army;
	list_t armylist = EOL, ataris = EOL;
	int minsize;
	int result;
	tree_t movetree;

	passval = passval;
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])
			continue;
		if (grcolor[g] == color)
			continue;
		if (G_ALIVE(g) == DEAD)
			continue;
		if (G_THREATENED(g) == 2) {
			addlist(grarmy[g],&armylist);
			continue;
		}
		if (grlibs[g] == 2 && G_ALIVE(g) <= WEAK) {
			for (ptr2 = grlbp[g]; ptr2 != EOL; ptr2 = link[ptr2]) {
				s = list[ptr2];
				if (G_ALIVE(lgr[s]) == DEAD && grcolor[lgr[s]] == color)
					continue; /* don't atari from dead group */
				if (G_ALIVE(g) == SEKI && S_NEUTRAL(s) &&
					grlibs[list[nbgrp[s][color]]] == 2)
					continue;	/* don't atari in seki */
				if (lnbn[s] > 1 || S_NEUTRAL(s)) {
					if (addlist(s, &ataris)) {
						if (G_ALIVE(g) <= ALIVE && !S_NEUTRAL(s) || gralive[g] == SEKI)
							fire_strat_rule(s, TRY_ATARI, -60, NOSQUARE, 0);
						else
							fire_strat_rule(s, TRY_ATARI, -10, NOSQUARE, 0);
					}
					else if (lnbn[s] || lnbf[s][color] && grlibs[list[nbgrp[s][color]]] > 2) {
						minsize = 1000;
						for (ptr = nbgrp[s][grcolor[g]]; ptr != EOL; ptr = link[ptr])
							if (grsize[list[ptr]] + grlibs[list[ptr]] < minsize)
								minsize = grsize[list[ptr]] + grlibs[list[ptr]];
						fire_strat_rule(s, DOUBLE_ATARI1, minsize * 25, NOSQUARE, 0);
					}
				}
			}
		}
		if (G_ALIVE(g) > WEAK)
			continue;
		if (G_ALIVE(g) == VERY_ALIVE)
			continue;	/* 5/01 don't attck living groups */
		addlist(grarmy[g],&armylist);
	}
	killist(&ataris);
	for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
		army = (army_t)list[ptr];
		result = readliferesult((group_t)list[armygroups[army]],FALSE,&movetree);
		if (result <= V_UNKNOWNRES) {	/* if can't read it try to attack it anyway */
			if (A_THREATENED(army))  /* if threatened, just kill it.  attack_weak_army moves look silly against a threatened group */
				kill_th_army(army,passval);
			else {
				alive = A_ALIVE(army);
				if (kosquare != NOSQUARE && alive <= ALIVE)
					poke_eye_ko(army, passval);  /* ko threat */

				attack_weak_army(army, passval);
			}
		}
		else if (result > V_UNKNOWNRES) {	/* 5/01 don't try to kill when it doesn't work */
			firekillrules((group_t)list[armygroups[army]],movetree);  /* already read it */
		}
	}
	killist(&armylist);
	return;
}


/* find move to attack weak, unsettled, or miaialive army
 * blocking extensions are handled elsewhere
 */

static void  attack_weak_army(army_t army, int passval) {
	sqr_t s, sn, sqr;
	list_t ptr, slist = EOL;
	group_t g;
	int aval, val, guess;
	list_t movelist;

	aval = gratkval[list[armygroups[army]]];
	sqr = A_KEYPOINT(army);

	movelist = killmoves(board[sqr]);   /* move generator */
	for (ptr = movelist; ptr != EOL; ptr = link[ptr]) {
		s = MV_MV(list[ptr]);
		if (s == PASS)continue;
		val = 50-MV_PROB(list[ptr]);  /* confidence of killing with this move */
		val = aval*val/50;
		guess = 100+val*2;
		if (val > guess)
			guess = val;
		fire_strat_rule(s, ATTACK_GROUP, val, sqr, guess);  /* only urgent to kill if read it out */
		addlist(s, &slist);
		}
	killist(&movelist);


	if (armyrn_pot[army] == 0 || armyrn_pot[army] > easyrun[A_COLOR(army)] ) {
		killist(&slist);
		return;
		}
	g = (group_t)list[armygroups[army]];
	s = mvs[grpieces[g]];
	if (armysize[army] == 1)
		for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
			sn = list[ptr];
			if (edge[sn] > edge[s] && ld[2*sn-s] == NOLD &&
				lnbf[sn][1-grcolor[g]] == 0 && lnbf[sn][grcolor[g]] == 1 &&
				addlist((sqr_t)(2*sn-s),&slist)) {
				fire_strat_rule((sqr_t)(2*sn-s),ATTACK_WITH_CAP,aval,S_KEYPOINT(s),0);
				}
			}

	killist(&slist);
	}



/* play in key spot to kill eye */

static void poke_eye_ko(army_t army, int passval)
{
	list_t ptr;
	sqr_t s;
	int atkval;

	atkval = gratkval[list[armygroups[army]]];
	for (ptr = armyvitalpoints[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr]&EYEPOINTMASK;
		fire_strat_rule(s,POKE_EYE_KO,atkval+100,A_KEYPOINT(army),0);
	}
}


#ifdef NEVER
/* find out how many eyes army has including eyes of best group can
 * connect to.
 */


static  geteyes(army_t army) {
	int eyes;
	eyes = armyeyes[army];
	return(eyes);
	}
#endif


	
static void  kill_th_army(army_t army, int passval) {
	list_t ptr,tmplist = EOL,t2 = EOL,ptr3;
	sqr_t s;
	int defval;  /* value of defending neighboring groups */
	int cutval;  /* value of capturing cutting stones */
	/* int safeval;   extra for safe capture */
	int val = 0;  /* value of attacking group */

	defval = 0;
	if (armynbp[army] == EOL)getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr])
		if (A_ALIVE(list[ptr]) > ALIVE && A_ALIVE(list[ptr]) <= WEAK)
			defval ++;

	cutval = cut_stones_val(army);	/* see if cutting stones */

	val = gratkval[list[armygroups[army]]];
	
	if (!cutval && !defval && A_ALIVE(army) > WEAK) {  /* don't bother with really weak groups unless */
		if (msptr == 0 || mvs[msptr-1] == PASS ||     /* cutting stones or part of last stone played */
			S_ARMY(mvs[msptr-1]) != army || ahead < 2)
			return;
		}
        
	if (A_ALIVE(army) >= WEAK && msptr != 0 && mvs[msptr-1] != PASS && S_ARMY(mvs[msptr-1]) == army)
		val += 100; /* respond to last move with killing move */
        
	if (armylibs[army] == 1 && (defval || cutval)) {
		fire_strat_rule(list[grlbp[list[armygroups[army]]]],TAKE_OFF_BOARD,0,NOSQUARE,0);
		addlist(list[grlbp[list[armygroups[army]]]],&tmplist);
		}
	else
		for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
			t2 = th_cap_moves((group_t)list[ptr]);
			for (ptr3 = t2; ptr3 != EOL; ptr3 = link[ptr3])
				addlist(list[ptr3],&tmplist);  /* since t2 is not sorted */
			killist(&t2);
			}

				/* find moves to try to capture this group */

	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		fire_strat_rule(s,CAPTURE_THREATENED,val,A_KEYPOINT(army),0);
		if (cutval != 0)
			fire_strat_rule(s,CAPTURE_CUTTING_STONES,cutval,A_KEYPOINT(army),0);
		if (defval != 0)
			fire_strat_rule(s,CAPTURE_DEF_WEAK,defval*50,NOSQUARE,0);
		}
	killist(&tmplist);
	}

/* return a list of all moves that capture the threatened group g */
/* list is not sorted - iscaptured suggested move is first */
	
 list_t th_cap_moves(group_t g) {
	list_t tmplist = EOL, retlist = EOL, ptr;
	sqr_t tmp;
	sqr_t s,sn;

	tmp = grcapmove[g];
   	tmplist = getatkmoves(g);  /* 7/01 better move list */

	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (s == tmp)continue;
		if (canbecaptured(s,1-grcolor[g],mvs[grpieces[g]],grcolor[g],1-grcolor[g], 80, cancapsize[playlevel], taclibs[playlevel], NOGROUP))
			addlist(s,&retlist);
		else if (lnbn[s] == 1) {
			sn = list[nblbp[s]];
			if (sn == tmp)continue;
			if (canbecaptured(sn,1-grcolor[g],mvs[grpieces[g]],grcolor[g],1-grcolor[g], 80, cancapsize[playlevel], taclibs[playlevel], NOGROUP))
				addlist(sn,&retlist);
			}
		}
	if (tmp != NOSQUARE && tmp != PASS) {
		if (inlist(tmp,&retlist))dellist(tmp,&retlist);
		adflist(tmp,&retlist);
		}
	killist(&tmplist);
	return(retlist);
	}

	

	
void kothreat(int color) {
   group_t g;
	list_t ptr;
	sqr_t s;
	int val,live = FALSE;
	int tval = 0;  /* value of winning the ko */
	for (ptr = nbgrp[kosquare][color]; ptr != EOL; ptr = link[ptr])
		if (G_ALIVE(list[ptr]) > ALIVE)
			tval += (grsize[list[ptr]]+grlibs[list[ptr]]) * 50;
	for (ptr = nbgrp[kosquare][1-color]; ptr != EOL; ptr = link[ptr])
		if (G_ALIVE(list[ptr]) > ALIVE)
			tval += (grsize[list[ptr]]+grlibs[list[ptr]]) * 50;
	for (g = 0; g < maxgr; ++g) {
      if (!grlv[g])continue;
      if (grcolor[g] == color)continue;
      if (grlibs[g] != 2 || G_ALIVE(g) > ALIVE)continue;
      if ((sqr_t)list[grlbp[g]] == kosquare || (sqr_t)list[link[grlbp[g]]] == kosquare)
         continue;
      if (badkothreat(g))continue;
      val = grsize[g]*50;
      for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
      	if (gralive[list[ptr]] < WEAK)live = TRUE;
      	else val += (grsize[list[ptr]] + grlibs[list[ptr]]) * 50;
      	}
	  if (val > tval)  /* only so much ast stake in the ko fight */
		  val = tval;
      if (!live)continue;
      for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
	    s = list[ptr];
	    if (lnbn[s] != 0 || S_NEUTRAL(s))
		    fire_strat_rule(list[ptr],ATARI_KOTHREAT,val,NOSQUARE,0);
            }
      }
   }

/* return TRUE if ko threat to capture g is a bad one */

static int badkothreat(group_t g) {
	int i,ldtmp,c;
	sqr_t s = NOSQUARE;
	list_t ptr;
	
	c = grcolor[g];
	i = fdir[kosquare];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		s = kosquare + nbr[i];
		if (grcolor[board[s]] == c && grlibs[board[s]] == 1 &&
		   grsize[board[s]] == 1)break;  /* found ko stone */
		}
	for (ptr = grnbp[board[s]]; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[list[ptr]] != 1)continue;
		/* found neighbor in atari */
		if (inlist(g,&grnbp[list[ptr]]))return(TRUE);
		}
	return(FALSE);
	}

/* connect two groups together */

static void  try_connect(conn_t cn,group_t g1, group_t g2)
{
	int val;
	sqr_t s;
	
	if (cnprot[cn] == SOLID_CONNECT)
		return;
	val = conn_val(G_ARMY(g1), G_ARMY(g2));
	if (val == 0)
		return;
#ifdef NEVER
	to be consistent with pat conenct
	if (G_THREATENED(g1) || G_THREATENED(g2))
		val = 25;
              /* let saving threatened stones determine value */
#endif
	if (cnprot[cn] == AJI_CONNECT && val > 100)
		val = 100;
	if (cncnum[cn] == 1 && cntype[cn] == CN_ONEPOINTJUMP) {  /* hane connections handled by shapes */
		s = list[cnptr[cn]];
		if (cnprot[cn] == KO_CONNECT)
			val = 0;
		fire_strat_rule(s, SINGLE_CONNECTION, val, NOSQUARE, 0);
		s = connect_bamboo(g1,g2,s);
		if (s != NOSQUARE)
			fire_strat_rule(s, CONNECT_BAMBOO, val, NOSQUARE, 0);
	}
	else if (cnlknum[cn] == 2) {
		connect_2_links(cn, val);
	}
}

static void connect_2_links(conn_t cn, int val) {
	sqr_t s, s2;
	if (cntype[cn] == CN_KNIGHTSMOVE && canconnlink(cn,list[cnlkptr[cn]],&s,&s2,NOGROUP)) {
		fire_strat_rule(s,CONNECT_KNIGHTS,val,NOSQUARE,0);
		if (s2 != NOSQUARE)
			fire_strat_rule(s2,CONNECT_KNIGHTS,val,NOSQUARE,0);
		}
    /* only doing one side of move since I'm in a hurry */
	}


/* strategy for cutting and connecting */
/* only connecting long linkages */


int cut_connect(int color) 
{
	conn_t cn;
	int c;
	group_t g1,g2;
	list_t cutpoints = EOL;
   
	for (cn = 0; cn < NUMCONNS; ++cn) {
		if (cncnum[cn] == 0 && cnlknum[cn] == 0)continue;
		g1 = cngr1[cn];
		c = grcolor[g1];
		if (c != color)continue;	/* all cutting moves are handled by shapes - only connect here now */
		g2 = cngr2[cn];
		if (grarmy[g1] == grarmy[g2])continue;
		if (G_ALIVE(g1) == DEAD || G_ALIVE(g2) == DEAD)continue;
		/* found cutting point */
		try_connect(cn, g1, g2);
	}
	killist(&cutpoints);
	return FALSE;
}



static int crawlunder[] = { 0,0,50,100,150,50,0,0,0,0,0,0,0,0,0,0 };
static int crawlover[] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
static int getlibs[] = { 0,0,25,50,75 };
static int takelibs[] = { 0,50,100,50,50 };

/* I saw a segmentation fault in this routine */

/* make blocking move when stones contact diagonally 
 * Get BLOCKING_MOVE at point with two liberties and
 * enemy stones at right angles.
 * If blocking would make an empty triangle then skip instead.
 * If blocking along the edge and have enough liberties then 
 * skip instead
 * color is the color to move
 */


void block_move(int color) {
	sqr_t s,sn,sgood,sbad;  /* sgood is color to move */
    int i,ldtmp,val,along_edge,under,toward_edge,sum;
    int empty_triangle,toward_friend,toward_enemy;
    int enemy_behind;
	
    for (s = firstsquare; s < lastsquare; s++) {
    	if (!S_NEUTRAL(s) || lnbn[s] < 2)continue;
    	sum = 0;
		sgood = sbad = NOSQUARE;
    	i = fdir[s];
    	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
    		sn = s + nbr[i];
    		if (grcolor[board[sn]] == color)sgood = sn;
			if (grcolor[board[sn]] == 1-color)sbad = sn;
			if (board[sn] != NOGROUP)sum += nbr[i];
			}
#ifdef CHECK
		if (sgood == NOSQUARE || sbad == NOSQUARE)
			outerror("something weird in block_move!\n");
#endif
		if (sum == 0)continue;
		if (S_ALIVE(sgood) == DEAD || S_ALIVE(sbad) == DEAD)
			continue;
		if (S_ALIVE(sgood) > WEAK)continue;
		if (S_ALIVE(sbad) > WEAK && ahead <= 2)continue;
		if (S_ALIVE(sbad) == HAS_TWO_EYES && S_ALIVE(sgood) >= WEAK_POTENTIAL &&
			S_NUMLIBS(sbad) >= 4)
			continue;
		if (S_ALIVE(sgood) == HAS_TWO_EYES && S_ALIVE(sbad) > WEAK)
			continue; 
		if (S_THREATENED(sgood) || S_THREATENED(sbad))
			continue;
		if (S_ALIVE(sgood) <= ALIVE && S_ALIVE(sbad) <= ALIVE &&
			rule_fired(s, ENDPATMATCH))
			continue;  /* have good endgame pattern value already */
		fire_strat_rule(s,BLOCKING_MOVE,0,NOSQUARE,0);

       /* found blocking move */
		along_edge = edge[sgood] == edge[s];
		under = lnbf[s+s-sbad][1-color] == 0 &&
			(edge[sbad] > edge[s] || edge2[sbad] > edge2[s]);
		toward_edge = edge[s] < edge[sgood];
		toward_friend = 
	       grcolor[board[s+s-sgood+sbad-s]] == color ||
		       grcolor[board[s+s-sgood+s-sbad]] == color &&
			       (lnbf[s+s-sbad][1-color] != 0 ||
				lnbf[s+s-sbad][color] == 1);
		if (edge[s+s-sgood] > 1)
	       toward_friend |= grcolor[board[s+s-sgood+s-sgood]] == color;
		toward_enemy = lnbf[s+s-sgood][1-color] != 0;
		if (!toward_enemy && edge[s+s-sgood] > 1)
	       toward_friend |= lnbf[s+s-sgood+s-sgood][color] != 0;
		enemy_behind = ld[s+s-sbad] > 1 && grcolor[lgr[s+s-sbad]] == 1-color ||
	       S_NEUTRAL(s+s-sbad);
		sn = s + s - sbad;
		empty_triangle = board[sgood+s-sbad] == board[sgood] &&
			board[s + s-sgood] == NOGROUP && board[s+ s-sbad] == NOGROUP &&
			lnbn[s + s - sgood] >= 3;
		if (empty_triangle) {
			if (rule_fired(s, ENDPATMATCH))
		      fire_strat_rule(s,EMPTY_TRIANGLE,-20,NOSQUARE,0);
			else
	          fire_strat_rule(s,EMPTY_TRIANGLE,-50,NOSQUARE,0);
		}
		if (lnbf[sn][1-color] == 0 && 
       		rterv[sn][color] - rterv[sn][1-color] > MAXRTVAL/4)
	       fire_strat_rule(s,BLOCK_DEF_TERR,0,NOSQUARE,0);
		if (toward_friend && S_ALIVE(sbad) <= WEAK) {
#ifdef NEVER
			val = conn_val(S_ARMY(sgood),G_ARMY(lgr[s+s-sgood]));
			if (val > 100)val = 100;
#endif			
			fire_strat_rule(s,TOWARD_FRIEND,0,NOSQUARE,0);
			if (grlibs[board[sgood]] > 3 && lnbn[s+s-sgood] > 1)
				fire_strat_rule((sqr_t)(s+s-sgood),SKIP_FOR_BLOCK,0,NOSQUARE,0);
			}
		if (toward_enemy)
			continue;
		if (!empty_triangle) {
			if (grlibs[board[sgood]] < 5 && G_ALIVE(board[sgood]) > VERY_ALIVE) {
				fire_strat_rule(s,GET_MORE_LIBS,getlibs[lnbn[s]],NOSQUARE,0);
				}
			if (grlibs[board[sbad]] < 5 && S_ALIVE(sbad) <= WEAK) {
				fire_strat_rule(s,TAKE_AWAY_LIBS,takelibs[grlibs[board[sbad]]],NOSQUARE,0);
				}
			}
		if (along_edge && under) {
			if (empty_triangle) {
				fire_strat_rule((sqr_t)(s+s-sgood),SKIP_FROM_MT_TRI,crawlunder[edge[s]],NOSQUARE,0);
#ifdef NEVER				
				if (edge2[s+s-sgood] > edge2[s] &&
					lnbf[s+s+sgood][grcolor[board[sgood]]] == 0)
					fire_strat_rule(s+s-sgood,OUT_OF_CORNER,0,NOSQUARE,0);
#endif					
				}
			else if (S_ALIVE(sbad) <= UNSETTLED) {
				val = crawlunder[edge[s]];
				if (ld[s+s-sbad] >=3 && ld[s+s-sbad] != NOLD ||
					board[s+s-sbad+s-sgood] != NOGROUP)val = 0;
				fire_strat_rule(s,CRAWL_UNDER,val,NOSQUARE,0);
#ifdef NEVER
				if (edge2[s+s-sgood] > edge2[s])
					fire_strat_rule(s+s-sgood,OUT_OF_CORNER,0,NOSQUARE,0);
#endif					
				}
			}
       else if (along_edge && !under) {
          if (empty_triangle) {
	     	fire_strat_rule((sqr_t)(s+s-sgood),SKIP_FROM_MT_TRI,crawlover[edge[s]],NOSQUARE,0);
             }
	  else if (S_ALIVE(sbad) <= UNSETTLED)
	     fire_strat_rule(s,CRAWL_OVER,crawlover[edge[s]],NOSQUARE,0);
          }
       else if (toward_edge && edge[s] < 4 && !enemy_behind) {
	       fire_strat_rule(s,DEFEND_EDGE_TERR,edge[s]*25,NOSQUARE,0);
	       }
#ifdef NEVER
       else if (!toward_edge && edge[s] < 5)
          fire_strat_rule(s,BLOCK_AWAY,0,NOSQUARE,NOT_URGENT,0);
#endif
       else if (empty_triangle) {
	    fire_strat_rule((sqr_t)(s+s-sgood),SKIP_FROM_MT_TRI,100,NOSQUARE,0);
          }
       }
   }

/* return number of real dame.  if it is nonzero, pass is not allowed */
int fill_dame(int color)
{
	list_t ptr, ptr2, ptr3;
	sqr_t s, sn, s1, capmove;
	group_t i;
	int j, ldtmp, dbonus = 0;
	list_t liblist = EOL, tmplist = EOL;
	int strongest[2];
	int count = 0;
	int capture;
	int libs;
	
	for (i = 0; i < maxgr; ++i) {
		if (!grlv[i])
			continue;
		if (grcolor[i] == color && G_ALIVE(i) <= ALIVE) {
			cpylist(grlbp[i], &liblist);  /* since iscaptured changes grlbp[i] */
			for (ptr = liblist; ptr != EOL; ptr = link[ptr]) {
				s = list[ptr];
				if (S_NEUTRAL(s)) {
					dbonus = 0;
					libs = lnbn[s];
					strongest[0] = strongest[1] = 25;
					capture = FALSE;
					for (ptr3 = nbgrp[s][1 - color]; ptr3 != EOL; ptr3 = link[ptr3]) {
						if (G_NUMLIBS(list[ptr]) == 1)
							capture = TRUE;
						if (G_ALIVE(list[ptr3]) < strongest[1-color])
							strongest[1-color] = G_ALIVE(list[ptr3]);
						if (G_NUMLIBS(list[ptr3]) == 2 && 
							(lnbn[s] > 0 || link[nbgrp[s][1-color]] != EOL)) {  /* opponent can't push in here anyway */
							dbonus += 5;
							for (ptr2 = grlbp[list[ptr3]]; ptr2 != EOL; ptr2 = link[ptr2]) {
								if (!S_NEUTRAL(list[ptr2]))
									dbonus += 40;	/* force to fill territory */
							}
						}
					}
					for (ptr3 = nbgrp[s][color]; ptr3 != EOL; ptr3 = link[ptr3]) {
						libs += G_NUMLIBS(list[ptr3]) - 1;
						if (G_ALIVE(list[ptr3]) < strongest[color])
							strongest[color] = G_ALIVE(list[ptr3]);
						if (G_NUMLIBS(list[ptr3]) == 2 &&
							link[nbgrp[s][color]] != EOL)
							dbonus += 10;  /* connect 2 liberty group thru dame */
					}
#ifdef NEVER
					j = fdir[s];
					for (ldtmp = ldir[j]; j < ldtmp; ++j)  /* better to fill dame with atari */
						if (S_COLOR(s+nbr[j]) == 1-color && 
							S_NUMLIBS(s+nbr[j]) == 2 && 
							(lnbn[s] > 0 || link[nbgrp[s][1-color]] != EOL)) {  /* opponent can't push in here anyway */
							dbonus += 5;
							for (ptr2 = grlbp[board[s+nbr[j]]]; ptr2 != EOL; ptr2 = link[ptr2])
								if (!S_NEUTRAL(list[ptr2]))
									dbonus += 40;	/* force to fill territory */
							}
						else if (S_COLOR(s+nbr[j]) == color && 
							S_NUMLIBS(s+nbr[j]) == 2 &&
							link[nbgrp[s][color]] != EOL)
							dbonus += 10;  /* connect 2 liberty group thru dame */
#endif
					for (ptr3 = eyevitrec[s]; ptr3 != EOL; ptr3 = link[ptr3]) {
						if (cntlist(&eyevital[list[ptr3]&EYEPOINTMASK]) < 4)
							dbonus += 12 - 3 * cntlist(&eyevital[list[ptr3] & EYEPOINTMASK]);
					}
					dbonus += cntlist(&cnbrd[s]) * 6;  /* prefer to connect in dame */

					if (strongest[0] < WEAK && strongest[1] < WEAK && 
						(libs > 1 || capture) &&	/* no self atari in a dame */
						addlist(s, &tmplist) && !rule_fired(s, ENDPATMATCH)) {
					   	fire_strat_rule(s, FILLDAME_NEUTRAL, lnbn[s] + dbonus, NOSQUARE, 0);
						if (strongest[0] <= UNSETTLED && strongest[1] <= UNSETTLED) 
							count++;
					}

					if (lnbn[s] == 0 && strongest[0] <= ALIVE && strongest[1] <= ALIVE) {
						j = fdir[s];
						for (ldtmp = ldir[j]; j < ldtmp; ++j) {
							sn = s + nbr[j];
							if (S_COLOR(sn) == color && S_NUMLIBS(sn) <= 3) {
								mvs[msptr] = s;
								mvcolor[msptr] = color;
								if (lupdate(msptr)) {
									++msptr;
									if (iscaptured(board[s], 80, cancapsize[playlevel], taclibs[playlevel], mvmost[playlevel], 1-color, NOGROUP, &capmove, 1-color) && addlist(capmove,&tmplist) &&
										nbgrp[capmove][color] != EOL && link[nbgrp[capmove][color]] != EOL) {
										fire_strat_rule(capmove, FILLDAME_CONNECT, dbonus, NOSQUARE, 0);
										count++;
									}
									--msptr;
								}
								ldndate(msptr);
							}
						}
					}
					if (lnbf[s][color] == 1 && G_ALIVE(lgr[s]) <= ALIVE) {
						j = fdir[s];
						for (ldtmp = ldir[j]; j < ldtmp; ++j) {
							sn = s + nbr[j];
							if (S_COLOR(sn) == color && S_NUMLIBS(sn) == 2) {
								s1 = list[grlbp[board[sn]]];
								if (s1 == s)
									s1 = list[link[grlbp[board[sn]]]];
								if (addlist(s1, &tmplist)) {
									fire_strat_rule(s1, FILLDAME_CONNECT, dbonus, NOSQUARE, 0);
									count++;
								}
								for (ptr3 = grnbp[board[sn]]; ptr3 != EOL; ptr3 = link[ptr3]) {
									if (grlibs[list[ptr3]] == 1) {
										fire_strat_rule(list[grlbp[list[ptr3]]], FILLDAME_CONNECT, dbonus, NOSQUARE, 0);
										count++;
									}
								}
							}
						}
					}								
				}
				else {
					j = fdir[s];
					for (ldtmp = ldir[j]; j < ldtmp; ++j) {
						sn = s + nbr[j];
						if (gralive[lgr[s]] == DEAD && board[sn] != NOGROUP &&
							gralive[board[sn]] <= ALIVE) {
							for (ptr2 = grlbp[board[sn]]; ptr2 != EOL; ptr2 = link[ptr2]) {
								if ((sqr_t)list[ptr2] != s && addlist(list[ptr2],&tmplist)) {
									fire_strat_rule(list[ptr2], FILLDAME_CONNECT, 0, NOSQUARE, 0);
									count++;
								}
							}
						}
						if (ld[sn] != NOLD && !S_NEUTRAL(sn) && 
							grcolor[lgr[sn]] != grcolor[lgr[s]] &&
							G_ALIVE(lgr[sn]) < WEAK) {
								if (lnbn[sn] > 1) {
									if (addlist(sn,&tmplist) && !rule_fired(sn, ENDPATMATCH)) {
										fire_strat_rule(sn, FILLDAME, dbonus + 50, NOSQUARE, 0);
										count++;
									}
									if (addlist(s,&tmplist) && !rule_fired(s, ENDPATMATCH)) {
										fire_strat_rule(s, FILLDAME, 25, NOSQUARE, 0);
										count++;
									}
								}
								else if (addlist(s, &tmplist) && !rule_fired(s, ENDPATMATCH)) {
									fire_strat_rule(s, FILLDAME, dbonus + 25, NOSQUARE,0);
									count++;
							}
						}
					}
				}
			}
			killist(&liblist);
		}
	}
	killist(&tmplist);
	return count;
}

static int obv_val[NUMLEVELS] =
{
	300, 200, 150, 75, 50, 25, 25 
};

void obvious_answer(int ply)
{
	list_t moves, ptr;
	int val = obv_val[playlevel];
	if (msptr == 0)
		return;
	moves = genobviousmoves(1., ply, -1, FALSE);

	for (ptr = moves; ptr != EOL; ptr = link[ptr]) {
		if ((sqr_t)list[ptr] == PASS || 
			(sqr_t)list[ptr] == NOSQUARE || (sqr_t)list[ptr] == kosquare)
			continue;
		fire_strat_rule(list[ptr], OBVIOUSANSWER, val, NOSQUARE, 0);  
	}
	killist(&moves);
}


static void squirm_capture(group_t g)
{
	list_t ptr;
	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr])
		fire_strat_rule(list[ptr], SQUIRM_CAPTURE, 0, NOSQUARE, 0);
}
    

/* called when behind to try to find odd move that might work */
	
void squirm(int passval, int color) {
	group_t g;
	list_t sarmy = EOL,garmy = EOL, marmy = EOL,ptr;
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])continue;
		if (grcolor[g] != color && (G_ALIVE(g) == BARELY_ALIVE ||
					G_ALIVE(g) == MIAI))
					addlist(grarmy[g],&marmy);
		if (grcolor[g] != color && G_ALIVE(g) == DEAD)
			for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr])
				if (G_ALIVE(list[ptr]) > WEAK) {
					squirm_capture(g);
					break;
					}
		if (grcolor[g] == color && G_ALIVE(g) > WEAK && 
		   armyeyespace[grarmy[g]] >= 8)
		   	addlist(grarmy[g],&sarmy);
		}
	if (ahead == 0) {
		for (ptr = sarmy; ptr != EOL; ptr = link[ptr])
			addlist(list[armygroups[list[ptr]]],&garmy);
		for (ptr = garmy; ptr != EOL; ptr = link[ptr])
			save_weak_army(grarmy[list[ptr]],passval,color);
		}
	if (ahead <= 1)
		try33invasion(color);
	for (ptr = marmy; ptr != EOL; ptr = link[ptr])
		play_inside((army_t)list[ptr]);
	killist(&marmy);
	killist(&garmy);
	killist(&sarmy);
	}

static void play_inside(army_t army) {
	list_t ptr;
	int c;
	sqr_t s1,s2;
	list_t vital = EOL;
	
	c = A_COLOR(army);
	for (ptr = armyvitalpoints[army]; ptr != EOL; ptr = link[ptr]) {
		if (addlist((sqr_t)(list[ptr]&EYEPOINTMASK), &vital))
			fire_strat_rule((sqr_t)(list[ptr]&EYEPOINTMASK),BEHIND_VITAL,
					0,NOSQUARE,0);
		}
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr])
		if (inlistm(list[ptr],&armyvitalpoints[army],EYEPOINTMASK))
			continue;
		else if (lnbn[list[ptr]] == 1 && lnbf[list[ptr]][1-c] == 0 && 
		   lnbn[list[nblbp[list[ptr]]]] > 1 &&
		   lnbf[list[nblbp[list[ptr]]]][1-c] == 0) {
			if (addlist(list[nblbp[list[ptr]]], &vital))
				fire_strat_rule(list[nblbp[list[ptr]]],BEHIND_VITAL,
					0,NOSQUARE,0);
			}
		else if (lnbn[list[ptr]] == 2 && lnbf[list[ptr]][1-c] == 0) {
			s1 = list[nblbp[list[ptr]]];
			s2 = list[link[nblbp[list[ptr]]]];
			if (lnbn[s1] == 3 && lnbf[s1][1-c] == 0 && 
			   lnbn[s2] == 4 && addlist(s2, &vital))
				fire_strat_rule(s2,BEHIND_VITAL,
					0,NOSQUARE,0);
			if (lnbn[s2] == 3 && lnbf[s2][1-c] == 0 && 
			   lnbn[s1] == 4 && addlist(s1, &vital))
				fire_strat_rule(s1,BEHIND_VITAL,
					0,NOSQUARE,0);
			}
	killist(&vital);
	}
    
                




	            

void center(int color)
{
	sqr_t s,so,so2,mins = NOSQUARE;
	int c,minval = 1000,guess = 0;
	list_t ptr,crawl = EOL,block = EOL;
	int friends;
	
	for (s = 0; s < boardsquare; s++) {
		if (board[s] != NOGROUP)continue;
		if (ld[s] != NOLD && !S_NEUTRAL(s) && ld[s] < 4 &&
		   G_ALIVE(lgr[s]) <= ALIVE) {
			c = grcolor[lgr[s]];
			friends = FALSE;
			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
				if (lnbf[list[ptr]][color] != 0)
					friends = TRUE;
			if (rterv[s][1-c] > MAXRTVAL/6)
				for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
					so = list[ptr];
					if (edge[so] == 1)continue;
					if (ld[so] != NOLD)continue;
					so2 = so + so - s;
					if (rterv[so][c] < rterv[so][1-c] ||
						ld[so2] == NOLD && rterv[so2][c] < rterv[so2][1-c] &&
						rterv[so2][1-c] > rterv[so][1-c]) {
						guess = 0;
						if (ld[so2] == NOLD)guess = 150;
						else if (ld[so2] == 2)guess = 100;
						if (c == color) {
							if (!rule_fired(so, EXTEND_TO_ENEMY) && 
								!rule_fired(so, ENDPATMATCH) &&
								!rule_fired(so, PATRUN) &&  /* already values entering enemy terr */
								addlist(so,&crawl))
								fire_strat_rule(so,INTO_ENEMY_TERR,guess,NOSQUARE,guess);
							}
						else if (c == 1-color) {
							if (friends || rule_fired(so, PATSURROUND))guess = 0; /* let patterns handle it */
							if (addlist(so,&block))
								fire_strat_rule(so,BLOCK_ENEMY_TERR,guess,NOSQUARE,guess);
							}
						}
					}
		}
		if (edge[s] <= 4)continue;
		if (lgr[s] != NOGROUP)continue;
		if (rterv[s][1-color] == 0 && rterv[s][color] < minval) {
			minval = rterv[s][color];
			mins = s;
		}
		if (!rule_fired(s, PATMATCH) &&  /* 7/01 redundant with shape pattern */
			g2abs(rterv[s][1] - rterv[s][0]) < MAXRTVAL/4 && isjunc(s)) {
			fire_strat_rule(s,PLAY_IN_CENTER,(rterv[s][0]+rterv[s][1])/2,NOSQUARE,0);
		}
	}
	if (mins != NOSQUARE && ahead > 2) {
		fire_strat_rule(mins,DEFEND_MOYO,0,NOSQUARE,0);
	}
	killist(&crawl);
	killist(&block);
}

static int isjunc(sqr_t s) {
	sqr_t sn;
	int i,ldtmp,pos,neg;
	
	pos = neg = FALSE;
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		sn = s + nbr[i];
		if (ld[sn] != NOLD)continue;
		if (rterv[sn][0] > rterv[sn][1] && rterv[s][1] > 0 &&
		   rterv[sn][0] > rterv[s][0])neg = TRUE;
		if (rterv[sn][0] < rterv[sn][1] && rterv[s][0] > 0 &&
		   rterv[sn][1] > rterv[s][1])pos = TRUE;
		} 
	return(neg && pos);
	}



