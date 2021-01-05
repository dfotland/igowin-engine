/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2hd.h"
#include "learn.h"
#ifdef UCT
#include "uct.h"
#endif

static void newgrouparmy(group_t g);
static void combine(group_t i,group_t j,int upptr);
static void makenewgroup(sqr_t s,int c,int upptr);
static void kilgrp(group_t g,int c);
static void split(group_t i, group_t j);   /* bring i back to life and split it from j */ 


int numcmoves[4];
  
extern int cfac[3];

/* add liberties due to stone going away
 * s is square where stone was taken away.
 * gb is group number of group being destroyed
 * c is color of stone removed
 */
  
void adplib(sqr_t s, group_t gb, int c) {
	sqr_t sn;
	int i;
	group_t g;
	int ldtmp;
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i ) {
		sn = s + nbr[i];
		--lnbf[sn][c];
		++lnbn[sn];
		addlist(s,&nblbp[sn]);
		g = board[sn];
		if (g == NOGROUP)continue;
		if (g == gb)continue;
		if (addlist(s,&grlbp[g])) {
			++grlibs[g];
			adflist(g,&nbgrp[s][grcolor[g]]);
			} 
		} 
	}
  
 
void deplib(sqr_t s, group_t gb) {  /* subtract liberties due to stone resurrecting */
   sqr_t sn;
   int i,c;
   group_t g;
   int ldtmp;
   c = grcolor[gb];
   i = fdir[s];
   for (ldtmp = ldir[i]; i < ldtmp; ++i ) {
      sn = s + nbr[i];
      ++lnbf[sn][c];
      --lnbn[sn];
      dellist(s,&nblbp[sn]);
      g = board[sn];
      if (g == NOGROUP)continue;
      if (g == gb)continue;
      if (dellist(s,&grlbp[g])) {
         --grlibs[g];
         dlflist(g,&nbgrp[s][grcolor[g]]);
         }
      addlist(g,&grnbp[gb]);
      addlist(gb,&grnbp[g]);
      }
   }
  
void adlibs(sqr_t s, group_t g) { /* add liberties due to new stone in group */
	sqr_t sn;
	group_t g2;
	int i,ldtmp,c;
	c = grcolor[g];
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) { 
		sn = s + nbr[i]; 
		g2 = board[sn];
		if (g2 == NOGROUP) { 
			if (addlist(sn,&grlbp[g])) {
				++grlibs[g];
				adflist(g,&nbgrp[sn][c]);
				}
			} 
		else if (grcolor[g2] != c) {
			addlist(g2,&grnbp[g]);
			addlist(g,&grnbp[g2]);
			} 
		}
	}
  
  
void delibs(sqr_t s, group_t g) {  /* delete liberties,neighbors due to stone going away */
	group_t g1,g2;
	sqr_t sn,sn2,sn3;
	int ptr;  /* mvs ptr not list ptr */
   int i,j,lflag,nflag,k,ldtmp,ldtm2,c;
   c = grcolor[g];
   i = fdir[s]; 
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      sn = s + nbr[i];
      g1 = board[sn]; 
      nflag = FALSE;
      if (g1 != NOGROUP) {
        if (grcolor[g1] == c)continue;
        if (grsize[g1] > grsize[g]) { 
           g2 = g1; 
           g1 = g;
           }
        else
           g2 = g;
                    /* search the smallest group */ 
        ptr = grpieces[g1]; 
        while(ptr != -1) {
           sn2 = mvs[ptr];
           if (edge[sn2] < 2) { 
            k = fdir[sn2];
            for (ldtm2 = ldir[k]; k < ldtm2; ++k) {
              sn3 = sn2 + nbr[k];
              if (board[sn3] == g2) {
                 nflag = TRUE;
                 break;
                 }
              }
            }
           else
              if (board[sn2+1] == g2 || board[sn2-1] == g2 ||
                 board[sn2+boardsize]
                 == g2 || board[sn2-boardsize] == g2)nflag = TRUE;
           if (nflag)break;
           ptr = mvnext[ptr];
           }
        if (!nflag) {
           dellist(g1,&grnbp[g2]);
           dellist(g2,&grnbp[g1]);
           }
        continue;
        }
      lflag = TRUE;
      if (edge[sn] < 2) {
       j = fdir[sn];
       for (ldtm2 = ldir[j]; j < ldtm2; ++j) {
         sn2 = sn + nbr[j];
         if (board[sn2] == g) {
            lflag = FALSE;
            break;
            }
         }
       }
      else
         if (board[sn+1] == g || board[sn-1] == g ||
          board[sn+boardsize] == g || board[sn-boardsize] == g)lflag = FALSE;
      if (lflag) {
         --grlibs[g];
         dellist(sn,&grlbp[g]);
		 dlflist(g,&nbgrp[sn][c]); 
         }
      } 
   }
  

void resurrect(group_t g, int c) { /* bring back group g.  c is enemy color */
	sqr_t s;
	int lptr;
	grlv[g] = TRUE;
	lptr = grpieces[g];
	while(lptr != -1) {
		--numpris[c];
		s = mvs[lptr]; 
		board[s] = g;
		ADDBITS(s,1-c);
		upxy(s); 
  
       if (pclsnext < NUMPCLS)
	       pcls[pclsnext++] = s;
#ifdef CHECK
       if (pclsnext > NUMPCLS) { 
          outerror("pcls overflow ");
          } 
#endif
       uscan(s);
       deplib(s,g); 	/* fix liberty and neighbor lists */
       brkconns(s);    /* break connections due to resurrection*/
	   diagconn(s);		/* add diag conns due to group coming back */
       lptr = mvnext[lptr]; 
       }
   pscr += gralprob[g] * grsize[g] * cfac[grcolor[g]];
   newgrouparmy(g);
   }

static void newgrouparmy(group_t g) {
   army_t army;
   group_t g2;
	list_t ptr;
   army = NOARMY;
   for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
      g2 = (group_t)list[ptr];
      if ((gralive[g2] & 31) == DEAD && grdeadarmy[g2] != NOARMY)
	 army = grdeadarmy[g2];
      }
   if (army == NOARMY) {
      army = (army_t)gtflist(&armyfreelist);
#ifdef CHECK
      if (army == NOARMY) {
         outerror("out of armies");
         turnoffcplay();
		return;
         }
#endif
      if (army == G2ERROR) {
#ifdef TEST
	      clearerror();
	      outerr("Out of armies in newgrouparmy");
	      turnoffcplay();
#endif	      
		  army = 0;
	      armysize[army] += grsize[g];
	      }
	  else

	      armysize[army] = grsize[g];
      }
   else
      armysize[army] += grsize[g];
   grarmy[g] = army;
   addlist(g,&armygroups[army]);
   }

/*
 * take back a move.  return the move that was taken back 
 */
g2status takebackamove(int rules, sqr_t *s) 
{
	int c;
	if (msptr == 0) {
		*s = NOSQUARE;
		return G2FAIL;
	}
	c = mvcolor[msptr - 1];
	if (mvs[msptr - 1] == PASS && rules == JAPANESE) {  /* get a point for passing, since eval is chinese style */
		passscr += c == BLACKCOLOR ? 50 : -50;
	}
	if (pclsnext < NUMPCLS && mvs[msptr - 1] != PASS) {
		pcls[pclsnext++] = mvs[msptr - 1];  /* so life will get called */
	}
	dndate();
	if (rules == AMERICAN && mvs[msptr] == PASS)	/* get a prisoner for passing under american rules */
		numpris[1 - mvcolor[msptr]]--;

	findmatch(msptr, FALSE);
	jdndate();
#ifdef UCT
	uct_takebackmove();
#endif

	*s = mvs[msptr];
	return G2OK;
}

/*
 * take back a move.  return the move that was taken back 
 */
g2status takebackauctmove(int rules, sqr_t *s) 
{
	int c;
	if (msptr == 0) {
		*s = NOSQUARE;
		return G2FAIL;
	}
	c = mvcolor[msptr - 1];
	if (mvs[msptr - 1] == PASS && rules == JAPANESE) {  /* get a point for passing, since eval is chinese style */
		passscr += c == BLACKCOLOR ? 50 : -50;
	}
	if (pclsnext < NUMPCLS && mvs[msptr - 1] != PASS) {
		pcls[pclsnext++] = mvs[msptr - 1];  /* so life will get called */
	}
	dndate();
	if (rules == AMERICAN && mvs[msptr] == PASS)	/* get a prisoner for passing under american rules */
		numpris[1 - mvcolor[msptr]]--;

	findmatch(msptr, FALSE);
	jdndate();
//	uct_takebackmove();

	*s = mvs[msptr];
	return G2OK;
}

static void split(group_t i, group_t j) {   /* bring i back to life and split it from j */ 
   army_t army;
   list_t ptr, lptr;
   int mptr;  /* mvs ptr, not list ptr */
   int c;
   
   c = grcolor[j];
   grlibs[j] = 0; 
   grlv[i] = TRUE;
   for (ptr = grlbp[j]; ptr != EOL; ptr = link[ptr])
   	dlflist(j,&nbgrp[list[ptr]][c]);
   killist(&grlbp[j]); 
   if ((gralive[j]&31) == DEAD)
      for (ptr = grnbp[j]; ptr != EOL; ptr = link[ptr]) {
         addlist(mvs[grpieces[list[ptr]]],&charmy);
         }
   lptr = grnbp[j]; 
   while(lptr != EOL) {
      dellist(j,&grnbp[list[lptr]]);
      lptr = link[lptr];
      } 
   killist(&grnbp[j]); 
   grsize[j] -= grsize[i];
   army = grarmy[j];
   grarmy[i] = army;
   addlist(i,&armygroups[army]); 
   for (mptr = grpieces[i]; mptr != -1; mptr = mvnext[mptr]) {
      adlibs(mvs[mptr],i);
      board[mvs[mptr]] = i; 
      cscan(mvs[mptr],j); 
      dncons(mvs[mptr],j);  
      chkcon(mvs[mptr],i);
      rstrlks(i,j,mvs[mptr]);
      } 
   mptr = grpieces[j];
   while(mvnext[mptr] != (signed)grpieces[i]) 
      mptr = mvnext[mptr];
   mvnext[mptr] = -1;
   mptr = grpieces[j];
   while(mptr != -1) {
      adlibs(mvs[mptr],j);
      mptr = mvnext[mptr];
      } 
   gralive[i] = gralive[j];
   gralprob[i] = gralprob[j]; 
   grthreatened[i] = grthreatened[j];
   }

  
void dndate(void) {
	sqr_t s,sn2;
	group_t g,g2;
   int lptr,c,x,y,ptr,lptr2;
   unsigned int dnptr;
   army_t army;

   msptr--;
   dnptr = msptr;
	if (savemove[1-mvcolor[dnptr]] > dnptr)
		savevalid[1-mvcolor[dnptr]] = FALSE;
   kosquare = kosave[dnptr];
   if (kosquare != NOSQUARE) {
       if (pclsnext < NUMPCLS)
	       pcls[pclsnext++] = kosquare;
	mrglist(cnbrd[kosquare],&cnchgd);
	gralive[lgr[kosquare]] |= 32;
	}
   s = mvs[dnptr];
   if (s == PASS) {
	   if (showtactics)looktakeback(PASS);
		return;
   		}
   c = mvcolor[dnptr];
   DELBITS(s,c);
   if (edge[s] <= 4) {
      x = xval[s];
      y = yval[s];
      if (xmin > x-3)xmin = x-3;
      if (xmax < x+3)xmax = x+3;
      if (ymin > y-3)ymin = y-3;
      if (ymax < y+3)ymax = y+3;
      }
   g = board[s];
   if (g == NOGROUP) {  /* last move was suicide */
   		if (mvcapt[dnptr] != EOL) {
	   		g = (group_t)list[mvcapt[dnptr]];
   			resurrect(g,1-c);
   			}
#ifdef TEST   			
   		else
   			outerror("Bad dnptr in resurrect");
#endif   			
   		}
   	else
  
     for (ptr = mvcapt[dnptr]; ptr != EOL; ptr = link[ptr]) { 
                         /* bring back killed groups */ 
         g2 = (group_t)list[ptr];
         resurrect(g2,c);  /* resurrect enemy group */
         }
   for (ptr = mvconn[dnptr]; ptr != EOL; ptr = link[ptr]) {
                         /* split combined groups */
         g2 = (group_t)list[ptr];
         split(g2,g);
	 if (grlibs[g2] == 2) {
		 sn2 = list[grlbp[g2]];
		 if (sn2 == s)sn2 = list[link[grlbp[g2]]];
		 if (edge[sn2] <= 4) {
			 x = xval[sn2];
			 y = yval[sn2];
			 if (xmin > x-3)xmin = x-3;
			 if (xmax < x+3)xmax = x+3;
			 if (ymin > y-3)ymin = y-3;
			 if (ymax < y+3)ymax = y+3;
			 }
		 }
         }
   if (grlibs[g] == 1) {
	   sn2 = list[grlbp[g]];
	   if (edge[sn2] <= 4) {
		   x = xval[sn2];
		   y = yval[sn2];
		   if (xmin > x-3)xmin = x-3;
		   if (xmax < x+3)xmax = x+3;
		   if (ymin > y-3)ymin = y-3;
		   if (ymax < y+3)ymax = y+3;
		   }
	   }
   dscan(s);
   dncons(s,g);
   dnxy(s,NOCOLOR); 
   board[s] = NOGROUP;
   pscr -= gralprob[g] * cfac[grcolor[g]];
   --grsize[g]; 
   --armysize[grarmy[g]];
  
       if (pclsnext < NUMPCLS)
	       pcls[pclsnext++] = s;
   if (grsize[g] == 0) {  /* only one piece in group */ 
#ifdef CHECK
      if (maxgr != g+1) { 
          outerr("maxgr is wrong in dndate");
          }
#endif
      if (grdeadarmy[g] != NOARMY) {
         dellist(g,&armydeadgroups[grdeadarmy[g]]);
         grdeadarmy[g] = NOARMY;
         }
      army = grarmy[g];
      dellist(g,&armygroups[army]);
      if (armygroups[army] == EOL) {
		make_army_free(army);
        }
      for (lptr = grlbp[g]; lptr != EOL; lptr = link[lptr])
      	dlflist(g,&nbgrp[list[lptr]][c]);
      killist(&grlbp[g]);     /* return liberties to free list */
      grlibs[g] = 0;
      grlv[g] = FALSE;
      if ((gralive[g]&31) == DEAD && grnbp[g] != EOL)
         addlist(mvs[grpieces[list[grnbp[g]]]],&charmy);
	  gralive[g] = 0;
      lptr = grnbp[g];
      while(lptr != EOL) {
         dellist(g,&grnbp[list[lptr]]);
         lptr = link[lptr];
         }
      killist(&grnbp[g]); /* return neighbors to free list */
      kill_ldrflags(g);
      --maxgr;      /* delete group */
      }
   else {                  /* take piece away from group */
      lptr = grpieces[g];
      while(mvnext[lptr] != -1) {
         lptr2 = mvnext[lptr];
         if (mvs[lptr2] == s) {
            mvnext[lptr] = -1;
            break;
            } 
         lptr = mvnext[lptr]; 
         }
      delibs(s,g);
      } 
   adplib(s,NOGROUP,c); 
   adcons(s,NOGROUP);
   if (showtactics) {
   		looktakeback(s);
   		}
   }


void make_army_free(army_t army) {
	int ptr;
   	int i;
   	for (i = 0; i < NUMRUN; ++i)killist(&armyrun[army][i]);
#ifdef CHECK

	for (ptr = armyfreelist; ptr != EOL; ptr = link[ptr])
		if (list[ptr] == army)
			outerror("freeing free army!");

	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr])
		if (grarmy[list[ptr]] == army)outerror("Freeing army, still has group \n");
#endif
	killist(&armygroups[army]);
	for (ptr = armydeadgroups[army]; ptr != EOL; ptr = link[ptr])
		if (grdeadarmy[list[ptr]] == army)
			grdeadarmy[list[ptr]] = NOARMY;
	killist(&armydeadgroups[army]);
	killist(&armylbp[army]);
	if (armynbp[army] != EOL)killist(&armynbp[army]);
	killist(&armyvitalpoints[army]);
	killist(&armyeyerecs[army]);
	armylibs[army] = 0;
	adflist(army,&armyfreelist);
	killist(&armypot[army]);
	}
     
/* return TRUE if move is illegal ko capture */
     
int illegalko (sqr_t s, int msptr, int rules)
{
	if (s == kosquare) {
		return TRUE;
	}
	if (rules == JAPANESE) {
		return FALSE;  /* japanese rules only forbid recapture of simple ko */
	}
	if (kosquare != NOSQUARE && msptr > 10 &&
		kosave[msptr-1] != NOSQUARE &&
		kosave[msptr-2] != NOSQUARE && 
		kosave[msptr-3] != NOSQUARE && 
		kosave[msptr-4] != NOSQUARE && 
		s == kosave[msptr-2]) {
		return TRUE;
	}
	return FALSE;
}
 
int noroomerror;

int noroomleft(void) 
{
 	int t;
	if (msptr >= MAXMOVE) {  /* can't take any more moves */
		noroomerror = 1;
		return TRUE;
	}
	if (maxgr >= MAXGROUP) {
		noroomerror = 2;
		return TRUE;
	}
	if (cntlist(&armyfreelist) < ARMIESTOMOVE) {
		noroomerror = 3;
		return TRUE;
	}
	if (cntlist(&eyefreelist) < EYESTOMOVE) {
		noroomerror = 4;
		return TRUE;
	}
	if (cntlist(&cnfreelist) < CONNSTOMOVE) {
		noroomerror = 5;
		return TRUE;
	}
	if ((t = cntlist(&freelist)) < 10000) {
		noroomerror = 6;
		return TRUE;
	}
	noroomerror = 0;
	return FALSE;
}

/* make a move in the uct search */
g2status makeuctmove(sqr_t s, int c, int rules)
{
	int t;

	if (s == PASS && rules == JAPANESE) { /* get a point for passing for real, since eval is chinese style */
		passscr += c == BLACKCOLOR ? -50 : 50;
	}
 	
//	life(FALSE);  /* so can check attribute for pattern matches */
	checkmoveatts(s, c);  /* check that attributes match for moves added to already matched patterns. */
		/* will be a problem with illegal suicide */
	t = update(s, c, rules != GOE);
	if (t == TRUE) {
		if (s == PASS && rules == AMERICAN) {
				numpris[1-c]++;
		}
		jupdate(s, c);
		findmatch(msptr - 1, TRUE);
//		uct_makeamove(s, c);
		return G2OK ;
	}
	if (t != -1) {
		dndate();
	}
	return G2SUICIDE;
}
 
/* make a move for real.  do all the update stuff, and also 
 * update the shpbrd structure and the josekis structures
 * return atari TRUE if move is atari.  Only use to make
 * an actual move on the real board.
 * if koisok, will allow illegal ko captures.  use this when replaying
 * a game from a file, since ko rules vary
 *
 * rules tells which rule set is to be used
 *
 * return G2OCCUPIED or G2KO for illegal moves, G2NOROOM if data structures out of space
 * return G2OK for a legal move.
 */

g2status makeamove(sqr_t s, int c, int rules, int *atari, int koisok)
{
	int t, i, rep, ldtmp;
	sqr_t sn;
	group_t g;

	*atari = FALSE;

	if (noroomleft()) {
		return G2NOROOM;
	}

	if (s < 0 || s != PASS &&
		(s >= boardsquare || board[s] != NOGROUP)) {
		return G2OCCUPIED ;
	}

	if (!koisok && s == kosquare) {
		return G2KO;
	}

	if (s == PASS && rules == JAPANESE) { /* get a point for passing for real, since eval is chinese style */
		passscr += c == BLACKCOLOR ? -50 : 50;
	}
 	
//	life(FALSE);  /* so can check attribute for pattern matches */
//	checkmoveatts(s, c);  /* check that attributes match for moves added to already matched patterns. */
		/* will be a problem with illegal suicide */
	t = update(s, c, rules != GOE);
	if (t == TRUE) {
		if (s != PASS) {
			i = fdir[s];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {   /* look at neighbors */ 
				sn = s + nbr[i];
				g = board[sn];
				if (g != NOGROUP && grcolor[g] == 1-c && grlibs[g] == 1) {
					*atari = TRUE;
				}
			}
		}
		if (s != PASS && rules != JAPANESE) {	/* check for superko if not Japanese rules */
			rep = rules == CGOS? 1 : 2;
			for (i = msptr-2; i >= 0; i -= rep) {
				if (hashval[i][0] == hashval[msptr][0]) {
					dndate();
					return G2SUPERKO;		/* illegal move */
				}
			}
		}
		if (s == PASS && rules == AMERICAN) {
				numpris[1-c]++;
		}
		jupdate(s, c);
		findmatch(msptr - 1, TRUE);
#ifdef UCT
		uct_makeamove(s, c);
#endif
		return G2OK ;
	}
	if (t != -1) {
		dndate();
	}
	return G2SUICIDE;
}  

#if 0
/* return the point modified for a symmetry */ 
sqr_t hashsym(sqr_t s, int h) {
	int x = xval[s];
	int y = yval[s];
	int tmp;
	if (h&1)
		x = boardsize-1-x;
	if (h&2)
		y = boardsize-1-y;
	if (h&4) {
		tmp = x;
		x = y;
		y = tmp;
	}
	return y*boardsize+x;
}
#endif
        
/* update the data structures for a move made.
 * use for any kind of full board lookahead.  Do not call from
 * inside life().
 * s and c are the move to make.
 * if nosuicide is TRUE, suicide is not allowed
 * update returns TRUE if the move is legal, FALSE or -1 otherwise
 * if it returns FALSE, the move has still been made, so must call dndate to take it back
 * if move is on a stone, not updated, so dndate must not dndate it!  return is -1
 * does not check for illegal Ko capture
 * updates all the basic datastructures from lupdate, plus ld[] and connections
 * sets up what needs reeval for next life() call.
 */
  
 
int update(sqr_t s, int c, int nosuicide) {
	int i, gflag, lptr;
	group_t g, gnew = NOGROUP;
	sqr_t sn, sn2;
	int x, y, ldtmp;
	list_t ptr;
	int ptr2;
	int upptr = msptr;
   
#ifdef CHECK
	if (pclsnext > NUMPCLS) {
		outerror("pcls overflow "); 
	}
#endif
	if (s != PASS && board[s] != NOGROUP) {
#ifdef CHECK
		outerror("Bad move in update!\n");
#endif
	   return -1;	/* illegal move */
	}

   /* stack the move */
	mvs[msptr] = s;
	mvcolor[msptr] = c;
	msptr++;

	if (gralvalid) { /* save current gralive values */
		savevalid[c] = TRUE;
		savemove[c] = upptr;
		for (i = 0; i < maxgr; ++i)
			savegral[c][i] = gralive[i];
	}
	gralvalid = FALSE;
	kosave[upptr] = kosquare;
	if (kosquare != NOSQUARE) {
		if (pclsnext < NUMPCLS)
			pcls[pclsnext++] = kosquare;
		mrglist(cnbrd[kosquare], &cnchgd);
		gralive[lgr[kosquare]] |= 32;
	}
	kosquare = NOSQUARE;
	killist(&mvconn[upptr]); 
	killist(&mvcapt[upptr]);
	if (s == PASS) {
		if (showtactics) {
			lookmove(mvs[upptr], mvcolor[upptr]);
		}
		learn_hash_move(msptr);
		return TRUE;	/* pass always legal */
	}
	if (edge[s] <= 4) {
		x = xval[s];
		y = yval[s];
		if (xmin > x-3)xmin = x-3;
		if (xmax < x+3)xmax = x+3;
		if (ymin > y-3)ymin = y-3;
		if (ymax < y+3)ymax = y+3;
	}
	if (pclsnext < NUMPCLS)
		pcls[pclsnext++] = s;
#ifdef G2DEBUGOUTPUT
	++nummoves;
#endif
	gflag = FALSE;
	ADDBITS(s,c);
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {   /* look at neighbors */ 
		sn = s + nbr[i];
		g = board[sn];
		++lnbf[sn][c];
		--lnbn[sn]; 
		dellist(s, &nblbp[sn]);
		if ( grcolor[g] == 1-c) {      /* unfriendly neighbor */
			if (dellist(s, &grlbp[g])) {
				--grlibs[g];
				dlflist(g, &nbgrp[s][1-c]);
				if (grlibs[g] == 0) {  /* killed enemy group */ 
					adflist(g, &mvcapt[upptr]);
				} 
			}
		}
	}
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) { 
		sn = s + nbr[i];
		g = board[sn];
		if (grcolor[g] == c) {   /* friendly neighbor */
			if (grlibs[g] == 2) {
				sn2 = list[grlbp[g]];
				if (sn2 == s)sn2 = list[link[grlbp[g]]];
				if (edge[sn2] <= 4) {
					x = xval[sn2];
					y = yval[sn2];
					if (xmin > x-3)xmin = x-3;
					if (xmax < x+3)xmax = x+3;
					if (ymin > y-3)ymin = y-3;
					if (ymax < y+3)ymax = y+3;
				}
			}
			if (!gflag) {
				gnew = g; 
				gflag = TRUE; 
				adlibs(s,gnew); 
				board[s] = gnew;
				upxy(s);
				++grsize[gnew]; 
				++armysize[grarmy[gnew]];
				pscr += gralprob[gnew] * cfac[grcolor[gnew]]; 
				uscan(s); 
				--grlibs[gnew]; 
				chkcon(s, gnew); 
				dellist(s, &grlbp[gnew]);
				dlflist(gnew, &nbgrp[s][c]);
				lptr = grpieces[gnew];
				while(mvnext[lptr] != -1)
					lptr = mvnext[lptr]; 
				mvnext[upptr] = -1;
				mvnext[lptr] = upptr; 
			}
			else combine(gnew, g, upptr);
		}
	} 
  
	brkconns(s);  

	if (!gflag)   /* create new group */
		makenewgroup(s, c, upptr);

	for (ptr = mvcapt[upptr]; ptr != EOL; ptr = link[ptr])
		kilgrp((group_t)list[ptr], c);  
	if (showtactics) {
		lookmove(mvs[upptr], mvcolor[upptr]);
   	}		
	learn_hash_move(msptr);
	learn_hash_stone(s, c, boardsize, msptr);
	for (ptr = mvcapt[upptr]; ptr != EOL; ptr = link[ptr]) {
		for (ptr2 = grpieces[list[ptr]]; ptr2 != -1; ptr2 = mvnext[ptr2]) {
			learn_hash_stone(mvs[ptr2], 1-c, boardsize, msptr);
		}
	}
    if (grlibs[board[s]] == 0) {  /* committed suicide */
		if (nosuicide || grsize[board[s]] == 1) {
			return(FALSE);  /* illegal suicide */
		}
		else {
			for (ptr2 = grpieces[board[s]]; ptr2 != -1; ptr2 = mvnext[ptr2]) {
				learn_hash_stone(mvs[ptr2], c, boardsize, msptr);
			}
			addlist(board[s], &mvcapt[upptr]);
			kilgrp(board[s], 1-c);
		}
	}
	if (upptr > MAXMOVE)
		return FALSE;
	if (maxgr > MAXGROUP)
		return FALSE;
	if (cntlist(&armyfreelist) < ARMIESTOUPDATE)
		return FALSE;
	if (cntlist(&eyefreelist) < EYESTOUPDATE)
		return FALSE;
	if (cntlist(&cnfreelist) < CONNSTOUPDATE)
		return FALSE;
    return TRUE;
} 



/* start a new group at s with color c */

static void makenewgroup(sqr_t s,int c,int upptr) {
	army_t army;
	group_t j;
	int i,ldtmp;
	sqr_t sn;
	list_t ptr;

#ifdef CHECK
	if (maxgr > NUMGROUPS - 2) { 
		outerror("out of groups in regular update\n");
		return;
	}
	if (grlbp[maxgr] != EOL) {
		outerr("Liberty Error!\n");
	}
	if (maxgr > msptr) {
		outerr("Update maxgr error\n");
	}

#endif 
	j = maxgr++; 
	board[s] = j;
	grsize[j] = 1; 
	gralive[j] = NEW_ALIVE;
	gralprob[j] = 0;
	grthreatened[j] = FALSE;
	grlv[j] = TRUE;
	grcolor[j] = c;
	grlibs[j] = 0;
	grcapmove[j] = grsavemove[j] = NOSQUARE;
	army = (army_t)gtflist(&armyfreelist);
	if (army == G2ERROR) {
#ifdef TEST
		clearerror();
		outerror("Out of armies in makenewgroup.");
		turnoffcplay();
#endif            
		army = 0;
		armysize[army] += 1;
	} else {
		armysize[army] = 1;
	}
	grarmy[j] = army;
	addlist(j,&armygroups[army]);
	upxy(s); 
	i = fdir[s]; 
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		sn = s + nbr[i];
		if (board[sn] == NOGROUP) {
			addlist(sn,&grlbp[j]);
			adflist(j,&nbgrp[sn][c]);
			++grlibs[j];
		} else {
			addlist(board[sn],&grnbp[j]);
			addlist(j,&grnbp[board[sn]]);
		}
	}
	if (grlibs[j] == 0 && mvcapt[upptr] != EOL &&
		grsize[list[mvcapt[upptr]]] == 1 &&
		link[mvcapt[upptr]] == EOL) {
		kosquare = mvs[grpieces[list[mvcapt[upptr]]]];
	}
	uscan(s);
	mvnext[upptr] = -1;
	grpieces[maxgr-1] = upptr;
	chkcon(s,board[s]);
	for (ptr = grnbp[j]; ptr != EOL; ptr = link[ptr]) {
		if ((gralive[list[ptr]]&31) == DEAD) {
			if (grdeadarmy[list[ptr]] != NOARMY) {
				combinearmy(grarmy[j],grdeadarmy[list[ptr]]);
			}
		}
	}
	if (grlibs[j] == 1) {
		grsavemove[j] = grcapmove[j] = list[grlbp[j]];
	}
}

  
/* remove group g from board since it is captured
 * c is color of group doing capturing
 */

  
static void kilgrp(group_t g,int c) {
   army_t army;
   sqr_t s;
   int lptr;
   pscr -= gralprob[g] * grsize[g] * cfac[grcolor[g]]; 
   if (grdeadarmy[g] != NOARMY) {
      dellist(g,&armydeadgroups[grdeadarmy[g]]);
      grdeadarmy[g] = NOARMY;
      }
   gralive[g] = 1; 
   grlv[g] = FALSE;
   addlist(mvs[grpieces[list[grnbp[g]]]],&charmy);
 
   lptr = grnbp[g];
   while(lptr != EOL) {
      dellist(g,&grnbp[list[lptr]]);
      lptr = link[lptr];
      }
   killist(&grnbp[g]);

   army = grarmy[g];
   dellist(g,&armygroups[army]);
   grarmy[g] = NOARMY;
   armysize[army] -= grsize[g];
   if (armygroups[army] == EOL) {
      make_army_free(army);
      }      
	kill_ldrflags(g);
   lptr = grpieces[g];
   while(lptr != -1) {
      s = mvs[lptr];
      DELBITS(s,1-c);
      dscan(s);
      dndiags(s,g);
      dnxy(s,1-c);	/* dont add links to this color */
      board[s] = NOGROUP;
       if (pclsnext < NUMPCLS)
	       pcls[pclsnext++] = s;
#ifdef CHECK
      if (pclsnext > NUMPCLS) {
         outerror("pcls overflow ");
         }
#endif
      adplib(s,g,1-c);
      ++numpris[c];
      adcons(mvs[lptr],g);
      lptr = mvnext[lptr];
      }
  }
  
  
/* combine group j into group i */  
  
static void combine(group_t i, group_t j, int upptr) {
	army_t army;
	group_t g2;
	sqr_t sn;
	list_t ptr,ptr2;
	int lptr,x,y,c;
	if (i == j)return;
	c = grcolor[j];
	if (G_THREATENED(j) && !G_THREATENED(i)) { /* eliminating threatened group*/
		markgroup(j);
		for (ptr = grnbp[j]; ptr != EOL; ptr = link[ptr])
			for (ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
				if (cntype[list[ptr2]] == CN_THREAT)
					addlist(list[ptr2],&cnchgd);
		}
	if ((gralive[j] & 31) == DEAD && (gralive[i] & 31) != DEAD) {         /* eliminating dead group */
		markgroup(j);
		adflist(mvs[grpieces[j]],&eyelist);
		for (lptr = grlbp[j]; lptr != EOL; lptr = link[lptr])
			if (!S_NEUTRAL(list[lptr]))
				adflist(list[lptr],&eyelist);
		for (lptr = grpieces[j]; lptr != -1; lptr = mvnext[lptr]) {
			sn = mvs[lptr];
			x = xval[sn];
			y = yval[sn];
			if (x-4 < xmin)xmin = x-4;
			if (x+4 > xmax)xmax = x+4;
			if (y-4 < ymin)ymin = y-4;
			if (y+4 > ymax)ymax = y+4;
			}
		if (grnbp[j] != EOL) {
			addlist(mvs[grpieces[list[grnbp[j]]]],&charmy);
			}
		}
	pscr = pscr + (gralprob[i] - gralprob[j])*grsize[j] *
         cfac[grcolor[j]];
	if (grdeadarmy[j] != NOARMY) {
		dellist(j,&armydeadgroups[grdeadarmy[j]]);
		grdeadarmy[j] = NOARMY;
		}
	grlibs[j] = 0;
	moveconns(j,i);
	adflist(j,&mvconn[upptr]);
	gralive[j] = 1;
	grlv[j] = FALSE;
	for (ptr = grlbp[j]; ptr != EOL; ptr = link[ptr]) {
		if (lgr[list[ptr]] == j)
			lgr[list[ptr]] = i;
		dlflist(j,&nbgrp[list[ptr]][c]);
		}
	killist(&grlbp[j]);
	army = grarmy[j];
	dellist(j,&armygroups[army]);
	grarmy[j] = NOARMY;
	armysize[army] -= grsize[j];
	if (army != grarmy[i])combinearmy(army,grarmy[i]);
	armysize[grarmy[i]] += grsize[j];
	
	lptr = grnbp[j];
	while(lptr != EOL) {
		dellist(j,&grnbp[list[lptr]]);
		lptr = link[lptr];
		}
	killist(&grnbp[j]);
	kill_ldrflags(j);
	lptr = grpieces[i];
	while(mvnext[lptr] != -1)
		lptr = mvnext[lptr];
	mvnext[lptr] = grpieces[j];
	lptr = mvnext[lptr];
	while(lptr != -1) {
		adlibs(mvs[lptr],i);
		board[mvs[lptr]] = i;
		++grsize[i]; 
		cuscan(mvs[lptr],j); 
		lptr = mvnext[lptr]; 
		}
	if ((gralive[i] & 31) == DEAD) {
		if (grnbp[i] != EOL) {
			g2 = (group_t)list[grnbp[i]];
			grdeadarmy[i] = grarmy[g2];
			addlist(i,&armydeadgroups[grarmy[g2]]);
			for (ptr2 = link[grnbp[i]]; ptr2 != EOL; ptr2 = link[ptr2]) {
				combinearmy(grarmy[list[ptr2]],grarmy[g2]);
				}
			}
		}
	}


int incn(sqr_t s, int corner, int pastedge) {
	int mid,x,y,incorner;
	if (s == NOSQUARE || s == PASS)return(FALSE);
	mid = (boardsize+1)/2;
	if (mid < 6)mid = 6;	/* for small boards */
   	x = xval[s] + 1;  /* 1 to 19 */
   	y = yval[s] + 1;
    if (corner > 1)
		y = boardsize+1-y;
	if (corner%2 == 1)
		x = boardsize+1-x;
	incorner = x <= mid+pastedge && y <= mid+pastedge && x+y <= mid+pastedge+5;
	return(incorner);
	}



void dnnumcmoves(sqr_t s)
{
	int corner;
	for (corner = 0; corner < 4; ++corner) {
		if (incn(s, corner, 1)) {
			numcmoves[corner]--;
		}
	}
}
