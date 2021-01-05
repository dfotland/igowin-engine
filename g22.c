/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

# include "g2basic.h"

extern int ldrhash[2], zobhash[361][2];

static void addnbgrp(group_t g) {
	list_t ptr;
	int c;
	c = G_COLOR(g);
	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
#ifdef CHECK
		if (inflist(g,&nbgrp[list[ptr]][c]))
			outerror("Bad addnbgrp\n");
#endif			
		adflist(g,&nbgrp[list[ptr]][c]);
		}
	}  

/* combine 2 or more old groups into a single new group */
static group_t lcombine(group_t fnew[4],int upptr, int fn) {
  int lptr,i,c;
  list_t ptr;
  group_t g,g2;
#ifdef CHECK
  if (maxgr > NUMGROUPS - 2) {
      outerror("out of groups in lcombine\n");
      }
#endif
  c = G_COLOR(fnew[0]);
  g = maxgr++;
  grsize[g] = 0;
  grthreatened[g] = 0;
  grcapmove[g] = NOSQUARE;
  grsavemove[g] = NOSQUARE;
  G_ALIVE(g) = 1;
  for (i = fn-1; i >= 0; --i) {
      g2 = fnew[i];
      grsize[g] += grsize[g2];
      adflist(g2,&mvconn[upptr]);
      for (ptr = grnbp[g2]; ptr != EOL; ptr = link[ptr]) {
         dellist(g2,&grnbp[list[ptr]]);
         addlist(g,&grnbp[list[ptr]]);
         }
      for (ptr = grlbp[g2]; ptr != EOL; ptr = link[ptr])
      	dlflist(g2,&nbgrp[list[ptr]][c]);  /* clear out old liberty list */
      }
   g2 = fnew[0];
   G_COLOR(g) = G_COLOR(g2);
   cpylist(grlbp[g2],&grlbp[g]);
   grlibs[g] = grlibs[g2];
   cpylist(grnbp[g2],&grnbp[g]);
   grpieces[g] = grpieces[g2];
   lptr = grpieces[g];
   for (i = 1; i < fn; ++i) {
      g2 = fnew[i];
      grlibs[g] += (unsigned char)mrglist(grlbp[g2],&grlbp[g]);
      mrglist(grnbp[g2],&grnbp[g]); 
      while(mvnext[lptr] != -1) { 
         board[mvs[lptr]] = g;
         lptr = mvnext[lptr]; 
         }
      board[mvs[lptr]] = g; 
      mvnext[lptr] = grpieces[g2];
      lptr = grpieces[g2];
      }
   while(lptr != -1) {
      board[mvs[lptr]] = g;
      lptr = mvnext[lptr];
      }
   addnbgrp(g);
   return(g);
  }

  
static void lkilgrp(group_t g) {
	sqr_t s, sn;
	group_t g2;
   int lptr,i,ldtmp,c;
   c = G_COLOR(g);
   lptr = grnbp[g]; 
   while(lptr != EOL) {
      dellist(g,&grnbp[list[lptr]]);
      lptr = link[lptr];
      } 
   lptr = grpieces[g];
   killist(&grnbp[g]); 
   while(lptr != -1) {
      s = mvs[lptr];
	  ldrhash[c] ^= zobhash[s][c];
      board[s] = NOGROUP; 
      i = fdir[s];
      for (ldtmp = ldir[i]; i < ldtmp; ++i) { 
         sn = s + nbr[i]; 
         --lnbf[sn][c];
         ++lnbn[sn];
         addlist(s,&nblbp[sn]);
         g2 = board[sn];
         if (g2 == NOGROUP)continue; 
         if (g2 == g)continue; 
         if (addlist(s,&grlbp[g2])) {
         	adflist(g2,&nbgrp[s][G_COLOR(g2)]);
           ++grlibs[g2];
           }
         }
      lptr = mvnext[lptr];
      }
  }
  
static group_t lsplit(list_t ptr, group_t j) { 
   int lptr,lptr2;
   list_t ptr2, ptr3;
   group_t g2;
   for (ptr3 = grlbp[j]; ptr3 != EOL; ptr3 = link[ptr3])
   	dlflist(j,&nbgrp[list[ptr3]][G_COLOR(j)]);
   killist(&grlbp[j]); 
   lptr = grnbp[j];
   while(lptr != EOL) {
      dellist(j,&grnbp[list[lptr]]);
      lptr = link[lptr];
      }
   if (grnbp[j] != EOL)
	   killist(&grnbp[j]);
#ifdef CHECK
   if (j != maxgr-1) {
     outerror("maxgr wrong in lsplit\n");
     }
#endif
  
   --maxgr;              /* group j is gone! */
  
   lptr = mvnext[grpieces[j]];   /* unlink pieces list */
   ptr2 = ptr;
   g2 = (group_t)list[ptr2];
   addnbgrp(g2);
   ptr2 = link[ptr2];
   lptr2 = grpieces[list[ptr2]];
   while(lptr != -1) {
      board[mvs[lptr]] = g2;
      if (mvnext[lptr] == lptr2) {
         mvnext[lptr] = -1;
         lptr = lptr2;
         g2 = (group_t)list[ptr2];
         addnbgrp(g2);
         ptr2 = link[ptr2]; 
         if (ptr2 == EOL)
            lptr2 = -2; 
         else 
            lptr2 = grpieces[list[ptr2]]; 
         }
      else lptr = mvnext[lptr]; 
      } 
  
  
   ptr2 = ptr;
   while(ptr2 != EOL) {   /* fix neighbors */
      g2 = (group_t)list[ptr2];
      lptr = grnbp[g2]; 
      ptr2 = link[ptr2];
      while(lptr != EOL) { 
         addlist(g2,&grnbp[list[lptr]]);
         lptr = link[lptr];
         }
      }

   return((group_t)list[ptr]);
   }
  
  
static void lresurrect(group_t g) {     /* bring back group g */
   int lptr; /* mvs ptr, not list ptr */
   int i,ldtmp,c;
   sqr_t s,sn;    
   group_t g2;
   c = G_COLOR(g);
   lptr = grpieces[g];
   while(lptr != -1) {
       s = mvs[lptr];
 	   ldrhash[c] ^= zobhash[s][c];
       board[s] = g;
       i = fdir[s];
       for (ldtmp = ldir[i]; i < ldtmp; ++i ) {
          sn = s + nbr[i];
          ++lnbf[sn][c];
          --lnbn[sn];
          dellist(s,&nblbp[sn]);
          g2 = board[sn];
          if (g2 == NOGROUP)continue;
          if (g2 == g)continue;
          if (dellist(s,&grlbp[g2])) {
             grlibs[g2]--;
             dlflist(g2,&nbgrp[s][G_COLOR(g2)]);
             }
          addlist(g2,&grnbp[g]);
          addlist(g,&grnbp[g2]);
          } 
       lptr = mvnext[lptr]; 
       }
   }
  
void ldndate(int dnptr)
{
	group_t tmp;
	int i, lptr, c, splflag, ldtmp;
   list_t ptr;
   sqr_t s,sn;
   group_t g,g2;
   kosquare = kosave[dnptr];
   s = mvs[dnptr];
   if (s == PASS) {  /* pass */
#ifdef TEST
	   if (showtactics > 1)
		   fixsd(dnptr,FALSE);
#endif		   
      return;
      }
   g = board[s];
   c = G_COLOR(g);
   ldrhash[c] ^= zobhash[s][c];
   splflag = FALSE;
   board[s] = NOGROUP;
   if (mvconn[dnptr] != EOL) {
      g = lsplit(mvconn[dnptr],g);    /* bring back combined groups */
      splflag = TRUE;
      }
   for (ptr = mvcapt[dnptr]; ptr != EOL; ptr = link[ptr]) {
         g2 = (group_t)list[ptr]; /* bring back killed groups */
             lresurrect(g2);  /* resurrect enemy group */
         }
  
   if (!splflag) {        /* remove piece from group */
      grsize[g]--;
      if (grsize[g] == 0) {  /* only one piece in group */
         --maxgr;           /* delete group */
#ifdef CHECK
         if (maxgr != g) {
             outerror("maxgr is wrong in ldndate\n");
             }
#endif
		for (lptr = grlbp[g]; lptr != EOL; lptr = link[lptr])
			dlflist(g,&nbgrp[list[lptr]][c]);
         killist(&grlbp[g]);     /* return liberties to free list */
         lptr = grnbp[g];
         while(lptr != EOL) {
            dellist(g,&grnbp[list[lptr]]);
            lptr = link[lptr];
            }
         if (grnbp[g] != EOL)
		 killist(&grnbp[g]); /* return neighbors to free list */
         }
      else {                  /* take piece away from group */
         grpieces[g] = mvnext[grpieces[g]]; 
         ptr = lbply[dnptr];
         while(ptr != EOL) { 
            dellist(list[ptr],&grlbp[g]); 
            --grlibs[g];
            dlflist(g,&nbgrp[list[ptr]][c]);
            ptr = link[ptr];
            } 
         killist(&lbply[dnptr]); 
         ptr = nbply[dnptr];
         while(ptr != EOL) { 
            tmp = (group_t)list[ptr];
            dellist(tmp,&grnbp[g]); 
            dellist(g,&grnbp[tmp]); 
            ptr = link[ptr];
            } 
         if (nbply[dnptr] != EOL)
		 killist(&nbply[dnptr]); 
         }
      if (edge[s] < 2) {
         i = fdir[s]; 
         for (ldtmp = ldir[i]; i < ldtmp; ++i) {
            sn = s + nbr[i];
            --lnbf[sn][c];
            ++lnbn[sn]; 
            addlist(s,&nblbp[sn]);
            g = board[sn];
            if (g == NOGROUP)continue;
            if (addlist(s,&grlbp[g])) {
              adflist(g,&nbgrp[s][G_COLOR(g)]);
              ++grlibs[g];
              }
            }
         }
      else{
         --lnbf[s+1][c];
         ++lnbn[s+1];
         addlist(s,&nblbp[s+1]);
         --lnbf[s-1][c];
         ++lnbn[s-1];
         addlist(s,&nblbp[s-1]);
         --lnbf[s+boardsize][c];
         ++lnbn[s+boardsize];
         addlist(s,&nblbp[s+boardsize]);
         --lnbf[s-boardsize][c];
         ++lnbn[s-boardsize];
         addlist(s,&nblbp[s-boardsize]);
         g = board[s+1];
         if (g != NOGROUP)
            if (addlist(s,&grlbp[g])) {
				adflist(g,&nbgrp[s][G_COLOR(g)]);
				++grlibs[g];
				}
         g = board[s-1];
         if (g != NOGROUP )
            if (addlist(s,&grlbp[g])) {
				adflist(g,&nbgrp[s][G_COLOR(g)]);
				++grlibs[g];
				}
         g = board[s+boardsize];
         if (g != NOGROUP )
            if (addlist(s,&grlbp[g])) {
				adflist(g,&nbgrp[s][G_COLOR(g)]);
				++grlibs[g];
				}
         g = board[s-boardsize];
         if (g != NOGROUP )
            if (addlist(s,&grlbp[g])) {
				adflist(g,&nbgrp[s][G_COLOR(g)]);
				++grlibs[g];
				}
         }
      }
   else {
      if (edge[s] < 2) {
         i = fdir[s];
         for (ldtmp = ldir[i]; i < ldtmp; ++i) {
            sn = s + nbr[i];
            --lnbf[sn][c];
            ++lnbn[sn];
            addlist(s,&nblbp[sn]);
            g = board[sn];
            if (G_COLOR(g) != 1-c)continue;
            if (addlist(s,&grlbp[g])) {
				adflist(g,&nbgrp[s][1-c]);
              	++grlibs[g];
              	}
            }
         }
      else{
         --lnbf[s+1][c];
         ++lnbn[s+1];
         addlist(s,&nblbp[s+1]);
         --lnbf[s-1][c];
         ++lnbn[s-1];
         addlist(s,&nblbp[s-1]);
         --lnbf[s+boardsize][c];
         ++lnbn[s+boardsize];
         addlist(s,&nblbp[s+boardsize]);
         --lnbf[s-boardsize][c];
         ++lnbn[s-boardsize];
         addlist(s,&nblbp[s-boardsize]);
         g = board[s+1];
         if (G_COLOR(g) == 1-c)
            if (addlist(s,&grlbp[g])) {
				adflist(g,&nbgrp[s][1-c]);
				++grlibs[g];
				}
         g = board[s-1];
         if (G_COLOR(g) == 1-c)
            if (addlist(s,&grlbp[g])) {
				adflist(g,&nbgrp[s][1-c]);
				++grlibs[g];
				}
         g = board[s+boardsize];
         if (G_COLOR(g) == 1-c)
            if (addlist(s,&grlbp[g])) {
				adflist(g,&nbgrp[s][1-c]);
				++grlibs[g];
				}
         g = board[s-boardsize];
         if (G_COLOR(g) == 1-c)
            if (addlist(s,&grlbp[g])) {
				adflist(g,&nbgrp[s][1-c]);
				++grlibs[g];
				}
         }
      }
#ifdef TEST      
   if (showtactics > 1)
	   fixsd(dnptr,FALSE);
#endif	   
   }
  
  
/* update ladder flags for point s, into ldrno. */  

void addldrflag(sqr_t s, listval_t ldrno) {
	int sn,i,ldtmp,g2;
	if (ldrno == NOGROUP)return;
	if (addlist(ldrno,&ldrflag[s])) {
       		adflist(s,&grldr[ldrno]);
       		}
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		sn = s + nbr[i];
		g2 = board[sn];
		if (g2 != NOGROUP) {
			if (addlist(ldrno,&ldrflag[mvs[grpieces[g2]]]))
			   adflist(mvs[grpieces[g2]],&grldr[ldrno]);
			}
		}
	}

  
/* set ladder flags at spots where moves will affect this ladder
 * ladder flags have places plays were made in a ladder and places where
 * first stone is in adjacent groups 
 * (should be only adjacent groups that were on board when ladder started)
 * if ldrno is NOGROUP, don't set flags
 */

void upldrflags(int upptr, listval_t ldrno) {
	sqr_t s;
	s = mvs[upptr];
	if (s == PASS)return;
#ifdef CHECK
	if (s > boardsquare)
		outerror("upldrflags bad s\n");
	if (ldrno >= NUMGROUPS+NUMCONNS+NUMEYERECS)
		outerror("bad ldrno in upldrflags\n");
#endif
	if (ldrno == NOGROUP)return;
	addldrflag(s,ldrno);
	}

  
  
/* update data structures needed for tactical analyzer for move in 
 * move stack at
 * upptr.  
 * data structures updated are: kosquare, board[], lnbn[], lnbf[][], 
 * grpieces[], grsize[], grcolor[], grlibs[], grlbp[], nblbp[], nbgrp[][]
 *
 * ld[], lgr[], ltr1[], ltr2[], ltrgd[], army, eye, and connection data
 * structures are not modified
 *
 * return TRUE if move is legal
 */

int lupdate(int upptr)
{
    int i, c, l, m;
	sqr_t lnew[4];
    group_t g, g2, gnew, grp[4], fnew[4];
    int ldtmp, fn = 0;
    sqr_t s, sn;
    int flag, j;
#ifdef CHECK
	char buf[10];
#endif    
	++numnodes;
    if (mvconn[upptr] != EOL)
		killist(&mvconn[upptr]);
    if (mvcapt[upptr] != EOL)
		killist(&mvcapt[upptr]);
    kosave[upptr] = kosquare;
    kosquare = NOSQUARE;
    s = mvs[upptr];
    if (s == PASS) {  /* pass */
#ifdef TEST    
		if (showtactics > 1) {
			fixlm(mvs[upptr], mvcolor[upptr]);
		}
#endif	       
		return TRUE;
	}
#ifdef CHECK
    if (s < firstsquare || s >= lastsquare || board[s] != NOGROUP) {
		outerror("lupdate for non empty square\n");
		outerror(ssqr(s, buf));
		waitaction();
	}
#endif
  
	c = mvcolor[upptr];
	ldrhash[c] ^= zobhash[s][c];
	l = 0;
	m = 0;
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {   /* look at neighbors */
		sn = s + nbr[i];
		++lnbf[sn][c];
		--lnbn[sn];
		dellist(s, &nblbp[sn]);
		g = board[sn];
		if (g == NOGROUP) {   /* empty nbr, add a liberty */
			lnew[m++] = sn;
		} else if (G_COLOR(g) != c) {      /* unfriendly neighbor */
			if (dellist(s, &grlbp[g])) {
	         	dlflist(g,&nbgrp[s][1-c]);
				if (--grlibs[g] == 0) {  /* killed enemy group */
					lnew[m++] = sn;
					adflist(g, &mvcapt[upptr]);
					lkilgrp(g);
				} else {
					grp[l++] = g; 
				}
			}
		} else {
			flag = TRUE;
			for (j = 0; j < fn; ++j) {
				if (fnew[j] == g) {
					flag = FALSE;
					break;
				}
			}
			if (flag) {
				fnew[fn++] = g;      /* friendly neighbor */
			}
		}
	} 
    if (fn == 0) { 		/* create a new group */
		if (maxgr > NUMGROUPS - 2) { 
#ifdef CHECK
			outerror("out of groups in ladder update\n");
#endif
			return(FALSE); /* probably need more work here */
		} 
         gnew = maxgr++;
         board[s] = gnew; 
         grsize[gnew] = 1;
         grcolor[gnew] = (char)c;
         grlibs[gnew] = m;
		 gralive[gnew] = 1;
		 grthreatened[gnew] = FALSE;
		 grcapmove[gnew] = NOSQUARE;
		 grsavemove[gnew] = NOSQUARE;
#ifdef CHECK
         if (grnbp[gnew] != EOL)
			outerror("nonempty grnbp in lupdate new group \n");
#endif
         for (i = 0; i < l; ++i) {
            g = grp[i];
            addlist(g,&grnbp[gnew]);
            addlist(gnew,&grnbp[g]);
            }
         for (i = 0; i < m; ++i) {
			 if (addlist(lnew[i],&grlbp[gnew])) {
				adflist(gnew,&nbgrp[lnew[i]][c]);
				}
		 	}
         mvnext[upptr] = -1;
         grpieces[gnew] = upptr;
         if (grlibs[gnew] == 1 && mvcapt[upptr] != EOL &&
            grsize[list[mvcapt[upptr]]] == 1 &&
            link[mvcapt[upptr]] == EOL) {
            kosquare = list[grlbp[gnew]];
			grthreatened[gnew] = 1;
			grsavemove[gnew] = kosquare;
			grcapmove[gnew] = kosquare;
			}
		 else if (grlibs[gnew] == 1) {
			grthreatened[gnew] = 2;
			grsavemove[gnew] = grcapmove[gnew] = list[grlbp[gnew]];
			}
         }
   else {
      if (fn == 1) { /* add stone to group */
         gnew = fnew[0];
         for (i = 0; i < l; ++i) {
            g2 = grp[i];
            if (addlist(g2,&grnbp[gnew])) {
               addlist(gnew,&grnbp[g2]);
               adflist(g2,&nbply[upptr]);
               }
            }
         for (i = 0; i < m; ++i)
		   if (addlist(lnew[i],&grlbp[gnew])) {
            ++grlibs[gnew];
            adflist(gnew,&nbgrp[lnew[i]][c]);
            adflist(lnew[i],&lbply[upptr]);
            }
         }
      else{            /* combine groups */
         gnew = lcombine(fnew,upptr,fn);
         for (i = 0; i < l; ++i) {
            g2 = grp[i];
            if (addlist(g2,&grnbp[gnew])) {
               addlist(gnew,&grnbp[g2]);
               }
            }
         for (i = 0; i < m; ++i)if (addlist(lnew[i],&grlbp[gnew])) {
            ++grlibs[gnew];
			adflist(gnew,&nbgrp[lnew[i]][c]);
         	}
         }
      board[s] = gnew;
      ++grsize[gnew];
      mvnext[upptr] = grpieces[gnew];
      grpieces[gnew] = upptr;
      --grlibs[gnew];
      dellist(s,&grlbp[gnew]);
      dlflist(gnew,&nbgrp[s][c]);
      }
#ifdef TEST
    if (showtactics > 1)
	    fixlm(mvs[upptr], mvcolor[upptr]);
#endif	    
    if (grlibs[gnew] == 0)return(FALSE);  /* illegal suicide */
	if (maxgr >= NUMGROUPS-2)
		return FALSE;
	if (upptr >= MAXLADDER)
		return FALSE;
    return TRUE;
}
