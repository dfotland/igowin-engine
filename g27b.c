/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.  
 */

#ifdef CHECK
#include <stdio.h>
#endif
# include "g2hd.h"

 
  
int opdir[4] = { DOWN,RIGHT,LEFT,UP };   /* opposite direction for a direction */
  
  
int dirnm[52] = { /* numbers for directions  0 = -19, 1 = -1, 2 = 1, 3 = 19 */
2,3,
1,3,
2,0,
1,0,
1,2,3,
0,3,2,
0,3,1,
1,2,0,
2,3,1,0,
1,3,2,0,
2,0,1,3,
1,0,2,3,
1,2,0,3,
0,3,1,2,
0,3,2,1,
1,2,3,0  }; 
  
  
  
  
  
void upxy(sqr_t s) {     /* add stone at s to ltrxy */
	group_t g;
	sqr_t sn;
#ifdef CHECK	
	char buf[50]; 
#endif
   int i,j,offs,oldedge;
   int x,y,ldtmp,dir; 
   g = board[s];
#ifdef CHECK
   if (g == NOGROUP) {
   		sprintf(buf,"upxy error s:%d g:%d\n",s,g);
   		outerror(buf);
   		}
#endif
   if (edge[s] <= 5) {
      x = xval[s];
      y = yval[s];
      if (x < xmin)xmin = x;
      if (y < ymin)ymin = y;
      if (x > xmax)xmax = x;
      if (y > ymax)ymax = y;
      }
   i = fdir[s];
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      offs = nbr[i];
      sn = s;
      oldedge = edge[s];
      dir = opdir[dirnm[i]];
      for (j = 0; j < 4; ++j) {
         sn += offs;
         if (ld[sn] != 0 && ld[sn] != NOLD) {  /* found a liberty */
            if (sqrbrd[sn][dir] != NOSQUARE && 
              dstbrd[sn][dir] == 1 )
               brklks(sn,sqrbrd[sn][dir],s,board[sqrbrd[sn][dir]]);
            if (j == 1 )
               addlks(sn,g,NOSQUARE);  /* fix links */
            if (sqrbrd[sn][dir] != NOSQUARE && 
              (dstbrd[sn][dir] == 2  ))
               brklkgs(sn,sqrbrd[sn][dir],s,board[sqrbrd[sn][dir]]);
            if (j == 2 )
               addlkgs(sn,g,NOSQUARE);  /* fix links */
            if (sqrbrd[sn][dir] != NOSQUARE && 
              (dstbrd[sn][dir] == 3  ))
               brkolkgs(sn,sqrbrd[sn][dir],s,board[sqrbrd[sn][dir]]);
            if (j == 3 )
               addolkgs(sn,g,NOSQUARE);  /* fix links */
            }
         sqrbrd[sn][dir] = s;
         dstbrd[sn][dir] = (char)j;
         if (board[sn] != NOGROUP)break;
         if (edge[sn] <= 4) {
            x = xval[sn];
            y = yval[sn];
            if (x < xmin)xmin = x;
            if (y < ymin)ymin = y;
            if (x > xmax)xmax = x;
            if (y > ymax)ymax = y;
            }
         if (edge[sn] == 0)break;
         if (edge[sn] == 1 && oldedge == 2)break;
         oldedge = edge[sn];
         }
      } 
   }
  
  
  
  
void dnxy(sqr_t s, int noc) { 
   int end,dir,dst,ldtmp,sqr;
   int i,j,offs,oldedge,x,y;
   sqr_t sn;
    
   if (edge[s] <= 5) {
      x = xval[s];
      y = yval[s];
      if (x < xmin)xmin = x;
      if (x > xmax)xmax = x;
      if (y < ymin)ymin = y;
      if (y > ymax)ymax = y;
      }
   i = fdir[s];
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      sn = s;
      offs = nbr[i];
      dir = opdir[dirnm[i]];
      oldedge = edge[s];
      for (j = 0; j < 4; ++j) {  /* delete xy refs to stone removed */
         sn = sn + offs;
         if (ld[sn] != 0 && ld[sn] != NOLD) {
            if ((j == 1  ) && noc == NOCOLOR)
               brklks(sn,s,NOSQUARE,board[s]);
            if ((j == 2) && noc == NOCOLOR)
               brklkgs(sn,s,NOSQUARE,board[s]);
            if ((j == 3) && noc == NOCOLOR)
               brkolkgs(sn,s,NOSQUARE,board[s]);
            }
         sqrbrd[sn][dir] = NOSQUARE;
         dstbrd[sn][dir] = 0;
         if (board[sn] != NOGROUP)break;
         if (edge[sn] <= 4) {
            x = xval[sn];
            y = yval[sn];
            if (x < xmin)xmin = x;
            if (x > xmax)xmax = x;
            if (y < ymin)ymin = y;
            if (y > ymax)ymax = y;
            }
         if (edge[sn] == 0)break;
         if (edge[sn] == 1 && oldedge > 1)break;
         }
      }
  
/*   if (ltrxy[s] == 0)return;  efficiency hack? */
   if (edge[s] == 0)return;
   i = fdir[s];
   end = ldir[i];
   if (edge[s] == 1)--end; 
   for (; i < end; ++i) {       /* propagate xy past stone removed */
      dir = opdir[dirnm[i]];
      sqr = sqrbrd[s][dir];
      if (sqr == NOSQUARE)continue;
      dst = dstbrd[s][dir];
      sn = s; 
      offs = nbr[i];
      oldedge = edge[sn]; 

      for (j = dst+1; j < 4; ++j) { 
         sn += offs;
         if (ld[sn] != 0 && ld[sn] != NOLD && (j == 1) &&
             S_COLOR(sqr) != noc)
            addlks(sn,board[sqr],s);
         if (ld[sn] != 0 && ld[sn] != NOLD && (j == 2) &&
             S_COLOR(sqr) != noc)
            addlkgs(sn,board[sqr],s);
         if (ld[sn] != 0 && ld[sn] != NOLD && (j == 3) &&
             S_COLOR(sqr) != noc)
            addolkgs(sn,board[sqr],s);
         sqrbrd[sn][dir] = sqr;
         dstbrd[sn][dir] = (char)j;
         if (board[sn] != NOGROUP)break; 
         if (edge[sn] == 0)break;
         if (edge[sn] == 1 && oldedge > 1)break;
         }
      } 
  
   }
  
