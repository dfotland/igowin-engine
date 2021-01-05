/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

# include "g2hd.h"
  
void dnn1(sqr_t s) { 
   int sn,i,ldtmp;
   i = fdir[s]; 
   for (ldtmp = ldir[i]; i != ldtmp; ++i ) {
      sn = s + nbr[i];
      if (ld[sn] > 1 && ld[sn] < 9 && G_COLOR(lgr[sn]) == 
          G_COLOR(lgr[s])) {     /* decn1 */ 
          --ld[sn]; 
          } 
      } 
   }
  
void upn1(sqr_t s, sqr_t s1) {
   sqr_t sn;
   int i,ldtmp; 
#ifdef CHECK
   if (ld[s] > 1 && ld[s] < 9)outerror("in upn1 bad ld"); 
#endif
   ld[s] = 0; 
   if (edge[s] <= 1)ld[s] += 2;
   if (edge[s] == 0)ld[s] += 2;
   i = fdir[s]; 
   for (ldtmp = ldir[i]; i != ldtmp; ++i ) {
      sn = s + nbr[i];
      if (sn == s1)continue; 
      if (ld[sn] == 0) {
         ld[s] += 2;
         }
      if (ld[sn] > 1 && ld[sn] < 9 && G_COLOR(lgr[sn]) == 
         G_COLOR(lgr[s])) {
            ++ld[s];
            } 
      } 
   i = fdir[s]; 
   for (ldtmp = ldir[i]; i != ldtmp; ++i ) {
      sn = s + nbr[i];
      if (sn == s1)continue; 
      if (ld[sn] > 1 && ld[sn] < 9 && G_COLOR(lgr[sn]) == 
         G_COLOR(lgr[s]))++ld[sn]; 
      } 
   }
  
void uscan(sqr_t s) { /* update ld for move at s */
	sqr_t sn,sn2;
	group_t g;
   int i; 
   int j,ldtmp,ldtm2;
   if (ld[s] > 1 && ld[s] < 9) { 
      dnn1(s);
      } 
   ld[s] = 0; 
   g = board[s];
   lgr[s] = g;
   i = fdir[s]; 
   for (ldtmp = ldir[i]; i != ldtmp; ++i) { 
      sn = s + nbr[i];
      if (ld[sn] == NOLD) {     /* new n1 */
         lgr[sn] = g; 
         ld[sn] = 2;
         if (edge[sn] <= 1)ld[sn] += 2;
         if (edge[sn] == 0)ld[sn] += 2;
         j = fdir[sn];
         for (ldtm2 = ldir[j]; j != ldtm2; ++j) { 
            sn2 = sn + nbr[j];
            if (ld[sn2] > 1 && ld[sn2] < 9 && 
               G_COLOR(lgr[sn2]) == G_COLOR(g)) {
               ++ld[sn2];
               ++ld[sn];
               }
            } 
         }
      else if (ld[sn] >= 2) {     /* old n1 */
	      ld[sn] += 2;
	      if (G_COLOR(lgr[sn]) != G_COLOR(board[s])) { 
		      dnn1(sn); 
		      ld[sn] = NEUTRALLD;
		      } 
	      }
      } 
   }
  

void dscan(sqr_t s) {
	sqr_t sn,sn2,s1;
	group_t g;
   int i,j,k; 
   int c,ldtm2,ldtmp;
   g = board[s];
   i = fdir[s]; 
   for (ldtmp = ldir[i]; i != ldtmp; ++i ) {
      sn = s + nbr[i];
      if (ld[sn] == 2) {     /* n1 going away */
         ld[sn] = NOLD; 
         tscr -= terv[sn];
         terv[sn] = 0;
         lgr[sn] = NOGROUP;
         }
      else if (ld[sn] > 1 && ld[sn] < 9) { 
         dnn1(sn);
         ld[sn] = NOLD; 
         j = fdir[sn];
         for (ldtm2 = ldir[j]; j != ldtm2; ++j ) {
            s1 = sn+nbr[j]; 
            if (s1 == s)continue;
            if (ld[s1] == 0) {              /* dont worry about making -1 */
               lgr[sn] = lgr[s1]; 
               ld[sn] = 10;
               break; 
               }
            } 
         if (ld[sn] == 10) { 
            upn1(sn,s); 
            } 
         else {            /* n1 goes away */ 
            lgr[sn] = NOGROUP; 
            tscr -= terv[sn]; 
            terv[sn] = 0; 
            } 
         }
      else if (ld[sn] == NEUTRALLD) {       /* fix the neutral lds */ 
         k = fdir[sn];
         for (ldtm2 = ldir[k]; k != ldtm2; ++k ) {
            sn2 = sn + nbr[k];
            if (sn2 == s)continue; 
            if (ld[sn2] != 0)continue; 
            if (G_COLOR(lgr[sn2]) == 
               G_COLOR(g)) { 
               lgr[sn] =  lgr[sn2]; 
               ld[sn] = NEUTRALLD; 
               break; 
               }
            else if (ld[sn] == NEUTRALLD) {
                lgr[sn] = lgr[sn2]; 
                ld[sn] = 10; 
                } 
            } 
         if (ld[sn] == 10) {   /* neutral changes to n1 */
            upn1(sn,s); 
            } 
         }
      } 
   i = fdir[s]; 
   for (ldtmp = ldir[i]; i != ldtmp; ++i ) {   /* now do square stone was on */ 
      c = lgr[s]; 
      sn = s + nbr[i];
      if (ld[sn] == 0) {
         if (ld[s] == 0) {
            ld[s] = 10;
            lgr[s] = lgr[sn]; 
            } 
         else if (G_COLOR(c) != G_COLOR(lgr[sn])) { 
            ld[s] = NEUTRALLD; 
            } 
         }
      } 
   if (ld[s] == 0) {    /* no neighbors, not an n1 */ 
      ld[s] = NOLD; 
      lgr[s] = NOGROUP;
      tscr -= terv[s];
      terv[s] = 0;
      } 
   else if (ld[s] == 10) { /* new n1 since has neighbor */ 
      upn1(s,NOSQUARE); 
      } 
   }
  
  
  
void cscan(sqr_t s, group_t g) {	/* subtract n1 and n2 for split */
	sqr_t sn;
   int i,ldtmp;
   lgr[s] = board[s]; 
   i = fdir[s]; 
   for (ldtmp = ldir[i]; i != ldtmp; ++i ) {
      sn = s + nbr[i];
      if (lgr[sn] == g)
         lgr[sn] = lgr[s];
      } 
   }
  
void cuscan(sqr_t s, group_t g) { 
	sqr_t sn;
	group_t g2;
   int i,ldtmp;
   g2 = board[s]; 
   lgr[s] = g2; 
   i = fdir[s]; 
   for (ldtmp = ldir[i]; i != ldtmp; ++i) { 
      sn = s + nbr[i];
      if (ld[sn] == 0 || ld[sn] == NEUTRALLD || ld[sn] == NOLD)continue;
  
      if (lgr[sn] != g)continue; 
      lgr[sn] = g2; 
      } 
   }
