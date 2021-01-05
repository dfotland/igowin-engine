/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#ifdef CHECK
# include <stdio.h>
#endif

# include "g2hd.h"

int brconn(group_t,sqr_t);
static void adddiag(group_t, group_t, sqr_t);
static void brddconn(group_t g,sqr_t sn);

  
void dncons(sqr_t s, group_t g) {    /* delete cons when stone removed */
	int i,cflag,ldtmp,j,ldtm2,c;
	list_t tmplist,tmplist2,tmplist3;
	sqr_t sn,sn2;
	c = G_COLOR(g);
	i = fdir[s];
	for (ldtmp = ldir[i]; i != ldtmp; ++i) {
		sn = s + nbr[i];
		if (board[sn] != NOGROUP)continue;
		cflag = TRUE;
		j = fdir[sn];
		for (ldtm2 = ldir[j]; j != ldtm2; ++j ) {
			sn2 = sn + nbr[j];
			if (sn2 == s)continue;
			if (board[sn2] == g) {
				cflag = FALSE;
				continue;
				}
			}
  
		if (cflag && cnbrd[sn] != EOL) 
			brconn(g,sn);  /* true if deleted protected connection */
		if (cflag) {
			tmplist = EOL;
			tmplist2 = EOL;
			tmplist3 = EOL;
			for (j = 0; j < 4; ++j) {
				if (S_COLOR(sqrbrd[sn][j]) == c &&
					(dstbrd[sn][j] == 1 ) &&
					addlist(board[sqrbrd[sn][j]],&tmplist))
					realbrklink(g,board[sqrbrd[sn][j]],sqrbrd[sn][j],sn);
				if (S_COLOR(sqrbrd[sn][j]) == c &&
					(dstbrd[sn][j] == 2 ) &&
					addlist(board[sqrbrd[sn][j]],&tmplist2))
					realbrklkg(g,board[sqrbrd[sn][j]],sqrbrd[sn][j],sn);
				if (S_COLOR(sqrbrd[sn][j]) == c &&
					(dstbrd[sn][j] == 3 ) &&
					addlist(board[sqrbrd[sn][j]],&tmplist3))
					realbrkolkg(g,board[sqrbrd[sn][j]],sqrbrd[sn][j],sn);
				}
			killist(&tmplist);
			killist(&tmplist2);
			killist(&tmplist3);
			}
		} 
	dndiags(s,g);
	}
	   
	   
void dndiags(sqr_t s,group_t g) {
	int i,ldtmp,cflag,j,ldtm2;
	sqr_t sn, sn2;
	i = fdir[s];
	for (ldtmp = ldiag[i]; i != ldtmp; ++i) {
		sn = s + diags[i];
		if (board[sn] != NOGROUP || lnbn[sn] < 2) 
			continue;  /* no connection here */
		cflag = TRUE;
		j = fdir[sn];
		for (ldtm2 = ldiag[j]; j != ldtm2; ++j ) {
			sn2 = sn + diags[j];
			if (sn2 == s)continue;
			if (board[sn2] == g)
				cflag = FALSE;
			}
		if (cflag)
			brddconn(g,sn);
		}
	}
  

/* break all double diagonal oonnection at sn from g */    
                         
static void brddconn(group_t g, sqr_t sn) {
	list_t ptr, tmplist;
	conn_t i;
#ifdef CHECK
	char buf[80];
	if (sn >= boardsquare) {
		sprintf(buf,"brddconn called sn is %d\n",sn);
		outerror(buf);
		}
#endif
	tmplist = EOL;
	cpylist(ddbrd[sn],&tmplist);
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		i = list[ptr];
		if (cngr1[i] == g || 
			cngr2[i] == g) {
			--cnddnum[i];
			dellist(sn,&cnddptr[i]);
			dellist(i,&ddbrd[sn]);
			if (cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0 && cnollnum[i] == 0 && cnddnum[i] == 0) {
				delconnrec(i);
				}
			else
            	addlist(i,&cnchgd);
			}
		}
	killist(&tmplist);
	}
  
                        
void brkconns(sqr_t s) {   /* break all connections through s */
   conn_t i;
   list_t ptr,ptr2;
   list_t tmplist = EOL;
#ifdef CHECK
	char buf[80];
   if (s >= boardsquare) {
      sprintf(buf,"\nbrkconns called s is %d\n",s);
      outerror(buf);
      }
#endif
   for (ptr = cnbrd[s]; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      dellist(s,&cnptr[i]);
      cncnum[i]--;
      if (cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0 && cnollnum[i] == 0 && cnddnum[i] == 0) {
	       delconnrec(i);
	       }
      else if (cncnum[i] == 1)
         addlist(i,&cnchgd);
      }
   for (ptr = lkbrd[s]; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      dellist(s,&cnlkptr[i]);
      cnlknum[i]--;
      if (cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0 && cnollnum[i] == 0 && cnddnum[i] == 0) {
	       delconnrec(i);
	       }
      }
   for (ptr = llbrd[s]; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      dellist(s,&cnllptr[i]);
      cnllnum[i]--;
      if (cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0 && cnollnum[i] == 0 && cnddnum[i] == 0) {
	       delconnrec(i);
	       }
      }
   for (ptr = ollbrd[s]; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      dellist(s,&cnollptr[i]);
      cnollnum[i]--;
      if (cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0 && cnollnum[i] == 0 && cnddnum[i] == 0) {
	       delconnrec(i);
	       }
      }
   for (ptr = ddbrd[s]; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      dellist(s,&cnddptr[i]);
      cnddnum[i]--;
      if (cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0 && cnollnum[i] == 0 && cnddnum[i] == 0) {
	       delconnrec(i);
	       }
      }
	for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2])
    	if (lnbn[list[ptr2]] == 1 && ddbrd[list[ptr2]] != EOL) {
    		cpylist(ddbrd[list[ptr2]],&tmplist);
			for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
				i = list[ptr];
				dellist(list[ptr2],&cnddptr[i]);
				dellist(i,&ddbrd[list[ptr2]]);
				cnddnum[i]--;
				if (cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0 && cnollnum[i] == 0 && cnddnum[i] == 0) {
					delconnrec(i);
					}
				}                 
			killist(&tmplist);
			}
	killist(&llbrd[s]);
	killist(&cnbrd[s]);
	killist(&lkbrd[s]);
	killist(&ollbrd[s]);
	killist(&ddbrd[s]);
	}


  
  
  
int brconn(group_t g, sqr_t s) {  /* break all 1 pt connections to g through s */
	list_t ptr;
   conn_t i;
   int flag;
   list_t tmplist;
#ifdef CHECK
	char buf[80];
   if (s >= boardsquare) {
      sprintf(buf,"brconn called s is %d\n",s);
      outerror(buf);
      }
#endif
   flag = FALSE;
   tmplist = EOL;
   cpylist(cnbrd[s],&tmplist);
   for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      if (cngr1[i] == g || cngr2[i] == g) {
         --cncnum[i];
         dellist(s,&cnptr[i]);
         dellist(i,&cnbrd[s]);
         if (cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0 && cnollnum[i] == 0 && cnddnum[i] == 0) {
	       delconnrec(i);
	       }
         else if (cncnum[i] == 1)
            addlist(i,&cnchgd);
         }
      }
   killist(&tmplist);
   return(flag);
   }


  
void moveconns(group_t g1, group_t g2) {   /* move connections from g1 to g2. g1 ends up unconnected*/
	group_t g3,g4;
	list_t ptr,ptr2;
    conn_t i, j=0;
	int p2;
#ifdef CHECK
	char buf[80];
   if (g2 > NUMGROUPS-2)
   {
   	sprintf(buf,"bad group %d in moveconn",g2);
   	outerror(buf);
   	}
#endif
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      g3 = cngr1[i];
      if (g3 == g1)g3 = cngr2[i];
      if (g3 == g2) {   /* would be conn from group to itself */
	 dellist(i,&grcnp[g2]);
	 if (grldr[(int)NUMGROUPS+i] != EOL)kill_ldrflags((listval_t)(NUMGROUPS+i));
         for (p2 = cnptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&cnbrd[list[p2]]);
            }
         for (p2 = cnlkptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&lkbrd[list[p2]]);
            }
         for (p2 = cnllptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&llbrd[list[p2]]);
            }
         for (p2 = cnollptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&ollbrd[list[p2]]);
            }
         for (p2 = cnddptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&ddbrd[list[p2]]);
            }
         cncnum[i] = 0;
         cnlknum[i] = 0;
		cnllnum[i] = 0;
		cnollnum[i] = 0;
	 	cnddnum[i] = 0;
         killist(&cnptr[i]);
         killist(&cnlkptr[i]);
	 killist(&cnllptr[i]);
	 killist(&cnollptr[i]);
	 killist(&cnddptr[i]);
	 adflist(i,&cnfreelist);
         continue;
         }
      g4 = NOGROUP;
      for (ptr2 = grcnp[g2]; ptr2 != EOL; ptr2 = link[ptr2]) {
         /* see if g2 already has connection to same group */
         j = list[ptr2];
         g4 = cngr1[j];
         if (g4 == g2)g4 = cngr2[j];
         if (g4 == g3)break;
         }
      if (g4 != g3) {  /* move connection as whole */
         if (cngr1[i] == g1)cngr1[i] = g2;
         else cngr2[i] = g2;
         addlist(i,&grcnp[g2]);
         }
      else { /* delete conn i. add to conn j */
         if (PROTCONN(i) && !PROTCONN(j)) {
		addlist(mvs[grpieces[cngr1[i]]],&charmy);
		addlist(mvs[grpieces[cngr2[i]]],&charmy);
		}
         dellist(i,&grcnp[g3]);
         addlist(j,&grcnp[g3]);
	 if (grldr[(int)NUMGROUPS+i] != EOL)kill_ldrflags((listval_t)(NUMGROUPS+i));
	 adflist(i,&cnfreelist);
         
         for (p2 = cnptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&cnbrd[list[p2]]);
            if (addlist(j,&cnbrd[list[p2]])) {
               addlist(list[p2],&cnptr[j]);
               ++cncnum[j];
               }
            }
         cncnum[i] = 0;
         killist(&cnptr[i]);

         for (p2 = cnlkptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&lkbrd[list[p2]]);
            if (addlist(j,&lkbrd[list[p2]])) {
               addlist(list[p2],&cnlkptr[j]);
               ++cnlknum[j];
               }
            }
         cnlknum[i] = 0;
         killist(&cnlkptr[i]);

         for (p2 = cnllptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&llbrd[list[p2]]);
            if (addlist(j,&llbrd[list[p2]])) {
               addlist(list[p2],&cnllptr[j]);
               ++cnllnum[j];
               }
            }
         cnllnum[i] = 0;
         killist(&cnllptr[i]);

         for (p2 = cnollptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&ollbrd[list[p2]]);
            if (addlist(j,&ollbrd[list[p2]])) {
               addlist(list[p2],&cnollptr[j]);
               ++cnollnum[j];
               }
            }
         cnollnum[i] = 0;
         killist(&cnollptr[i]);

         for (p2 = cnddptr[i]; p2 != EOL; p2 = link[p2]) {
            dellist(i,&ddbrd[list[p2]]);
            if (addlist(j,&ddbrd[list[p2]])) {
               addlist(list[p2],&cnddptr[j]);
               ++cnddnum[j];
               }
            }
         cnddnum[i] = 0;
         killist(&cnddptr[i]);

         }
      }
   killist(&grcnp[g1]);
   }
  
  
void adcons(sqr_t s, group_t g) {       /* add connections through an empty point (take back or die) */
										/* but not to group g */
   int i,sn,j,k,ldtmp,c,dst;
   group_t n[4];
   list_t ptr;
   k = 0;
   i = fdir[s];
   for (ldtmp = ldir[i]; i != ldtmp; ++i) {
      sn = s + nbr[i];
      if (board[sn] != NOGROUP && board[sn] != g)
         n[k++] = board[sn];
      }
   for (i = 0; i < k-1; ++i)
      for (j = i + 1; j != k; ++j)
         if (G_COLOR(n[i]) == G_COLOR(n[j]))
            addconn(n[i],n[j],s);

   for (i = 0; i < 4; ++i) {
      sn = sqrbrd[s][i];
      dst = dstbrd[s][i];
      if (sn != NOSQUARE && (dst == 1 )) {
         c = S_COLOR(sn);
         for (j = 0; j < k; ++j)
            if (G_COLOR(n[j]) == c)
               addlink(n[j],board[sn],s);
         }
      if (sn != NOSQUARE && (dst == 2 )) {
         c = S_COLOR(sn);
         for (j = 0; j < k; ++j)
            if (G_COLOR(n[j]) == c)
               addlkg(n[j],board[sn],s);
         }
      if (sn != NOSQUARE && (dst == 3 )) {
         c = S_COLOR(sn);
         for (j = 0; j < k; ++j)
            if (G_COLOR(n[j]) == c)
               addolkg(n[j],board[sn],s);
         }
      }
   
	if (lnbn[s] >= 2) {
   		k = 0;
		i = fdir[s];
		for (ldtmp = ldiag[i]; i != ldtmp; ++i) {
			sn = s + diags[i];
			if (board[sn] != NOGROUP && board[sn] != g)
				n[k++] = board[sn];
			}
		for (i = 0; i < k-1; ++i)
			for (j = i + 1; j != k; ++j)
				if (G_COLOR(n[i]) == G_COLOR(n[j]))
					adddiag(n[i],n[j],s);
		}

	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
		if (lnbn[list[ptr]] < 2)continue;
		k = 0;
		i = fdir[list[ptr]];
		for (ldtmp = ldiag[i]; i != ldtmp; ++i) {
			sn = list[ptr] + diags[i];
			if (board[sn] != NOGROUP && board[sn] != g)
				n[k++] = board[sn];
			}
		for (i = 0; i < k-1; ++i)
			for (j = i + 1; j != k; ++j)
			if (G_COLOR(n[i]) == G_COLOR(n[j]))
				adddiag(n[i],n[j],list[ptr]);
		}	

   }
  
void addconn(group_t g1, group_t g2, sqr_t s) {  /* add a connection from g1 to g2 at s */
	list_t ptr;
    conn_t i;
#ifdef CHECK
	char buf[80];
#endif
   if (g1 == g2)return;
#ifdef CHECK
   if ( g1 > NUMGROUPS-2 || g2 > NUMGROUPS-2) {
      sprintf(buf,"bad groups in addconn %d %d",g1,g2);
      outerror(buf);
   }
#endif
   ptr = grcnp[g1];
   while(ptr != EOL) {
      i = list[ptr];
      ptr = link[ptr];
      if (cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1) {
         if (addlist(s,&cnptr[i])) {
            ++cncnum[i];
            if (cncnum[i] < 3)
               addlist(i,&cnchgd);
            }
         addlist(i,&cnbrd[s]);
         return;
         }
      }
   i = gtflist(&cnfreelist);
#ifdef CHECK   
   if (i == G2ERROR) {
	   outerror("Ran out of connection records in addconn!\n");
	   return;
	   }
#endif	   
   addlist(i,&cnchgd);
   addlist(s,&cnptr[i]);
   addlist(i,&cnbrd[s]);
   addlist(i,&grcnp[g1]);
   addlist(i,&grcnp[g2]);
   cngr1[i] = g1;
   cngr2[i] = g2;
   cncnum[i] = 1;
   cnprot[i] = CANT_CONNECT;
   if (grarmy[g1] == grarmy[g2]) {
      cnprot[i] = AJI_CONNECT;
      }
   }
 
/* add diag connections for a new stone at s */ 
void diagconn(sqr_t s) 
 {
	int i, ldtmp, j, ldtm2;
	sqr_t sn;
	i = fdir[s];
	for (ldtmp = ldiag[i]; i < ldtmp; ++i) {
		sn = s + diags[i];
		if (board[sn] != NOGROUP)continue;
		if (lnbn[sn] < 2)continue;
		j = fdir[sn];
		for (ldtm2 = ldiag[j]; j < ldtm2; ++j)
			if (S_COLOR(sn + diags[j]) == S_COLOR(s) &&
				board[sn + diags[j]] != board[s])
				adddiag(board[sn+diags[j]], board[s], sn);
		}
}
  
void chkcon(sqr_t s, group_t g) {  /* add connections to g at libs next to new stone at s */
	int i,j,cflag,gflag,c;
	int k,l,ldtmp,ldtm2;
	sqr_t sn,sn2;
	group_t grp[4];
	c = G_COLOR(g);
	i = fdir[s];
	for (ldtmp = ldir[i]; i != ldtmp; ++i) {
		sn = s + nbr[i];
		if (board[sn] != NOGROUP)continue;
		for (j = 0; j < 4; ++j) {
			if (S_COLOR(sqrbrd[sn][j]) == c &&
				(dstbrd[sn][j] == 1  ))
				addlink(g,board[sqrbrd[sn][j]],sn);
			if (S_COLOR(sqrbrd[sn][j]) == c &&
				(dstbrd[sn][j] == 2  ))
				addlkg(g,board[sqrbrd[sn][j]],sn);		
			if (S_COLOR(sqrbrd[sn][j]) == c &&
				(dstbrd[sn][j] == 3  ))
				addolkg(g,board[sqrbrd[sn][j]],sn);		
			}
		cflag = FALSE;
		k = 0;
		j = fdir[sn];
		for (ldtm2 = ldir[j]; j != ldtm2; ++j) {
			sn2 = sn + nbr[j];
			if (sn2 == s)continue;
			if (board[sn2] == NOGROUP)continue;
			gflag = FALSE;
			for (l = 0; l != k; ++l)
				if (grp[l] == board[sn2])gflag = TRUE;
			if (gflag)continue;
			if (board[sn2] != g) {
				grp[k++] = board[sn2];
				}
			else cflag = TRUE;
			}
		if (!cflag)
			for (j = 0; j != k; ++j)
				if (G_COLOR(grp[j]) == c)addconn(grp[j],g,sn);
		}
	diagconn(s);
	}
  

  

void addlks(sqr_t s, group_t g, sqr_t nos) {	/* addlinks from g to groups adjacent to s */
				   /* dont add links to nos */
	group_t g2;
	sqr_t sn;
   int c1,i,ldtmp;
   c1 = G_COLOR(g);
   i = fdir[s];
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      sn = s + nbr[i];
      if (sn == nos)continue;
      g2 = board[sn];
      if (g2 == g || G_COLOR(g2) != c1)continue;
      addlink(g,g2,s);
      }
   }

void addlkgs(sqr_t s, group_t g, sqr_t nos) {	/* addlkgs from g to groups adjacent to s */
			  /* dont add links to nos */
	group_t g2;
	sqr_t sn;
   int c1,i,ldtmp;
   c1 = G_COLOR(g);
   i = fdir[s];
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      sn = s + nbr[i];
      if (sn == nos)continue;
      g2 = board[sn];
      if (g2 == g || G_COLOR(g2) != c1)continue;
      addlkg(g,g2,s);
      }
   }

void addolkgs(sqr_t s, group_t g, sqr_t nos) {	/* addlkgs from g to groups adjacent to s */
			  /* dont add links to nos */
	group_t g2;
	sqr_t sn;
   int c1,i,ldtmp;
   c1 = G_COLOR(g);
   i = fdir[s];
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      sn = s + nbr[i];
      if (sn == nos)continue;
      g2 = board[sn];
      if (g2 == g || G_COLOR(g2) != c1)continue;
      addolkg(g,g2,s);
      }
   }

void brklks(sqr_t s, sqr_t sqr, sqr_t s2, group_t g)	/* s is empty sqr has stone on it */
{
		  /* break links from s to sqr. ignore group at s2 */
			/* g is group at sqr to break link to */
	group_t g2;
	int c2, i, ldtmp;
	list_t tmplist,ptr;
	tmplist = EOL;
	c2 = G_COLOR(g);
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		if (s+nbr[i] == s2)
			continue;
		g2 = board[s+nbr[i]];
		if (G_COLOR(g2) == c2)
			addlist(g2,&tmplist);
	}
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr])
		brklink((group_t)list[ptr],g,sqr,s);
	killist(&tmplist);
}


void brklkgs(sqr_t s, sqr_t sqr, sqr_t s2, group_t g) {	/* s is empty sqr has stone on it */
		  /* break links from s to sqr. ignore group at s2 */
			/* g is group at sqr to break link to */
	group_t g2;
	int c2,i,ldtmp;
	list_t tmplist,ptr;
   tmplist = EOL;
   c2 = G_COLOR(g);
   i = fdir[s];
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      if (s+nbr[i] == s2)continue;
      g2 = board[s+nbr[i]];
      if (G_COLOR(g2) == c2)
         addlist(g2,&tmplist);
      }
   for (ptr = tmplist; ptr != EOL; ptr = link[ptr])
      brklkg((group_t)list[ptr],g,sqr,s);
   killist(&tmplist);
   }

void brkolkgs(sqr_t s, sqr_t sqr, sqr_t s2, group_t g) {	/* s is empty sqr has stone on it */
		  /* break links from s to sqr. ignore group at s2 */
			/* g is group at sqr to break link to */
	group_t g2;
	int c2,i,ldtmp;
	list_t tmplist,ptr;
   tmplist = EOL;
   c2 = G_COLOR(g);
   i = fdir[s];
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      if (s+nbr[i] == s2)continue;
      g2 = board[s+nbr[i]];
      if (G_COLOR(g2) == c2)
         addlist(g2,&tmplist);
      }
   for (ptr = tmplist; ptr != EOL; ptr = link[ptr])
      brkolkg((group_t)list[ptr],g,sqr,s);
   killist(&tmplist);
   }




void rstrlks(group_t gnew, group_t gold, sqr_t s) {  /* restore links from gold to gnew */
			/* s has stone on it. */
	sqr_t sn;
   int i,ldtmp,offs,oldedge,j;
   i = fdir[s];
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      sn = s;
      offs = nbr[i];
      oldedge = edge[s];
      for (j = 0; j < 4; ++j) {
         sn = sn + offs;
         if (board[sn] != NOGROUP)break;
         if (j == 1 && ld[sn] != 0 && ld[sn] != NOLD) { /* found one */
            brklks(sn,s,NOSQUARE,gold);
            addlks(sn,gnew,NOSQUARE);
            }
         if (j == 2 && ld[sn] != 0 && ld[sn] != NOLD) { /* found one */
            brklkgs(sn,s,NOSQUARE,gold);
            addlkgs(sn,gnew,NOSQUARE);
            }
         if (j == 3 && ld[sn] != 0 && ld[sn] != NOLD) { /* found one */
            brkolkgs(sn,s,NOSQUARE,gold);
            addolkgs(sn,gnew,NOSQUARE);
            }
         if (edge[sn] == 0)break;
         if (edge[sn] == 1 && oldedge > 1)break;
         }
      }
   }
   





void brklink(group_t g1, group_t g2, sqr_t s1, sqr_t s2) {  /* break link from g1 to g2 (s1 part of g2) at s2 */
		/* don'y break it if multiple links same square */
   int i,j,ptr,ldtmp,cflag;
   conn_t cn;
#ifdef CHECK
	char buf[80];
#endif
   if (g1 == g2)return;
   cflag = FALSE;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for (ldtmp = ldir[i]; i < ldtmp; ++i)
      if (board[s2+nbr[i]] == g2) {
         cflag = TRUE;
         break;
         }
      for (j = 0; j < 4; ++j) {
         if (sqrbrd[s2][j] != s1 &&
            (board[sqrbrd[s2][j]] == g2 ||
             (cflag && board[sqrbrd[s2][j]] == g1)) && 
            (dstbrd[s2][j] == 1 /* || dstbrd[s2][j] == 2 */)) {
            return;
            }
         }
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      cn = list[ptr];
      if (cngr1[cn] == g1 && cngr2[cn] == g2 ||
         cngr1[cn] == g2 && cngr2[cn] == g1) {
         if (!dellist(s2,&cnlkptr[cn])) {
#ifdef TEST
#ifdef CHECK
		 outerror("brklink error\n");
            psqr(s2);
#endif            
            turnoffcplay();
	    outerror("brklink error");
#endif	    
            }
         else {
            --cnlknum[cn];
            }
         dellist(cn,&lkbrd[s2]);
         if (cnlknum[cn] == 0 && cncnum[cn] == 0 && cnllnum[cn] == 0 && cnollnum[cn] == 0 && cnddnum[cn] == 0) {
	    delconnrec(cn);
            }
	 else if (cnlknum[cn] < 3)
		 addlist(cn,&cnchgd);
         return;
         }
      }
#ifdef CHECK      
   sprintf(buf,"brklink cant break link between %d and %d at ",g1,g2);
   outerror(buf);
   psqr(s2);
   outerror(" sqr is ");
   psqr(s1);
#endif
   }

void brklkg(group_t g1, group_t g2, sqr_t s1, sqr_t s2) {  /* break link from g1 to g2 (s1 part of g2) at s2 */
		/* don'y break it if multiple links same square */
   int i,j,ptr,ldtmp,cflag;
   conn_t cn;
#ifdef CHECK
	char buf[80];
#endif
   if (g1 == g2)return;
   cflag = FALSE;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for (ldtmp = ldir[i]; i < ldtmp; ++i)
      if (board[s2+nbr[i]] == g2) {
         cflag = TRUE;
         break;
         }
      for (j = 0; j < 4; ++j) {
         if (sqrbrd[s2][j] != s1 &&
            (board[sqrbrd[s2][j]] == g2 ||
             (cflag && board[sqrbrd[s2][j]] == g1)) && 
            (dstbrd[s2][j] == 2)) {
            return;
            }
         }
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      cn = list[ptr];
      if (cngr1[cn] == g1 && cngr2[cn] == g2 ||
         cngr1[cn] == g2 && cngr2[cn] == g1) {
         if (!dellist(s2,&cnllptr[cn])) {
#ifdef TEST   	
            turnoffcplay();
	    	outerror("brklkg error");
#endif	    
            }
         else {
            --cnllnum[cn];
            }
         dellist(cn,&llbrd[s2]);
         if (cnllnum[cn] == 0 && cncnum[cn] == 0 && cnlknum[cn] == 0 && cnollnum[cn] == 0 && cnddnum[cn] == 0) {
		    delconnrec(cn);
            }
         else if (cnllnum[cn] < 3)
               addlist(cn,&cnchgd);
         return;
         }
      }
#ifdef CHECK      
   sprintf(buf,"brklkg cant break link between %d and %d at ",g1,g2);
   outerror(buf);
   psqr(s2);
   outerror(" sqr is ");
   psqr(s1);
#endif
   }

void brkolkg(group_t g1, group_t g2, sqr_t s1, sqr_t s2) {  /* break link from g1 to g2 (s1 part of g2) at s2 */
		/* don'y break it if multiple links same square */
   int i,j,ptr,ldtmp,cflag;
   conn_t cn;
#ifdef CHECK
	char buf[80];
#endif
   if (g1 == g2)return;
   cflag = FALSE;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for (ldtmp = ldir[i]; i < ldtmp; ++i)
      if (board[s2+nbr[i]] == g2) {
         cflag = TRUE;
         break;
         }
      for (j = 0; j < 4; ++j) {
         if (sqrbrd[s2][j] != s1 &&
            (board[sqrbrd[s2][j]] == g2 ||
             (cflag && board[sqrbrd[s2][j]] == g1)) && 
            (dstbrd[s2][j] == 3)) {
            return;
            }
         }
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      cn = list[ptr];
      if (cngr1[cn] == g1 && cngr2[cn] == g2 ||
         cngr1[cn] == g2 && cngr2[cn] == g1) {
         if (!dellist(s2,&cnollptr[cn])) {
#ifdef TEST   	
            turnoffcplay();
	    	outerror("brkolkg error");
#endif	    
            }
         else {
            --cnollnum[cn];
            }
         dellist(cn,&ollbrd[s2]);
         if (cnllnum[cn] == 0 && cncnum[cn] == 0 && cnlknum[cn] == 0 && cnollnum[cn] == 0 && cnddnum[cn] == 0) {
		    delconnrec(cn);
            }
         else if (cnollnum[cn] < 3)
               addlist(cn,&cnchgd);
         return;
         }
      }
#ifdef CHECK      
   sprintf(buf,"brkolkg cant break link between %d and %d at ",g1,g2);
   outerror(buf);
   psqr(s2);
   outerror(" sqr is ");
   psqr(s1);
#endif
   }


void realbrklink(group_t g1, group_t g2, sqr_t s1, sqr_t s2) {  /* break link from g1 to g2 (s1 part of g2) at s2 */
		/* break it even if other conns */
   int i,j,ptr,ldtmp;
   conn_t cn;
#ifdef CHECK
	char buf[80];
#endif
   i = s1;  /* make lint/penpoint happy */
   if (g1 == g2)return;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for (ldtmp = ldir[i]; i < ldtmp; ++i)
      if (board[s2+nbr[i]] == g2) {	/* found g2 near. */
         for (j = 0; j < 4; ++j)	/* are any g1's far? */
            if (board[sqrbrd[s2][j]] == g1 &&
               (dstbrd[s2][j] == 1 /* || dstbrd[s2][j] == 2 */)) {
               return;
               }
         break;
         }
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      cn = list[ptr];
      if (cngr1[cn] == g1 && cngr2[cn] == g2 ||
         cngr1[cn] == g2 && cngr2[cn] == g1) {
         if (!dellist(s2,&cnlkptr[cn])) {
#ifdef TEST   	
#ifdef CHECK            
            sprintf(buf,"realbrklink error! g1 %d g2 %d \n",g1,g2);
            outerror(buf);
            outerror(ssqr(s2,buf));
            outerror(ssqr(s1,buf));
#endif            
            turnoffcplay();
	    outerror("realbrklink error");
#endif	    
            }
         else {
            --cnlknum[cn];
             }
         dellist(cn,&lkbrd[s2]);
         if (cnlknum[cn] == 0 && cncnum[cn] == 0 && cnllnum[cn] == 0 && cnollnum[cn] == 0 && cnddnum[cn] == 0) {
		    delconnrec(cn);
            }
         else if (cnlknum[cn] < 3)
               addlist(cn,&cnchgd);
         return;
         }
      }
#ifdef CHECK      
   sprintf(buf,"realbrklink cant break link between %d and %d at ",g1,g2);
   outerror(buf);
   outerror(ssqr(s2,buf));
   outerror(" sqr is ");
   outerror(ssqr(s1,buf));
#endif
   }


void realbrklkg(group_t g1, group_t g2, sqr_t s1, sqr_t s2) {  /* break link from g1 to g2 (s1 part of g2) at s2 */
			/* break it even if other conns */
	list_t ptr;
   int i,j,ldtmp;
   conn_t cn;
#ifdef CHECK
	char buf[80];
#endif
   i = s1;  /* make lint happy */
   if (g1 == g2)return;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for (ldtmp = ldir[i]; i < ldtmp; ++i)
      if (board[s2+nbr[i]] == g2) {	/* found g2 near. */
         for (j = 0; j < 4; ++j)	/* are any g1's far? */
            if (board[sqrbrd[s2][j]] == g1 &&
               (dstbrd[s2][j] == 2 )) {
               return;
               }
         break;
         }
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      cn = list[ptr];
      if (cngr1[cn] == g1 && cngr2[cn] == g2 ||
         cngr1[cn] == g2 && cngr2[cn] == g1) {
         if (!dellist(s2,&cnllptr[cn])) {
#ifdef TEST   	
#ifdef CHECK
            sprintf(buf,"realbrklkg error! g1 %d g2 %d \n",g1,g2);
            outerror(buf);
            outerror(ssqr(s2,buf));
            outerror(ssqr(s1,buf));
#endif
            turnoffcplay();
	   		outerror("realbrklink error");
#endif	    
            }
         else {
            --cnllnum[cn];
             }
         dellist(cn,&llbrd[s2]);
         if (cnlknum[cn] == 0 && cncnum[cn] == 0 && cnllnum[cn] == 0 && cnollnum[cn] == 0 && cnddnum[cn] == 0) {
		    delconnrec(cn);
            }
         else if (cnllnum[cn] < 3)
               addlist(cn,&cnchgd);
         return;
         }
      }
#ifdef CHECK      
   sprintf(buf,"realbrklkg cant break link between %d and %d at ",g1,g2);
   outerror(buf);
   outerror(ssqr(s2,buf));
   outerror(" sqr is ");
   outerror(ssqr(s1,buf));
#endif
   }

void realbrkolkg(group_t g1, group_t g2, sqr_t s1, sqr_t s2) {  /* break link from g1 to g2 (s1 part of g2) at s2 */
			/* break it even if other conns */
	list_t ptr;
   int i,j,ldtmp;
   conn_t cn;
#ifdef CHECK
	char buf[80];
#endif
   i = s1;  /* make lint happy */
   if (g1 == g2)return;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for (ldtmp = ldir[i]; i < ldtmp; ++i)
      if (board[s2+nbr[i]] == g2) {	/* found g2 near. */
         for (j = 0; j < 4; ++j)	/* are any g1's far? */
            if (board[sqrbrd[s2][j]] == g1 &&
               (dstbrd[s2][j] == 3 )) {
               return;
               }
         break;
         }
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      cn = list[ptr];
      if (cngr1[cn] == g1 && cngr2[cn] == g2 ||
         cngr1[cn] == g2 && cngr2[cn] == g1) {
         if (!dellist(s2,&cnollptr[cn])) {
#ifdef TEST   	
#ifdef CHECK
            sprintf(buf,"realbrkolkg error! g1 %d g2 %d \n",g1,g2);
            outerror(buf);
            outerror(ssqr(s2,buf));
            outerror(ssqr(s1,buf));
#endif
            turnoffcplay();
	   		outerror("realbrkolkg error");
#endif	    
            }
         else {
            --cnollnum[cn];
             }
         dellist(cn,&ollbrd[s2]);
         if (cnlknum[cn] == 0 && cncnum[cn] == 0 && cnllnum[cn] == 0 && cnollnum[cn] == 0 && cnddnum[cn] == 0) {
		    delconnrec(cn);
            }
         else if (cnollnum[cn] < 3)
               addlist(cn,&cnchgd);
         return;
         }
      }
#ifdef CHECK      
   sprintf(buf,"realbrkolkg cant break link between %d and %d at ",g1,g2);
   outerror(buf);
   outerror(ssqr(s2,buf));
   outerror(" sqr is ");
   outerror(ssqr(s1,buf));
#endif
   }


void delconnrec(conn_t i) {
#ifdef CHECK
	if (cncnum[i] != 0 || cnlknum[i] != 0 || cnllnum[i] != 0 || cnollnum[i] != 0 && cnddnum[i] != 0)
		outerror("delconnrec not empty\n");
#endif
	dellist(i,&grcnp[cngr1[i]]);
	dellist(i,&grcnp[cngr2[i]]);
	adflist(i,&cnfreelist);
	if (grldr[(int)NUMGROUPS+i] != EOL)kill_ldrflags((listval_t)(NUMGROUPS+i));
	if (PROTCONN(i)) {
		addlist(mvs[grpieces[cngr1[i]]],&charmy);
		addlist(mvs[grpieces[cngr2[i]]],&charmy);
		}
	}
	
void addlink(group_t g1, group_t g2, sqr_t s) {  /* add a link from g1 to g2 at s */
	list_t ptr;
   conn_t i;
#ifdef CHECK
	char buf[80];
#endif
   if (g1 == g2)return;
#ifdef CHECK
   if (g1 > NUMGROUPS-2 || g2 > NUMGROUPS-2) {
      sprintf(buf,"bad groups in addlink %d %d",g1,g2);
      outerror(buf);
   }
#endif
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      if (cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1) {
         if (addlist(s,&cnlkptr[i])) {
            ++cnlknum[i];
            if (cnlknum[i] > 1)
               addlist(i,&cnchgd);
            }
         addlist(i,&lkbrd[s]);
         return;
         }
      }
   i = gtflist(&cnfreelist);
#ifdef CHECK   
   if (i == G2ERROR) {
	   outerror("Ran out of connection records in addlink!\n");
	   return;
	   }
#endif

   addlist(i,&cnchgd);
   addlist(s,&cnlkptr[i]);
   addlist(i,&lkbrd[s]);
   addlist(i,&grcnp[g1]);
   addlist(i,&grcnp[g2]);
   cngr1[i] = g1;
   cngr2[i] = g2;
   cnlknum[i] = 1;
   cnprot[i] = CANT_CONNECT;
   if (grarmy[g1] == grarmy[g2])
   		cnprot[i] = AJI_CONNECT;
   }



void addlkg(group_t g1, group_t g2, sqr_t s) {  /* add a linkage from g1 to g2 at s */
	list_t ptr;
   conn_t i;
#ifdef CHECK
	char buf[80];
#endif
   if (g1 == g2)return;
#ifdef CHECK
   if (g1 > NUMGROUPS-2 || g2 > NUMGROUPS-2) {
      sprintf(buf,"bad groups in addlkg %d %d",g1,g2);
      outerror(buf);
   }
#endif
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      if (cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1) {
         if (addlist(s,&cnllptr[i])) {
            ++cnllnum[i];
            if (cnllnum[i] > 1)
               addlist(i,&cnchgd);
            }
         addlist(i,&llbrd[s]);
         return;
         }
      }
   i = gtflist(&cnfreelist);
#ifdef CHECK   
   if (i == G2ERROR) {
	   outerror("Ran out of connection records in addlkg!\n");
	   return;
	   }
#endif
   addlist(i,&cnchgd);
   addlist(s,&cnllptr[i]);
   addlist(i,&llbrd[s]);
   addlist(i,&grcnp[g1]);
   addlist(i,&grcnp[g2]);
   cngr1[i] = g1;
   cngr2[i] = g2;
   cnllnum[i] = 1;
   cnprot[i] = CANT_CONNECT;
   if (grarmy[g1] == grarmy[g2])
   		cnprot[i] = AJI_CONNECT;
   }

void addolkg(group_t g1, group_t g2, sqr_t s) {  /* add a long linkage from g1 to g2 at s */
	list_t ptr;
   conn_t i;
#ifdef CHECK
	char buf[80];
#endif
   if (g1 == g2)return;
#ifdef CHECK
   if (g1 > NUMGROUPS-2 || g2 > NUMGROUPS-2) {
      sprintf(buf,"bad groups in addlkg %d %d",g1,g2);
      outerror(buf);
   }
#endif
   for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
      i = list[ptr];
      if (cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1) {
         if (addlist(s,&cnollptr[i])) {
            ++cnollnum[i];
            if (cnollnum[i] > 1)
               addlist(i,&cnchgd);
            }
         addlist(i,&ollbrd[s]);
         return;
         }
      }
   i = gtflist(&cnfreelist);
#ifdef CHECK   
   if (i == G2ERROR) {
	   outerror("Ran out of connection records in addlkg!\n");
	   return;
	   }
#endif
   addlist(i,&cnchgd);
   addlist(s,&cnollptr[i]);
   addlist(i,&ollbrd[s]);
   addlist(i,&grcnp[g1]);
   addlist(i,&grcnp[g2]);
   cngr1[i] = g1;
   cngr2[i] = g2;
   cnollnum[i] = 1;
   cnprot[i] = CANT_CONNECT;
   if (grarmy[g1] == grarmy[g2])
   		cnprot[i] = AJI_CONNECT;
   }


static void adddiag(group_t g1, group_t g2, sqr_t s) {  /* add a double diag conn from g1 to g2 at s */
	list_t ptr;
	conn_t i;
#ifdef CHECK
	char buf[80];
#endif
	if (g1 == g2)return;
#ifdef CHECK
	if (g1 > NUMGROUPS-2 || g2 > NUMGROUPS-2 || lnbn[s] < 2) {
		sprintf(buf,"bad groups in adddiag %d %d",g1,g2);
		outerror(buf);
	}
#endif
	for (ptr = grcnp[g1]; ptr != EOL; ptr = link[ptr]) {
		i = list[ptr];
		if (cngr1[i] == g1 && cngr2[i] == g2 ||
			cngr1[i] == g2 && cngr2[i] == g1) {
			if (addlist(s,&cnddptr[i])) {
				++cnddnum[i];
				if (cnddnum[i] > 1)
					addlist(i,&cnchgd);
				}
			addlist(i,&ddbrd[s]);
			return;
			}
		}
	i = gtflist(&cnfreelist);
#ifdef CHECK   
	if (i == G2ERROR) {
		outerror("Ran out of connection records in addlkg!\n");
		return;
		}
#endif
	addlist(i,&cnchgd);
	addlist(s,&cnddptr[i]);
	addlist(i,&ddbrd[s]);
	addlist(i,&grcnp[g1]);
	addlist(i,&grcnp[g2]);
	cngr1[i] = g1;
	cngr2[i] = g2;
	cnddnum[i] = 1;
	cnprot[i] = CANT_CONNECT;
	if (grarmy[g1] == grarmy[g2])
   		cnprot[i] = AJI_CONNECT;
	}
