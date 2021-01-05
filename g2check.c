/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#ifdef CHECK
/* only include this code if we are checking data structures */

# include <stdio.h>
# include <time.h>
# include "g2hd.h"
# include "g2tree.h"
# include "g2dist.h"


void fixtime();

extern list_t hcpoints;

int maxlist, maxgroup, maxpot, maxarmy, maxconn, maxeye;
listval_t maxstrat;

extern list_t undocommands;
extern void dumpeyerec(eye_t rn);


#ifdef TEST

extern int pfac[3],cfac[3],ccl[3],nextpot;

char listflag[NUMLIST];       

static int setflag(list_t p) {
	char buf[100];
	if (listflag[p]) {
		sprintf(buf,"List element %d, value %d is in two lists!\n",p,list[p]);
		outerr(buf);
		return(FALSE);
		}
	listflag[p] = TRUE;
	return TRUE;
	}

extern list_t swaplist;

int dscheck(int flag) {
	group_t g, g2, lgrp;
	army_t army;
	list_t ptr, tmplist, ptr2;
	char buf[200], buf2[10];
	sqr_t s;
	conn_t cn;
	eye_t rn;
	int mptr;
	int i, lcount, ltot, ldrcnt, nbncnt, cncnt, mvkcnt, ltrtot, gral, tltrgd, tltr1, tltrc;
	int cscnt,lbpcnt,nbpcnt,lfgcnt,cnbcnt,eyecnt,tercnt;
	int total,retval,dgrcnt,amycnt,cnfcnt,amfcnt,lklcnt,cnunusedcnt;
	int tmpcnt,size,armiesused,lkbcnt,llbcnt,cnlkcnt,cnllcnt;
	int eyedcnt,reasoncnt,lnbncnt,nblbcnt,j,ldtmp;
	int shpcnt,amlcnt,lnbbcnt,lnbwcnt,atvcnt,aeycnt,eptcnt,evtcnt,efrcnt;
	int evrscnt,hghcnt,sphghcnt,hcpcnt,arncnt,sum,numfreerecs,npbcnt = 0;
	int eyerecnum,aptnum,amncnt=0,npfcnt = 0,bstcnt = 0,undocnt;
	int charmycnt = 0, cnchcnt = 0, chgrpcnt = 0,pseqcnt = 0,nbgrpcnt = 0;
	int boundcount = 0;
	int swapcnt = 0,treecnt;
	int cnpathcnt = 0;

	life(FALSE);  /* get aliveness values so can check well. */
	for (i = 0; i < NUMLIST; ++i)
		listflag[i] = FALSE;
	retval = TRUE;
	atvcnt = 0;
	aeycnt = 0;
	shpcnt = 0;
	amlcnt = 0;
	nblbcnt = 0;
	dgrcnt = 0;
	amycnt = 0;
	ltot = 0;
	nbncnt = 0;
	cncnt = 0;
	mvkcnt = 0;
	cscnt = 0;
	tmplist = EOL;
	tmpcnt = 0;
	reasoncnt = 0;
	hghcnt = 0;
	sphghcnt = 0;
	hcpcnt = 0;
	undocnt = 0;
	// check ltrgd
	for (s = 0; s < boardsquare; ++s) {
		if (ltr1[s] == 0) continue;
		tltrgd = getltrgd(s,FALSE,&tltr1,&tltrc);
		if (tltrgd != ltrgd[s] || tltr1 != 0 && ltr1[s] == 0 || tltr1 == 0 && ltr1[s] != 0) {
			sprintf(buf, "ltr error move %d at %s, ltr1/gd %d %d, getltrgd %d %d\n",
				msptr, ssqr(s, buf2), ltr1[s], ltrgd[s], tltr1, tltrgd);
			outerr(buf);
			tltrgd = getltrgd(s,FALSE,&tltr1,&tltrc);	/* for debugging it */
		}
	}
	treecnt = checktree();
	if (treecnt != 0) {
		sprintf(buf,"Lost trees: %d\n",treecnt);
		outerror(buf);
		retval = FALSE;
		}
	if (list[EOL] != 0xFFFF) {
		sprintf(buf,"List[EOL] corrupted = %d\n",list[EOL]);
		outerror(buf);
		retval = FALSE;
		}

	for (ptr = swaplist; ptr != EOL; ptr = link[ptr]) {
   		swapcnt++;
   		if (!setflag(ptr))
   			outerror("Swaplist");;
   		}
   if (board[NOSQUARE] != NOGROUP) {
      retval = FALSE;
      sprintf(buf,"board[NOSQUARE] is wrong\n");
      outerror(buf);
      }
   if (grcolor[NOGROUP] != NOCOLOR) {
      retval = FALSE;
      sprintf(buf,"grcolor[NOGROUP] is wrong!\n");
      outerror(buf);
      }
	if (grarmy[NOGROUP] != NOARMY) {
		retval = FALSE;
		sprintf(buf,"grarmy[NOGROUP] is wrong!\n");
        outerror(buf);
		}
#ifdef MODEM
	for (ptr = undocommands; ptr != EOL; ptr = link[ptr]) {
		undocnt++;
   		if (!setflag(ptr))
   			outerr("undocommands");
		}
#endif

   for (s = firstsquare; s < lastsquare; s++) {
		tmpcnt += terv[s];
		if (terv[s] > 50 || terv[s] < -50) {
			sprintf(buf,"Lib terr out of range %d at %d",terv[s],s);
      		outerror(buf);
			retval = FALSE;
			}
   		}
   if (tmpcnt != tscr) {
      retval = FALSE;
      sprintf(buf,"tscr is wrong! %d %d\n",tscr,tmpcnt);
      outerror(buf);
      }

   tmpcnt = 0;
   for (s = firstsquare; s < lastsquare; ++s)
      if (board[s] != NOGROUP)
         tmpcnt += gralprob[board[s]]*cfac[grcolor[board[s]]];
   if (tmpcnt != pscr) {
       retval = FALSE;
       sprintf(buf,"pscr is wrong! %d %d\n",pscr,tmpcnt);
       outerror(buf);
        }
   for (g = 0; g < maxgr; g++) {
      if (!grlv[g]) {
         if (grlbp[g] != EOL) {
            retval = FALSE;
            sprintf(buf,"group %d still has libs\n",g);
      		outerr(buf);
            }
         if (grcnp[g] != EOL) {
            retval = FALSE;
            sprintf(buf,"group %d still has connections!\n",g);
      		outerr(buf);
            }
         if (grarmy[g] != NOARMY) {
            sprintf(buf,"not grlv group %d has grarmy %d\n",g,grarmy[g]);
            outerr(buf);
            }
         if (grdeadarmy[g] != NOARMY) {
            sprintf(buf,"not grlv group %d has grdeadarmy %d\n",g,grdeadarmy[g]);
            outerr(buf);
            }
         }
      else {
      	 if (msptr > 0 && g < maxgr-1 && savevalid[mvcolor[msptr-1]])
      	 {
      	 	if (gralive[g] >= WEAK && savegral[mvcolor[msptr-1]][g] <= ALIVE ||
      	 		gralive[g] <= ALIVE && savegral[mvcolor[msptr-1]][g] >= WEAK)
      	 		{
      	 			sprintf(buf,"life changed too fast move %d from %d to %d for %d at move %d at %d\n",
      	 				msptr, savegral[mvcolor[msptr-1]][g], gralive[g], g, msptr, mvs[msptr-1]);
      	 			outerror(buf);
      	 			higroup(g);
      	 		}
      	 		
      	 }
         if (G_THREATENED(g) > 2 && G_THREATENED(g) != 0) {
            sprintf(buf,"bad grthreatened %d for group %d\n",G_THREATENED(g),g);
            outerr(buf);
		    retval = FALSE;
		    }
         addlist(grarmy[g],&tmplist);
         if (grlv[g] != 1) {
            retval = FALSE;
            sprintf(buf,"group %d grlv %d\n",g,grlv[g]);
      		outerr(buf);
            }
         if (G_ALIVE(g) < 1 || G_ALIVE(g) > 25) {
            retval = FALSE;
            sprintf(buf,"group %d alive %d\n",g,G_ALIVE(g));
      		outerr(buf);
            }
         size = 0;
         for (mptr = grpieces[g]; mptr != -1; mptr = mvnext[mptr]) {
            if (mvs[mptr] > 360) {
               retval = FALSE;
               sprintf(buf,"mvs out of range %d\n",mvs[mptr]);
      			outerr(buf);
               }
            if (board[mvs[mptr]] != g) {
               retval = FALSE;
               sprintf(buf,"s %d board[s] %d group %d\n",mvs[mptr],board[mvs[mptr]],
                      g);
      			outerr(buf);
               }
            ++size;
            }
         if (grsize[g] != size) {
            retval = FALSE;
            sprintf(buf,"group %d grsize %d should be %d\n",g,grsize[g],size);
	      outerr(buf);
	            }
         if (size == 0) {
            retval = FALSE;
            sprintf(buf,"group %d has size zero\n",g);
      		outerr(buf);
            }
         if (grcolor[g] != 1 && grcolor[g] != 0) {
            retval = FALSE;
            sprintf(buf,"group %d color %d\n",g,grcolor[g]);
      		outerr(buf);
            }
         ltot += grlibs[g];
         ptr = grlbp[g];
         lcount = 0;
         while(ptr != EOL) {
            if (board[list[ptr]] != NOGROUP) {
               retval = FALSE;
               sprintf(buf,"nonempty liberty. group %d s %d\n",g,list[ptr]);
      			outerr(buf);
               }
	   		if (!setflag(ptr))
	   			outerr("liberty list");
            lcount ++;
            ptr = link[ptr];
            }

         if (lcount != grlibs[g]) {
            retval = FALSE;
            sprintf(buf,"count %d grlibs %d group %d\n",lcount,grlibs[g],g);
      		outerr(buf);
            }
         for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
	   		if (!setflag(ptr))
	   			outerr("grnbp");
            ++nbncnt;
	    	if (list[ptr] == NOGROUP) {
		    	sprintf(buf,"NOGROUP is nbr for group %d\n",g);
      			outerr(buf);
		    	retval = FALSE;
		    	}
	    	}
         for (ptr = grcnp[g]; ptr != EOL; ptr = link[ptr]) {
            ++cncnt;
	   		if (!setflag(ptr))
	   			outerr("grcnp");
            if (cncnum[list[ptr]] == 0 && cnlknum[list[ptr]] == 0 && 
				cnllnum[list[ptr]] == 0 && cnollnum[list[ptr]] == 0 && 
				cnddnum[list[ptr]] == 0) {
               sprintf(buf,"group %d has empty connection\n",g);
      			outerr(buf);
               retval = FALSE;
               }
            }
#ifdef NEVER
         if (G_ALIVE(g) == DEAD) {
            if (grsize[g] != armysize[grarmy[g]]) {
               sprintf(buf,"group %d alive %d size %d is in army %d size %d!\n",
				g,G_ALIVE(g),grsize[g],grarmy[g],armysize[grarmy[g]]);
      			outerr(buf);
               retval = FALSE;
               }
            }
         if (G_ALIVE(g) == DEAD) {
            for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
               if (G_ALIVE(list[ptr]) != DEAD && 
			!G_THREATENED(list[ptr]) &&
			grarmy[list[ptr]] != grdeadarmy[g]) {
                  sprintf(buf,"group %d grdeadarmy %d wrong!\n",g,grdeadarmy[g]);
      				outerr(buf);
                  retval = FALSE;
                  }
               }
            }
         else if (grdeadarmy[g] != NOARMY) {
            sprintf(buf,"group %d has deadarmy %d\n",g,grdeadarmy[g]);
      		outerr(buf);
            retval = FALSE;
            }
#endif
         }
      }
   armiesused = 0;
   arncnt = 0;
   for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
	army = (army_t)list[ptr];
	armiesused++;
	for (i = 0; i < NUMRUN; ++i)
		for (ptr2 = armyrun[army][i]; ptr2 != EOL; ptr2 = link[ptr2]) {
	   		if (!setflag(ptr2))
	   			outerr("armyrun");
			++arncnt;
			}
	}
	killist(&tmplist);
	mvkcnt = 0;
	for (i = 0; i < NUMMOVES; ++i) {
		for (ptr = mvcapt[i]; ptr != EOL; ptr = link[ptr]) {
			++mvkcnt;
			if (!setflag(ptr))
				outerr("mvcapt");
			}
		for (ptr = mvconn[i]; ptr != EOL; ptr = link[ptr]) {
			++mvkcnt;
			if (!setflag(ptr))
				outerr("mvconn");
			}
		}
  
	ldrcnt = 0;
	lfgcnt = 0;
	cnbcnt = 0;
	eyedcnt = 0;
	lkbcnt = 0;
	llbcnt = 0;
	ltrtot = 0;
	evrscnt = 0;
	boundcount = 0;
	for (ptr = bestseq[NOSQUARE]; ptr != EOL; ptr = link[ptr]) {
		if (!setflag(ptr))
			outerr("bestseq NSQUARE");
		++bstcnt;
	    }
	for (ptr = bestseq[PASS]; ptr != EOL; ptr = link[ptr]) {
		if (!setflag(ptr))
			outerr("bestseq PASS");
		++bstcnt;
	    }
	for (s = firstsquare; s < lastsquare; ++s) {
		if (board[s] == NOGROUP && boundary[s] != EOL) {
			outerr("bad boundary");
			retval = FALSE;
			}
		for (ptr = boundary[s]; ptr != EOL; ptr = link[ptr]) {
			if (!setflag(ptr))
				outerr("boundary");
			boundcount++;
			}
		for (ptr = bestseq[s]; ptr != EOL; ptr = link[ptr]) {
			if (!setflag(ptr))
				outerr("bestseq");
			++bstcnt;
			}
		gral = 0;
		lgrp = board[s];
		for (ptr = nbgrp[s][0]; ptr != EOL; ptr = link[ptr]) {
			if (gralive[list[ptr]] > gral)
			{
				gral = gralive[list[ptr]];
				lgrp = (group_t)list[ptr];
			}
			++nbgrpcnt;
	   		if (!setflag(ptr))
	   			outerr("nbgrp 0");
			if (!inlist(s,&grlbp[list[ptr]]) || grcolor[list[ptr]] != 0) {
				retval = FALSE;
				sprintf(buf,"Bad nbgrp\n");
      			outerr(buf);
				}
			}
		for (ptr = nbgrp[s][1]; ptr != EOL; ptr = link[ptr]) {
			if (gralive[list[ptr]] > gral)
			{
				gral = gralive[list[ptr]];
				lgrp = (group_t)list[ptr];
			}
			++nbgrpcnt;
	   		if (!setflag(ptr))
	   			outerr("nbgrp 1");
			if (!inlist(s,&grlbp[list[ptr]]) || grcolor[list[ptr]] != 1) {
				retval = FALSE;
				sprintf(buf,"Bad nbgrp\n");
				outerr(buf);
				}
			}
		if (lgrp != lgr[s] && 
			(lgrp == NOGROUP || 
			 lgr[s] == NOGROUP || 
			 gralive[lgrp] > gralive[lgr[s]] && 
			  (!G_THREATENED(lgr[s]) || G_THREATENED(lgrp))))
		{
			retval = FALSE;
			sprintf(buf, "Bad lgr is %d, not %d at ",lgr[s], lgrp);
			outerr(buf);
			psqr(s);
			outerr("\n");
		}
		sum = 0;
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
	   		if (!setflag(ptr)) {
	   			sprintf(buf,"nblbp for %d\n",s);
	   			outerr(buf);
	   			}
			++nblbcnt;
			++sum;
			if (board[list[ptr]] != NOGROUP) {
				retval = FALSE;
				psqr(s);
				sprintf(buf," has empty liberty at ");
      			outerr(buf);
				psqr(list[ptr]);
				sprintf(buf,"\n");
    	  		outerr(buf);
				}
			}
		if (sum != lnbn[s]) {
			retval = FALSE;
			sprintf(buf,"lnbn and nblbp disceprency\n");
			outerr(buf);
			}
		lnbbcnt = lnbwcnt = 0;
		lnbncnt = 0;
		j = fdir[s];
		for (ldtmp = ldir[j]; j < ldtmp; ++j) {
			if (board[s+nbr[j]] == NOGROUP)++lnbncnt;
			if (grcolor[board[s+nbr[j]]] == BLACKCOLOR)++lnbbcnt;
			if (grcolor[board[s+nbr[j]]] == WHITECOLOR)++lnbwcnt;
			}
		if (lnbncnt != lnbn[s]) {
			psqr(s);
			sprintf(buf,": bad lnbn value %d %d\n",lnbn[s],lnbncnt);
			outerr(buf);
			retval = FALSE;
			}
		if (lnbbcnt != lnbf[s][0]) {
			psqr(s);
			sprintf(buf,": bad lnbf[0] value %d %d\n",lnbf[s][0],lnbbcnt);
      		outerr(buf);
			retval = FALSE;
			}
		if (lnbwcnt != lnbf[s][1]) {
			psqr(s);
			sprintf(buf,": bad lnbf[1] value %d %d\n",lnbf[s][1],lnbwcnt);
      		outerr(buf);
			retval = FALSE;
			}

		for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
	   		if (!setflag(ptr)) {
				sprintf(buf," in stratreasons at %d\n",s);
	   			outerr(buf);
				}
         	++reasoncnt;
	    	}
      	for (ptr = eyevitrec[s]; ptr != EOL; ptr = link[ptr]) {
	   		if (!setflag(ptr))
	   			outerr("eyevitrec");
			evrscnt++;
			if (eyevital[list[ptr]&EYEPOINTMASK] == EOL) {
				sprintf(buf,"eyevital points at empty rn %d at :",list[ptr]&EYEPOINTMASK);
      			outerr(buf);
				psqr(s);
				sprintf(buf,"\n");
      			outerr(buf);
				retval = FALSE;
				}
			}
      	rn = eyerec[s];
#ifdef NEVER
		if (rn != 0 && board[i] == NOGROUP && lnbn[s] > 2 && eyetype[rn] != CORNEREYE) {
			psqr(s);
			sprintf(buf,": eyerec %d lnbn > 2\n",rn);
			outerr(buf);
			}
		if (rn != 0 && board[s] != NOGROUP && S_ALIVE(s) != DEAD &&
	   		!S_THREATENED(s)) {
			psqr(s);
			sprintf(buf,": eyerec %d group is not dead or threatened\n",rn);
			outerr(buf);
			}
      	if (rn != 0 && eyepot[rn] != eyeval[rn] && eyevital[rn] == EOL) {
			retval = FALSE;
			sprintf(buf,"eyerec %d has no vital points!\n",rn);
			outerr(buf);
			}
#endif
      for (ptr = cnbrd[s]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("cnbrd");
	    if (!inlist(s,&cnptr[list[ptr]])) {
	    	sprintf(buf,"cnbrd at %d isn't in eyeptr\n",s);
	    	outerr(buf);
	    	retval = FALSE;
	    	}
         ++cnbcnt;
	    }
      for (ptr = lkbrd[s]; ptr != EOL; ptr = link[ptr]) {
	    if (!inlist(s,&cnlkptr[list[ptr]])) {
	    	sprintf(buf,"lkbrd at %d isn't in cnlkptr\n",s);
	    	outerr(buf);
	    	retval = FALSE;
	    	}
         ++lkbcnt;
   		if (!setflag(ptr))
   			outerr("lkbrd");
		}
      for (ptr = llbrd[s]; ptr != EOL; ptr = link[ptr]) {
	    if (!inlist(s,&cnllptr[list[ptr]])) {
	    	sprintf(buf,"llbrd at %d isn't in cnllptr\n",s);
	    	outerr(buf);
	    	retval = FALSE;
	    	}
   		if (!setflag(ptr))
   			outerr("llbrd");
         ++llbcnt;
	    }
      for (ptr = ollbrd[s]; ptr != EOL; ptr = link[ptr]) {
	    if (!inlist(s,&cnollptr[list[ptr]])) {
	    	sprintf(buf,"ollbrd at %d isn't in cnollptr\n",s);
	    	outerr(buf);
	    	retval = FALSE;
	    	}
   		if (!setflag(ptr))
   			outerr("llbrd");
         ++llbcnt;
	    }
      for (ptr = ddbrd[s]; ptr != EOL; ptr = link[ptr]) {
	    if (!inlist(s,&cnddptr[list[ptr]])) {
	    	sprintf(buf,"ddbrd at %d isn't in cnddptr\n",s);
	    	outerr(buf);
	    	retval = FALSE;
	    	}
   		if (!setflag(ptr))
   			outerr("ddbrd");
         ++llbcnt;
	    }
      for (ptr = ldrflag[s]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("ldrflag");
	      if (list[ptr] >= NUMGROUPS+NUMCONNS+NUMEYERECS) {
		      sprintf(buf,"ldrflag wrong: is %d : ",list[ptr]);
      		outerr(buf);
		      psqr(s);
		      retval = FALSE;
		      }
	      ++lfgcnt;
	      }
      }
   for (i = 0; i < NUMGROUPS+NUMCONNS+NUMEYERECS; ++i) {
      for (ptr = grldr[i]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("grldr");
          ++ldrcnt;
	    }
      if (grldr[i] == EOL)continue;
	   if (i < NUMGROUPS && !grlv[i]) {
		   sprintf(buf,"group %d is gone but still has ladder\n",i);
      		outerr(buf);
		   retval = FALSE;
		   }
	   if (i >= NUMGROUPS && i < NUMGROUPS+NUMCONNS &&
	      cncnum[i-NUMGROUPS] == 0 && cnlknum[i-NUMGROUPS] == 0 &&
	      cnllnum[i-NUMGROUPS] == 0 && cnollnum[i-NUMGROUPS] == 0 && cnddnum[i-NUMGROUPS] == 0) {
		   sprintf(buf," cn %d (from %d to %d)- no conns, but still has ladder\n",i-NUMGROUPS,cngr1[i-NUMGROUPS], cngr2[i-NUMGROUPS]);
      		outerr(buf);
		   retval = FALSE;
		   }
	   if (i >= NUMGROUPS+NUMCONNS && eyeptr[i-NUMGROUPS-NUMCONNS] == EOL) {
		   sprintf(buf,"eye number %d still has ladder\n",i-NUMGROUPS-NUMCONNS);
		   outerr(buf);
		   retval = FALSE;
		   }
      }
   if (ldrcnt != lfgcnt) {
        sprintf(buf,"error ldrflag and grldr dont agree %d %d\n",lfgcnt,ldrcnt);
      outerr(buf);
        retval = FALSE;
        }

   if (G_THREATENED(NOGROUP)) {
	   sprintf(buf,"NOGROUP is threatened!\n");
      outerr(buf);
	   retval = FALSE;
	   }
  
	eyecnt = 0;
	for (ptr = eyelist; ptr != EOL; ptr = link[ptr]) {
		if (!setflag(ptr))
			outerr("eyelist");
		eyecnt++;
		}

	tercnt = 0;
  
	cscnt = 0; 
	cnlkcnt = 0;
	cnllcnt = 0;
	cnunusedcnt = 0;
	for (cn = 0; cn < NUMCONNS; ++cn) { 
      if (cncnum[cn] != 0 && cnptr[cn] == EOL) {
            retval = FALSE;
            sprintf(buf,"no cnptr for conn record %d\n",cn); 
      		outerr(buf);
            sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      		outerr(buf);
            }
      if (cnlknum[cn] != 0 && cnlkptr[cn] == EOL) {
            retval = FALSE;
            sprintf(buf,"no cnlkptr for conn record %d\n",cn); 
      		outerr(buf);
            sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      		outerr(buf);
            }
      if (cnllnum[cn] != 0 && cnllptr[cn] == EOL) {
            retval = FALSE;
            sprintf(buf,"no cnllptr for conn record %d\n",cn); 
      		outerr(buf);
            sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      		outerr(buf);
            }
      if (cnollnum[cn] != 0 && cnollptr[cn] == EOL) {
            retval = FALSE;
            sprintf(buf,"no cnollptr for conn record %d\n",cn); 
      		outerr(buf);
            sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      		outerr(buf);
            }
      if (cnddnum[cn] != 0 && cnddptr[cn] == EOL) {
            retval = FALSE;
            sprintf(buf,"no cnddptr for conn record %d\n",cn); 
      		outerr(buf);
            sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      		outerr(buf);
            }
      if (cncnum[cn] == 0 && cnptr[cn] != EOL) {
            retval = FALSE;
            sprintf(buf,"cnptr for empty conn record\n"); 
      		outerr(buf);
            }
      if (cnlknum[cn] == 0 && cnlkptr[cn] != EOL) {
            retval = FALSE;
            sprintf(buf,"cnlkptr for empty conn record\n"); 
      		outerr(buf);
            sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      		outerr(buf);
            }
      if (cnllnum[cn] == 0 && cnllptr[cn] != EOL) {
            retval = FALSE;
            sprintf(buf,"cnllptr for empty conn record\n"); 
      		outerr(buf);
            sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      		outerr(buf);
            }
      if (cnollnum[cn] == 0 && cnollptr[cn] != EOL) {
            retval = FALSE;
            sprintf(buf,"cnollptr for empty conn record\n"); 
      		outerr(buf);
            sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      		outerr(buf);
            }
      if (cnddnum[cn] == 0 && cnddptr[cn] != EOL) {
            retval = FALSE;
            sprintf(buf,"cnddptr for empty conn record\n"); 
      		outerr(buf);
            sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      		outerr(buf);
            }
	  for (ptr = cnpathptr[cn]; ptr != EOL; ptr = link[ptr]) {
		  cnpathcnt++;
		  setflag(ptr);
		}
      if (cnptr[cn] == EOL && cnlkptr[cn] == EOL && 
      	cnllptr[cn] == EOL && cnollptr[cn] == EOL && cnddptr[cn] == EOL) {
         ++cnunusedcnt;
         continue;
         }
      if (!grlv[cngr1[cn]] || !grlv[cngr2[cn]]) {
         retval = FALSE;
         sprintf(buf,"connection to nonexistant group %d %d\n",cngr1[cn],cngr2[cn]);
      	outerr(buf);
      	   }

      if (cngr1[cn] == cngr2[cn]) {
         retval = FALSE;
         sprintf(buf,"group %d connected to itself\n",cngr1[cn]);
      	outerr(buf);
         }
      if (PROTCONN(cn) && grarmy[cngr1[cn]] != grarmy[cngr2[cn]]) {
         retval = FALSE;
         sprintf(buf,"armies different! groups %d %d\n",cngr1[cn],cngr2[cn]);
      	outerr(buf);
         }
      if (cnprot[cn] != getprot(cn)) {
      	retval = FALSE;
      	sprintf(buf,"bad cnprot value (not updated?), cn %d is %d, should be %d",
      		cn, (int)cnprot[cn], getprot(cn));
      	outerr(buf);
      	outaconn(cn);
      	}
      if (cnprot[cn] > SOLID_CONNECT || cnprot[cn] < 0) {
      	retval = FALSE;
      	sprintf(buf,"bad cnprot value %d for connection %d",cnprot[cn],cn);
      	outerr(buf);
      	outaconn(cn);
      	}
      tmpcnt = 0;
      for (ptr = cnptr[cn]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("cnptr");
         cscnt++;
         ++tmpcnt;
         }
      if (cncnum[cn] != tmpcnt) {
         sprintf(buf,"cncnum doesn't match in conn record\n");
      	outerr(buf);
         retval = FALSE;
         }
      tmpcnt = 0;
      for (ptr = cnlkptr[cn]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("cnlkptr");
         cnlkcnt++;
         ++tmpcnt;
         }
      if (cnlknum[cn] != tmpcnt) {
         sprintf(buf,"cnlknum doesn't match in conn record\n");
      	outerr(buf);
         sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      	outerr(buf);
         retval = FALSE;
         }
      tmpcnt = 0;
      for (ptr = cnllptr[cn]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("cnllptr");
         cnllcnt++;
         ++tmpcnt;
         }
      if (cnllnum[cn] != tmpcnt) {
         sprintf(buf,"cnllnum doesn't match in conn record\n");
      		outerr(buf);
         sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      	outerr(buf);
         retval = FALSE;
         }
      tmpcnt = 0;
      for (ptr = cnollptr[cn]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("cnollptr");
         cnllcnt++;
         ++tmpcnt;
         }
      if (cnollnum[cn] != tmpcnt) {
         sprintf(buf,"cnollnum doesn't match in conn record\n");
      		outerr(buf);
         sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      	outerr(buf);
         retval = FALSE;
         }
      tmpcnt = 0;
      for (ptr = cnddptr[cn]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("cnddptr");
         cnllcnt++;
         ++tmpcnt;
         }
      if (cnddnum[cn] != tmpcnt) {
         sprintf(buf,"cnddnum doesn't match in conn record\n");
      	outerr(buf);
         sprintf(buf,"groups %d %d\n",cngr1[cn],cngr2[cn]);
      	outerr(buf);
         retval = FALSE;
         }
      } 
   if (cscnt != cnbcnt) {
      sprintf(buf,"cscnt != cnbcnt!!! %d %d\n",cscnt,cnbcnt);
      outerr(buf);
      retval = FALSE;
      }
  
   lbpcnt = 0;
   nbpcnt = 0;
   for (i = 0; i < NUMMOVES; ++i) { 
      if (nbply[i] != EOL) {
         retval = FALSE;
         sprintf(buf,"still have nbply!\n"); 
      outerr(buf);
         }
      if (lbply[i] != EOL) {
         retval = FALSE;
         sprintf(buf,"still have lbply!\n"); 
      outerr(buf);
         }
      for (ptr = lbply[i]; ptr != EOL; ptr = link[ptr]) {
         lbpcnt++;
   		if (!setflag(ptr))
   			outerr("lbply");
		}
      for (ptr = nbply[i]; ptr != EOL; ptr = link[ptr]) {
         nbpcnt++;
   		if (!setflag(ptr))
   			outerr("nbply");
		}
      } 
   aptnum = 0;
   for (army = 0; army < NUMARMIES; ++army) {
      if (armygroups[army] == EOL) {
         if (armydeadgroups[army] != EOL) {
            	sprintf(buf,"army %d still has dead groups\n",army);
      			outerr(buf);
            	retval = FALSE;
            	}
	 if (armyvitalpoints[army] != EOL) {
		sprintf(buf,"army %d still has vital points!\n",army);
      outerr(buf);
		retval = FALSE;
		}
	 if (armyeyerecs[army] != EOL) {
		sprintf(buf,"army %d still has eye recs!\n",army);
      outerr(buf);
		retval = FALSE;
		}
	 if (armylbp[army] != EOL) {
		sprintf(buf,"army %d still has liberties!\n",army);
      outerr(buf);
		retval = FALSE;
		}
	 if (armynbp[army] != EOL) {
		sprintf(buf,"army %d still has neighbors!\n",army);
      outerr(buf);
		retval = FALSE;
		}
	 if (armypot[army] != EOL) {
		 sprintf(buf,"army %d still has potential!\n",army);
      outerr(buf);
		 retval = TRUE;
		 }
         continue;
         }
      for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
	      aptnum++;
		if (!setflag(ptr))
			outerr("armypot");
	      }
/*      if (armylibs[army] == 0) {
	      sprintf(buf,"Army %d has armylibs == 0!",army);
	      retval = FALSE;
	      }
*/
      for (ptr = armyeyerecs[army]; ptr != EOL; ptr = link[ptr]) {
	 if (eyetype[list[ptr]] == NOEYE) {
		sprintf(buf,"army %d has eyerec with type NO EYE\n",army);
      outerr(buf);
		retval = FALSE;
		}
	if (eyeptr[list[ptr]] == EOL) {
		sprintf(buf,"army %d has eye with no points in it!\n",army);
      outerr(buf);
		retval = FALSE;
		}
	}
      for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
	      ++amlcnt;
   		if (!setflag(ptr))
   			outerr("armylbp");
	      }
      for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
	      ++amncnt;
   		if (!setflag(ptr))
   			outerr("armynbp");
	      }
	for (ptr = armyvitalpoints[army]; ptr != EOL; ptr = link[ptr]) {
		++atvcnt;
   		if (!setflag(ptr))
   			outerr("armyvitalpoints");
		}
	for (ptr = armyeyerecs[army]; ptr != EOL; ptr = link[ptr]) {
		++aeycnt;
   		if (!setflag(ptr))
   			outerr("armyeyerecs");
		}
      size = 0;
      for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("armygroups");
         ++amycnt;
         g = (group_t)list[ptr];
         size += grsize[g];
         if (!grlv[g]) {
             sprintf(buf,"dead group %d still in army %d\n",g,army);
      		outerr(buf);
             retval = FALSE;
             }
         if (grarmy[g] != army) {
            sprintf(buf,"group %d is in armygroups for army %d and it has grarmy %d\n",g,army,grarmy[g]);
      		outerr(buf);
            retval = FALSE;
            }
         }
      if (size != armysize[army]) {
         sprintf(buf,"army %d size is %d should be %d\n",army,armysize[army],size);
      	outerr(buf);
         retval = FALSE;
         }
      for (ptr = armydeadgroups[army]; ptr != EOL; ptr = link[ptr]) {
   		if (!setflag(ptr))
   			outerr("armydeadgroups");
         dgrcnt++;
         g = (group_t)list[ptr];
         if (!grlv[g]) {
            sprintf(buf,"inactive group %d in armydeadgroups for army %d",g,army);
      		outerr(buf);
            retval = FALSE;
            }
         if (G_ALIVE(g) != DEAD) {
            sprintf(buf,"group %d in armydeadgroups army %d alive %d",g,army,
             G_ALIVE(g));
      		outerr(buf);
             retval = FALSE;
             }
         for (ptr2 = grnbp[g]; ptr2 != EOL; ptr2 = link[ptr2]) {
            g2 = (group_t)list[ptr2];
            if (G_ALIVE(g2) != DEAD && !G_THREATENED(g2) &&
		grarmy[g2] != army) {
               sprintf(buf,"deadgroup %d nbr army %d g %d in army %d \n",
                  g,grarmy[g2],g2,army);
      			outerr(buf);
               retval = FALSE;
               }
            }
         }
      }

	cnfcnt = 0;
	for (ptr = cnfreelist; ptr != EOL; ptr = link[ptr]) {
		if (!setflag(ptr))
			outerr("cnfreelist");
		++cnfcnt;
		cn = list[ptr];
		if (cncnum[cn] != 0 || cnlknum[cn] != 0 || cnllnum[cn] != 0 || cnollnum[cn] != 0 || cnddnum[cn] != 0) {
			retval = FALSE;
			sprintf(buf,"cn - %d is in free list with cn %d lk %d ll %d oll %d dd %d\n",
				cn,cncnum[cn],cnlknum[cn],cnllnum[cn],cnollnum[cn], cnddnum[cn]);
			outerr(buf);
			}
		}
	if (cnfcnt != cnunusedcnt) {
		retval = FALSE;
		sprintf(buf,"unused conns not in free list.  unused %d free %d\n",
			cnunusedcnt,cnfcnt);
		outerr(buf);
		}
	numfreerecs = 0;
	for (rn = 1; rn < NUMEYERECS; ++rn) {
		if (eyetype[rn] == NOEYE || eyetype[rn] == UNKNOWNEYE) {
			++numfreerecs;
			if (!inflist((listval_t)rn, &eyefreelist)) {
				sprintf(buf,"NOEYE %d is not in eyefreelist\n", rn);
				outerr(buf);
				dumpeyerec(rn);
				retval = FALSE;
				}
			if (eyeptr[rn] != EOL) {
				sprintf(buf, "NOEYE %d has points in it\n", rn);
				outerror(buf);
				retval = FALSE;
				}
			}
		else {
			if (eyeptr[rn] == EOL) {
				sprintf(buf,"used eye record %d has no points in it\n",rn);
     		 	outerror(buf);
				retval = FALSE;
				}
			for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr])
				if (eyerec[list[ptr]] != rn) {
					sprintf(buf,"eyerec %d type %d has point with bad eyerec (%d): ",
						rn,eyetype[rn],eyerec[list[ptr]]);
	      			outerr(buf);
					psqr(list[ptr]);
					retval = FALSE;
					}
				}
			}
		for (ptr = newpatfreelist; ptr != EOL; ptr = link[ptr]) {
   			if (!setflag(ptr))
   				outerr("newpatfreelist");
			++npfcnt;
		    }
		efrcnt = 0;
		for (ptr = eyefreelist; ptr != EOL; ptr = link[ptr]) {
	   		if (!setflag(ptr))
   				outerr("eyefreelist");
			efrcnt++;
			eyerecnum = list[ptr];
			if (eyetype[eyerecnum] != NOEYE) {
				sprintf(buf,"eyerec in free list has bad type %d\n",eyetype[eyerecnum]);
      			outerr(buf);
				retval = FALSE;
				}
		if (eyeptr[eyerecnum] != EOL || eyevital[eyerecnum] != EOL) {
			sprintf(buf,"Nonempty eyerec! %d\n",eyerecnum);
			outerr(buf);
			retval = FALSE;
			}
		}
	if (numfreerecs != efrcnt) {
		sprintf(buf,"free eye recs and eyefreelist are not the same! %d %d\n",
			numfreerecs, efrcnt);
		outerr(buf);
		retval = FALSE;
		}
	if (efrcnt > NUMEYERECS-1) {
		sprintf(buf,"too many entries in eye free list!\n");
		outerr(buf);
		for (ptr = eyefreelist; ptr != EOL; ptr = link[ptr]) {
			sprintf(buf,"%d ",list[ptr]);                   
			outerr(buf);
			}
		}
	evtcnt = 0;
	eptcnt = 0;
	for (i = 1; i < NUMEYERECS; ++i) {
		for (ptr = eyevital[i]; ptr != EOL; ptr = link[ptr]) {
			++evtcnt;
   			if (!setflag(ptr))
   				outerr("eyevital");
			}
		for (ptr = eyeptr[i]; ptr != EOL; ptr = link[ptr]) {
			++eptcnt;
	   		if (!setflag(ptr))
	   			outerr("eyeptr");
			}
		}
   amfcnt = 0;
   for (ptr = armyfreelist; ptr != EOL; ptr = link[ptr]) {
	army = (army_t)list[ptr];
	if (armylibs[army] != 0) {
		sprintf(buf,"army %d in freelist with %d libs\n",army,armylibs[army]);
      outerr(buf);
		retval = FALSE;
		}
	if (armylbp[army] != EOL) {
		sprintf(buf,"armylbp for army %d nonempty\n",army);
      outerr(buf);
		retval = FALSE;
		}
   		if (!setflag(ptr))
   			outerr("armyfreelist");
	++amfcnt;
	}

   if (amfcnt + armiesused != NUMARMIES-1) {
      sprintf(buf,"lost armies used %d free %d total %d\n",armiesused,amfcnt,
          NUMARMIES-1);
      outerr(buf);
      retval = FALSE;
      }

	for (ptr = charmy; ptr != EOL; ptr = link[ptr]) {
		charmycnt ++;
   		if (!setflag(ptr))
   			outerr("charmy");
		}

	if (chalive != EOL) {
		sprintf(buf,"chalive not empty!\n");
		outerror(buf);
		retval = FALSE;
		}

	lklcnt = 0;
	for (ptr = lookldr; ptr != EOL; ptr = link[ptr]) {
		lklcnt++;
   		if (!setflag(ptr))
   			outerr("lookldr");
		}

	for (ptr = cnchgd; ptr != EOL; ptr = link[ptr]) {
		cnchcnt++;
   		if (!setflag(ptr))
   			outerr("cnchgd");
		}
   
	if (!checkpatterns(&npbcnt))retval = FALSE;

	if (evrscnt != evtcnt) {
		sprintf(buf,"vital point lists are different sizes %d %d\n",evrscnt,evtcnt);
		outerr(buf);
		retval = FALSE;
		}
	
	for (ptr = hcpoints; ptr != EOL; ptr = link[ptr]) {
		hcpcnt++;
   		if (!setflag(ptr))
   			outerr("hcpoints");
		}

	total = ldrcnt+lfgcnt+nbncnt+cncnt+ltot+eyecnt+tercnt+mvkcnt+bstcnt+
           cscnt+lbpcnt+nbpcnt+cnbcnt+dgrcnt+amycnt+cnfcnt+amfcnt+lklcnt+
           cnlkcnt+cnllcnt+lkbcnt+llbcnt+eyedcnt+reasoncnt+nblbcnt+shpcnt +
		amlcnt+atvcnt+aeycnt+eptcnt+evtcnt+efrcnt+evrscnt+undocnt+
		hghcnt+hcpcnt+arncnt+aptnum+amncnt+npfcnt+npbcnt+sphghcnt +
			charmycnt+cnchcnt+chgrpcnt+pseqcnt+nbgrpcnt+swapcnt+boundcount+cnpathcnt;

   if (total > NUMLIST-2000) {
	   sprintf(buf,"DSCHECK warning: used %d list elements out of %d\n",
		  total, NUMLIST);
      outerr(buf);          
      }


	lcount = 0;
	ptr = freelist;
	while(ptr != EOL) {
   		if (!setflag(ptr))
   			outerr("freelist");
		lcount ++;
		ptr = link[ptr];
		}
	if (NUMLIST-1-lcount-total != 0 || flag) {					
		sprintf(buf,"boundary %d\n",boundcount);
		outerr(buf);
		sprintf(buf,"grldr %d, ladderflag %d\n",ldrcnt,lfgcnt); 
		outerr(buf);
		sprintf(buf,"grnbp %d, grcnp %d\n",nbncnt,cncnt); 
		outerr(buf);
		sprintf(buf,"liberty lists used %d\n",ltot);
		outerr(buf);
		sprintf(buf,"eyelist list %d\n",eyecnt);
		outerr(buf);
		sprintf(buf,"best sequence lists %d\n",bstcnt);
		outerr(buf);
		sprintf(buf,"lookldr list %d\n",lklcnt);
		outerr(buf);
		sprintf(buf,"terhd list %d\n",tercnt);
		outerr(buf);
		sprintf(buf,"mvcapt/mvconn list %d\n",mvkcnt); 
		outerr(buf);
		sprintf(buf,"cnptr list %d cnlkptr %d cnllptr %d cnbrd %d lkbrd %d llbrd %d path\n",
			cscnt,cnlkcnt,cnllcnt,cnbcnt,lkbcnt,llbcnt,cnpathcnt); 
		outerr(buf);
		sprintf(buf,"lbply %d, nbply %d  ",lbpcnt, nbpcnt);
		outerr(buf);
		sprintf(buf,"deadgr %d\n",dgrcnt);
		outerr(buf);
		sprintf(buf,"armygroup %d armylibs %d armynbrs %d armyvital %d armyeyes %d\n",amycnt,amlcnt,amncnt,atvcnt,aeycnt);
		outerr(buf);
		sprintf(buf,"cnfreelist %d\n",cnfcnt);
		outerr(buf);
		sprintf(buf,"armyrun %d armypot %d armyfreelist %d\n",arncnt,aptnum,amfcnt);    
		outerr(buf);
		sprintf(buf,"stratreason %d\n",reasoncnt);
		outerr(buf);
		sprintf(buf,"nblbcnt %d pass_seq %d\n",nblbcnt,pseqcnt);     
		outerr(buf);
		sprintf(buf,"shpcnt %d newpatcount %d highlighted %d, spechigh %d, hcaps %d\n",shpcnt,npbcnt,hghcnt,sphghcnt, hcpcnt);
		outerr(buf);
		sprintf(buf,"eptcnt %d, evtcnt %d, efrcnt %d evrscnt %d\n",eptcnt,evtcnt,efrcnt,evrscnt);
		outerr(buf);
		sprintf(buf,"charmycnt %d, cnchcnt %d\n",charmycnt,cnchcnt);
		outerr(buf);
		sprintf(buf,"new pat free list %d\n",npfcnt);
		outerr(buf);
		sprintf(buf,"lost list %d\n",NUMLIST-1-lcount-total);
		outerr(buf);
		sprintf(buf,"Using %d list elements (%d free) out of %d\n",total,lcount,NUMLIST);
		outerr(buf);
		sprintf(buf,"Using %d groups of %d\n",maxgr,NUMGROUPS);
		outerr(buf);
		sprintf(buf,"Using %d strats of %d\n",nextstrat,NUMSTRATS);
		outerr(buf);
		sprintf(buf,"Using %d potentials of %d\n",nextpot,NUMPOTENTIAL);
		outerr(buf);
		sprintf(buf,"Using %d armies of %d\n",NUMARMIES-amfcnt,NUMARMIES);
		outerr(buf);
		sprintf(buf,"Using %d connections of %d\n",NUMCONNS-cnfcnt,NUMCONNS);
		outerr(buf);
		sprintf(buf,"Using %d eyes out of %d\n",NUMEYERECS-efrcnt-1,NUMEYERECS-1);
		outerr(buf);
		if (NUMLIST-1-lcount-total != 0)
			retval = FALSE;
		for (i = 0; i < NUMLIST-1; ++i)
	      if (!listflag[i]) {
		      sprintf(buf,"Lost list element %d value %d link %d\n",
			     i,list[i],link[i]);
            outerror(buf);
			}
      }
   if (total > maxlist)maxlist = total;
   if (maxgr > maxgroup)maxgroup = maxgr;
   if (nextstrat > maxstrat)maxstrat = nextstrat;
   if (nextpot > maxpot)maxpot = nextpot;
   if (NUMARMIES-amfcnt > maxarmy)maxarmy = NUMARMIES-amfcnt;
   if (NUMCONNS-cnfcnt > maxconn)maxconn = NUMCONNS-cnfcnt;
   if (NUMEYERECS-efrcnt > maxeye)maxeye = NUMEYERECS-efrcnt;
   if (!retval) {
	   outerror("dscheck found error!\n");
	   if (!problemflag)
	   		turnoffcplay();
	   }
	else if (flag)
		outerr("Check passed\n");
   return(retval);
   }

#endif  /* of ifdef TEST */

#endif  /* of ifdef CHECK */
