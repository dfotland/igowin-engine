/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */
#include "g2hd.h"
#include "g2dist.h"
#include "g2fcache.h"
#ifdef G2DEBUGOUTPUT
#include <stdio.h>
#endif

int iskopoint(sqr_t s, int *color);
static int loses_all_semeai(army_t army, army_t winner, int rval);
static int winsemnbr(army_t army);
int getpoteyes(army_t army, int *min);
static int geteyespace3(army_t a1, army_t a2, army_t a3);
static int cnshreyespace(army_t olda, army_t army, int c, int cn);
static int cnshreyepot(army_t olda, army_t army, int c, int cn);
static int geteyespace(army_t a1, army_t a2);
static int ucutdist(sqr_t s, int sdir, int dir, int c);
static void deadgroups(army_t army);
static int th_terr(army_t army, army_t a);
static int th_conns(army_t army, army_t a);
static int getnumeyes(army_t army, short *max);
static void markspot(sqr_t);
static void markpcls(int);
static void findcaptured(int);
static void newdeadgroup(group_t,int,int);
static void fixwasthgroup(group_t);
static void fixwasdeadgroup(group_t);
static void fixarmies(void);
static void fixgralive(void);
static void initarmyalive(army_t);
static int startalive(army_t, int *);
static void getarmylibs(army_t);
static void getarmyex_pot(army_t);
static void check_ex(sqr_t,int,int,army_t,int,int);
static void getarmyuc_pot(army_t);
static void getarmycn_pot(army_t);
static void cnthreat(army_t,group_t,group_t,int);
extern void getarmytv_pot(army_t);
static int threat_live(army_t);
static int connect_live(army_t);
void getarmycnrn_pot(army_t); 
static void getarmyrn_pot(army_t);
static void getarmywk_pot(army_t);
static void semeaialive(army_t);
static int miaialive(army_t army);
static void weakalive(army_t);
static void promoteweak(army_t army);
static void newalive(army_t,int,int, int);
static void donewbest(army_t army, list_t *);

# define MAXTERR 18

list_t splitlist;	/* list of groups whose armies need splitting */    
listval_t nextpot;  /* next free potential record */

/* scoring philosophy:
 *
 * internal scores are kept Chinese style, in the range of +50 per square
 * to -50 per square, where +50 is one point for white and -50 is
 * one point for black.
 *
 * A live white stone gets score = +50, a dead white stone gets -50, and
 * and unsettled white stone gets 0.
 *
 * The score of a position is the sum of the scores for each of the squares
 * plus a correction for sente and threatened groups.  The side to move can
 * add capturing the biggest threatened enemy group or saving the biggest
 * threatened friendly group or the value of sente, whichever is biggest.
 * Value of capturing or saving a threatened group is 50 per stone or liberty.
 * (We assume that capturing or saving threatened groups will be gote)
 *
 * There is another correction for dead groups inside dead groups, which
 * are actually alive.
 *
 * The value of a move is the difference between the score after that move
 * is made and the score after a pass is made.
 *
 * The value of sente (obaval) is 7 points at the beginning of the game
 * (since black wins by 7 with the first move
 * and declines to 1 point at the end.  Added are points for sente attacks on 
 * enemy groups
 */

extern int pfac[NUMALIVE];
extern int sumeyes[41], sumeyesmax[41], sumpots[41],terr1[5],terr2[5],terr3[5],terr4[5],terr5[5][6];
extern int cfac[3];
extern int pfac8[16], pfac11[16], pfac16[16]; 

/* since live is total run adds up to 16,
 * need two of 0, 3 of 1,2, or 4 of 3, or a 0 and 2 of 1,2,3
 * and big groups will have some other low valued ones to add as well.
 * use value 1 for a running place where you really can't run at all
 * no matter how many of value 1 there are, they will only total to 1.
 */                 
                 /* value for 7 must be same or less than value for 3 */
/* 7 and 8 double count with exensions, so keep value low (was 5,2)  5/99 */
int runval[NUMRUN] =  { 9, 8, 7, 5, 4, 3, 2, 4, 1, 1, 5, 2, 3, 1 };
int run2val[NUMRUN] = { 5, 4, 4, 3, 2, 2, 1, 3, 1, 1, 3, 1, 1, 1 };  /* from 2 stone wall - must be >= 1/2 of runval */	


/* what is runval left if opponent moves first here */
int minrunval[NUMRUN] = { 4, 3, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};

/* see if g (which is a DEAD group) is inside very weak groups
 * which means that g is actually alive!
 */

int deadinsidedead(group_t g) {
	list_t ptr;
	if (armynbp[grarmy[g]] == EOL)getarmynbp(grarmy[g]);
	for (ptr = armynbp[grarmy[g]]; ptr != EOL; ptr = link[ptr])
		if (A_ALIVE(list[ptr]) <= UNSETTLED)
			return(FALSE);
	if (armynbp[grarmy[g]] == EOL)return(FALSE);
	return(TRUE);
}

static void markldr(sqr_t s)
{
	list_t ptr, ptr2;
	int bptr;
	for (ptr = ldrflag[s]; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] >= NUMGROUPS+NUMCONNS) {
			if (eyeptr[list[ptr]-NUMGROUPS-NUMCONNS] != EOL) {
				adflist(list[eyeptr[list[ptr]-NUMGROUPS-NUMCONNS]], &eyelist);
			}
#ifdef CHECK					      
			else {
   				outerror("eyerec error grpieces\n");
			}
#endif	    		      	
		}
		else if (list[ptr] >= NUMGROUPS)
			addlist((listval_t)(list[ptr]-NUMGROUPS),&cnchgd);
		else {	/* mark the group and any eye that depends on this group */
			gralive[list[ptr]] |= 32;
			for (bptr = grpieces[list[ptr]]; bptr != -1; bptr = mvnext[bptr]) {
				for (ptr2 = ldrflag[mvs[bptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (list[ptr2] >= NUMGROUPS+NUMCONNS) {
						if (eyeptr[list[ptr2]-NUMGROUPS-NUMCONNS] != EOL) {
							adflist(list[eyeptr[list[ptr2]-NUMGROUPS-NUMCONNS]], &eyelist);
						}
					}
				}
			}
		}
	}
}

static void markspot_all(sqr_t s) {
	list_t ptr;
	if (eyerec[s] != 0)
		adflist(list[eyeptr[eyerec[s]]],&eyelist);
	if (cnbrd[s] != EOL)
		mrglist(cnbrd[s],&cnchgd);
	if (lkbrd[s] != EOL)
		mrglist(lkbrd[s],&cnchgd);
	if (llbrd[s] != EOL)
		mrglist(llbrd[s],&cnchgd);
	if (ollbrd[s] != EOL)
		mrglist(ollbrd[s],&cnchgd);
	mrglist(ldrflag[s],&lookldr);
	if (board[s] != NOGROUP) {
		gralive[board[s]] |= 32;
		mrglist(grcnp[board[s]],&cnchgd);  /* all connections to this group */
		if (grlibs[board[s]] < 4) {
		      /* reeval neighbors if low on liberties */
			for (ptr = grnbp[board[s]]; ptr != EOL; ptr = link[ptr])
				gralive[list[ptr]] |= 32;
		}
	}
	markldr(s);
}

static void markspot(sqr_t s)
{
	int bptr;	/* mvs ptr, not list ptr */
	list_t ptr;
	if (eyerec[s] != 0)
		adflist(list[eyeptr[eyerec[s]]],&eyelist);
	if (cnbrd[s] != EOL)
		mrglist(cnbrd[s],&cnchgd);
	if (lkbrd[s] != EOL)
		mrglist(lkbrd[s],&cnchgd);
	if (llbrd[s] != EOL)
		mrglist(llbrd[s],&cnchgd);
	if (ollbrd[s] != EOL)
		mrglist(ollbrd[s],&cnchgd);
	mrglist(ldrflag[s],&lookldr);
	if (board[s] != NOGROUP) {
		gralive[board[s]] |= 32;
		mrglist(grcnp[board[s]],&cnchgd);  /* all connections to this group */
		if (grlibs[board[s]] < 4) {
		      /* reeval neighbors if low on liberties */
			for (ptr = grnbp[board[s]]; ptr != EOL; ptr = link[ptr])
				gralive[list[ptr]] |= 32;
		}

	      /* in case move added stone to group */
		for (bptr = grpieces[board[s]]; bptr != -1; bptr = mvnext[bptr]) {
			markldr(mvs[bptr]);
		}
	}
	else { /* empty point */
		markldr(s);
	}
}

/* look at places where pieces were added or removed from board since last
 * evaluation.  mark neighboring groups, connections
 * for reevaluation.  Eyes are marked in g2eye.c in findeyelist
 * if everything is TRUE, all groups will be reevaluated.
 * if everything is 2, mark all connections for evaluation.
 */
  
static void markpcls(int everything) {
	list_t ptr;
	sqr_t s, sn, sn2, sn3, lptr;
	int i, ldtmp, j, ldtm2, k, ldtm3, pptr;
	list_t seen = EOL, markseen = EOL;
	conn_t cn;
	for (ptr = lookldr; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] >= NUMGROUPS+NUMCONNS) {
			if (eyeptr[list[ptr]-NUMGROUPS-NUMCONNS] != EOL)
				adflist(list[eyeptr[list[ptr]-NUMGROUPS-NUMCONNS]],&eyelist);
		}
		else if (list[ptr] >= NUMGROUPS) {
			cn = list[ptr]-NUMGROUPS;
			if (cncnum[cn] != 0 || cnlknum[cn] != 0 || cnllnum[cn] != 0 || cnddnum[cn] != 0 || cnollnum[cn] != 0)
				addlist(cn,&cnchgd);
	    }
		else/* if (gralive[list[ptr]] != DEAD) */
			gralive[list[ptr]] |= 32;
	}
	killist(&lookldr);
	if (everything == 2 || pclsnext > 20) {  /* just do them all */
		for (lptr = 0; lptr < boardsquare; ++lptr) {
			markspot_all(lptr);
			if (ld[lptr] >= 4 && ld[lptr] <= 8 ||
				ltrgd[lptr] == 0 && ltr1[lptr] != 0 && ld[lptr] != NOLD)
				adflist(lptr,&eyelist);
		}
		return;
	}

	for (pptr = 0; pptr < pclsnext; ++pptr) {
		s = pcls[pptr];
		if (!addlist(s, &seen)) {
			continue;
		}
		if (addlist(s, &markseen)) {
			markspot(s);
		}

		i = fdir[s];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = s + nbr[i];
			if (board[sn] != NOGROUP && board[sn] != board[s]) {
				if (addlist(sn, &markseen)) {
					markspot(sn);  /* another group */
				}
			}
			else if (board[sn] == NOGROUP) {  /* empty spot */
				markspot(sn);
				j = fdir[sn];
				for (ldtm2 = ldir[j]; j < ldtm2; ++j) {
					sn2 = sn + nbr[j];
					if (sn2 == s)continue;
					if (board[sn2] != NOGROUP && board[sn2] != board[s] ) {
						if (addlist(sn2, &markseen)) {
							markspot(sn2);
						}
					}
					else if (board[sn2] == NOGROUP) {  /* empty spot */
						markspot(sn2);
						k = fdir[sn2];
						for (ldtm3 = ldir[k]; k < ldtm3; ++k) {
							sn3 = sn2 + nbr[k];
							if (sn3 == sn)continue;
							if (addlist(sn3, &markseen)) {
								markspot(sn3);
							}
						}
					}      
				}
			}
		}
	}
	killist(&seen);
	killist(&markseen);
}

/* get rid of the ladder flags for ladder g (group number, connection, or
 * eye.
 */

void kill_ldrflags(listval_t g) {  /* must be int since range bigger than NUMGROUPS */
	list_t ptr;
	for (ptr = grldr[g]; ptr != EOL; ptr = link[ptr])
		dellist(g,&ldrflag[list[ptr]]);
	killist(&grldr[g]);
}


/* return TRUE if group g can't be captured if it moves first.
 * can't be captured if it gets mlibs+1 in one move, or if it
 * can capture a neighboring group big enough.  Assume g wins kos.
 */

int cantbecaptured(group_t g, int mlibs) {
	list_t tmplist = EOL, ptr, ptr2;
	group_t g2;
	int libs,maxlibs,cn;
	int libstoescape,snap;
	libstoescape = mlibs+1;
	maxlibs = grlibs[g];
	/* look for neighboring groups which can be captured */

	if (maxlibs < libstoescape) {
	    tmplist = EOL;
	    for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
			g2 = (group_t)list[ptr];
			if (grlibs[g2] == 1) {
				snap = FALSE;
				if (grsize[g2] == 1) {
					for (ptr2 = grnbp[g2]; ptr2 != EOL; ptr2 = link[ptr2])
						if (grlibs[list[ptr2]] == 1 &&
							list[grlbp[list[ptr2]]] == 
							list[grlbp[g2]] && 
							lnbn[list[grlbp[g2]]] == 0) {
							snap = TRUE;
							break;  /* snapback */
							}
					}
				if (snap)continue;
				grsavemove[g] = list[grlbp[g2]];
				libs = grlibs[g];
				cpylist(grlbp[g],&tmplist);
				libs += grsize[g2];
				if (mrglist(grlbp[g2],&tmplist) == 0)--libs;
/*				if (grlibs[g] > 1)	/* 3/04 play it out if only one liberty for under stones */
				  for (ptr2 = grnbp[g2]; ptr2 != EOL; ptr2 = link[ptr2])
					if (list[ptr2] != g)
						libs += mrglist(grlbp[list[ptr2]],&tmplist);
				killist(&tmplist);
				if (libs > maxlibs) {
					maxlibs = libs;
					addldrflag(list[grlbp[g2]],g);
					}
				if (maxlibs >= libstoescape)break;
				}
			}
	    }

	/* look for extensions and connections for new libs */

	if (maxlibs < libstoescape || 
		grsavemove[g] == kosquare)	/* look for a better move than taking a ko */
	    for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
	    	addldrflag(list[ptr],g);
			libs = grlibs[g];
			cpylist(grlbp[g],&tmplist);
			libs += mrglist(nblbp[list[ptr]],&tmplist)-1;
			for (ptr2 = cnbrd[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
				cn = list[ptr2];
				if (cngr1[cn] != g && cngr2[cn] != g)continue;
				if (cngr1[cn] == g)
					g2 = cngr2[cn];
				else
					g2 = cngr1[cn];
				if (cncnum[cn] != 0 || grlibs[g] > 2)
					libs += mrglist(grlbp[g2],&tmplist);
				}
			if (libs > maxlibs) {
				maxlibs = libs;
				}
			killist(&tmplist);
			grsavemove[g] = list[ptr];
			if (maxlibs >= libstoescape)break;
			}
	if (maxlibs >= libstoescape) {
		return(TRUE);
		}
	return(FALSE);
	}


/* canbethreatened returns true if a group can possibly be captured if
 * the opponent moves first
 */


int canbethreatened(group_t g)
{
	int maxlibs,mlibs;
	if (grlibs[g] > taclibs[playlevel]) {
		return FALSE;
	}
	maxlibs = getefflibs(g,taclibs[playlevel],g,&mlibs);
	if (maxlibs > taclibs[playlevel]) {
		return FALSE;
	}
	return TRUE;
}

/* return TRUE if g is a cutting stone (conservatively).  armies aren't figured
 * out yet so just look at nearby groups
 */

int cutstone(group_t g)
{
	if (grnbp[g] == EOL || link[grnbp[g]] == EOL)
		return FALSE;
	return TRUE;
}

/* findcaptured finds all the groups which are tactically simple to
 * capture even if group moves first and wins all kos.  These contribute to
 * eyes and unbreakable connections.
 */

static void findcaptured(int everything)
{
	group_t g,g2;
	list_t comblist = EOL;  /* new dead groups, combine armirs */
	list_t ptr,ptr2;
	int lvl;
/*	if (noladder)return; */
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g])continue;
		if (!everything && gralive[g] < 32)continue;

		if (grldr[g] != EOL)kill_ldrflags(g);
		lvl = cancapsize[playlevel]*2;
		if (grlibs[g] > taclibs[playlevel] || cantbecaptured(g,taclibs[playlevel])) {
			if (gralive[g] == DEAD)gralive[g] |= 32;
		}
		else if (iscaptured(g,80,lvl,taclibs[playlevel],mvmost[playlevel],grcolor[g],g,&grsavemove[g],grcolor[g])) {
			newdeadgroup(g, DEAD, pfac[DEAD]);
			addlist(g,&comblist);
		}
		else if (gralive[g] == DEAD) {
			gralive[g] |= 32;
		}
		if (gralive[g] == (32 | DEAD))
   			fixwasdeadgroup(g);
	}
	for (ptr = comblist; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[ptr];
   		for (ptr2 = grnbp[g]; ptr2 != EOL; ptr2 = link[ptr2])
	 		if (gralive[list[ptr2]] != DEAD && 
			   !G_THREATENED(list[ptr2]))break; 
   		if (ptr2 != EOL) {
	   		g2 = (group_t)list[ptr2];
	   		addlist(g,&armydeadgroups[grarmy[g2]]);
   			grdeadarmy[g] = grarmy[g2];
       		for (ptr2 = link[ptr2]; ptr2 != EOL; ptr2 = link[ptr2]) {
       			if (gralive[list[ptr2]] != DEAD &&
				   !G_THREATENED(list[ptr2])) {
       				combinearmy(grarmy[list[ptr2]],grarmy[g2]);
				}
			}

    	}
   	}
   	killist(&comblist);
}



/* bdead finds groups which are easily captured so they can count
 * as connections and eyes, and groups that are threatened with capture
 */

extern int numgrtacnodes;

static void bdead(int everything)
{
	group_t g;
	int flag, color;
	sqr_t workingmove;

	findcaptured(everything);	/* find DEAD groups */
	flag = everything;
  
	for (g = 0; g < maxgr; ++g) {  /* find threatened groups */
		if ( !grlv[g]) {
			continue;
		}
		if (gralive[g] == DEAD) {
			if (G_THREATENED(g)) {
				grthreatened[g] = FALSE;
				fixwasthgroup(g);
			}
			continue;
		}
		if (flag || gralive[g] >= 32) {   /* found ladder cantidate */

			if (grlibs[g] == 1 && gralive[g] != DEAD && !snapback(g, 1-grcolor[g])) {
				if (iskopoint(list[grlbp[g]], &color)) {
					grthreatened[g] = TRUE;
				} else {
					grthreatened[g] = 2;
				}
				grcapmove[g] = list[grlbp[g]];
				newdeadgroup(g, WEAK_GROUP, pfac[WEAK_GROUP]);
				gralive[g] |= 32;
			}
			else if ((grlibs[g] > 1) &&
				gralive[g] != DEAD &&
				canbethreatened(g)) {
				if (iscaptured(g,80,cancapsize[playlevel],taclibs[playlevel],mvmost[playlevel],1-grcolor[g],g,&grcapmove[g],1-grcolor[g])) {
					if (iscaptured(g,80,cancapsize[playlevel],taclibs[playlevel],mvmost[playlevel],1-grcolor[g],g,&workingmove,grcolor[g])) {
						grcapmove[g] = workingmove;  /* change capturing move to reflect certain capture */
						if (grthreatened[g] != 2) {
							grthreatened[g] = 2;  /* unconditionally threatened */
							newdeadgroup(g, WEAK_GROUP, pfac[WEAK_GROUP]);
						}
					}
					else if (grthreatened[g] != 1) {  /* this if experimental - 10/1/94 */
						grthreatened[g] = 1;
						newdeadgroup(g, WEAK_GROUP, pfac[WEAK_GROUP]);
					}
					gralive[g] |= 32;
				}
				else if (G_THREATENED(g)) {
					grthreatened[g] = FALSE;
					fixwasthgroup(g);
				}
			}
			else if (G_THREATENED(g)) {
				grthreatened[g] = FALSE;
				fixwasthgroup(g);
			}
			if (gralive[g] == DEAD + 32) {	/* 9/05 former dead groups get lower alive value for later checks of attributes */
				newdeadgroup(g, WEAK_GROUP, pfac[WEAK_GROUP]);
				gralive[g] |= 32;
			}
		} 
	}  /* end of groups loop */
}
  
  
static void newdeadgroup(group_t g,int newalive,int newalprob) {
	list_t ptr,ptr2;
	sqr_t s;
	armyeyes[grarmy[g]] = 0;
	killist(&armyeyerecs[grarmy[g]]);
	killist(&armyvitalpoints[grarmy[g]]);

	if (newalive == DEAD)
		grcapmove[g] = grsavemove[g] = NOSQUARE;

	if (grdeadarmy[g] != NOARMY) { 
		addlist(list[armygroups[grdeadarmy[g]]],&splitlist);
	}

	if (armysize[grarmy[g]] > grsize[g]) {
		addlist(g,&splitlist);
	}
	else if (newalive == DEAD) {  /* wont be split, so delete dead armies here - dead group can't have deadarmies */
		for (ptr = armydeadgroups[grarmy[g]]; ptr != EOL; ptr = link[ptr]) {
			grdeadarmy[list[ptr]] = NOARMY;
		}
		killist(&armydeadgroups[grarmy[g]]);
	}
#ifdef CHECK
	if (newalive > DEAD)
		outerror("Bad aliveness in newdeadgroup\n");
#endif
	pscr = pscr + (newalprob - gralprob[g])*grsize[g]*
		cfac[grcolor[g]];
	gralive[g] = newalive;
	gralprob[g] = gralval[g][0] = gralval[g][1] = newalprob;

	markgroup(g);

	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (gralive[lgr[s]] != DEAD)
			lgr[s] = g;
	}
	mrglist(grcnp[g],&cnchgd);  /* not redundant! */
	if (G_THREATENED(g))
		for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr])
			for (ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
				if (cncnum[list[ptr2]] == 1)
					addlist(list[ptr2],&cnchgd);
}

/* group g changed from dead/notdead or threatened/not threatened, so
 * mark anything relevant for reevaluation 
 */

void markgroup(group_t g) { 
	int mptr;  /* mvs ptr, not list ptr */
	list_t ptr,ptr2,ptr3,elist = EOL;
	sqr_t sn;
	int x,y,i,ldtmp;
#ifdef CHECK
	if (g >= NUMGROUPS || !grlv[g])
		outerror("Markgroup bad group\n");
#endif
	for (mptr = grpieces[g]; mptr != -1; mptr = mvnext[mptr])
		for (ptr2 = ldrflag[mvs[mptr]]; ptr2 != EOL; ptr2 = link[ptr2])
			if (list[ptr2] >= NUMGROUPS+NUMCONNS) {
				if (eyeptr[list[ptr2]-NUMGROUPS-NUMCONNS] != EOL)
					addlist((listval_t)(list[ptr2]-NUMGROUPS-NUMCONNS),&elist);
				}
			else if (list[ptr2] >= NUMGROUPS)
				addlist((listval_t)(list[ptr2]-NUMGROUPS),&cnchgd);
   	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
#ifdef CHECK
	    if (list[ptr] > 360)
	    	outerror("terhd error markgroup\n");
#endif
		if (cnbrd[list[ptr]] != EOL)
			mrglist(cnbrd[list[ptr]],&cnchgd);
		if (lkbrd[list[ptr]] != EOL)
			mrglist(lkbrd[list[ptr]],&cnchgd);
		if (llbrd[list[ptr]] != EOL)
			mrglist(llbrd[list[ptr]],&cnchgd);
		if (ollbrd[list[ptr]] != EOL)
			mrglist(ollbrd[list[ptr]],&cnchgd);
		if (eyerec[list[ptr]] != 0)addlist(eyerec[list[ptr]],&elist);
		if (!S_NEUTRAL(list[ptr]) && ld[list[ptr]] > 5)
			adflist(list[ptr],&eyelist);
		i = fdir[list[ptr]];
		for (ldtmp = ldiag[i]; i < ldtmp; ++i) {
			if (board[list[ptr]+diags[i]] != g && eyerec[list[ptr]+diags[i]] != 0)
				addlist(eyerec[list[ptr]+diags[i]],&elist);
			}
		for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (cnbrd[list[ptr2]] != EOL)
				mrglist(cnbrd[list[ptr2]],&cnchgd);
			if (lkbrd[list[ptr2]] != EOL)
				mrglist(lkbrd[list[ptr2]],&cnchgd);
			if (llbrd[list[ptr2]] != EOL)
				mrglist(llbrd[list[ptr2]],&cnchgd);
			if (ollbrd[list[ptr2]] != EOL)
				mrglist(ollbrd[list[ptr2]],&cnchgd);
			if (eyerec[list[ptr2]] != 0)
				addlist(eyerec[list[ptr2]],&elist);
			i = fdir[list[ptr2]];
			for (ldtmp = ldiag[i]; i < ldtmp; ++i) {
				if (board[list[ptr2]+diags[i]] != g && eyerec[list[ptr2]+diags[i]] != 0)
					addlist(eyerec[list[ptr2]+diags[i]],&elist);
				}
			for (ptr3 = nblbp[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3]) {
				if (list[ptr3] == list[ptr])continue;
				if (lkbrd[list[ptr3]] != EOL)
					mrglist(lkbrd[list[ptr3]],&cnchgd);
				if (llbrd[list[ptr3]] != EOL)
					mrglist(llbrd[list[ptr3]],&cnchgd);
				if (ollbrd[list[ptr3]] != EOL)
					mrglist(ollbrd[list[ptr3]],&cnchgd);
				}
			}			
		}
   	for (mptr = grpieces[g]; mptr != -1; mptr = mvnext[mptr]) {
   		sn = mvs[mptr];
		adflist(sn,&eyelist);
		i = fdir[sn];
		for (ldtmp = ldiag[i]; i < ldtmp; ++i) {
			if (cnbrd[sn+diags[i]] != EOL)
				mrglist(cnbrd[sn+diags[i]],&cnchgd);
			if (board[sn+diags[i]] != g && eyerec[sn+diags[i]] != 0)
				addlist(eyerec[sn+diags[i]],&elist);
			}
		x = xval[sn];
		y = yval[sn];
		if (x-4 < xmin)xmin = x-4;
		if (x+4 > xmax)xmax = x+4;
		if (y-4 < ymin)ymin = y-4;
		if (y+4 > ymax)ymax = y+4;
   		}
	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr])
		if (eyerec[list[ptr]] != 0)
			addlist(eyerec[list[ptr]], &elist);
   	for (ptr = elist; ptr != EOL; ptr = link[ptr])
   		adflist(list[eyeptr[list[ptr]]],&eyelist);
   	killist(&elist);
	}


/* group was threatened.  Must update its connections and territory
 * if nbr has THREAT connections, they must be reevaled.
 */


static void fixwasthgroup(group_t g) {
	list_t ptr,ptr2;
	markgroup(g);
	grcapmove[g] = grsavemove[g] = NOSQUARE;  /* only threatened groups have these moves */
	for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr])
		for (ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
			if (cntype[list[ptr2]] == CN_THREAT)
				addlist(list[ptr2],&cnchgd);

	if (gralive[g] != DEAD)
   	    for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {

      		if (gralive[list[ptr]] == DEAD) {
          		if (grdeadarmy[list[ptr]] != NOARMY) {
         			combinearmy(grarmy[g],grdeadarmy[list[ptr]]);
			}
			else {
				grdeadarmy[list[ptr]] = grarmy[g];
				addlist(list[ptr],&armydeadgroups[grarmy[g]]);
			}
		}

	}
}

/* group was dead and is no more.  must update its armies, connections,
 * eyes, and territory.  Mark nearby connections for reeval.
 * change its alive to 33 so patterns don't match dead
 */

static void fixwasdeadgroup(group_t g) {
	list_t ptr;
	int i,ldtmp;
	if (grdeadarmy[g] != NOARMY) {
		addlist(list[armygroups[grdeadarmy[g]]],&splitlist);
		dellist(g,&armydeadgroups[grdeadarmy[g]]);
		grdeadarmy[g] = NOARMY;
		}
	for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
		if (!S_NEUTRAL(list[ptr]))
			adflist(list[ptr], &eyelist);
		i = fdir[list[ptr]];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			if (S_ALIVE(list[ptr]+nbr[i]) == DEAD)
				lgr[list[ptr]] = board[list[ptr]+nbr[i]];
			}
		}

	markgroup(g);

	for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {

		if (gralive[list[ptr]] == DEAD) {
			if (grdeadarmy[list[ptr]] != NOARMY) {
				combinearmy(grarmy[g],grdeadarmy[list[ptr]]);
				}
			else {
				grdeadarmy[list[ptr]] = grarmy[g];
				addlist(list[ptr],&armydeadgroups[grarmy[g]]);
				}
			}
		
		}
	}


static void fixarmies(void) {
   list_t split1,ptr;
   split1 = EOL;        /* fix up armies */
   for (ptr = splitlist; ptr != EOL; ptr = link[ptr])
      addlist(grarmy[list[ptr]],&split1);
   killist(&splitlist);
   for (ptr = charmy; ptr != EOL; ptr = link[ptr]) {
      if (board[list[ptr]] != NOGROUP)
         addlist(grarmy[board[list[ptr]]],&split1);
      }
   killist(&charmy);
   for (ptr = split1; ptr != EOL; ptr = link[ptr])
      splitarmy((army_t)list[ptr]);
   killist(&split1);
   }

/* starts 8 points, reduces to 1/2 point at 1/2 boardsize moves 
 * starting value should be the komi, plus a point to account for chinese counting, so 8 points
 * value of making an oba - a big territory only gote move.
 * should be about half the score change due to a move, so that oba is always added to the side to move to make the 
 * score independent of who is to move.
 * starts larger on smaller boards
 * oba must be smaller than actual temperature of the game to avoid strange moves
 * so start it at 7 rather than 8
 */

/* min oba must be greater than 25 since that is value of filling dame */
/* must be less than 100 to avoid stupid ataris at the end of the game */
/* since add oba after each move, it should be half the value of a big move
 * at this point in the game, so it should end at 1/2 point */

# define MINOBAVAL 25
#define MAXOBAVAL (7*50)
 
void getobaval()
{
	double progress;
	int lastmove;  /* last move for reducing obaval */
	if (problemflag == SOLVEPROBLEM || problemflag == SOLVELDPROBLEM) {
		obaval = 150;
		return;
	}
	lastmove = boardsquare/2;  /* 180 for a full size board */
	if ((signed)msptr > lastmove) {
		obaval = MINOBAVAL;
	}
	progress = (((double)lastmove - msptr))/lastmove;  /* 1 to 0 from move 1 to move 180 */
	if (progress < 0)
		progress = 0;
	obaval = MINOBAVAL + (int)((MAXOBAVAL - MINOBAVAL) * progress);  /* 8 points maximum */
	if (playlevel < SENTELEVEL)
		obaval /= 2;
}

/* figure out the aliveness of all groups on board.
 * as side effect, must figure out connections and
 * radiate influence.
 *
 * if everything is FALSE, do an incremental evaluation with cached tactics, connections
 * if everything is TRUE, reevaluate life of everything, all conns, tactics.
 * if everything is 2, reevaluate all eyes as well.
 */

extern int numlifecalls, debugnumlifecalls, numlifetacnodes, numfixgrtacnodes;
extern int numcntacnodes;
extern int numeyetacnodes;

void life(int everything)
{
	int nodes;
	int lnodes = numnodes;
	if (!everything && pclsnext == 0)
	   return;				/* nothing changed - no need to reeval */
	numlifecalls++;			/* cant/must count all calls - during lookahead should be no extra ones */
							/* otherwise debug and release play differently - since no changes for pass */
	debugnumlifecalls++;
	if (pclsnext >= NUMPCLS)
		everything = 2;		/* overflowed change array */
	splitlist = EOL;
	markpcls(everything);	/* mark eyes, connections, and groups for reeval */

	nodes = numnodes;
	bdead(everything);		/* find groups which are captured, threatened - before fixli and fixcnprot */
	numgrtacnodes += (numnodes - nodes);

	fixdistance();			/* distance to nearest stones of each color */

	nodes = numnodes;
	fixcnprot(everything);	/* find protected connections - before fixarmies */
	numcntacnodes += (numnodes - nodes);

	fixarmies();			/* collect groups into armies */
   
	upltr();				/* find edge territory - before fixli since eye eval uses ltr values */
							/* after fixcnprot since ltr values depend on good connection */
	nodes = numnodes;
	fixli();				/* find eyes (must be after fixarmies!) */
	numeyetacnodes += (numnodes - nodes);

	nodes = numnodes;
	fixgralive();			/* make all aliveness values correct */
	numfixgrtacnodes += (numnodes - nodes);

	fixlgr();
	radiateterr(EOL);		/* radiate influence for territory and update liberty values */
	pclsnext = 0;
	gralvalid = TRUE;		/* gralive values are valid */
	numlifetacnodes += (numnodes - lnodes);
}

/* if a dead army has a shared connection to a not dead army,
 *  promote it to unsettled
 * if dead army has connection to weak_potential army, propmote to weak_potential 
 */

static int connalive(army_t army, int *bgl, int *bprob) {
	list_t ptr, cnptr, ptr2;
	group_t g;
	int gl, alprob, bestgl = DEAD, bestalprob = -50;
	if (A_ALIVE(army) < WEAK || A_ALIVE(army) == DEAD)
		return FALSE;
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		for (cnptr = grcnp[list[ptr]]; cnptr != EOL; cnptr = link[cnptr]) {
			g = cngr1[list[cnptr]];
			if (grarmy[g] == army)
				g = cngr2[list[cnptr]];
			if (grarmy[g] == army)
				continue;
			if (gralive[g] >= WEAK)
				continue;
			if (cnprot[list[cnptr]] >= SHARED_CONNECT) {
				gl = gralive[g];
				alprob = gralprob[g]-10;
				if (alprob < -50)
					alprob = -50;
				if (gl <= ALIVE) {
					gl = UNSETTLED_DEAD;
					alprob = 0;
					}
				if (gl < bestgl || gl == bestgl && alprob > bestalprob) {
					bestgl = gl;
					bestalprob = alprob;
				}
/*				newalive(army, gl, alprob, -50);  3/04 - get best for reproducibility */
				}
			else if (cnprot[list[cnptr]] >= CAN_CONNECT) {
				gl = WEAK_POTENTIAL;
				alprob = -48;
				if (gl < bestgl || gl == bestgl && alprob > bestalprob) {
					bestgl = gl;
					bestalprob = alprob;
				}
/*				newalive(army, gl, alprob, -50); */
				}
			}
		}
	if (armynbp[army] == EOL)
		getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		if (A_ALIVE(list[ptr]) != DEAD)continue;
		if (armynbp[list[ptr]] == EOL)
			getarmynbp((army_t)list[ptr]);
		for (ptr2 = armynbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (list[ptr2] == army)continue;
			gl = A_ALIVE(list[ptr2]);
			alprob = gralprob[list[armygroups[list[ptr2]]]]-10;
			if (alprob < -50)
				alprob = -50;
			if (gl <= ALIVE) {
				gl = UNSETTLED_DEAD;
				alprob = 0;
				}
			if (gl < bestgl || gl == bestgl && alprob > bestalprob) {
				bestgl = gl;
				bestalprob = alprob;
				}
			}
		}
	if (bestgl < DEAD) {
/*	newalive(army, bestgl, bestalprob, -50); */
		*bgl = bestgl;
		*bprob = bestalprob;
		return TRUE;
	}
	return FALSE;
}

/* fixgralive finds the new alive value for all marked groups
 * and their associated armies 
 * groups with aliveness DEAD have already been found
 * first pass finds point eyes, dead group eyes, surrounded
 * territory and the liberties of the army.  If this is enough for 2 eyes,
 * we are done.
 * second pass looks at edgeeyes, and tries to find miai for 2 eyes among
 * connecting to another army, expanding along edge, capturing threatened
 * group, playing a vital eyemaking point;
 * third pass radiates thickness, and judges weak groups based on
 * strength of surrounding groups, and ability to run away.
 */

#define MAXGL 200
extern int nummiaitacnodes;

static void fixgralive(void)
{
	list_t armylist, armylist2, miailist, changed = EOL, ptr;
	int numeyes, numsafeeyes, alprob;
	int gl[MAXGL], prob[MAXGL], nextval, i, j;
	army_t army, carmy[MAXGL];
	group_t g;
	int numn;
	
	armylist = armylist2 = miailist = EOL;
	nextpot = 1;
	for (g = 0; g < maxgr; ++g) {
		if (!grlv[g]) {
			gralive[g] &= 31;
			continue;
		}
		if (gralive[g] == DEAD) {	
			initarmyalive(grarmy[g]);
			armylibs[grarmy[g]] = grlibs[g];
			cpylist(grlbp[g], &armylbp[grarmy[g]]);
			continue;
		}
		else if (addlist(grarmy[g], &armylist))
			initarmyalive(grarmy[g]);
	}

	for (ptr = armylist; ptr != EOL; ptr = link[ptr])
		getarmylibs((army_t)list[ptr]);	/* every army gets a liberty list */

	for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
		army = (army_t)list[ptr];
		if (armygroups[army] == EOL ||
			armysize[army] == 0 || 
			list[armygroups[army]] >= NUMGROUPS ||
			!grlv[list[armygroups[army]]]) {
#ifdef CHECK
			outerror("army has no groups in fixgralive\n");
#endif
			continue;
		}
		numeyes = startalive(army,&numsafeeyes);
		armysafeeyes[army] = numsafeeyes;
		if (armyeyes[army] >= 24 || numsafeeyes >= 16) {
			if (A_THREATENED(army))
				newalive(army, UNSETTLED_THREATENED, pfac[UNSETTLED_THREATENED], 50);  /* 4/02 changed to 50 since this group is really alive */
			else
				newalive(army, HAS_TWO_EYES, pfac[HAS_TWO_EYES], -50);
		}
		else if (armyeyes[army] >= 8 && armyeyespace[army] >= 20 ||
				armyeyespace[army] >= 24) {  
			if (A_THREATENED(army))
				newalive(army,UNSETTLED_THREATENED,pfac[UNSETTLED_THREATENED],-50);
			else
				newalive(army, HAS_TWO_EYES, pfac[HAS_TWO_EYES], -50);
		}
		else
			 adflist(army, &armylist2);
			
	}
	killist(&armylist);

		/* find groups with miai for two eyes */

	numn = numnodes;
	for (ptr = armylist2; ptr != EOL; ptr = link[ptr]) {
		if (!miaialive((army_t)list[ptr])) {  /* calls all getpots and bestpot */
			adflist(list[ptr], &armylist);
		}
		else if (A_ALIVE(list[ptr]) > VERY_ALIVE) { /* 5/02 - get run for any group not very alive */
			addlist(list[ptr],&miailist);
		}
	}
	killist(&armylist2);
	nummiaitacnodes += (numnodes - numn);

	radiaterun(EOL);  /* radiate territory/thickness from alive and dead stones */
                                /* must be before rn_pot */

	/* must be after miaialive, since run value depends on extend value */
	for (ptr = miailist; ptr != EOL; ptr = link[ptr])
		getarmyrn_pot((army_t)list[ptr]);

	for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
		getarmyrn_pot((army_t)list[ptr]);
		getarmywk_pot((army_t)list[ptr]);
	}

	for (ptr = miailist; ptr != EOL; ptr = link[ptr]) {
		if (armyrn_pot[list[ptr]] < easyrun[A_COLOR(list[ptr])]) {
			if (threat_live((army_t)list[ptr]))  /* might modify threat pot record */
				addlist(list[ptr], &changed);
		}
	}

	for (ptr = armylist; ptr != EOL; ptr = link[ptr])
		getarmycnrn_pot((army_t)list[ptr]);		/* can connect for running away potential */
				/* before seki and semeai since they need to know */
			
	for (ptr = miailist; ptr != EOL; ptr = link[ptr]) /* 5/03 need cnrn for all groups since defval depends on it */
		getarmycnrn_pot((army_t)list[ptr]);		/* can connect for running away potential */
				/* before seki and semeai since they need to know */
			
	for (ptr = miailist; ptr != EOL; ptr = link[ptr]) {
		if (A_ALIVE(list[ptr]) <= MIAI && armyrn_pot[list[ptr]] > easyrun[A_COLOR(list[ptr])] &&
			armyrest[list[ptr]] > 0)  /* promote to strong miai */
			newalive((army_t)list[ptr], STRONG_MIAI, pfac[STRONG_MIAI], -50);
		else if (A_ALIVE(list[ptr]) <= MIAI) {
			alprob = pfac[A_ALIVE(list[ptr])] + armyrn_pot[list[ptr]]/2;
			if (alprob > 50)
				alprob = 50;
			newalive((army_t)list[ptr], A_ALIVE(list[ptr]), alprob, -50);	/* let running help alive strength */
		}
	}

	killist(&miailist);


	for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
		if (isseki((army_t)list[ptr])) {
			newalive((army_t)list[ptr], SEKI, pfac[SEKI], -50);
		}
	}

	for (ptr = armylist; ptr != EOL; ptr = link[ptr])
		semeaialive((army_t)list[ptr]); /* find groups that can win semeai */

	/* threat_live and connect_live must be after isseki and semeaialive */

	for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
		if (threat_live((army_t)list[ptr]))
			addlist(list[ptr], &changed);
	}

	for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
		if (connect_live((army_t)list[ptr]))                
			addlist(list[ptr], &changed);
	}

	for (ptr = armylist; ptr != EOL; ptr = link[ptr])
		getarmywk_pot((army_t)list[ptr]);  /* redo after seki and semeai */

	for (ptr = changed; ptr != EOL; ptr = link[ptr])
		donewbest((army_t)list[ptr], &armylist);	/* redo bestpot, and find new groups that are alive */
	killist(&changed);

	for (ptr = armylist; ptr != EOL; ptr = link[ptr])
		weakalive((army_t)list[ptr]);  /* evaluate aliveness of groups that are not alive*/

	/* promote 19 to 18 */
	for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
		if (A_ALIVE((army_t)list[ptr]) == WEAK)
			promoteweak((army_t)list[ptr]);
	}

	for (j = 0; j < 2; ++j) {	/* expand twice */
		nextval = 0;
		for (ptr = armylist; ptr != EOL; ptr = link[ptr]) {
			if (nextval < MAXGL && connalive((army_t)list[ptr], &gl[nextval], &prob[nextval])) {
				carmy[nextval] = (army_t)list[ptr];
				nextval++;
			}
		}
		for (i = 0; i < nextval; ++i)
			newalive(carmy[i], gl[i], prob[i], -50); 
	}

	killist(&armylist);
}

/* may be called more than once for the same army */

static void initarmyalive(army_t army)
{
	int i;
	for (i = 0; i < NUMRUN; ++i)
		if (armyrun[army][i] != EOL)
			killist(&armyrun[army][i]);
	if (armylbp[army] != EOL) {
		killist(&armylbp[army]);
	}
	if (armynbp[army] != EOL) {
		killist(&armynbp[army]);
	}
	if (armyvitalpoints[army] != EOL)
		killist(&armyvitalpoints[army]);
	if (armyeyerecs[army] != EOL)
		killist(&armyeyerecs[army]);
	if (armypot[army] != EOL) {
		killist(&armypot[army]);
	}
	armyeyepotential[army] = 0;
	armyeyespace[army] = 0;
	armyeyespacemax[army] = 0;
	armyminsemeyes[army] = 0;
	armymaxsemeyes[army] = 0;
	armyeyes[army] = 0;
	armysafeeyes[army] = 0;
	armyrn_pot[army] = 0;
	armycnrn_pot[army] = 0;
	armywk_pot[army] = 0;
	armybestpot[army] = armybestmax[army] = armybestpot2[army] = armysecond[army] = armykopot[army] = armyrest[army] = armypthreat[army] = 0;
	armybestmove[army] = armybestmove2[army] = armysecondmove[army] = armykillmove[army] = armykillmove2[army] = NOSQUARE;
}


/* startalive makes a liberty list for each army in armylist. It looks
 * at the group neighbors and calculates 
 * armyeyes - eyes from point eyes and dead neighbors
 * armyeyespace - eyes from points, dead nbrs, and solid territory
 * it constructs the armyeyerecs and armyvitalpoints lists for each army
 * return number of separate eyes found
 */

static int startalive(army_t army, int *numsafeeyes) {
	list_t ptr;
	int numeyes = 0,numdeadeyes = 0;
    
    *numsafeeyes = 0;
	
	armyeyes[army] = getnumeyes(army, &(armyeyespacemax[army]));
	armyeyespace[army] = armyeyes[army];
	
	if (armyeyes[army] >= 24)return(3);
	
	for (ptr = armyeyerecs[army]; ptr != EOL; ptr = link[ptr]) {
		if (eyepot[list[ptr]] != 0)
			mrglist(eyevital[list[ptr]],&armyvitalpoints[army]);
		*numsafeeyes += eyemin[list[ptr]];	/* worst case assume threats affect all eyes at once */
		if (eyeval[list[ptr]] >= 8) {
			if (eyetype[list[ptr]] == DEADEYE)
				++numdeadeyes;
			else
				++numeyes;
			}
		}
	if (numdeadeyes)
		++numeyes;  /* more cautious with dead eyes */
	if (*numsafeeyes > armyeyes[army])
		*numsafeeyes = armyeyes[army];
	return(numeyes);
	}

/* does army, cut at s, inside rn, have additional contact with the eye */
static int eye_contact(army_t army, int rn, sqr_t s)
{
	int ptr;
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] == s)continue;
		if (eyerec[list[ptr]] == rn && !inlist(list[ptr], &nblbp[s]))
			return TRUE;
	}
	return FALSE;
}

/* pointeyes looks in the liberties for eyes and adds them up.
 * it also looks for eyes due to nearby dead groups on the edge
 * don't count eyes that touch army at cutting points.
 * max is the max number of eyes from eyevalmax
 */

int pointeyes(army_t army, short *max) {
	conn_t rn;
	int eyes = 0, ldtmp, j, c, interflag, interfere, cut;
	sqr_t s, sn;
	list_t eyevit = EOL, ptr, ptr2;  /* vital points of real eyes */
	list_t interlist = EOL; /* interfering vital points */
	army_t army2;

	/* find all of the poential eyes for this army - in liberties or close to liberties */
	c = grcolor[list[armygroups[army]]];
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		rn = eyerec[s];
		if (rn != 0 && eyecolor[rn] == c && eyetype[rn] != THRTEYE) {
#ifdef NEVER
			BAD! BAD! BAD!
			if (A_THREATENED(army)) {
				if (grsavemove[list[armygroups[army]]] != NOSQUARE && grsavemove[list[armygroups[army]]] != PASS) {
					adflist(nextpot,&armypot[army]);
					pots[nextpot].pot_type = VITAL;
					pots[nextpot].pot_val = eyeval[rn];
					pots[nextpot].pot_max = eyeval[rn] + eyepot[rn];
					pots[nextpot++].pot_where = s;    /* 4/02 can get full value if save oneself */
					}
				}
			else
#endif
			addlist(rn,&armyeyerecs[army]);
			}
		if (edge[s] != 1) {
			if (S_NEUTRAL(s)) {
				for (ptr2 = nbgrp[s][1-c]; ptr2 != EOL; ptr2 = link[ptr2])
					if (eyerec[mvs[grpieces[list[ptr2]]]] != 0 &&
						eyetype[eyerec[mvs[grpieces[list[ptr2]]]]] != THRTEYE)
						addlist(eyerec[mvs[grpieces[list[ptr2]]]],&armyeyerecs[army]);
				}
			continue;
			}
		/* eyes not in contact with on edge */
		if (edge[s] != 0) {
	       	j = fdir[s];
	    	for (ldtmp = j+2; j < ldtmp; ++j) {	
			/* only look along edge (j+2) */
	        	sn = s + nbr[j];
	        	if (eyerec[sn] != 0 && S_NEUTRAL(s) && 
	        		board[sn] != NOGROUP &&
	        		grcolor[board[sn]] != c) {
					rn = eyerec[sn];
	        		if (rn != 0 && eyetype[rn] != THRTEYE)
						addlist(rn,&armyeyerecs[army]);
					}
				if (edge[sn] == 0)continue;
	        	sn = sn + nbr[j];
	        	if (eyerec[sn] != 0 && board[sn] != NOGROUP && grcolor[board[sn]] != c &&
	          		ld[sn] == 0 && grnbp[board[sn]] == EOL) {
					rn = eyerec[sn];
	        		if (rn != 0 && eyetype[rn] != THRTEYE)
						addlist(rn,&armyeyerecs[army]);
					}
				}
			}
		}

	/* remove eyes that involve cuts only one side of cut gets credit, so other side must conenct to save itself */
	if (!A_THREATENED(army) && A_ALIVE(army) != DEAD)	
	  for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		rn = eyerec[s];
		if (rn != 0 && eyecolor[rn] == c && eyetype[rn] == VERYBIGEYE) {
			cut = FALSE;
			for (ptr2 = cnbrd[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (grcolor[cngr1[list[ptr2]]] == A_COLOR(army) && 
					cncnum[list[ptr2]] == 1 &&	/* direct cut */
					grarmy[cngr1[list[ptr2]]] != grarmy[cngr2[list[ptr2]]]) {
					army2 = grarmy[cngr1[list[ptr2]]];
					if (army2 == army)
						army2 = grarmy[cngr2[list[ptr2]]];
					if (armysize[army] + armylibs[army] <= armysize[army2] + armylibs[army2] &&
						!eye_contact(army, rn, s)) {
						cut = TRUE;	/* smaller army doesn't get credit */
						break;
						}
					}
				}
			if (cut) {
				dellist(rn, &armyeyerecs[army]);
				continue; /* only count big eyes that don't have internal cuts */
			}
		}
	}	
	interfere = 0;
	interflag = FALSE;
	interlist = EOL;
	for (ptr = armyeyerecs[army]; ptr != EOL; ptr = link[ptr]) {
		rn = list[ptr];
		eyes += eyeval[rn];
		*max += eyevalmax[rn];
		if (eyeval[rn] != eyemin[rn]) {  /* don't add eyes that interfere */
			if (eyeval[rn] - eyemin[rn] > interfere)
				interfere = eyeval[rn]-eyemin[rn];
			if (andlist(eyevital[rn],eyevit,&interlist))  /* COULD BE WRONG WITH ADD?RM ONLY FLAGS */
				interflag = TRUE;  /* interference */
			mrglist(eyevital[rn],&eyevit);
			}
		}
	if (interflag) {  /* was interference */
		eyes -= interfere;
		for (ptr2 = interlist; ptr2 != EOL; ptr2 = EOL) {
			if (nextpot < NUMPOTENTIAL-1) {
				adflist(nextpot,&armypot[army]);
				pots[nextpot].pot_type = VITAL;
				pots[nextpot].pot_val = interfere;
				pots[nextpot].pot_max = interfere;
				/* leave max value the same */
				pots[nextpot++].pot_where = list[ptr2]&EYEPOINTMASK;
				}
			}
		}
	if (eyes > 40)eyes = 40;
	killist(&interlist);
	killist(&eyevit);
	return(eyes);
	}


/* getarmynbp finds all the neighboring armies of army and puts them in
 * armynbp[army].
 */

void getarmynbp(army_t army) {
	list_t ptr,ptr2,ptr3;
	int i,ldtmp,c;
	sqr_t sn;
	if (armygroups[army] == EOL)
		return;
	c = grcolor[list[armygroups[army]]];
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
			addlist(grarmy[list[ptr2]],&armynbp[army]);
		for (ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (!S_NEUTRAL(list[ptr2])) {
				for (ptr3 = nblbp[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3])
					if (lnbf[list[ptr3]][1-c] != 0) {
						i = fdir[list[ptr3]];
						for (ldtmp = ldir[i]; i < ldtmp; ++i) {
							sn = list[ptr3] + nbr[i];
							if (grcolor[board[sn]] == 1-c)
								addlist(grarmy[board[sn]],&armynbp[army]);
							}
						}
				continue;
				}
			i = fdir[list[ptr2]];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				sn = list[ptr2] + nbr[i];
				if (grcolor[board[sn]] == 1-c)
					addlist(grarmy[board[sn]],&armynbp[army]);
				}
			}
		}
	}
	


/* getarmylibs finds all of the liberties of army and puts them in
 * armylibs[army] and armylbp[army]
 */

static void getarmylibs(army_t army) {
	list_t ptr;
	int libs;
	group_t g;
	libs = grlibs[list[armygroups[army]]];
	cpylist(grlbp[list[armygroups[army]]],&armylbp[army]);

	for (ptr = link[armygroups[army]]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[ptr];
		libs += mrglist(grlbp[g],&armylbp[army]);
		}
	armylibs[army] = libs;
	}


/* th_run figures out if can run by capturing enemy stone
 *
 */

int th_run(army_t a) {
	list_t ptr;
	int maxnbn=0,totnbn=0;
	for (ptr = armylbp[a]; ptr != EOL; ptr = link[ptr]) {
		if (lnbn[list[ptr]] > maxnbn)maxnbn = lnbn[list[ptr]];
		totnbn += lnbn[list[ptr]];
		}
	if (totnbn - maxnbn > 4)return(4);
	if (totnbn - maxnbn > 2)return(2);
	return(0);
	}



/* getarmyth_pot looks at the neighbors of the groups in army.  
 * it finds all the threatened neighbors
 * eye potential comes
 * from other army could be connected to.
 * NOTE: th_pot records can be changed later by threat_live
 */

static void getarmyth_pot(army_t army) {
	list_t ptr, ptr2, tmplist = EOL;
	sqr_t s, sn;
	int th_pot;
	int c,i,ldtmp,atariflag = FALSE;
	c = grcolor[list[armygroups[army]]];
	if (armynbp[army] == EOL)getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		if (A_THREATENED(list[ptr]))
			addlist(list[ptr],&tmplist);
		if (A_ALIVE(list[ptr]) == DEAD) { /* && A_THREATENED(army)*/
			if (armynbp[list[ptr]] == EOL)getarmynbp((army_t)list[ptr]);
			if (armynbp[list[ptr]] == EOL || link[armynbp[list[ptr]]] == EOL)
				continue;	/* no connection here */
			for (ptr2 = armynbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
				if (A_THREATENED(list[ptr2])) {
					addlist(list[ptr], &tmplist);
					break;
				}
			}
		}

	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (S_NEUTRAL(s)) {
			i = fdir[s];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				sn = s + nbr[i];
				if (S_COLOR(sn) == 1-c && 
				   S_THREATENED(sn))
					addlist(S_ARMY(sn),&tmplist);
				}
			}
		}

	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		if (armylibs[list[ptr]] == 1)atariflag = TRUE;
		th_pot = 0;
#ifdef NEVER		
		/* this value will be associated with eyevitrec in howmuchvital,
		   so don't dulplicate it here. */
		/* must do it in howmuchvital, since the vital point move
		   in howmuchvital could take group off board, so howmuchvital
		   can't tell if there was threatened group here
		 */
		for (ptr2 = armygroups[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (eyerec[mvs[grpieces[list[ptr2]]]] != 0)
				th_pot += eyepot[eyerec[mvs[grpieces[list[ptr2]]]]];
			}
#endif			
		th_pot += th_conns(army,(army_t)list[ptr]);
		th_pot += th_run((army_t)list[ptr]);
		th_pot += th_terr(army,(army_t)list[ptr]);
		if (th_pot >= 8)
			th_pot++;	/* prefer to capture stoens to make eyes */
		if (th_pot > 64)th_pot = 64;
		if (nextpot < NUMPOTENTIAL-1) {
			adflist(nextpot,&armypot[army]);
			if (A_ALIVE(list[ptr]) == DEAD)
				pots[nextpot].pot_type = POTDEAD;
			else if (A_THREATENED(list[ptr]) == 2)
				pots[nextpot].pot_type = THREAT;
			else
				pots[nextpot].pot_type = POTTHREAT;  /* not necessarily captured */
			pots[nextpot].pot_val = th_pot;
			pots[nextpot].pot_max = th_pot;
			pots[nextpot++].pot_where = list[ptr];  /* army */
			}
#ifdef CHECK
		else
			outerr("th out of pots!\n");
#endif
		}
	killist(&tmplist);
	}


/* territory to add to army for killing a */

static int th_terr(army_t army, army_t a) {
	list_t ptr;
	int terr = 0,sub,min,eyeflag = 0,c;
	c = A_COLOR(a);
	for (ptr = armylbp[a]; ptr != EOL; ptr = link[ptr]) {
		if (ltrgd[list[ptr]] && ltr1[list[ptr]] > 1)
			terr += ltr1[list[ptr]]-1;  /* -1 since have to capture */
#ifdef NEVER
		/* I can't figure out what this is supposed to do, and
		 * it can cause eyes to be double counted
  		 */			
		for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			s = list[ptr2];
			if (lnbf[s][c] == 0 && lnbn[s] == 2) {
				s2 = list[nblbp[s]];
				if (s2 == list[ptr])
					s2 = list[link[nblbp[s]]];
				if (lnbf[s2][c] == 0 && lnbn[s2] == 2)
					eyeflag = 8;
				}
				
			}
#endif				
		}
	sub = armylibs[a];  /* have to fill liberties */
	min = 5-armysize[a];  /* can add stones up to size 5 */
	if (armylibs[a]-1 < min)min = armylibs[a]-1;  /* or until captured */
	terr -= min+sub;
#ifdef NEVER	
	if (eyeflag)terr--;
#endif	
	if (terr < 0)terr = 0;
	return(sumeyes[terr] + eyeflag);
	}


/* find eyespace value of connecting to nbrs
 * of a which are not in army. a is threatened enemy
 * group that is being captured to connect to something else
 */

static int th_conns(army_t army, army_t a) {
	army_t a2;
	sqr_t s;
	list_t ptr,tmplist = EOL, rnlist = EOL, ptr2;
	int ceyes,c,i,ldtmp,tr = 0;
	group_t g2;
	ceyes = 0;
/*	cpylist(armyeyerecs[army],&rnlist); */
	c = A_COLOR(army);
	if (armynbp[a] == EOL)getarmynbp(a);
	for (ptr = armynbp[a]; ptr != EOL; ptr = link[ptr]) {
		a2 = (army_t)list[ptr];
		if (A_ALIVE(a2) == DEAD)
			continue;
		if (a2 == army)continue;
		addlist(a2,&tmplist);
		if (A_THREATENED(a2) == 1 &&
			A_SIZE(a2) == 1 &&
			A_NUMLIBS(a2) == 1) {	/* ko stone, can connect to its friends */
			s = list[armylbp[a2]];
			if (lnbn[s] == 0 && lnbf[s][1-c] == 0)
				for (ptr2 = nbgrp[s][c]; ptr2 != EOL; ptr2 = link[ptr2])
					addlist(grarmy[list[ptr2]], &tmplist);
			}
		}
	for (ptr = armylbp[a]; ptr != EOL; ptr = link[ptr]) {
		/* if (lnbf[list[ptr]][c] == 0)continue; */
		i = fdir[list[ptr]];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			g2 = board[list[ptr]+nbr[i]];
			if (grcolor[g2] == 1-c && grlibs[g2] == 1) {  /* can capture another group (or ko) */
				a2 = grarmy[g2];
				if (armynbp[a2] == EOL)getarmynbp(a2);
				for (ptr2 = armynbp[a2]; ptr2 != EOL; ptr2 = link[ptr2])
					if (list[ptr2] != army)
						addlist(list[ptr2], &tmplist);
				}
			if (grcolor[g2] != c)continue;
			if (grarmy[g2] == army)continue;
			addlist(grarmy[g2],&tmplist);
			}
		}
	for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
		for (ptr2 = armyeyerecs[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
			if (!inlist(list[ptr2],&armyeyerecs[army]))
				addlist(list[ptr2],&rnlist);
		}
	for (ptr = rnlist; ptr != EOL; ptr = link[ptr]) {
		ceyes += eyeval[list[ptr]];
		if (eyepot[list[ptr]] >= eyeval[list[ptr]] + 8)
			ceyes += (eyepot[list[ptr]]-eyeval[list[ptr]])/2;
		}
	ceyes += sumeyes[tr];
/*	ceyes -= armyeyes[army]; */
	killist(&rnlist);
	killist(&tmplist);
	return(ceyes);
	}


/* return number of eyes in army.  Update armyeyerecs to point at eyes */

static int getnumeyes(army_t army, short *max) {
	int eyes;
	deadgroups(army);
	eyes = pointeyes(army, max);
	if (A_THREATENED(army))
		eyes = 0;
	return(eyes);
	}


/* deadgroups looks at the enemy dead groups of this army and returns the number
 * of eyes.  Dead groups are enemy groups in contact with or sharing a liberty
 * with army.
 */

static void deadgroups(army_t army) {
	list_t ptr, ptr2, ptr3, deadgroups = EOL;
	group_t g, g2;
	conn_t rn;
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[ptr];
		for (ptr2 = grnbp[g]; ptr2 != EOL; ptr2 = link[ptr2]) {
			g2 = (group_t)list[ptr2];
			if (gralive[g2] != DEAD && !G_THREATENED(g2))continue;
			if (gralive[g2] == DEAD)
				addlist(g2, &deadgroups);
			rn = eyerec[mvs[grpieces[g2]]];
			if (rn != 0) {
				addlist(rn,&armyeyerecs[army]);
				}
			}
       	}
    for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
    	if (S_NEUTRAL(list[ptr])) {
			for (ptr2 = nbgrp[list[ptr]][1-A_COLOR(army)]; ptr2 != EOL; ptr2 = link[ptr2]) {
    			if (gralive[list[ptr2]] == DEAD  || G_THREATENED(list[ptr2])) {
					if (gralive[list[ptr2]] == DEAD)
						addlist(list[ptr2], &deadgroups);
					rn = eyerec[mvs[grpieces[list[ptr2]]]];
					/* 5/9/96 allow threatened eye also for big eyes with
					   threatened stones inside, not in contact 
					   dangerous if vital points of threatened group don't actually capture it!
					 */
					if (rn != 0 /* && eyetype[rn] == DEADEYE */) {
						addlist(rn,&armyeyerecs[army]);
						}
					}
				}
    		}
    	}
	for (ptr = deadgroups; ptr != EOL; ptr = link[ptr]) {
		for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (grarmy[list[ptr2]] != army && gralive[list[ptr2]] != DEAD) {
				for (ptr3 = grlbp[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3]) {
					if (eyerec[list[ptr3]] != 0 && eyecolor[eyerec[list[ptr3]]] == A_COLOR(army)) /* 2/01 make sure eye is right color */
						addlist(eyerec[list[ptr3]],&armyeyerecs[army]);
					}
				}
			}
		}
	killist(&deadgroups);
	return;
	}




/* get values for eyepotential for army for
 * extensions
 * find the outermost liberty along each edge on the 4 line or lower.
 * 
 */


static void getarmyex_pot(army_t army) {
	list_t ptr;
	int lmin,lmax,rmin,rmax,tmin,tmax,bmin,bmax,i;
	sqr_t s, points[8];
	int x,y;
	if (armysize[army] == 1 && armylibs[army] <= 3)return;
	    /* can't extend if in contact fight, no time */
	if (A_THREATENED(army))return;
	   /* no time to extend if threatened */

	/* find the outsidemost liberties along each edge.  In case of
		tie take liberty closest to edge on 4th line to 2nd line */

	for (i = 0; i < 8; ++i)points[i] = NOSQUARE;
	tmax = bmax = lmax = rmax = -1;
	tmin = bmin = lmin = rmin = 100;
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (edge[s] > 4 || edge[s] == 0)continue;

		x = xval[s];
		y = yval[s];
		if (x < 4) {
			if (y > lmax || 
				y == lmax && edge[s] > 1 &&
					(edge[s] < edge[points[0]] ||
					 edge[points[0]] <= 1)) {
				lmax = y;
				points[0] = s;
				}
			if (y < lmin ||
				y == lmin && edge[s] > 1 &&
					(edge[s] < edge[points[1]] ||
					 edge[points[1]] <= 1)) {
				lmin = y;
				points[1] = s;
				}
			}
		else if (x > boardsize - 5) {
			if (y > rmax ||
				y == rmax && edge[s] > 1 &&
					(edge[s] < edge[points[2]] ||
					 edge[points[2]] <= 1)) {
				rmax = y;
				points[2] = s;
				}
			if (y < rmin  ||
				y == rmin && edge[s] > 1 &&
					(edge[s] < edge[points[3]] ||
					 edge[points[3]] <= 1)) {
				rmin = y;
				points[3] = s;
				}
			}
		if (y < 4) {
			if (x > tmax  ||
				x == tmax && edge[s] > 1 &&
					(edge[s] < edge[points[4]] ||
					 edge[points[4]] <= 1)) {
				tmax = x;
				points[4] = s;
				}
			if (x < tmin ||
				x == tmin && edge[s] > 1 &&
					(edge[s] < edge[points[5]] ||
					 edge[points[5]] <= 1)) {
				tmin = x;
				points[5] = s;
				}
			}
		else if (y > boardsize - 5) {
			if (x > bmax ||
				x == bmax && edge[s] > 1 &&
					(edge[s] < edge[points[6]] ||
					 edge[points[6]] <= 1)) {
				bmax = x;
				points[6] = s;
				}
			if (x < bmin && x > 1 ||
				x == bmin && edge[s] > 1 &&
					(edge[s] < edge[points[7]] ||
					 edge[points[7]] <= 1)) {
				bmin = x;
				points[7] = s;
				}
			}
		}
	if (lmax != -1 && yval[points[0]] < boardsize-3)
		check_ex(points[0],boardsize,1,army,DOWN,UP);
	if (lmin != 100 && yval[points[1]] > 2)
		check_ex(points[1],-boardsize,1,army,UP,DOWN);
	if (rmax != -1 && yval[points[2]] < boardsize-3)
		check_ex(points[2],boardsize,-1,army,DOWN,UP);
	if (rmin != 100 && yval[points[3]] > 2)
		check_ex(points[3],-boardsize,-1,army,UP,DOWN);
	if (tmax != -1 && xval[points[4]] < boardsize-3)
		check_ex(points[4],1,boardsize,army,RIGHT,LEFT);
	if (tmin != 100 && xval[points[5]] > 2)
		check_ex(points[5],-1,boardsize,army,LEFT,RIGHT);
	if (bmax != -1 && xval[points[6]] < boardsize-3)
		check_ex(points[6],1,-boardsize,army,RIGHT,LEFT);
	if (bmin != 100 && xval[points[7]] > 2)
		check_ex(points[7],-1,-boardsize,army,LEFT,RIGHT);
	}

/* check_ex looks for how much eyespace can be added by extending from 
 * s in direction dir.  
 * s is the end liberty on this edge for an army closest to edge of board
 *   s is on the 1 thru 4 line
 * Dir2 is direction away from edge of board
 * c is the color of the group extending
 * can't extend from edge
 * can't extend if end liberty is between stone and edge
 * sdir, sdir2 are the square direction for use in sqrbrd etc.
 * sdir points out along the edge and sdir2 points the opposite
 */

static void check_ex(sqr_t s, int dir, int dir2, army_t army, int sdir, int sdir2) {
	sqr_t su,sd,so,suo,soo; /* up, down, out, up and out, out twice */
	int c,at,terr,g,two_stone_wall,under_enemy,dist,tmp,toward_friend;
	
	if (edge[s] <= 3 && ltr1[s] == 0)return;  /* stones between liberty and edge */
	at = 0;
	c = grcolor[list[armygroups[army]]];

	/* can't extend from contact fight */
	if (armylibs[army] < 4 && armysize[army] == 1)return;  
	if (armysize[army] > 1 && link[armygroups[army]] == EOL &&
		armylibs[army] < 4)return;
	
	
	su = s + dir2;  /* point up from liberty */
	while(edge[su] < 4 && board[su] == NOGROUP && 
		inlist(su,&armylbp[army]) && board[su+dir] == NOGROUP) {
		s = su;
		su = s + dir2;  /* search up wall for highest liberty */
	}
	if (edge[s] <= 1)
		sd = s;
	else
		sd = s - dir2;  /* point down from liberty */
	so = s + dir;  /* point out from liberty */
	suo = s + dir + dir2;
	if (grcolor[board[sd]] == c)return; /* lib towards center */
	if (grcolor[board[so]] == c)return; /* inside lib */
	if (edge[so] <= 2 && edge[s] > edge[so])return;  /* don't extend into corner */
	if (edge[s] == 1 && edge2[s] <= 4 && edge2[so] < edge2[s])return;
	if (ltr1[s] == 0)return;  /* enemy stones underneath */
	if (grcolor[board[su]] == c) { /* underneath lib, enemy next at suo, hane for more, or under 5th line stone */
		if (edge[s] < 3)return;
		if (S_NEUTRAL(s))return;
		if (ltrgd[s] >= 8)return;
		/* if (edge[s] >= 4 && lnbn[s] == 3)return;  under high stones */
		if (board[so] != NOGROUP)return;  /* inside lib */
		if (lnbf[so][1-c] > 1)return;
		if (lnbf[sd][1-c] != 0)return;
		if (lnbf[s-dir][1-c] != 0)return;  /* enemy behind */
		if (armysize[army] == 1 && armylibs[army] < 3)return;
		g = board[suo];
		if (g == NOGROUP && grarmy[board[su]] == army) {
			if (edge[s] != 4)return;
			if (lnbf[so][1-c] != 0)return;
			if (ltrgd[s] < 4)
				terr = 6;  /* below 5th line stone */
			else
				terr = 3;
		}
			
		else if (g != NOGROUP && grsize[g] == 1 && 
			(grlibs[g] <= 2 || grlibs[g] == 3 && edge[suo] > 1 && lnbn[suo+dir2] == 3))
			terr = edge[s] + edge[s] - 1;  /* get to extend twice */
		else
			terr = edge[s] - 1;
		if (nextpot < NUMPOTENTIAL-1) {
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = terr;
			pots[nextpot].pot_max = terr;
			pots[nextpot++].pot_where = s;
			return;
		}
#ifdef CHECK
		else{
			outerr("ex out of pots!\n");
			return;
		}
#endif
	}

/* outside edge lib */


	if (S_NEUTRAL(s)) {  
		if (edge[s] <= 1)return;
		if (armysize[army] == 1 && armylibs[army] < 4)return;
		if (grcolor[board[sd]] == 1-c)return;
		if (board[sd+dir] != NOGROUP)return;
		if (board[sd+dir+dir] != NOGROUP)return;
		if (board[so] != NOGROUP)return;
		/* can crawl under */
		terr = terr2[edge[s]];
		if (inlistm(s,&armyvitalpoints[army],EYEPOINTMASK))
			terr -= edge[s]-1;  /* don't double count with vital point */
			/* eye analysis is more accurate than territory */
		/* crawling into corner */ 
		if (edge[s] > 2 && edge2[so] < edge2[s] && edge2[s] <= 4)++terr;
		/* jump out on second line */
		if (edge[so] == 2 && lnbn[so+dir] == 4)++terr;
		/* slide to second line */
		if (edge[so] == 3 && 
			lnbn[so-dir2] == 4 && 
			S_COLOR(so+dir) != 1-c &&
			board[so-dir2] == NOGROUP)
			terr ++;

		if (nextpot < NUMPOTENTIAL-1) {
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = terr;
			pots[nextpot].pot_max = terr;
			pots[nextpot++].pot_where = s;
			}
#ifdef CHECK
		else
			outerr("ex out of pots!\n");
#endif
	}
	else if (ltrgd[s] == 1 || ltrgd[s] == 0) {
		two_stone_wall = ld[so] == NOLD &&
			(grcolor[lgr[su]] == c && ltrgd[su] == 1 ||
			 grcolor[lgr[sd]] == c && ltrgd[sd] == 1) 
			 && edge[s] < 4;
		if (!two_stone_wall && ld[so] == NOLD && ld[su] == NOLD &&
		   ld[su-dir] > 4)two_stone_wall = TRUE;
		under_enemy = grcolor[lgr[so]] == 1-c && !S_NEUTRAL(so) &&  /* sliding under enemy stone */
			(edge[so] <= 1 || board[so-dir2] == NOGROUP);  /* can't be a stone under so */
		if (!under_enemy && edge[so] > 1 && lnbf[so][c] == 0 && lnbf[so][1-c] == 0 &&
			grcolor[lgr[so+dir]] == 1-c && !S_NEUTRAL(so+dir) &&
			(edge[so+dir] <= 1 || board[so+dir-dir2] == NOGROUP))under_enemy = TRUE;
		toward_friend = FALSE;	/* friendly stone at 3 pt jump, lg knight, ex lg knight */
		if (edge[so] > 1 && edge2[so] > 3) {
			soo = so+dir;
			if (grcolor[board[soo+dir]] == c || 
			   edge[soo] <= 3 && (grcolor[board[soo+dir2]] == c ||
					      grcolor[board[soo+dir2+dir]] == c) ||
			   edge[so] == 4 && (grcolor[board[soo-dir2]] == c ||
					     grcolor[board[soo-dir2+dir]] == c))
				toward_friend = TRUE;
		}
		terr = 0;
		if (under_enemy)
			terr = terr4[edge[s]];
		else if (two_stone_wall) {
			terr = terr3[edge[s]];
			if (toward_friend)terr += 3;
		}
		else if (ld[so] == NOLD) {
			terr = terr1[edge[s]];
			if (toward_friend)terr += 3;
		}
		if (ltrgd[s] == 0 &&
			(eyerec[s] == 0 || eyepot[eyerec[s]] != 0)) {  /* already eye */
			terr -= ltr1[s];
			if (terr < 1)terr = 1;
		}
		if (nextpot < NUMPOTENTIAL-1) {
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = terr;
			pots[nextpot].pot_max = terr;
			pots[nextpot++].pot_where = s;
		}
#ifdef CHECK
		else
			outerr("ex out of pots!\n");
#endif
	}
	else if (ltrgd[s] == 3 || ltrgd[s] == 2) {
		terr = 0;  /*terr4[edge[s]]; don't double count with block */
		if (edge2[so] < edge2[s] &&
			(edge2[so] <= 3 || edge2[so] == 4 && lnbf[so+dir][1-c] == 0))
			terr += terr4[edge[s]];
		else if (!S_NEUTRAL(s) && board[so] == NOGROUP && lnbn[so] == 4) {
			soo = so + dir;
			if (edge[so] > 1 && board[soo] == NOGROUP && lnbn[soo] == 4)
			/* can make a 1 pt jump at least */
				terr += (terr1[edge[s]] - edge[s]);  /* subtract for double count of block */
			else
				terr += (terr5[edge[s]][3] - edge[s]);
		}
		if (terr < 0)
			terr = 0;
		if (nextpot < NUMPOTENTIAL-1) {
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = terr;
			pots[nextpot].pot_max = terr;
			pots[nextpot++].pot_where = s;
		}
#ifdef CHECK
		else
			outerr("ex out of pots!\n");
#endif
	}
	else if (ltrgd[s] >= 4 && /*ltrgd[s] <= 7 && 5/03 sometimes still good to extend */lnbf[sd][1-c] == 0) {  /* undercut one way */
		if (edge[s] <= 1)return;
		dist = ucutdist(s,sdir,-dir2,c);
		tmp = ucutdist(s,sdir2,-dir2,c);  /* how far undercut from behind? */
//		if (tmp < 5)dist -= 5-tmp;
		if (dist == 5 && tmp == 5)dist = 2;  /* can't find enemy, must be near corner */
		if (tmp == 5 && undercut(s - dir2, -dir2, dir, c, sdir, sdir2))
			dist--;
		    /* undercut from below in front (not just line stone is on) */
		
		under_enemy = grcolor[lgr[so]] == 1-c && !S_NEUTRAL(so) &&  /* sliding under enemy stone */
			(edge[so] <= 1 || board[so-dir2] == NOGROUP);  /* can't be a stone under so */
		if (!under_enemy && edge[so] > 1 && lnbf[so][c] == 0 && lnbf[so][1-c] == 0 &&
			grcolor[lgr[so+dir]] == 1-c && !S_NEUTRAL(so+dir) &&
			(edge[so+dir] <= 1 || board[so+dir-dir2] == NOGROUP))under_enemy = TRUE;
#ifdef NEVER
		under_enemy = grcolor[lgr[so]] == 1-c && !S_NEUTRAL(so);
		if (!under_enemy && edge[so] > 1 && grcolor[lgr[so+dir]] == 1-c
		   && !S_NEUTRAL(so+dir))under_enemy = TRUE;
#endif
		if (dist < 0)dist = 0;
		terr = terr5[edge[s]][dist];
		if (under_enemy)--terr;
		if (tmp < 5)--terr;	/* undercut from behind */
		if (edge2[so] < edge2[s] && edge2[s] <= 4)++terr; /* into corner */
		if (dist == 5 && ucutdist(s,sdir,-dir2,1-c) < 5) /* 1/03 friendly stone in this direction, and no enemy */
				terr += 3;	/* to be consistent with 1 */
		if (ltrgd[s] >= 8)	/* undercut both directions */
			terr /= 2;
		if (terr < 0)terr = 0;
		if (nextpot < NUMPOTENTIAL-1) {
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = terr;
			pots[nextpot].pot_max = terr;
			pots[nextpot++].pot_where = s;
		}
#ifdef CHECK
		else
			outerr("ex out of pots!\n");
#endif
	}
}

/* what is distance to undercutting stone in direction sdir.  dir is
 * offset to edge of board.  s is starting point.  c is color of group being
 * undercut.  distance returned (1-4) is number of lines between
 * this stone and the undercutting stone.  returns 5 if out of range.
 */

static int ucutdist(sqr_t s, int sdir, int dir, int c) {
	int dist = 4;
	sqr_t sn;
	sn = s-dir;
	do {
		sn = sn+dir;
		if (grcolor[board[sqrbrd[sn][sdir]]] == 1-c && dstbrd[sn][sdir]
		   < dist)dist = dstbrd[sn][sdir];
		} while(edge[sn] > 1);
	return(dist+1);
	}



/* find all of the territory for army which is undercut once but
 * otherwise good
 */


static void getarmyuc_pot(army_t army) {
	list_t ptr,ptr2;
	sqr_t s,sn;
	int terr,c,i,ldtmp, extra, lt1;
	int cantblock, addextra;
	c = grcolor[list[armygroups[army]]];
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (ltr1[s] == 0 || ltrgd[s] == 0)continue;
		terr = 0;
		extra = 0;
		if (ltrgd[s] == 1)continue;
		if (eyerec[s] != 0)continue;	/* already in an eye */
		if (lnbn[s] == 0)continue;	/* no points to protect */
		if (ltrgd[s] == 4 || (ltrgd[s] == 6 && !S_NEUTRAL(s))) {
			if (edge[s] == 0 || lnbn[s] == 0)continue;
			terr = ltr1[s];
			if (terr > edge[s]) {
				terr = edge[s];
				extra = ltr1[s]-edge[s];
				}
			i = fdir[s];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				sn = s + nbr[i];
				if (edge[sn] < edge[s] && lnbf[sn][c] == 0 &&
					lnbf[sn][1-c] != 0 && ltr1[sn] > edge[sn])
					terr += ltr1[sn] - edge[sn];
				}
			if (eyerec[s] != 0 || eyevitrec[s] != EOL) {
				--terr;
				if (edge[s] > 1)
					++extra;  /* can't make terr on edge */
				}
			else if (ltrgd[s] == 6)--terr;
			if (armysize[army] == 1 && grcnp[list[armygroups[army]]] == EOL) {
				terr = 0;
				extra = 0;
				}
			} 
		else if (ltrgd[s] == 3) {
			terr = ltr1[s];
			if (terr > edge[s]) {
				terr = edge[s];
				extra = ltr1[s]-edge[s];
				if (edge[s] == 1)
					extra--;
				}
			if (eyerec[s] != 0) {
				--terr;
/*				++extra; too optimistic - counts terr on edge */
				}
			if (edge[s] == 0)--terr;
			for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2])
				 if (lnbf[list[ptr2]][1-c] != 0 && terr > 1) {
					 --terr;
					 if (extra > 1)extra = 1;
					 break;
					 }
			}
		else if (ltrgd[s] == 2 && lnbf[s][1-c] <= 1) {
			terr = edge[s]-1;  /* must fill one point to get rest */
			lt1 = ltr1[s];
			if (edge[s] == 2 && edge2[s] == 2 && S_NEUTRAL(s) && lnbn[s] == 2)
				lt1 = 4;
			if (lt1 > terr && lnbn[s] >= 2) {
				extra = lt1-edge[s];
				if (extra > 1)
					extra = 1;
				}
			i = fdir[s];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				if (edge[s+nbr[i]] <= edge[s] && /* if enemy below or beside don't get terr towards near edge */
					edge2[s] >= edge[s] &&
				   (board[s+nbr[i]] != NOGROUP || /* stone towards near edge */
					lnbf[s+nbr[i]][1-c] != 0)) {
					   terr -= edge[s]-1;
					   break;
					   }
				}
			if (lt1 > edge[s]) {  /* clear shot to second nearest edge */
				i = fdir[s];
				for (ldtmp = ldir[i]; i < ldtmp; ++i) {
					if (edge2[s+nbr[i]] <= edge2[s] && /* if enemy below or beside don't get terr towards near edge */
				   		(grcolor[board[s+nbr[i]]] == 1-c || lnbf[s+nbr[i]][1-c] != 0)) {
				   			terr -= (ltr1[s] - edge[s]);
					   		break;
							}
					}
				}
			}
		else if (ltrgd[s] == 7) {
			terr = ltr1[s];
			if (terr > edge[s]) {
				terr = edge[s];
				extra = ltr1[s]-edge[s];
				if (extra > edge[s])
					extra = edge[s];	/* 4/01, can't get too much for horrible blocks in the corner */
				}
			if (eyerec[s] != 0)terr--; /* eye already counted */
			if (edge[s] == 0)terr--;
			cantblock = FALSE;
			addextra = TRUE;
			for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (lnbf[list[ptr2]][1-c] != 0 && lnbf[list[ptr2]][c] == 0) {
					cantblock = TRUE;
					addextra = FALSE;
				}
				if (edge[list[ptr2]] > edge[s])
					cantblock = TRUE;
				}
			if (cantblock) {
				--terr;
				if (extra > 1)extra = 1;
				if (addextra)
					++extra;
				}
		/*	if (terr == 1)terr = 0; */
			}
		else continue;
		if (terr < 0) {
			terr = 0;
			if (extra > 1)
				extra = 1;
		}
		for (ptr2 = armypot[army]; ptr2 != EOL; ptr2 = link[ptr2])
			if (pots[list[ptr2]].pot_type == EXTEND &&
				pots[list[ptr2]].pot_where == s) {
				if (terr > 1) {
					terr = 1;   /* don't double count with extension */
					extra = 0;
				}
				break;
			}
		if (nextpot < NUMPOTENTIAL-1) {
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = UNDERCUT;
			pots[nextpot].pot_val = terr;
			pots[nextpot].pot_max = terr+extra;
			pots[nextpot++].pot_where = s;
			}
		}
	}


/* look for double connect between army and a with move at s */
	

int doubleconnect(army_t army, army_t a, sqr_t s) {
	list_t ptr;
	group_t g;
	army_t a2;
	int val = 0,c;
	g = (group_t)list[armygroups[a]];
	c = grcolor[g];
	for (ptr = grcnp[g]; ptr != EOL; ptr = link[ptr]) {
		a2 = grarmy[cngr1[list[ptr]]];
		if (a2 == a)a2 = grarmy[cngr2[list[ptr]]];
		if (a2 == army)continue;
		if (cnprot[list[ptr]] == CANT_CONNECT)continue;
		if (cnptr[list[ptr]] == EOL)continue;
		if (lnbf[list[cnptr[list[ptr]]]][1-c] != 0)continue;
		if (!inlist(list[cnptr[list[ptr]]],&nblbp[s]))continue;
		if (cnprot[list[ptr]] == MIGHT_CONNECT && 
			armyeyespace[a2] > 8)
			val += 8;
		else
			val += armyeyespace[a2];
		}
	return(val);
	}


/* figure out the eye potential for connecting to another army
 * don't double count territory that is shared
 */

static void getarmycn_pot(army_t army)
{
	list_t ptr,ptr2,ptr3;
	int cn;
	army_t a;
	group_t g1,g2;
	int val, maxval;
	list_t tmplist = EOL; /* list of connection points */
	int c;

	c = grcolor[list[armygroups[army]]];

	/* Find all the connections */

	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g1 = (group_t)list[ptr];
		for (ptr2 = grcnp[g1]; ptr2 != EOL; ptr2 = link[ptr2]) {
			cn = list[ptr2];
			/*
			if (!CANCONN(cnprot[cn]) && cnprot[cn] != MIGHT_CONNECT)
			continue;
			*/
			/*			if (cnprot[cn] == CANT_CONNECT)
			continue; 9/99 include with value 0 so can generate threat moves */
			if (cnprot[cn] == CANT_CONNECT && cntype[cn] == CN_DOUBLEDIAG)
				continue;	/* can't threaten this connection since there is no path */
			g2 = cngr1[cn];
			if (grcolor[g2] != c)continue;
			if (g2 == g1)g2 = cngr2[cn];
			a = grarmy[g2];
			if (a == army)continue;
			if (gralive[g2] == DEAD)continue;
			/* if (G_THREATENED(g2))continue; too pessimistic! */

			/* found army can connect to */

			val = geteyespace(army, a) - armyeyespace[army];
			val += cnshreyespace(army, a, c, cn);
			if (cnprot[cn] == SHARED_CONNECT && cnshcent[cn] == g1)
				val += cnshreyepot(army, a, c, cn);	/* 3/01 since really connected, add up potential eyes as well as real ones */
			/* if (G_THREATENED(g2) && G_THREATENED(g1))  /* 8/99 both sides must be threatened */
			/* if only one is threatened, the connection must be breakable */
			if (cnprot[cn] == THRT_CONNECT)
				cnthreat(army, g1 ,g2, cn);
			if (armysize[a] == 1 && cntype[cn] == CN_HANE &&
				lnbf[list[cnptr[cn]]][1-c] == 0 &&
				lnbn[list[cnptr[cn]]] == 2) /* hanging connection can make other connection too */
				for (ptr3 = nblbp[list[cnptr[cn]]]; ptr3 != EOL; ptr3 = link[ptr3])
					if (comlist(nblbp[list[ptr3]],armylbp[a]))
						val += doubleconnect(army,a,list[ptr3]);
			if (val > 40)val = 40;
			maxval = val;
			if (cnprot[cn] == MIGHT_CONNECT && val > 8)
				val = 8;
			if (cnprot[cn] == CANT_CONNECT) {
				if (val > 1)
					val = 1;
				if (maxval > 4)
					maxval = 4;
			}
			if (maxval < val)
				maxval = val;
			if (maxval > 40)
				maxval = 40;
			if (nextpot < NUMPOTENTIAL-1) {
				adflist(nextpot,&armypot[army]);
				pots[nextpot].pot_type = CONNECT;
				pots[nextpot].pot_val = val;
				pots[nextpot].pot_max = maxval;
				pots[nextpot++].pot_where = cn;
			}
#ifdef CHECK
			else
				outerr("cn out of pots!\n");
#endif
		}
	}
	if (tmplist != EOL)
		killist(&tmplist);
}

/* find best additional eye potential through a shared connection from army that doesn't go back to olda
 */ 
static int cnshreyepot(army_t olda, army_t army, int c, int oldcn) {
	list_t ptr,ptr2;
	group_t g1,g2;
	army_t a;
	int cn,val = 0,eyes;

	/* Find all the connections */

	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g1 = (group_t)list[ptr];
		for (ptr2 = grcnp[g1]; ptr2 != EOL; ptr2 = link[ptr2]) {
			cn = list[ptr2];
			if (cnprot[cn] != CAN_CONNECT)continue;
			g2 = cngr1[cn];
			if (grcolor[g2] != c)continue;
			if (g2 == g1)g2 = cngr2[cn];
			a = grarmy[g2];
			if (a == army)continue;
			if (a == olda)continue;
			if (gralive[g2] == DEAD)continue;
			if (comlist(cnptr[cn],cnptr[oldcn]))continue;
			if (comlist(cnptr[cn],cnlkptr[oldcn]))continue;
			eyes = geteyespace(army,a) - armyeyespace[army];
			if (eyes > val)val = eyes;
			}
		}
	return(val);
	}


/* find additional eyespace available through a shared connection from
 * army that doesn't go back to olda.
 * and doesn't share a point with oldcn
 */
 
                                          
static int cnshreyespace(army_t olda, army_t army, int c, int oldcn) {
	list_t ptr,ptr2;
	group_t g1,g2;
	army_t a;
	int cn,val = 0,eyes;

	/* Find all the connections */

	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g1 = (group_t)list[ptr];
		for (ptr2 = grcnp[g1]; ptr2 != EOL; ptr2 = link[ptr2]) {
			cn = list[ptr2];
			if (cnprot[cn] != SHARED_CONNECT)continue;
			if (cnshcent[cn] != g1)continue;
			g2 = cngr1[cn];
			if (grcolor[g2] != c)continue;
			if (g2 == g1)g2 = cngr2[cn];
			a = grarmy[g2];
			if (a == army)continue;
			if (a == olda)continue;
			if (gralive[g2] == DEAD)continue;
			if (comlist(cnptr[cn],cnptr[oldcn]))continue;
			if (comlist(cnptr[cn],cnlkptr[oldcn]))continue;
			eyes = geteyespace(army,a) - armyeyespace[army];
			if (eyes > val)val = eyes;
			}
		}
	return(val);
	}
	
	
static int connecthere(unsigned int cn, group_t g1, group_t g3) {
	int ptr, p2;
	for (ptr = cnptr[cn]; ptr != EOL; ptr = link[ptr]) {
		for (p2 = cnbrd[list[ptr]]; p2 != EOL; p2 = link[p2]) {
			if (list[p2] == cn)
				continue;
			if (cngr1[list[p2]] == g1 && cngr2[list[p2]] == g3 ||
				cngr1[list[p2]] == g3 && cngr2[list[p2]] == g1)
				return TRUE;
			}
		}
	return FALSE;
	}
	                                         
/* if threatened group g1 is actually unbreakably connected to g2, 
 * look to connections
 * of that group
 */

static void cnthreat(army_t army,group_t g1,group_t g2,int cn) {
	list_t ptr;
	unsigned int cn2;
	int val=0;
	group_t g3;
#ifdef never
	6/03 already figured this out for thrt_connect
	if (cncnum[cn] == 0)return;
	if (grlibs[g1] == 1)return; /* can't make far connection */
	if (cncnum[cn] == 1) {
		s = list[cnptr[cn]];
		if (lnbf[s][1-grcolor[g1]] != 0)return;
		if (lnbn[s] > 1)return;
		}
#endif
	for (ptr = grcnp[g2]; ptr != EOL; ptr = link[ptr]) {
		cn2 = list[ptr];
		if (cnprot[cn2] == CANT_CONNECT)continue;
		g3 = cngr1[cn2];
		if (g3 == g2)g3 = cngr2[cn2];
		if (grarmy[g3] == grarmy[g2])continue;
		if (grarmy[g3] == grarmy[g1])continue;
		if (gralive[g3] == DEAD)continue;
		if (connecthere(cn2, g1, g3))
			continue;  /* g1 and g3 can connect here, already, don't double count it */
		val = geteyespace3(army,grarmy[g2],grarmy[g3]) - geteyespace(army,grarmy[g2]);
		if (val > 16)val = 16;
		if (cnprot[cn2] == MIGHT_CONNECT && val > 8)
			val= 8;
		if (val < 0)
			val = 0;
		if (nextpot < NUMPOTENTIAL-1) {
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = CONNECT;
			pots[nextpot].pot_val = 0;		/* 6/03, might not get this pot since other connection has a threatened group */
			pots[nextpot].pot_max = val;
			pots[nextpot++].pot_where = cn2;
			}
#ifdef CHECK
		else
			outerr("cn out of pots!\n");
#endif
		}
	}

/* add up the eye space in common between two armies 
 * total eyespace for the combination of a1 and a2 (a1 connecting to a2)
 */

static int geteyespace(army_t a1, army_t a2) {
	list_t ptr;
	int eyes = 0,numpot = 0;
	list_t tmplist = EOL;
	if (comlist(armyeyerecs[a1],armyeyerecs[a2])) {
		cpylist(armyeyerecs[a1],&tmplist);
		mrglist(armyeyerecs[a2],&tmplist);
		for (ptr = tmplist; ptr != EOL; ptr = link[ptr])
			eyes += eyeval[list[ptr]];
		killist(&tmplist);
		}
	else
		eyes = armyeyes[a1] + armyeyes[a2];  /* none in common, just add */
	for (ptr = armyeyerecs[a2]; ptr != EOL; ptr = link[ptr]) {
		if (eyepot[list[ptr]] - eyeval[list[ptr]] >= 8 &&
			!inlist(list[ptr],&armyeyerecs[a1]))numpot++;
		}
	eyes += (numpot/2) * 8;
	/* if (armylibs[a2] > 15)eyes += 4; */
	return(eyes);
	}


/* add up the eye space in common between three armies - eyespace added to 
 * a1 and a2 for connecting to a3
 */

static int geteyespace3(army_t a1, army_t a2, army_t a3) {
	list_t rnlist = EOL,ptr;
	int eyes = 0, numpot = 0;
	cpylist(armyeyerecs[a1],&rnlist);
	mrglist(armyeyerecs[a2],&rnlist);
	mrglist(armyeyerecs[a3],&rnlist);
	for (ptr = rnlist; ptr != EOL; ptr = link[ptr]) {
		eyes += eyeval[list[ptr]];
		if (eyepot[list[ptr]] - eyeval[list[ptr]] >= 8)numpot++;
		}
	if (numpot >= 2)eyes += 8;
	if (numpot >= 4)eyes += 8;
	if (armylibs[a2] +armylibs[a3] > 15)eyes += 4;
	killist(&rnlist);
	return(eyes);
	}





/* see if army that can't run can capture neighbor to connect to army that can run 
 * or is already alive.
 */

static int threat_live(army_t army) {
	list_t ptr, ptr2, pptr;
	int changed = FALSE, totrun, best = 0, val, bestbest = 0;
	for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
		if (pots[list[ptr]].pot_type == THREAT ||
			pots[list[ptr]].pot_type == POTTHREAT) {
			totrun = 0;
			bestbest = 0;
			if (armynbp[pots[list[ptr]].pot_where] == EOL)
				getarmynbp((army_t)pots[list[ptr]].pot_where);
			for (ptr2 = armynbp[pots[list[ptr]].pot_where];
			    ptr2 != EOL; ptr2 = link[ptr2]) {
			    if (list[ptr2] == army)
			    	continue;
				if (armyrn_pot[list[ptr2]] > 3 ||
					armyrn_pot[army] <= 3) {     /* 4/20/99 */
					totrun += armyrn_pot[list[ptr2]];
			   	}
				if (armyrn_pot[list[ptr2]] > easyrun[A_COLOR(list[ptr2])] ||
				   A_ALIVE(list[ptr2]) <= WINS_SEMEAI) {	/* strong neighbor */  
					if (pots[list[ptr]].pot_val < 16) {
						pots[list[ptr]].pot_val = 16;
						if (pots[list[ptr]].pot_max < 16)
							pots[list[ptr]].pot_max = 16;
						changed = TRUE;
						}
					continue;
				}
				/* find one neighbor with 2 or more ways to make another eye which is not already alive */
				best = 0;	/* number of pots one eye or better */
				for (pptr = armypot[list[ptr2]]; pptr != EOL; pptr = link[pptr]) {
					if (pots[list[pptr]].pot_type == VITAL && 
						inlistm((sqr_t)(pots[list[pptr]].pot_where&EYEPOINTMASK), &armyvitalpoints[army], EYEPOINTMASK))
						continue;	/* would double count with me */
					if ((pots[list[pptr]].pot_type == THREAT ||
						 pots[list[pptr]].pot_type == POTTHREAT))
						 /* && pots[list[pptr]].pot_where == pots[list[ptr]].pot_where) eval order problem */
						continue;   /* don't double count this threatened group */
					if (pots[list[pptr]].pot_type == CONNECT &&
						(grarmy[cngr1[pots[list[pptr]].pot_where]] == army ||
						 grarmy[cngr2[pots[list[pptr]].pot_where]] == army))
						 continue;	/* don't count connection back to myself :) */
					if (pots[list[pptr]].pot_type == UNDERCUT ||
						pots[list[pptr]].pot_type == EXTEND)
						val = sumeyes[pots[list[pptr]].pot_val];
					else
						val = pots[list[pptr]].pot_val;
					if (val >= 8)
						best++;
				}
				if (best > bestbest)
					bestbest = best;
			}
			if (bestbest > 1) {	/* have two ways to get another eye */
				pots[list[ptr]].pot_val += 8;
				pots[list[ptr]].pot_max += 8;
				changed = TRUE;
			}
			if (totrun >= easyrun[A_COLOR(army)] && pots[list[ptr]].pot_val < 16) {
				pots[list[ptr]].pot_val = 16;
				if (pots[list[ptr]].pot_max < 16)
					pots[list[ptr]].pot_max = 16;
				changed = TRUE;
			}
			else if (totrun >= LIMP_RUN /* 5/01 && pots[list[ptr]].pot_val < 8 */) {
				if (bestbest == 1) {	/* 5/01 can run or make an eye */
					pots[list[ptr]].pot_val += 12;
					pots[list[ptr]].pot_max += 16;
				}
				else {
					pots[list[ptr]].pot_val += 8;
					pots[list[ptr]].pot_max += 8;
					if (pots[list[ptr]].pot_max < 12)
						pots[list[ptr]].pot_max = 12;
				}
				changed = TRUE;
			}
			else if (totrun > pots[list[ptr]].pot_val) {
				pots[list[ptr]].pot_val = totrun;
				changed = TRUE;
				if (totrun > pots[list[ptr]].pot_max)
					pots[list[ptr]].pot_max = totrun;
			}
		}
	}
		
	return(changed);
}
/* add extra value to a connection potential. */

static void addcnpot(army_t army, int val, int max, unsigned int cn)
{
	list_t ptr;
	if (val < 0) {
#ifdef CHECK
		outerror("addcnprot < 0");
#endif
		return;
	}
	if (val > 17)
		val = 17;
	for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
		if (pots[list[ptr]].pot_type == CONNECT &&
			pots[list[ptr]].pot_where == cn) { /* found it */
				pots[list[ptr]].pot_val += val;
				pots[list[ptr]].pot_max += max;
				return;
		}
	}
	if (nextpot < NUMPOTENTIAL-1) {  /* make new one */
		adflist(nextpot,&armypot[army]);
		pots[nextpot].pot_type = CONNECT;
		pots[nextpot].pot_val = val; 
		pots[nextpot].pot_max = max; 
		pots[nextpot++].pot_where = cn;
	}
#ifdef CHECK
	else
		outerr("cn out of pots!\n");
#endif
}


/* extra running ability we get for army connecting to a2.
 * doesn't count running at shared liberties
 */

int extrarun(army_t army, army_t a2)
{
	int extra, i;
	list_t ptr3;
	extra = armyrn_pot[a2];
	for (i = 0; i < NUMRUN; ++i) {  /* 5/99 don't count duplicates */
		for (ptr3 = armyrun[a2][i]; ptr3 != EOL; ptr3 = link[ptr3])
			if (inlist(list[ptr3], &armylbp[army]))
				extra -= runval[i];
		}
	if (extra < 0)
		extra = 0;
	return extra;
}

/* see if army can connect to a group that can run 
 * a group is not surrounded if it can connect to a running group!
 * don't count connections to strong groups, since they are already
 * counted as eye potential
 */

void getarmycnrn_pot(army_t army) {
	army_t a2;
	list_t ptr,ptr2;
	group_t g;
	int c,cn;
	
/*	if (armyrn_pot[army] >= 3)return; */
	c = grcolor[list[armygroups[army]]];
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[ptr];
		for (ptr2 = grcnp[g]; ptr2 != EOL; ptr2 = link[ptr2]) {
			cn = list[ptr2];
			if (CANCONN(cnprot[cn]) &&
				 grcolor[cngr1[cn]] == c) {
				a2 = grarmy[cngr1[cn]];
				if (a2 == army)a2 = grarmy[cngr2[cn]];
				if (a2 == army)continue;  /* don't connect to self */

				if (/* 1/23/99 armyrn_pot[a2] > 3 && */ armyeyespace[a2] <= 8) {
					armycnrn_pot[army] += extrarun(army, a2);
					}
				}
			}
		}
	}
	
/* see if army that can't run can connect to army that can run 
 * only add run ability if can connect to army with rn > 3,
 * and limit amount added to rn = 3, so that running ability
 * can only propagate one level, and not be dependent on order
 * of evaluation
 * if can connect to group that is already alive, 
 * or can make life, add new pot for life.
 */


static int connect_live(army_t army)
{
	army_t a2;
	list_t ptr, ptr2;
	group_t g;
	int c, cn, changed = FALSE, i, val, max;

	/*	if (armyrn_pot[army] >= easyrun[A_COLOR(army)])return(FALSE);  mor running makes group weaker :( */
	c = grcolor[list[armygroups[army]]];
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[ptr];
		for (ptr2 = grcnp[g]; ptr2 != EOL; ptr2 = link[ptr2]) {
			cn = list[ptr2];
			if (CANCONN(cnprot[cn]) && grcolor[cngr1[cn]] == c) {
					a2 = grarmy[cngr1[cn]];
					if (a2 == army)
						a2 = grarmy[cngr2[cn]];
					if (a2 == army)
						continue;  /* don't connect to self */
					/*	causes problems based on order of evaluation			if (armyrn_pot[a2] > 1 && armyrn_pot[army] < 2) 
					armyrn_pot[army] ++;  */

					/* first, see if the group can live by connecting */
					if (armyrn_pot[a2] >= easyrun[A_COLOR(a2)] ||
						armyrn_pot[army] > 3 && 
						armyrn_pot[a2] >= armyrn_pot[army] && 
						armyrn_pot[army] + extrarun(army, a2) >= easyrun[A_COLOR(a2)] ||
						A_ALIVE(a2) <= WINS_SEMEAI && armyeyes[a2] < 16) {
							addcnpot(army, 17, 17, cn);  /* 17 so bestpot will prefer connecting */
							changed = TRUE; 
					}
					else if (armyeyes[a2] + armybestpot[a2] >= 16 && 
						armyrn_pot[a2] > LIMP_RUN) {
							if (armyeyespace[army] + armybestpot[army] < 17)
								addcnpot(army, 17 - armyeyespace[army] + armybestpot[army], 17 - armyeyespace[army] + armybestpot[army], cn);  /* 17 so bestpot will prefer connecting */
							else
								addcnpot(army, 4, 4, cn);
							changed = TRUE; 
					}
#ifdef NEVER
					/* this code causes horrible mistakes when potentials are common to both groups! */
					else if (armysecond[a2] + armyrest[a2] - armyeyes[army] > 0) {  /* connect to alive group */
						/* subtract armyeyes[a1] since will be double counted */
						addcnpot(army, armysecond[a2] + armyrest[a2] - armyeyes[army], cn);  
						changed = TRUE; 
					}	
#endif
					else {
						val = 0;	/* eyes can get by connecting for sure */
						max = 0; /*armybestpot[a2] + armysecond[a2] - armyeyes[army]; 7/01 horrible double counting!	 */
						/* eyes extra max eyes possible  - don't double count my eyes */
						if (armyrn_pot[army] > 3 && armyrn_pot[a2] > 3 &&
							armyrn_pot[a2] + armyrn_pot[army] > LIMP_RUN ||
							armyrn_pot[a2] > LIMP_RUN) {
								if (armyeyespace[army] <= 8 && 
									armyeyespace[army] + armybestpot[army] <= 12)
									val += 12 - armyeyespace[army] - armybestpot[army];
								else 
									val += 4;  /* already alive, not worth much */
						}
						else if (armyrn_pot[a2] > 3) {
							addcnpot(army, 4, 8, cn);
							changed = TRUE;
						}
						else if (armyrn_pot[a2] > 0) {
							for (i = 0; i < NUMRUN; ++i) { /* is a real run for a2? */
								if (armyrun[a2][i] != EOL) {
									if (armyrn_pot[a2] > 1)
										addcnpot(army, 4, 4, cn);
									else
										addcnpot(army, 2, 4, cn);
									changed = TRUE;
									break;
								}
								if (val != 0 || max != 0) {
									if (max < val)
										max = val;
									addcnpot(army, val, max, cn);
									changed = TRUE;
								}
							}
						}
					}
			}
		}
	}
	return(changed);
}

/* get eye potential for running into center 
 */
int armyrunval(army_t army)
{
	sqr_t s;
	list_t ptr;
	int rtype, psum=0;
	int val = 0, bestrun = 0;
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (ld[s] >= 6 && ld[s] <= 8)
			continue;
		rtype = canrunhere(army,s);
		if (rtype >= 0) {
			if (runval[rtype] == 1)
				psum++;
			else if (lnbn[s] == 3 && !S_NEUTRAL(s) && ld[s] > 2 ||
				lnbn[s] == 2 && !S_NEUTRAL(s) && ld[s] > 4)
				val += run2val[rtype];  /* from 2 stone wall */
			else
				val += runval[rtype];
			if (runval[rtype] > bestrun)
				bestrun = runval[rtype];
		}
	}
	if (psum > 2)
		psum = 2;  /* at most two horrible running moves */
	if (val < 3)
		val += psum;  /* to check for completely surrounded */
	if (val < bestrun + psum)
		val = bestrun + psum;  /* least can count one run move */
	return val;
}

static void getarmyrn_pot(army_t army)
{
	sqr_t s;
	list_t ptr;
	int rtype,psum=0;
	list_t liblib = EOL;  /* list of liberties of liberties */
	int newnbrs;
	
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (ld[s] >= 6 && ld[s] <= 8)continue;
		rtype = canrunhere(army,s);
		if (rtype >= 0) {
			addlist(s,&armyrun[army][rtype]);
			if (runval[rtype] == 1)psum++;
		}
	}
	for (rtype = 0; rtype < NUMRUN; ++rtype) {
		if (runval[rtype] == 1)
			continue;
		for (ptr = armyrun[army][rtype]; ptr != EOL; ptr = link[ptr]) {
			s = list[ptr];
			newnbrs = mrglist(nblbp[s],&liblib);
			if (comlist(nblbp[s],armyrun[army][rtype]))
				armyrn_pot[army] += run2val[rtype];  /* from 2 stone wall */
			else if (newnbrs != lnbn[s])
				armyrn_pot[army] += (runval[rtype]*newnbrs)/lnbn[s]; 
				/* don't double count nearby runs of stronger type */
			else
				armyrn_pot[army] += runval[rtype];
		}
	}
	killist(&liblib);
	if (psum > 1)psum = 1;
	if (armyrn_pot[army] < 3)
		armyrn_pot[army] += psum;
}

/* miaialive figures out life and death for remaining armies
 * it can give aliveness values:
 * 1 - 2 eyes in eyespace plus territory and miai for another
 * 2 -  miai for 2 1/2  eyes or 2 eyes and potential for another 
 * 3 - 2 eyes in eyespace plus territory (in danger)
 * 4 - miai for 2 eyes exactly and some extra. (in danger)
 *	connection to group
 *	threatened neighbors
 *	extending for more eyespace
 *	taking a key eyemaking point
 *	defending undercut eyespace
 * return TRUE if determine aliveness of army, FALSE otherwise
 * if it returns FALSE, it sets aliveness to 23 (WEAK_GROUP)
 */
extern int numtvtacnodes;

static int miaialive(army_t army) {
	int secondbest,best,bestmax,rest,pthreat,poteyes,kopot;
	int bestval2,bestshared;
	sqr_t bestmove2,bestkillmove,bestkillmove2,bestmove, secondmove;
	int secondkocolor;
	int weakpot;  /* can't use here since not correct */
	int mineyes;  /* sum of eyemin */           
	int meyes;  /* eyespace plus shared eyes */
	int numrest;  /* number of potential eye moves in rest */
	int numn;
#ifdef CHECK
	if (armygroups[army] == EOL)
		outerror("miaialive for bad army\n");
#endif

	armyeyespace[army] = armyeyes[army];

	if (armyeyespace[army] >= 20 && armysize[army] > 3 && !A_THREATENED(army)) {
		newalive(army,HAS_TWO_EYES,pfac[HAS_TWO_EYES],-50);
		return(TRUE);
		}
	poteyes = getpoteyes(army,&mineyes);
	if (armyeyespace[army] >= 16 && armylibs[army] >= 15 && poteyes > 0 && !A_THREATENED(army)) {
		newalive(army,HAS_TWO_EYES,pfac[HAS_TWO_EYES],-50);
		return(TRUE);
		}

	if (armyeyespace[army] >= 16 && poteyes > 1 && !A_THREATENED(army) && !A_THREATENED(army)) {
		newalive(army,HAS_TWO_EYES,pfac[HAS_TWO_EYES],-50);
		return(TRUE);
		}
	/* 10/96 poteyes of 3 is not enough - let many groups die */
	if ((mineyes >= 8 && poteyes >= 4 || armyeyespace[army] + poteyes*4 >= 24) && !A_THREATENED(army)) {
		newalive(army,STRONG_MIAI,pfac[STRONG_MIAI],-50);
		return(TRUE);
		}
		
	getarmyth_pot(army);  /* threatened group eyes */
	getarmycn_pot(army);  /* connection for eyes */
	getarmyex_pot(army);  /* extend for eyes */
	getarmyuc_pot(army);  /* undercut territory, must be after ex_pot */
	numn = numnodes;
	getarmytv_pot(army);  /* vital eye point for eyes - must be last so vital points are first in pot list! */
	numtvtacnodes += (numnodes - numn);

	best = bestpot(army, &secondbest, &bestval2, &bestshared, &bestmove, &bestmove2, &bestkillmove, &bestkillmove2, &secondmove, &rest, &pthreat, &kopot, &weakpot, &numrest, &bestmax);
	armybestpot[army] = best;
	armybestpot2[army] = bestval2;
	armysecond[army] = secondbest;
	armybestshared[army] = bestshared;
	armybestmove[army] = bestmove;
	armybestmove2[army] = bestmove2;
	armysecondmove[army] = secondmove;
	armykillmove[army] = bestkillmove;
	armykillmove2[army] = bestkillmove2;
	armyrest[army] = rest;
	armypthreat[army] = pthreat;
	armynumrest[army] = numrest;
	armykopot[army] = kopot;
	armyweakpot[army] = weakpot;
	armyeyepotential[army] = bestmax + secondbest + rest;
	armybestmax[army] = bestmax;

	if (armyeyes[army] == armysafeeyes[army])
		armysafeeyes[army] += armyrest[army];
	else
		armysafeeyes[army] += armysecond[army];

	if (armyeyespace[army] + armyeyepotential[army] + armybestshared[army] < 16) {
		newalive(army,WEAK_GROUP,pfac[WEAK_GROUP],-50);  /* can't possibly be miai */
		return(FALSE);
		}


	if (A_THREATENED(army)) {
		newalive(army,WEAK_GROUP,pfac[WEAK_GROUP],-50);
		return(FALSE);
		}

	if (armysafeeyes[army] >= 16) {
		newalive(army,HAS_TWO_EYES,pfac[HAS_TWO_EYES],-50);
		return TRUE;
	}


	meyes = armyeyespace[army]+armybestshared[army];  /* eyes that I can get if I want.  I can always choose to take the shared eyes  */
	if (meyes >= 16) {
		if (armyeyepotential[army] > 12 &&
			secondbest >= 8)
			/* armyeyepotential[army] >= 16)  WRONG! 1019-4 mv 197 */
				newalive(army,STRONG_MIAI,pfac[STRONG_MIAI],-50);
		else newalive(army,BARELY_ALIVE,pfac[BARELY_ALIVE],-50);
		return(TRUE);
		}
	
	if (secondbest == 0)rest = 0;  /* may be potential in armyeyepotential that */
	   /* shouldn't be counted - due to two threatened groups for example */
	
	iskopoint(secondmove,&secondkocolor);
	if (secondkocolor == 1-A_COLOR(army)) {  /* ko is not miai */
		newalive(army,WEAK_GROUP,pfac[WEAK_GROUP],-50);  /* for now, make weak groups really weak */
		return(FALSE);
		}

	if (meyes + secondbest >= 8 && rest >= 28 && numrest >= 3||
	   rest >= 16 && meyes + secondbest >= 16 || 
	   rest >= 24 && meyes + secondbest >= 12 && numrest >= 2) {
		newalive(army,STRONG_MIAI,pfac[STRONG_MIAI],-50);
		return(TRUE);
		}

	if (meyes + secondbest >= 20 ||
	   meyes + secondbest >= 16 && rest >= 8 ||
		meyes == 8 && secondbest >= 8 || /* has one, can get another */
		 rest >= 20 && numrest >= 2 && meyes + secondbest >= 12 && meyes + best >= 16 ||
		meyes + secondbest >= 8 && numrest >= 2 && rest >= 24 && meyes + best >= 12 || 
		/* poteyes >= 4 && secondbest >= 8 || 4/01 to avoid miai alive with low gralprob values */
		secondbest >= 12 && secondbest + rest >= 24 && numrest >= 2) {
		newalive(army,MIAI,pfac[MIAI],-50);
		return(TRUE);
		}

	newalive(army,WEAK_GROUP,pfac[WEAK_GROUP],-50);  /* for now, make weak groups really weak */
	return(FALSE);
	}


int getpoteyes(army_t army, int *min) {
	list_t ptr;
	int pot = 0,plibs = 0;
	*min = 0;
	for (ptr = armyeyerecs[army]; ptr != EOL; ptr = link[ptr]) {
		if (eyepot[list[ptr]] >= eyeval[list[ptr]]+8 && eyetype[list[ptr]] != THRTEYE)++pot;
		*min += eyemin[list[ptr]];
		}
	if (pot < 2 && armylibs[army] >= 10) {
		for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr])
			if (lnbn[list[ptr]] > 2 && eyerec[list[ptr]] == 0 &&
				ltr1[list[ptr]] == 0)++plibs;
		pot += plibs/8;
		}
	return(pot);
	}

/* does moving at point s capture a ko, or throwin to make a ko
 * return in color the color that can connect the ko, or NOCOLOR is
 * there is no ko here.
 */
 
int iskopoint(sqr_t s, int *color) {
	int koflag;
	list_t tptr;
	if (s == NOSQUARE) {
		*color = NOCOLOR;
		return(FALSE);
		}
	*color = lnbf[s][1] != 0;
	if (s == kosquare)return(TRUE);
	if (lnbn[s] == 1 && lnbf[s][1-*color] == 0 &&  /* throwin ko */
		lnbn[list[nblbp[s]]] == 1 &&
		lnbf[list[nblbp[s]]][*color] == 0) {
		koflag = TRUE;
		for (tptr = nbgrp[s][*color]; tptr != EOL; tptr = link[tptr])
			if (G_NUMLIBS(list[tptr]) == 1) {
				koflag = FALSE;
				break;
				}
		if (koflag)
			for (tptr = nbgrp[list[nblbp[s]]][1-*color]; tptr != EOL; tptr = link[tptr])
				if (G_NUMLIBS(list[tptr]) == 1) {
					koflag = FALSE;
					break;
					}
		if (koflag)
			return(TRUE);
		}
	if (lnbn[s] == 0 && lnbf[s][1-*color] == 0) {  /* capture ko */
		koflag = 0;
		for (tptr = nbgrp[s][*color]; tptr != EOL; tptr = link[tptr]) {
			if (grlibs[list[tptr]] == 1) {
				if (grsize[list[tptr]] == 1)
					koflag ++;
				else {
					*color = NOCOLOR;
					return FALSE;	/* big group capture */
					}
				}
			}
		if (koflag == 1)return(TRUE);
		}
	*color = NOCOLOR;
	return(FALSE);
	}
      	
# define MAXPOTS 100
/* Find the eye potentials for army.
 * 
 * If army moves first, return/bestmove is the move and value that
 * gets the most eye potential.  bestval2/bestmove2 is the second
 * best move.  If the two values are the same, the moves are sorted
 * such that if the moves are vital points, bestmove2 has the least
 * eyemin - loses more if opponent gets two moves here.
 * If one of the two equal best moves captures a ko, it is used as
 * bestmove2.  If one of the two equal best moves connects a ko, it
 * is used as bestmove.  Later, if the best move connects a ko, aliveness
 * will be strong_ko, and if it captures a ko, aliveness will be weak_ko.
 * if there is a complete tie, the move closests to the upper right is chosen
 * for repeatability
 *
 * kopot adds up all of the eye potential in ko, whether or not the
 * ko capture is illegal.  If the group just needs to conenct the
 * ko to live, then bestpot will have the connecting move, and kopot
 * will double count the potential.  If the ko needs to be captured
 * to live, bestpot will have the capture, for 1/2 eye, and kopot
 * will have the other half.
 *
 * The two best moves of the enemy's for removing eye potential are
 * returned in bestkillmove and bestkillmove2.  bestkillmove must
 * remove at least one potential that is gained by bestmove (unless
 * there is no way to do so, for example an add-only eye vital point).
 *
 * Among potential not eliminated by bestkill, the best remaining eye
 * making move is identified as second/secondmove.
 *
 * illegal potential moves due to ko are not counted towards bestpot,
 * but their potential is counted and returned in kopot.  Kopot is the
 * amount of eye potential that can be gained by making a ko threat,
 * then capturing and connecting the ko.
 *
 * If a group has a pair of shared connections, bestpot returns the
 * potential available through the largest of the pair, since this
 * potential is always available.
 *
 */     	

/* OLD! if army moves first, what is most additional eyespace it can get 
 * look at all the eye potentials and find the two best moves
 *  (return in *bestmove and *bestmove2, with return value and *bestval2)
 * find the best eyes to add due to shared connections
 *
 * look at all moves that eliminate eye potential and find the best
 * killing moves (return in *bestkillmove and *bestkillmove2)
 *
 * among potential not eliminated by bestkill, find the best remaining
 * potential.  ( return in *secondmove and *second)
 *
 * see if same move can effect more than one potential (except threat) 
 * rest is total eye pot not counted in best and secondbest and kopot 
 * weakpot is eye potential of groups can connect to
 * kopot is ko potential for eyes
 */

static int bestconnval[NUMARMIES];	/* best pot val for each army can connect to */
static int maxconnval[NUMARMIES];	/* best pot val for each army can connect to */

int bestpot(army_t army, int *second, int *bestval2, int *bestshared, sqr_t *bestmove, sqr_t *bestmove2, sqr_t *bestkillmove, sqr_t *bestkillmove2, sqr_t *secondmove, int *rest, int *pthreat, int *kopot, int *weakpot, int *numrest, int *bestmax) {
	int best,best2,bestkill,val,maxval,tval,terr,tterr,kocolor,c;
	listval_t i;
	conn_t cn;
	int losebest, losebest2, tmplose;  /* how much do I lose if I let him remove bestpot */
	int thval,bigeyeterr = 0, bigeyes = 0;
	int havevital;
	list_t sharedarmy = EOL;
	list_t adpots[MAXPOTS], allpot = EOL, adptr, ptr, ptr2;
	list_t rmpots[MAXPOTS], allrmpot = EOL, rmptr;
	list_t bestpotnums = EOL;  /* list of indices into the pot[] array for those pots gained by bestmove */
	list_t killpotnums = EOL;  /* list of indices into the pot[] array for those pots destroyed by bestkillmove */
	int killsbest = FALSE;	/* bestkillmove kills a pot in bestpotnums */
	int killtmp;
	list_t kopotnums = EOL;
	list_t tmplist = EOL,tpotnums = EOL, rmbest = EOL;
	army_t a,atmp;
	list_t connarmy = EOL;
	list_t  cattmp = EOL;
	list_t eyecounted = EOL;  /* counted eyes by eyevitrec (eye is 8 lsb only) unsorted! */
	list_t eyptr, ept2;
	list_t resteyelist;  /* eyes counted in rest */
	list_t koeyes = EOL;
	sqr_t stmp;

	best = 0;
	*bestmax = 0;
	*kopot = 0;
	*weakpot = 0;
	*bestval2 = 0;
	*bestshared = 0;
	*bestmove = NOSQUARE;
	*bestmove2 = NOSQUARE;
	*bestkillmove = NOSQUARE;
	*bestkillmove2 = NOSQUARE;
	*second = 0;
	*secondmove = NOSQUARE;
	*numrest = 0;
    c = A_COLOR(army);

	/* find the total of big eyes and territory to help evaluate blocking moves */
	for (ptr = armyeyerecs[army]; ptr != EOL; ptr = link[ptr]) {
		if (eyetype[list[ptr]] == VERYBIGEYE) {
			bigeyeterr += cntlist(&eyeptr[list[ptr]]);
			bigeyes += eyeval[list[ptr]];
			}
		}
	if (bigeyeterr > 15)bigeyeterr = 15;

	/* look at each potential eye and find all moves to add or remove tha eye */
	
	addlist(army,&connarmy);  /* so don't count your own eyes */
	for (i = 0,ptr = armypot[army]; ptr != EOL && i < MAXPOTS; ++i,ptr = link[ptr]) {
		if (pots[list[ptr]].pot_type == CONNECT) {  /* check for weak connection */
			cn = pots[list[ptr]].pot_where;   /* add potential eyes that can connect to */
			if (cnprot[cn] == SHARED_CONNECT && 
				grarmy[cnshcent[cn]] == army) {
				a = grarmy[cngr1[cn]];
				if (a == army)
					a = grarmy[cngr2[cn]];
				if (addlist(a, &sharedarmy))	  /* added 7/11/97, full value for connection */
					*bestshared += pots[list[ptr]].pot_val;
				}
			a = grarmy[cngr1[cn]]; /* both groups might be different army */
			if (addlist(a,&connarmy)) {
				for (ptr2 = armyeyerecs[a]; ptr2 != EOL; ptr2 = link[ptr2])
					if (!inlist(list[ptr2],&armyeyerecs[army])) {
						*weakpot += eyepot[list[ptr2]] - eyeval[list[ptr2]];
						}
				}
			a = grarmy[cngr2[cn]];
			if (addlist(a,&connarmy)) {
				for (ptr2 = armyeyerecs[a]; ptr2 != EOL; ptr2 = link[ptr2])
					if (!inlist(list[ptr2],&armyeyerecs[army])) {
						*weakpot += eyepot[list[ptr2]] - eyeval[list[ptr2]];
						}
				}
			}
			
		/* get all adpots and rmpots */

		if (pots[list[ptr]].pot_max == 0) {
			adpots[i] = EOL;
			rmpots[i] = EOL;
			continue;  /* no potential here */
			}
		if (pots[list[ptr]].pot_type == VITAL) {
			for (ptr2 = eyevitrec[pots[list[ptr]].pot_where&EYEPOINTMASK]; ptr2 != EOL; ptr2 = link[ptr2])
				if (addlist((listval_t)(list[ptr2]&EYEPOINTMASK), &koeyes) && /* 9/99 only count each eye once */
					eyecolor[list[ptr2]&EYEPOINTMASK] == c &&
					eyeko[list[ptr2]&EYEPOINTMASK]) {
					*kopot += eyepot[list[ptr2]&EYEPOINTMASK] - eyeval[list[ptr2]&EYEPOINTMASK];
					}
			}
	
		adpots[i] = adpot(army,list[ptr]);
		rmpots[i] = rmpot(army,list[ptr]);
		cpylist(adpots[i],&tmplist);
		mrflist(&tmplist,&allpot,0);  /* every potential move */
		cpylist(rmpots[i],&tmplist);
		mrflist(&tmplist,&allrmpot,0);  /* every potential move */
		}

	unflist(allpot);  /* each move in list once only */
	unflist(allrmpot);  /* each move in list once only */
	killist(&koeyes);

	/* check each move that adds potential - find the best and 
	   second best eye making moves. (bestmove, bestmove2) */
	best = 0;
	losebest = 0;	/* just used for breaking ties. play move that removes biggest ko threat */
	losebest2 = 0;
	for (adptr = allpot; adptr != EOL; adptr = link[adptr]) {
		terr = 0;  /* for each move */
		val = 0;
		maxval = 0;		/* for bestmax */
		killist(&tpotnums);
		killist(&connarmy);
/*		cpylist(sharedarmy, &connarmy); no, already rejected below */
		tmplose = 0;
		thval = 0;
		if (board[list[adptr]] != NOGROUP) {
#ifdef CHECK
			outerror("allpot has illegal move\n");
#endif
			continue;
			}
		havevital = FALSE;	/* don't double count vital point and blocking move */
		for (i = 0, ptr = armypot[army]; ptr != EOL && i < MAXPOTS; ++i, ptr = link[ptr]) {
			if (pots[list[ptr]].pot_max == 0)continue;
			if (!inflist(list[adptr],&adpots[i]))continue;
			if (pots[list[ptr]].pot_type == POTTHREAT)continue;
				/* POTTHREAT aren't guaranteed */

			    /* if this point is kosquare and a connection, it's surrounded by my stones, and
				 * the opponent can't play here.  I can play here */                                                  
			if (pots[list[ptr]].pot_type == CONNECT) {
				cn = pots[list[ptr]].pot_where; /* don't double count connections to same army through same point */
				atmp = grarmy[cngr1[cn]];
				if (atmp == army)
					atmp = grarmy[cngr2[cn]];
				if (atmp == army)
					continue;
				if (inlist(atmp,&sharedarmy))
				   continue;  /* bestpot can't include connections to shared army - why? */
				if (inlist(atmp,&connarmy)) {
					if (pots[list[ptr]].pot_val > bestconnval[atmp])
						bestconnval[atmp] = pots[list[ptr]].pot_val;
					if (pots[list[ptr]].pot_max > maxconnval[atmp])
						maxconnval[atmp] = pots[list[ptr]].pot_max;
					addlist(i, &tpotnums);
					continue;
					}
				addlist(atmp,&connarmy);
				bestconnval[atmp] = pots[list[ptr]].pot_val;
				maxconnval[atmp] = pots[list[ptr]].pot_max;
				addlist(i, &tpotnums);
				continue;
				}
				
			/* found each eye potential that can be realized by this move */
			if (pots[list[ptr]].pot_type == THREAT) {
				atmp = (army_t)pots[list[ptr]].pot_where;
				thval = 0;
				if (armynbp[atmp] == EOL)getarmynbp(atmp);
				for (ptr2 = armynbp[atmp]; ptr2 != EOL; ptr2 = link[ptr2]) {	/* allocate it to the various armies */
					if (list[ptr2] != army && !inlist(list[ptr2],&connarmy)) {
						addlist(list[ptr2],&connarmy);
						bestconnval[list[ptr2]] = armyeyespace[list[ptr2]];
						maxconnval[list[ptr2]] = armyeyespacemax[list[ptr2]];
						thval += armyeyespace[list[ptr2]];
						}
					}
				thval = pots[list[ptr]].pot_val-thval;	/* how much left over */
				if (thval < 0)
					thval = 0;
				val += thval; 
				maxval += thval;
				}
			else if (pots[list[ptr]].pot_type == UNDERCUT) {
				if (!havevital)
					terr += pots[list[ptr]].pot_max;
				else if (pots[list[ptr]].pot_val > 0) {
					val += 1;	/* prefer vital point that also blocks */
					maxval += 1;
				}
			}
			else if (pots[list[ptr]].pot_type == EXTEND) {
				if (!havevital)
					terr += pots[list[ptr]].pot_max;	/* vital point more accurate */
			}
				  /* prefer to capture if all else is equal */
			else {
				val += (pots[list[ptr]].pot_max+pots[list[ptr]].pot_val)/2;  /* use max potential for best moves 3/02 use mean value */
				maxval += pots[list[ptr]].pot_max;
			}
			if (pots[list[ptr]].pot_type == VITAL) {
				havevital = TRUE;
				for (ptr2 = eyevitrec[pots[list[ptr]].pot_where&EYEPOINTMASK]; ptr2 != EOL; ptr2 = link[ptr2])
					if (eyecolor[list[ptr2]&EYEPOINTMASK] == c &&
						eyeval[list[ptr2]&EYEPOINTMASK] - eyemin[list[ptr2]&EYEPOINTMASK] > tmplose)
						tmplose = eyeval[list[ptr2]&EYEPOINTMASK] - eyemin[list[ptr2]&EYEPOINTMASK];
				}
			addlist(i, &tpotnums);
			}
		for (ptr2 = connarmy; ptr2 != EOL; ptr2 = link[ptr2]) {
			val += bestconnval[list[ptr2]];
			maxval += maxconnval[list[ptr2]];
		}
			
		if (terr != 0) {
			val += sumeyesmax[terr]+1;  /* prefer to block or extend */
			maxval += sumeyesmax[terr]+1;  /* prefer to block or extend */
		}
        
		if (maxval > *bestmax)
			*bestmax = maxval;
        if ((sqr_t)list[adptr] == kosquare && lnbf[(sqr_t)list[adptr]][1-c]) {  /* potential for winning ko */
        	*kopot += val;  /* when illegal for me to play here, not illegal for opponent */
        	}	
		else if (val > best ||	/* better value */
			val == best && tmplose > losebest ||  /* better at avoiding further loss */
			val == best && tmplose == losebest && (sqr_t)list[adptr] < *bestmove) { /* consistency */
			*bestval2 = best;
			*bestmove2 = *bestmove;  /* new second best and best */
			losebest2 = losebest;
			killist(&bestpotnums);	/* new list of potentials for this best move */
			bestpotnums = tpotnums;
			tpotnums = EOL;
			
			best = val;
			*bestmove = list[adptr];
			losebest = tmplose;
			}
		else if (val > *bestval2 || val == *bestval2 && tmplose > losebest2 ||
			val == *bestval2 && tmplose == losebest2 && (sqr_t)list[adptr] < *bestmove2) {
			*bestval2 = val;  /* new second best move only */
			*bestmove2 = list[adptr];
			losebest2 = tmplose;
			}
		killist(&tpotnums);
		}           
	/* TODO - must integreate this into the test above so bestpotnums is correct */	
	if (best == *bestval2 &&  /* exchange best and best2 */
		(iskopoint(*bestmove,&kocolor) && kocolor == c ||
		 iskopoint(*bestmove2,&kocolor) && kocolor == 1-c)) {
		 val = best;     
		 best = *bestval2;
		 *bestval2 = val; 
		 tmplose = losebest;
		 losebest = losebest2;
		 losebest2 = tmplose;
		 stmp = *bestmove;
		 *bestmove = *bestmove2;
		 *bestmove2 = stmp;
		}

	/* check each move - find the best and second best eye killing moves. */
	
	bestkill = 0;
	best2 = 0;	
	losebest = 0;
	losebest2 = 0;
	for (rmptr = allrmpot; rmptr != EOL; rmptr = link[rmptr]) {
		if ((sqr_t)list[rmptr] ==  kosquare && lnbf[(sqr_t)list[rmptr]][1-c])continue;  /* illegal ko capture */
		terr = 0;  /* for each move */
		val = 0;
		killist(&connarmy);
		killist(&tpotnums);
		tmplose = 0;
		thval = 0;
		havevital = FALSE;
		for (i = 0, ptr = armypot[army]; ptr != EOL && i < MAXPOTS; ++i, ptr = link[ptr]) {
			if (pots[list[ptr]].pot_val == 0)continue;
			if (!inflist(list[rmptr],&rmpots[i]))continue;
			if (pots[list[ptr]].pot_type == POTTHREAT)continue;

			/* found each eye potential that can be destroyed by this move */
		/* in kill need to block multiple connections first */
			if (pots[list[ptr]].pot_type == CONNECT) {
				cn = pots[list[ptr]].pot_where; /* don't double count connections to same army through same point */
	/*				if (cnprot[cn] != SHARED_CONNECT)   can connect either side of shared connect */
				atmp = grarmy[cngr1[cn]];
				if (atmp == army)
					atmp = grarmy[cngr2[cn]];
				if (atmp == army)
					continue;
				if (inlist(atmp,&connarmy)) {
					/*val++;  /* still better to cut multiple connections at once */
					/*if (pots[list[ptr]].pot_val > bestconnval[atmp]) ful value for multiple cuts when removing 9/05*/
					bestconnval[atmp] += pots[list[ptr]].pot_val;
					addlist(i,&tpotnums);  /*list of pots killed by this move */
					continue;
				   	}
				addlist(atmp,&connarmy);
				bestconnval[atmp] = pots[list[ptr]].pot_val;
				addlist(i,&tpotnums);  /*list of pots killed by this move */
				continue;
				}
			if (pots[list[ptr]].pot_type == THREAT) {
				atmp = (army_t)pots[list[ptr]].pot_where;
				thval = 0;
				if (armynbp[atmp] == EOL)getarmynbp(atmp);
				for (ptr2 = armynbp[atmp]; ptr2 != EOL; ptr2 = link[ptr2]) {
					if (list[ptr2] != army && !inlist(list[ptr2],&connarmy)) {
						addlist(list[ptr2],&connarmy);
						bestconnval[list[ptr2]] = armyeyespace[list[ptr2]];
						maxconnval[list[ptr2]] = armyeyespacemax[list[ptr2]];
						thval += armyeyespace[list[ptr2]];
						}
					}
				thval = pots[list[ptr]].pot_val-thval;	/* how much left over */
				if (thval < 0)
					thval = 0;
				val += thval; 
				maxval += thval;
				}
			else if (pots[list[ptr]].pot_type == UNDERCUT) {
				if (!havevital)
					terr += pots[list[ptr]].pot_max;
				}
			else if (pots[list[ptr]].pot_type == EXTEND)
				terr += pots[list[ptr]].pot_max;
				  /* prefer to capture if all else is equal */
			else
				val += pots[list[ptr]].pot_max;  /* use max pot for kill moves so finds same ones as bestpot */
			if (pots[list[ptr]].pot_type == VITAL) {
				havevital = TRUE;
				for (ptr2 = eyevitrec[pots[list[ptr]].pot_where&EYEPOINTMASK]; ptr2 != EOL; ptr2 = link[ptr2])
					if (eyecolor[list[ptr2]&EYEPOINTMASK] == c &&
						eyeval[list[ptr2]&EYEPOINTMASK] - eyemin[list[ptr2]&EYEPOINTMASK] > tmplose)
						tmplose = eyeval[list[ptr2]&EYEPOINTMASK] - eyemin[list[ptr2]&EYEPOINTMASK];
				}
			addlist(i,&tpotnums);  /*list of pots killed by this move */
			}
		for (ptr2 = connarmy; ptr2 != EOL; ptr2 = link[ptr2]) {
			val += bestconnval[list[ptr2]];
		}
		if (terr != 0)
			val += sumeyesmax[terr]+1;  /* prefer to block or extend */

		killtmp = cmflist(tpotnums, bestpotnums);
		if (killtmp && !killsbest || /* prefer to kill a bestpot at any value */
			killtmp == killsbest && /* only replace based on value if both moves are or aren't killing best move */
			(val > bestkill || val == bestkill && tmplose > losebest ||
		   val == bestkill && tmplose == losebest && 
		   /*!inlist(i,&killpotnums) && can't be right */(sqr_t)list[rmptr] < *bestkillmove)) {
			*bestkillmove2 = *bestkillmove;  /* new second best and best */
			best2 = bestkill;
			losebest2 = losebest;
			
			killist(&killpotnums);  /* new best */
			killpotnums = tpotnums;
			tpotnums = EOL;
			killsbest = killtmp;
			bestkill = val;
			*bestkillmove = list[rmptr];
			losebest = tmplose;
			}
		else if (val > best2 || val == best2 && tmplose > losebest2 ||
			val == best2 && tmplose == losebest2 && (sqr_t)list[rmptr] < *bestkillmove2) {
			killist(&tpotnums);  /* new second best move only */
			best2 = val;
			*bestkillmove2 = list[rmptr];
			losebest2 = tmplose;
			}
		else killist(&tpotnums);
		}

	if (link[armygroups[army]] == EOL && A_THREATENED(army) == 2 && 
		grcapmove[list[armygroups[army]]] != NOSQUARE) {	/* single threatened army */
		*bestkillmove2 = *bestkillmove;
		best2 = bestkill;
		losebest2 = losebest;
		*bestkillmove = grcapmove[list[armygroups[army]]];
		}

	if (*bestkillmove != NOSQUARE)
		cpylistm(eyevitrec[*bestkillmove],&eyecounted,EYEPOINTMASK);

		
    /* find second best potential (assuming bestkillmove by opponent, so this move
	   can't use any of the potentials that were killed by bestkillmove ) */

	for (adptr = allpot; adptr != EOL; adptr = link[adptr]) { /* check each move */
		terr = 0;
		val = 0;
		if (board[list[adptr]] != NOGROUP)
			continue;	/* illegal move on a stone (a bug) */
		if ((sqr_t)list[adptr] == kosquare && lnbf[(sqr_t)list[adptr]][1-c])
			continue;  /* illegal ko capture */
		if ((sqr_t)list[adptr] == *bestmove)
			continue; /* 5/01 still horrible to have move best and second best 
			6/99 best kill might not be killing best due to bug */
		killist(&connarmy);
		havevital = FALSE;
		for (i = 0, ptr = armypot[army]; ptr != EOL && i < MAXPOTS; ++i, ptr = link[ptr]) {
			if (pots[list[ptr]].pot_val == 0)continue;
			if (inflist(i,&killpotnums))continue;  /* this pot was counted in bestkillpot */
			if (!inflist(list[adptr],&adpots[i]))continue;
			if (pots[list[ptr]].pot_type == POTTHREAT)continue;
				/* POTTHREAT aren't guaranteed */
			
			/* if (inflist(*bestmove,&adpots[i]))continue;  NO-using killpotnums instead. 
			  counted in best */
			if (pots[list[ptr]].pot_type == VITAL) {
				havevital = TRUE;
				for (eyptr = eyecounted; eyptr != EOL; eyptr = link[eyptr]) {
					if (inlistm(list[eyptr],&eyevitrec[pots[list[ptr]].pot_where&EYEPOINTMASK],EYEPOINTMASK))
						goto end;  /* bestmove can kill eye */
					for (ept2 = eyevitrec[pots[list[ptr]].pot_where&EYEPOINTMASK]; ept2 != EOL; ept2 = link[ept2])
						if (comlistm(eyevital[list[eyptr]], eyevital[list[ept2]&EYEPOINTMASK], EYEPOINTMASK))
							goto end;
						/* best move and this move share vital point */
					}
				}
			if (pots[list[ptr]].pot_type == CONNECT &&
				cnprot[pots[list[ptr]].pot_where] != SHARED_CONNECT) {  /* can connect on either side of shared connection */
				cn = pots[list[ptr]].pot_where; /* don't double count connections to same army */
				atmp = grarmy[cngr1[cn]];
				if (atmp == army)
					atmp = grarmy[cngr2[cn]];
				if (atmp == army)
					continue;
				if (inlist(atmp, &sharedarmy))
					continue;
				if (inlist(atmp, &connarmy)) {
					if (pots[list[ptr]].pot_val > bestconnval[atmp])
						bestconnval[atmp] = pots[list[ptr]].pot_val;
					continue;
					}
				addlist(atmp,&connarmy);
				bestconnval[atmp] = pots[list[ptr]].pot_val;
				continue;
				}
			if (pots[list[ptr]].pot_type == UNDERCUT) {
				if (!havevital)
					terr += pots[list[ptr]].pot_val;
				}
			else if (pots[list[ptr]].pot_type == EXTEND)
				terr += pots[list[ptr]].pot_val;
			else
				val += pots[list[ptr]].pot_val;
			end:;
			}
		for (ptr2 = connarmy; ptr2 != EOL; ptr2 = link[ptr2]) {
			val += bestconnval[list[ptr2]];
		}
		if (terr != 0)
			val += sumeyes[terr];
		val -= losebest;
		if (val > *second || val == *second && (sqr_t)list[adptr] > *secondmove) {
			*second = val;
			*secondmove = list[adptr];
			}
		}
	if (*secondmove != NOSQUARE) {
		cpylistm(eyevitrec[*secondmove],&cattmp,EYEPOINTMASK);
		catlist(&cattmp,&eyecounted);  /* list of eye numbers involved in this vital point move */
		}
	if (*second > 96)
		*second = 96;

	resteyelist = EOL;  /* eyes counted in rest of potential */	
	killist(&connarmy);
	killist(&sharedarmy);
	*rest = 0;  /* find rest of potential */
	*pthreat = 0;
	terr = val = 0;
	for (i = 0, ptr = armypot[army]; ptr != EOL && i < MAXPOTS; ++i, ptr = link[ptr]) {
		tval = 0;
		tterr = 0;
		if (pots[list[ptr]].pot_val == 0)continue;
		if (inflist(i,&killpotnums))continue;  /* this pot was killed by bestkillmove */
		/* if (inflist(i,&kopotnums))continue;  this pot was counted in kopot */
		if (pots[list[ptr]].pot_type == POTTHREAT) {
			*pthreat += pots[list[ptr]].pot_val;
			continue;	/* don't count these in rest, but don't cancel with best, second best 5/99 */
			}
		if (inflist(*bestmove,&adpots[i]))continue;  /* counted in best */
		if (inflist(*secondmove,&adpots[i]))continue;  /* counted in second best */
		/* if (cmflist(adpots[i],rmbest))continue;   conflicts with best */
		if (pots[list[ptr]].pot_type == VITAL) {
			for (eyptr = eyecounted; eyptr != EOL; eyptr = link[eyptr]) {
				if (inlistm(list[eyptr],&eyevitrec[pots[list[ptr]].pot_where&EYEPOINTMASK],EYEPOINTMASK))
					goto end2;  /* bestmove or secondmove can kill eye */
					/* best move and this move share vital point */
				}
			for (ept2 = eyevitrec[pots[list[ptr]].pot_where&EYEPOINTMASK]; ept2 != EOL; ept2 = link[ept2]) {
				if (inlist((listval_t)(list[ept2]&EYEPOINTMASK),&resteyelist))
					continue;
				if (eyecolor[list[ept2]&EYEPOINTMASK] == 1-c)
					continue;	/* don't count enemy eyes */
				tval += eyepot[list[ept2]&EYEPOINTMASK] - eyeval[list[ept2]&EYEPOINTMASK];
				addlist((listval_t)(list[ept2]&EYEPOINTMASK),&resteyelist);
				}

			continue;
			}
		if (pots[list[ptr]].pot_type == CONNECT) {
			cn = pots[list[ptr]].pot_where; /* don't double count connections to same army */
			atmp = grarmy[cngr1[cn]];
			if (atmp == army)
				atmp = grarmy[cngr2[cn]];
			if (atmp == army)continue;
			if (inlist(atmp, &connarmy)) {
				if (pots[list[ptr]].pot_val > bestconnval[atmp])
					bestconnval[atmp] = pots[list[ptr]].pot_val;
				continue;
			}
			addlist(atmp,&connarmy);
				bestconnval[atmp] = pots[list[ptr]].pot_val;
			continue;
			}
		if (pots[list[ptr]].pot_type == UNDERCUT ||
			pots[list[ptr]].pot_type == EXTEND)
			tterr += pots[list[ptr]].pot_val;
		else
			tval += pots[list[ptr]].pot_val;
		end2:;
		val += tval;
		terr += tterr;
		if (tval + sumeyes[tterr] >= 8)
			(*numrest)++;       /* number of different eye making moves in rest */
		}					   
	for (ptr2 = connarmy; ptr2 != EOL; ptr2 = link[ptr2]) {
		val += bestconnval[list[ptr2]];
	}
	*rest = val + sumeyes[terr] + sumpots[terr];
	if (*second == 0)
		*rest = 0;	/* 5/02 can't have any remaining eyes if second turns out ot be zero */
	
	killist(&eyecounted);		
	killist(&resteyelist);
	killist(&allpot);
	killist(&allrmpot);
	for (i = 0,ptr = armypot[army]; ptr != EOL && i < MAXPOTS; ++i,ptr = link[ptr]) {
		killist(&adpots[i]);
		killist(&rmpots[i]);
		}
	killist(&bestpotnums);
	killist(&killpotnums);
	killist(&kopotnums);
	killist(&rmbest);
	killist(&connarmy);
	if (best > 96)
		best = 96;
	if (*bestshared > 96)
		*bestshared = 96;
	if (*bestval2 > 96)
		*bestval2 = 96;
	if (*second > 96)
		*second = 96;
	if (*rest > 96)
		*rest = 96;
	if (*pthreat > 96)
		*pthreat = 96;
	if (*kopot > 96)
		*kopot = 96;
	return(best);
	}




/* update armyweak potential with the number of weak adjacent armies that
 * are not threatened - only count armies big enough to give two eyes if
 * captured.  (not big enough not implemented yet)
 */

static void getarmywk_pot(army_t army) {
	list_t ptr;
	group_t g;
	int i,ldtmp;
	sqr_t sn;
	armywk_pot[army] = 0;
	if (armynbp[army] == EOL)getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[armygroups[list[ptr]]];
		if (/* !G_THREATENED(g) (so semeaialive works) */ gralive[g] <= WEAK_GROUP &&
				gralive[g] > SEMEAI && 
				(armysize[list[ptr]] > 1 || armylibs[list[ptr]] >= 3) ||   /* 4/00 allow size 1 groups to be in semeai */
				gralive[g] == SEKI)
			armywk_pot[army]++;
		else if (armysize[army] == 1 && gralive[g] > ALIVE && grlibs[g] < 4)
			armywk_pot[army]++;  /* some potential here for mischief */
		}
	if (armywk_pot[army] == 0)
		for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr])
			if (S_NEUTRAL(list[ptr])) {
				i = fdir[list[ptr]];
				for (ldtmp = ldir[i]; i < ldtmp; ++i) {
					sn = list[ptr]+nbr[i];
					if (S_COLOR(sn) == 1-A_COLOR(army) && S_ALIVE(sn) <= WEAK_GROUP &&
					S_ALIVE(sn) > SEMEAI || S_ALIVE(sn) == SEKI) {
						armywk_pot[army]++;
						break;
						}
					}
				}
	}

/* check army to see if it can't win any semeai againt armies other than winner.
 * army's neighbors must all be alive or DEAD or can run away.
 * return TRUE if army can't win fights against all neighbors except winner.
 * already know that it can't win against winner
 * it' sok if it can make a seki
 */
 
static int cant_win_semeai(army_t army, army_t winner) {
	list_t ptr;
	int res;
	if (armynbp[army] == EOL)getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] == winner)continue;
		if (A_ALIVE(list[ptr]) == DEAD)continue;
		if (A_ALIVE(list[ptr]) < WINS_SEMEAI)continue;
		res = semeai_result(army, (army_t)list[ptr]);
		if (res >= 2)continue;
		return(FALSE);
		}
	return(TRUE);
	}	

/* isseki returns TRUE if army is alive as a seki 
 * this means that there is an opponent group with common liberties
 * that can't live and can't bring this group into atari without
 * being captured, and is worth two eyes if it is captured.
 * the opposing group might not also be a seki
 * The opposing group must not be able to capture another one.
 * There are several cases:
 *
 *
 * 1:  The opposing group is dead and surrounded.  In this case, I have
 *     enough liberties to capture the enclosed group, but I only get
 *     one eye when I do, and the enclosed group can't make nakade.
 * 2:  The opposing group is threatened and surounded (and I have no eyes).
 *     Seki if capturing opposing group only gives one eye, and
 *     army has no other eyes, but
 *     opp group plus liberties is too big for him to make nakade.
 * 3:  The opposing group can't be captured.
 *     Seki if opp group can't make nakade, and
 *     capturing opposing group doesn't connect out
 *     or if opposing group can make nakade, but capturing him makes a
 *     false eye real.
 * it is possible that I am threatened, but can make nakade if he moves
 * to capture me.
 */

/* TO DO: add code for 2 liberties, but not threatened */

int isseki(army_t army)
{
	group_t g2;
	list_t ptr;
	int eyes,maxeyes;

/*	if (A_THREATENED(army))return(FALSE); no! interior surrounded group that can't make nakade can be seki */

	if (armynbp[army] == EOL)
		getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		g2 = (group_t)list[armygroups[list[ptr]]];
		if (gralive[g2] < SEKI || 
			gralive[g2] > WEAK_GROUP && gralive[g2] != DEAD)
			continue;
        if (armyeyespace[list[ptr]] + armyeyepotential[list[ptr]] >= 16)
			continue;
		if (armyrn_pot[list[ptr]])
			continue;
		if (A_ALIVE(list[ptr]) != DEAD && /* obviously win vs dead group - but if can;t make nakade, still seki */
			semeai_result(army, (army_t)list[ptr]) != 5)
			continue;
		if (newlist(armygroups[army], grnbp[g2]) == 0 && /* nakade doesn't matter if the group is cutting */
			canmakenakade((army_t)list[ptr]) &&	/* enemy can force one eye here */
			(A_ALIVE(list[ptr]) == DEAD || armyeyespace[list[ptr]] < 8)) {
			continue;
		}
		eyes = armyeyespace[army];
		eyes += eyesifcapture((army_t)list[ptr], army, &maxeyes);
		/* make sure the other group can't capture another nearby group */
		if (eyes >= 16 && cant_win_semeai((army_t)list[ptr], army))
			return TRUE;
		if (armynbp[list[ptr]] == EOL)
			getarmynbp((army_t)list[ptr]);
		if ((armynbp[list[ptr]] == EOL || 
			link[armynbp[list[ptr]]] == EOL))  /* surrounded enemy cant nakade */
			return TRUE;
	}
	return FALSE;
}

/* army can win a semeai and get two eyes while doing it */

int win_semeai(army_t  army, group_t *enemy) 
{
	list_t ptr;
	group_t g;
	int eyes,maxeyes;
	*enemy = NOGROUP;
	if (armynbp[army] == EOL)
		getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[armygroups[list[ptr]]];

		if (gralive[g] < WINS_SEMEAI || gralive[g] > WEAK_GROUP)
			continue;
		if (armyeyespace[list[ptr]] + armyeyepotential[list[ptr]] >= 16)
			continue;
		if (armyrn_pot[list[ptr]])
			continue;
		if (semeai_result(army,(army_t)list[ptr]) != 0)
			continue;
		eyes = armyeyespace[army];
		eyes += eyesifcapture((army_t)list[ptr],army,&maxeyes);
		if (eyes >= 16 && 
			/*(armylibs[list[ptr]] > 2 || 1/03 - can't make taking away a liberty worse result /* groups of 1-3 liberties must be read dead */
			/* armyrn_pot[army] >= LIMP_RUN) &&  /* unless I can also run away */
			loses_all_semeai((army_t)list[ptr],army, 0)) {
				*enemy = g;
				return TRUE;
		}
	}
	return FALSE;
}
	
/* check army to see if it really loses the semeai against winner.
 * army's neighbors must all be alive or DEAD or can run away.
 * return TRUE if army loses fights against all neighbors except winner.
 * already know that it loses against winner
 */
 
static int loses_all_semeai(army_t army, army_t winner, int rval) {
	list_t ptr;
	int res;
	if (armynbp[army] == EOL)getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] == winner)continue;
		if (A_ALIVE(list[ptr]) == DEAD)continue;
		/*	if (A_ALIVE(list[ptr]) == STRONG_SEMEAI)continue; no, causes order dependencies */
		if (A_ALIVE(list[ptr]) <= BARELY_ALIVE)continue;
		res = semeai_result((army_t)list[ptr],army);
		if (res > rval)
			return(FALSE);
	}
	return(TRUE);
}	

/* army is in (one or more) semeai and we don't know who will win and 
 * if army wins it
 * gets two eyes or a seki
 * return new aliveness value and new alive prob (50 to -50), or FALSE
 * and best probability of winning a semeai to get two eyes
 * return the minimum and maximum eyes gained from semeais that are won, even if opponent moves first
 */

int uns_semeai(army_t army, int *alprob, int *semprob, int *mineyes, int *maxeyes)
{
	list_t ptr;
	group_t g;
	int l1min, l1max, l1typ, hemin, hemax, memin, memax, sekinbr = FALSE;
	int eyes, min, max, sr, gl = 0, toteyes;
	int winprob = 0;  /* prob of winning (0-100) - maximum of all neighbors */
	int prob, found = FALSE;
	*alprob = -50;
	*semprob = -50;
	*mineyes = 0;
	*maxeyes = 0;
	eyes = armyeyespace[army];
	toteyes = eyes;
	if (armynbp[army] == EOL)
		getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[armygroups[list[ptr]]];
		/* 7/99 need sem prob to win even if threatened group moves first
		if (G_THREATENED(g) == 2)continue;  /* can still win semeai against weakly threatened group */
		/* threatened group will be counted as potential eye already */
		if (gralive[g] == SEKI)
			sekinbr = TRUE;
		if (gralive[g] <= WINS_SEMEAI && gralive[g] != SEKI && gralive[g] != MIAI ||
			gralive[g] > WEAK_GROUP)
			continue;
		if (armyrn_pot[grarmy[g]] >= SEMEAI_RUN)
			continue;

		sr = semeai_result(army, grarmy[g]);
		if (sr == 4 || sr == 6)
			continue;
		if (G_THREATENED(g) != 2)
			found = TRUE;	/* can't be weak semeai since threatened group already counted as eye potential */

		min = eyesifcapture((army_t)list[ptr], army, &max);
		toteyes += max;
		if (sr == 0) {
			*mineyes += min;
			*maxeyes += max;
		}
		if (G_THREATENED(g) == 2)
			toteyes -= eyepot[eyerec[mvs[grpieces[g]]]];
		if (G_THREATENED(g) != 2 && eyes + max >= 16 && sr <= 1 && gralive[g] != SEKI && gralive[g] != MIAI &&
			armyeyespace[list[ptr]] + armybestpot[army] < 16 && loses_all_semeai((army_t)list[ptr], army, 1)) {
				/* maximum value is same as wins_semeai */
				prob = (winsemprob(army, (army_t)list[ptr], 0, 0) - 50) * pfac[WINS_SEMEAI] / 50;
				if (prob > *alprob) {
					*alprob = prob;
					*semprob = *alprob;
				}
				gl = STRONG_SEMEAI;
		}
		if (gl == STRONG_SEMEAI)
			continue;

		prob = winsemprob(army, (army_t)list[ptr], 0, 0);	/* 0-100, 100 is better */
		if (eyes + max >= 16 && prob - 50 > *semprob)
			*semprob = prob - 50;	/* probability of winning a semeai that can gain two eyes */
		if (gralive[g] == SEKI && prob > 10)
			prob = 10;
		if (prob > winprob)
			winprob = prob;
		if (sr == 7)
			gl = SEMEAI;
		else if (max >= 16 && 
			(gralive[g] != SEKI && (sr <= 2 || sr == 5)))	/* can be unsettled against a seki if I can make a seki myself */
			gl = SEMEAI;
	}
	if (gl == STRONG_SEMEAI)
		return gl;
	semeailibs(army, &l1min, &l1max, &l1typ, &hemin, &hemax, &memin, &memax);
	if (gl == SEMEAI) {
		*alprob = winprob - 50;
		*alprob += armyrn_pot[army] * 2;
		*alprob += armyeyepotential[army];
		*alprob += armypthreat[army] / 2;
		*alprob += l1typ - 10;
		if (*alprob > 45)
			*alprob = 45;
		if (*alprob < -45)
			*alprob = -45;
		if (*alprob > *semprob)
			*semprob = *alprob;
		return SEMEAI;  /* found one semeai */
	}

	if (found && toteyes + armyeyepotential[army] >= 12) {  /* for groups that have one eye, but are tac threatened, or have threat to make second eye */
		*alprob = winprob - 50;
		*alprob += armyrn_pot[army] * 2;
		*alprob += armyeyepotential[army] > 16 ? 16 : armyeyepotential[army];
		*alprob += armypthreat[army] / 2;
		*alprob += l1typ - 10;
		if (*alprob > 45)
			*alprob = 45;
		if (*alprob < -45)
			*alprob = -45;
		if (*alprob > *semprob)
			*semprob = *alprob;
		if (toteyes >= 16 && armyrn_pot[army] > 2 && winprob > 40)
			return SEMEAI;
		if (armyrn_pot[army] > 3 && l1max > 15)
			return FALSE;	/* can run away */
		return WEAK_SEMEAI;
	}
	return FALSE;
}

#ifdef G2DEBUGOUTPUT
void outdeadshape(sqr_t s)
{
	army_t a;
	eye_t rn;
	int ptr;  /* mvs ptr, not list ptr */
	int eyes, maxeyes;
	char buf[80];

	a = S_ARMY(s);
	if (armysize[a] >= 3 && armysize[a] <= 6 && link[armygroups[a]] == EOL) {
		rn = (eye_t)gtflist(&eyefreelist);
		if (rn == G2ERROR) {
			outerror("Out of eye records");
		}
		else {
			for (ptr = grpieces[list[armygroups[a]]]; ptr != -1; ptr = mvnext[ptr])
				addlist(mvs[ptr],&eyeptr[rn]);
			deadshape((group_t)list[armygroups[a]],rn,armylbp[a],(listval_t)(NUMGROUPS+NUMCONNS+rn), eyetaclibs[playlevel]);
			sprintf(buf,"Deadshape returns min-%d, eyes-%d, pot-%d",eyemin[rn], eyeval[rn], eyepot[rn]);
			outerr(buf);
			delete_eye(rn);
		}
	}
	eyes = eyesifcapture(a,NOARMY, &maxeyes);
	sprintf(buf,"eyesifcapture is %d, maxeyes %d, can make nakade %d", eyes, maxeyes, canmakenakade(a));
	outerr(buf);	
}
#endif

/* how much eye space can army get when a is captured
 * by false eyes of army becoming true eyes
 */

static int falsetotrue(army_t army, army_t a)
{
	list_t ptr;
	int i, ldtmp, eyes = 0;
	sqr_t s, sn;
	int gc = 0;
	for (ptr = armyeyerecs[army]; ptr != EOL; ptr = link[ptr]) {
		if (eyepot[list[ptr]] != 0)continue;
		if (eyetype[list[ptr]] != ONEPOINTEYE)continue;
		s = list[eyeptr[list[ptr]]];
		if (edge[s] == 1)
			gc = 1;  /* good stone count */
		else
			gc = 0;
		i = fdir[s];
		for (ldtmp = ldiag[i]; i < ldtmp; ++i) {
			sn = s+diags[i];
			if (S_ARMY(sn) == army || S_ARMY(sn) == a)gc++;
		}
		if (gc >= 3)
			eyes += 8;
	}
	return eyes;
}

/* 
 * return minimum number of eyes army gets from capturing army a
 * return maxeyes as maximum number of eyes from capture
 * this is eyes from eye potential or shape, or connections
 */

int eyesifcapture(army_t a, army_t army, int *maxeyes)
{
	list_t ptr, totlibs = EOL;
	int mptr;  /* mvs ptr, not list ptr */
	eye_t rn;
	int eyes = 0,size;
	army_t a2;
	*maxeyes = 0;
	size = armysize[a] + armylibs[a];
	cpylist(armylbp[a], &totlibs);
	if (A_THREATENED(a)) {
		/* 4/01 use the armyth_pot value, if it exists, since it is more accurate measure of connection eyes */
		for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
			if ( (pots[list[ptr]].pot_type == THREAT || pots[list[ptr]].pot_type == POTTHREAT) &&
				pots[list[ptr]].pot_where == a) {
					eyes = pots[list[ptr]].pot_val;
					*maxeyes = pots[list[ptr]].pot_max;
					break;
			}
		}
		eyes += eyepot[eyerec[mvs[grpieces[list[armygroups[a]]]]]];
	}
	else if (A_ALIVE(a) == DEAD) {
		rn = eyerec[mvs[grpieces[list[armygroups[a]]]]];
		*maxeyes = eyepot[rn] - eyeval[rn];  /* already counted in eyespace of a */
	}
	else {	
		if (size > 7)eyes = 16;	/* can't make a nakade */
		else if (size > 2 && !A_THREATENED(a))  /* if threatened, theyes already counts this */
			eyes = 8;
		if (armysize[a] >= 3 && armysize[a] <= 6 && 
			link[armygroups[a]] == EOL) {
				rn = (eye_t)gtflist(&eyefreelist);
				if (rn == G2ERROR) {
#ifdef CHECK				
					outerror("Out of eye records");
#endif				
				}
				else {
					for (mptr = grpieces[list[armygroups[a]]]; mptr != -1; mptr = mvnext[mptr])
						addlist(mvs[mptr],&eyeptr[rn]);
					deadshape((group_t)list[armygroups[a]],rn,armylbp[a],(listval_t)(NUMGROUPS+NUMCONNS+rn), eyetaclibs[playlevel]);
					eyes = eyeval[rn];
					*maxeyes = eyepot[rn]-eyeval[rn];
					delete_eye(rn);
				}
		}
		else if (size <= 7) {
			for (ptr = armylbp[a]; ptr != EOL; ptr = link[ptr]) {
				if (!S_NEUTRAL(list[ptr])) {
					if (ltr1[list[ptr]])
						size += ltr1[list[ptr]];
					else
						size++;               
				}
				else 
					size += mrglist(nblbp[list[ptr]], &totlibs);
			}
			if (size > 6)*maxeyes += 8;
		}
	}  /* end of if ! dead */
	if (armynbp[a] == EOL)
		getarmynbp(a);

	if (army != NOARMY && !A_THREATENED(a)) {
		for (ptr = armynbp[a]; ptr != EOL; ptr = link[ptr]) {
			a2 = (army_t)list[ptr];
			if (a2 == army)
				continue;
			eyes += geteyespace(army,a2) - armyeyespace[army];
			eyes += armyrn_pot[a2];
		}
	}
	eyes += falsetotrue(army, a);  /* 2/02 into eyes to make badseki3 work */
	*maxeyes += eyes;
	killist(&totlibs);
	return eyes;
}


/* semeaialive finds all the groups that are alive in semeai
 * 7 alive because wins semeai
 */

static void semeaialive(army_t army) 
{
	group_t enemy;
	int prob;
	if (armywk_pot[army] && !A_THREATENED(army) &&
		A_ALIVE(army) > SEKI && win_semeai(army, &enemy)) {
			prob = winsemprob(army, G_ARMY(enemy), 0, 0);
			if (prob < 85)
				prob = 85;
			newalive(army, WINS_SEMEAI, prob - 50, prob - 50);
	}
}


/* 4/01, return the maximum number of eyes the army gets if it moves first */
int getmaxeyes(army_t army)
{
	int max =	/* 7/01 bestpot already gets cretid for max eyespace, so use eyspace here rather than eyespacemax */
		armyeyespace[army] + armybestshared[army] + armybestpot[army] + armymaxsemeyes[army];  /* 3/02 +armypthreat[army]; */
	if (armybestpot[army] >= 12 && armysecond[army] > 4)
		max += armysecond[army] - 4;	/* best move is 8 in sente so get second also */
	else if (armybestpot[army] >= 14)
		max += 2;	/* 5/02 - bestpot is a range, so count the full eye */
	if (armyrest[army] >= 8)  /* 7/01 if rest less than 8, just a sente threat for eye */
		max += armyrest[army];
	if (armykopot[army] > armybestpot[army])
		max = max - armybestpot[army] + armykopot[army];  /* can choose to take ko instead of best */
	return max;
}

static void donewbest(army_t army, list_t *armylist)
{
	sqr_t bestmove, secondmove, bestkillmove, bestkillmove2, bestmove2;
	int best,bestmax,secondbest,rest,pthreat,numrest,kopot,weakpot,bestval2, bestshared, secondkocolor;
	best = bestpot(army,&secondbest,&bestval2,&bestshared,&bestmove,&bestmove2,&bestkillmove,&bestkillmove2,&secondmove,&rest,&pthreat,&kopot,&weakpot,&numrest, &bestmax);
	armybestpot[army] = best;
	armybestpot2[army] = bestval2;
	armysecond[army] = secondbest;
	armybestshared[army] = bestshared;
	armybestmove[army] = bestmove;
	armybestmove2[army] = bestmove2;
	armysecondmove[army] = secondmove;
	armykillmove[army] = bestkillmove;
	armykillmove2[army] = bestkillmove2;
	armyrest[army] = rest;
	armypthreat[army] = pthreat;
	armynumrest[army] = numrest;
	armykopot[army] = kopot;
	armyweakpot[army] = weakpot;
	armybestmax[army] = bestmax;
	iskopoint(secondmove,&secondkocolor);
	if (A_ALIVE(army) > MIAI && !A_THREATENED(army) && secondkocolor != 1-A_COLOR(army) &&
		(armyeyes[army] + secondbest >= 20 ||  /* second chance for miai */
	   	 armyeyes[army] + secondbest >= 16 && (rest >= 8 || !(armyrn_pot[army] < 3 && armycnrn_pot[army] < 3)) ||
		 armyeyes[army] == 8 && secondbest >= 8 || /* has one, can get another */
		 rest >= 16 && armyeyes[army] + secondbest >= 12 && armyeyes[army] + best >= 16 ||
		 armyeyes[army] + secondbest >= 8 && rest >= 24 && armyeyes[army] + best >= 12 || 
		 secondbest >= 12 && secondbest + rest >= 24)) {
		newalive(army, MIAI, pfac[MIAI], -50);
		dellist(army, armylist);
	}
}

/* look at armies that are not alive or dead and figure out how
 * weak they are.  Look at ability to run away.
 * can set alive to MIAI if can make two eyes and also run a little
 * It can give aliveness values from 8-22
 * if newbest, must redo bestpot()
 */

static void weakalive(army_t army)
{  
	group_t gl;
	list_t ptr;
	sqr_t bestmove, secondmove, bestkillmove, bestkillmove2, bestmove2;
	int libs, best, bestmax, secondbest, surrounded, rest, pthreat, kopot, weakpot;
	int bestval2, bestshared;
	int bestkocolor, kosquarecolor = NOCOLOR;
	int best2kocolor, secondkocolor;
	int c;  /* color of army */ 
	int alprob = 1000;  /* new alive probability -50 to 50 */
	int runpot, gltmp, semprob = -50;
	int semres = FALSE;	/* semeai result */
	int min, max;

	runpot = armyrn_pot[army] + armycnrn_pot[army] / 2;
	if (runpot > 15)
		runpot = 15;
	if (gralive[list[armygroups[army]]] <= WINS_SEMEAI)
		return;
	surrounded = armyrn_pot[army] < 3 && armycnrn_pot[army] < 3; /* includes connect or kill to run */
	libs = armylibs[army];
	if (libs > 5)
		libs = 5;
	c = A_COLOR(army);
	best =	 armybestpot[army];
	bestval2 = armybestpot2[army];
	secondbest = armysecond[army];
	bestshared = armybestshared[army];
	bestmove = armybestmove[army];
	bestmove2 = armybestmove2[army];
	secondmove = armysecondmove[army];
	bestkillmove = armykillmove[army];
	bestkillmove2 = armykillmove2[army];
	rest = armyrest[army];
	pthreat = armypthreat[army];
	kopot = armykopot[army];
	weakpot = armyweakpot[army];
	bestmax = armybestmax[army];
	iskopoint(bestmove, &bestkocolor);  /* is there a ko */ 
	iskopoint(bestmove2, &best2kocolor);
	iskopoint(secondmove, &secondkocolor);
	if (kosquare != NOSQUARE)
		iskopoint(kosquare, &kosquarecolor);

/*	if (armywk_pot[army]) 5/01 can be weak semeai against seki or miai group */
	semres = uns_semeai(army, &alprob, &semprob, &armyminsemeyes[army], &armymaxsemeyes[army]);	
	if (A_NUMLIBS(army) > 3 && 
		!A_THREATENED(army) && secondkocolor != 1-c &&  /* 7/01 fixed bug != instead of == */
		armyrn_pot[army] >= LIMP_RUN &&
		(armyeyes[army] + secondbest >= 12 /* && armyeyes[army] >= 8 7/01 fuseki prob 4 */ ||
		 secondbest >= 4 && armyeyes[army] + secondbest + armyrn_pot[army] + rest/2 >= 20)) /* 10/96 */
		gl = RUN_OR_LIVE;  /* weak miai for second eye plus can run */
	else if (getmaxeyes(army) + armypthreat[army] >= 16 || 
		armyrn_pot[army] >= 2*easyrun[A_COLOR(army)] && 
		armyeyespace[army] + best >= 8) { 
		/* can live in one move */
		if (A_THREATENED(army)) {	 /* killable in one move! */
			if (bestkocolor == c && armyeyespace[army] + best >= 16)
				gl = STRONG_KO;
			else if (kosquare != NOSQUARE &&  /* enemy can't capture */
				grcapmove[list[armygroups[army]]] == kosquare) {
				if (lnbf[kosquare][c] != 0)
					gl = STRONG_KO;
				else
					gl = WEAK_KO;
			}
			else if (best2kocolor == c && armyeyespace[army]+bestval2 >= 16)
				gl = STRONG_KO; 
			else if (bestkocolor == 1-c)
				gl = WEAK_KO;   /* capture ko to live */
			else if (kopot+armyeyespace[army] >= 16 && kosquarecolor == c)
				gl = STRONG_KO;
			else if (kopot && kosquarecolor == 1-c)
				gl = WEAK_KO;
			else
				gl = UNSETTLED_THREATENED;
		}
		else {  /* not obviously killed in one move */ 
			if (A_NUMLIBS(army) > 3 &&  /* 4/99 must have one eye at least, since running only good for one more */
				(armyrn_pot[army] >= LIMP_RUN  &&  /* 11/02 LIMP_RUN since I will move first and get more running ability */ 
				 (armyeyes[army] + best >= 16 || armyeyes[army] + secondbest >= 8) ||
					armyrn_pot[army] > LIMP_RUN+2 && armyeyes[army] + secondbest >= 8 &&
					armyeyes[army] + best + armyrest[army] >= 16)) {
	            gl = RUN_OR_LIVE;  /* miai to live or run away */
				alprob = pfac[gl];
			}
			else if (surrounded) {
				if (bestkocolor == c && armyeyespace[army] + best >= 16)
					gl = STRONG_KO; /* fill ko to live */
				else if (best2kocolor == c && armyeyespace[army] + bestval2 >= 16)
					gl = STRONG_KO; 
				else if (bestkocolor == 1-c)
					gl = WEAK_KO;   /* capture ko to live */
				else if (kopot && kosquarecolor == c)
					gl = STRONG_KO;
				else if (kopot && kosquarecolor == 1-c)
					gl = WEAK_KO;
				else 
					gl = UNSETTLED_DEAD;  /* only if surrounded */
				alprob = pfac[gl];
			}
			else {
				if (armyrn_pot[army] >= easyrun[A_COLOR(army)] &&
					(armysize[army] > 1 || armylibs[army] > 3))
					gl = RUN_NO_EYES;
				else
					gl = UNSETTLED_LIMP;
				alprob = pfac8[armyrn_pot[army] + (armycnrn_pot[army]+1)/2];
			}
			/* extra factors for possible life after killing move */
			alprob += 5*armyrn_pot[army];
			alprob += (secondbest-4)*2;
			if (armyeyespace[army] >= 8 && armyeyespace[army] + best > 20)
				alprob += (armyeyespace[army] + best - 20) * 2;
			if (armyeyespacemax[army] + secondbest >= 16)  /* may actually be alive */
				alprob += 30;
			for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
				if (pots[list[ptr]].pot_max > 8)  /* forcing to make eyes */
					alprob += 2;
			}
			if (alprob > 45)
				alprob = 45;
			if (gl == STRONG_KO) {
				alprob += rest*2;  /* for local ko threats */
				if (alprob > 45)
					alprob = 45;
			}
		}
	}
	else if (armyrn_pot[army] >= easyrun[A_COLOR(army)] &&
		(armysize[army] > 1 || armylibs[army] > 3)) {  /* one stone group with
														less than 4 libs can't be
														settled */
 		if (A_THREATENED(army))
			gl = UNSETTLED_THREATENED;
		else
			gl = RUN_NO_EYES;
	}
	else if (semres &&
		armyrn_pot[army]+armycnrn_pot[army] < LIMP_RUN) {
		gl = semres;
		/* extra factors for possible life after killing move */
		alprob += 5*armyrn_pot[army];
		alprob += (secondbest-4)*2;
		if (armyeyespace[army] >= 8 && armyeyespace[army] + best > 20)
			alprob += (armyeyespace[army] + best - 20) * 2;
		if (armyeyespacemax[army] + secondbest >= 16)  /* may actually be alive */
			alprob += 30;
		if (alprob > 45)
			alprob = 45;
	}

	else if (!surrounded) {
		if (armyeyespace[army] + armyeyepotential[army] + armypthreat[army] >= 4 && 
			armyrn_pot[army] >= UNSET_RUN ||
			armywk_pot[army] && armyrn_pot[army] >= UNSET_RUN) {
			gl = RUNNING_FIGHT;
			alprob = pfac11[runpot];
		}
		else if (armyrn_pot[army] >= UNSET_RUN ||
			armyrn_pot[army] + armycnrn_pot[army] >= LIMP_RUN) {
			gl = UNSETTLED_RUN;
			alprob = pfac16[runpot];
		}
		else {
			gl = WEAK_LIMP;
			alprob = pfac16[runpot];
		}
		/* potential for second eye */
		alprob += (armyeyespace[army] + armyeyepotential[army] - 8) * 2;
		for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
			if (pots[list[ptr]].pot_max > 8)  /* forcing to make eyes */
				alprob += 2;
		}
		if (alprob > 45)
			alprob = 45;
	}
	else if (armyeyespace[army] + best + kopot >= 16) {
		gl = WEAK_KO;
	}
	else if (armyrn_pot[army] > 1 /* && armyeyepotential[army] 10/96 */ ||
		armycnrn_pot[army] > 1 ||
		armycnrn_pot[army] > 3 ||
		armyeyespacemax[army] + armyeyepotential[army] + armypthreat[army] + kopot >= 10 ||
		armyeyespacemax[army] + armybestpot[army] >= 10 || /* bestpot might be more than eyepotential since eyepotential not updated with recent bestpot */ 
		secondbest >= 8 || /* armyrn_pot[army] >= 3 || */ 
		bestval2 >= 8 ||
		weakpot >= 16 || 
		armybestpot[army] + kopot > 8) {
		gl = WEAK_POTENTIAL;
		alprob = -50 + 
			armyeyespacemax[army]/4 +
			armybestpot[army]/4 +
			armysecond[army]/4 +
			armyrn_pot[army]*2 + 
			armycnrn_pot[army]*2 + 
			armywk_pot[army] +
			armybestshared[army]*2;
		if (armyeyespacemax[army] + armybestpot[army] > 8)
			alprob += (armyeyespace[army] + armybestpot[army] - 8) * 2;
		for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
			if (pots[list[ptr]].pot_max > 8)  /* forcing to make eyes */
				alprob += 2;
		}
		if (alprob > 35)
			alprob = 35;
	}
	else if (armywk_pot[army] && armyeyespacemax[army] + armyeyepotential[army] >= 4 ||
		armyeyepotential[army] >= 8 || armyrn_pot[army]) {
		gl = WEAK;
		alprob = -50 + 
			armyeyespacemax[army]/4 +
			armybestpot[army]/4 +
			armysecond[army]/4 +
			armyrn_pot[army]*2 + 
			armycnrn_pot[army]*2 +
			armywk_pot[army] +
			armybestshared[army]*2;
		if (armyeyespacemax[army] + armybestpot[army] > 8)
			alprob += (armyeyespace[army] + armybestpot[army] - 8) * 2;
		for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr]) {
			if (pots[list[ptr]].pot_max > 8)  /* forcing to make eyes */
				alprob += 2;
		}
		if (alprob > 35)
			alprob = 35;
	}
	else if (armywk_pot[army] ||
		winsemnbr(army)) {
		gl = LOSE_SEMEAI;
	}
	else if (armyeyespacemax[army] >= 8 ||
		armyeyepotential[army] >= 8)
		gl = LOOKS_DEAD;
	else 
		gl = MUST_BE_DEAD;
	if (gl == UNSETTLED_DEAD &&  !A_THREATENED(army) && 
		armywk_pot[army] && armyrn_pot[army] < LIMP_RUN) { 
		gltmp = uns_semeai(army, &alprob, &semprob, &min, &max);
		if (gltmp == STRONG_SEMEAI) {
			gl = STRONG_SEMEAI;   /* promote to STRONG_SEMEAI */
		}
	}
	if (alprob == 1000)alprob = pfac[gl];  /* default */
	if (alprob > 50)
		alprob = 50;
	newalive(army, gl, alprob, semprob);
}

static void promoteweak(army_t army) {
	list_t ptr;
	group_t g2;
	if (armynbp[army] == EOL)
		getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
		if ( (A_ALIVE(list[ptr]) == MIAI || A_ALIVE(list[ptr]) == BARELY_ALIVE) &&
			armyrn_pot[list[ptr]] <= 2 && armyrest[list[ptr]] + armyeyes[list[ptr]] <= 12) {
			g2 = (group_t)list[armygroups[army]];
			newalive(army, WEAK_POTENTIAL, groldalprob[g2], grsemval[g2]);
			return;
		}
	}
}

static int winsemnbr(army_t army) {
	list_t ptr;
	if (armynbp[army] == EOL)getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr])
		if (gralive[list[armygroups[list[ptr]]]] == WINS_SEMEAI)return(TRUE);
	return(FALSE);
	}
	
int canextend(army_t army) {
	list_t ptr;
	for (ptr = armypot[army]; ptr != EOL; ptr = link[ptr])
		if (pots[list[ptr]].pot_type == EXTEND)return(TRUE);
	return(FALSE);
	}

/* Amount to add to life probability from eyes for running,
 * assuming he moves first, depending on the number of eyes it already has.
 */
 
int pnoeye[21] = {  /* 7/01 reduced values a lot to encourage making one eye for a base */
-50,-49,-45,-35, -30, -20, -10, -5, 0,0,5,15,25,30,35,40,45,45,48,49,50,
};
 
int poneeye[21] = {
-25,-20,-15, -5,10,15,25,35, 40, 45, 50, 55, 60, 64, 68,70,72,75,75,75,75,
};

int ptwoeye[21] = {
 0, 0, 0,  0,10,20,25,30, 35, 37, 39, 41, 42, 43, 44,45,50,50,50,50,50,
};

/* alive prob for number of eyes I have, assuming he moves first */
int eyeprob[21] = {/*        8               12            16          */
  0,  0,  0,  0, 0, 0, 0, 0,-25,-25,-20,-20, 20, 25, 30,40,50,50,55,55,75,
};


/* what is alive probability (-50 to 50) for given number of eyes, runs, with opponent to move */

static int getprobforlife(int eyes, int runs) {
	int prob;

	if (eyes > 20)
		eyes = 20;
	prob = eyeprob[eyes];
	if (runs > 20)
		runs = 16;
	if (eyes >= 16)
		prob += ptwoeye[runs];
	else if (eyes >= 8)
		prob += poneeye[runs];
	else
		prob += pnoeye[runs];
	if (prob > 50)
		prob = 50;
	if (prob < -50)
		prob = -50;
	return prob;
}

extern int semalive[];

/* calculate and set gralprob as the average of gralvals */
/* alprob is suggested value from the group strength evaluator */
/* semprob is the semeai winning probability (-50 to 50 ) */

/* expected new run value after run here */
extern int newrunval[NUMRUN];

void getalprob(army_t army, int alive, int alprob, int semprob, int *alval)
{
	int mineyes, maxeyes, minruns, maxruns, secondrun = 0;
	int i,opt1, opt2, opt3, meval,heval;
	int bestrun, bval = 0;
	int min, typ, max, hemin, hemax, memin, memax;
	int totrunval = 0;

	alval[0] = alval[1] = alprob;

	/* what if I move first */
	maxeyes = getmaxeyes(army);
	maxruns = armyrn_pot[army];
	if (armycnrn_pot[army] > 1 || maxruns == 0)
		maxruns += armycnrn_pot[army];  /* value 1 means any number of bad places to run */

	/* if he moves first */
	if (A_THREATENED(army)) {	/* 4/02 can't run if he moves first and I'm threatened */
		mineyes = armyeyespace[army];
		minruns = 0;
	}
	else {
		mineyes = armyeyespace[army] + armysecond[army] + armybestshared[army] + armyminsemeyes[army];
		if (mineyes >= 12)
			mineyes += armyrest[army];	/* second best is sente */
		minruns = armyrn_pot[army] + armycnrn_pot[army];
	}
	bestrun = armycnrn_pot[army];
	for (i = 0; i < NUMRUN; ++i)
		if (armyrun[army][i] != EOL) {
			totrunval += runval[i];
			if (runval[i] - minrunval[i] > bestrun) {
				bestrun = runval[i] - minrunval[i];
			}
			if (newrunval[i] > bval) {
				if (link[armyrun[army][i]] != EOL)
					secondrun = newrunval[i];
				else
					secondrun = bval;
				bval = newrunval[i];
			}
			else if (newrunval[i] > secondrun)
				secondrun = newrunval[i];
		}
	maxruns += bval;  /* add extra for best running point */
	if (!A_THREATENED(army)) {
		if (bestrun > armycnrn_pot[army]) {
			if (totrunval > armyrn_pot[army])
				minruns += totrunval - armyrn_pot[army];
			minruns -= bestrun;	/* take off best run */
			}
		else
			minruns -= armycnrn_pot[army];
	}
	/* if I move first, I get max of maxeyes + runs or mineyes + maxruns */
	/* if he moves first, I get min of mineyes + maxruns or maxeyes + minruns */
	opt1 = getprobforlife(maxeyes, armyrn_pot[army]);
	opt2 = getprobforlife(mineyes, maxruns);
	meval = opt1 > opt2 ? opt1 : opt2;  /* maximum */
	if (meval < 15 && maxeyes + armypthreat[army] > 15)
		meval = 15;  /* 3/02 if pthreat is a ko, get to 2/3 with  my move */
	opt1 = getprobforlife(maxeyes, minruns);
	opt2 = getprobforlife(mineyes, armyrn_pot[army]+bval);  /* can't use maxruns since same move may remove bestpot and cnrn */
	opt3 = getprobforlife(armyeyespace[army], minruns+secondrun);  /* I can choose opt1 or opt3 if he attacks running */
	opt1 = opt1 > opt3 ? opt1 : opt3;   /* maximum */
	heval = opt1 < opt2 ? opt1 : opt2;  /* minimum */
	semeailibs(army, &min, &max, &typ, &hemin, &hemax, &memin, &memax);
	if (min > 30)
		min = 30;
	if (typ > 30)
		typ = 30;
	if (semprob > -40) {	/* 3/01 only if have some semeai potential */
		if (typ > 5)
			meval += typ-5;
		heval += min-10;	/* extra value for lots of liberties */
	}
	if (armyeyespacemax[army] >= 16) {	/* probably lives - eval high.  reading will find killing move if exists */
		if (meval < 45)
			meval = 45;
		if (heval < -30)  /* 4/00 this must be small or static eval way off */
			heval = -30;
	}

	if (meval > 50)
		meval = 50;	/* otherwise next line can reduce meval */
	meval += (50 - meval) * (50 + semprob) / 100;	/* combine probabilities (me failiing * prob of sem success ) */
	if (semprob > -50)  /* less extra value when he moves first */
		heval += (50 - heval) * (50 + semprob) / 100;  /* combine probabilities */
	if (A_THREATENED(army) == 2)
		heval = -50;		/* 8/02 can be unconditionally captured */
	if (A_THREATENED(army) == 1 && heval > -17)
		heval = -17;	/* 8/02 can be captured in a ko */
	if (meval > 50)
		meval = 50;
	if (meval < -50)
		meval = -50;
	if (heval > 50)
		heval = 50;
	if (heval < -50)
		heval = -50;

	if (alive == VERY_ALIVE || alive == SEKI || alive == WINS_SEMEAI) {
		alval[0] = alval[1] = 50;
	}
	else if (alive <= ALIVE) {
		alval[0] = 50;	/* If I am already alive, I'm certainly alive if I move first */
		alval[1] = heval;
	}
	else if (alive >= WEAK_POTENTIAL) {
		alval[0] = meval;
		alval[1] = -50;
	}
	else if (alive == WEAK_SEMEAI) {
		alval[1] = -50;
		alval[0] = semprob + semprob + 50;
		if (meval > alval[0])
			alval[0] = meval;
		if (alval[0] > 0)
			alval[0] = 0;
	}
	else if (alive == SEMEAI) {
		alval[0] = semprob + 40;
		if (meval > alval[0])
			alval[0] = meval;
		if (alval[0] > 50)
			alval[0] = 50;
		alval[1] = semprob - 40;
		if (heval > alval[1])
			alval[1] = heval;
		if (alval[1] < -50)
			alval[1] = -50;
	}
	else if (alive == STRONG_SEMEAI) {
		alval[0] = meval;
		alval[1] = semprob;
		if (heval > alval[1])
			alval[1] = heval;
		if (alval[1] > alval[0])
			alval[0] = alval[0];
	}
	else if (!semalive[alive]) {
		alval[0] = meval;
#ifdef NEVER	/* 4/02 can't do this here since group could be strong due to neighboring dead group */
		if (A_THREATENED(army) == 2)
			alval[1] = -50;	/* can be captured in one move */
		else if (A_THREATENED(army) == 1 && heval > 0)
			alval[1] = 0;	/* group is unsettled */
		else
#endif
			alval[1] = heval;
		if (alive == STRONG_KO && alval[1] < -17)
			alval[1] = -17;
		if (alive == WEAK_KO && alval[0] > 17)
			alval[0] = 17;
	}
	else 
		assert(TRUE);	/* should ahve all cases covered */
#ifdef NEVER
	// destabilizes the search.  let search find the life/death result instead

	if (foughtalready(biggestarmygroup(armygroups[army]), TRYTOLIVE, A_COLOR(army), &mt)) {	/* use results of reading */
		res = treeresval(mt);
		prob = treealprob(mt, alval[0], TRUE);	/* prob is -50 to 50 probability of success */
		if (res >= V_WINLIKELY && prob > alval[0]) /* he can live */
			alval[0] = prob;
		else if (res <= V_LOSELIKELY && prob < alval[0])
			alval[0] = prob;
		}

	if (foughtalready(biggestarmygroup(armygroups[army]), TRYTOKILL, 1-A_COLOR(army), &mt)) {	/* use results of reading */
		res = treeresval(mt);
		prob = treealprob(mt, alval[1], FALSE);	/* prob is -50 to 50 probability of living (negative prob of killing) */
		if (res >= V_WINLIKELY && prob < alval[1]) /* he can be killed */
			alval[1] = prob;
		else if (res <= V_LOSELIKELY && prob > alval[1])
			alval[1] = prob;
		}
#endif
	if (alval[0] < alval[1])	/* correct possible inversion */
		alval[0] = alval[1] = (alval[0] + alval[1]) / 2;
}


/* give army army the new aliveness value, alive, and probablility to live
 * alprob. (50 to -50)
 * semprob is the probability of winning a semeai to make life(-50 if no semeai)
 */

static void newalive(army_t army, int alive, int alprob, int semprob) {
	list_t ptr;
	group_t g2;
	int alval[2], newalprob;
#ifdef CHECK
	char buf[90];
	if (alive < HAS_TWO_EYES || alive > DEAD)
		{
		sprintf(buf,"Bad alive in newalive %d\n",alive);
		outerror(buf);
		}
#endif

	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g2 = (group_t)list[ptr];
#ifdef CHECK
		if (gralive[g2] == DEAD || !grlv[g2]) {
			sprintf(buf,"newalive: group %d alive %d grlv %d in army %d\n",g2,gralive[g2],grlv[g2],army);
			outerror(buf);
			turnoffcplay();
			continue;
			}
#endif
		gralive[g2] &= 31;
		gralive[g2] = alive;
		}

	getalprob(army, alive, alprob, semprob, alval);
	newalprob = (alval[0] + alval[1])/2;
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g2 = (group_t)list[ptr];
		pscr = pscr + (newalprob - gralprob[g2]) *
			grsize[g2] * cfac[grcolor[g2]];
		gralprob[g2] = newalprob;
		groldalprob[g2] = alprob;
		gralval[g2][0] = alval[0];
		gralval[g2][1] = alval[1];
		grsemval[g2] = semprob;
		}
	}
