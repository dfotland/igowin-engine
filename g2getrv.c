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

#include "g2hd.h"
#include "g2rldef.h"
#include "g2getr.pr"
#include "g2tree.h"
#include "g2fcache.h"

#define MIN(x, y) ((x) > (y) ? (y) : (x))

extern int obaval;  /* value of taking a big gote point */
extern int atkv[NUMALIVE],kval[NUMALIVE],ctval[NUMALIVE],connval[NUMALIVE],senteattack[NUMALIVE],defatkv[NUMALIVE];

/* TRUE if there is another possible connection between a1 and a2
 * that isn't capturing army
 */
 
int otherconn(army_t a1, army_t a2, army_t army) {
	list_t ptr, ptr2;
	
	if (armynbp[a1] == EOL)getarmynbp(a1);
	for (ptr = armynbp[a1]; ptr != EOL; ptr = link[ptr])
		if (list[ptr] != army && A_THREATENED(list[ptr])) {
			if (armynbp[list[ptr]] == EOL)getarmynbp((army_t)list[ptr]);
			if (inlist(a2,&armynbp[list[ptr]]))
				return(TRUE);  /* can capture other group */
			}
	if (comlist(armylbp[a1], armylbp[a2]))
		return TRUE;  /* the armies have a common liberty so they can connect */
	for (ptr = armygroups[a1]; ptr != EOL; ptr = link[ptr])
		for (ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
			if (cnprot[list[ptr2]] == CAN_CONNECT && 
				(inlist(cngr1[list[ptr2]], &armygroups[a2]) ||
				 inlist(cngr2[list[ptr2]], &armygroups[a2])))
				 return TRUE;
	return FALSE;
	}

/* extra value for capturing stones when ahead */    
    
int safecaptval(army_t army, int passval) {
	int val;
	if (ahead <= 2)
		return 0;  /* no safe plays unless ahead */
	val = armylibs[army]*15 + armysize[army]*30;
	if (msptr > 0 && inlist(board[mvs[msptr-1]],&armygroups[army]))  /* last move */
		val += 250;
	else if (val > 100)val = 100+val/4;
	if (armynbp[army] == EOL)
		getarmynbp(army);
	if (armynbp[army] != EOL && link[armynbp[army]] != EOL)
		val += 100;  /* cutting stones */
	passval = g2abs(passval);
	if (passval < 50*50)
		val = (int)(val * (float)passval/(50*50));
	return val;
	}

/* does group g have two eyes not counting dead stones */	            
	            
static int has2eyes(group_t g) {
	list_t ptr,eyes = 0;
	for (ptr = armyeyerecs[grarmy[g]]; ptr != EOL; ptr = link[ptr])
		if (eyetype[list[ptr]] != DEADEYE)
			eyes += eyeval[list[ptr]];
	return(eyes >= 16);
	}


/* return TRUE if group g should be safely captured if ahead */                
                
int safecaptarmy(army_t army, int color, int handicap) {
	list_t ptr;
	group_t g,g2;
	
	if (ahead <= 2)
		return FALSE;
	if (A_COLOR(army) != 1-color)
		return FALSE;  /* only try to capture enemy groups */
	for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[ptr];
		if (msptr > 0 && mvs[msptr-1] != PASS && 
	   	((sqr_t)msptr > (boardsquare/8) || handicap >= (boardsquare/60) && color == 0) &&
			g == board[mvs[msptr-1]]) {
			if (G_ALIVE(g) >= WEAK || G_THREATENED(g))
				return TRUE; /* last move in weak group */
			}
                /* capture DEAD cutting stones if you don't have an eye */
		if (G_ALIVE(g) == DEAD &&
		   grlibs[g] > 1)
			for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
				g2 = (group_t)list[ptr];
				if (G_ALIVE(g2) <= BARELY_ALIVE &&
				    !has2eyes(g2) || G_ALIVE(g2) == BARELY_ALIVE || 
				   G_ALIVE(g2) == MIAI || G_ALIVE(g2) == WINS_SEMEAI)
					return TRUE;
				}
		if (G_ALIVE(g) == DEAD &&
			grsize[g] > 7 && grlibs[g] > 2)
			return TRUE;
		
		if (G_ALIVE(g) >= WEAK_POTENTIAL && 
			G_ALIVE(g) < MUST_BE_DEAD &&
		   armylibs[grarmy[g]] > 3)
		   	return TRUE;
		if (G_THREATENED(g)) {
			if (armysize[army] > 2)
				return TRUE;
			if (armynbp[army] == EOL)
				getarmynbp(army);
			if (armynbp[army] != EOL && link[armynbp[army]] != EOL)  /* cutting stones */
				return TRUE;  /* cutting stones */
			}
		}
	return FALSE;
	}

   
/* value of capturing or saving cutting stones, (army)
 * add up values of all cuts involving this stone.
 * don't count cutting dead or threatened groups (since we should just capture them)
 * add bonus if this group is part of threatened connection
 */

int cut_stones_val(army_t army) {
	group_t g1;
	sqr_t s,sn;
	int cutval = 0,i,ldtmp,thcut,cv, iscut = FALSE;
	list_t ptr, ptr2, armylist = EOL, ptr3;
	int erun;
	
	if (armynbp[army] == EOL)getarmynbp(army);
	for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr])
			/* no benefit in cutting dead or threatened stones */
		if (A_ALIVE(list[ptr]) != DEAD  && 
		  (!A_THREATENED(list[ptr]) || A_THREATENED(army) ) )
			addlist(list[ptr],&armylist);
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		if (!S_NEUTRAL(s))continue;
		i = fdir[s];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = s + nbr[i];  /* don't count cuts of dead ot threatened groups */
			if (grcolor[board[sn]] == 1-A_COLOR(army) && 
				G_ALIVE(board[sn]) != DEAD && !G_THREATENED(board[sn]))
				addlist(grarmy[board[sn]],&armylist);
			}
		}
	cutval = 0;
	for (ptr = armylist; ptr != EOL; ptr = link[ptr])
		for (ptr2 = link[ptr]; ptr2 != EOL; ptr2 = link[ptr2]) {
			iscut = TRUE;
			cv = cut_val((army_t)list[ptr], (army_t)list[ptr2]);
			if (cv > 200 && otherconn((army_t)list[ptr], (army_t)list[ptr2], army))
				cv = 200;
			if (comlist(armylibs[list[ptr]], armylibs[list[ptr2]]))
				cv /= 2;	/* groups can still connect */
			cutval += cv;
			}
	if (cutval != 0) {
		cutval *= 2;
		cutval /= cntlist(&armylist);  /* adjust for multiple groups cut at same time */
		}							/* since will double count or more */
		
	killist(&armylist);
	
	thcut = 0;
	if (A_THREATENED(army) && iscut) {
		for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr])
			for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
				for (ptr3 = grcnp[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3])
					if (cntype[list[ptr3]] == CN_THREAT) {
						g1 = cngr1[list[ptr3]];
						if (g1 == list[ptr2])
							g1 = cngr2[list[ptr3]];
						if (inlist(g1,&grnbp[list[ptr]])) {
							thcut += 50+obaval/2;  /* each conn is counted twice */
							}
						}
		if (cutval != 0)
			thcut += armysize[army] * atkv[A_ALIVE(army)];
		cutval /= 2;  /* threatened cutting stones counted well already */
		}
	if (thcut)
		cutval += thcut;  /* cut is sente */
	if (!A_THREATENED(army)) {
		erun = easyrun[A_COLOR(army)];
		if (armyrn_pot[army] >= erun)
			cutval = 0;
		else
		/* probably won't kill it */
			cutval = (int)(cutval * ((float)erun - armyrn_pot[army])/(float)erun);
		}
	return(cutval);
	}






/* value of connecting armies a1 and a2.
 */

/* extra value for conencting when ahead */
static int connalive[] = 
	{ 0, 0, 25, 50, 100 };
static int connalive2[] = 
	{ 0, 0, 0, 50, 100 };

int conn_val(army_t a1, army_t a2)
{
	int numl1,numl2,cval;
	int cval1, cval2;
	if (a1 == a2)
		return 0;
	if (A_ALIVE(a1) == DEAD || A_ALIVE(a2) == DEAD)
		return 0;
	if (A_ALIVE(a1) > WEAK && A_ALIVE(a2) > WEAK && ahead > 2)
		return 0;
	if (A_ALIVE(a1) >= MUST_BE_DEAD && A_ALIVE(a2) >= MUST_BE_DEAD)
		return 0;  

	numl1 = A_NUMLIBS(a1) + A_SIZE(a1) - 1; 
	numl2 = A_NUMLIBS(a2) + A_SIZE(a2) - 1; 
	cval1 = connval[A_ALIVE(a1)];
	if (armyrn_pot[a1] > EASY_RUN * 2)
		cval1 = 5;
	else if (armyrn_pot[a1] > EASY_RUN) {
		cval1 = cval1 * (EASY_RUN * 2 - armyrn_pot[a1]) / EASY_RUN;
		if (cval1 < 5) {
			cval1 = 5;
		}
	}
	cval2 = connval[A_ALIVE(a2)];
	if (armyrn_pot[a2] > EASY_RUN * 2)
		cval2 = 5;
	else if (armyrn_pot[a2] > EASY_RUN) {
		cval2 = cval2 * (EASY_RUN * 2 - armyrn_pot[a2]) / EASY_RUN;
		if (cval2 < 5)
			cval2 = 5;
	}
	if (A_ALIVE(a1) <= VERY_ALIVE && A_ALIVE(a2) <= VERY_ALIVE)
		return MIN(connalive[ahead] * obaval / 100, 100);
	if (A_ALIVE(a1) <= ALIVE && numl1 > 10)
		numl1 = 10;
	if (A_ALIVE(a2) <= ALIVE && numl2 > 10)
		numl2 = 10;
	cval = 100;
	if (A_ALIVE(a1) <= ALIVE && A_ALIVE(a2) <= ALIVE) { /* both alive */
		cval = cval1 * numl1;
		if (cval2 * numl2 > cval)
			cval = cval2 * numl2;
		cval /= 2;
		if (cval > 300)
			cval = 300;
		
		return cval;
	}
	if (A_ALIVE(a1) <= ALIVE || A_ALIVE(a2) <= ALIVE) {  /* either alive */
		if (A_ALIVE(a1) <= ALIVE)
			cval1 /= 4;  /* less for live stone connecting */
		if (A_ALIVE(a2) <= ALIVE)
			cval2 /= 4;
		cval = cval1 * numl1 +
			cval2 * numl2;
		if (!A_THREATENED(a1) && !A_THREATENED(a2))
	        cval += connalive2[ahead];
		     /* save weak or unsettled group */
		if (cval > 400)
			cval = 400;
		return cval;
	}
	if (A_ALIVE(a1) > WEAK && A_ALIVE(a2) > WEAK && ahead < 2)
		return 50;	/* both dead */
	if (A_ALIVE(a1) <= WEAK || A_ALIVE(a2) <= WEAK) {
		if (A_ALIVE(a1) >= WEAK_POTENTIAL) {
			cval2 = 0;	/* no benefit to connect to dead group */
			cval1 = gralprob[A_GROUP(a2)]+50;
		}
		if (A_ALIVE(a2) >= WEAK_POTENTIAL) {
			cval1 = 0;
			cval2 = gralprob[A_GROUP(a1)]+50;
		}
		cval = numl1 * cval1 + numl2 * cval2;
		cval /= 2;
		if (cval > 250)
			cval = 250;
		if (A_ALIVE(a1) == SEMEAI)
			cval += A_NUMLIBS(a2) * 25;
		if (A_ALIVE(a2) == SEMEAI)
			cval += A_NUMLIBS(a1) * 25;	
		return cval;
	}
	return cval;
}




/* value of cutting apart these two armies
 * if either group is alive use the ctval table to 
 * figure out how much it is worth to connect to it
 * if both groups are unsettled or have some eye potntial
 * also use this table
 *
 * note: shouldn't use cut_val when cutting apart threatened groups,
 * capture them instead.
 */

int cut_val(army_t a1, army_t a2)
{
	int cutvalue = 0, numl1, numl2;
#ifdef CHECK
	char buf[100];
	if (A_ALIVE(a1) > DEAD || A_ALIVE(a2) > DEAD) {
		sprintf(buf,"cut_val alive %d %d, armies %d %d\n",A_ALIVE(a1),A_ALIVE(a2),a1,a2);
		outerror(buf);
	}
#endif
#ifdef CHECK
	if (pclsnext != 0)
		outerror("cut_val called without life update\n");
#endif
	if (a1 == NOARMY || a2 == NOARMY || A_ALIVE(a1) == DEAD && A_ALIVE(a2) > WEAK || 
	   A_ALIVE(a2) == DEAD && A_ALIVE(a1) > WEAK)
	   return(0);
	numl1 = A_NUMLIBS(a1);
	if (numl1 > 10)
		numl1 = 10;
	numl1 += A_SIZE(a1)-1;
	if (numl1 < 6)numl1++;
	numl2 = A_NUMLIBS(a2);
	if (numl2 > 10)
		numl2 = 10;
	numl2 += A_SIZE(a2)-1;
	if (numl2 < 6)numl2++;
#ifdef NEVER	
	if (A_THREATENED(a1) && !A_THREATENED(a2) || 
	   A_THREATENED(a2) && !A_THREATENED(a1)) {
		cutvalue = 0;
		if (A_ALIVE(a1) > WEAK && A_ALIVE(a2) <= UNSETTLED)
			cutvalue = numl1 * ctval[A_ALIVE(a1)];
		if (A_ALIVE(a2) > WEAK && A_ALIVE(a1) <= UNSETTLED)
			cutvalue = numl2 * ctval[A_ALIVE(a2)];
		return(cutvalue);
		}
	         /* can capture a group so that is better than cut */
#endif
	if (A_ALIVE(a1) <= ALIVE && A_ALIVE(a2) > ALIVE) {
		cutvalue = ctval[A_ALIVE(a1)] * numl1;
		if (cutvalue > 300)
			cutvalue = 300;
		cutvalue = (cutvalue * armyeyespace[a2])/16;
		cutvalue += ctval[A_ALIVE(a2)] * numl2;
		cutvalue = cutvalue * (50-A_ALPROB(a2))/100;  /* less value for cutting strong groups */
	}

	else if (A_ALIVE(a1) > ALIVE && A_ALIVE(a2) <= ALIVE) {
		cutvalue = ctval[A_ALIVE(a2)] * numl2;
		if (cutvalue > 300)
			cutvalue = 300;
		cutvalue = (cutvalue * armyeyespace[a1])/16;
		cutvalue += ctval[A_ALIVE(a1)] * numl1;
		cutvalue = cutvalue * (50-A_ALPROB(a1))/100;
	}

	else if (A_ALIVE(a1) <= ALIVE && A_ALIVE(a2) <= ALIVE) {
		cutvalue = ctval[A_ALIVE(a1)] * numl1;
		if (ctval[A_ALIVE(a2)] * numl2 > cutvalue)
			cutvalue = ctval[A_ALIVE(a2)] * numl2;
		if (A_ALIVE(a2) <= VERY_ALIVE && A_ALIVE(a1) <= VERY_ALIVE &&
			cutvalue > 100)cutvalue = 100;
		else if (cutvalue > 400)cutvalue = 400;
	}
	else if (A_ALIVE(a1) <= UNSETTLED && A_ALIVE(a2) <= UNSETTLED) {
		cutvalue = ctval[A_ALIVE(a1)]*numl1;
		if (armyeyepotential[a1] + armyeyepotential[a2] + armyeyespace[a1] + armyeyespace[a2] >= 24)
			cutvalue += ctval[A_ALIVE(a2)] * numl2;
		else if (ctval[A_ALIVE(a2)] * numl2 < cutvalue)
			cutvalue = ctval[A_ALIVE(a2)] * numl2;
		if (cutvalue > 600)cutvalue = 600;
	}
	else if (A_ALIVE(a1) <= WEAK && A_ALIVE(a2) <= WEAK) {
		cutvalue = ctval[A_ALIVE(a1)]*numl1;
		if (ctval[A_ALIVE(a2)] * numl2 < cutvalue)
			cutvalue = ctval[A_ALIVE(a2)] * numl2;
		if (ahead > 2)cutvalue += 150;
		if (cutvalue > 300)cutvalue = 300;
	}
	else if (A_ALIVE(a1) > WEAK && A_ALIVE(a2) > WEAK) {
		if (ahead > 2)cutvalue = 50*(numl1+numl2);
		else cutvalue = 0;
		if (cutvalue > 300)
			cutvalue = 300;
	}
	else if (A_ALIVE(a1) > WEAK) {
		cutvalue = ctval[A_ALIVE(a1)] * numl1;
		if (ahead > 2)
			cutvalue += 100;
	}
	else if (A_ALIVE(a2) > WEAK) {
		cutvalue = ctval[A_ALIVE(a2)] * numl2;
		if (ahead > 2)
			cutvalue += 100;
	}

	if ((A_THREATENED(a1) || A_THREATENED(a2)) && cutvalue > 200)
		cutvalue = 200;	/* just preference for cutting - should already be valued for capture */

	/* less cutting at beginner levels */
	if (playlevel == 0) {
		cutvalue /= 4;
	} else if (playlevel == 1) {
		cutvalue /= 2;
	}

	return cutvalue;
}

/* is army a1 stronger than army a2? */

static int stronger(army_t a1, army_t a2)
{
	if (A_ALIVE(a1) < A_ALIVE(a2))
		return TRUE;
	if (A_ALIVE(a1) > A_ALIVE(a2))
		return FALSE;
	if (A_ALIVE(a1) == RUNNING_FIGHT &&
		armyrn_pot[a1] > armyrn_pot[a2])
		return TRUE;

	return FALSE;
}

/* value of defending army.  def_val should only be added to strat rules
 * fired from save_group so that it only gets added once per move per group
 * alval1 is the alive value when enemy moves first (usually gralval[1])
 * def_val measures how many points the enemy gets by attacking this group.
 * if the attack doesn't kill the group, double the value since the attack will be sente
 */

int def_val(army_t army, int alval1) {
	list_t ptr,ptr2,tmplist = EOL,liblist = EOL;
	int size,val,aval,erun;
	int gral;
	int connval = 0;
	int dfac, prob;
	tree_t mt;
	gral = A_ALIVE(army);
	if (army == NOARMY)
		return 0;
#ifdef CHECK
	if (pclsnext != 0)
		outerror("def_val called without life update\n");
#endif
	erun = easyrun[A_COLOR(army)];
	if (army == NOARMY)return(0);
	size = armysize[army];
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		if (ltr1[list[ptr]] && ltrgd[list[ptr]] != 2)size += ltr1[list[ptr]] - 1;
		else if (!S_NEUTRAL(list[ptr]) &&
		   lnbn[list[ptr]] > 1)
		   	for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
		   		if (ld[list[ptr2]] == NOLD && 
		   		   ltr1[list[ptr2]] == 0 &&
		   		   addlist(list[ptr2],&liblist))size ++;
	}
	killist(&liblist);
	size += armylibs[army]-1;  /* since enemy will fill one to catch me */
	if (size > 2)
		size++;  /* a little extra to encourage saving groups */
#ifdef NEVER
	9/97 need to run away with single stone often - need tighter condition on light
	if (armysize[army] == 1 && size > 7)
		size = 7;   /* single stone is light */
#endif
	addlist(army,&tmplist);
	if (A_ALIVE(army) > ALIVE) {  /* save this group gets 1/2 of weaker groups can connect to */
			/* 3/01 only 1/2 for weak groups.  much less for unsettled groups, since they can help themselves */
		for (ptr = armygroups[army]; ptr != EOL; ptr = link[ptr])
			for (ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (cnprot[list[ptr2]] != CAN_CONNECT && cnprot[list[ptr2]] != MIGHT_CONNECT &&
					cnprot[list[ptr2]] != KO_CONNECT)continue;
				if (addlist(grarmy[cngr1[list[ptr2]]], &tmplist) && 
					gralive[cngr1[list[ptr2]]] > ALIVE) {
					if (gralive[cngr1[list[ptr2]]] >= WEAK_POTENTIAL)
						dfac = 2;
					else
						dfac = 5;
					connval += (armysize[grarmy[cngr1[list[ptr2]]]]+armylibs[grarmy[cngr1[list[ptr2]]]])*
					(50-gralprob[cngr1[list[ptr2]]])/dfac;
					}
				else if (addlist(grarmy[cngr2[list[ptr2]]], &tmplist) && 
					gralive[cngr2[list[ptr2]]] > ALIVE) {
					if (gralive[cngr2[list[ptr2]]] >= WEAK_POTENTIAL)
						dfac = 2;
					else
						dfac = 5;
					connval += (armysize[grarmy[cngr2[list[ptr2]]]]+armylibs[grarmy[cngr2[list[ptr2]]]])*
					(50-gralprob[cngr2[list[ptr2]]])/dfac;
				}
			}	
	}
	killist(&tmplist);				
	if (size > 25 && A_ALIVE(army) <= STRONG_SEMEAI)size = 25;
	/* strictly the amount the group will be weakened by enemy attack */
	val = gralprob[list[armygroups[army]]] - alval1;
	if (val/2 < alval1 - (-50))
		val += val/2;  /* 7/01 if can be continued to attack, add more for followup move */
	else
		val += (alval1 - (-50))/2;  /* not enough attack left to double value */
/*	val = defv[gral];  5/99 */
	if (gral == BARELY_ALIVE || gral == MIAI) {
		if (ahead >= 2)
			val += 5;
		if (gral == MIAI && armyrn_pot[army] + armycnrn_pot[army]/2 < LIMP_RUN)
			val += LIMP_RUN-armyrn_pot[army] - armycnrn_pot[army]/2;
		val += 1;	/* must have some minimum value */
	}
	else if (gral == UNSETTLED_LIMP || gral == UNSETTLED_DEAD) {
		if (ahead >= 2)
			val += 5;
		if (armyrn_pot[army] + armycnrn_pot[army]/2 < EASY_RUN)
			val += (EASY_RUN-armyrn_pot[army] - armycnrn_pot[army]/2)/2;
		val += 1;
	}

	if (val > 75) val = 75;
	if (foughtalready((group_t)list[armygroups[army]], TRYTOKILL, 1-A_COLOR(army), &mt)) {	/* 4/01 use results of reading */
		prob = treealprob(mt, A_ALPROB(army),FALSE);	/* prob is -50 to 50 probability of living after attack */
		if (A_ALPROB(army)-prob > val)
			val = A_ALPROB(army)-prob;
	}
	val *= size;
	val += connval;  /* 8/99 get extra for enabling saving of nearby groups */
	if (armyeyespace[army] < 8 && armybestpot[army] >= 8) {	/* make a base */
		if (armysecond[army] >= 8)
			val += obaval/4;
		else
			val += obaval/2;
	}
	if (gral >= BARELY_ALIVE && armysize[army] > 1 &&
		(!A_THREATENED(army) || eyeval[eyerec[mvs[grpieces[list[armygroups[army]]]]]] > 4)) {
		if (armynbp[army] == EOL)getarmynbp(army);
		aval = 0;
		for (ptr = armynbp[army]; ptr != EOL; ptr = link[ptr]) {
			if (A_ALIVE(list[ptr]) != UNSETTLED_DEAD &&
				!A_THREATENED(list[ptr]) &&
				A_ALIVE(list[ptr]) != UNSETTLED_LIMP
				&& stronger((army_t)list[ptr],army)) {
				aval += atk_val((army_t)list[ptr], gralval[list[armygroups[list[ptr]]]][0],0);
				aval += defatkv[A_ALIVE(list[ptr])] * (armysize[list[ptr]] + armylibs[list[ptr]]);
				}
			else if (!A_THREATENED(list[ptr]))
				aval += 50;
		}
#ifdef NEVER
		if (gral <= RUN_OR_LIVE && !urg) {
			if (val > 200)val = 200;
			aval /= 2;
			if (aval > 200)aval = 200;
		}
#endif
		if (aval > 400)aval = 400;
		val += aval;
	}
	if (val > 3000)val = 3000;
	if (armyrn_pot[army] >= 32 && armyeyespace[army] >= 8 && val > 50)
		val = 50;
	else if (armyrn_pot[army] > 16 && armyeyespace[army] >= 8 && val > 50)
		val = 50 + (val-50)*(32-armyrn_pot[army])/16;	/* 5/02 if have lots of extra run ability, no point in defending it */
	return val;
}


/* value of attacking army - how many points do we actually expect to get when we attack this group (and capture it or chase it around) */
/* alval0 is the group strength when it moves first */
/* full value for attacking groups that can be obviously killed */
/* otherwise, just give a little over obaval */
/* reduce value when ahed - don't start fights when ahead! */
/* reduce value of attacking dead groups when behind - can't afford to waste moves */
/* if we are certain the group can live with a move, value is the difference between current gralprob and 50 */
/* in general, we are measuring the amount the opponent can strengthen the group with a move here */
/* so the real score of the move will reflect the difference between moves by either side here */

int atk_val(army_t army, int alval0, int passval)
{
	int size,val,erun,esize=0, prob, res = V_UNKNOWNRES;
	list_t ptr, ptr2;
	tree_t mt;
	int gral;
	int alval1 = gralval[list[armygroups[(army)]]][1];	/* strength og group after I attack it */

	assert(pclsnext == 0);  /* atk_val called without life update */
	if (army == NOARMY)return(0);

	/* if have reading results, trust them more than aliveness estimates */

	if (foughtalready((group_t)list[armygroups[army]], TRYTOLIVE, A_COLOR(army), &mt)) {	/* 4/01 use results of reading */
		res = treeresval(mt);
		prob = treealprob(mt, A_ALPROB(army), TRUE);	/* prob is -50 to 50 probability of success */
		if (res >= V_WINLIKELY && prob > alval0) /* he can live */
			alval0 = prob;
		else if (res <= V_LOSELIKELY && prob < alval0)
			alval0 = prob;
	}

	if (foughtalready((group_t)list[armygroups[army]], TRYTOKILL, 1-A_COLOR(army), &mt)) {	/* 4/01 use results of reading */
		res = treeresval(mt);
		prob = -treealprob(mt, A_ALPROB(army), TRUE);	/* prob is -50 to 50 probability of living (negative prob of killing) */
		if (res >= V_WINLIKELY && prob < alval1) /* I can kill */
			alval1 = prob;
		else if (res <= V_LOSELIKELY && prob > alval1)
			alval1 = prob;
	}
	if (alval1 > alval0)
		alval1 = alval0;

	gral = A_ALIVE(army);
	/* how big is the group */
	size = armysize[army];
	size += armylibs[army] - 1;  /* since have to fill one */
	for (ptr = armylbp[army]; ptr != EOL; ptr = link[ptr]) {
		if (!S_NEUTRAL(list[ptr]) && lnbn[list[ptr]] > 1) {
		   for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
		   		if (ld[list[ptr2]] == NOLD && ltr1[list[ptr2]] == 0)
					esize ++;
		   }
		}
		if (ltr1[list[ptr]])
			esize += ltr1[list[ptr]]-1;
		else if (lnbn[list[ptr]] == 3)esize++;
	}
	size += esize;  /* 3/2002, was esize/2 */
	if (armysize[army] == 1 && size > 5)
		size = 5;	/* single stone is light */
	if (gral == RUN_OR_LIVE && size > 6)
		size = 6;
	else if (gral < UNSETTLED_THREATENED && size > 15)size = 15;

	val = alval0-(alval0+alval1)/2;
	if (gral == WEAK_POTENTIAL && armyrn_pot[army])
		val += 10;
	if (gral >= WEAK && passval < 0) {
		if (passval < -50*50)
			val = 0;
		else
			val /= 2;
	}

	if (val < 1)
		val = 1;
	val *= size;

	if (armyeyespace[army] < 8 && armybestpot[army] >= 8 && 
		armyeyespace[army] + armybestpot[army] < 16)
		val += obaval;	/* prevent from getting a base */

	erun = easyrun[A_COLOR(army)];
	if (armyrn_pot[army] >= erun && val > 200)
		val = 200;	/* no more than 4 points for chasing something that can get away */
	if (safecaptarmy(army,1-A_COLOR(army),0))
		val += safecaptval(army,passval);
	if (val > 2500)val = 2500;
	if (problemflag != 1 && res < V_WINLIKELY && /* 5/01 keep full value when solving problems */
		ahead > 2 && grthreatened[list[armygroups[army]]] != 2) {  /* when ahead, stop fighting - Kerwin. */
		if (g2abs(passval) > 80 * 50)                               /* but capturing threatened groups still gets full value */
			val /= 4;
		else if (g2abs(passval) > 20 * 50) {
			val = (int)(val * (100. - g2abs(passval) / 50.) / 80.);  /* float to avoid overflow */
		}
	}
	if (armyrn_pot[army] >= 32 && armyeyespace[army] >= 8 && val > 50)
		val = 50;
	else if (armyrn_pot[army] > 16 && armyeyespace[army] >= 8 && val > 50)
		val = 50 + (val - 50) * (32 - armyrn_pot[army]) / 16;	/* 5/02 if have lots of extra run ability, no point in attacking it */
	return val;
}
