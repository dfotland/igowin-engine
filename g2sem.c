/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

# include "g2hd.h"
#include "g2sem.h"

int semrunlibs[NUMRUN] = /* how many new liberties per running type */
/* in addition to liberties gained by extending at liberty */
{ 12,12,10,8,4,3,2,2,2,2,1,1,1,1};
int semrunminlibs[NUMRUN] = /* minimum extra liberties for running here */
{  6, 6, 5,4,3,2,1,1,1,1,0,0,0,0};                                                      


/* return the probability of a1 winning a semeai against a2 by 
 * capturing a2. (0 to 100 percent). 
 * based on the differences
 * in min/max liberty counts, eyes, and common liberties.
 * limited by the amount of running it has
 * input is armies and offset in liberties*LIBVALUE.
 * even if probability is zero, it is still possible for a1 to live by making 2 eyes or a seki
 * this is only probability of a capture.
 */
 
int winsemprob(army_t a1, army_t a2, int lessa2, int lessa1) {
	int prob = 50;
	int min1,typ1,max1,hemin1,hemax1,memin1,memax1,min2,typ2,max2,hemin2,hemax2,memin2,memax2;
	int comlibs;
	int maxprob;
	int a1prob = 50, a2prob = 50;	/* probs with a1 or a2 moving first */
	
	if (armyrn_pot[a2] >= WINSEM_RUN)
		return 0;
	if (armyeyespace[a2] >= 16)
		return 0;
	/* limit the maximum prob of winning by the running value of a2 */
	maxprob = 100*(WINSEM_RUN - armyrn_pot[a2])/WINSEM_RUN;
	
	if (a1 == NOARMY || a2 == NOARMY)
		return 0;
	semeailibs(a1,&min1,&max1,&typ1,&hemin1,&hemax1,&memin1,&memax1);
	semeailibs(a2,&min2,&max2,&typ2,&hemin2,&hemax2,&memin2,&memax2);
	comlibs = comlist(armylbp[a1],armylbp[a2]);
	if (comlibs > 0) {
		/* neither has an eye */
		if (armyeyespace[a1] < 8 && armyeyespace[a2] < 8) {
			if (comlibs > 1) {  /* could be a seki */
				hemin2 += comlibs;
				hemax2 += comlibs;
				memin2 += comlibs;
				memax2 += comlibs;
				}
			else if (armyeyespace[a2] + armybestpot[a2] >= 8) {
				memin2 += comlibs-1;	/* might have to fill a lib to make the eye */
				memax2 += comlibs;
				}
			if (armyeyespace[a1] + armybestpot[a1] >= 8) {
				memin1 += comlibs-1;
				memax1 += comlibs;
				}
			}
		/* enemy has an eye */
		else if (armyeyespace[a1] < 8 && armyeyespace[a2] >= 8) {
			hemin2 += comlibs;
			hemax2 += comlibs;
			memin2 += comlibs;
			memax2 += comlibs;
			if (armyeyespace[a1] + armybestpot[a1] >= 8) {
				memin1 += comlibs-1;
				memax1 += comlibs;
				}
			}
		else if (armyeyespace[a1] >= 8 && armyeyespace[a2] < 8) {
			hemin1 += comlibs;
			hemax1 += comlibs;
			memin1 += comlibs;
			memax1 += comlibs;
			if (armyeyespace[a2] + armybestpot[a2] >= 8) {
				memin2 += comlibs-1;
				memax2 += comlibs;
				}
			}
		else if (armyeyespace[a1] >= 8 && armyeyespace[a2] >= 8) {
			/* he gets comlibs to avoid making a seki */
			hemin2 += comlibs;
			hemax2 += comlibs;
			memin2 += comlibs;
			memax2 += comlibs;
			}
		}
	/* he can live */
	if (armyeyespace[a2] + armybestpot[a2] >= 16) {
		memin2 += 20;
		memax2 += 20;
		}
	/* I can live */
	if (armyeyespace[a1] + armybestpot[a1] >= 16) {
		memin1 += 20;
		memax1 += 20;
		}
	/* assume for this purpose that he can make his potential eye */
	if (comlibs > 0 && armyeyespace[a1] < 8 && armyeyespace[a2] + armybestpot[a2] >= 8) {
		min1 -= comlibs;
		typ1 -= comlibs;
		max1 -= comlibs;
		}
	if (comlibs > 0 && armyeyespace[a2] +armybestpot[a2] < 8 && armyeyespace[a1] >= 8) {
		min2 -= comlibs;
		typ2 -= comlibs;
		max2 -= comlibs;
		}

	min1 *= LIBVALUE;
	typ1 *= LIBVALUE;
	max1 *= LIBVALUE;
	min2 *= LIBVALUE;
	typ2 *= LIBVALUE;
	max2 *= LIBVALUE;
	hemin1 *= LIBVALUE;
	hemax1 *= LIBVALUE;
	memin1 *= LIBVALUE;
	memax1 *= LIBVALUE;
	hemin2 *= LIBVALUE;
	hemax2 *= LIBVALUE;
	memin2 *= LIBVALUE;
	memax2 *= LIBVALUE;

	memax1 -= lessa1;
	if (memax1 < 1)
		memax1 = 1;
	if (memin1 > memax1)
		memin1 = memax1;
	if (hemax1 > memax1)
		hemax1 = memax1;
	if (hemin1 > hemax1)
		hemin1 = hemax1;

	memax2 -= lessa2;
	if (memax2 < 1)
		memax2 = 1;
	if (memin2 > memax2)
		memin2 = memax2;
	if (hemax2 > memax2)
		hemax2 = memax2;
	if (hemin2 > hemax2)
		hemin2 = hemax2;

	max1 -= lessa1;
	if (max1 < 1) {
		min2 += 1-max1;
		if (typ2 < min2)
			typ2 = min2;
		if (max2 < min2)
			max2 = min2;
		max1 = 1;
	}
/*	typ1 -= lessa1; */
	max2 -= lessa2;
	if (max2 < 1) {
		min1 += 1-max2;
		if (typ1 < min1)
			typ1 = min1;
		if (max1 < min1)
			max1 = min1;
		max2 = 1;
	}
/*	typ2 -= lessa2; */
	if (typ1 > max1)
		typ1 = max1;
	if (typ2 > max2)
		typ2 = max2;
	if (min1 > typ1)
		min1 = typ1;
	if (min2 > typ2)
		min2 = typ2;

#ifdef NEVER
	if (max1 < min2 + LIBVALUE)
		max1 = min2+LIBVALUE;
	if (max1-min2 > 30*LIBVALUE)
		max1 = min2 + 30*LIBVALUE;  /* prevent overflow */
	if (max2 < min1 + LIBVALUE)
		max2 = min1+LIBVALUE;
	ASSERT(max2-min1+max1-min2 >=0)
	if (max2-min1+max1-min2 <= 0) {
		if (max1 > max2)
			prob = maxprob;
		else if (max1 == max2)
			prob = maxprob/2;
		else prob = 0;
		}
	else
		prob = (maxprob * (max1-min2))/(max2-min1+max1-min2);
	 
	if (typ1 < min2 + LIBVALUE)
		typ1 = min2+LIBVALUE;
	if (typ1-min2 > 30*LIBVALUE)
		typ1 = min2 + 30*LIBVALUE;  /* prevent overflow */
	if (typ2 < min1 + LIBVALUE)
		typ2 = min1+LIBVALUE;
	ASSERT(typ2-min1+typ1-min2 >=0)
	if (typ2-min1+typ1-min2 <= 0) {
		if (typ1 > typ2)
			typprob = maxprob;
		else if (typ1 == typ2)
			typprob = maxprob/2;
		else typprob = 0;
		}
	else
		typprob = (maxprob * (typ1-min2))/(typ2-min1+typ1-min2); 
/*	prob = (prob+typprob)/2; */
#endif
	/* 4/01 calc probs with either side to move and average them */
	/* add a libvalue to each difference since value must be > to win, not = */
	/* first with a1 to move */
	if (memin1 > hemax2)
		a1prob = maxprob;
	else if (memax1 < hemin2)
		a1prob = 0;
	else if (hemax2-memin1+memax1-hemin2 <= 0) {
		a1prob = maxprob/2;
		}
	else
		a1prob = (maxprob * (memax1-hemin2+LIBVALUE))/(hemax2-memin1+memax1-hemin2+2*LIBVALUE);

	/* now with a2 to move first - can get at least maxprob by running */
	if (hemin1 > memax2)
		a2prob = maxprob;
	else if (hemax1 < memin2)
		a2prob = 0;
	else if (memax2-hemin1+hemax1-memin2 <= 0) {
		a2prob = maxprob/2;
		}
	else
		a2prob = (maxprob * (hemax1-memin2+LIBVALUE))/(memax2-hemin1+hemax1-memin2+2*LIBVALUE);

	/* 4/00 max and min probs when ranges don't overlap */
	if (min1 > max2)
		prob = maxprob;
	else if (min2 > max1)
		prob = 0;
	else if (typ2-min1+typ1-min2 <= 0) {
		if (typ1 > typ2)
			prob = maxprob;
		else if (typ1 == typ2)
			prob = maxprob/2;
		else prob = 0;
		}
	else if (max2-min1+max1-min2+typ2-min1+typ1-min2 <= 0)
		prob = maxprob/2;
	else
	/* 4/99 better prob when typ is much less than max */
		prob = (maxprob * (max1-min2+typ1-min2))/(max2-min1+max1-min2+typ2-min1+typ1-min2);
	if (armyeyespace[a2] + armybestpot[a2] >= 16 && prob > maxprob/2)
		prob = (maxprob/2+prob)/2;  /* if enemy can live limit prob to 75 */
	if (armyeyespace[a2] + armybestpot[a2] >= 12 && prob > maxprob*3/4)
		prob = (maxprob*3/4+prob)/2;  /* enemy 1.5 eye, limit prob to 87.5 */
	if (armyeyespace[a1] + armybestpot[a1] >= 16 && prob < maxprob/2)
		prob = (maxprob/2+prob)/2; /* if friend can live, at least 25 */
	if (A_THREATENED(a2) && prob < maxprob/2)
		prob = (maxprob/2+prob)/2;

	/* if one side or other has already moved, give prob just from one side point of view */
	if (lessa1)
		prob = a1prob;
	else if (lessa2)
		prob = a2prob;
	else
		prob = (a1prob+a2prob)/2;
	if (prob < 0)
		prob = 0;
	if (prob > maxprob)	/* final check for value in range */
		prob = maxprob;
	return prob;
	}





/* number of approach moves needed by color c to fill liberty at s
 * CANTAPPROACH means that this liberty can never be filled
 * a is army being attacked, so ignore moves that capture is
 * connecting to grp has already been counted, so ignore it
 * must be c color stone adjacent to s, and s is lnbn 0
 */
	
static int approachneeded(army_t a, sqr_t s, int c, group_t grp, group_t grp2) {
	sqr_t l, l2;	/* other liberties of 2 liberty groups can connect to */
	group_t g = NOGROUP;	/* first adjacent 2 liberty enemy gorup */
	group_t g2 = NOGROUP;	/* second adjacent 2 liberty enemy gorup */
	list_t ptr, ptr2;
	int onelib = FALSE;
	for (ptr = nbgrp[s][1-c]; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[list[ptr]] == 1) {
			if (grsize[list[ptr]] > 1)
				return 0;	/* no approach if can capture on s (check for snapback later) */
			}
		}
	if (lnbn[s] == 1)
		return 1;
	assert(lnbn[s] == 0);
	assert(nbgrp[s][c] != EOL);
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[list[ptr]] > 2)return(0);  /* no approach if more than 2 liberties */
		if (gralive[list[ptr]] == DEAD)return 0;  /* an interior eye, no approach move */
		}

	/* look at enemy groups next to liberty.  If filling libery would be self-atari, need approach */
	for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] == grp || list[ptr] == grp2)continue;
		if (grlibs[list[ptr]] == 1)onelib = TRUE;
		if (grlibs[list[ptr]] == 2) {
			if (g2 != NOGROUP)
				return 1;	/* can't handle 3 groups here yet */
			if (g != NOGROUP) {	/* more than one 2 liberty group here */
				g2 = g;
				l2 = list[grlbp[list[ptr]]];
				if (l2 == s)l2 = list[link[grlbp[list[ptr]]]];
				if (l2 != l)
					return 0;	/* 2/02 two groups connect to have 2 liberties */
				}
			g = (group_t)list[ptr];
			l = list[grlbp[list[ptr]]];
			if (l == s)l = list[link[grlbp[list[ptr]]]];
			if (lnbn[l] > 1)
				return 1;  /* can extend for more liberties */
			}
		}

	if (g == NOGROUP) {	/* there are no adjacent group with two liberites that can't get more */
		if (onelib)		/* adjacent enemy group has only 1 liberty */
			return 1;	/* need to save 1 liberty group first (know its not dead, so can be saved) */
		else
			return CANTAPPROACH;  /* can't get more liberties */
	}

	/* g is a 2 liberty group.  g2 if exists, also 2 liberty group, sharing same other liberty */
	/* need at least one approach move.  see if need even more */

	/* can get more liberties with a capture? */
	for (ptr = grnbp[g]; ptr != EOL; ptr = link[ptr]) {
		if (grarmy[list[ptr]] == a)continue;
		if (grlibs[list[ptr]] == 1) {
			if (grsize[list[ptr]] > 2)return 1;  /* can capture and throwin doesn't reduce again */
			for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
				if (list[ptr2] != g && list[ptr2] != g2 && 
					grlibs[list[ptr2]] >= 2)
					return 1;	/* can capture for another liberty (not exact) */
			}
		}

	if (g2 != NOGROUP)
	  for (ptr = grnbp[g2]; ptr != EOL; ptr = link[ptr]) {
		if (grarmy[list[ptr]] == a)continue;
		if (grlibs[list[ptr]] == 1) {
			if (grsize[list[ptr]] > 2)return 1;  /* can capture and throwin doesn't reduce again */
			for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
				if (list[ptr2] != g2 && list[ptr2] != g &&
					grlibs[list[ptr2]] >= 2)
				return 1;	/* can capture for another liberty (not exact) */
			}
		}

	/* can we sacrifice these stones with a nakade shape? if so, no extras counted here, since nakade counted elsewhere */
	if (link[nbgrp[l][c]] == EOL && link[nbgrp[s][c]] == EOL && grsize[g] <= 2)
		return 0;
#ifdef NEVER
	
	/* can get more liberties with a connection? */
	for (ptr = nbgrp[l][c]; ptr != EOL; ptr = link[ptr]) {
		if (list[ptr] == g)continue;
		if (list[ptr] == g2)continue;
		if (grlibs[list[ptr]] > 2)
			return 1;	/* can connect for more liberties */
		if (grlibs[list[ptr]] == 2) {
			if (g3 != NOGROUP)
				return 1;	/* can connect to two more groups */
			g3 = (group_t)list[ptr];
			}
		}
	if (g3 == NOGROUP)
		return 100;		/* can never fill this liberty */
#endif
	return 1 + approachneeded(a, l, c, g, g2);  /* see how many moves needed for this new group to get 3 liberties */
	}

#ifdef NEVER
/* nakade libs less one for filling a point to make the capture */
static int deadlibs[7] = { 0, 0, 1, 2, 4, 7, 11 };
#endif

static int libsfordeadgroup(group_t g, army_t a) {
	int libs = 0;
	list_t ptr;
#ifdef NEVER
	4/01 handled in eye size
	size = grsize[g];
	if (size > 6)size = 6;
	libs = deadlibs[size];
#endif
	if (armynbp[grarmy[g]] == EOL)
		getarmynbp(grarmy[g]);
	for (ptr = armynbp[grarmy[g]]; ptr != EOL; ptr = link[ptr])
		if (list[ptr] != a)
			libs += armylibs[list[ptr]];
	return libs;  /* 4/01 don't subtract libs of groups since have to capture it, since eye eval will do it */
	}


/* how many extra liberties does army a get by playing at s 
 * must use the same formula for semeailibs and fight newlibcount
 * to get correct probabilities of winning fights 
 */

int extralibs(army_t a, sqr_t s) {
	int libs = 0, tmp;
	int canjump = !S_NEUTRAL(s);	/* ok to jump since no enemy nearby */
	list_t ptr2;
	int l2count = 0;
	int extra = FALSE;

	if (lnbn[s] >= 2) {  /* new liberites adjacent to move, less 1 for move */
		tmp = lnbn[s]-1-comlist(nblbp[s],armylbp[a]);
		if (tmp > 0)libs += tmp;
		}

	if (canjump)
	  for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2])
		if (lnbf[list[ptr2]][1-A_COLOR(a)] || edge[list[ptr2]] <= 1)
			canjump = FALSE;	/* enemy can cut, or too close to edge */

	if (ld[s] >= 3 && lnbn[s] == 3 && canjump) {	/* can jump here */
		libs++;
		}
	for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
		if (lnbn[list[ptr2]] >= 3 && !inlist(list[ptr2], &armylbp[a]) &&
			lnbn[list[ptr2]] - comlist(nblbp[list[ptr2]],armylbp[a]) >= 2)
			l2count++;
		if (lnbn[list[ptr2]] == 4)
			extra = TRUE;  /* access to more liberties */
		}
	if (l2count > 1)
		libs += l2count-1;
	if (libs && extra)  /* 4/00 need nonzero libs to make the try for extra sente worth something */
		libs++;

	return libs;
}

#define MAXBIGEXTRA 7
/* extra liberties for big eye by size */
int bigextra[MAXBIGEXTRA] = {
	0, 0, 0, 0, 1, 3, 6
};

/* how many new liberties does a get for making the connection at cn? */

int newconnlibs(army_t a, conn_t cn, army_t *anew) {
	int libs = 0;
	list_t ptr3, ptr;

	*anew = NOARMY;
	if (!CANCONN(cnprot[cn]))
		return 0;
	if (grarmy[cngr1[cn]] == grarmy[cngr2[cn]]) {  /* connection to self */
		return 0;
		}
	if (grarmy[cngr1[cn]] != a)*anew = grarmy[cngr1[cn]];
	else if (grarmy[cngr2[cn]] != a)*anew = grarmy[cngr2[cn]];
	else return 0;
	libs = armylibs[*anew]-1;
	if (armyrn_pot[*anew] > LIMP_RUN)libs += 10;
	else libs += armyrn_pot[*anew];
	if (armyeyes[*anew] >= 16)libs += 50;
	else if (armyeyes[*anew] >= 8)libs += armylibs[*anew]-1;
	if (armycnrn_pot[*anew] > armyrn_pot[a])	/* crude approximation */
		libs += armycnrn_pot[*anew] - armyrn_pot[a];
	if (libs < 50) {
		if (armynbp[*anew] == EOL)getarmynbp(*anew);
		for (ptr3 = armynbp[*anew]; ptr3 != EOL; ptr3 = link[ptr3])
			if (A_THREATENED(list[ptr3]))
				libs += A_SIZE(list[ptr3]) + A_NUMLIBS(list[ptr3]);
		for (ptr = armylbp[*anew]; ptr != EOL; ptr = link[ptr])
			if (lnbn[list[ptr]] == 1 && lnbf[list[ptr]][1-A_COLOR(*anew)] == 0 &&
				lnbf[list[nblbp[list[ptr]]]][A_COLOR(*anew)] == 0)libs++;	/* approach move required */
		}
	return libs;
}

/* return number of liberties (min, typ and max) army a has in a semeai.
 * and the number of ko approach moves needed to capture the group
 * each liberty is an unanswered move against a group.
 * add one for protected liberties and places where can extend.
 * add liberties for adjacent threatened groups or dead groups
 * min is minimum number of libs if enemy moves first (before the enemy move).
 *   adjusted down for liberties which are connections - assume save whole group
 *   including ko connections, since for minimum assume he can win the ko.
 * typ is typical number of liberties with no moves (he moves first against eyes, running, or connection).
 * max is max number of libs if I move first.
 *   adjusted up if he needs approach moves, 
 *   add one for each moves for taking a ko
 *   adjust up if there are dead stones to take off the board
 * if I am not tactically captured, max will be at least 5
 * includes liberties of group I can connect to.
 * I can tenuki and still win the fight if my min liberties is greater than his max liberties
 * hemin-hemax is bounds on liberties if he moves first
 * memin-memax is bounds on liberties if I move first
 * these counts are before the move is actually made!
 * msut be called after eye evaluation is complete
 */

int semeailibs(army_t a, int *min, int *max, int *typ, int *hemin, int *hemax, int *memin, int *memax) {
	list_t ptr, ptr2, ptr3, ptr4, conlist = EOL, tmplist = EOL;
	group_t g;
	conn_t cn;
	int maxconn = 0, libs, thlibs, deadlibs = 0;
	list_t forceconn = EOL;
	list_t connpoints = EOL;	/* only allow one connection through each point */
	int secondconn = 0, eyes;
	int apr = 0;  /* approach moves needed for this move */
	int totapr = 0;  /* total approach moves needed */
	int bigapr = 0;  /* biggest approach move needed */
	int cnt = 0;	/* total extra liberties for extending everywhere */
	int maxcnt = 0, secondcnt = 0, mincnt = 0;  /*  best, second best, 3rd best extensions */ 
	int koapr = 0;
	list_t sente = EOL;  /* number libs can remove in sente */
	int c,tmp,i;
	int minrun = 0;
	int bestrun = 0, secondrun = 0, totrun = 0;  /* liberties for running */
	int size = 0;	/* for big eye liberty bonus */
	int best = 0, second = 0, third = 0;  /* best, etc improvements I can make */
	int inside = 0;	/* number of points already filled inside the eye */
	int nakade = 0;
	int force;		/* forced to play here and lose a liberty */
	int twolibs = 0;
	army_t anew;
	c = grcolor[list[armygroups[a]]];
	eyes = armyeyespace[a];
	/* look at each liberty to see if approach move is needed, and how many new liberties from extending */
	for (ptr = armylbp[a]; ptr != EOL; ptr = link[ptr]) {
		apr = 0;
		if (lnbn[list[ptr]] == 1 && lnbf[list[ptr]][1-c] == 0 &&
			lnbf[list[nblbp[list[ptr]]]][c] == 0)
			apr += 1; /* + approachneeded(a, list[nblbp[list[ptr]]], 1-c, NOGROUP, NOGROUP); */
		if (lnbn[list[ptr]] == 0 && lnbf[list[ptr]][1-c] == 0 &&
			eyeval[eyerec[list[ptr]]] == 0 && armylibs[a] > 1) {
			for (ptr2 = nbgrp[list[ptr]][c]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (grsize[list[ptr2]] <= 2) {
					koapr++;  /* will be a ko or capture/recapture/ko */
					break;
				}
			}
		}
		if (lnbn[list[ptr]] == 0 && lnbf[list[ptr]][1-c] != 0)
			apr += approachneeded(a, list[ptr], 1-c, NOGROUP, NOGROUP);
		tmp = extralibs(a, list[ptr]);
		cnt += tmp;
		if (tmp > maxcnt) {	
			mincnt = secondcnt;
			secondcnt = maxcnt;
			maxcnt = tmp;
		}
		else if (tmp > secondcnt) {
			mincnt = secondcnt;
			secondcnt = tmp;
		}
		else if (tmp > mincnt) {
			mincnt = tmp;
		}
		totapr += apr;
		if (apr > bigapr)bigapr = apr;
	}
	
	for (ptr = armygroups[a]; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[list[ptr]] == 2)
			twolibs++;  /* opponent can force filling of one liberty unless sac group */
		for (ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			cn = list[ptr2];
			if (PROTCONN(cn)) {  /* connection within army */
				if (cnptr[cn] != EOL) {
					if (cncnum[cn] == 1) {  /* sente peep? */
						if (eyerec[list[cnptr[cn]]] != 0 &&  /* sente atari later to fill here */
							eyepot[eyerec[list[cnptr[cn]]]] <= 4 && (lnbn[list[cnptr[cn]]] == 1 ||
							comlist(nblbp[list[cnptr[cn]]],armylbp[a]) == lnbn[list[cnptr[cn]]]))
							addlist(list[cnptr[cn]], &forceconn);  /* opp can force connection here and get no new liberties when connect */
						else {
						  for (ptr3 = nblbp[list[cnptr[cn]]]; ptr3 != EOL; ptr3 = link[ptr3])
							if (lnbf[list[ptr3]][c] &&  /* peep fills liberty */
								(lnbn[list[ptr3]] == 3 || lnbf[list[ptr3]][1-c] != 0 && gralive[lgr[list[ptr3]]] != DEAD)) {
								addlist(list[cnptr[cn]],&sente);
								break;
							}
						}
					}
					else if (cncnum[cn] == 2) {
						for (ptr3 = cnptr[cn]; ptr3 != EOL; ptr3 = link[ptr3]) {
							force = TRUE;
							for (ptr4 = nbgrp[list[ptr3]][1-c]; ptr4 != EOL; ptr4 = link[ptr4])
								if (gralive[list[ptr4]] == DEAD) {
									force = FALSE;  /* get new liberty when capture here */
									break;
								}
							if (force)
								addlist(list[ptr3], &conlist);
						}
					}
				}
				else if (cnlkptr[cn] != EOL)
					mrglist(cnlkptr[cn],&conlist);
				else
					mrglist(cnllptr[cn],&conlist);
			}
			if (CANCONN(cnprot[cn]) &&
				grarmy[cngr1[cn]] == grarmy[cngr2[cn]] &&  /* connection to self */
				cnptr[cn] != EOL)
				mrglist(cnptr[cn],&conlist);  /* can push thru? */
			libs = newconnlibs(a, cn, &anew);
			if (anew != NOARMY)
				tmplist = rmconnect(anew, cn);
			if (libs > maxconn || 
				libs == maxconn && cntlist(&tmplist) > cntlist(&connpoints) ||
				libs == maxconn && cntlist(&tmplist) == cntlist(&connpoints) &&
				list[tmplist] < list[connpoints]) {	/* order independent */
				if (cnprot[cn] == SHARED_CONNECT && libs > secondconn)
					secondconn = libs;  /* can't cut shared conn */
				killist(&connpoints);
				maxconn = libs;
				connpoints = tmplist;
				tmplist = EOL;
			}
			killist(&tmplist);
		}
	}
	for (ptr = armygroups[a]; ptr != EOL; ptr = link[ptr]) {
		for (ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			cn = list[ptr2];
			if (!PROTCONN(cn)) {  /* connection within army */
				libs = newconnlibs(a, cn, &anew);
				if (libs <= secondconn || anew == NOARMY)
					continue;
				tmplist = rmconnect(anew, cn);
				if (!comlist(tmplist, connpoints)) {
					secondconn = libs;
				}
				killist(&tmplist);
			}
		}
	}
	killist(&connpoints);
	if (A_THREATENED(a))
		secondconn = 0;  /* no chance to connect since will die */
	thlibs = 0;
	deadlibs = 0;
	if (armynbp[a] == EOL)getarmynbp(a);
	for (ptr = armynbp[a]; ptr != EOL; ptr = link[ptr]) {
		g = (group_t)list[armygroups[list[ptr]]];
		if (G_ALIVE(g) == DEAD) {
			deadlibs = libsfordeadgroup(g,a);
			continue;
		}
		if (!G_THREATENED(g)) {
			if (grlibs[g] == 2 && comlist(grlbp[g], armylbp[a]) == grlibs[g] && /* all common liberties */
				newlist(armygroups[a], grnbp[g]) == 0 &&	/* capture doesn't connect to other groups */
				grsize[g] + grlibs[g] < MAXBIGEXTRA &&
				canmakenakade((army_t)list[ptr])) {
				nakade += bigextra[grsize[g] + grlibs[g]];  /* he can sacrifice to make nakade */
			}
			continue;
		}
		thlibs += grsize[g];
		if (grlibs[g] == 1)thlibs += lnbn[list[grlbp[g]]];
		if (armynbp[list[ptr]] == EOL)getarmynbp((army_t)list[ptr]);
		for (ptr2 = armynbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (list[ptr2] == a)continue;
			thlibs += armylibs[list[ptr2]];
		}
	}

	for (i = 0; i <= MAXOPENRUN; ++i) {
		for (ptr = armyrun[a][i]; ptr != EOL; ptr = link[ptr]) {
			if (bestrun == 0)
				bestrun = semrunlibs[i];
			else if (secondrun == 0) {
				secondrun = semrunlibs[i];
				minrun = semrunminlibs[i];
			}
			else minrun += semrunminlibs[i];
			totrun += semrunlibs[i];
		}
	}
	
	if (koapr != 0 && armyeyespacemax[a] < 8)koapr--;
	     /* if no eyes, don't need to make last approach move */
	/* get extra liberties for a big eye */
	if (armyeyespace[a] >= 8) {
		for (ptr = armyeyerecs[a]; ptr != EOL; ptr = link[ptr]) {
			if (eyeval[list[ptr]] < 8)
				continue;
			size = cntlist(&eyeptr[list[ptr]]);
#ifdef NEVER
			2/02 still count dead stones as liberties
			if (size <= 3)
				continue;	/* must be at least 3 points for extra liberties */
#endif
			inside = 0;
			/* DEADEYE is exactly one dead group, not including its liberties */
			if (eyetype[list[ptr]] == DEADEYE) {
				for (ptr2 = eyeptr[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
					size += mrglist(grlbp[board[list[ptr2]]], &tmplist);
				killist(&tmplist);
			}
#ifdef NEVER
			for (ptr2 = eyeptr[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
				if (board[list[ptr2]] != NOGROUP)
					inside++;	/* stones already played inside the eye */
				else if (inlist(list[ptr2], &armylbp[a]))
					inside++;	/* already counted as a liberty */
#endif
			if (eyetype[list[ptr]] == OPENLINEEYE)size -= 4;
			else if (eyetype[list[ptr]] == LINEEYE)size -= 2;
			else {
				for (ptr2 = eyeptr[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
					for (ptr3 = nblbp[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3]) {
						if (eyerec[list[ptr3]] != list[ptr]) {
							size--;		/* open edge of eye can be reduced */
							break;	
						}
					}
				}
			}
			if (size > 6)size = 6;
			if (size < 0)size = 0;
			nakade += bigextra[size]; /*+ size - inside*/
		}
	}
	apr = totapr;			/* approach move needed - don't need biggest if don't have an eye */
	if (armyeyespace[a] < 8)
		apr -= bigapr;
	if (apr < 0)
		apr = 0;
	if (apr > CANTAPPROACH)
		apr = CANTAPPROACH;
	*min = armylibs[a] + minrun + mincnt -  /* get liberties for running */
		cntlist(&conlist) - cntlist(&sente) -  /* may have to fill all */
		cntlist(&forceconn) + deadlibs + nakade - twolibs;				     /* internal connections - must make approach moves */
	if (mincnt > 0 || minrun > 0)	/* opponent takes a liberty in exchange for this one */
		(*min)--;
	if (*min < 1)
		*min = 1;
	*min += apr;
	*max = armylibs[a] + apr + cnt + koapr + maxconn + thlibs + totrun - secondrun + deadlibs + nakade;
	*typ = armylibs[a] + apr + secondconn + secondrun + secondcnt + deadlibs + nakade;
	*memin = armylibs[a] + apr + koapr + deadlibs + nakade;
	best = maxcnt+mincnt;
	second = secondcnt;
	if (maxconn > best) {
		third = second;
		second = best;
		best = maxconn;
	}
	else if (maxconn > second) {
		third = second;
		second = maxconn;
	}
	else if (maxconn > third)
		third = maxconn;
	if (secondconn > best) {
		third = second;
		second = best;
		best = secondconn;
	}
	else if (secondconn > second) {
		third = second;
		second = secondconn;
	}
	else if (secondconn > third)
		third = secondconn;
	if (thlibs > best) {
		third = second;
		second = best;
		best = thlibs;
	}
	else if (thlibs > second) {
		third = second;
		second = thlibs;
	}
	else if (thlibs > third)
		third = thlibs;
	if (bestrun > best) {
		third = second;
		second = best;
		best = bestrun;
	}
	else if (bestrun > second) {
		third = second;
		second = bestrun;
	}
	else if (bestrun > third)
		third = bestrun;
	if (secondrun > best) {
		third = second;
		second = best;
		best = secondrun;
	}
	else if (secondrun > second) {
		third = second;
		second = secondrun;
	}
	else if (secondrun > third)
		third = secondrun;
	if (minrun > best) {
		third = second;
		second = best;
		best = minrun;
	}
	else if (minrun > second) {
		third = second;
		second = minrun;
	}
	else if (minrun > third)
		third = minrun;
	*memin += best+third;
	if (thlibs && armylibs[a] > 1)(*typ)++;  /* to save threatened group */
	if (thlibs >= bestrun || maxconn >= bestrun) {
		*typ += bestrun;
		*min += minrun;  /* add it a second time since best m ust be at least as good */
	}
	else {
		*typ += secondrun;
	}

	if (armyeyespace[a] >= 16) {
		(*min) += 20;
		(*typ) += 20;
		(*hemin) += 20;
		(*max) += 20;
	}
#ifdef NEVER
	/* can't use pot eyes here, since weaklive can change then, and result is order dependent */
	else if (armyeyespace[a] + armybestpot[a] >= 16) {
		(*max) += 20;
		(*hemin) += 20;
		if (armyeyespace[a] + armysecond[a] >= 16)
			(*typ) += 20;
		}
#endif
	*hemin = *min;
	*hemax = *typ;
	*memax = *max;
	killist(&conlist);
	killist(&forceconn);
	killist(&sente);
	return koapr;
}
	
/* if has CANTCATCHLIBS libs minimum - impossible to catch */
# define CANTCATCHLIBS 20
/* if has CANTESCAPELIBS libs maximum - can't be certain it's caught */
# define CANTESCAPELIBS 10
/* if both have CANTTELLLIBS libs maximum - too many liberties to tell who
 * is ahead
 */
# define CANTTELLLIBS 25
/* typ lib difference to probably win fight */
# define AHEADLIBS 5

/* return result of semeai between a1 and a2. (can a1 win the semeai)
 * 0 - a1 wins, even if a2 moves first
 * 1 - a1 is ahead
 * 2 - about even
 * 3 - a2 is ahead
 * 4 - a2 wins, even if a1 moves first
 * 5 - seki (a1 gets at least a seki, provided that a2 can't make a nakade shape)
 * 6 - both can run away or both have too many liberties
 * 7 - unsettled seki - a1 can get a seki if a1 moves first
 */

int semeai_result(army_t a1, army_t a2)
{ 
	int l1min, l1max, l2min, l2max, lcom, esc1, esc2, l1typ, l2typ, a1eyes, a2eyes;
	int l1hemin, l1hemax, l1memin, l1memax, l2hemin, l2hemax, l2memin, l2memax;
	int cantcatch1, cantcatch2;  /* group 1 or 2 has enough liberties or running that other group can't catch it */
	list_t ptr, ptr2;
	int lapr = 0;	/* how many approach moves does a2 need to make to catch a1 */
	int a1ko, a2ko;	/* a1 or a2 need ko capture to win fight */
	int a1pot = 0, a2pot = 0;
	int a1g2libs;	/* all a1 groups have 2 liberties */

	/* ASSERT(a1 < NUMARMIES && a2 < NUMARMIES); fixes bug */
	esc1 = armyrn_pot[a1] + armycnrn_pot[a1] > 1;
	esc2 = armyrn_pot[a2] + armycnrn_pot[a2] > 1;
	
	a1eyes = armyeyespace[a1];
	if (armysecond[a1] >= 8)		/* can't prevent making an eye */
		a1eyes += 8;
	if (armybestpot[a1] >= 8)
		a1pot = 8;
	a2eyes = armyeyespace[a2];
	if (armysecond[a2] >= 8)
		a2eyes += 8;
	if (armybestpot[a2] >= 8)
		a2pot = 8;

	a1ko = semeailibs(a1, &l1min, &l1max, &l1typ, &l1hemin, &l1hemax, &l1memin, &l1memax);
	a2ko = semeailibs(a2, &l2min, &l2max, &l2typ, &l2hemin, &l2hemax, &l2memin, &l2memax);
	
	/* both have too many liberties or running potential to catch */
	cantcatch1 = armyrn_pot[a1] + armycnrn_pot[a1] >= SEMEAI_RUN ||
		 (l1min & LIBMASK) >= CANTCATCHLIBS ||
		 (l1typ & LIBMASK) >= CANTTELLLIBS || a1eyes >= 16;
	cantcatch2 = armyrn_pot[a2] + armycnrn_pot[a2] >= SEMEAI_RUN ||
		 (l2min & LIBMASK) >= CANTCATCHLIBS  ||
		 (l2typ & LIBMASK) >= CANTTELLLIBS || a2eyes >= 16;

	lcom = 0;	/* 4/01 better lcom alg */
	for (ptr = armylbp[a1]; ptr != EOL; ptr = link[ptr]) {
		if (inlist(list[ptr],&armylbp[a2]))
			lcom++;   /* same as old code */
		else { 
			for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (!inlist(list[ptr2], &armylbp[a1]) && inlist(list[ptr2], &armylbp[a2])) {
					lcom++;  /* new better test, 5/01 */
					break;
				}
			}
		}
	}

	a1g2libs = TRUE;
	for (ptr = armygroups[a1]; ptr != EOL; ptr = link[ptr]) {
		if (grlibs[list[ptr]] != 2) {
			a1g2libs = FALSE;
			break;
		}
	}

	if (l1min >= CANTAPPROACH && lcom)
		return 5;	/* a1 can't be put into atari */

	/* maybe can still get a seki if he needs to make approach moves */
	if (l2min >= CANTAPPROACH && !esc2 && !cantcatch2) {
		if (a1eyes < 8 && a2eyes < 8 && lcom >= 2 ||
			a1eyes >= 8 && a2eyes >= 8 && lcom >= 1) {

			for (ptr = armylbp[a1]; ptr != EOL; ptr = link[ptr])
				if (inlist(list[ptr],&armylbp[a2]) && lnbn[list[ptr]] == 0)
					lapr += approachneeded(a1, list[ptr], grcolor[list[armygroups[a2]]], NOGROUP, NOGROUP);
			/* every approach move gives me a chance to make an extra move to fill a liberty */
				/* use l2max since assume a2 moves first */
			if ( a1eyes < 8 && (l2max & LIBMASK)-lapr <= l1min+lcom-2 && lcom > 1 && 
				armybestpot[a2] < 8)  /* 8/01, can't be seki if opponent can make an eye */
				return 5;  /* seki (at least for now) l2 can't bring l1 into atari*/
			if (a1eyes >= 8 && (l2max & LIBMASK)-lapr <= l1min+lcom-1 && lcom)
				return 5;  /* seki (at least for now) l2 can't bring l1 into atari*/
			if (/*armylibs[a1]*/ a1g2libs && lcom == 2 && (l2max & LIBMASK)-lapr < l1max+lcom &&	/* 11/08 seki can have extra libs if all groups have 2 libs */
				A_ALIVE(a1) != DEAD && !A_THREATENED(a1))
				return 5;
			}
		}

	/* maybe can still get a seki if I move first */
	if (l2min >= CANTAPPROACH && !esc2 && !cantcatch2) {
		if (a1eyes < 8 && a2eyes < 8 && lcom >= 2 ||
			(a1eyes >= 8 || A_THREATENED(a1) && armyeyepotential[a1] >= 8) && a2eyes >= 8 && lcom >= 1) {

			for (ptr = armylbp[a1]; ptr != EOL; ptr = link[ptr])
				if (inlist(list[ptr],&armylbp[a2]) && lnbn[list[ptr]] == 0)
					lapr += approachneeded(a1, list[ptr], grcolor[list[armygroups[a2]]], NOGROUP, NOGROUP);
			/* if one more liberty, a1 can make a seki if it moves first */
			if (a1eyes < 8 && (l2min & LIBMASK)-lapr <= l1min+lcom-1 && lcom > 1 && 
				armybestpot[a2] < 8)  /* 8/01, can't be seki if opponent can make an eye */
				return 2;  /* can make seki (at least for now) l2 can't bring l1 into atari*/
			if ((a1eyes >= 8 || A_THREATENED(a1) && armyeyepotential[a1] >= 8) && (l2min & LIBMASK)-lapr <= l1min+lcom && lcom)
				return 2;  /* can make seki (at least for now) l2 can't bring l1 into atari*/
			if (armylibs[a1] == 2 && lcom == 2 && (l2min & LIBMASK)-lapr == l1max+lcom &&
				A_ALIVE(a1) != DEAD && !A_THREATENED(a1))
				return 2;
			}
		}

	if (cantcatch1 && cantcatch2)
		return 6; 

	if (cantcatch2 && l2typ > l1typ && l2typ >= CANTCATCHLIBS)
		return 4;
	if (armyrn_pot[a2] + armycnrn_pot[a2] >= SEMEAI_RUN) {
		if (l1max < CANTESCAPELIBS && l2min > l1max)
			return 4;  /* running group wins */
		return 3;  /* can't capture if can run away */
	}

	          /* too many liberties to catch */
	if (cantcatch2 && l1max < CANTESCAPELIBS)
		return 4;
	if (cantcatch1 && l2max < CANTESCAPELIBS) 
		return 0;
	if (cantcatch2)
		return 3;
	if (cantcatch1)
		return 1;

	if (a1eyes < 8 && a2eyes < 8 ||
	   a1eyes >= 8 && a2eyes >= 8) {  /* equal eyes case */
		if (lcom == 0 || lcom == 1 && a1eyes < 8) { /* cant be seki */
			if ( a2ko == 0 && l1min > l2max && l2max < CANTESCAPELIBS+(l1min-l2max)/2 &&
				l1min > l2min /* + 1 4/01 not right for simple fights */ && !esc2 && !cantcatch2 && a2pot == 0)
				return 0;
			if (l1min > l2min && l1max > l2max && !esc2 && !cantcatch2)return 1;
			if (a1ko == 0 && l2min > l1max && l1max < CANTESCAPELIBS+(l2min-l1max)/2 &&
				l2min > l1min+1 && !esc1 && !cantcatch1 && a1pot == 0)return 4;
			if (l2min > l1min && l2max > l1max && !esc1 && !cantcatch1)return 3;
			if (l1typ > l2typ + AHEADLIBS && 
				l2typ <= CANTESCAPELIBS &&
				!cantcatch2)
				return 1;
			if (l2typ > l1typ + AHEADLIBS && 
				l1typ <= CANTESCAPELIBS &&
				!cantcatch1)
				return 3;
			if (l1typ > l2typ + AHEADLIBS &&
				l1max > l2max + AHEADLIBS && !cantcatch2)
				return 1;
			if (l2typ > l1typ + AHEADLIBS &&
				l2max > l1max + AHEADLIBS && !cantcatch1)
				return 3;
			if (armyrn_pot[a1] > 3 && armyrn_pot[a2] + armycnrn_pot[a2] == 0 && 
				l1typ > l2typ)
				return 1;
			if (armyrn_pot[a2] > 3 && armyrn_pot[a1] +armycnrn_pot[a1] == 0 && 
				l2typ > l1typ)
				return 3;
			return 2; 
		}
		if (a2ko == 0 && l1min > l2max+lcom && l2max < CANTESCAPELIBS && l1min > l2min+lcom+1 &&
		   !esc2 && !cantcatch2 && a2pot == 0)
		   return 0;
		if (a1ko == 0 && l2min > l1max+lcom && l1max < CANTESCAPELIBS && l2min > l1min+lcom+1 &&
		   !esc1 && !cantcatch1 && a1pot == 0)return 4;

		if (!esc2 && !cantcatch2 && a1eyes < 8 && l2max <= l1min+lcom-2 && lcom >= 2 && 
			armybestpot[a2] < 8)  /* 8/01, can't be seki if opponent can make an eye */
			return 5;  /* seki (at least for now) l2 can't bring l1 into atari*/
		if (!esc2 && !cantcatch2 && a1eyes >= 8 && l2max <= l1min+lcom-1 && lcom)
			return 5;  /* seki (at least for now) l2 can't bring l1 into atari*/
		if (!esc2 && !cantcatch2 && armylibs[a1] == 2 && lcom == 2 && l2max < l1max+lcom &&
			A_ALIVE(a1) != DEAD && !A_THREATENED(a1))
			return 5;

		/* 5/03 use l2min, not max for graded1c problem 56 */
		if (!esc2 && !cantcatch2 && a1eyes < 8 && l2min-1 == l1min+lcom-2 && lcom >= 2)
			return 7;  /* l1 can make seki l2 can't bring l1 into atari*/
		if (!esc2 && !cantcatch2 && a1eyes >= 8 && l2min-1 == l1min+lcom-1 && lcom)
			return 7;  /* l1 can make seki l2 can't bring l1 into atari*/

		if (l1min > l2min && l1typ > l2typ && 
			l1max >= l2max+lcom && !esc2 && !cantcatch2)
			return 1;
		if (l2min > l1min && l2typ > l1typ && 
			l2max >= l1max+lcom && !esc1 && !cantcatch1)
			return 3;
		if (l1typ > l2typ + AHEADLIBS && l2max <= CANTESCAPELIBS &&
			!cantcatch2)
			return 1;
		if (l2typ > l1typ + AHEADLIBS && l1max <= CANTESCAPELIBS &&
			!cantcatch1)
			return 3;
		if (armyrn_pot[a1] > 3 && armyrn_pot[a2] == 0 && l2max <= CANTESCAPELIBS &&
			l1typ > l2typ)
			return 1;
		if (armyrn_pot[a2] > 3 && armyrn_pot[a1] == 0 && l1max <= CANTESCAPELIBS &&
			l2typ > l1typ)
			return 3;
		if (l1typ > l2typ + AHEADLIBS &&
			l1max > l2max + AHEADLIBS)
			return 1;
		if (l2typ > l1typ + AHEADLIBS &&
			l2max > l1max + AHEADLIBS)
			return 3;
		if (!esc2 && !cantcatch2 && armylibs[a2] == 2 && lcom && l2max == l1max+lcom)
			return 7;
		if (!esc2 && !cantcatch2 && armylibs[a1] == 2 && lcom == 2 && l2max == l1max+lcom &&
			A_ALIVE(a1) != DEAD && !A_THREATENED(a1))
			return 7;
		return 2;  /* can't tell how it will come out */
	}

	if (a1eyes >= 8 && a2eyes < 8) {  
		if (a2ko == 0 && l1min > l2max - lcom && l2max < CANTESCAPELIBS+(l1min-l2max+lcom)/2 &&
			l1min > l2min-lcom+1 && !esc2 && !cantcatch2) {
			if (armybestpot[a2] >= 8 && l2max+lcom >= l1min)
				return 5;	/* he might make a seki with his eye */
			else
				return 0;
		}
		if (!esc2 && !cantcatch2 && l2max < l1min+lcom && lcom /* 4/01 put back or can't be seki && lcom 99 graaded1c problem 58 */)  /* actually, can't be a seki */
			return 5;  /* at least a seki since a2 can't bring a1 into atari */
		if (l1min > l2min-lcom && 
			l1typ > l2typ-lcom && 
			l1max > l2max-lcom && !cantcatch2)
			return 1;
		if (a1ko == 0 && l2min - lcom > l1max && l1max < CANTESCAPELIBS+(l2min-lcom-l1max)/2 &&
			l2min-lcom > l1min+1 && !esc1 && !cantcatch1)return 4;
		if (l2min - lcom > l1min && 
			l2typ - lcom > l1typ && 
			l2max - lcom > l1max && !cantcatch1)
			return 3;
		if (l1typ > l2typ - lcom + AHEADLIBS && l2max <= CANTESCAPELIBS &&
			!cantcatch2)
			return 1;
		if (l2typ - lcom > l1typ + AHEADLIBS && l1max <= CANTESCAPELIBS &&
			!cantcatch1)
			return 3;
		if (armyrn_pot[a1] > 3 && armyrn_pot[a2] == 0 && l2max <= CANTESCAPELIBS &&
			l1typ > l2typ-lcom)
			return 1;
		if (armyrn_pot[a2] > 3 && armyrn_pot[a1] == 0 && l1max <= CANTESCAPELIBS && 
			l2typ > l1typ-lcom)
			return 3;
		if (l1typ > l2typ - lcom + AHEADLIBS &&
			l1max > l2max - lcom + AHEADLIBS)
			return 1;
		if (l2typ - lcom  > l1typ + AHEADLIBS &&
			l2max - lcom  > l1max + AHEADLIBS)
			return 3;
		return 2;
	}
	if (a1eyes < 8 && a2eyes >= 8) {
		if (a2ko == 0 && l1min - lcom > l2max && l2max < CANTESCAPELIBS+(l1min-lcom-l2max)/2 &&
			l1min - lcom > l2min+1 && !esc2 && !cantcatch2)return 0;
		if (l1min - lcom > l2min && 
			l1typ - lcom > l2typ && 
			l1max-lcom > l2max && !cantcatch2)
			return 1;
		if (l1typ - lcom > l2typ + AHEADLIBS && l2max <= CANTESCAPELIBS &&
			!cantcatch2)
			return 1;
		if (armyrn_pot[a1] > 3 && armyrn_pot[a2] == 0 && l2max <= CANTESCAPELIBS && 
			l1typ-lcom > l2typ)
			return 1;
		if (l1typ - lcom > l2typ + AHEADLIBS &&
			l1max - lcom  > l2max + AHEADLIBS)
			return 1;
		if (!esc2 && !cantcatch2 && lcom >= 1 && armybestpot[a1] >= 8 &&
			l1min + lcom >= l2min)
			return 7;
		if (a1ko == 0 && l2min  > l1max-lcom && l1max < CANTESCAPELIBS+(l2min-l1max+lcom)/2 &&
			l2min > l1min-lcom+1 && !esc1 && !cantcatch1 && armybestpot[a1] < 8)return 4;
		if (a1ko == 0 && l2min  > l1max && l1max < CANTESCAPELIBS+(l2min-l1max+lcom)/2 &&
			l2min > l1min+1 && !esc1 && !cantcatch1 && armybestpot[a1] >= 8 && armybestpot[a1] < 16)
			return 4;
		if (l2min  > l1min-lcom && l2typ > l1typ-lcom && 
			l2max > l1max-lcom && !cantcatch1)
			return 3;
		if (l2typ > l1typ - lcom + AHEADLIBS && l1max <= CANTESCAPELIBS &&
			!cantcatch1)
			return 3;
		if (armyrn_pot[a2] > 3 && armyrn_pot[a1] == 0 && l1max <= CANTESCAPELIBS && 
			l2typ > l1typ-lcom)
			return 3;
		if (l2typ > l1typ - lcom  + AHEADLIBS &&
			l2max  > l1max - lcom  + AHEADLIBS)
			return 3;
		return 2;
	}

	return 2;
}


