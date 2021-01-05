/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2hd.h"
#ifdef G2DEBUGOUTPUT
#include <stdio.h>
#endif


/* how has eye potential increased for eye at rn with move at sn?
 * return range of increase in pot and *max, and potential if another move here in pmax (4 for 8 extra potential)
 * with new points in rn in *counted.
 * color is color of enemy eyes 
 */

static int increasedpot(sqr_t sn, int rn, int color, list_t *counted, int *pmax, int *max)
{
	int pot = 0;
	int ignore[2]; /* two best eyes I can ignore since min==val */
	eye_t rn2;
	list_t ptr;
	*pmax = 0;
	*max = 0;

	/* sum up the eyeval after the move, and then subtract the old eyeval */
	ignore[0] = ignore[1] = 0;
	for (ptr = eyeptr[rn]; ptr != EOL; ptr = link[ptr]) {
		if ((sqr_t)list[ptr] == sn)
			continue;
		if (board[list[ptr]] == NOGROUP && lnbf[list[ptr]][1-color] == 0)
			continue;
		if (!inlist(list[ptr],counted)) {
			rn2 = (eye_t)gtflist(&eyefreelist);
			if (rn2 == G2ERROR) {
#ifdef CHECK				
				outerror("Out of eye records");
#endif				
				return pot;
			}
			evaleye(list[ptr], rn2, (listval_t)(NUMGROUPS+NUMCONNS+rn), FALSE, EYEVITLIBS);
			if (eyeval[rn2] == 7)
				eyeval[rn2] = 8;
			if (eyemin[rn2] == eyeval[rn2]) {
				if (eyepot[rn2]-eyeval[rn2] > ignore[0]) {
					ignore[1] = ignore[0];
					ignore[0] = eyepot[rn2]-eyeval[rn2];
				}
				else if (eyepot[rn2]-eyeval[rn2] > ignore[1])
					ignore[1] = eyepot[rn2]-eyeval[rn2];
			}
			pot += eyeval[rn2];  /* does this vital point work? */
			*max += eyevalmax[rn2];
			if (eyepot[rn2] >= eyevalmax[rn2]+7)
				*pmax += 4;
			mrglist(eyeptr[rn2],counted);
			delete_eye(rn2);
		}
	}
	pot -= eyeval[rn];
	*max -= eyeval[rn];
	pot += ignore[1];	/* don't have to respond, so can get extra value for one eye */
	*max += ignore[1];
	if (ignore[1] >= 7)
		*pmax -= 4;		/* don't double count pmax and ignore */
#ifdef NEVER
	if (pot < 0)
		pot = 0;  /* return can be negative if two former eyes are
		           * included in a single new eye.  one's eye gets subtracted
		           * from new eye, and other will return negative value
		           */
#endif
	return pot;
}




/* how much extra eyespace for army do eyes get with move at sn 
 * color is color of the eye in question 
 * eyepoints is all points in all eyes for this army 
 * max returns the maximum value of extra eyespace 
 *
 * find all eyes that have this vital point,
 * make the move, find the new values for each of those eyes
 * add values for any new eyes created
 * subtract values for any eyes that don't have this vital point and are included in the new eyes
 */
#define MAXSAVE 50
int howmuchvital(sqr_t sn, army_t army, int color, list_t eyepoints,int *max)
{
	sqr_t sn2;
	eye_t rn=0, rn2;
	int flag,pot,bigeyebon,tld,tpot,mpot,pmax, tmax, rmmax = 0;
	list_t tmplist= EOL;
	list_t counted = EOL;		// all points that have been included in this analysis so far
	list_t ptr, ptr2, ptr3, othereyes = EOL;
	list_t eyescounted = EOL;
	int val, i;
	int oldalive[MAXSAVE];
	int oldthr[MAXSAVE];
	int oldgroup[MAXSAVE];
	int nextold = 0;
	group_t g;
	sqr_t move;
#ifdef CHECK	
	char buf[100];
#endif	

	if (board[sn] != NOGROUP) {
#ifdef CHECK
		outerror("Nonempty vital point at ");
		outerror(ssqr(sn,buf));
		outerror("\n");
#endif
		return 0;
	}

	val = EYEVALNONE;
	*max = 0;
	bigeyebon = 0;

	mvs[msptr] = sn;
	mvcolor[msptr] = color;
	grarmy[maxgr] = NOARMY;	/* in case lupdate makes a new group, it must be NOARMY so evalverybigeye expansion works */
	flag = lupdate(msptr);  /* put down stone (doesn't update ld[] or lgr[], or ltrgd[]) so not entirely accurate */
	if (!flag || grlibs[board[sn]] == 0) {
		ldndate(msptr);
		return(0);
	}
	++msptr;

	/* make any one liberty groups threatened */
	g = S_GROUP(sn);
	if (grlibs[g] == 1 && gralive[g] != DEAD && !grthreatened[g]) {
		oldgroup[nextold] = g;
		oldalive[nextold] = gralive[g];
		oldthr[nextold] = grthreatened[g]; 
		grthreatened[g] = 2;
		nextold++;
	}

	for (ptr2 = grnbp[g]; ptr2 != EOL; ptr2 = link[ptr2]) {
		if (grlibs[list[ptr2]] == 1 && gralive[list[ptr2]] != DEAD && !grthreatened[list[ptr2]] && nextold < MAXSIZE) {
			oldgroup[nextold] = list[ptr2];
			oldalive[nextold] = gralive[list[ptr2]];
			oldthr[nextold] = grthreatened[list[ptr2]]; 
			grthreatened[list[ptr2]] = 2;
			nextold++;
		}
	}

	/* set all of the threatened groups that this move kills to dead */
	for (ptr2 = eyevitrec[sn]; ptr2 != EOL; ptr2 = link[ptr2]) {
		rn = (eye_t)(list[ptr2]&EYEPOINTMASK); /* eyes that this is vital point of */
		if (eyecolor[rn] != color)
			continue;	/* don't count opponent's eyes */
		if (eyetype[rn] != THRTEYE)
			continue;
		if (nextold >= MAXSAVE-1)
			continue;
		g = S_GROUP(list[eyeptr[rn]]);
		if (g == NOGROUP || G_COLOR(g) == color)
			continue;		/* already captured the group or friendly group */
		if (sn == grcapmove[g] &&
			grthreatened[g] > 1 ||  /* can be captured for sure */
			grlibs[g] <= EYEVITLIBS && iscaptured(g, vitalcapdepth[playlevel], vitalcapsize[playlevel],
				EYEVITLIBS, eyemost[playlevel], 1-color, NOGROUP, &move, 1-color)) {
			oldgroup[nextold] = g;
			oldalive[nextold] = gralive[g];
			oldthr[nextold] = grthreatened[g];
			nextold++;
			gralive[g] = DEAD;
			grthreatened[g] = 0;
		}
	}

	/* set all the groups that this move saves to not threatened */
	for (ptr2 = eyevitrec[sn]; ptr2 != EOL; ptr2 = link[ptr2]) {
		rn = (eye_t)(list[ptr2]&EYEPOINTMASK); /* eyes that this is vital point of */
		if (eyecolor[rn] != 1-color)
			continue;	
		if (eyetype[rn] != THRTEYE)
			continue;
		if (nextold >= MAXSAVE-1)
			continue;
		g = S_GROUP(list[eyeptr[rn]]);
		if (g == NOGROUP || G_COLOR(g) == 1-color)
			continue;		/* already captured the group or friendly group */
		if (sn == grsavemove[g]) {
			oldgroup[nextold] = g;
			oldalive[nextold] = gralive[g];
			oldthr[nextold] = grthreatened[g];
			nextold++;
			gralive[g] = 13;
			grthreatened[g] = 0;
		}
	}

	pot = 0;

	/* first do the increase in eyes that have this vital point */
	for (ptr2 = eyevitrec[sn]; ptr2 != EOL; ptr2 = link[ptr2]) {
		rn = (eye_t)(list[ptr2]&EYEPOINTMASK); /* eyes that this is vital point of */
		if (eyecolor[rn] != color)
			continue;	/* don't count opponent's eyes */
		if (!inlist(rn, &armyeyerecs[army]))  /* only get extra pot for my own eyes */
			continue;
		addlist(rn,&eyescounted);
		tpot = increasedpot(sn, rn, 1-color, &counted, &pmax, &mpot);
		pot += tpot;  /* only get extra potential if make eye better too */
		tmax = 0;
		tmax += mpot;
		tmax += pmax;
		if ((list[ptr2]&EYERMONLY) && eyepot[rn]-eyeval[rn] > rmmax) 
			rmmax = eyepot[rn]-eyeval[rn];     /* allow remove to get full value, even if add here doesn't get much */
		*max += tmax;
	}
	if (rmmax > *max - pot)
		*max = pot + rmmax;


	/* then increase in pot for other eyes that might now be mine (add to max only) */
	for (ptr2 = eyevitrec[sn]; ptr2 != EOL; ptr2 = link[ptr2]) {
		rn = (eye_t)(list[ptr2]&EYEPOINTMASK); /* eyes that this is vital point of */
		if (eyecolor[rn] != color)
			continue;	/* don't count opponent's eyes */
		if (inlist(rn, &armyeyerecs[army]))  /* only get extra pot for my own eyes */
			continue;
		addlist(rn,&eyescounted);
		tpot = increasedpot(sn, rn, 1-color, &counted, &pmax, &mpot);
		if (tpot > 0)
			*max += tpot;  /* 5/03 must sum here so get the same answer if two eyes, on inlist and one not, indepent of eval order */
	}

	/* subtract off the prior value of each of these eyes */
	for (ptr = counted; ptr != EOL; ptr = link[ptr]) {
		if (eyerec[list[ptr]] != 0 &&
			!inlistm(eyerec[list[ptr]],&eyevitrec[sn],EYEPOINTMASK) && 
			addlist(eyerec[list[ptr]],&othereyes)) {
			pot -= eyeval[eyerec[list[ptr]]];
			*max -= eyeval[eyerec[list[ptr]]];
		}
	}

	killist(&othereyes);                     
	cpylist(nblbp[sn],&tmplist); /* new eyes in points adjacent to sn */
	for (ptr3 = tmplist; ptr3 != EOL; ptr3 = link[ptr3]) {
		sn2 = list[ptr3]; /* where new eye will be */
		if (lnbf[sn2][1-color] != 0)
			continue;
		if (inlist(sn2,&counted))
			continue;
		if (eyerec[sn2] != 0) {
			if (inlistm(sn2, &eyevital[eyerec[sn2]], EYEPOINTMASK))
				continue;  /* will be counted together in bestpot() */
		}
		tld = getldval(sn2);
		if (tld >= 5 && 
			inlist(sn2, &armylbp[army])) {  /* 4/99 must add eye to this group, not some other one */
			rn2 = (eye_t)gtflist(&eyefreelist);
			if (rn2 == G2ERROR) {
#ifdef CHECK
				outerror("Out of eye records");
#endif
				break;
			}				
			evaleye(sn2, rn2, (listval_t)(NUMGROUPS+NUMCONNS+rn), FALSE, EYEVITLIBS);
			if (!comlist(eyepoints,eyeptr[rn2])) {
				pot += eyeval[rn2];
				*max += eyevalmax[rn2];
				/* if (eyepot[rn2] >= eyeval[rn2] + 8)pot += 4; 5/03 - makes it prefer to threaten to make an eye over filling a ko to make a real eye*/
				if (eyepot[rn2] >= eyevalmax[rn2] + 8)
					*max += 4;
			}
			else if (eyerec[sn2] != 0 && eyeval[rn2] > eyeval[eyerec[sn2]]) {
				pot += eyeval[rn2]-eyeval[eyerec[sn2]];
				*max += eyevalmax[rn2]-eyeval[eyerec[sn2]];
			}
			mrglist(eyeptr[rn2],&counted);
			delete_eye(rn2);
		}
	}
	for (ptr3 = tmplist; ptr3 != EOL; ptr3 = link[ptr3]) {
		sn2 = list[ptr3]; /* where vital point is */
		for (ptr2 = eyevitrec[sn2]; ptr2 != EOL; ptr2 = link[ptr2]) {
			rn = (eye_t)(list[ptr2]&EYEPOINTMASK); /* eyes that this is vital point of */
			if (eyecolor[rn] != color)
				continue;
			if (inlist(rn,&eyescounted))
				continue;
			addlist(rn,&eyescounted);
			if (inlist(rn, &armyeyerecs[army])) {  /* only get extra pot for my own eyes */
				pot += increasedpot(sn2, rn, 1-color, &counted, &pmax, &mpot);
				*max += mpot;
			}
		}
	}

	killist(&tmplist);
	killist(&counted);
	killist(&eyescounted);
	--msptr;
	ldndate(msptr);
	for (i = nextold-1; i >= 0; --i) {  /* reverse order in case a group was changed twice */
		g = oldgroup[i];
		gralive[g] = oldalive[i];
		grthreatened[g] = oldthr[i];
	}

	if (pot < 0)
		pot = 0;
	val = pot;
	val += bigeyebon;
	*max += bigeyebon;
	if (*max < val)
		*max = val;
	
	return(val);
}


/* get eye potential for playing on a vital point
 * if the vital point is for an eye or eyes, they get the potential
 * playing in the vital point can add territory.
 * playing in the vital point can add more eyespace
 * if there are multiple vital points for the same eye, only count the best one
 */

extern listval_t nextpot;

#ifdef G2DEBUGOUTPUT

void testtv_pot(army_t army, sqr_t s) {
	sqr_t sn;
	char buf[80];
	int max;
	int color,rn;
	int val,eyes;
	list_t eyepoints = EOL, ptr, ptr2;
	eyes = armyeyespace[army];
	color = grcolor[list[armygroups[army]]];  /* enemy color */
	for (ptr2 = armyeyerecs[army]; ptr2 != EOL; ptr2 = link[ptr2])
			if (eyetype[list[ptr2]] != THRTEYE &&
			   eyetype[list[ptr2]] != DEADEYE &&
			   eyeval[list[ptr2]] >= 8)  /* list of all points in eyes already */
				mrglist(eyeptr[list[ptr2]],&eyepoints);
	for (ptr = armyeyerecs[army]; ptr != EOL; ptr = link[ptr]) {
		rn = list[ptr];
		if (eyevital[rn] == EOL)continue;
		/* if (eyeval[rn] == eyepot[rn])continue; can't do this since potential might be more than eyepot! */

		for (ptr2 = eyevital[rn]; ptr2 != EOL; ptr2 = link[ptr2]) {
			sn = list[ptr2]&EYEPOINTMASK;
			if (sn != s) continue;
			val = howmuchvital(sn, army, color, eyepoints, &max);
			sprintf(buf, "potential is %d-%d", val, max);
			outerr(buf);
			break;
			}
		}
	killist(&eyepoints);
}

#endif

/* how much additional eye space does a group get by playing on a vital point
 */

void getarmytv_pot(army_t army) {
	sqr_t sn;
	int max, ptmp;
	int color,rn;
	int val,eyes,capture;
	int firstpot = nextpot;	/* first pot record for this army */
	list_t vitalpoints = EOL, eyepoints = EOL, ptr, ptr2, ptr3;
	eyes = armyeyespace[army];
	color = grcolor[list[armygroups[army]]];  /* enemy color */
	for (ptr2 = armyeyerecs[army]; ptr2 != EOL; ptr2 = link[ptr2])
			if (eyetype[list[ptr2]] != THRTEYE &&
			   eyetype[list[ptr2]] != DEADEYE &&
			   eyeval[list[ptr2]] >= 8)  /* list of all points in eyes already */
				mrglist(eyeptr[list[ptr2]],&eyepoints);

	for (ptr = armyeyerecs[army]; ptr != EOL; ptr = link[ptr]) {
		rn = list[ptr];

		for (ptr2 = eyevital[rn]; ptr2 != EOL; ptr2 = link[ptr2]) {
			sn = list[ptr2]&EYEPOINTMASK;
			if (inlistm(sn, &vitalpoints, EYEPOINTMASK)) {
				if (inlist(list[ptr2], &vitalpoints))
					continue;  /* already evaluated - exact match*/
				/* remove the flag if there are two diffferent flag values */
				for (ptmp = firstpot; ptmp < nextpot; ptmp++)
					if (pots[ptmp].pot_type == VITAL && (pots[ptmp].pot_where&EYEPOINTMASK) == sn &&
						pots[ptmp].pot_where != list[ptr2])
						pots[ptmp].pot_where &= EYEPOINTMASK;
				continue;
			}
			if (armylibs[army] == 1 && list[armylbp[army]] != sn) {
				capture = FALSE;
				for (ptr3 = grnbp[list[armygroups[army]]]; ptr3 != EOL; ptr3 = link[ptr3])
					if (grlibs[list[ptr3]] == 1 && list[grlbp[list[ptr3]]] == sn) {
						capture = TRUE;
						break;
						}
				if (capture) {
					val = howmuchvital(sn, army, color, eyepoints, &max);
					}
				else {
					val = 0;	/* can't count eye if group will be captured */
					max = 0;
					}
			}
			else
				val = howmuchvital(sn, army, color, eyepoints, &max);
			addlist(list[ptr2], &vitalpoints);  /* with the rm, add flags */
			if (nextpot < NUMPOTENTIAL-1) {
				adflist(nextpot, &armypot[army]);
				pots[nextpot].pot_type = VITAL;
				pots[nextpot].pot_val = val;
				if (max >= val)
					pots[nextpot].pot_max = max;
				else
					pots[nextpot].pot_max = val;
				pots[nextpot++].pot_where = list[ptr2];  /* 1/10/99, with rm, add flags */
				}
#ifdef CHECK
			else
				outerror("vt out of pots!\n");
#endif
			}
		}
	killist(&eyepoints);
	killist(&vitalpoints);
	}
