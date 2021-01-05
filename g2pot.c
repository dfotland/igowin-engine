/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

# include "g2hd.h"

static list_t stopuc(army_t army, sqr_t s, int rm);
static list_t blockextend(army_t army, sqr_t s);
list_t rmconnect(army_t army, conn_t cn);
static sqr_t saveadjacent(sqr_t s, army_t army);
static void extendforeyes(army_t,int,list_t *);
static void findblock(sqr_t,sqr_t,list_t *,int,int);
static list_t connectonepoint(sqr_t s, conn_t cn);

extern sqr_t brddir[4];
extern int numpotmoves[NUMLEVELS];  /* number of moves to read */

/* dir islong direction from s1 to s2, dir2 is short direction from s1 to s2 */

void adknight(sqr_t s1, sqr_t s2, sqr_t dir, sqr_t dir2, int c, list_t *rlist) {
	list_t ptr;
	adflist((sqr_t)(s1+dir),rlist);
	adflist((sqr_t)(s1+dir+dir2),rlist);
	if (lnbf[s1+dir][1-c] == 0 && S_NUMLIBS(s1+2*dir) > 3)
		adflist(s1,rlist);
	if (lnbf[s1+dir+dir2][1-c] == 0 && S_NUMLIBS(s1+dir2) > 3)
		adflist(s2,rlist);                                   
	if (lnbf[s1][1-c] == 0 && lnbf[s1+dir][1-c] == 0 && S_NUMLIBS(s1+2*dir) > 3) {
		for (ptr = nblbp[s1]; ptr != EOL; ptr = link[ptr])
			if (lnbn[list[ptr]] == 4)
				adflist(list[ptr],rlist);
		}
	if (lnbf[s2][1-c] == 0 && lnbf[s1+dir+dir2][1-c] == 0 && S_NUMLIBS(s1+dir2) > 3) {
		for (ptr = nblbp[s2]; ptr != EOL; ptr = link[ptr])
			if (lnbn[list[ptr]] == 4)
				adflist(list[ptr],rlist);
		}
	}

/* find all moves that make the connection cn to army */

list_t addconnpoints(army_t army, conn_t cn)
{
	list_t tmplist=EOL, ptr;
	list_t rlist = EOL;
	int c, neutral, xv, yv, delta, dir;
	group_t g, g2;
	sqr_t s, sn, s1, s2;

	switch (cntype[cn]) {
	case  CN_THREAT:
		adflist(list[cnptr[cn]], &rlist);
		andlist(grnbp[cngr1[cn]], grnbp[cngr2[cn]], &tmplist);
		for (ptr = tmplist; ptr != EOL; ptr = link[ptr]) {
			if (G_THREATENED(list[ptr]) && !inlist(list[cnptr[cn]],&grlbp[list[ptr]]))
				adflist(grcapmove[list[ptr]], &rlist);
		}
		killist(&tmplist);
		break;
	case CN_HANE:
		c = grcolor[list[armygroups[army]]];
		s = list[cnptr[cn]];
		if (!G_THREATENED(cngr1[cn]) && !G_THREATENED(cngr2[cn]) &&
		   lnbf[s][c] == 2 && lnbf[s][1-c] == 0)
			mrglist(nblbp[s], &rlist);
		cpylist(cnptr[cn], &tmplist);
		g = cngr1[cn];
		g2 = cngr2[cn];
		if (G_ARMY(g) != army) {
			g = cngr2[cn];	/* g is the side of army */
			g2 = cngr1[cn];
		}
		if (G_THREATENED(g) && 
			grsavemove[g] != NOSQUARE &&
			grsavemove[g] != PASS)
			adflist(grsavemove[g], &rlist);
		if (G_THREATENED(g) != 2 && G_THREATENED(g2) && 
			grsavemove[g2] != NOSQUARE &&
			grsavemove[g2] != PASS)
			adflist(grsavemove[g2], &rlist);
		catlist(&tmplist,&rlist);
		s = saveadjacent(list[cnptr[cn]], army);
		if (s != NOSQUARE)
			adflist(s, &rlist); /* save adjacent stone */
		break;
	case CN_ONEPOINTJUMP:
		cpylist(cnptr[cn], &rlist);
		g = cngr1[cn];
		g2 = cngr2[cn];
		if (G_ARMY(g) != army) {
			g = cngr2[cn];	/* g is the side of army */
			g2 = cngr1[cn];
		}
		if (G_THREATENED(g) && 
			grsavemove[g] != NOSQUARE &&
			grsavemove[g] != PASS &&
			!inflist(grsavemove[g], &rlist))
			adflist(grsavemove[g], &rlist);
		if (G_THREATENED(g) != 2 && G_THREATENED(g2) && 
			grsavemove[g2] != NOSQUARE &&
			grsavemove[g2] != PASS &&
			!inflist(grsavemove[g2], &rlist))
			adflist(grsavemove[g2], &rlist);
		s = connect_bamboo(cngr1[cn], cngr2[cn], list[cnptr[cn]]);
		if (s != NOSQUARE && !inflist(s, &rlist))
			adflist(s,&rlist); /* bamboo joint connection is best */
		s = saveadjacent(list[cnptr[cn]], army);
		if (s != NOSQUARE && !inflist(s,&rlist))
			adflist(s, &rlist); /* save adjacent stone */
		s = list[cnptr[cn]];
		if (!S_NEUTRAL(s) && grlibs[cngr1[cn]] > 1 && grlibs[cngr2[cn]] > 1 && (grlibs[cngr1[cn]] > 2 || grlibs[cngr2[cn]] > 2)) {
			tmplist = connectonepoint(s, cn);
			catlist(&tmplist, &rlist);
		}
		return rlist;
		break;
	case CN_TWOPOINTJUMP:
		if (S_NEUTRAL(list[cnlkptr[cn]])) {
			adflist(list[link[cnlkptr[cn]]], &rlist);
			adflist(list[cnlkptr[cn]], &rlist); /* neutral one first */
		}
		else if (S_NEUTRAL(list[link[cnlkptr[cn]]])) {
			adflist(list[cnlkptr[cn]], &rlist); /* neutral one first */
			adflist(list[link[cnlkptr[cn]]], &rlist);
		}
		else { /* no neutral one */
			if (ld[list[cnlkptr[cn]]] > 2) {
				adflist(list[link[cnlkptr[cn]]], &rlist);
				adflist(list[cnlkptr[cn]], &rlist); 
			}
			else {
				adflist(list[cnlkptr[cn]], &rlist); 
				adflist(list[link[cnlkptr[cn]]], &rlist);
			}
		}
		neutral = S_NEUTRAL(list[cnlkptr[cn]]) ||
			  S_NEUTRAL(list[link[cnlkptr[cn]]]);
		g = cngr1[cn];
		if (grlibs[g] < 4 && !neutral) {
			for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
				if (lnbn[list[ptr]] > 2 && !inflist(list[ptr], &rlist))
				   adflist(list[ptr], &rlist);
			}
		}
		g = cngr2[cn];
		if (grlibs[g] < 4 && !neutral) {
			for (ptr = grlbp[g]; ptr != EOL; ptr = link[ptr]) {
				if (lnbn[list[ptr]] > 2 && !inflist(list[ptr], &rlist))
				   adflist(list[ptr], &rlist);
			}
		}
		return rlist;
		break;
	case CN_THREEPOINTJUMP:
		threepointjump(cngr1[cn], cngr2[cn], grcolor[cngr1[cn]], cn, list[cnllptr[cn]], list[link[cnllptr[cn]]], &s, NOGROUP);
		if (S_NEUTRAL(list[cnllptr[cn]]))
			adflist(list[cnllptr[cn]], &rlist);
		if (S_NEUTRAL(list[link[cnllptr[cn]]]))
			adflist(list[link[cnllptr[cn]]], &rlist);
		adflist(s, &rlist);
		break;
	case CN_HALFLARGEKNIGHT:
		canconnlkg(cn, &s, &s2, NOGROUP);
		adflist(s, &rlist);
		break;
	case CN_HALFKNIGHT:
		canconnlink(cn, list[cnlkptr[cn]], &s, &s2, NOGROUP);
		adflist(s, &rlist);
		if (s2 != NOSQUARE)
			adflist(s2, &rlist);
		break;
	case CN_LARGEKNIGHT:
		s1 = list[cnllptr[cn]];
		s2 = list[link[cnllptr[cn]]];
		yv = yval[s2]-yval[s1];
		if (yv == 3) {
			s1 += boardsize;
			s2 -= boardsize;
		}
		else if (xval[s1] > xval[s2]) {
			s1--;
			s2++;
		}
		else {
			s1++;
			s2--;
		}
		adflist(s1, &rlist);
		adflist(s2, &rlist);
		break;
	case CN_KNIGHTSMOVE:
		s1 = list[cnlkptr[cn]];
		s2 = list[link[cnlkptr[cn]]];
		xv = xval[s2] - xval[s1];
		c = grcolor[list[armygroups[army]]];
		if (xv == -1) {
			adknight(s1, s2, boardsize, -1, c, &rlist);
		}
		else if (xv == 1) {
			adknight(s1, s2, boardsize, 1, c, &rlist);
		}
		else if (xv == 2) {
			adknight(s1, s2, 1, boardsize, c, &rlist);
		}
		else if (xv == -2) {
			adknight(s1, s2, -1, boardsize, c, &rlist);
		}				
		break;
	case  CN_EXTRALARGEKNIGHT:
		   /*   s1 +  s  +  O   dir ->
			*   O  +  sn +  s2
			*/

		s1 = list[cnollptr[cn]];
		s2 = list[link[cnollptr[cn]]];
		delta = s2 - s1;
		if (delta == boardsize + 4) {
			dir = 1;
		}
		else if (delta == boardsize - 4) {
			dir = -1;
		}
		else {
			dir = boardsize;
		}
		s = s1 + 2 * dir;
		sn = s2 - 2 * dir;
		adflist(s, &rlist);
		adflist(sn, &rlist);
		if (ld[sn-dir] >= 4)
			adflist((listval_t)(s2-dir), &rlist);
		if (ld[s+dir] >= 4)
			adflist((listval_t)(s1+dir), &rlist);
		break;
	case CN_DOUBLEDIAG:
		canconndd(cngr1[cn], cngr2[cn], grcolor[cngr1[cn]], cn, NOGROUP, &rlist);
		if (rlist == EOL)  /* compensate for possible canconndd bug */
			cpylist(cnddptr[cn], &rlist);
		break;
	case CN_DIAGONAL:
		cpylist(cnptr[cn], &rlist);
		for (ptr = cnptr[cn]; ptr != EOL; ptr = link[ptr]) {
			if (lnbn[list[ptr]] == 1 && lnbf[list[ptr]][1-A_COLOR(army)] == 0 &&
				cntplyhere(list[ptr], A_COLOR(army), NOGROUP) == YES) {
				aeflist(list[nblbp[list[ptr]]], &rlist);
			}
		}
		break;
	default:
		if (cntype[cn] != CN_BAMBOOJOINT)
			rlist = rmconnect(army, cn);  /* at least the removing moves */
		break;
	}

	if (G_THREATENED(cngr1[cn]) && 
		grsavemove[cngr1[cn]] != NOSQUARE &&
		grsavemove[cngr1[cn]] != PASS &&
		!inflist(grsavemove[cngr1[cn]], &rlist))
		aeflist(grsavemove[cngr1[cn]], &rlist);
	if (G_THREATENED(cngr2[cn]) && 
		grsavemove[cngr2[cn]] != NOSQUARE &&
		grsavemove[cngr2[cn]] != PASS &&
		!inflist(grsavemove[cngr2[cn]], &rlist))
		aeflist(grsavemove[cngr2[cn]], &rlist);
	return rlist;
}

/* find all the moves that realize a particular eye potential 
 * return list of moves, sorted from best to worst.
 * NOTE THAT THIS LIST IS NOT SORTED BY VALUE!
 * NOTE THAT illegal ko captures may be returned.
 */

list_t adpot(army_t army, int p)
{
#ifdef CHECK
	group_t g;
	int emptyok = FALSE;
#endif
	sqr_t s;
	list_t rlist = EOL, ptr;
	army_t army2;
	switch (pots[p].pot_type) {
	case CONNECT:
		rlist = addconnpoints(army, pots[p].pot_where);
		break;
	case THREAT:
	case POTTHREAT:
		army2 = (army_t)pots[p].pot_where;
		if (playlevel <= 3) {  /* just one capturing move */
			if (armylibs[army2] == 1)
				adflist(list[armylbp[army2]],&rlist);
			else if (grcapmove[list[armygroups[army2]]] != NOSQUARE)
				adflist(grcapmove[list[armygroups[army2]]],&rlist);
			break;
		}
		adthreat(army2,&rlist,numpotmoves[playlevel]);
		break;
	case POTDEAD:
		army2 = (army_t)pots[p].pot_where;
		if (armynbp[army2] == EOL)
			getarmynbp(army2);
		for (ptr = armynbp[army2]; ptr != EOL; ptr = link[ptr]) {
			if (A_THREATENED((army_t)list[ptr]))
				adthreat((army_t)list[ptr], &rlist, numpotmoves[playlevel]);
		}
		break;
	case VITAL:
		if (pots[p].pot_where & EYERMONLY) {
#ifdef CHECK
			emptyok = TRUE;
#endif
			break;	/* only remove here, don't add */
		}
	    s = pots[p].pot_where & EYEPOINTMASK;
		if (board[s] == NOGROUP)
			adflist(s, &rlist);
		else 
			cpylist(grlbp[board[s]], &rlist);
		break;
	case EXTEND:
		extendforeyes(army,p,&rlist);
		break;
	case UNDERCUT:
		rlist = stopuc(army,(sqr_t)pots[p].pot_where,FALSE);
		break;
	default:
		rlist = rmpot(army,p);  /* at least the removing moves */
	}                                                     
#ifdef CHECK
	if (rlist == EOL && pots[p].pot_val > 0 && !emptyok)
		outerror("Adpot returning empty list");
	for (ptr = rlist; ptr != EOL; ptr = link[ptr]) {
		if ((sqr_t)list[ptr] >= boardsquare || 
			board[list[ptr]] != NOGROUP) {
			g = board[list[ptr]];
			outerror("adpot generated illegal move or pass\n");
			turnoffcplay();
		}
	}
#endif	
	return unflist(rlist);
}

/* find connection points for one point jump other than at point s
 * 
 *          A C
 *    + B 3 2 1
 *      O + O D
 *    + + + + +
 *
 *    priortity is:
 *    1 if no stone at B and no stone at A and no friendly stone at D
 *    2 if no stone at B and no friendly stone at D
 *    3
 */

static list_t connectonepoint(sqr_t s, conn_t cn) {
	list_t reslist = EOL, ptr, ptr2;
	sqr_t sn,so,sl,sr,se;
	int c;
	if (S_NEUTRAL(s))
		return(EOL);
	c = grcolor[cngr1[cn]];
	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
		sn = list[ptr];
		if (lnbn[sn] > 1 || lnbf[sn][c] != 0)
			adflist(sn,&reslist);  /* (3) generally worst moves */
		}
	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
		sn = list[ptr];       /* (2) */   
		if (edge[sn] == 1)
			continue;
		so = sn + sn - s;
		if (lnbf[sn][1-c] > 1 || S_COLOR(so) != 1-c && lnbf[sn][1-c])
			continue;  /* no (2) if sr or sl have enemy stone */
		sr = sn;
		sl = sn;
		for (ptr2 = nblbp[sn]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if ((sqr_t)list[ptr2] == so || (sqr_t)list[ptr2] ==s)continue;
			if (sr == sn)sr = list[ptr2];
			else sl = list[ptr2];
			}
		if (S_COLOR(sr) == 1-c || S_COLOR(sl) == 1-c)
			break;    /* oops, this is wrong! sl, sr not on stones */
		if (edge[sr] > 1 && sr != sn) {
			se = (signed)s + sr*2 - sn*2;
			if (S_COLOR(se) != c)
				adflist(sr,&reslist);  /* two stone wall */
			}
		if (edge[sl] > 1 && sl != sn) {
			se = (signed)s + sl*2 - sn*2;
			if (S_COLOR(se) != c)
				adflist(sl,&reslist);  /* two stone wall */
			}
		}
	if (grlibs[cngr1[cn]] <= 2 || grlibs[cngr2[cn]] <= 2)
		return reslist;
	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
		sn = list[ptr];       /* (1) */
		if (edge[sn] == 1)
			continue;
		so = sn + sn - s;
		if (lnbf[sn][1-c] > 1 || S_COLOR(so) != 1-c && lnbf[sn][1-c])
			continue;  /* no (3) if sr or sl have enemy stone */
		sr = sn;
		sl = sn;
		for (ptr2 = nblbp[sn]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if ((sqr_t)list[ptr2] == so || (sqr_t)list[ptr2] ==s)continue;
			if (sr == sn)sr = list[ptr2];
			else sl = list[ptr2];
			}
		if (S_COLOR(sr) == 1-c || S_COLOR(sl) == 1-c)
			break;
		if (edge[sr] > 1 && lnbn[sr] == 3 && sr != sn) {
			if (lnbn[sr+sn-s] == 4)
				adflist((sqr_t)(sr+sn-s),&reslist);       /* jump sideways */
			se = (signed)s + sr*2 - sn*2;
			if (S_COLOR(se) != c)
				adflist((sqr_t)(sr+sr-sn),&reslist);  /* diagonal */
			}
		if (edge[sl] > 1 && lnbn[sl] == 3 && sl != sn) {
			if (lnbn[sl+sn-s] == 4)
				adflist((sqr_t)(sl+sn-s),&reslist);
			se = (signed)s + sl*2 - sn*2;
			if (S_COLOR(se) != c)
				adflist((sqr_t)(sl+sl-sn),&reslist);  /* two stone wall */
			}
		}
	return(reslist);
	}

             
/* find moves that kill threatened army2, and put in rlist, best to worst 
 * numpotmoves is the number of moves to read.  Always include grcapmoves
 * move to capture biggest groups in army merged first
 */             
             
void adthreat(army_t army2, list_t *rlist, int maxmoves) {
	list_t tmplist = EOL, ptr, ptr2, grplist = EOL, movelist = EOL;
	sqr_t mv, s;
	group_t g;
	int count = 0, big, skip = 0, winsko;
	cpylist(armygroups[army2], &grplist);
	while(grplist != EOL && count < maxmoves) {
		big = 0;
		for (ptr = grplist; ptr != EOL; ptr = link[ptr]) {
			if (grsize[list[ptr]] > big) {  /* do biggest groups first */
				g = (group_t)list[ptr];
				big = grsize[g];
				}
			}
		dellist(g, &grplist);
    	if (!G_THREATENED(g))continue;  /* in case threat and nonthreat group in same army! */
		if (G_THREATENED(g) == 2)
			winsko = grcolor[g];	/* if unconditionally captured, only allow unconditional moves */
		else
			winsko = 1-grcolor[g];
    	tmplist = getatkmoves(g);
    	for (ptr2 = eyevital[eyerec[mvs[grpieces[g]]]]; ptr2 != EOL; ptr2 = link[ptr2])
    		if (board[list[ptr2]&EYEPOINTMASK] == NOGROUP &&
    			!inflist((listval_t)(list[ptr2]&EYEPOINTMASK), &tmplist))
    			aeflist((listval_t)(list[ptr2]&EYEPOINTMASK), &tmplist);
    	dlflist(grcapmove[g],&tmplist);
		for (ptr2 = tmplist; ptr2 != EOL && count < maxmoves; ptr2 = link[ptr2]) {
			s = list[ptr2];
			if (inflist(s, rlist))
				continue;	/* already found this good move */
			if (board[s] != NOGROUP || s == grcapmove[g] || s == PASS || s == NOSQUARE)
				continue;
			if (canbecaptured(s,1-grcolor[g], mvs[grpieces[g]], grcolor[g], winsko, 80, conncapsize[playlevel], conntaclibs[playlevel], NOGROUP)) {  /* 5/03 use winsko to limit moves when fight is unconditional */
				adflist(s,&movelist);
				}
			count++;
			}
		killist(&tmplist);
		mv = grcapmove[g]; /* put capture move in front */
		if (mv != NOSQUARE && mv != PASS) {
			adflist(mv,&movelist);
			}
		mrflist(&movelist, rlist, skip);
		skip++;
		}
	killist(&grplist);
	unflist(*rlist);
	}
	
/* find moves that save threatened army2, and put in rlist, best to worst 
 * Always include grsavemove as first move, and try numpotmoves[] more moves for each group
 * in the threatened army
 */             
             
void rmthreat(army_t  army2, list_t *rlist) {
	list_t tmplist = EOL, ptr, ptr2;
	sqr_t mv = NOSQUARE, s;
	group_t g;
	int count;
    for (ptr = armygroups[army2]; ptr != EOL; ptr = link[ptr]) {
    	g = (group_t)list[ptr];
    	if (!G_THREATENED(g))continue;  /* in case army contains both th and non-th groups */
    	tmplist = getdefmoves(g);
    	dlflist(grsavemove[g],&tmplist);
		count = 0;
		for (ptr2 = tmplist; ptr2 != EOL && count < numpotmoves[playlevel]; ptr2 = link[ptr2]) {
			s = list[ptr2];
			if (board[s] != NOGROUP || s == grsavemove[g] || s == PASS || s == NOSQUARE)
				continue;
			if (!canbecaptured(s, grcolor[g], mvs[grpieces[g]], 1 - grcolor[g], NOCOLOR, 80, conncapsize[playlevel], conntaclibs[playlevel],NOGROUP)) {
				aeflist(s,rlist);	/* keep in generated order */
				}
			count++;
			}
		mv = grsavemove[g]; /* put capture move in front */
		if (*rlist == EOL && mv == NOSQUARE)
			*rlist = tmplist;  /* no move to save since lived due to out of nodes */
		else
			killist(&tmplist);
		if (mv != NOSQUARE && mv != PASS) {
			adflist(mv,rlist);
			}                                                   
		}
	}
	

static void extendforeyes(army_t army, int p, list_t *rlist) {
	sqr_t s,so = NOSQUARE,soo,sn;
	int twostonewall = FALSE,c;
	sqr_t dir2;  /* Towards edge of board */
	list_t ptr;
	dir2 = 0;  /* toward edge */
	c = grcolor[list[armygroups[army]]];
	s = pots[p].pot_where;
	if (!S_NEUTRAL(s)) {  /* look for higher liberty */
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
			if (edge[list[ptr]] > edge[s] && board[s] == NOGROUP) {
				if ((ld[list[ptr]] == 3 ||
				    ld[list[ptr]] == 4) && lnbn[list[ptr]] ==3  &&
				   grcolor[lgr[list[ptr]]] == grcolor[lgr[s]] &&
					board[list[ptr]+list[ptr]-s] == NOGROUP) {
					if (edge[list[ptr]] <= 4) {
				   		sn = list[ptr]+list[ptr]-s;
						s = list[ptr];
						}
					else
						sn = s;
					twostonewall = TRUE;
					if ((ld[sn] == 3 ||
					    ld[sn] == 4) && lnbn[sn] == 3  && board[sn] == NOGROUP &&
					   grcolor[lgr[sn]] == grcolor[lgr[s]] && edge[sn] <= 4)
					   	s = sn;
					}
				}
		}
	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
		if (edge[list[ptr]] == edge[s])
			so = list[ptr];
		if (edge[list[ptr]] < edge[s])dir2 = list[ptr]-s;
		if (edge[s] == 1 && edge[list[ptr]] == 2)
			dir2 = s-list[ptr];  /* no liberty below, so find one above */
		}
	if (so == NOSQUARE) {
		adflist(s,rlist);
		return;
		}
	if (S_COLOR(so) == c)return;    /* already friendly stone this way*/
	if (edge[so] > 1 && S_COLOR(so+dir2) == c) {
		if (board[s+dir2] == NOGROUP)
			adflist((sqr_t)(s+dir2),rlist);
		else
			adflist(s,rlist);
		return;  
		}
	      

	if (!S_NEUTRAL(s) && 
		(edge[s] == 1 || S_COLOR(s+dir2) != c) && S_COLOR(s+s-so) == c &&
 		(S_NUMLIBS(s+s-so) <= 2 || 
 		 S_NUMLIBS(s+s-so) == 3 && G_SIZE(S_GROUP(s+s-so)) == 1)) {
		adflist(s,rlist);
		if (S_NUMLIBS(s+s-so) == 3 && G_SIZE(S_GROUP(s+s-so)) == 1)
			adflist(so,rlist);
		return;  /* opponent can atari, just play in liberty */
		}
		
	if (S_NEUTRAL(s)) {
		if (edge[s] > 2 && lnbf[s+dir2][c] != 0 && board[so] == NOGROUP) {
			if (lnbf[s][c] != 0 || lnbn[so] == 4)
				adflist(so,rlist);  /* jump from two stone wall */
			}
		else if (S_COLOR(s+s-so) == c && grlibs[board[s+s-so]] > 4 && 
			edge[s] > 1 && S_COLOR(s+dir2) != 1-c) {
			adflist(s,rlist);
			adflist(so,rlist);
			}
		else
			adflist(s,rlist);
		if (edge[s] > 2 && lnbn[so+dir2] == 4 && board[so+dir2] == NOGROUP)
			addlist((sqr_t)(so+dir2),rlist);
		return;
		}
	else if (lnbf[so][1-c] != 0) {  /* enemy stone above */
		if (S_COLOR(so-dir2) == 1-c &&
			(lnbf[so][c] || lnbn[so] == 3))
			addlist(so,rlist);  /* just contact below */
		if (lnbn[so+dir2] == 4 && board[so+dir2] == NOGROUP)
			addlist((sqr_t)(so+dir2),rlist);
		else if (edge[so] == 2 && lnbn[so+dir2] == 3 && board[so+dir2] == NOGROUP &&
			lnbn[so] == 3)adflist(so,rlist);
		else adflist(s,rlist);
		return;
		}
	else if (armylibs[army] < 4) {  /* can't make long jump if low on liberties */
		adflist(so,rlist);
		adflist(s,rlist);
		}
	else if (edge[so] > 2) {
		soo = NOSQUARE; /* 2 pt jump */
		for (ptr = nblbp[so]; ptr != EOL; ptr = link[ptr]) {
			if ((sqr_t)list[ptr] == s)continue;
			if (edge[list[ptr]] == edge[so]) {
				soo = list[ptr];
				break;
				}
			}
		if (soo == NOSQUARE) {  /* must be stone here */
			soo = so + so - s;
			if (S_COLOR(soo) == c && board[soo+dir2] == NOGROUP)
				adflist((sqr_t)(soo+dir2),rlist);
			else
				adflist(s,rlist);
			return;
			}
		if (S_COLOR(soo+dir2) == c) { /* already stone below 2 pt jump point */
			adflist(s,rlist);
			adflist((sqr_t)(s+dir2),rlist);
			adflist((sqr_t)(so+dir2),rlist);
			return;
			}
		if (lnbn[soo] == 4) { 
			if (edge[soo] == 3 && lnbf[soo-dir2][c] != 0 && 
				board[so-dir2] == NOGROUP)
				adflist((sqr_t)(so-dir2),rlist);
			else if (edge[s] == 4 && board[s+s-so] == NOGROUP)
				adflist(so,rlist);  /* from stone on 5th line */
			else {
				adflist(soo,rlist); /* two point jump */
				if (twostonewall && edge[soo] > 2 && edge2[soo] > 3 &&
				   lnbn[soo+soo-so] == 4)
					adflist((sqr_t)(soo+soo-so),rlist);
				if (edge[s] == 4) {  /* can extend on 3rd line also */
					if (lnbf[soo+dir2][1-c] != 0)
						adflist((sqr_t)(so+dir2),rlist); /* enemy near, hold back one */
					else
						adflist((sqr_t)(soo+dir2),rlist);
					if (twostonewall && edge[soo] > 2 && edge2[soo] > 3 &&
						board[soo+soo-so+dir2] == NOGROUP &&
						lnbn[soo+soo-so+dir2] == 4)
						adflist((sqr_t)(soo+soo-so+dir2),rlist);
					}
				}
			}
		else if (lnbf[soo][1-c] != 0) {  /* enemy stone */
			if (board[soo+dir2] == NOGROUP && lnbn[soo+dir2] == 4)
				adflist((sqr_t)(soo+dir2),rlist);
			else if (board[so+dir2] == NOGROUP && lnbn[so+dir2] >= 3) {
				adflist((sqr_t)(so+dir2),rlist);
				if (lnbn[so+dir2] == 3 && lnbn[s+dir2] == 4)
					adflist((sqr_t)(s+dir2), rlist);	/* 7/01, can diagonal also */
				}
			else
				adflist(so,rlist);
			
			}
		else {   /* friendly stone */
			adflist(so,rlist);
			if (edge[so] == 3 && lnbn[so-dir2] == 4 && S_COLOR(so-dir2) == NOCOLOR)
				adflist((sqr_t)(so-dir2),rlist);
			if (edge[so] == 4 && lnbn[so+dir2] == 4)
				adflist((sqr_t)(so+dir2),rlist);
			}
		}
	else if (edge[so] == 2) {
		adflist(so,rlist);
		if (board[so-dir2] == NOGROUP &&
		   lnbn[so-dir2] == 4 &&
		   lnbn[so] == 4)
		   adflist((sqr_t)(so-dir2),rlist);
		}
	else if (edge[so] == 1) {
		adflist((sqr_t)(s-dir2),rlist);
		if (board[so-dir2] == NOGROUP && lnbf[s-dir2][1-c] == 0)
			adflist((sqr_t)(so-dir2),rlist);
		}
	}


/* find all moves which remove a particular eye potential 
 * return unsorted list of moves, best move first.
 * list can include an illegal ko capture.
 */

list_t rmpot(army_t army, int p) {
	sqr_t s;
	list_t rlist = EOL;
	list_t ptr;
	army_t army2;
	int emptyok = FALSE;
	switch(pots[p].pot_type) {
      case CONNECT:
		rlist = rmconnect(army,pots[p].pot_where);
		break;
      case VITAL:
	    if (pots[p].pot_where & EYEADDONLY) {
		  emptyok = TRUE;
		 break;
		 }
	    s = pots[p].pot_where&EYEPOINTMASK;
		if (board[s] == NOGROUP)
			adflist(s,&rlist);
		else 
			cpylist(grlbp[board[s]],&rlist);
		break;
      case UNDERCUT:
		rlist = stopuc(army,(sqr_t)pots[p].pot_where,TRUE);
		break;
      case THREAT:
	     rmthreat((army_t)pots[p].pot_where,&rlist);
		 break;
	  case POTTHREAT:
	     rmthreat((army_t)pots[p].pot_where,&rlist);
		 emptyok = TRUE;
#ifdef NEVER	     
	    for (ptr = armygroups[pots[p].pot_where]; ptr != EOL; ptr = link[ptr]) {
	    	tmplist = getdefmoves((group_t)list[ptr]);
	    	mrflist(&tmplist,&rlist);
			mv = grsavemove[list[ptr]];
			if (mv != NOSQUARE && mv != PASS) {
				dlflist(mv,&rlist);
				adflist(mv,&rlist);
				}
			}
#endif			
		break;
	case POTDEAD:
		army2 = (army_t)pots[p].pot_where;
		if (armynbp[army2] == EOL)getarmynbp(army2);
		for (ptr = armynbp[army2]; ptr != EOL; ptr = link[ptr])
			if (A_THREATENED((army_t)list[ptr]))
				rmthreat((army_t)list[ptr], &rlist);
		break;
      case EXTEND:
		rlist = blockextend(army,(sqr_t)pots[p].pot_where);
		break;
		}
#ifdef CHECK
	if (rlist == EOL && pots[p].pot_val > 0 && !emptyok)
		outerror("Rmpot returning empty list");
	for (ptr = rlist; ptr != EOL; ptr = link[ptr]) {
		if ((sqr_t)list[ptr] >= boardsquare && (sqr_t)list[ptr] != PASS && (sqr_t)list[ptr] != NOSQUARE || board[list[ptr]] != NOGROUP) {
			outerror("rmpot generated illegal move\n");
/*			turnoffcplay(); */
			}
		}
#endif	
	return(unflist(rlist));
	}
                
                
static int weakdbl(sqr_t s, int c) {
	list_t ptr;
	int count = 0;
	if (s == NOSQUARE)
		return FALSE;
	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
		if (lnbf[list[ptr]][c] > 1 && lnbf[list[ptr]][1-c] == 0)
			count++;
	return(count < 2);
	}                

void knightcutpoints(int cn, list_t *rlist) {
	sqr_t s1, s2;
	int xv;
	s1 = list[cnlkptr[cn]];
	s2 = list[link[cnlkptr[cn]]];
	xv = xval[s2]-xval[s1];
	if (xv == -1) {
		adflist((sqr_t)(s1+boardsize),rlist);
		adflist((sqr_t)(s1+boardsize-1),rlist);
		}
	else if (xv == 1) {
		adflist((sqr_t)(s1+boardsize),rlist);
		adflist((sqr_t)(s1+boardsize+1),rlist);
		}
	else if (xv == 2) {
		adflist((sqr_t)(s1+1),rlist);
		adflist((sqr_t)(s1+boardsize+1),rlist);
		}
	else if (xv == -2) {
		adflist((sqr_t)(s2-boardsize+1),rlist);
		adflist((sqr_t)(s2+1),rlist);
		}
	}

extern int dirnm[];
list_t rmconnect(army_t army, conn_t cn) {
	list_t rlist=EOL, ptr, ptr2, tmplist = EOL;
	sqr_t s, s1, s2, gs1 = NOSQUARE, gs2 = NOSQUARE;
	group_t g;
	int xv, yv, c, i, ldtmp, dir, fg1, fg2, delta;
	int offs;

	army = army;
	switch(cntype[cn]) {
	  case CN_BAMBOOJOINT:  /* one side must be threatened */
		break;
	  case CN_DIAGONAL:
		cpylist(cnptr[cn],&rlist);
		break;
	  case CN_HANE:
	  case CN_ONEPOINTJUMP:
		cpylist(cnptr[cn],&rlist);
		if (cnprot[cn] == SHARED_CONNECT)
			for (ptr = nblbp[list[cnptr[cn]]]; ptr != EOL; ptr = link[ptr])
				adflist(list[ptr], &rlist);  /* try the peeping moves first */
		break;
	  case CN_THREAT:  /* save threatened common nbr to cut */
		andlist(grnbp[cngr1[cn]],grnbp[cngr2[cn]],&tmplist);
		for (ptr = tmplist; ptr != EOL; ptr = link[ptr])
			if (G_THREATENED(list[ptr])) {
				if (grsavemove[list[ptr]] != NOSQUARE &&
					grsavemove[list[ptr]] != PASS &&
					!inlist(list[cnptr[cn]],&grlbp[list[ptr]])) {
					adflist(grsavemove[list[ptr]],&rlist);
					}
				else {
					cpylist(grlbp[list[ptr]], &rlist);
					for (ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
						if (grlibs[list[ptr2]] == 1)
							adflist(list[grlbp[list[ptr2]]], &rlist);
					}
				}
		killist(&tmplist);
	    break;
	  case CN_HALFKNIGHT:
		s1 = list[cnlkptr[cn]];
		canconnlink(cn,s1,&s,&s2,NOGROUP);    /* get cutting point */
		c = grcolor[cngr1[cn]];
		if (lnbn[s1] >= 2)
			cpylist(cnlkptr[cn],&rlist);
		if (s2 != NOSQUARE)
			adflist(s2, &rlist);
		if (cntplyhere(s, c, NOGROUP) != YES)
			adflist(s,&rlist);
		break;
	  case CN_TWOPOINTJUMP:
		cpylist(cnlkptr[cn],&rlist);
		if (S_NEUTRAL(list[link[rlist]]))revlist(&rlist);  /* put peeped point first */
		else if (!S_NEUTRAL(list[rlist]) && ld[list[rlist]] > ld[list[link[rlist]]])
			revlist(&rlist);  /* cut at weaker end */
		if (S_NEUTRAL(list[rlist]) && 
			lnbn[list[link[rlist]]] == 3)
			for (ptr = nblbp[list[link[rlist]]]; ptr != EOL; ptr = link[ptr])
				if (lnbn[list[ptr]] == 4 || lnbf[list[ptr]][1-grcolor[cngr1[cn]]])
					adflist(list[ptr], &rlist);	/* stylish knights move across 2 pt jump */
		break;
	  case CN_THREEPOINTJUMP:
		s1 = (list[cnllptr[cn]]+list[link[cnllptr[cn]]])/2;  /* the center point */
		c = grcolor[cngr1[cn]];
		if (lnbf[s1][1-c]) {	/* enemy stone next to center point */
			cpylist(cnllptr[cn],&rlist);
			adflist(s1, &rlist);
			for (ptr = nblbp[s1]; ptr != EOL; ptr = link[ptr])
				if (!inflist(list[ptr], &cnllptr[cn]))
					adflist(list[ptr], &rlist);
		}
		else {
			cpylist(cnllptr[cn],&rlist);
			adflist(s1, &rlist);
		}
		break;
	  case CN_KNIGHTSMOVE:
		  knightcutpoints(cn, &rlist);
		
		break;
	  case CN_LARGEKNIGHT:
		s1 = list[cnllptr[cn]];
		s2 = list[link[cnllptr[cn]]];
		xv = xval[s2]-xval[s1];
		yv = yval[s2]-yval[s1];
		if (yv == 3)s1 += boardsize;
		if (xv == -1)s1--;
		if (xv == 3)s1++;
		if (xv == -3)s1 -= 2;
		adflist(s1,&rlist);
		adflist((sqr_t)(s1+1),&rlist);
		adflist((sqr_t)(s1+boardsize),&rlist);
		adflist((sqr_t)(s1+1+boardsize),&rlist);
		break;
	  case CN_HALFLARGEKNIGHT:
		canconnlkg(cn,&s,&s2,NOGROUP);
		if ((sqr_t)list[cnllptr[cn]] != s && (sqr_t)list[cnllptr[cn]] != s2 && 
			(S_NEUTRAL(list[cnllptr[cn]]) || lnbn[list[cnllptr[cn]]] > 2))
			adflist(list[cnllptr[cn]],&rlist);
/*		if (lnbf[s][1-grcolor[cngr1[cn]]] != 0) { */
		if (lnbf[s2][1-grcolor[cngr1[cn]]] != 0 || lnbn[s2] >= 2) {
			for (ptr = nblbp[s2]; ptr != EOL; ptr = link[ptr])
				if (list[ptr] != s)
					adflist(list[ptr],&rlist);  /* cut by playing next to connection line */
			adflist(s2,&rlist);
			}
		adflist(s,&rlist);                    
#ifdef NEVER			
			}
		else {
			if (lnbf[s][1-grcolor[cngr1[cn]]] != 0 || lnbn[s] >= 2)
				adflist(s,&rlist);
			adflist(s2,&rlist);
			}
#endif			
		break;
	  case CN_UNKNOWN:
		if (cncnum[cn] != 0) {
			cpylist(cnptr[cn],&rlist);
			break;
		}
		for (ptr = cnlkptr[cn]; ptr != EOL; ptr = link[ptr]) {
			i = fdir[list[ptr]];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				offs = nbr[i];
				dir = dirnm[i];
				if (dstbrd[list[ptr]][dir] == 1 && S_COLOR(sqrbrd[list[ptr]][dir]) == A_COLOR(army)) {
					aeflist((listval_t)(list[ptr] + offs), &rlist);
					}
				}
			aeflist(list[ptr], &rlist);
		}
		for (ptr = cnllptr[cn]; ptr != EOL; ptr = link[ptr]) {
			i = fdir[list[ptr]];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				offs = nbr[i];
				dir = dirnm[i];
				if (dstbrd[list[ptr]][dir] == 2 && S_COLOR(sqrbrd[list[ptr]][dir]) == A_COLOR(army)) {
					aeflist((listval_t)(list[ptr] + offs), &rlist);
					aeflist((listval_t)(list[ptr] + offs*2), &rlist);
					}
				}
			aeflist(list[ptr], &rlist);
		}
		for (ptr = cnollptr[cn]; ptr != EOL; ptr = link[ptr]) {
			i = fdir[list[ptr]];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				offs = nbr[i];
				dir = dirnm[i];
				if (dstbrd[list[ptr]][dir] == 3 && S_COLOR(sqrbrd[list[ptr]][dir]) == A_COLOR(army)) {
					aeflist((listval_t)(list[ptr] + offs*2), &rlist);
					aeflist((listval_t)(list[ptr] + offs*3), &rlist);
					aeflist((listval_t)(list[ptr] + offs), &rlist);
					}
				}
			aeflist(list[ptr], &rlist);
		}
		break;
	  case CN_DOUBLEDIAG:
	  	tmplist = EOL;
		canconndd(cngr1[cn],cngr2[cn],grcolor[cngr1[cn]],cn,NOGROUP,&tmplist);
		if (tmplist == EOL) /* compensate for bug in canconndd */
			cpylist(cnddptr[cn],&tmplist);
		fg1 = fg2 = 0;
		for (ptr2 = tmplist; ptr2 != EOL; ptr2 = link[ptr2]) {
			s = list[ptr2];
			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
				if (inlist(list[ptr],&grlbp[cngr1[cn]])) {
					fg1++;
					gs1 = list[ptr];
					}
				if (inlist(list[ptr],&grlbp[cngr2[cn]])) {
					fg2++;
					gs2 = list[ptr];
					}
				}
			if (fg1 == 1)
				adflist(gs1,&rlist);
			if (fg2 == 1)
				adflist(gs2,&rlist);
			c = grcolor[cngr1[cn]];
			if (weakdbl(s,c))
				for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
					if (lnbf[list[ptr]][1-c] != 0 && !inflist(list[ptr],&rlist))
						adflist(list[ptr],&rlist);
					}
			}
		catlist(&tmplist,&rlist);
		break;
	case CN_MULTIHALFKNIGHT:
		mrglist(cnlkptr[cn],&rlist);
		break;
	case CN_MULTIPLE:  /* lk or ll or oll > 2 */
		c = grcolor[cngr1[cn]];
		if (cnlknum[cn] > 2) {
			mrglist(cnlkptr[cn],&rlist);
			}
		else if (cnllnum[cn] > 2) {
			for (ptr = cnllptr[cn]; ptr != EOL; ptr = link[ptr]) {
				s = list[ptr];
				if (lnbf[s][1-c] != 0)
					adflist(s,&rlist);
				else {
					g = cngr1[cn];
					i = fdir[s];
					for (ldtmp = ldir[i]; i < ldtmp; ++i)
						if (board[s+nbr[i]] == g)g = cngr2[cn];
					for (dir = 0; dir < 4; dir++)
						if (dstbrd[s][dir] == 2 && board[sqrbrd[s][dir]] == g)
							adflist((sqr_t)(s+brddir[dir]),&rlist);
					}
				}
			}
		else if (cnollnum[cn] > 2) {
			for (ptr = cnollptr[cn]; ptr != EOL; ptr = link[ptr]) {
				s = list[ptr];
				if (lnbf[s][1-c] != 0)
					adflist(s,&rlist);
				else {
					g = cngr1[cn];
					i = fdir[s];
					for (ldtmp = ldir[i]; i < ldtmp; ++i)
						if (board[s+nbr[i]] == g)g = cngr2[cn];
					for (dir = 0; dir < 4; dir++)
						if (dstbrd[s][dir] == 3 && board[sqrbrd[s][dir]] == g)
							adflist((sqr_t)(s+brddir[dir]),&rlist);
					}
				}
			}
		break;
	case CN_EXTRALARGEKNIGHT:
		s1 = list[cnollptr[cn]];
		s2 = list[link[cnollptr[cn]]];
		delta = s2-s1;
		if (delta == boardsize + 4) {
			adflist((sqr_t)(s1+2),&rlist);
			adflist((sqr_t)(s2-2),&rlist);
			}
		else if (delta == boardsize - 4) {
			adflist((sqr_t)(s1-2),&rlist);
			adflist((sqr_t)(s2+2),&rlist);
			}
		else if (delta == 4*boardsize + 1) {
			adflist((sqr_t)(s1+2*boardsize),&rlist);
			adflist((sqr_t)(s2-2*boardsize),&rlist);
			}
		else if (delta == 4*boardsize -1) {
			adflist((sqr_t)(s1+2*boardsize),&rlist);
			adflist((sqr_t)(s2-2*boardsize),&rlist);
			}
		break;
	case CN_HALFEXTRAKNIGHT:
		s1 = list[cnollptr[cn]];
		g = cngr1[cn];
		if (inflist(g, &nbgrp[s1][grcolor[g]]))
			g = cngr2[cn];
		i = fdir[s1];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			offs = nbr[i];
			dir = dirnm[i];
			if (dstbrd[s1][dir] == 3 && board[sqrbrd[s1][dir]] == g) {
				if (lnbf[s1+offs][1-grcolor[g]])
					adflist((listval_t)(s1+offs), &rlist);
				else
					adflist((listval_t)(s1+2*offs), &rlist);
				break;
				}
			}
		break;
	case CN_FOURPOINTJUMP:
		s1 = list[cnollptr[cn]];
		s2 = list[link[cnollptr[cn]]];
		adflist((sqr_t)(s1+(s2-s1)/3), &rlist);
		adflist((sqr_t)(s2-(s2-s1)/3), &rlist);
		adflist(s1,&rlist);
		adflist(s2,&rlist);
		break;

#ifdef CHECK	  
	  default:
	  	outerr("Bad connection type in rmpot!\n");
#endif	  	
		}
	if (G_THREATENED(cngr1[cn]) && grcapmove[cngr1[cn]] != NOSQUARE &&
		!inflist(grcapmove[cngr1[cn]],&rlist))
		adflist(grcapmove[cngr1[cn]],&rlist);
	if (G_THREATENED(cngr2[cn]) && grcapmove[cngr2[cn]] != NOSQUARE &&
		!inflist(grcapmove[cngr2[cn]],&rlist))
		adflist(grcapmove[cngr2[cn]],&rlist);
	return(rlist);
	}


static list_t blockextend(army_t army, sqr_t s) {
	list_t rlist = EOL,ptr,ptr2;
	int c,flag;
	c = grcolor[list[armygroups[army]]];
	if (S_NEUTRAL(s)) {
		if (edge[s] >= 3) {
			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
				if (edge[list[ptr]] < edge[s])
					adflist(list[ptr],&rlist);
			}
		adflist(s,&rlist);  /* blocking is best */
		}
	else {
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])  /* find highest */
			if (edge[list[ptr]] > edge[s] && edge[list[ptr]] <= 4 &&
				board[s] == NOGROUP &&
				(ld[list[ptr]] == 3 ||
				    ld[list[ptr]] == 4) && lnbn[list[ptr]] ==3  &&
				   grcolor[lgr[list[ptr]]] == grcolor[lgr[s]]) {
				   	s = list[ptr];
				   }
		flag = FALSE;
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
			if (!flag && lnbf[list[ptr]][1-c] != 0) {  /* play close if stone nearby */
				for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2])
					if (edge[list[ptr2]] > 1 && 
						edge[list[ptr2]] < edge[s] &&
						lnbn[list[ptr2]] == 4) {
						adflist(list[ptr2],&rlist);
						flag = TRUE;
						break;
						}
				if (!flag)adflist(s,&rlist);
				}
			if (edge[list[ptr]] == edge[s]) {
				if (lnbn[list[ptr]] == 4) {
					if (lnbn[list[ptr]+list[ptr]-s] == 4 &&
					   edge[list[ptr]+list[ptr]-s] == edge[s])
						adflist((sqr_t)(list[ptr]+list[ptr]-s),&rlist);
					adflist(list[ptr],&rlist);
					for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (edge[list[ptr2]] >= 3 &&
							edge[list[ptr2]] < edge[list[ptr]] &&
							lnbn[list[ptr2]] == 4)
							adflist(list[ptr2],&rlist);
						}
					}
				else if (S_COLOR(list[ptr]+list[ptr]-s) != 1-c && edge[s] > 1)
					adflist(list[ptr],&rlist);
				else
					for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {
						if (edge[list[ptr2]] > 1 &&
							edge[list[ptr2]] < edge[list[ptr]] &&
							lnbn[list[ptr2]] == 4)
							adflist(list[ptr2],&rlist);
						else if (edge[s] == 1 && edge[list[ptr2]] == 2)
							adflist(list[ptr2],&rlist);
						}
				}
			if (edge[s] == 1 && edge[list[ptr]] == 2)
				adflist(list[ptr],&rlist);
			}
		}
	if (rlist == EOL)
		cpylist(nblbp[s],&rlist);
	return(rlist);
	}

/* moves to add or remove undercut territory.
 * rm is TRUE if are removing (eliminating the potential)
 */	

static list_t stopuc(army_t army, sqr_t s, int rm) {
	list_t ptr,ptr2,rlist = EOL;
	int c,crawl,nbthreat = FALSE;
	sqr_t so,sd;
	c = grcolor[list[armygroups[army]]];
	if (ltrgd[s] == 7)
		for (ptr = nbgrp[s][A_COLOR(army)]; ptr != EOL; ptr = link[ptr])
			if (G_THREATENED(list[ptr]))
				nbthreat = TRUE;
	if (ltrgd[s] == 4 || ltrgd[s] == 5 || 
		ltrgd[s] == 7 && !nbthreat) {  /* if 7 due to threatened nbr, no time to block, must save group */
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
			if (edge[list[ptr]] == edge[s] && edge2[s] <= 4 &&
			   edge2[list[ptr]] < edge2[s] && ltrgd[list[ptr]] >= 4)
				findblock(s,list[ptr],&rlist,c,rm);
			if (edge[s] == 1 && edge[list[ptr]] == 1 &&
				lnbf[list[ptr]][1-c] != 0) {
				if (lnbf[list[ptr]][c] != 0 || 
					lnbn[list[ptr]] > 1 || rm)
					adflist(list[ptr],&rlist);
				}
			if (ltrgd[list[ptr]] == 2)
				adflist(list[ptr], &rlist);
			if (edge[list[ptr]] >= edge[s])continue;
			findblock(s,list[ptr],&rlist,c,rm);
			}
  		if (rm && ltrgd[s] == 7)
  			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr])
  				if (lnbf[list[ptr]][1-c] != 0 && !inflist(s,&rlist)) {
					adflist(s,&rlist);  /* hane or contact from stone */
					break;
					}
		if (rlist == EOL)
			adflist(s,&rlist);
		}
	if (ltrgd[s] == 3 || ltrgd[s] == 7) {
		for (ptr2 = nbgrp[s][c]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (grlibs[list[ptr2]] != 2)continue; /* extend with 2 liberties */
			for (ptr = grlbp[list[ptr2]]; ptr != EOL; ptr = link[ptr])
				if (lnbn[list[ptr]] > 1)adflist(list[ptr],&rlist);
			}
		if (lnbf[s][c] == 1 /* && A_NUMLIBS(army) <= 3 1/1/99 */)  /* just extend */
			adflist(s,&rlist);                                             
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
			if (edge[list[ptr]] > edge[s] && edge[list[ptr]] <= 4 &&
				A_NUMLIBS(army) > edge[s] && 
				lnbf[list[ptr]][1-c] == 1 && lnbn[list[ptr]] == 3 &&
				S_COLOR(list[ptr]+list[ptr]-s) != 1-c)
					adflist(list[ptr],&rlist);
						/* hane up from edge */
			if (ltrgd[list[ptr]] == 2)
				adflist(list[ptr], &rlist);
			if (edge[list[ptr]] != edge[s])continue;
			so = list[ptr]+list[ptr]-s;  /* 2 pt jump or knight's move */
			if (edge[list[ptr]] == 1) { /* push along 2nd line */
				for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
					if (edge[list[ptr2]] == 2 &&
					   (S_NEUTRAL(list[ptr2]) || 
					    lnbf[list[ptr2]+list[ptr]-s][1-c] != 0 &&
					    board[list[ptr2]+list[ptr]-s] == NOGROUP))
						adflist(list[ptr2],&rlist);
				}
			else if (lnbf[list[ptr]][1-c] != 0) {  /* hane */
				adflist(list[ptr],&rlist);
				if (lnbn[list[ptr]] < 3 || rm)
					adflist(s,&rlist);
				}
			else if (board[so] == NOGROUP && lnbf[so][1-c] != 0) {
				adflist(list[ptr],&rlist);  /* hane */
				if (lnbf[list[ptr]][c] != 0)
					adflist(so,&rlist);
				}
			else if (edge[list[ptr]] == edge[s] && lnbn[list[ptr]] == 4) {
				for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2])
					if (edge[list[ptr2]] > edge[s] && lnbf[list[ptr2]][1-c] != 0) {
						adflist(list[ptr2],&rlist);
						adflist(list[ptr],&rlist);
						break;
						}
				for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
					if (edge[list[ptr2]] > edge[s] && lnbf[list[ptr2]][1-c] != 0) {
						adflist(list[ptr2],&rlist);
						adflist(list[ptr],&rlist);
						break;
						}
				for (ptr2 = nblbp[so]; ptr2 != EOL; ptr2 = link[ptr2])
					if (lnbf[list[ptr2]][1-c] != 0) {
						adflist(so,&rlist);
						adflist(list[ptr],&rlist);
						break;
						}
				}
			}
		if (rlist == EOL || rm)
			adflist(s,&rlist);
		}
	if (rlist == EOL) {
		if (S_NEUTRAL(s)) {
			crawl = FALSE;
			so = sd = NOSQUARE;
			for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
				if (edge[list[ptr]] == edge[s]) {
					so = list[ptr];
					crawl = TRUE;
					}
				if (edge[list[ptr]] < edge[s]) {
					sd = list[ptr];
					}
				if (edge[s] == 2 && edge[list[ptr]] == 1 &&
					S_COLOR(s+s-list[ptr]) == c && lnbn[s] == 2 &&
					lnbn[list[ptr]] == 3)
					adflist(list[ptr], &rlist);  /* 5/02 block all the way to edge */
				}
			if (lnbf[s][c] > 1)adflist(s,&rlist);
			else if (crawl) {
				if (lnbn[so] == 3 && lnbf[so][c] == 1 && so != NOSQUARE) {
					adflist(so,&rlist);
					adflist(s,&rlist);
					}
				else adflist(s,&rlist);
				}
			else if (sd != NOSQUARE && lnbf[s][c] > 1)adflist(sd,&rlist);
			else adflist(s,&rlist);
			}
		else if (G_THREATENED(lgr[s]))
			adflist(s,&rlist);
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
			if (edge[list[ptr]] < edge[s])continue;
			if (edge[list[ptr]] > edge[s] && 
			   (edge2[s] > 3 || edge2[list[ptr]] != edge2[s]))continue;
			if (lnbf[list[ptr]][1-c] != 0) {
				if (grcolor[lgr[list[ptr]]] != 1-c || gralive[lgr[list[ptr]]] != DEAD) {
					adflist(list[ptr],&rlist);
					if (lnbn[list[ptr]] < 3)
						adflist(s,&rlist);
					}
				}
			else if (board[list[ptr]+list[ptr]-s] == NOGROUP) {
				if (lnbf[list[ptr]+list[ptr]-s][1-c] != 0)
					adflist(list[ptr],&rlist);
				}
/*			else for (ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2])
				if (edge[list[ptr2]] > edge[s] && lnbn[list[ptr2]] > 2)
					adflist(list[ptr2],&rlist); seems no good when crawling under enemy stone */
			}
		}
	return(unflist(rlist));
	}


/* find a list of moves that block from undercutting at point s
 * sn is between s and the edge of the board.
 */

static void findblock(sqr_t s, sqr_t sn, list_t *rlist, int c, int rm) {
	sqr_t dir,sn2 = NOSQUARE;
	group_t g;
	int dist, sdir;
	list_t tmplist = EOL;
	dir = sn-s;  /* towards edge */
	for (sdir = 0; sdir < 4; sdir++) {
		if (brddir[sdir] == dir)continue;  /* towards edge */
		if (brddir[sdir] == -dir) {  /* away from edge */
			g = board[sqrbrd[s][sdir]];
			if (grcolor[g] == c && G_THREATENED(g) && dstbrd[s][sdir] <= 2) {
				if (grsavemove[g] != NOSQUARE && grsavemove[g] != PASS)
					adflist(grsavemove[g],rlist); /* save threatened group above */
				else {
					cpylist(grlbp[g],&tmplist);
					catlist(&tmplist,rlist);
				}
			}
#ifdef NEVER				
			if (grcolor[g] == 1-c && dstbrd[s][sdir] <= 3 && gralive[g] != DEAD) {
				adflist(s,rlist);
			}
#endif				
			continue;
		}
			
		if (rm && (ltrgd[s] & 4) && S_COLOR(sqrbrd[s][sdir]) == 1-c && edge[s] > 1) {
			/* knights move or monkey jump */
			if (dstbrd[s][sdir] == 1 &&  /* left or right */
			   lnbf[s+dir+brddir[sdir]][c] == 0 &&
			   lnbf[s+dir][c] == 0) {
				adflist((sqr_t)(s+dir),rlist);
				if (edge[s] == 2 && 
					edge[s+brddir[sdir]] == 2 &&
					lnbn[s+dir-brddir[sdir]] == 3)
					adflist((sqr_t)(s+dir-brddir[sdir]),rlist); /* monkey jump */
				continue;
			}
			if (dstbrd[s][sdir] == 2 && 
			   board[s+dir+brddir[sdir]] == NOGROUP &&
				 lnbf[s+dir+brddir[sdir]][c] == 0) {
				adflist((sqr_t)(s+dir+brddir[sdir]),rlist);
				if (edge[s] == 2 && lnbn[s+dir] == 3 ||
					lnbf[s+dir][c] == 0 &&
					lnbf[s+brddir[sdir]][c] == 0 &&
					lnbf[s+brddir[sdir]+brddir[sdir]][c] == 0)
					adflist((sqr_t)(s+dir),rlist);
				continue;
			}
		}
			
		sn = s-dir;
		dist = 100;
		do {
			sn += dir;	/* walk sn from s towards the edge of the board */
			if (board[sn] != NOGROUP)/* ADD HERE CHECK FOR sn has stone on it */
				break;      /* can't undercut through a stone, so no block in this direction */
			if (sqrbrd[sn][sdir] == NOSQUARE)continue;  /* look along edge */
			g = board[sqrbrd[sn][sdir]];
			if (grcolor[g] == c && G_THREATENED(g) && 
				grsavemove[g] != sn) {  /* save move below block point can't add points above it */
				if (grsavemove[g] != NOSQUARE && grsavemove[g] != PASS)
					adflist(grsavemove[g],rlist);
				else {
					cpylist(grlbp[g],&tmplist);
					catlist(&tmplist,rlist);
				}
			}
			if (grcolor[g] == 1-c) {  /* find closest enemy stone */
				if (dstbrd[sn][sdir] <= dist) {
					dist = dstbrd[sn][sdir];
					if (dist > 1 && lnbf[sn+brddir[sdir]][1-c]) {
						dist = 1;  /* enemy stone above or below line */
						sn2 = sn+brddir[sdir];
					}
					else
						sn2 = sn + brddir[sdir]*dstbrd[sn][sdir]; /* point in contact with enemy */
					if (dist > 0 && lnbf[sn2][c] == 0 && lnbf[sn2-brddir[sdir]][c] == 0 &&
						(lnbf[sn2-dir][c] == 0 || board[sn2-dir] != NOGROUP) &&
						(dstbrd[sn][sdir] >= 2 || edge[sn2] == 1 && dstbrd[sn][sdir] > 0) &&
						grlibs[g] > 2 || lnbf[sn2][1-c] > 1) {
						sn2 -= brddir[sdir];  /* back off one */
					}
				}
					if (dist == 0)
						adflist((sqr_t)(sn-dir),rlist);
			}
		} while(edge[s] > 1 && edge[sn] > 1 || edge[s] == 1 && edge[sn] > 0);

		if (dist != 100 && board[sn2] == NOGROUP) {
			if (dist > 0 && lnbf[sn2][1-c] && !lnbf[sn2][c] && 
				(edge[sn2] > 1 || edge[sn2] == 1 && edge[sn2+brddir[sdir]] != 2) &&  /* not going off edge */
				board[sn2-brddir[sdir]] == NOGROUP) {
				adflist((sqr_t)(sn2-brddir[sdir]),rlist);  /* back off one from contact */
				if (!lnbf[sn2-dir][1-c] && lnbf[sn2-dir][c])
					adflist(sn2,rlist);	/* contact ok if friend nearby */
			}
			else {
				adflist(sn2,rlist);  /* direct block */
			}
		}
	}
}



static sqr_t saveadjacent(sqr_t s, army_t army) {
	int i,ldtmp,ptr;
	sqr_t sn;
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		sn = s+nbr[i];
		if (S_COLOR(sn) == A_COLOR(army) && S_NUMLIBS(sn) == 1)
			for (ptr = grnbp[S_GROUP(sn)]; ptr != EOL; ptr = link[ptr])
				if (grlibs[list[ptr]] == 1)
				return(list[grlbp[list[ptr]]]);
		}
	return(NOSQUARE);
	}
	

 
