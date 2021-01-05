/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2hd.h"
#include "g2pat.h"
#include "g2dist.h"
#ifdef G2DEBUGOUTPUT
#include <stdio.h>
#endif

static int cnpatonepoint(group_t g, group_t g2, int c, conn_t cn, sqr_t s,int dir,int dir2,listval_t ldrno, int *retpat);
static int sharedthreat(conn_t cn, group_t g, group_t g2, group_t thnbr,int c);
static int checkforcut(sqr_t s1, sqr_t s2, sqr_t s3,conn_t cn, char c);
static int cnmultlk(conn_t cn, int c, group_t g1, group_t g2, listval_t ldrno);
static int twoenemytwopoint(sqr_t s, sqr_t s2, conn_t cn, char c);
static int cnmlkgs(group_t g, group_t g2, int c, conn_t cn, listval_t ldrno);
static int canpushcut3(sqr_t s1, sqr_t s2, sqr_t s3, conn_t cn, char c, int debug);
static int largeknight(group_t g, group_t g2, int c, conn_t cn, sqr_t s, sqr_t s2, int dir, listval_t ldrno);
static int canconnect4(sqr_t s1, sqr_t s2, sqr_t s3, sqr_t s4, conn_t cn, char c);
int cntwolkgs(group_t g, group_t g2, int c, conn_t cn, listval_t ldrno, int *type);
int canconnoll(group_t g, group_t g2, int c, conn_t cn, listval_t ldrno, int *type);
#ifdef TEST
/* static int can_def_peep(unsigned int cn, group_t g, group_t g2, int c, sqr_t s, int dir,int dir2); */
static int can_def_wedge(unsigned int cn, group_t g, group_t g2, int c, sqr_t s, int dir,int dir2);
/*static int cnonepoint(group_t g, group_t g2, int c, conn_t cn, sqr_t s,int dir,int dir2,listval_t ldrno); */
#endif
static void fixaprot(conn_t cn);
void combinearmy(army_t,army_t);
void splitarmy(army_t);
static void fixconnarmies(conn_t cn, int oldprot);


extern list_t splitlist;

#ifdef G2DEBUGOUTPUT
char *protnam[] = {
"NO ",
"MAY",
"CAN",
"THR",
"SHR",
"KO",
"AJI",
"SLD",
"Read Aji",
"Read",
"Read",
"Read"
};

extern char *cntpstr[16];

#endif


#ifdef NEVER

/* check g for connecttions to other groups than g2, see if throw
 * in in that connection affects this one
 * a rich source of evaluation order bugs since how solid
 * one connection is depends on how solid another one is!
 */


static int otherweakconn(group_t g, group_t g2, sqr_t scut) {
	conn_t cn;
	list_t ptr;
	sqr_t s;
	if (grlibs[g] >= 4)return(FALSE);
	if (comlist(nblbp[scut],grlbp[g]))
		return FALSE;
	for (ptr = grcnp[g]; ptr != EOL; ptr = link[ptr]) {
		cn = list[ptr];
		if (cngr1[cn] == g2 || cngr2[cn] == g2)continue;
		if (cncnum[cn] > 1)continue;
		if (cncnum[cn] == 0 && cnlknum[cn] > 1 && PROTCONN(cn) &&
			!inlist(scut, &cnlkptr[cn]))return(TRUE);
		if (cncnum[cn] == 0 && cnlknum[cn] == 0 && cnllnum[cn] > 1 &&  add cnollnum!
			 PROTCONN(cn))return(TRUE);
		if (cncnum[cn] == 0)continue;
		s = list[cnptr[cn]];  /* exactly one connection */
		if (ld[s] == 4 && lnbn[s] > 1)return(TRUE);
		}
	return(FALSE);
	}

#endif

/* check connection at s between g and g2 to see if is is unbreakable.
 * c is color of g and g2
 * return the type of the connection in type
 * This is the only connection between the groups 
 * return strength of connectivity
 * need more work on shared threatened group (shared_connect)
 */

static int cnoneconn(group_t g,group_t g2,int c,conn_t cn,listval_t ldrno,int *type, int *pat) {
	int sum,i,dir,dir2 = 1,ldtmp,opjdir2, onecap = 0, onecap2 = 0;
	sqr_t s,sn;
	list_t ptr, ptr3, tmplist;
	group_t g3,thnbr;
	int one_point_jump = FALSE;
	sqr_t tmp;
	int thr_friendly;	/* number of adjacent unconditional threatened friendly neighbors */
	int newres;
	group_t dead_friendly_group = NOGROUP;
	int unequalthrt;		/* sides of connection have unequal threat values */
	int sg1, sg2;

    /* is it a one point jump */
	s = list[cnptr[cn]];

	thr_friendly = 0;
	sum = 0;
	i = fdir[s];
	dir = 0;
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		sn = s + nbr[i];
		if (board[sn] == g) {
			dir2 = nbr[i];		/* direction to friendly stone */
			if ((edge[s] > 1 || edge[s] == 1 && edge[sn] < 2) && board[s-nbr[i]] == g2) {
				one_point_jump = TRUE;  /* if any part is a one point jump */
				opjdir2 = nbr[i];
			}
		}
		if (grcolor[board[sn]] == c && grsize[board[sn]] == 1 &&
			grlibs[board[sn]] == 1)
			onecap++;
		if (board[sn] == g || board[sn] == g2) {
			sum += nbr[i];
		}
		else if (grcolor[board[sn]] == c && S_UNCTHREATENED(sn)) {
			thr_friendly++;
			dead_friendly_group = board[sn];
		}
		else if (grcolor[board[sn]] == 1-c)dir = nbr[i];	/* dir to enemy */
		else if (board[sn] == NOGROUP && dir == 0)dir = nbr[i];
	}
	if (one_point_jump) {
		*type = CN_ONEPOINTJUMP;
		dir2 = opjdir2;
	}
	else *type = CN_HANE;

	unequalthrt = G_THREATENED(g) != G_THREATENED(g2);	/* can't be connected when threat values are unequal */

	if (kosquare == s)
		return(KO_CONNECT);  /* uncuttable due to ko */
	if (onecap == 1 && lnbn[s] == 0 && lnbf[s][1-c] == 0)
		return(KO_CONNECT);	/* enemy can capture a ko at this point */
	if (onecap == 0 && lnbf[s][1-c] == 0 && lnbn[s] == 1 && lnbf[list[nblbp[s]]][1-c] == 3) {	/* throw in for ko */
		for (ptr = nbgrp[list[nblbp[s]]][1-c]; ptr != EOL; ptr = link[ptr]) {
			if (grlibs[list[ptr]] == 1)
				onecap2++;
		}
		if (onecap2 == 0)
			return KO_CONNECT;
	}

	if (G_THREATENED(g) && G_THREATENED(g2) && grlibs[g] > 1 &&
		grlibs[g2] > 1 && lnbf[s][1-c] == 0 && lnbn[s] <= 1)return(AJI_CONNECT);
	if (grcolor[lgr[s]] == 1-c && gralive[lgr[s]] == DEAD && !unequalthrt) 
		return(AJI_CONNECT);	/* geta */
	if (thr_friendly == 2)return(CAN_CONNECT);
	if (thr_friendly && grlibs[g] > 1 && grlibs[g2] > 1) {
		if (lnbn[s] != 0)return(CAN_CONNECT);
		if (thr_friendly && grsize[dead_friendly_group] == 1 && 
			grlibs[dead_friendly_group] == 1)
			return(CAN_CONNECT); /* ko */
		if (thr_friendly == 1 && !S_NEUTRAL(s) &&
			(grsize[dead_friendly_group] > 1 || grlibs[dead_friendly_group] > 1))
			return(unequalthrt?THRT_CONNECT:AJI_CONNECT);  /* can recapture or time to connect */
		return(CAN_CONNECT);
	}

	if (one_point_jump) {
   		newres = cnpatonepoint(g, g2, c, cn, s, dir, dir2, ldrno, pat);
		if (newres > THRT_CONNECT && unequalthrt)
			return THRT_CONNECT;
   		return newres;
   	}

	if (cntplyhere(s,c,(listval_t)(NUMGROUPS+cn))) {
		return(unequalthrt ? THRT_CONNECT : AJI_CONNECT);
	}

	if (unequalthrt) return(THRT_CONNECT);  /* 12/02 can't connect unless equal threat values */

   /* look for common adjacent threatened group */

	tmplist = EOL;
	andlist(grnbp[g],grnbp[g2],&tmplist);
	thnbr = NOGROUP;
	for (ptr3 = tmplist; ptr3 != EOL; ptr3 = link[ptr3]) {
		g3 = (group_t)list[ptr3];
		if (G_THREATENED(g3) >= 2 && !inlist(s,&grlbp[g3])) { 
			if (thnbr != NOGROUP) {  /* two common threatened groups */
				killist(&tmplist);
				*type = CN_THREAT;
				return(AJI_CONNECT);
			}
			thnbr = g3;
		}
	}
	killist(&tmplist);
    
    if (thnbr != NOGROUP &&  /* make sure can stil capture neighbor after he cuts */
    	canbecaptured(list[cnptr[cn]], 1-c, mvs[grpieces[thnbr]], c, 1-c, conncapdepth[playlevel], conncapsize[playlevel], conntaclibs[playlevel], ldrno)) {  /* AJI or SHARED? */
    	*type = CN_THREAT;
		sg1 = sharedthreat(cn,g,g2,thnbr,c);
		sg2 = sharedthreat(cn,g2,g,thnbr,c);
		if (sg1 && sg2) {
			cnshcent[cn] = g < g2?g:g2;
    	 	return(SHARED_CONNECT);
		}
		if (sg1) {
			cnshcent[cn] = g;
    		return(SHARED_CONNECT);
		}
		if (sg2) {
			cnshcent[cn] = g2;
    		return(SHARED_CONNECT);
		}
    	return(AJI_CONNECT);
    }
    
/* try to capture the opposing hane stone quickly */
	g3 = board[s+sum];
	if (grcolor[g3] == 1-c && g3 != thnbr &&
		lnbn[s] == 2 && grlibs[g] > 2 && grlibs[g2] > 2 &&
		grsize[g3] == 1 && grlibs[g3] == 2 && playlevel >= READCONNLEVEL)
		if (iscaptured(g3, conncapdepth[playlevel], conncapsize[playlevel], taclibs[playlevel], connmost[playlevel], c, ldrno, &tmp, 1-c) &&
			canbecaptured(list[cnptr[cn]], 1-c, mvs[grpieces[g3]], c, 1-c, conncapdepth[playlevel], conncapsize[playlevel], conntaclibs[playlevel], ldrno))
			return(AJI_CONNECT);
	if (thnbr == NOGROUP &&
		lnbf[s][c] == 2 &&
		grlibs[g] + grlibs[g2] + lnbn[s] - 2 <= 1)
		return CANT_CONNECT;	/* not enough liberties to connect */
	return(CAN_CONNECT);
}


/* does g have another hane connection to group in contact with thnbr? */

static int sharedthreat(conn_t cn, group_t g, group_t g2, group_t thnbr, int c) {
	list_t ptr3;
	group_t g3;
	sqr_t s, sn;
	int i, ldtmp, opj;

   	for (ptr3 = grcnp[g]; ptr3 != EOL; ptr3 = link[ptr3]) {
   		if (list[ptr3] == cn)continue;
		if (cncnum[cn] != 1)continue;
		s = list[cnptr[list[ptr3]]];
   		g3 = cngr1[list[ptr3]];
   		if (g3 == g)
   			g3 = cngr2[list[ptr3]];

		/* is it a one pt jmp? */
		opj = FALSE;
		i = fdir[s];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			sn = s + nbr[i];
			if (board[sn] == g) {
				if ((edge[s] > 1 || edge[s] == 1 && edge[sn] < 2) && board[s-nbr[i]] == g3) {
					opj = TRUE;
					break;  /* if any part is a one point jump */
				}
			}
		}
		if (opj)
			continue;

   		if (inlist(g3, &grnbp[thnbr]) &&
   			(lnbf[s][1-c] != 0 ||
   			 lnbn[s] > 1))
     		return TRUE;
	}
   	return FALSE;
}

int type2prot[10] = 
{ 0, 1, 2, 3, 4, 5, 1, 2, 3, 4 };
   
/* dir is to enemy (or empty square if no enemy, or zero if 4 friends), dir2 is to friend */
/* threatened groups in connection already handled */

static int cnpatonepoint(group_t g, group_t g2, int c, conn_t cn, sqr_t s,int dir,int dir2,listval_t ldrno, int *retpat) {
	int pat, o, cnval;
	pat = matchtypebest(ONEPOINTFILE, s, c, (listval_t)(cn + NUMGROUPS), &o, cn, readopj);
	cnval = gettype(pat);
	*retpat = pat;
	return cnval;
}
  
/* return AJI_CONNECT if 2 point jump at s and s2 is unbreakable.
 * unbreakable 2 point jumps are along edge or from two stone wall
 * needs lots more work!
 */
static int prot_two_point(group_t g, group_t g2,sqr_t s,sqr_t s2,conn_t cn, listval_t ldrno)
{
	sqr_t stone_g, stone_g2;
	int tmp, along_edge, enough_libs, near_edge, i, ldtmp;
	int two_stone_wall;
	int enemy_near;		/* enemy stone within 2 points of the connection */
	int enemy_near_1 = FALSE;  /* enemey stone within 2 points of protected conn point */
	list_t ptr;
	int weak_nbr = FALSE;		/* adjacent friendly stone is threatened */
	int enemy_below = FALSE;  /* enemy stones nearer to edge */
	int dir, dir2 = 1;	/* along conn and at right angles */
	char c;
#ifdef CHECK
	char buf[10];
#endif
	c = grcolor[g];
   	stone_g2 = s2 * 2 - s;
   	stone_g = s * 2 - s2;
  	if (board[stone_g] != g) {
		tmp = g;
		g = g2;
		g2 = tmp;
	}
	dir = s2 - s;
	if (dir == 1 || dir == -1) {
		dir2 = boardsize;
	}
   	if (board[stone_g] != g || board[stone_g2] != g2) {
#ifdef CHECK
		outerror("bad two point jump at ");
		outerror(ssqr(s, buf));
#endif
		return CANT_CONNECT;
	}
	addldrflag(s, ldrno);
	addldrflag(s2, ldrno);
	/* enemy stones threateneing connection */
	if (lnbf[s][1 - c] + lnbf[s2][1 - c] >= 3)
		return CANT_CONNECT;
	if (lnbf[s][1 - c] + lnbf[s2][1 - c] == 2)
		return twoenemytwopoint(s, s2, cn, c);

	if (grthreatened[g] || grthreatened[g2])
		return CAN_CONNECT;
   	along_edge = edge[s] == edge[s2];
	enemy_near = FALSE;
	for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
		if (lnbf[list[ptr]][1-c] != 0) {
			enemy_near = TRUE;
			if (ld[s] == 3)
				enemy_near_1 = TRUE;
			if (edge[list[ptr]] < edge[s])
				enemy_below = TRUE;
		}
	}
	for (ptr = nblbp[s2]; ptr != EOL; ptr = link[ptr]) {
		if (lnbf[list[ptr]][1-c] != 0) {
			enemy_near = TRUE;
			if (ld[s2] == 3)
				enemy_near_1 = TRUE;
			if (edge[list[ptr]] < edge[s2])
				enemy_below = TRUE;
		}
	}
	if (!S_NEUTRAL(s) && lnbf[s][c] > 1) {
		i = fdir[s];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			if (S_COLOR(s + nbr[i]) == c) {
				if (addlist((listval_t)(cn + NUMGROUPS), &ldrflag[s + nbr[i]])) {
					adflist((sqr_t)(s + nbr[i]), &grldr[cn + NUMGROUPS]);
				}
				if (S_THREATENED(s + nbr[i]) || S_NUMLIBS(s + nbr[i]) <= 2) {
					weak_nbr = TRUE;
					break;
				}
			}
		}
	}

	if (!weak_nbr && !S_NEUTRAL(s2) && lnbf[s2][c] > 1) {
		i = fdir[s2];
		for (ldtmp = ldir[i]; i < ldtmp; ++i) {
			if (S_COLOR(s2 + nbr[i]) == c) {
				if (addlist((listval_t)(cn + NUMGROUPS), &ldrflag[s2 + nbr[i]])) {
					adflist((sqr_t)(s2 + nbr[i]),&grldr[cn + NUMGROUPS]);
				}
				if (S_THREATENED(s2 + nbr[i]) || S_NUMLIBS(s2 + nbr[i]) <= 2) {
					weak_nbr = TRUE;
					break;
				}
			}
		}
	}

	/* enemy can capture friend on the connection point */
	if (weak_nbr) {
		return CAN_CONNECT;
	}
	
	enough_libs = (grlibs[g] > 3 || grlibs[g] == 3 && edge[stone_g] == 1 ||
					grlibs[g] == 3 && ld[s] > 4) && 
				  (grlibs[g2] > 3 || grlibs[g2] == 3 && edge[stone_g2] == 1 ||
					grlibs[g2] == 3 && ld[s2] > 4);

	if (!enough_libs) {
		return CAN_CONNECT;
	}

	if (!enemy_near) {
		return AJI_CONNECT;
	}

	near_edge = edge[s] <= 4;

	/* along the edge good connection */
   	if (near_edge && along_edge && !enemy_below &&
      		!S_NEUTRAL(s) && !S_NEUTRAL(s2) && !weak_nbr)
		return AJI_CONNECT;

	/* protected end */
	if (lnbf[s][c] > 1 || lnbf[s2][c] > 1) {
		if (lnbf[s][1 - c] || lnbf[s2][1 - c])
			return CAN_CONNECT;
		return AJI_CONNECT;
	}

   	two_stone_wall = lnbn[s] == 3 && lnbn[s2] == 3 && edge[s] > 1 && edge[s2] > 1 && 
			(S_COLOR(s - dir + dir2) == c || 
			 S_COLOR(s - dir - dir2) == c || 
			 S_COLOR(s2 + dir + dir2) == c || 
			 S_COLOR(s2 + dir - dir2) == c);

	if (two_stone_wall &&
		ld[s] > 3 && cntplyhere(s, c, ldrno) == NO)
		two_stone_wall = FALSE;
	if (two_stone_wall &&
		ld[s2] > 3 && cntplyhere(s2, c, ldrno) == NO)
		two_stone_wall = FALSE;

   	if (two_stone_wall && !enemy_near_1) {
		return AJI_CONNECT;
	}
	return CAN_CONNECT;
}

/* two point jump with two enemy stones next to it */

static int twoenemytwopoint(sqr_t s, sqr_t s2, conn_t cn, char c) {
	sqr_t sg1 = NOSQUARE, sg2 = NOSQUARE, sn;	/* points of the two enemy stones */
	sqr_t so, s2o;  /* oppisite points from stones */ 
	int i, ldtmp;
	int dir, dir2;	/* direction to first enemy stone */
	/* first, two stones adjacent to the same point */
	if (lnbf[s][1-c] == 2) {
		if (lnbf[s2][c] > 1)return(CAN_CONNECT);
		if (checkforcut(NOSQUARE, s, s2, cn, c))
			return CANT_CONNECT;
		return CAN_CONNECT;
	}
	if (lnbf[s2][1-c] == 2) {
		if (lnbf[s][c] > 1)return(CAN_CONNECT);
		if (checkforcut(NOSQUARE, s2, s, cn, c))
			return CANT_CONNECT;
		return CAN_CONNECT;
	}
	/* two more cases:  stones on same side, and stones on opposite sides */
	i = fdir[s];
	for (ldtmp = ldir[i]; i != ldtmp; ++i) {
		sn = s + nbr[i];
		if (grcolor[board[sn]] == 1-c) {
			sg1 = sn;  /* direction to single enemy stone */
			dir = nbr[i];
			break;
		}
	}
	i = fdir[s2];
	for (ldtmp = ldir[i]; i != ldtmp; ++i) {
		sn = s2 + nbr[i];
		if (grcolor[board[sn]] == 1-c) {
			sg2 = sn;  /* direction to single enemy stone */
			dir2 = nbr[i];
			break;
		}
	}
	if (sg1 == NOSQUARE || sg2 == NOSQUARE)
		return CAN_CONNECT;	/* some strange problem... */

	if (dir != dir2) {
		/* on opposite sides */
		if (!checkforcut(NOSQUARE, s, s2, cn, c) ||
			!checkforcut(NOSQUARE, s2, s, cn, c))
			return CAN_CONNECT;
		return CANT_CONNECT;
	}

	/* on same side */
	if (edge[s] == 1 && edge[s2] == 1) {
		if (!checkforcut(NOSQUARE, s, s2, cn, c) ||
			!checkforcut(NOSQUARE, s2, s, cn, c))
			return CAN_CONNECT;
		return CANT_CONNECT;
	}
	so = s-dir;
	s2o = s2-dir;
	/* empty points opposite */
	if (board[so] == NOGROUP && board[s2o] == NOGROUP) {
		if (edge[so] == 1 && lnbf[so][c] ||
			edge[s2o] == 1 && lnbf[s2o][c])
			return AJI_CONNECT;	/* has part of a monkey jump */
		if (lnbf[so][1-c] == 0 && lnbf[s2o][1-c] == 0) {
			if (canconnect4(s, s2, s2o, so, cn, c) ||
				canconnect4(s2, s, so, s2o, cn, c))
				return CAN_CONNECT;
			return CANT_CONNECT;
		}
	}
	return(CANT_CONNECT);
}

/* evaluate extra large knight connection between g and g2.  s and s2 are the linkage
 * points.  (s is upper).  dir is direction from s to s2. g and g2 may be reversed.
 *
 *   s  +  sn  +  g2   dir ->
 *   g  +  sn2 +  s2
 *
 */

static int exlargeknight(group_t g, group_t g2, int c, conn_t cn, sqr_t s, sqr_t s2, int dir, listval_t ldrno) {
	int on_edge;
	sqr_t sn,sn2;

	cn = cn;
	on_edge = edge[s] <= 4 && edge[s2] <= 4 && edge[s] != edge[s2];
	if (on_edge) {
		if (edge[s2] < edge[s] && 
			(lnbf[s2][1-c] || lnbf[s2-dir][1-c]) ||
			edge[s] < edge[s2] &&
			(lnbf[s][1-c] || lnbf[s+dir][1-c]))
			on_edge = FALSE;  /* enemy stone between connection and edge */
		}
		
	sn = s + dir + dir;
	sn2 = s2-dir-dir;
#ifdef CNPATH
	addlist(sn, &cnpathptr[cn]);
	addlist((listval_t)(sn-dir), &cnpathptr[cn]);
	addlist((listval_t)(sn+dir), &cnpathptr[cn]);
	addlist(sn2, &cnpathptr[cn]);
	addlist((listval_t)(sn2-dir), &cnpathptr[cn]);
	addlist((listval_t)(sn2+dir), &cnpathptr[cn]);
#endif
	addldrflag(s,ldrno);
	addldrflag(s2,ldrno);
	addldrflag(sn,ldrno);
	addldrflag(sn2,ldrno);
	if (edge[s] == 1 && edge[s2] == 2 ||
	   edge[s] == 2 && edge[s2] == 1) {  /* large monkey jump */
		if (grlibs[g] >= 3 && grlibs[g2] >= 3)
			return(CAN_CONNECT);
		}
	if (G_THREATENED(g) || G_THREATENED(g2))return(CANT_CONNECT);
	if ((ld[s] >= 4 && !S_NEUTRAL(sn2) ||
		ld[s2] >= 4 && !S_NEUTRAL(sn)) &&
		grlibs[g] > 2 && grlibs[g2] > 2 &&
		(grlibs[g] >= 4 || grlibs[g2] >= 4)) {
		if (on_edge && lnbf[sn][1-c] == 0 &&
			lnbf[sn2][1-c] == 0 && 
			lnbf[sn+dir][1-c] == 0 && lnbf[sn2+dir][1-c] == 0 &&
			lnbf[sn-dir][1-c] == 0 && lnbf[sn2-dir][1-c] == 0
			)
			return(AJI_CONNECT);
		return(CAN_CONNECT);
		}
	if (lnbf[sn][1-c] != 0 && lnbf[sn2][1-c] != 0)
		return(CANT_CONNECT);
	if (grlibs[g] >= 4 && grlibs[g2] >= 4 && 
		lnbf[sn-dir][1-c] == 0 && lnbf[sn2+dir][1-c] == 0 &&
		lnbf[sn][1-c] == 0 && lnbf[sn+dir][1-c] == 0 &&
		lnbf[sn2][1-c] == 0 && lnbf[sn2-dir][1-c] == 0 && 
		(on_edge || ld[s] > 3 || ld[s2] > 3 || lnbf[sn][c] > 1 && !S_NEUTRAL(s) ||
	   lnbf[sn2][c] > 1 && !S_NEUTRAL(s2)))return(AJI_CONNECT);
	return(CAN_CONNECT);
	}


/* one or more long linkages, no linkages or links */

int canconnoll(group_t g, group_t g2, int c, conn_t cn, listval_t ldrno, int *type) {
	sqr_t s,s2,s3,s4;
	int delta, enemy_close, near_edge;
	if (cnollnum[cn] == 1) {
#ifdef CNPATH
		addlist(list[cnollptr[cn]], &cnpathptr[cn]);
#endif
		*type = CN_HALFEXTRAKNIGHT;
		return CANT_CONNECT;
		}
	if (cnollnum[cn] == 2) {
		s = list[cnollptr[cn]];
		s2 = list[link[cnollptr[cn]]];
#ifdef CNPATH
		addlist(s, &cnpathptr[cn]);
		addlist(s2, &cnpathptr[cn]);
#endif
		delta = s2-s;
		if (delta == 3 && board[s+1] == NOGROUP && board[s+2] == NOGROUP || 
			delta == 3*boardsize && board[s+boardsize] == NOGROUP && 
			board[s+2*boardsize] == NOGROUP) { /* 4 pt jump */
			if (board[s-(s2-s)/3] != g && board[s2+(s2-s)/3] != g) {
				*type = CN_UNKNOWN;
				return(CANT_CONNECT);
				}
			if (board[s-(s2-s)/3] != g2 && board[s2+(s2-s)/3] != g2) {
				*type = CN_UNKNOWN;
				return(CANT_CONNECT);
				}
			*type = CN_FOURPOINTJUMP;
			if (G_THREATENED(g) || G_THREATENED(g2))
				return(CANT_CONNECT);
			s3 = s+(s2-s)/3;
			s4 = s2-(s2-s)/3;
#ifdef CNPATH
			addlist(s3, &cnpathptr[cn]);
			addlist(s4, &cnpathptr[cn]);
#endif
			addldrflag(s,ldrno);
			addldrflag(s2,ldrno);
			addldrflag(s3,ldrno);
			addldrflag(s4,ldrno);
			if (lnbf[s][1-c] ||
				lnbf[s2][1-c] ||
				lnbf[s3][1-c] ||
				lnbf[s4][1-c])
				return CANT_CONNECT;
			enemy_close = distance[s][1-c] <= 3 || 
				distance[s2][1-c] <= 3 || 
				distance[s3][1-c] <= 3 ||
				distance[s4][1-c] <= 3;
			near_edge = edge[s] <= 4 && edge[s2] <= 4;
			if (!enemy_close && near_edge)
				return CAN_CONNECT;
			return MIGHT_CONNECT;
			}
		else if (delta == boardsize + 4) {
			*type = CN_EXTRALARGEKNIGHT;
			return(exlargeknight(g,g2,c,cn,s,s2,1, ldrno));
			}
		else if (delta == boardsize - 4) {
			*type = CN_EXTRALARGEKNIGHT;
			return(exlargeknight(g,g2,c,cn,s,s2,-1,ldrno));
			}
		else if (delta == 4*boardsize + 1) {
			*type = CN_EXTRALARGEKNIGHT;
			return(exlargeknight(g,g2,c,cn,s,s2,boardsize,ldrno));
			}
		else if (delta == 4*boardsize -1) {
			*type = CN_EXTRALARGEKNIGHT;
			return(exlargeknight(g,g2,c,cn,s,s2,boardsize,ldrno));
			}
		*type = CN_UNKNOWN;
		return(CANT_CONNECT);
		}
	*type = CN_MULTIPLE;
	if (G_THREATENED(g) || G_THREATENED(g2))
		return(CANT_CONNECT);
	return MIGHT_CONNECT;
	}

/* analyze a connection of two linkages, and no links */

int cntwolkgs(group_t g, group_t g2, int c, conn_t cn, listval_t ldrno, int *type)
{
	sqr_t s,s2,s3;
	int delta;
	s = list[cnllptr[cn]];
	s2 = list[link[cnllptr[cn]]];
#ifdef CNPATH
	addlist(s, &cnpathptr[cn]);
	addlist(s2, &cnpathptr[cn]);
#endif
	delta = s2-s;
	if (delta == 2 && board[s+1] == NOGROUP || 
		delta == 2*boardsize && board[s+boardsize] == NOGROUP) { /* 3 pt jump */
		*type = CN_THREEPOINTJUMP;
		if (board[s-(s2-s)/2] != g && board[s2+(s2-s)/2] != g) {
			*type = CN_UNKNOWN;
			return(CANT_CONNECT);
		}
		if (board[s-(s2-s)/2] != g2 && board[s2+(s2-s)/2] != g2) {
			*type = CN_UNKNOWN;
			return(CANT_CONNECT);
		}
#ifdef CNPATH
		addlist((listval_t)((s+s2)/2), &cnpathptr[cn]);
#endif
		if (G_THREATENED(g) || G_THREATENED(g2))
			return(CANT_CONNECT);
		return threepointjump(g, g2, c, cn, s, s2, &s3, ldrno);
	}
	else if (delta == boardsize + 3) {
		*type = CN_LARGEKNIGHT;
		return largeknight(g, g2, c, cn, s, s2, 1, ldrno);
	}
	else if (delta == boardsize - 3) {
		*type = CN_LARGEKNIGHT;
		return largeknight(g, g2, c, cn, s, s2, -1, ldrno);
	}
	else if (delta == 3*boardsize + 1) {
		*type = CN_LARGEKNIGHT;
		return largeknight(g, g2, c, cn, s, s2, boardsize, ldrno);
	}
	else if (delta == 3 * boardsize - 1) {
		*type = CN_LARGEKNIGHT;
		return largeknight(g, g2, c, cn, s, s2, boardsize, ldrno);
	}
	else {
		*type = CN_UNKNOWN;
		return CANT_CONNECT;
	}
}

/* evaluate a three point jump, return point to play to connect */
/* called from outside, so ldrno might be NOGROUP */

int threepointjump(group_t g, group_t g2, int c, conn_t cn, sqr_t s, sqr_t s2, sqr_t *sr, listval_t ldrno) {
	sqr_t s3;
	int near_edge,safe,tmp;
	list_t ptr;
	int enemy_close = FALSE;

	cn = cn;
	s3 = (s2+s)/2;  /* center point */
	*sr = s3;
	addldrflag(s,ldrno);
	addldrflag(s2,ldrno);
	addldrflag(s3,ldrno);
	if (lnbf[s][1-c] + lnbf[s2][1-c] + lnbf[s3][1-c] > 1)
		return(CANT_CONNECT);
	if (!inlist(s,&grlbp[g])) {   /* s is liberty of g */
		tmp = g;
		g = g2;
		g2 = tmp;
		}
	if ((edge[s] > 3 || edge[s2] > 3) &&
	   (grlibs[g] < 4 || grlibs[g2] < 4 ||
		lnbf[s][1-c] != 0 && lnbn[s2] == 2 ||
	  	lnbf[s2][1-c] != 0 && lnbn[s] == 2 ||
		lnbf[s3][1-c] != 0))return(CANT_CONNECT);
	if (edge[s] > 1 &&  edge[s2] > 1 && (grlibs[g] < 4 && grlibs[g2] < 4 ||
		grlibs[g] < 3 || grlibs[g2] < 3))
		return(CANT_CONNECT);
	if (lnbf[s3][1-c] > 0 && (lnbf[s][1-c] > 0 || lnbf[s2][1-c] > 0))
		return(CANT_CONNECT);
	if (edge[s] == 1 && grlibs[g] < 3 && S_NEUTRAL(s) ||
	   edge[s2] == 1 && grlibs[g2] < 3 && S_NEUTRAL(s2))
	   	return CANT_CONNECT;
	if (lnbf[s][1-c] > 0 || lnbf[s2][1-c] > 0)
		return MIGHT_CONNECT;
	enemy_close = distance[s][1-c] <= 3 || distance[s2][1-c] <= 3 || distance[s3][1-c] <= 3;
	near_edge = edge[s] <= 4 && edge[s2] <= 4;
	safe = near_edge && !enemy_close && (ld[s] > 2 || ld[s2] > 2) &&
	   lnbf[s][1-c] == 0 && lnbf[s2][1-c] == 0 && lnbf[s3][1-c] == 0 &&
		grlibs[g] > 3 && grlibs[g2] > 3;
	if (safe) {
		if (ld[s] > 2) {
			for (ptr = nbgrp[s][c]; ptr != EOL; ptr = link[ptr]) {
				if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]]))
					adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
				if (G_NUMLIBS(list[ptr]) <= 2 || G_THREATENED(list[ptr]))
					safe = FALSE;
				}
			}
		if (ld[s2] > 2) {
			for (ptr = nbgrp[s2][c]; ptr != EOL; ptr = link[ptr]) {
				if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]]))
					adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
				if (G_NUMLIBS(list[ptr]) <= 2 || G_THREATENED(list[ptr]))
					safe = FALSE;
				}
			}
		if (safe)
			return(AJI_CONNECT);
		}
	if (grlibs[g] < 4)*sr = s;
	else if (grlibs[g2] < 4)*sr = s2;
	else if (S_NEUTRAL(s))*sr = s;
	else if (S_NEUTRAL(s2))*sr = s2;
	else if (S_NEUTRAL(s3))*sr = s3;
	return(CAN_CONNECT);
	}

/* evaluate large knight connection between g and g2.  s and s2 are the linkage
 * points.  (s is upper).  dir is direction from s to s2. g and g2 may be reversed.
 *
 *   s  +   sn  g2    dir ->
 *   g  sn2 +   s2
 *
 */

static int largeknight(group_t g, group_t g2, int c, conn_t cn, sqr_t s, sqr_t s2, int dir, listval_t ldrno)
{
	int on_edge;
	sqr_t sn,sn2;

	cn = cn;
	on_edge = edge[s] <= 4 && edge[s2] <= 4 && edge[s] != edge[s2];
	if (on_edge) {
		if (edge[s2] < edge[s] && 
			(lnbf[s2][1 - c] || lnbf[s2 - dir][1 - c] || lnbf[s2 - dir - dir][1 - c]) ||
			edge[s] < edge[s2] &&
			(lnbf[s][1 - c] || lnbf[s + dir][1 - c] || lnbf[s + dir + dir][1 - c])) {
			on_edge = FALSE;  /* enemy stone between connection and edge */
		}
	}
		
	sn = s + dir + dir;
	sn2 = s2 - dir - dir;
#ifdef CNPATH
	addlist(sn, &cnpathptr[cn]);
	addlist(sn2, &cnpathptr[cn]);
	addlist((listval_t)(s + dir), &cnpathptr[cn]);
	addlist((listval_t)(s2 - dir), &cnpathptr[cn]);
#endif
	addldrflag(s, ldrno);
	addldrflag(s2, ldrno);
	addldrflag(sn, ldrno);
	addldrflag(sn2, ldrno);
	if (edge[s] == 1 && edge[s2] == 2 ||
	   edge[s] == 2 && edge[s2] == 1) {  /* monkey jump */
		if (grlibs[g] >= 3 && grlibs[g2] >= 3 &&
			!G_THREATENED(g) && !G_THREATENED(g2))
			return(AJI_CONNECT);
		else
			return(CAN_CONNECT);
	}
	if (G_THREATENED(g) || G_THREATENED(g2))
		return(CANT_CONNECT);
	if ((ld[s] >= 4 && !S_NEUTRAL(sn2) ||
		ld[s2] >= 4 && !S_NEUTRAL(sn)) &&
		grlibs[g] > 2 && grlibs[g2] > 2 &&
		(grlibs[g] >= 4 || grlibs[g2] >= 4)) {
		if (on_edge && lnbf[s + dir][1 - c] == 0 && lnbf[sn2 + dir][1 - c] == 0)
			return AJI_CONNECT;
		return CAN_CONNECT;
	}
	if (lnbf[sn][1 - c] != 0 && lnbf[sn2][1 - c] != 0)
		return CANT_CONNECT;
	if (grlibs[g] >= 4 && grlibs[g2] >= 4 && 
		lnbf[s + dir][1 - c] == 0 && lnbf[sn2 + dir][1 - c] == 0 &&
		(on_edge || ld[s] > 3 || ld[s2] > 3 || lnbf[sn][c] > 1 && !S_NEUTRAL(s) ||
	   lnbf[sn2][c] > 1 && !S_NEUTRAL(s2)))
	   return AJI_CONNECT;
	return CAN_CONNECT;
}
  
/* analyze a connection of two links */

int linktypes[41];
    /* 1-2pt jump. 2-nights move. 0-neither.  types by distance */
 
int cntwolinks(group_t g, group_t g2, int c, conn_t cn, sqr_t s, sqr_t s2, listval_t ldrno, int *ctype, int dbg) {
	sqr_t sn,sn2;
	sqr_t stone_g,stone_g2;
	int delta,type,tmp,from_4_to_2;
	int two_point_jump,knights_move,enough_libs;
	int near_edge,along_edge,prot_shape,one_side_threatened,captatari;
	list_t ptr;
	int res1, res2;
#ifdef NEVER
	char buf[100];
#endif	
	one_side_threatened = 
		G_THREATENED(g) || G_THREATENED(g2);
/*	s = list[cnlkptr[cn]];
	s2 = list[link[cnlkptr[cn]]];  5/99 */
	delta = s2-s;
	if (delta > 39)delta = 40;	/* 39 is biggest knight's move, so 40 is unknown */
	type = linktypes[delta];

	two_point_jump = type == 1;
	if (two_point_jump) {
		if (board[s+s-s2] != g && board[s2+s2-s] != g)
			two_point_jump = FALSE;
		if (board[s+s-s2] != g2 && board[s2+s2-s] != g2)
			two_point_jump = FALSE;
		}
	if (two_point_jump) {
		*ctype = CN_TWOPOINTJUMP;
/*		if (one_side_threatened)return(CANT_CONNECT); too conservative */
#ifdef CNPATH
		addlist(s, &cnpathptr[cn]);
		addlist(s2, &cnpathptr[cn]);
#endif
		return(prot_two_point(g,g2,s,s2,cn,ldrno));
		}
	knights_move = type == 2;

	/* knight's move relationship between s and s2.  find other points */
	sn = s;
	sn2 = s2;
	delta = xval[s2]-xval[s];
	if (delta == 2 || delta == -2) {
         stone_g = s2 - delta;
         stone_g2 = s + delta;
         sn = s2 - delta/2;
         sn2 = s + delta/2;
         }
	else {
         stone_g = s2 - boardsize*2;
         stone_g2 = s + boardsize*2;
         sn = s2 - boardsize;
         sn2 = s + boardsize;
         }
	if (board[stone_g] != g) {
         tmp = g;
         g = g2;
         g2 = tmp;
         }

	if (!knights_move || board[sn] != NOGROUP || board[sn2] != NOGROUP) {
		/* not a real knight's move since a stone is in the way */
		*ctype = CN_MULTIHALFKNIGHT;
		res1 = canconnlink(cn,list[cnlkptr[cn]],&s,&s2,ldrno);
#ifdef CNPATH
		addlist(s, &cnpathptr[cn]);
		addlist(s2, &cnpathptr[cn]);
#endif
		res2 = canconnlink(cn,list[link[cnlkptr[cn]]],&s,&s2,ldrno);
#ifdef CNPATH
		addlist(s, &cnpathptr[cn]);
		addlist(s2, &cnpathptr[cn]);
#endif
		if (res1 == AJI_CONNECT && res2 == AJI_CONNECT)
			return AJI_CONNECT;
		return(CAN_CONNECT);
	}

		 
	/* now check knights move to see if it is unbreakable 
	 *
	 *	stone_g2 O  sn2 s
	 *	         s2 sn  O stone_g
	 */


	*ctype = CN_KNIGHTSMOVE;
#ifdef CNPATH
	addlist(s2, &cnpathptr[cn]);
	addlist(sn, &cnpathptr[cn]);
	addlist(sn2, &cnpathptr[cn]);
	addlist(s, &cnpathptr[cn]);
#endif
	if (one_side_threatened)return(CAN_CONNECT);
   addldrflag(sn,ldrno);
    addldrflag(sn2,ldrno);
	if (board[stone_g] != g || board[stone_g2] != g2) {
#ifdef NEVER
		sprintf(buf,"bad knights linkage g %d g2 %d stone_g stoneg2 s sn s2 sn2 \n",g,g2);
		outerror(buf);
		outerror(ssqr(stone_g,buf));
		outerror(ssqr(stone_g2,buf));
		outerror(ssqr(s,buf));
		outerror(ssqr(sn,buf));
		outerror(ssqr(s2,buf));
		outerror(ssqr(sn2,buf));
		outerror("\n");
#endif
		*ctype = CN_UNKNOWN;
		return CANT_CONNECT;
		}

	captatari = FALSE;  /* friendly stone can be captured or ataried */
	if (link[nbgrp[sn][c]] != EOL)
		for (ptr = nbgrp[sn][c]; ptr != EOL; ptr = link[ptr]) {
			if (addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]]))
				adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
			if (grlibs[list[ptr]] <= 2)
				captatari = TRUE;
			}

	if (link[nbgrp[sn2][c]] != EOL)
		for (ptr = nbgrp[sn2][c]; ptr != EOL; ptr = link[ptr]) {
			if (addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]]))
				adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
			if (grlibs[list[ptr]] <= 2)
				captatari = TRUE;
			}

	enough_libs = (grlibs[g] > 3 ||
					grlibs[g] == 3 && !S_NEUTRAL(s) && lnbn[s] <= 2 ||
					grlibs[g] == 3 && edge[stone_g] == 1) && 
                  (grlibs[g2] > 3 || 
					grlibs[g2] == 3 && !S_NEUTRAL(s2) && lnbn[s2] <= 2 ||
                  	grlibs[g2] == 3 && edge[stone_g2] == 1);

	prot_shape = (ld[s] > 4 && !S_NEUTRAL(sn2) && (lnbf[sn2+sn2-sn][1-c] < 2 || S_NEUTRAL(sn2+sn2-s2))) || 
		(ld[s2] > 4 && !S_NEUTRAL(sn) && edge[sn] > 1 && (lnbf[sn+sn-sn2][1-c] < 2 || S_NEUTRAL(sn+sn-sn2)));


	if (!captatari && enough_libs && prot_shape)return(AJI_CONNECT);

	along_edge = g2abs(edge[s] - edge[s2]) < 2;

	near_edge = edge[s] <= 4 && edge[s2] <= 4;
#ifdef NEVER
	if (near_edge) {
    will read it below 
		if (edge[s] < edge[s2] && (lnbf[s][1-c] != 0 || lnbf[sn2][1-c] != 0))
			near_edge = FALSE;
		if (edge[s2] < edge[s] && (lnbf[s2][1-c] != 0 || lnbf[sn][1-c] != 0))
			near_edge = FALSE;
		}
#endif

	from_4_to_2 = edge[s2] == 4 && edge[s] == 2 ||
		edge[s2] == 2 && edge[s] == 4;

	if (grlibs[g] <= 2 || grlibs[g2] <= 2)
		return(CAN_CONNECT);
	if (captatari)
		return(CAN_CONNECT);
	if (playlevel < READCONNLEVEL && enough_libs && near_edge && along_edge) {  /* probably connected */
		if (edge[s] < edge[s2] && (lnbf[s][1-c] != 0 || lnbf[sn2][1-c] != 0))
			return(CAN_CONNECT);
		if (edge[s2] < edge[s] && (lnbf[s2][1-c] != 0 || lnbf[sn][1-c] != 0))
			return(CAN_CONNECT);
		return(AJI_CONNECT);
		}
#ifdef NEVER
	if (enough_libs && near_edge && along_edge) {  /* probably connected */
		if (canpushcut3(sn,sn2,s,cn,c,FALSE) ||
			canpushcut3(sn2,sn,s2,cn,c,FALSE))
			return(CAN_CONNECT);
		return(AJI_CONNECT);  /* near edge default is can connect */
		}   
#endif		
	if (canpushcut3(sn,sn2,s,cn,(char)c,dbg))
		return(CAN_CONNECT);
	if (canpushcut3(sn2,sn,s2,cn,(char)c,dbg))
		return(CAN_CONNECT);
	return(AJI_CONNECT);
}

/*
 * play the cutting sequence in s (until NOPOINT), starting color c, alternating
 * check if any stones in capture(until NOPOINT) can be captured
 * return FALSE if any group in *capture is captured (may not read all of them),
 * or any move is illegal
 */ 
static int cutsequence(sqr_t *moves, int c, sqr_t *capture, conn_t cn, int dbg)
{
	group_t g;
	sqr_t tmp;
	int retval = TRUE;
	int count = 0;	/* number moves made */
	int old_debug = debug;
	listval_t ldrno = cn + NUMGROUPS;

	/* make all the moves */
	while (*moves != NOPOINT) {
		if (board[*moves] != NOGROUP) {
#ifdef CHECK
			outerror("cutsequence told to play on stone");
			outstone(*moves, "1");
#endif
			retval = FALSE;
			break;
		}
		mvs[msptr] = *moves++;
		mvcolor[msptr] = c;
		c = 1 - c;
		upldrflags(msptr, ldrno);
		count++;
#ifdef CHECK
		if (dbg)
			fixlm(mvs[msptr], mvcolor[msptr]);
#endif
		if (!lupdate(msptr++)) {
			retval = FALSE;
			break;
		}
	}

	/* check for captured stones */
	while (retval && *capture != NOPOINT) {
		g = board[*capture++];
		if (g == NOGROUP) {
			retval = FALSE;
			break;		/* captured */
		}
#ifdef CHECK
		if (dbg) {
			debug = 2000 + g;
		}
#endif
		if (grlibs[g] <= conntaclibs[playlevel] &&
			iscaptured(g, conncapdepth[playlevel], conncapsize[playlevel], conntaclibs[playlevel], connmost[playlevel], c, ldrno, &tmp, 1-c)) {
			retval = FALSE;
			break;
		}
	}

	/* take back the moves */
	while (count) {
		--msptr;
		ldndate(msptr);
#ifdef CHECK
		if (dbg)
			fixsd(msptr, FALSE);
#endif
	}

	debug = old_debug;
	return retval;
}

/* does the sequence s1,s2,s3 push through the nights move.
 * after the sequence can the groups at s1 or s3 be captured.
 * return TRUE if they can't be captured (if push through and cut works)
 */

static int canpushcut3(sqr_t s1, sqr_t s2, sqr_t s3, conn_t cn, char c, int dbg) {
	group_t g;
	listval_t ldrno;
	int mlibs,flag;
	sqr_t tmp;
#ifdef CHECK
	int olddebug;
	olddebug = debug;
#endif
	ldrno = cn + NUMGROUPS;
	if (board[s1] != NOGROUP || board[s2] != NOGROUP) {
#ifdef CHECK
		outerror("canpushcut told to play on stone");
		outstone(s1,"1");
		outstone(s2,"2");
		outstone(s3,"3");
#endif
		return FALSE;
		}
	mvs[msptr] = s1;
	mvcolor[msptr] = 1-c;
	flag = lupdate(msptr);
	upldrflags(msptr,ldrno);
	if (flag) {
#ifdef CHECK
		if (dbg)
			fixlm(s1, 1 - c);
#endif
		++msptr;
		mvs[msptr] = s2;
		mvcolor[msptr] = c;
		flag = lupdate(msptr);
		upldrflags(msptr,ldrno);
		if (flag) {
#ifdef CHECK
			if (dbg)
				fixlm(s2, c);
#endif
			++msptr;
			if (S_COLOR(s3) == 1-c && S_NUMLIBS(s3) > 1) { /* already stone at s3 */
				g = board[s1];
#ifdef CHECK
				if (dbg)
					debug = 2000+g;
#endif
				mlibs = conntaclibs[playlevel];
				/* if (edge[s1] < 4)mlibs = 3; */
	            if (g == NOGROUP)
                	flag = FALSE;
				else if (grlibs[g] <= mlibs)
					flag = !iscaptured(g,conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],1-c,ldrno,&tmp,1-c);
			}
			else {
				if (S_COLOR(s3) == 1-c && S_NUMLIBS(s3) == 1)
					mvs[msptr] = list[grlbp[board[s3]]];
				else
					mvs[msptr] = s3;
				mvcolor[msptr] = 1-c;
				flag = lupdate(msptr);
				upldrflags(msptr,ldrno);
				if (flag) {
					g = board[s1];
#ifdef CHECK
					if (dbg) {
						debug = 2000+g;
						fixlm(s3, 1 - c);
					}
#endif
					++msptr;
					mlibs = conntaclibs[playlevel];
					/* if (edge[s1] < 4)mlibs = 3; */
	                if (g == NOGROUP)
    	            	flag = FALSE;
					else if (grlibs[g] <= mlibs)
						flag = !iscaptured(g,conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],c,ldrno,&tmp,1-c);
					if (flag && board[s3] != g) {
						g = board[s3];
						mlibs = conntaclibs[playlevel];
						/* if (edge[s3] < 4)mlibs = 3; */
                	
#ifdef CHECK
						if (dbg)
							debug = 2000+g;
#endif
	                	if (g == NOGROUP)
	    	            	flag = FALSE;
						else if (grlibs[g] <= mlibs)
							flag = !iscaptured(g,conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],c,ldrno,&tmp,1-c);
						}
					--msptr;
#ifdef CHECK
					if (dbg)
						fixsd(msptr,FALSE);
#endif
					}
				ldndate(msptr);
				}
			--msptr;
#ifdef CHECK
			if (dbg)
				fixsd(msptr,FALSE);
#endif
			}
		ldndate(msptr);
		--msptr;
#ifdef CHECK
		if (dbg)
			fixsd(msptr,FALSE);
#endif
		}
	ldndate(msptr);
#ifdef CHECK
	if (dbg)
		debug = olddebug;
#endif
	return(flag);
	}

/* does the sequence s1, s2, s3, s4 connect the groups in cn?
 * connected if s2 or s4 can be captured when c moves first (return TRUE)
 */

static int canconnect4(sqr_t s1, sqr_t s2, sqr_t s3, sqr_t s4, conn_t cn, char c) {
	int ret;
	listval_t ldrno;
	ldrno = cn + NUMGROUPS;
	if (board[s1] != NOGROUP || board[s2] != NOGROUP || board[s3] != NOGROUP || board[s4] != NOGROUP) {
#ifdef CHECK
		outerror("canconnect4 told to play on stone");
		outstone(s1,"1");
		outstone(s2,"2");
		outstone(s3,"3");
		outstone(s4,"4");
#endif
		return FALSE;
	}
	mvs[msptr] = s1;
	mvcolor[msptr] = c;
	ret = lupdate(msptr);
	upldrflags(msptr,ldrno);
	if (ret) {
		++msptr;
		ret = !canpushcut3(s2, s3, s4, cn, c, FALSE);
		--msptr;
	}
	ldndate(msptr);
	return ret;
}



/* does the sequence s1,s2 push through the half knight's move.
 * after the sequence can one of the groups at s1 or s3 be captured.
 * (if they move first)
 * return TRUE if they can't be captured (if push through and cut works)
 *    -make the two moves at s1 and s2
 *    -if s1 captured if it moves first, done, return false
 *    -if s3 captured if it moves first, done, return false
 *    - now check that both:
 *        s1 makes saving move, s3 captured if it moves second, and
 *        s3 makes saving move, s1 captured if it moves second
 *        if both are true, return false
 * finally, return true if none of above
 */

static int canpushcut2(sqr_t s1, sqr_t s2, sqr_t s3, conn_t cn, char c, int dbg) {
	group_t g;
	listval_t ldrno;
	int mlibs,flag;
	sqr_t tmp;
	sqr_t save1=NOSQUARE, save2=NOSQUARE;  /* moves that save s1 and s3 if they move first */
#ifdef CHECK
	int olddebug;
	olddebug = debug;
#endif
	ldrno = cn + NUMGROUPS;
	if (board[s1] != NOGROUP || board[s2] != NOGROUP) {
#ifdef CHECK
		outerror("canpushcut told to play on stone");
		outstone(s1,"1");
		outstone(s2,"2");
		outstone(s3,"3");
#endif
		return FALSE;
		}
	mvs[msptr] = s1;
	mvcolor[msptr] = 1-c;
	flag = lupdate(msptr);
	upldrflags(msptr,ldrno);
	if (flag) {
#ifdef CHECK
		if (dbg)
			fixlm(s1, 1 - c);
#endif
		++msptr;
		mvs[msptr] = s2;
		mvcolor[msptr] = c;
		flag = lupdate(msptr);
		upldrflags(msptr,ldrno);
		if (flag) {
#ifdef CHECK
			if (dbg)
				fixlm(s2, c);
#endif
			++msptr;
			g = board[s1];
#ifdef CHECK
			if (dbg)
				debug = 2000+g;
#endif
			mlibs = conntaclibs[playlevel];
			/* if (edge[s1] < 4)mlibs = 3; */
            if (g == NOGROUP)
               	flag = FALSE;
			else if (grlibs[g] <= mlibs)
				flag = !iscaptured(g,conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],1-c,ldrno,&save1,NOCOLOR);
			if (flag) {  /* first group can escape */
				g = board[s3];
				mlibs = conntaclibs[playlevel];
				/* if (edge[s3] < 4)mlibs = 3; */
                	
#ifdef CHECK
				if (dbg)
					debug = 2000+g;
#endif
               	if (g == NOGROUP)
   	            	flag = FALSE;
				else if (grlibs[g] <= mlibs)
					flag = !iscaptured(g,conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],1-c,ldrno,&save2,NOCOLOR);
				if (flag && save1 != NOSQUARE && save2 != NOSQUARE && save1 != PASS && save2 != PASS) {
					/* now need to read both together */
					mvs[msptr] = save1;
					mvcolor[msptr] = 1-c;
					tmp = lupdate(msptr);
					upldrflags(msptr,ldrno);
					if (tmp) {
						++msptr;
						flag = !iscaptured(g,conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],c,ldrno,&save2,NOCOLOR);
						--msptr;
						}
					ldndate(msptr);
					if (!flag) {	/* FIRST TRY CAPTURED, TRY SECOND ONE */
						mvs[msptr] = save2;
						mvcolor[msptr] = 1-c;
						tmp = lupdate(msptr);
						upldrflags(msptr,ldrno);
						if (tmp) {
							++msptr;
							flag = !iscaptured(board[s1],conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],c,ldrno,&save2,NOCOLOR);
							--msptr;
							}
						ldndate(msptr);
						}
					}
				}
			--msptr;
#ifdef CHECK
			if (dbg)
				fixsd(msptr,FALSE);
#endif
			}
		ldndate(msptr);
		--msptr;
#ifdef CHECK
		if (dbg)
			fixsd(msptr,FALSE);
#endif
		}
	ldndate(msptr);
#ifdef CHECK
	if (dbg)
		debug = olddebug;
#endif
	return(flag);
	}


/* does the sequence s2,s3 connect? (s2 trys to connect, s3 cuts)
 * after the sequence can the group at s1 or s3 be captured.
 * s1 can be NOSQUARE.
 *  c is color to move first, (trying to connect)
 * return TRUE if can't be captured (if cut works)
 */

static int checkforcut(sqr_t s1, sqr_t s2, sqr_t s3,conn_t cn, char c) {
	group_t g;
	listval_t ldrno;
	int mlibs,flag;
	sqr_t tmp;
	ldrno = cn + NUMGROUPS;
	mvs[msptr] = s2;
	mvcolor[msptr] = c;
	flag = lupdate(msptr);
	upldrflags(msptr,ldrno);
	if (flag) {
		++msptr;
		mvs[msptr] = s3;
		mvcolor[msptr] = 1-c;
		flag = lupdate(msptr);
		upldrflags(msptr,ldrno);
		if (flag) {
			++msptr;
			if (s1 != NOSQUARE) {
				g = board[s1];
				mlibs = conntaclibs[playlevel];
				/* if (edge[s1] < 4)mlibs++; */
				if (grlibs[g] <= mlibs)
					flag = !iscaptured(g,conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],c,ldrno,&tmp,1-c);
				}
			if (flag) {
				g = board[s3];
				mlibs = conntaclibs[playlevel];
				/* if (edge[s3] < 4)mlibs++; */

				if (grlibs[g] <= mlibs)
					flag = !iscaptured(g,conncapdepth[playlevel],conncapsize[playlevel],mlibs,connmost[playlevel],c,ldrno,&tmp,1-c);

				}
			--msptr;
			}
		ldndate(msptr);
		--msptr;
		}
	ldndate(msptr);
	return(flag);
	}

/* figure out protection value for diagonal connection 
 *      (sn)
 *       O
 *     @ S @
 *    SO2@ SO
 *
 *
 *  dir is UP, toward enemy from S
 * s1 and s2 are the empty points in the connection
 * NOTE: doesn't handle case where both sides are peeped?
 */


int protdiag(conn_t cn, sqr_t s1, sqr_t s2, int c) {
	list_t ptr;
	sqr_t s,sn = 0,so,so2;
	int i;
	group_t g1,g2;
	int ldtmp,dir = 1;
	listval_t ldrno;
	if (!S_NEUTRAL(s1) && !S_NEUTRAL(s2))return(SOLID_CONNECT);
	g1 = cngr1[cn];
	g2 = cngr2[cn];
	ldrno = NUMGROUPS+cn;
	for (ptr = cnptr[cn]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];  /* s is connection that is peeped */
		if (!S_NEUTRAL(s))continue;  /* find point where enemy is peeping at one of diagonal connections */
		if (lnbf[s][c] != 3)continue;
		if (lnbf[s][1-c] != 1)continue;
		if (link[cnbrd[s]] == EOL)continue;
		i = fdir[s];
		for (ldtmp = ldir[i]; i != ldtmp; ++i) {
			sn = s + nbr[i];
			if (grcolor[board[sn]] == 1-c) {
				dir = nbr[i];  /* direction to single enemy stone */
				break;
				}
			}
		if (addlist(ldrno,&ldrflag[sn]))
			adflist(sn,&grldr[ldrno]);
		if (S_THREATENED(sn)) {
			continue;  /* sn has enemy stone on it */
			}
		if (gralive[board[sn]] == DEAD)continue;
		if (grlibs[board[sn]] == 2)continue;
		so = s1;
		if (s1 == s)so = s2;  /* so is other diagonal connection */

		/* is other diagonal defended */
		if (lnbf[so][1-c] == 0) {
			if (lnbn[so] <= 1)continue;
		/*	if (edge[so] == 2 && edge[s] == 3)continue; */
			if (ld[so] >= 6)continue;
			}
		

		i = fdir[s];
		for (ldtmp = ldir[i]; i != ldtmp; ++i) {
			sn = s + nbr[i];
			if (grcolor[board[sn]] == c && board[sn] != g1 &&
			   board[sn] != g2) {
				if (addlist(ldrno,&ldrflag[sn]))
					adflist(sn,&grldr[ldrno]);
				if (grlibs[board[sn]] == 2 &&
					!G_THREATENED(board[sn]) &&
					(grsize[board[sn]] > 2 ||
					grsize[g1] == 1 || grsize[g2] == 1)) {
					cnshcent[cn] = board[s-dir];
					return(SHARED_CONNECT);  /* can half cut diagonal with atari */
					}
				}
			}
		so2 = (signed)s*2 - dir*2 - (signed)so;  /* other shared connection */
		if (addlist((listval_t)(NUMGROUPS+cn),&ldrflag[so2]))
			adflist(so2,&grldr[NUMGROUPS+cn]);
		if (ld[so2] >= 6)continue;
		if (grcolor[board[so2]] == 1-c)continue;  /* already cut */
		if (grcolor[lgr[so2]] == 1-c) { 
			if (addlist((listval_t)(NUMGROUPS+cn),&ldrflag[mvs[grpieces[lgr[so2]]]]))
				adflist(mvs[grpieces[lgr[so2]]],&grldr[NUMGROUPS+cn]);
		   if (gralive[lgr[so2]] == DEAD)continue;  /* really should read it */
		   }
		if (lnbf[so2][1-c] == 0) {
			if (edge[so2] <= 1 || S_COLOR(so2+so2-s) == 1-c &&
				S_ALIVE(so2+so2-s) == DEAD)
				continue;  /* optimistic can't cut */
			}
		if (!canpushcut3(s,so,so2,cn,(char)c,FALSE))
			continue;
		if (!canpushcut3(s,so2,so,cn,(char)c,FALSE))
			continue;
		cnshcent[cn] = board[s-dir];
		return(SHARED_CONNECT);
		}
	return(SOLID_CONNECT);
	}


/* check a hane or diagonal connection to see if it has a common connection point with a one point
 * jump that is shared or was shared.  If so, demote its connection to be the same
 * as the one point jump
 */

void checkfixhane(conn_t cn) {
	int oldprot = cnprot[cn];
	list_t ptr, cp;

	for (cp = cnptr[cn]; cp != EOL; cp = link[cp])
		for (ptr = cnbrd[list[cp]]; ptr != EOL; ptr = link[ptr])
			if (cntype[list[ptr]] == CN_ONEPOINTJUMP && cnprot[list[ptr]] == SHARED_CONNECT) {
				cnprot[cn] = cnprot[list[ptr]];
				cnshcent[cn] = cnshcent[list[ptr]];
				fixconnarmies(cn, oldprot);
				return;
			}
}

/* reevaluate connections.  if everything is 2, do all connections on board */

void fixcnprot(int everything) {
	list_t ptr, ptr2, ptr3, was_shared = EOL;
	conn_t cn;
	int oldprot;
	if (everything == 2) {
		for (cn = 0; cn < NUMCONNS; ++cn)
			if (cnlknum[cn] != 0 || cncnum[cn] != 0 || cnllnum[cn] != 0 || 
				cnollnum[cn] != 0 || cnddnum[cn] != 0)
				fixaprot(cn);
		for (cn = 0; cn < NUMCONNS; ++cn)
			if ((cntype[cn] == CN_HANE || cntype[cn] == CN_DIAGONAL) && PROTCONN(cn))
				checkfixhane(cn);
	}
	else {
		for (ptr = cnchgd; ptr != EOL; ptr = link[ptr]) {
			cn = list[ptr];
			oldprot = cnprot[cn];
			fixaprot(cn);
			if (oldprot == SHARED_CONNECT && cnprot[cn] != SHARED_CONNECT && 
				cntype[cn] == CN_ONEPOINTJUMP)
				addlist(cn, &was_shared);
		}
		/* shared connection became unshared, so diag and hane that share points must be reevaluated */
		for (ptr = was_shared; ptr != EOL; ptr = link[ptr]) {
			for (ptr2 = cnptr[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {	/* each point in the connection */
				for (ptr3 = cnbrd[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3]) {
					if ((cntype[list[ptr3]] == CN_HANE || cntype[list[ptr3]] == CN_DIAGONAL) && cnprot[list[ptr3]] == SHARED_CONNECT)
						fixaprot(list[ptr3]);
				}
			}
		}
		killist(&was_shared);
		for (ptr = cnchgd; ptr != EOL; ptr = link[ptr]) {
			for (ptr2 = cnptr[list[ptr]]; ptr2 != EOL; ptr2 = link[ptr2]) {	/* each point in the connection */
				for (ptr3 = cnbrd[list[ptr2]]; ptr3 != EOL; ptr3 = link[ptr3]) {
					if ((cntype[list[ptr3]] == CN_HANE || cntype[list[ptr3]] == CN_DIAGONAL) && PROTCONN(list[ptr3]))
						checkfixhane(list[ptr3]);
				}
			}
		}
	}
	killist(&cnchgd);
}

#ifdef CHECK

int getprot(conn_t cn)
{
	group_t g, g2;
	int c;
	int prot = CANT_CONNECT, type;
	listval_t ldrno;
	sqr_t s,s2; 
	int pat;
	return cnprot[cn];  /* for now - bugs */
	
	g = cngr1[cn];
	g2 = cngr2[cn];
	c = grcolor[g];
	ldrno = NUMGROUPS + cn;
	
	if (cncnum[cn] > 1)
		prot = cnprot[cn];
	else if (cncnum[cn] == 1)   /* single connection */
		prot = cnoneconn(g, g2, c, cn, ldrno, &type, &pat);
	else if (cnlknum[cn] > 2) {
		prot = cnmultlk(cn, c, g, g2, ldrno);
	}
	else if (cnlknum[cn] == 2)
                              /* check linkages for unbreakable conns */
		prot = cntwolinks(g, g2, c, cn, list[cnlkptr[cn]], list[link[cnlkptr[cn]]], ldrno, &type, FALSE);
	else if (cnlknum[cn] == 1) {
		prot = canconnlink(cn, list[cnlkptr[cn]], &s, &s2, ldrno);
	}
	else if (cnllnum[cn] > 2) {
		prot = cnmlkgs(g, g2, c, cn, ldrno);
	}
	else if (cnllnum[cn] == 2) {
		prot = cntwolkgs(g, g2, c, cn, ldrno, &type);
	}
	else if (cnllnum[cn] == 1) {
		prot = canconnlkg(cn, &s, &s2, ldrno);
	}
    else if (cnddnum[cn] >= 1) {
    	prot = canconndd(g, g2, c, cn, ldrno, 0);
   	}
	else if (cnollnum[cn])
		prot = canconnoll(g, g2, c, cn, ldrno, &type);
    return prot;
}
    
#endif



static void evalprot(conn_t cn) 
{
	group_t g,g2;
	sqr_t s1,s2,s;
	int c,diff;
	int one_side_dead,no_two_libs,one_side_threatened;
	int type = CN_UNKNOWN;
	int pat = 0;
	listval_t ldrno = cn + NUMGROUPS;
	list_t ptr;
	
#ifdef CHECK
	char buf[100];
#endif
		
	cnpat[cn] = 0;
	g = cngr2[cn];
	g2 = cngr1[cn];
	c = grcolor[g];
	cnshcent[cn] = g2;	/* so it will be valid in case of bugs elsewhere */
	killist(&cnpathptr[cn]);	/* will set up a new path */

	if (g == NOGROUP || g2 == NOGROUP) {
#ifdef CHECK
		sprintf(buf,"bad group number in fixcnprot g1 %d g2 %d cn %d cncnum %d\n",
			g,g2,cn,cncnum[cn]);
		outerror(buf);
		outerror(ssqr(list[cnptr[cn]],buf));
#endif
		cnprot[cn] = CANT_CONNECT;
		return;
	}

	one_side_dead = 
		gralive[g] == DEAD || gralive[g2] == DEAD;
	one_side_threatened = 
		G_THREATENED(g) && !G_THREATENED(g2) ||
		!G_THREATENED(g) && G_THREATENED(g2);
	no_two_libs = grlibs[g] > 2 && grlibs[g2] > 2;
	if (one_side_dead)
		cntype[cn] = CN_ONESIDEDEAD;
	else if (cncnum[cn] > 1) {
#ifdef CNPATH
		cpylist(cnptr[cn], &cnpathptr[cn]);
#endif
		if (cncnum[cn] == 2) {
			s1 = list[cnptr[cn]];
			s2 = list[link[cnptr[cn]]];
			diff = s2-s1;
			if (diff == 1 || diff == boardsize) {
				cntype[cn] = CN_BAMBOOJOINT;
				cnprot[cn] = SOLID_CONNECT;
			}
			else if (diff == boardsize-1 &&
				S_COLOR(s1-1) == c && S_COLOR(s2+1) == c || 
				diff == boardsize+1 &&
				S_COLOR(s1+1) == c && S_COLOR(s2-1) == c) {
				cntype[cn] = CN_DIAGONAL;
				cnprot[cn] = protdiag(cn,s1,s2,c);
			}
			else {
				cntype[cn] = CN_MULTIPLE;
				cnprot[cn] = SOLID_CONNECT;
			}
		}
		else {
			cntype[cn] = CN_MULTIPLE;
			cnprot[cn] = SOLID_CONNECT;
		}
	}
	else if (cncnum[cn] == 1) {   /* single connection */
#ifdef CNPATH
		addlist(list[cnptr[cn]], &cnpathptr[cn]);
#endif
		cnprot[cn] = cnoneconn(g,g2,c,cn,ldrno,&type, &pat);
		cnpat[cn] = pat;
		cntype[cn] = type;
	}
	else if (cnlknum[cn] > 2) {
		cntype[cn] = CN_MULTIPLE;
		cnprot[cn] = cnmultlk(cn,c,g,g2,ldrno);
	}
	else if (cnlknum[cn] == 2) {
                              /* check linkages for unbreakable conns */
		cnprot[cn] = cntwolinks(g,g2,c,cn,list[cnlkptr[cn]], list[link[cnlkptr[cn]]], ldrno,&type,FALSE);
		cntype[cn] = type;
	}
	else if (cnlknum[cn] == 1) {
		cntype[cn] = CN_HALFKNIGHT;
		cnprot[cn] = canconnlink(cn,list[cnlkptr[cn]],&s,&s2,ldrno);
#ifdef CNPATH
		addlist(s, &cnpathptr[cn]);
		addlist(s2, &cnpathptr[cn]);
#endif
	}
	else if (cnllnum[cn] > 2) {
		cntype[cn] = CN_MULTIPLE;
		cnprot[cn] = cnmlkgs(g,g2,c,cn,ldrno);
	}
	else if (cnllnum[cn] == 2) {
		cnprot[cn] = cntwolkgs(g,g2,c,cn,ldrno,&type);
		cntype[cn] = type;
	}
	else if (cnllnum[cn] == 1) {
		cntype[cn] = CN_HALFLARGEKNIGHT;
		cnprot[cn] = canconnlkg(cn,&s,&s2,ldrno);
#ifdef CNPATH
		addlist(list[cnllptr[cn]], &cnpathptr[cn]);
		addlist(s, &cnpathptr[cn]);
		addlist(s2, &cnpathptr[cn]);
#endif
	}
    else if (cnddnum[cn] >= 1) {
    	cntype[cn] = CN_DOUBLEDIAG;
    	cnprot[cn] = canconndd(g,g2,c,cn,ldrno,0);
		for (ptr = cnddptr[cn]; ptr != EOL; ptr = link[ptr]) {
			s = list[ptr];
#ifdef CNPATH
			mrglist(nblbp[s], &cnpathptr[cn]);
			addlist(s, &cnpathptr[cn]);
#endif
		}
   	}
	else if (cnollnum[cn]) {
		cnprot[cn] = canconnoll(g, g2, c, cn, ldrno, &type);
		cntype[cn] = type;
	}
    else {
    	cntype[cn] = CN_UNKNOWN;
    	cnprot[cn] = CANT_CONNECT;
#ifdef CHECK
		outerror("connection fell through - bad conn");
		outaconn(cn);
#endif
   	}
	if (one_side_threatened && cnprot[cn] >= AJI_CONNECT)
		cnprot[cn] = CAN_CONNECT;
}


/* reevaluate a connection to see if it's protected */
/* updates the connection cn  (cnprot and cntype), and adds points to ladder tracker */
static void fixaprot(conn_t cn) {
	listval_t ldrno = cn + NUMGROUPS;  
	int oldprot = cnprot[cn];
	cnprot[cn] = CANT_CONNECT;
	cntype[cn] = CN_UNKNOWN;

	/* mark this connection for reevaluation later */
	if (grldr[NUMGROUPS+cn] != EOL)
		kill_ldrflags((listval_t)(NUMGROUPS+cn));

	if (cnlknum[cn] == 0 && cncnum[cn] == 0 && cnllnum[cn] == 0 && cnollnum[cn] == 0 && cnddnum[cn] == 0)
		return;		

	if (addlist(ldrno,&ldrflag[mvs[grpieces[cngr2[cn]]]]))
		adflist(mvs[grpieces[cngr2[cn]]],&grldr[ldrno]);
	if (addlist(ldrno,&ldrflag[mvs[grpieces[cngr1[cn]]]]))
		adflist(mvs[grpieces[cngr1[cn]]],&grldr[ldrno]);

	evalprot(cn);

	fixconnarmies(cn, oldprot);	/* reevaluate armies based on changed connections */

}


static void fixconnarmies(conn_t cn, int oldprot) {

	sqr_t s;
	list_t ptr;
	int x, y;
	group_t g, g2;

	g = cngr1[cn];
	g2 = cngr2[cn];

	if (!PROTCONN(cn) && PROTCVAL(oldprot)) {  /* broken connection */
		for (ptr = cnptr[cn]; ptr != EOL; ptr = link[ptr]) {
			s = list[ptr];
			if (edge[s] <= 4) {
				x = xval[s];
				y = yval[s];
				if (x < xmin)xmin = x;
				if (y < ymin)ymin = y;
				if (x > xmax)xmax = x;
				if (y > ymax)ymax = y;
				}
			}
		for (ptr = cnlkptr[cn]; ptr != EOL; ptr = link[ptr]) {
			s = list[ptr];
			if (edge[s] <= 4) {
				x = xval[s];
				y = yval[s];
				if (x < xmin)xmin = x;
				if (y < ymin)ymin = y;
				if (x > xmax)xmax = x;
				if (y > ymax)ymax = y;
				}
			}
		if (grlv[g]) {
			addlist(g,&splitlist);
			}
		if (grlv[g2]) {
			addlist(g2,&splitlist);
			}
		for (ptr = cnptr[cn]; ptr != EOL; ptr = link[ptr])
			if (eyerec[list[ptr]] != 0)
				adflist(list[ptr],&eyelist);
		for (ptr = cnlkptr[cn]; ptr != EOL; ptr = link[ptr])
			if (eyerec[list[ptr]] != 0)
				adflist(list[ptr],&eyelist);
		}
	else if (PROTCONN(cn) && !PROTCVAL(oldprot)) {  /* new solid connection */
		for (ptr = cnptr[cn]; ptr != EOL; ptr = link[ptr]) {
			s = list[ptr];
			adflist(list[ptr], &eyelist);
			if (edge[s] <= 4) {
				x = xval[s];
				y = yval[s];
				if (x < xmin)xmin = x;
				if (y < ymin)ymin = y;
				if (x > xmax)xmax = x;
				if (y > ymax)ymax = y;
				}
			}
		for (ptr = cnlkptr[cn]; ptr != EOL; ptr = link[ptr]) {
			s = list[ptr];
			if (edge[s] <= 4) {
				x = xval[s];
				y = yval[s];
				if (x < xmin)xmin = x;
				if (y < ymin)ymin = y;
				if (x > xmax)xmax = x;
				if (y > ymax)ymax = y;
				}
			}
		combinearmy(grarmy[g],grarmy[g2]);
		}
	}                                           

/* can connect a double diagonal connection? */
/* connection cn between g and g2, which have color c */
/* return list of connecting points unless scon is 0 */
/* *scon must be EOL or 0 on entry! */

int canconndd(group_t g, group_t g2, int c, conn_t cn, listval_t ldrno, list_t *scon) {
	list_t ptr,ptr2,mightlist = EOL;
	int pcount,lcount;
	int fg = 0, fg2 = 0;  /* empty points next to g and g2 */
	int fgsafe = 0, fg2safe = 0;  /* safe connections next to g and g2 */
	int canconn = FALSE, mightconn = FALSE;
	sqr_t s,sn;
	ASSERT(scon == 0 || *scon == EOL);
	if (grlibs[g] == 1 || grlibs[g2] == 1)
		return FALSE;
	for (ptr = cnddptr[cn]; ptr != EOL; ptr = link[ptr]) {
		s = list[ptr];
		addldrflag(s,ldrno);
		if (lnbn[s] == 4) { /* straight and empty double diagonal */
			if (scon != 0)
				adflist(s,scon);
			canconn = TRUE;
			continue;
			}                                    
		pcount = 0;  /* protected neighbor points */
		lcount = 0;  /* 2nd order liberties */
		for (ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
			sn = list[ptr2];
			addldrflag(sn,ldrno);
			if (inlist(sn,&grlbp[g])) {
				fg++;
				if (lnbf[sn][1-c] == 0 && lnbn[sn] <= 2)
					fgsafe++;  /* protected */
				}
			if (inlist(sn,&grlbp[g2])) {
				fg2++;
				if (lnbf[sn][1-c] == 0 && lnbn[sn] <= 2)
					fg2safe++;  /* protected */
				}
			lcount += lnbn[sn]-1;  /* don't count point at s */
			}
		if (!fg || !fg2)
			continue;  /* no possible path g to g2 */
		pcount = fgsafe + fg2safe;
		if (pcount >= 2 ||  /* makes two safe connections */
			fg == 2 && fg2safe ||
			fgsafe && fg2 == 2 ||
			fgsafe && fg2safe) {  /* safe connection and kosumi */
			if (scon != 0)
				adflist(s,scon);
			canconn = TRUE;
			continue;
			}
		for (ptr2 = nbgrp[s][1-c]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (grlibs[list[ptr2]] <= 2 && 
				(lcount >= 5 || pcount)) {
				if (scon != 0)
					adflist(s,scon);
				canconn = TRUE;
				break;
				}
			}
		if (canconn)
			continue;
		for (ptr2 = nbgrp[s][1-c]; ptr2 != EOL; ptr2 = link[ptr2]) {
			if (grlibs[list[ptr2]] <= 2 && lcount >= 2 ||
				pcount) {
				if (scon != 0)
					adflist(s,&mightlist);
				mightconn = TRUE;
				break;
				}
			}
				
		}
	if (canconn) {
		killist(&mightlist);
		return CAN_CONNECT;
		}
	else if (mightconn) {
		if (scon != 0)
			*scon = mightlist;
		else
			killist(&mightlist);
		return MIGHT_CONNECT;
		}
	else
		return CANT_CONNECT;
	}

/* multiple long links */
	
static int cnmlkgs(group_t g, group_t g2, int c, conn_t cn, listval_t ldrno) {
	int numgood = 0;
	list_t ptr;		

	g = g;
	g2 = g2;
#ifdef CNPATH
	cpylist(cnllptr[cn], &cnpathptr[cn]);
#endif
	for (ptr = cnllptr[cn]; ptr != EOL; ptr = link[ptr]) {
		addldrflag(list[ptr],ldrno);
		if (gralive[lgr[list[ptr]]] == DEAD || lnbf[list[ptr]][1-c] == 0)
			++numgood;
		}
	if (numgood >= 2)
		return(CAN_CONNECT);
	return(CANT_CONNECT);
	}
	
/* multiple linkages */

static int cnmultlk(conn_t cn, int c, group_t g1, group_t g2, listval_t ldrno) {
	int type, res, delta;
	int bestres = CANT_CONNECT, besttype = CN_MULTIPLE;
	list_t ptr, ptr2;
	sqr_t s1, s2;
	if (G_THREATENED(g1) || G_THREATENED(g2))return(CAN_CONNECT);
	if (grlibs[g1] <= 2 || grlibs[g2] <= 2)return(CAN_CONNECT);
	if (cnlknum[cn] > 4)return AJI_CONNECT;	/* for performance */

	/* try all pairwise connections to find maximum */
	for (ptr = cnlkptr[cn]; ptr != EOL && link[ptr] != EOL; ptr = link[ptr]) {
		addldrflag(list[ptr],ldrno);
		s1 = list[ptr];
		for (ptr2 = link[ptr]; ptr2 != EOL; ptr2 = link[ptr2]) {
			s2 = list[ptr2];
			delta = s2-s1;
			if (delta > 40)
				delta = 40;
			if (linktypes[delta] == 0)
				continue;	/* not a two point linkage */
			if ( inlist(s1, &grlbp[g1]) && inlist(s2, &grlbp[g2]) ||
				inlist(s2, &grlbp[g1]) && inlist(s1, &grlbp[g2])) {
				res = cntwolinks(g1, g2, c, cn, s1, s2, ldrno, &type, FALSE);
				if (res > bestres) {
					bestres = res;
					besttype = type;
					if (bestres == AJI_CONNECT)
						break;	/* for performance */
					}
				}
			}
		}

	return bestres;
	}



void combinearmy(army_t a1, army_t a2) {  /* combine two armies into one */
         /* a1 goes away, a2 gets bigger */
   group_t g;
	list_t ptr;
   if (a1 == a2) {
	return;
	}
   for (ptr = armygroups[a1]; ptr != EOL; ptr = link[ptr]) {
      g = (group_t)list[ptr];
      grarmy[g] = a2;
      }
   for (ptr = armydeadgroups[a1]; ptr != EOL; ptr = link[ptr])
      grdeadarmy[list[ptr]] = a2;
   mrglist(armygroups[a1],&armygroups[a2]);
   mrglist(armydeadgroups[a1],&armydeadgroups[a2]);
   armysize[a2] += armysize[a1];
   make_army_free(a1);
   }


/* splitarmy is called when an army is no longer in one piece.
 * It must be called after bdead completes since all completely
 * dead groups must be already known.  There are two reasons to call
 * splitarmy.  If a protected connection becomes breakable, or
 * if a dead group becomes not dead.  Note that this includes the cases
 * where a connection or a group goes away completely.
 * splitarmy is also called when a dead group becomes not a neighbor
 * any more or a new dead group becomes a neighbor.
 */


void splitarmy(army_t a) {   /* split one army into its pieces */
	army_t army;
	group_t g,g2,g3,ctgr;
	conn_t cn;
	list_t ptr,connptr,ctptr,dgrptr,ctp,ctpt2, gptr,ptr2;
#ifdef CHECK
	if (armygroups[a] == EOL) {
		outerror("trying to split army with no groups!\n");
		}
#endif
	for (gptr = armygroups[a]; gptr != EOL; gptr = link[gptr])
		if (grarmy[list[gptr]] == a) {
			g = (group_t)list[gptr];
			army = (army_t)gtflist(&armyfreelist);
			if (army == G2ERROR) {
#ifdef CHECK
				outerror("Out of armies, can't continue\n");
#endif
				outerr("Out of armies, can't continue");
#ifdef TEST				
				turnoffcplay();
#endif				
				continue;
				}
#ifdef TEST				
			if (army == a)
				outerror("big problems in splitarmy!");
#endif				
			connptr = EOL;
			ctptr = EOL;
			dgrptr = EOL;
			addlist(g,&connptr);
			grarmy[g] = army;
			armysize[army] = grsize[g];
			if (gralive[g] != DEAD /* && !grthreatened[g] since threatened group can have protected conn now */) {
				adflist(g,&ctptr);
				do{
  
					ctpt2 = EOL;
  
					for (ctp = ctptr; ctp != EOL; ctp = link[ctp]) {
						ctgr = (group_t)list[ctp]; 
  
                                   /* count groups with unbreakable conns*/ 
						for (ptr = grcnp[ctgr]; ptr != EOL; ptr = link[ptr]) {
							cn = list[ptr];
#ifdef CHECK
							if (cn > NUMCONNS-1)
								outerror("bad cn in splitarmy");
#endif
							if (PROTCONN(cn)) {
								g2 = cngr1[cn]; 
								if (g2 == ctgr)g2 = cngr2[cn]; 
#ifdef CHECK
								if (gralive[g2] == DEAD) {
									turnoffcplay();
									outerror("Bad protconn!");
									}
#endif
								if (grarmy[g2] != army) {
									grarmy[g2] = army;
									armysize[army] += grsize[g2];
									addlist(g2,&connptr);
									adflist(g2,&ctpt2); 
#ifdef CHECK
									if (g2 == NOGROUP) {
										outerr("Bad protconn!");
										}
#endif
									} 
								} 
							}
  
						if (!grthreatened[g])  /* threatened groups can't have dead connections */
						  for (ptr = grnbp[ctgr]; ptr != EOL; ptr = link[ptr]) {
							/* dead groups */
							g3 = (group_t)list[ptr];
							if (gralive[g3] == DEAD) {
								if (grdeadarmy[g3] != army) {
									addlist(g3,&dgrptr);
									grdeadarmy[g3] = army;
									for (ptr2 = grnbp[g3]; ptr2 != EOL; ptr2 = link[ptr2]) {
										g2 = (group_t)list[ptr2];
										if (g2 != g && gralive[g2] != DEAD && !G_THREATENED(g2)) {
											if (grarmy[g2] == a) {
												addlist(g2,&connptr);
												grarmy[g2] = army;
												armysize[army] += grsize[g2];
												adflist(g2,&ctpt2);
												}
											}
										}

									}
								}
							}

						}
  
					killist(&ctptr);
					ctptr = ctpt2;
					}while(ctpt2 != EOL);
				}
			armygroups[army] = connptr;
			armydeadgroups[army] = dgrptr;
			}
	make_army_free(a);
	}

  
/* cntplyhere is called with a square, a color, and a ladder number.
 * it returns YES if a stone played on that square, of the opposite color,
 * is captured in a short ladder.  It is used to determine if cuts
 * or eye stealing tesuji are possible.
 * returns NO if the stone can't be captured.
 * returns MAYBE if can't tell (ko or ladder is inconclusive)
 */


int cntplyhere(sqr_t msqr, int mclr, listval_t ldrno) {
	group_t g;
	sqr_t sn,s;
	int i,ldtmp,flag,can_capture=FALSE;
	sqr_t tmp;
	int numescape=0, capsize=0;
  
#ifdef CHECK
	if (ldrno >= NUMGROUPS+NUMCONNS+NUMEYERECS)
		outerror("bad ldrno in cntplyhere\n");
#endif

   mvs[msptr] = msqr;
   upldrflags(msptr,ldrno);

   if (lnbn[msqr] > taclibs[playlevel]) {  /* stone will get 3 liberties, not a ladder */
      return(NO);
      }

   i = fdir[msqr];
   for (ldtmp = ldir[i]; i < ldtmp; ++i) {
      sn = msqr + nbr[i];
      if (ld[sn] != 0) {	/* empty square */
		if (lnbn[sn] > taclibs[playlevel]+1)numescape++;
         continue;
         }
      g = lgr[sn];
      if (grcolor[g] == mclr) {
	      if (grlibs[g] == 1) {
				can_capture++;
				capsize = grsize[g];
		  		}
	      continue;
	      }
      if (gralive[g] == DEAD) {
         return(YES);     /* nets */
         }
      if (grlibs[g] + lnbn[msqr] > taclibs[playlevel]+1) {
	      s = mvs[grpieces[g]];
	      if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[s]))
		      adflist(s,&grldr[ldrno]);
	      return(NO);  /* new stone part of bigger group with enough libs */
	      }
      }
   if (lnbf[msqr][1-mclr] == 0 && lnbn[msqr] == 0 && can_capture == 1) {
   		if (capsize > 1)return(YES);
   		if (capsize == 1)return(MAYBE);  /* ko */
   		}
   if (lnbf[msqr][1-mclr] == 0 && lnbn[msqr] < 2 && !can_capture)return(YES);
   if (numescape > 1)return(NO);
   if (msqr == kosquare)return(MAYBE);  /* can't play in ko square */
   if (can_capture)return(NO);
   flag = FALSE;
   mvs[msptr] = msqr;
   mvcolor[msptr] = 1-mclr;
   flag = lupdate(msptr);  /* put down stone */
/*   upldrflags(msptr,ldrno);  already done earlier */
#ifdef G2DEBUG
   if (debug == 2000U+board[msqr])fixlm(mvs[msptr], mvcolor[msptr]);
#endif
   ++msptr;
   if (flag) {
      g = board[msqr];
      if (grlibs[g] == 1 &&
        (grsize[g] != 1 || grpieces[g] != (signed)msptr-1 ||
         mvcapt[msptr-1] == EOL || grsize[list[mvcapt[msptr-1]]] != 1)) {   
         s = list[grlbp[g]];
         if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[s]))
            adflist(s,&grldr[ldrno]);
         flag = FALSE;     /* can capture */
         }
      else if (grlibs[g] <= 3) {
		  /* 4/00 group trying to escape gets to win kos, since we are looking for uncondition capture here */
         flag = !iscaptured(g,conncapdepth[playlevel],conncapsize[playlevel],taclibs[playlevel],connmost[playlevel],mclr,ldrno,&tmp,1-mclr);
         }
      }
#ifdef G2DEBUG
   if (debug == 2000U+board[msqr])fixsd(msptr-1,FALSE);
#endif
   --msptr;
   ldndate(msptr);   /* take stone away */
   return(!flag);
   }

extern int dirnm[52];


/* return connection value of single linkage at cn.
 * connsqr has the point where the connection can be made
 *
 *  O @ +    or  O + @    or  O + + @  or  O + O     O @ @
 *  + + + O      + + + O      + + + O      + + + O   + + + O
 *                            + + + +     
 *
 *  case 1       case 2       case 3       case 4     case 5 (edge connect)
 *
 *  (g1)
 *  stsqr 
 *  s         s1    s2     s3(g2)
 */


/* return true if can connect single lkg (large knights move) at cn */
/* connsqr has point to play to connect. othsqr is other linkage point */
/* called from outside, so ldrno might be NOGROUP */

int canconnlkg(conn_t cn, sqr_t *connsqr, sqr_t *othsqr, listval_t ldrno) {
	sqr_t s, s1, s2, s3, stsqr = NOSQUARE;
	int c,i,dir,offs = 1,ldtmp;
	group_t g1;  /* group next to connection point */
	group_t g2;    /* other group */
#ifdef CHECK
	char buf[100];
#endif	

	ldrno = ldrno;
	if (cnllptr[cn] == EOL) {
#ifdef CHECK
		outerror("Bad llptr in canconnlkg");
		turnoffcplay();
#endif
		return(CANT_CONNECT);
		}
	s = list[cnllptr[cn]];
	if (inlist(s,&grlbp[cngr1[cn]])) {
		g1 = cngr1[cn];
		g2 = cngr2[cn];
		}
	else{
		g1 = cngr2[cn];
		g2 = cngr1[cn];
		}
	c = grcolor[g1];
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i)
		if (board[s+nbr[i]] == g1) {
			stsqr = s+nbr[i];
			break;
			}
	i = fdir[s];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		offs = nbr[i];
		dir = dirnm[i];
		if (dstbrd[s][dir] == 2 && board[sqrbrd[s][dir]] == g2)break;
		}
#ifdef CHECK
	if (board[sqrbrd[s][dir]] != g2) {
		sprintf(buf,"canconnlkg error! cn %d, g %d %d s %d\n",
			cn,board[sqrbrd[s][dir]],g2,s);
		outerror(buf);
		}
#endif
	s1 = s + offs;
	s2 = s1+offs;
	s3 = s2+offs;
#ifdef CHECK
	if (board[s3] != g2) {
		sprintf(buf,"canconnlkg error 2! s3 %d g %d %d ofs %d\n",s3,board[s3],g2,offs);
		outerror(buf);
		}
#endif
	if (edge[s] <= 2 && edge[s2] == edge[s] && !S_NEUTRAL(s)) { /* near edge */
		if (lnbf[s1][1-c] == 0  && ( lnbf[s2][1-c] == 0 || grlibs[g2] > 2 ) ||
		   lnbf[s2][1-c] == 0) {
		   	if (S_NEUTRAL(s2) || grlibs[g2] <= 2 && lnbf[s1][1-c] == 0) {
				*connsqr = s2;
				*othsqr = s1;
				}
			else {
				*connsqr = s1;
				*othsqr = s2;
				}
		   	if (G_ALIVE(lgr[s1]) == DEAD || G_ALIVE(lgr[s2]) == DEAD)
		   		return(AJI_CONNECT);
			return(CAN_CONNECT);
			}
		if (lnbf[s1][1-c] && lnbf[s2][1-c] && grlibs[list[nbgrp[s1][1-c]]] == 2) {
			*connsqr = s1;
			*othsqr = s2;
			return CAN_CONNECT;	/* he's short of liberties */
			}
		if (edge[s1] == 1 && edge[s2] == 1 && S_NEUTRAL(s2) && lnbf[s1][1-c] == 1 &&
			grlibs[list[nbgrp[s2][1-c]]] == 3) {
			*connsqr = s1;
			*othsqr = s2;
			return CAN_CONNECT;	/* case 5 */
			}
		}
	   
	else if (ld[s] >= 4 && lnbf[s2][1-c] == 0 && lnbf[s1][1-c] == 0) {
		if (grlibs[g1] <= 2) {
			*connsqr = s1+stsqr-s;
			*othsqr = s2;
			return(CAN_CONNECT);
			}
		*connsqr = s2;
		*othsqr = s1;   /* case 3 or case 4 */
		return(CAN_CONNECT);
		}

	else if (edge[s2] > 2 && board[s2+stsqr-s] == NOGROUP &&
		(ld[s3+s-stsqr] >= 4 || lnbf[s1+s-stsqr][c] >= 1) &&
		!S_NEUTRAL(s2) && lnbf[s1][1-c] == 0) {
		*connsqr = s1;
		*othsqr = s2;   /* case 3 */
		return(CAN_CONNECT);
		}


	else if (lnbf[s][1-c] == 0 && edge[s] <= 3 && edge[s] < edge[stsqr] &&
		!S_NEUTRAL(s2) && lnbf[s1][1-c] == 0) {
		*connsqr = s2;
		*othsqr = s1;   /* case 3 near edge */
		return(CAN_CONNECT);
		}

	else if (S_COLOR(s2+stsqr-s) == c && board[s1+stsqr-s] == NOGROUP) {
		if (lnbf[s1+stsqr-s][1-c] != 0 ||
			lnbf[s1+stsqr-s][c] == 3 &&
			 edge[s1+stsqr-s] > 1 &&
			 grlibs[board[s1+stsqr-s+stsqr-s]] == 1) {  /* case 4 */
			*connsqr = s1;
			*othsqr = s2;
			return(CANT_CONNECT);
			}
		if (lnbf[s2][1-c] == 0) {
			*connsqr = s1;
			*othsqr = s2;
			return(CAN_CONNECT);
			}
		}

	else if (!S_NEUTRAL(s2) && lnbf[s1][1-c] == 1) {
		*connsqr = s1;
		*othsqr = s2;  /* case 1 */
   		if (G_ALIVE(lgr[s1]) == DEAD)
   			return(AJI_CONNECT);
	    if (ld[s] >= 4 && grlibs[g2] > 2) {
			return(CAN_CONNECT);
			}
		if (grlibs[g1] > 2 && grlibs[g2] > 2 && !S_NEUTRAL(s)) {
			if (grsize[g1] > 1 || ld[s] > 2)
				return MIGHT_CONNECT;
			}
		}
	else if (ld[s] >= 4 && lnbf[s1][1-c] == 0 && lnbn[s1] < 4) {
		*connsqr = s2;
		*othsqr = s2;  /* case 2 */
	   	if (G_ALIVE(lgr[s2]) == DEAD)
	   		return(AJI_CONNECT);
		return(CAN_CONNECT);
		}
	
	if (lnbf[s2][1-c]) {
		*connsqr = s2;
		*othsqr = s1;
	}
	else {
		*connsqr = s1;
		*othsqr = s2;
	}
   	if (G_ALIVE(lgr[s1]) == DEAD || G_ALIVE(lgr[s2]) == DEAD)
   		return(AJI_CONNECT);
	return(CANT_CONNECT);
	}


/* return connection value of single linkage at cn.
 * pass in liberty where the linkage is
 * return connsqr has the point where the connection can be made and conn2 the other connection path point
 * called from outside, so ldrno might be NOGROUP
 *
 * case 2 can surely connect at connsqr, but may be cuttable.
 * case 1 can surely be cut, but may be connectable
 *
 *  case 1     case 2
 *
 *  O @ x  or  x O + @
 *  + + O      x + + O
 *
 *           (libg)
 *           stonesqr othersquare  esquare
 *   oppsqr  lib      connsqr     (conng)  backsqr
 *           oppsqr2
 */

int canconnlink(conn_t cn, sqr_t lib, sqr_t *connsqr, sqr_t *conn2, listval_t ldrno) {
	int i,ldtmp,thr;
	sqr_t sn;
	sqr_t stonesquare;  /* point next to lib with stone on it */
	group_t g1,g2;
	sqr_t othersquare;
	group_t libg = NOGROUP,conng;
	sqr_t oppsqr,backsqr, esquare;
	list_t ptr;

	*conn2 = NOSQUARE;
   
	g1 = cngr1[cn];
	g2 = cngr2[cn];
	if (G_ALIVE(g1) == DEAD || G_ALIVE(g2) == DEAD)return(CANT_CONNECT);
	thr = G_THREATENED(g1) || G_THREATENED(g2);
	if (!inlist(lib, &cnlkptr[cn])) {
#ifdef CHECK
		outerror("Bad lib in canconnlink");
		turnoffcplay();
#endif
		return(CANT_CONNECT);
		}
/*	lib = list[cnlkptr[cn]]; */
	stonesquare = lib;
	*connsqr = 0;  /* for c optimizer bug */
	i = fdir[lib];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		sn = lib + nbr[i];
		if (board[sn] == g1 || board[sn] == g2) {
			stonesquare = sn;
			libg = board[sn];
			break;
			}
		}
	if (libg == g1)conng = g2;
	else conng = g1;
	i = fdir[lib];
	for (ldtmp = ldir[i]; i < ldtmp; ++i) {
		sn = lib + nbr[i];
		if (sn != lib + lib - stonesquare &&  /* must go at right angles */
			board[sn] == NOGROUP) {
			*connsqr = sn;
			if (board[sn+sn-lib] == conng)
				break;
#ifdef NEVER
			j = fdir[sn];
			for (ldtm2 = ldir[j]; j < ldtm2; ++j) {
				sn2 = sn + nbr[j];
				if (board[sn2] == conng) {
					*connsqr = sn;
					break;
					}
				}
#endif
			}
		}
	if (*connsqr == 0)
		return CANT_CONNECT;	/* no really a connection here */
	othersquare = *connsqr + stonesquare - lib;
	if (edge[lib] == 1)
		oppsqr = lib;
   	else
   		oppsqr = lib + lib - *connsqr;

	if (grlibs[g1] > 2 && grlibs[g2] > 2 && !thr && 
		G_ALIVE(lgr[othersquare]) == DEAD)
		return(AJI_CONNECT);  /* dead enemy stone */

	esquare = othersquare + othersquare - stonesquare;
	if (board[othersquare] == NOGROUP) {  /* case 2 */
		
		for (ptr = nbgrp[othersquare][grcolor[g1]]; ptr != EOL; ptr = link[ptr])
			if (grlibs[list[ptr]] == 2)
				return CAN_CONNECT;  /* posible to cut with atari threat */
		if (edge[*connsqr] == 1 && grlibs[libg] > 2 && grlibs[conng] > 2 && !thr)
			return(AJI_CONNECT);  /* case 2 on 1st line */
		if (edge[*connsqr] > 1 && edge[*connsqr+*connsqr-lib] > 1)
			backsqr = *connsqr+(*connsqr-lib)*2;
		else
			backsqr = lib;

		if (board[*connsqr] != NOGROUP ||  /* to prevent problems in canpushcut */
			board[othersquare] != NOGROUP ||
			board[lib] != NOGROUP)
			return CAN_CONNECT;
			
		if (/*edge[*connsqr] == 2 &&  2/2001 can connect this way anywhere */
			!S_NEUTRAL(*connsqr) && 
			!S_NEUTRAL(lib) &&
			(S_COLOR(oppsqr) == grcolor[g1] || 
				lnbf[oppsqr][1-grcolor[g1]] == 0) && 
			!thr && 
			grlibs[libg] > 2 && 
			grlibs[conng] > 2 &&
			!canpushcut2(*connsqr,othersquare,esquare,cn,grcolor[libg],FALSE) &&
			!canpushcut3(othersquare, *connsqr, lib, cn, grcolor[libg], FALSE))
				return(AJI_CONNECT);
		if (S_COLOR(esquare) == 1-grcolor[g1] && (grlibs[board[esquare]] <= 2 ||
			lnbf[*connsqr][1-grcolor[g1]] == 0 &&
			    !checkforcut(esquare, othersquare, *connsqr, cn, grcolor[g1])))  /* 4/01 if can connect with a short ladder */
			*conn2 = othersquare;
		return(CAN_CONNECT);  /* can make double connection (case 2) */
		}

	/* below here, all case 1 */

	if (ldrno != NOGROUP && addlist(ldrno,&ldrflag[othersquare]))
		adflist(othersquare,&grldr[ldrno]);

	if (edge[*connsqr] == 1 && !S_NEUTRAL(lib) && grlibs[libg] > 1 && grlibs[conng] > 1)
		return(CAN_CONNECT);
	if (edge[*connsqr] > 1 &&
		board[*connsqr+*connsqr-othersquare] == conng &&
		grlibs[libg] > 3 && grlibs[conng] > 3 &&
		S_COLOR(lib+lib-stonesquare) != 1-grcolor[g1] &&
		grlibs[board[othersquare]] == 2)  /* when he pushes I can atari and connect */
		return AJI_CONNECT;   /* what if he hane?  assume can catch it :( */
	if (grlibs[board[othersquare]] == 1)return(CAN_CONNECT);
	if (grlibs[board[othersquare]] == 2) {
		if (grsize[board[othersquare]] > 1 || board[esquare] == NOGROUP || 
			lnbn[othersquare + othersquare - *connsqr] != 0 ||
			lnbf[othersquare + othersquare - *connsqr][grcolor[g1]] != 0)
			return(CAN_CONNECT);  /* can't be a ko */
		for (ptr = nbgrp[othersquare + othersquare - *connsqr][1-grcolor[g1]]; ptr != EOL; ptr = link[ptr])
			if (grlibs[list[ptr]] == 1)
				return CAN_CONNECT;  /* capture two groups, not a ko */
		return KO_CONNECT;
		}
	if (lnbn[lib] < 3 && !S_NEUTRAL(lib))return(CAN_CONNECT);  /* can make hanging connection */

	if (checkforcut(othersquare,*connsqr,lib,cn,grcolor[g1]))  /* reads cut */
		return(CANT_CONNECT);
	else
		return(CAN_CONNECT);

#ifdef NEVER		
	if (grlibs[conng] < 3 || grlibs[libg] < 3)
		return(CANT_CONNECT);  /* cut would be atari */
	if (S_NEUTRAL(lib))return(CANT_CONNECT);  /* opponent can cut */
	if (edge[stonesquare] < 2)return(CANT_CONNECT);
	if (ld[oppsqr] >= 4 && grcolor[lgr[oppsqr]] == 1-grcolor[conng])
		return(CANT_CONNECT);  /* opponent can cut */
		
	oppsqr2 = lib*2-stonesquare;
	if (ld[oppsqr2] >= 4 && grcolor[lgr[oppsqr2]] == 1-grcolor[conng])
		return(CANT_CONNECT);  /* opponent can cut */
	if (edge[lib] == 2 && edge[stonesquare] > edge[lib])
		return(CAN_CONNECT);  /* can make protected connection on two line */
	if (ld[oppsqr] != NOLD && ld[oppsqr] >= 4 ||
		ld[oppsqr2] != NOLD && ld[oppsqr2] >= 4)
		return(CAN_CONNECT);  /* can quickly capture cutting stone */
	return(CANT_CONNECT);
#endif
	}


sqr_t connect_bamboo(group_t g1, group_t g2, sqr_t s) {
	int i,ldtmp;
	sqr_t sn,sp;
	list_t ptr;
	if (edge[s] > 2) {
		for (ptr = nblbp[s]; ptr != EOL; ptr = link[ptr]) {
			sp = list[ptr];
			i = fdir[sp];
			for (ldtmp = ldir[i]; i < ldtmp; ++i) {
				sn = sp + nbr[i];
				if ((board[sn] == g1 || board[sn] == g2) &&
				   board[sp-nbr[i]] == NOGROUP && sp-nbr[i] != s) {
					return(sp-nbr[i]);
					}
				}
			}
		}
	return(NOSQUARE);
	}


#ifdef G2DEBUGOUTPUT



void outaconn(conn_t cn) {
	list_t ptr;
	unsigned int pat;
	char buf[20];
	int o;
	unhighlight();
	clearerror();
	higroup(cngr1[cn]);
	higroup(cngr2[cn]);
	outerr("\nBefore eval::Type - ");
	outerr(cntpstr[cntype[cn]]);
	outerr("\nProt - ");
	outerr(protnam[cnprot[cn]]);
	outerr("\n");
	evalprot(cn);
	outerr("\nAfter eval::Type - ");
	outerr(cntpstr[cntype[cn]]);
	outerr("\nProt - ");
	outerr(protnam[cnprot[cn]]);
	outerr("\n");
	if (cnprot[cn] == SHARED_CONNECT) {
		sprintf(buf, "cnshcent %d\n", cnshcent[cn]);
		outerr(buf);
	}
	if (cntype[cn] == CN_ONEPOINTJUMP) {
		outerr("Pattern prot - ");
		outerr(protnam[gettype(
			pat = matchtypebest(ONEPOINTFILE,list[cnptr[cn]],grcolor[cngr1[cn]], (listval_t)(NUMGROUPS+cn), &o, cn, readopj)
			)]);
		outerr(" at pat ");
		sprintf(buf,"%d:%d",pat, cnpat[cn]);
		outerr(buf);
		outerr("\n");
		}
	for (ptr = cnddptr[cn]; ptr != EOL; ptr = link[ptr]) {
		outstone(list[ptr],"d");
		}
	for (ptr = cnollptr[cn]; ptr != EOL; ptr = link[ptr]) {
		outstone(list[ptr],"o");
		}
	for (ptr = cnllptr[cn]; ptr != EOL; ptr = link[ptr]) {
		outstone(list[ptr],"l");
		}
	for (ptr = cnlkptr[cn]; ptr != EOL; ptr = link[ptr]) {
		outstone(list[ptr],"k");
		}
	for (ptr = cnptr[cn]; ptr != EOL; ptr = link[ptr]) {
		outstone(list[ptr],"c");
		}
	for (ptr = grldr[NUMGROUPS+cn]; ptr != EOL; ptr = link[ptr])
		histone(list[ptr]);
	}


void outconnections(sqr_t s) {
	list_t ptr;
	int olddebug;
	for (ptr = cnbrd[s]; ptr != EOL; ptr = link[ptr]) {
		olddebug = debug;
		debug = 2000+maxgr;
		cntplyhere(s, grcolor[cngr1[list[ptr]]], list[ptr]);
		debug = olddebug;
		outaconn(list[ptr]);
		outerr("Any key to continue");
		waitaction();
		}
	for (ptr = lkbrd[s]; ptr != EOL; ptr = link[ptr]) {
		outaconn(list[ptr]);
		outerr("Any key to continue");
		waitaction();
		}
	for (ptr = llbrd[s]; ptr != EOL; ptr = link[ptr]) {
		outaconn(list[ptr]);
		outerr("Any key to continue");
		waitaction();
		}
	for (ptr = ollbrd[s]; ptr != EOL; ptr = link[ptr]) {
		outaconn(list[ptr]);
		outerr("Any key to continue");
		waitaction();
		}
	for (ptr = ddbrd[s]; ptr != EOL; ptr = link[ptr]) {
		outaconn(list[ptr]);
		outerr("Any key to continue");
		waitaction();
		}
	unhighlight();
	clearerror();
	}
#endif
