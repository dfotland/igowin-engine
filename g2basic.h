/* Copyright 1984-1992 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#pragma warning(disable : 4996)
# include "ui2g2.h"
# include "g2def.h"
# include "g2proto.h"

/*
 * basic data used by tactical reading and basic eye analysis
 */

/* constants (could change with board size */
extern char xval[NUMSQUARES];
extern char yval[NUMSQUARES];
extern char edge[NUMSQUARES];
extern char edge2[NUMSQUARES];
extern char fdir[NUMSQUARES];
extern int showtactics, cancapsize[NUMLEVELS], playlevel, mintacval[NUMLEVELS], maxtacdiff[NUMLEVELS];
extern unsigned char taclibs[NUMLEVELS];
extern char mvmost[NUMLEVELS];
extern char ldir[52];
extern char ldiag[52];
extern int nbr[52];
extern int diags[52];
extern char *clr[3];
extern sqr_t boardsize;
extern sqr_t boardsquare,firstsquare,lastsquare;


/* the board */
extern group_t board[NUMSQUARES];
extern unsigned char lnbf[NUMSQUARES][2];
extern list_t nbgrp[NUMSQUARES][2];
extern unsigned char lnbn[NUMSQUARES];
extern list_t nblbp[NUMSQUARES];
extern list_t ldrflag[NUMSQUARES];


/* groups */
extern group_t maxgr;
extern char grlv[NUMGROUPS];
extern char grcolor[NUMGROUPS];
extern short grpieces[NUMGROUPS];
extern short grlibs[NUMGROUPS];
extern unsigned short grsize[NUMGROUPS]; 
extern unsigned char grthreatened[NUMGROUPS];
extern sqr_t grcapmove[NUMGROUPS];
extern sqr_t grsavemove[NUMGROUPS];
extern unsigned char gralive[NUMGROUPS];
extern list_t grnbp[NUMGROUPS];
extern list_t grlbp[NUMGROUPS];
extern list_t grldr[NUMGROUPS+NUMCONNS+NUMEYERECS];

/* moves */
extern unsigned int msptr;
extern sqr_t mvs[NUMMOVES];
extern char mvcolor[NUMMOVES];
extern short mvnext[NUMMOVES];
extern sqr_t kosave[NUMMOVES];
extern list_t mvcapt[NUMMOVES],mvconn[NUMMOVES];
extern list_t nbply[NUMMOVES],lbply[NUMMOVES]; 

/* other */
extern sqr_t kosquare;
extern char lookaheadflag;
extern list_t lookldr;

/* lists */
#ifdef ALLOCLIST
extern listval_t *list;
extern list_t *link;
#else
extern listval_t list[NUMLIST];
extern list_t link[NUMLIST];
#endif
extern list_t freelist;

/* debug */  
extern unsigned int debug;
extern int numnodes;
extern int nummoves;
