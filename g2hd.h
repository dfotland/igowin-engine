/* Copyright 1984-1995 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */
#include "g2basic.h"
#include "g2uihd.h" 
#include <assert.h> 

extern unsigned __int64 hashval[NUMMOVES][HASH_ARRAY_SIZE];	/* zobrist hash values for each move */
extern char savegral[2][NUMGROUPS];  /* saved alive values */
extern char savevalid[2];				/* is the saved life data valid */
extern unsigned int savemove[2];				/* msptr value associated with saved life */
extern int gralvalid;					/* are the gralive values valid */
extern char *progname;
extern int highlightlast;
extern struct rulestruct rules[250];
extern int maxmove;
extern int sc;
extern int beepflag;
extern eye_t eyerec[NUMSQUARES];
extern list_t eyevitrec[NUMSQUARES];
//extern unsigned char eyevitval[NUMSQUARES][2];  /* cache of values by color of this vital point */
//extern unsigned char eyevitvalmax[NUMSQUARES][2];  /* cache of values by color of this vital point */
extern list_t newpatfreelist;
extern list_t eyefreelist;
extern char eyetype[NUMEYERECS], eyeval[NUMEYERECS], eyevalmax[NUMEYERECS], eyepot[NUMEYERECS], eyemin[NUMEYERECS], eyestatus[NUMEYERECS], eyecolor[NUMEYERECS], eyeko[NUMEYERECS];
extern list_t eyeptr[NUMEYERECS],eyevital[NUMEYERECS];
extern list_t newpatbrd[NUMSQUARES];
extern list_t bestseq[NUMSQUARES];
/*extern list_t pass_seq; */
extern int numnodes;
extern int nummoves;
extern int gxstart,gxincr,gystart,gyincr;
extern sqr_t xmax,xmin,ymax,ymin;
extern char lookaheadflag;
extern list_t lookldr;
extern char edge[NUMSQUARES];
extern char fdir[NUMSQUARES];
extern char ldir[52];
extern char ldiag[52];
extern char edge2[NUMSQUARES];
extern unsigned char lnbf[NUMSQUARES][2];
extern list_t nbgrp[NUMSQUARES][2];
extern unsigned char lnbn[NUMSQUARES];
extern list_t nblbp[NUMSQUARES];
extern list_t cnfreelist;            /* free connection records */
extern struct potential pots[NUMPOTENTIAL];
extern list_t armyfreelist;          /* free army records */
extern list_t armypot[NUMARMIES];
extern list_t armydeadgroups[NUMARMIES];
extern short armyeyes[NUMARMIES];
extern short armysafeeyes[NUMARMIES];
extern list_t armylbp[NUMARMIES];
extern list_t armynbp[NUMARMIES];
extern list_t armyeyerecs[NUMARMIES];
extern list_t armyvitalpoints[NUMARMIES];
extern list_t armyrun[NUMARMIES][NUMRUN];
extern short armyrn_pot[NUMARMIES];
extern short armycnrn_pot[NUMARMIES];
extern short armywk_pot[NUMARMIES];
extern short armyeyespace[NUMARMIES];
extern short armyeyespacemax[NUMARMIES];
extern int armyaji[NUMARMIES];
extern int armyminsemeyes[NUMARMIES];
extern int armymaxsemeyes[NUMARMIES];
extern short armybestpot[NUMARMIES];  /* best potential eye for army */
extern short armybestmax[NUMARMIES];  /* best potential eye for army */
extern sqr_t armybestmove[NUMARMIES];  /* best move for eyes */
extern short armybestpot2[NUMARMIES];  /* best potential eye for army */
extern sqr_t armybestmove2[NUMARMIES];  /* best move for eyes */
extern short armybestshared[NUMARMIES];  /* best shared connection */
extern sqr_t armykillmove[NUMARMIES];  /* best potential reducing move */
extern sqr_t armykillmove2[NUMARMIES];  /* second best potential reducing move */
extern short armysecond[NUMARMIES];   /* second best val */
extern sqr_t armysecondmove[NUMARMIES]; /* second best move */
extern short armyrest[NUMARMIES];  /* rest of eye potential */
extern short armypthreat[NUMARMIES];  /* pthreat potential */
extern short armynumrest[NUMARMIES];
extern short armykopot[NUMARMIES]; /* ko potential eyes */
extern short armyweakpot[NUMARMIES]; /* connection wo weak group potential */
extern short armyeyepotential[NUMARMIES];
extern army_t grdeadarmy[NUMGROUPS];
extern int grdefval[NUMGROUPS];
extern int gratkval[NUMGROUPS];
extern short armysize[NUMARMIES];   /* number of stones in army */
extern list_t charmy;                /* armies which changed groups */
extern list_t chalive;               /* armies whose aliveness changed */
extern short armylibs[NUMARMIES];
extern list_t eyelist;
extern int pid;
extern int stratguess[MAXSEARCHDEPTH][NUMSQUARES];
extern short stratvalue[NUMSQUARES];
extern short stratpatvalue[NUMSQUARES];
extern char stratgoodreasons[NUMSQUARES];
extern list_t stratreasons[NUMSQUARES];
extern int stratbestpat[NUMSQUARES];
extern listval_t nextstrat;
extern struct strategy strat[NUMSTRATS];
/* extern int handicap; */
extern sqr_t firstsquare,lastsquare;
extern int timeused;
extern int hcapbonus[];
extern short terv[NUMSQUARES];
extern short runterv[NUMSQUARES][2];
extern short rterv[NUMSQUARES][2];
extern int tscr, ajiscr, dscr, bigscr;
extern int thrscr;
extern list_t freelist;
extern sqr_t pcls[NUMPCLS];
extern int pclsnext;
extern list_t cnbrd[NUMSQUARES],cnchgd;
extern list_t lkbrd[NUMSQUARES];
extern list_t llbrd[NUMSQUARES];
extern list_t ollbrd[NUMSQUARES];
extern list_t ddbrd[NUMSQUARES];
extern sqr_t sqrbrd[NUMSQUARES][4];
extern char dstbrd[NUMSQUARES][4];
extern list_t grlbp[NUMGROUPS];
extern list_t grldr[NUMGROUPS+NUMCONNS+NUMEYERECS];
extern list_t grcnp[NUMGROUPS];
extern list_t grnbp[NUMGROUPS];
extern list_t ldrflag[NUMSQUARES];
extern list_t nbply[NUMMOVES],lbply[NUMMOVES]; 
extern int cplay[2],cplayed[2];
  
extern int level[];  /* level of play for computer */ 
extern short grlibs[NUMGROUPS];
extern signed char gralprob[NUMGROUPS]; 
extern int gralval[NUMGROUPS][2];
extern int groldalprob[NUMGROUPS];
extern int grsemval[NUMGROUPS];
extern unsigned char grthreatened[NUMGROUPS];
extern sqr_t grcapmove[NUMGROUPS];
extern sqr_t grsavemove[NUMGROUPS];
extern char groldalive[NUMSQUARES];
extern signed char sqoldalprob[NUMSQUARES];
extern army_t groldarmy[NUMSQUARES];
extern int grolddefv[NUMSQUARES];
extern short groldrun[NUMSQUARES];
extern char groldthreatened[NUMSQUARES];
extern char grlv[NUMGROUPS];
extern unsigned short grsize[NUMGROUPS]; 
extern char ld[NUMSQUARES];
extern group_t lgr[NUMSQUARES];
extern char ltr1[NUMSQUARES];
extern char ltrgd[NUMSQUARES];
extern char ltrcolor[NUMSQUARES]; 
extern group_t cngr1[NUMCONNS],cngr2[NUMCONNS], cnshcent[NUMCONNS];
extern char cncnum[NUMCONNS],cnlknum[NUMCONNS],cnllnum[NUMCONNS],cnollnum[NUMCONNS],cnddnum[NUMCONNS];
extern list_t cnptr[NUMCONNS],cnlkptr[NUMCONNS],cnllptr[NUMCONNS],cnollptr[NUMCONNS],cnddptr[NUMCONNS],cnpathptr[NUMCONNS];
#define CNPATH
extern char cnprot[NUMCONNS],cntype[NUMCONNS];
extern short cnpat[NUMCONNS];
extern int numpris[2];
extern int maxpc;
extern int pscr,ctscr,passscr,obaval;
  
extern int nbr[52];
extern int diags[52];
extern char *clr[3];
  
extern char mvmost[NUMLEVELS];
extern char eyemost[NUMLEVELS];
extern char connmost[NUMLEVELS];
extern int mintacval[NUMLEVELS], maxtacdiff[NUMLEVELS], maxvariations[NUMLEVELS], cancapsize[NUMLEVELS];
extern int maxmoves[NUMLEVELS];
extern unsigned char eyetaclibs[NUMLEVELS], taclibs[NUMLEVELS], mdist[NUMLEVELS], wdist[NUMLEVELS], eyecapsize[NUMLEVELS];
extern unsigned char eyecapdepth[NUMLEVELS], vitalcapsize[NUMLEVELS], vitalcapdepth[NUMLEVELS];
extern unsigned char conncapdepth[NUMLEVELS], conntaclibs[NUMLEVELS];
extern unsigned char conncapsize[NUMLEVELS];
extern int ahead,agressiveness,phase,easyrun[3];
 
extern int termtype;

extern unsigned char bdw[4][8][4][32];  /* bit vectors for the board */
extern unsigned char bde[4][8][4][32];  /* 4 orientations.  corner matches upper left */
extern unsigned char bdb[4][8][4][32];  /* edge across top */

extern int last_search_level;

char *fname();
