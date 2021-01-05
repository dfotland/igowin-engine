/* Copyright 1984-1995 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* from g2list.c list routines */

/* # include <stdio.h> */
# include "g2tree.h"
# include "g2list.h"
# include "g2jos.h"
#include <time.h>

/* from g2init */
void fixplaylevel(int);

/* from g25.c for life */

int iskopoint(sqr_t s, int *color);
int winsemprob(army_t a1, army_t a2, int lessa2, int lessa1);
int getpoteyes(army_t army, int *min);
void life(int);
int deadinsidedead(group_t g);
void getarmynbp(army_t);
void kill_ldrflags(listval_t );
char *ssqr(sqr_t s,char *str);
int bestpot(army_t army,int *second,int *bestval2,int *bestshared,sqr_t *bestmove,sqr_t *bestmove2,sqr_t *bestkillmove,sqr_t *bestkillmove2,sqr_t *secondmove,int *rest,int *pthreat,int *kopot,int *weakpot, int *numrest, int *bestmax);
int eyesifcapture(army_t a, army_t army, int *maxeyes);
int isseki(army_t army);
int armyrunval(army_t army);
int howmuchvital(sqr_t sn, army_t army, int color, list_t eyepoints, int *max);
int livestonehere(sqr_t s, int c, int lgd, int tld, listval_t ldrno, int libs);
void getalprob(army_t army, int alive, int alprob, int semprob, int *alval);
int uns_semeai(army_t army, int *alprob, int *semprob, int *mineyes, int *maxeyes);
void markgroup(group_t g);

/* g27a.c */

void upltr(void);     /* fix the ltr data structure */
int findltrgd(sqr_t s, int dir, int dir2, int udir, int udir2, int dist);
int undercut(sqr_t s, int dir, int dir2, int c, int udir, int udir2);

/* from g27b */

void dnxy(sqr_t,int);
void upxy(sqr_t s);     /* add stone at s to ltrxy */ 
void uscan(sqr_t s); /* update lb for move at x,y. */ 
void dscan(sqr_t s);
void cscan(sqr_t s, group_t g);
void cuscan(sqr_t s, group_t g); 



/* from g28.c for connections */

void dncons(sqr_t, group_t);    /* delete cons when stone removed */
void brkconns(sqr_t);   /* break all connections through s */
void moveconns(group_t, group_t);   /* move connections from g1 to g2. g1 ends up unconnected*/
void adcons(sqr_t, group_t);       /* add connections through a point */
void addconn(group_t, group_t, sqr_t);  /* add a connection from g1 to g2 at s */
void chkcon(sqr_t, group_t);  /* add connections to g at libs next to s */
void addlks(sqr_t, group_t, sqr_t);	/* addlinks from g to groups adjacent to s */
void addlkgs(sqr_t, group_t, sqr_t);	/* addlkgs from g to groups adjacent to s */
void addolkgs(sqr_t, group_t, sqr_t);	/* addlkgs from g to groups adjacent to s */
void brklks(sqr_t, sqr_t, sqr_t, group_t);	/* s is empty sqr has stone on it */
void brklkgs(sqr_t, sqr_t, sqr_t, group_t);	/* s is empty sqr has stone on it */
void brkolkgs(sqr_t, sqr_t, sqr_t, group_t);	/* s is empty sqr has stone on it */
void rstrlks(group_t, group_t, sqr_t);  /* restore links from gold to gnew */
void brklink(group_t, group_t, sqr_t, sqr_t);  /* break link from g1 to g2 (s1 part of g2) at s2 */
void brklkg(group_t, group_t, sqr_t, sqr_t);  /* break link from g1 to g2 (s1 part of g2) at s2 */
void brkolkg(group_t, group_t, sqr_t, sqr_t);  /* break link from g1 to g2 (s1 part of g2) at s2 */
void realbrklink(group_t, group_t, sqr_t, sqr_t);  /* break link from g1 to g2 (s1 part of g2) at s2 */
void realbrklkg(group_t, group_t, sqr_t, sqr_t); /* break link from g1 to g2 (s1 part of g2) at s2 */
void realbrkolkg(group_t, group_t, sqr_t, sqr_t); /* break link from g1 to g2 (s1 part of g2) at s2 */
void delconnrec(conn_t);
void addlink(group_t, group_t, sqr_t);  /* add a link from g1 to g2 at s */
void addlkg(group_t, group_t, sqr_t);  /* add a linkage from g1 to g2 at s */
void addolkg(group_t, group_t, sqr_t);  /* add a linkage from g1 to g2 at s */
void dndiags(sqr_t s, group_t g);
void diagconn(sqr_t s);

/* from g29.c for tactics */

int iscaptured(group_t g, int maxply, int maxnodes, int maxlibs, int maxbr, int tm,listval_t ldrno, sqr_t  *movethatworks, int winsko);
void initbestresp(void);
int getefflibs(group_t g, int n, listval_t ldrno, int *maxlibs);
int canbecaptured(sqr_t s, int c, sqr_t gs, int ctm, int winsko, int depth, int size, int libs, listval_t ldrno);
list_t getatkmoves(group_t g);
list_t getdefmoves(group_t g);
int solidconnect(sqr_t s, int c, int winsko);
int snapback(group_t g, int winsko);
void dumpgenmoves(sqr_t cursorpos, int tm, int winsko);

#ifdef G2DEBUGOUTPUT
void psqr(sqr_t s);
#endif


/* from g22.c */

int lupdate(int upptr);
void ldndate(int dnptr);
void upldrflags(int upptr, listval_t ldrno);
void addldrflag(sqr_t s, listval_t ldrno);

/* from g2s2.c update and downdate */

int illegalko(sqr_t, int msptr, int rules);
void dndate(void);
int update(sqr_t s, int c, int nosuicide);
void make_army_free(army_t);
int noroomleft(void);
g2status makeuctmove(sqr_t s, int c, int rules);
g2status takebackauctmove(int rules, point_t *s);

/* from g23.c */
void getobaval(void);
int strategy(int color, int handicap, int rules);

/* from g2conn */
int cntwolinks(group_t g, group_t g2, int c, conn_t cn, sqr_t s1, sqr_t s2, listval_t ldrno, int *ctype, int dbg);


/* from g2eye.c eye evaluation */

#ifdef G2DEBUGOUTPUT
void outeyerec(sqr_t);
void outneweyerec(sqr_t);
int outgetcount(sqr_t cursorpos);
void outeyelist(void);
#endif
void addrteyelist(sqr_t s);
void fixli(void);
void evaleye(sqr_t s, eye_t rn, listval_t ldrno, int fulle, int libs);
void delete_eye(eye_t rn);
void deadshape(group_t g,eye_t rn, list_t liblist,listval_t ldrno, int libs);
int getltrgd(sqr_t s, int dump, int *ltr1, int *ltrc);
int getldval(sqr_t s);
int canmakenakade(army_t a); 
 /* g2inpat.c */

#ifdef ISHIFORMAT 
void insertpat(char *p);
#endif
void freepats(void);

void setdefvals(void);
void setatkvals(int passval);

/* from g2jos.c */

void initjflags(void);
void jdndate(void);
int josekisequence(sqr_t s, int color, int handicap, int rules, list_t *seq, int alpha);
int groupinjosekicorner(group_t g);
int notjoseki(sqr_t s);
 
/* from g2patrd */

int patmore(int ptr);
int patprev(int ptr);
int patsequence(list_t *seq, int move, int c, int handicap, int rules, int alpha);
int getnewpatnum(int p);
sqr_t getnewpatsqr(int p);
short getnewpato(int p);
short getnewpatcolor(int p);
short getnewpatmove(int p);
int checkpatterns(int *npbcnt);
int patgoodreason(int str, int move, int c);


/* from g2pat.c patterns */

#ifdef G2DEBUGOUTPUT
void pmatches(void);
void outhash(void);
#endif
list_t getpatmoves(int, int, int);
void initboardbits(int);
void addbits(sqr_t s, int c);
void delbits(sqr_t s, int c);
void allocatepats(void);
void findmatch(int mptr, int up);
void matchpatterns(sqr_t fsqr, sqr_t lsqr, sqr_t mv, int up);
list_t matchtypemoves(int pf, sqr_t s, int c, int mcolor, int debug);
int readopj(int pat, int o, int c,sqr_t corner, sqr_t connpoint, sqr_t h1, sqr_t h2, sqr_t h3, listval_t ldrno);
unsigned int matchtypebest(int pf, sqr_t s, int c, listval_t ldrno, int *reto, conn_t cn,
	int (*readpat)(int, int, int, sqr_t, sqr_t, sqr_t, sqr_t, sqr_t, listval_t));
void genpatmoves(int color, int passval);
void checkmoveatts(sqr_t mv, int c);
sqr_t patgetsqr(sqr_t s, int o, char xy);

/* from g2pot.c */

list_t adpot(army_t,int);
list_t addconnpoints(army_t, conn_t);
list_t rmconnect(army_t army, conn_t cn);
void adthreat(army_t army2, list_t *rlist,int maxmoves);
list_t rmpot(army_t,int);
void rmthreat(army_t  army2, list_t *rlist);
void knightcutpoints(int cn, list_t *rlist);

/* from g2terr */

void fixlgr(void);
int canrunhere(army_t army, sqr_t s);
void radiaterun(list_t swaplist);
void radiateterr(list_t swaplist);
list_t surroundpoints(army_t army, sqr_t s);
list_t runpoints(army_t army, sqr_t s, int type);
#ifdef G2DEBUGOUTPUT
void outltrgood(void);
void outnewltrgood(sqr_t, int);
void outltr1(void);
void outrad(int c);
void outrunrad(int c);
void outtterr(int tm, int handicap, int rules);
void dumpterritory(int tm, int handicap, int rules);
void outtltrgood(sqr_t);
#endif

/* from g2getr.c get reasons */

list_t th_cap_moves(group_t);
int fill_dame(int color);

/* g2getra */

void initreasons(int ply, int passval);
int get_reasons_for_moves(int passval, int color, int handicap, int rules, int randomize);
void get_search_reasons(int color, int handicap, int rules, double depth, int ply);

/* g2getrf */
void fire_strat_rule(sqr_t s, int rule, int val, int param, int guess);
int which_corner(sqr_t s);
void try33invasion(int c);
int rule_fired(sqr_t s, int rule);


/* g2getrv */
int conn_val(army_t a1,army_t a2);
int cut_val(army_t a1,army_t a2);
int def_val(army_t army, int alive);
int atk_val(army_t army, int alive, int passval);
int cut_stones_val(army_t army);
int safecaptval(army_t army, int passval);
int safecaptarmy(army_t army, int color, int handicap);

/* g2getrc */
int pincerstonefacing(int corner);

/* from g2conn.c connections */

int canconndd(group_t g, group_t g2, int c, conn_t cn, listval_t ldrno, list_t *scon);
int cntplyhere(sqr_t msqr, int mclr, listval_t ldrno);
sqr_t connect_bamboo(group_t g1, group_t g2, sqr_t s);
void combinearmy(army_t a1,army_t a2);
void splitarmy(army_t a);
void fixcnprot(int everything);
int canconnlkg(conn_t cn,sqr_t *connsqr,sqr_t *othsqr,listval_t ldrno);
int canconnlink(conn_t cn,sqr_t lib,sqr_t *connsqr,sqr_t *,listval_t ldrno);
int threepointjump(group_t g,group_t g2,int c,conn_t cn,sqr_t s,sqr_t s2,sqr_t *sr,listval_t ldrno);
#ifdef CHECK
int getprot(conn_t cn);  /* recalculate protectedness of connection */
#endif


/* g2sem */

int semeailibs(army_t a, int *min, int *max, int *typ, int *hemin, int *hemax, int *memin, int *memax);
int newconnlibs(army_t, conn_t, army_t *);
int semeai_result(army_t a1, army_t a2);

/* g2fcache */

void clearfights(void);
void clearafight(group_t g);

/* g2fight */
# define MV_MASK 0x1ff
/* probability of move failure in range 0-50 (to avoid the sign bit) */
/* prob 0 is best move so sorted lists will work and put best move in front of list */
# define MV_PROB(x) (((x)>>9) & 0x3f)
# define MV_MV(x) ((sqr_t)((x) & MV_MASK))
# define MV_MAKE(p,s) ((listval_t)((((p&0x3f))<<9) | s))
list_t lifemoves(group_t g);
list_t killmoves(group_t g);
int liveresult(group_t goodgroup, group_t badgroup);
int liveprob(group_t g);
int killresult(group_t goodgroup, group_t badgroup);
int killprob(group_t g);
int firekillrules(group_t g, tree_t movetree);
int fireliverules(group_t g, tree_t movetree);
int readliferesult(group_t g, int live, tree_t *movetree);
list_t weaksemneighbors(army_t);
/*int save_fight(army_t army,int passval,int alpha, int urg_alpha); */
tree_t canbekilled(group_t g, int maxnodes, int maxdepth);
tree_t canmakelife(group_t g, int maxnodes, int maxdepth);
int trytolive(group_t g);
void lifereading(int color, unsigned int secs, int nodes);
list_t justlifemoves(group_t g);
list_t justlifesetmoves(list_t groups);
list_t justkillmoves(group_t g, int minprob);
void save_capture_nbr(army_t army, list_t *slist, int defval);
#ifdef G2DEBUGOUTPUT
void outlifemoves(void);
void outsemmoves(sqr_t cp1, sqr_t cursorpos, int tm);
void debugsemmoves(sqr_t cursorpos, int tm);
int fightatkdefval(army_t army, group_t g, int tm);
#endif

/* g2search */
int search(sqr_t s, int alpha, int beta, int color, int handicap, int rules, float depth, unsigned int stoptime, int reasons);

/* g2look */

/*int lookahead(sqr_t s, int *goodmove, int alpha, int color, int handicap, int rules); */
list_t genobviousmoves(float depth, int ply, int goals, int havepattern);
list_t genobviouskill(army_t army);
list_t genobvioussave(army_t army);
extern	int groupaji(int *big);
void stval(int move);
int sumstrat(sqr_t s, int color);
army_t biggesttokill(int c, int *val);

/* from g2score.c */

int higroup(group_t); 
void outlgr(void);
int terr(int rules, sqr_t s, list_t swaplist);
#ifdef G2DEBUGOUTPUT
void outscores(void);
void outscoreorder(void);
void outlal(void);
void outalprob(void);
void outlgr(void);
void outconnections(sqr_t s);
void outaconn(conn_t cn);
#endif

/* misc */

#ifdef CHECK
int dscheck(int);
#endif
void turnoffcplay(void);
int incn(sqr_t s,int corner,int pastcenter);

/* UI routines called from g2
 */

#ifdef G2DEBUGOUTPUT
void spechilist(list_t head);

  /* these calls out to UI from g2 are only used by debug code */
void outerror(char *s);
void goeventloop(void);
void waitaction(void);
void clearerror(void);
void unhighlight(void);
void hispecstone(sqr_t);

#endif
