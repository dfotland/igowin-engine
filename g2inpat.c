/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* all code for pattern matching, generation, and updating */
#if defined(ISHIFORMAT) || defined(G2DEBUGOUTPUT)
# include <stdio.h>
#endif
#ifdef PATEDIT
# include <malloc.h>
#endif
# include "g2hd.h"
# include "g2pat.h"


#ifdef __STDC__
# define _fastcall
#ifdef ISHIFORMAT
void outpats(FILE *f);
#endif
static int readpatterns(char * filename);
#else
static int readpatterns();
#endif

extern UINT16 maxpatterns, maxpatmoves;

extern struct newpattern *pt;

extern int nextpattern;  /* index of next free pattern */
extern struct pmove *pm;

extern struct phash *ph;
extern struct pfile *patfiles, patfilestmp[];

/* start of hash chains for different types of patterns */
extern UINT16 *hstarta, hstartatmp[], *hstartc, hstartctmp[], *hstarte, hstartetmp[];  

extern UINT16 nexthash;  /* index of next free hash */

extern struct newpatstruct newpat[NUMNEWPATS];

extern unsigned int nextfreemovetmp, nextpatterntmp;
extern unsigned int nexthashtmp;  /* index of next free hash */
extern struct newpattern pttmp[];
extern struct pmove pmtmp[];
extern struct phash phtmp[];

/* use patternd from file if it exists, otherwise use compiled in patterns */

g2status initpatterns(void)
{
#ifdef PATEDIT
	if (pt != NULL && pt != pttmp)
	{
		freepats();
	}
	if (readpatterns("c:\\go\\wingovs\\patterns.dat") == G2OK)
		return G2OK;
	else
		outerr("can't read patterns.dat\n");
#endif
	maxpatterns = nextpatterntmp;
	maxpatmoves = nextfreemovetmp;
	nexthash = nexthashtmp;
	patfiles = patfilestmp;
	pt = pttmp;
	pm = pmtmp;
	ph = phtmp;
	hstarta = hstartatmp;
	hstarte = hstartetmp;
	hstartc = hstartctmp;
	return G2OK; 
}

#ifdef PATEDIT
/* binary pattern file format:
 *
 * number of patterns, number of moves, number of hash entries
 * pattern structure, move structure, hash structure, file structure
 */

static int readpatterns(char *patfile) {
	FILE *pats;
	unsigned int total;
	char buf[100];
	pats = fopen(patfile,"rb");
	if (pats == NULL) {
		sprintf(buf,getstring(ERRPAGE,8),patfile);
		outerr(buf);
		maxpatterns = 0;
		return(G2BADFILEOPEN);
		}
	hstartc = malloc(sizeof(UINT16)*NUMHASHCHAINS);
	hstarte = malloc(sizeof(UINT16)*NUMHASHCHAINS);
	hstarta = malloc(sizeof(UINT16)*NUMHASHCHAINS);
	if (hstartc == NULL || hstarte == NULL || hstarta == NULL) {
		sprintf(buf,getstring(ERRPAGE,10),(int)maxpatterns);
		outerr(buf);
		fclose(pats);
		maxpatterns = 0;
		return(G2FAIL);
		}
	if (fread(&maxpatterns,sizeof(UINT16),1,pats) != 1 ||
		fread(&maxpatmoves,sizeof(UINT16),1,pats) != 1 ||
		fread(&nexthash,sizeof(UINT16),1,pats) != 1 ||
		fread(hstartc,sizeof(UINT16),NUMHASHCHAINS,pats) != NUMHASHCHAINS ||
		fread(hstarte,sizeof(UINT16),NUMHASHCHAINS,pats) != NUMHASHCHAINS ||
		fread(hstarta,sizeof(UINT16),NUMHASHCHAINS,pats) != NUMHASHCHAINS) {
		        sprintf(buf,getstring(ERRPAGE,9), patfile);
			outerr(buf);
			fclose(pats);
			maxpatterns = 0;
			return(G2BADFILEACC);
			}
	if (maxpatterns == 0)
		outerr(getstring(ERRPAGE,7));
	total = maxpatterns * (sizeof(struct newpattern));
	if (total < 30000) {
		outerr(getstring(26,29));
		maxpatterns = 0;
		return G2BADFILEACC;
		}
	pt = malloc(total);
	if (pt == NULL) {
		sprintf(buf,getstring(ERRPAGE,10),(int)maxpatterns);
		outerr(buf);
		fclose(pats);
		maxpatterns = 0;
		return(G2FAIL);
		}
	if (fread(pt,total,1,pats) != 1) {
		sprintf(buf,getstring(ERRPAGE,11), patfile);
		outerr(buf);
		fclose(pats);
		maxpatterns = 0;
		return(G2BADFILEACC);
		}
	total = maxpatmoves * (sizeof(struct pmove));
	pm = malloc(total);
	if (pm == NULL) {
		outerr(getstring(ERRPAGE,12));
		fclose(pats);
		maxpatterns = 0;
		return(G2FAIL);
		}
	if (fread(pm,total,1,pats) != 1) {
		sprintf(buf,getstring(ERRPAGE,13),patfile);
		outerr(buf);
		fclose(pats);
		maxpatterns = 0;
		return(G2BADFILEACC);
		}
	total = nexthash * (sizeof(struct phash));
	ph = malloc(total);
	if (ph == NULL) {
		outerr(getstring(ERRPAGE,14));
		fclose(pats);
		maxpatterns = 0;
		return(G2FAIL);
		}
	if (fread(ph,total,1,pats) != 1) {
		sprintf(buf,getstring(ERRPAGE,15),patfile);
		outerr(buf);
		fclose(pats);
		maxpatterns = 0;
		return(G2BADFILEACC);
		}
	total = NUMPATFILES * (sizeof(struct pfile));
	patfiles = malloc(total);
	if (patfiles == NULL) {
		outerr(getstring(ERRPAGE,14));
		fclose(pats);
		maxpatterns = 0;
		return G2FAIL;
		}
	if (fread(patfiles,total,1,pats) != 1) {
		sprintf(buf,getstring(ERRPAGE,16),patfile);
		outerr(buf);
		fclose(pats);
		maxpatterns = 0;
		return(G2BADFILEACC);
		}
	fclose(pats);
	return(G2OK);
	}
#endif

void freepats(void) {
	if (pt == pttmp)
		return;
#ifdef PATEDIT
	if (pt != NULL) {
		free(pt);
		pt = NULL;
		}
	if (pm != NULL) {
		free(pm);
		pm = NULL;
		}
	if (ph != NULL) {
		free(ph);
		ph = NULL;
		}
	if (patfiles != NULL) {
		free(patfiles);
		patfiles = NULL;
		}
	if (hstarta != NULL) {
		free(hstarta);
		hstarta = NULL;
		}
	if (hstartc != NULL) {
		free(hstartc);
		hstartc = NULL;
		}
	if (hstarte != NULL) {
		free(hstarte);
		hstarte = NULL;
		}
#endif
	}

