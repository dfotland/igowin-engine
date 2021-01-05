/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */


/* tree structure routines for go 
 *
 * trees can be sorted by value
 *
 */
#include "g2def.h"
#include "g2tree.h"
#include <search.h>
#include <stdio.h>
#include <stdlib.h>

#pragma warning( disable : 4996) 

extern void lookbackup(sqr_t);
extern void lookmove(sqr_t, int);
extern void outerr(char *);
extern void addcomment(char *);
extern void outstone(sqr_t, char *);
extern void unhighlight(void);
extern char *getstring(int,int);

struct tree tr[MAXTREE];

# define TR_MAKEVAL(prob, result)  ((prob<<4) | result)

tree_t freetreenodes;  /* freelist */

void initree(void) {
	int i;
	for (i = 0; i < MAXTREE; ++i) {
		tr[i].sib = i+1;
	}
	tr[MAXTREE-1].sib = NONE;
	freetreenodes = 0;
}

/* how many tree nodes are in use now */
int usedtreenodes(void) {
	int count = 0;
	tree_t i;
	for (i = freetreenodes; i != NONE; i = tr[i].sib)
		count++;
	return MAXTREE - count;
}

/* count nodes in this t and its siblings */

int counttree(tree_t t) {
	int cnt = 0;
	while(t != NONE) {
		++cnt;
		if (tr[t].child != NONE)
			cnt += counttree(tr[t].child);
		t = tr[t].sib;
	}
	return cnt;
}


/* count evaluated nodes in this t and its siblings */

int countevaltree(tree_t t) {
	int cnt = 0;
	while(t != NONE) {
		if (tr[t].evaluated)
			++cnt;
		if (tr[t].child != NONE)
			cnt += countevaltree(tr[t].child);
		t = tr[t].sib;
		}
	return cnt;
	}

/* return number of lost tree elements */
extern int countfights(void);

int checktree(void) {
	int count = 0;
	tree_t i;
	for (i = freetreenodes; i != NONE; i = tr[i].sib)
		count++; 
	count += countfights();
	return(MAXTREE-count-1);
	}

#ifndef SMALLCODE

char *results[] = {
"Failure",
"Likely Failure",
"Unknown",
"Ko",
"Likely Success",
"Success",
};
	
static char *realresults[] = {
"Failure",
"Failure",
"Unclear",
"Ko",
"Success",
"Success",
};

extern char *clr[];
extern int oppvalue[];

/* return result string and probability in fight */
/* probability is 0 to 100 */
char *treeres(tree_t t, int *prob) {
	*prob = TR_PROB(t);
	return results[TR_RESULT(t)];
	}

char *opptreeres(tree_t t, int *prob) {
	*prob = 100-TR_PROB(t);
	return results[oppvalue[TR_RESULT(t)]];
	}

int treeresval(tree_t t) {
	return TR_RESULT(t);
	}


/* the gralprob corrsponding to the reading result for tree, and the current alprob estimate after the move */
/* live is true if this is a trytolive fight */
/* in the range of -50 to 50
/* Success get -25 to 50 */
/* likely success get -50 to 40 */
/* ko gets -33 to 33 */
/* unknown gets the passed in estimate */
/* likely failure gets 0 to -50 */
/* failure gets 0 to -50 */

int treealprob(tree_t t, int alprob, int live) {
	switch(TR_RESULT(t)) {
	case V_WIN:
		alprob =  -25+TR_PROB(t)*75/100;
		break;
	case V_WINLIKELY:
		alprob = -50+TR_PROB(t)*95/100;
		break;
	case V_WINKO:
		alprob = -33+TR_PROB(t)*66/100;
		break;
	case V_UNKNOWNRES:
		if (!live)
			alprob = -alprob;
		break;	/* just use best estimate */
	case V_LOSELIKELY:
		alprob = -50+TR_PROB(t)/2;
		break;
	case V_LOSE:
		alprob = -50+TR_PROB(t)/2;
		break;
		}
	if (!live)
		alprob = -alprob;	/* trying to kill, so success == death */
	return alprob; 
}

char *treesummary(tree_t t, char *buf) {
	int size;
	size = countevaltree(tr[t].child);
	if (TR_RESULT(t) <= V_UNKNOWNRES)
		sprintf(buf, "Can't find a solution. %s: %d%% confidence.  Looked at %d positions.\n",
			results[TR_RESULT(t)], TR_PROB(t), size);
	else
		sprintf(buf,"%s: %d%% confidence.  Looked at %d positions.\n",
			results[TR_RESULT(t)], TR_PROB(t), size);
	return buf;
}

#define MAXTREECHILD 100
struct chsort {
	tree_t child;
	int value;
} chval[MAXTREECHILD];

int sortfunc(const void *s1, const void *s2) {
	struct chsort *t1 = (struct chsort *)s1;
	int v1 = t1->value;
	struct chsort *t2 = (struct chsort *)s2;
	int v2 = t2->value;
	return v2-v1;
}

static void treesortchildren(tree_t t) {
	int count = 0;
	tree_t i;
	int ch;
	for (i = tr[t].child; i != NONE; i = tr[i].sib) {
		chval[count].child = i;
		chval[count].value = TR_PROB(i);
		count++;
	}
	if (count <= 1)
		return;
	qsort(chval, count, sizeof(struct chsort), sortfunc); 
	tr[t].child = chval[0].child;
	for (ch = 0; ch < count-1; ++ch)
		tr[chval[ch].child].sib = chval[ch+1].child;
	tr[chval[count-1].child].sib = NONE;
}

static void dtree(tree_t t, int c, int depth) {
	char buf[200];
	tree_t i;
	if (depth == 1)
	{
		addcomment(treesummary(t, buf));
#ifdef G2DEBUGOUTPUT
		sprintf(buf, "%d generated nodes.\n", counttree(tr[t].child));
		addcomment(buf);
#endif
	}
	treesortchildren(t);
	for (i = tr[t].child; i != NONE; i = tr[i].sib) {
		if (tr[i].s == NOSQUARE)
			continue;
#ifndef G2DEBUGOUTPUT
		if (!tr[i].evaluated)
			continue;	// don't show unevaluated moves in release version
#endif
		lookmove(tr[i].s,c);
		dtree(i, 1-c, depth+1);

#ifdef G2DEBUGOUTPUT
		if (!tr[i].evaluated)
			addcomment("Not evaluated.\n");
#endif
		if (tr[i].child != NONE)
		{
#ifdef G2DEBUGOUTPUT
			sprintf(buf,"%s: %d%% chance of success. %d%% generated confidence.  %d%% best failing child. Trial %d. %d moves in this tree. ob is %d.  kocolor %d\n",
				results[TR_RESULT(i)],
				TR_PROB(i), tr[i].genval, tr[i].bf, tr[i].trial, countevaltree(tr[i].child), tr[i].ob, tr[i].kocolor);
			addcomment(buf);
#else
			sprintf(buf,"%s: %d%% chance of success.\n%d moves in this tree.\n",
				results[TR_RESULT(i)],
				TR_PROB(i), countevaltree(tr[i].child));
			addcomment(buf);
			if (depth == 1 && TR_RESULT(i) >= 4)
			{
				sprintf(buf, "Correct answer\n");
				addcomment(buf);
			}
#endif
		}
		else 
		{
#ifdef G2DEBUGOUTPUT
			if (tr[i].kocolor != NOCOLOR) {
				sprintf(buf, "%s used ko threat(s)\r\n", clr[tr[i].kocolor]);
				addcomment(buf);
			}
			sprintf(buf,"Leaf position, trial %d, %s: %d%% confidence. %d%% generated confidence, bf is %d, ob is %d.\n",
				tr[i].trial, results[TR_RESULT(i)],
				TR_PROB(i), tr[i].genval, tr[i].bf, tr[i].ob);
#else
			sprintf(buf,"Final position, %s\n%d%% confidence.\n", 
				realresults[TR_RESULT(i)], TR_PROB(i));
#endif
			addcomment(buf);
		}
		lookbackup(tr[i].s);
	}
}
	
void dumptree(tree_t t, int c) {
	dtree(t, c, 1);
}

#endif

/* get a node from the free list */

static tree_t getnode(sqr_t s, int prob, int result) {
	tree_t ret;
	if (freetreenodes == NONE) {
#ifdef G2DEBUGOUTPUT
		outerr("Out of nodes");
#endif
		return NONE;
	}
	ret = freetreenodes;
	freetreenodes = tr[freetreenodes].sib;
	tr[ret].child = NONE;
	tr[ret].sib = NONE;
	tr[ret].s = s;
	tr[ret].val = TR_MAKEVAL(prob, result);
	tr[ret].genval = prob;
	tr[ret].bf = prob;
	tr[ret].evaluated = FALSE;
	tr[ret].ob = FALSE;
	tr[ret].kocolor = NOCOLOR;
	tr[ret].trial = -1;
	return ret;
	}

/* allocate a new tree.  return the root node */

tree_t newtree(void)
{
	tree_t tmp = getnode(NOSQUARE, 0, V_LOSE);
	return tmp;
	
}

/* delete all of the child subtrees of parent */
void deletechildren(tree_t parent) {
	tree_t ch, sb;
	if (tr[parent].child == NONE)
		return;	/* no children to delete */
	ch = tr[parent].child;
	tr[parent].child = NONE;
	do {
		sb = tr[ch].sib;
		tr[ch].sib = NONE;
		freetree(ch);
		ch = sb;
	} while(ch != NONE);
}

/* delete the child subtree from the parent node and free it */
void deletetree(tree_t parent, tree_t child) {
	tree_t tmp;
	if (tr[parent].child == child) {	/* first child */
		tr[parent].child = tr[child].sib;
		tr[child].sib = NONE;
		freetree(child);
		return;
	}
	for (tmp = tr[parent].child; tr[tmp].sib != child && tr[tmp].sib != NONE; tmp = tr[tmp].sib)
		;
	if (tr[tmp].sib == child) {
		tr[tmp].sib = tr[child].sib;
		tr[child].sib = NONE;
		freetree(child);
		return;
	}
	ASSERT(TRUE);	/* should never get here! */
}

/* return a tree to the free list */

void freetree(tree_t t) {
	if (t == NONE)
		return;
	if (tr[t].child != NONE) {
		freetree(tr[t].child);
		tr[t].child = NONE;
		}
	if (tr[t].sib != NONE) {
		freetree(tr[t].sib);
		tr[t].sib = NONE;
		}
	tr[t].sib = freetreenodes;
	freetreenodes = t;
	}

/* return the best node that is a child of this node */
tree_t bestchild(tree_t node, int *which)
{
	tree_t child, best = NONE;
	int bval = -1000, bres = V_LOSE;
	int count = 0;
	*which = 0;
	if (node == NONE)
		return NONE;
	for (child = tr[node].child; child != NONE; child = tr[child].sib) {
		count++;
		if (TR_RESULT(child) > bres) {
			bres = TR_RESULT(child);
			bval = TR_PROB(child);
			best = child;
			*which = count;
		}
		else if (TR_RESULT(child) == bres && TR_PROB(child) > bval) {
			bval = TR_PROB(child);
			best = child;
		}
	}
	return best;
}

#if 0

/* add a new child to node.  adds it as the first child */

tree_t addfirstchild(tree_t node, sqr_t s, int prob, int result)
{
	int newnode;
	tree_t t;
	newnode = getnode(s, prob, result);	/* TODO: check for NONE */
	if (newnode == NONE)
		return NONE;
	if (tr[node].child == NONE)
		tr[node].child = newnode;
	else {
		t = tr[node].child;
		tr[node].child = newnode;
		tr[newnode].sib = t;
	}
	tr[newnode].kocolor = tr[node].kocolor;
	return newnode;
}
#endif

/* add a new child to node.  adds it as the last child */

tree_t addlastchild(tree_t node, sqr_t s, int prob, int result)
{
	int newnode;
	tree_t t;
	newnode = getnode(s, prob, result);
	if (newnode == NONE)
		return NONE;
	if (tr[node].child == NONE)
		tr[node].child = newnode;
	else {
		t = tr[node].child;
		while(tr[t].sib != NONE)
			t = tr[t].sib;
		tr[t].sib = newnode;
	}
	tr[newnode].kocolor = tr[node].kocolor;
	return newnode;
}

#ifdef NEVER
/* add a new child to node.  adds it sorted by value (highest to lowest) */

tree_t addsortchild(tree_t node, sqr_t s, int prob, int result) {
	int new;
	tree_t t, tmp;
	new = getnode(s, prob, result);
	if (tr[node].child == NONE)  /* first child */
		tr[node].child = new;
	else if (tr[tr[node].child].val < val) {  /* new first child */
		t = tr[node].child;
		tr[node].child = new;
		tr[new].sib = t;
		}
	else {
		t = tr[node].child;
		while(tr[t].sib != NONE) {
			if (tr[tr[t].sib].val < val)break;
			t = tr[t].sib;
			}
		tmp = tr[t].sib;
		tr[t].sib = new;
		tr[new].sib = tmp;
		}
	tr[new].kocolor = tr[node].kocolor;
	return new;
	}
#endif
    
/* add the child tree c to the parent tree p, sorted by value (highest first) */    
    
void mrgsortchild(tree_t p, tree_t c) {
	tree_t t, tmp;
	if (tr[p].child == NONE)
		tr[p].child = c;
	else if (tr[tr[p].child].val < tr[c].val) {  /* new first child */
		t = tr[p].child;
		tr[p].child = c;
		tr[c].sib = t;
		}
	else {
		t = tr[p].child;
		while(tr[t].sib != NONE) {
			if (tr[tr[t].sib].val < tr[c].val)break;
			if (tr[c].val >= V_WINLIKELY &&
				tr[tr[t].sib].val == tr[c].val &&
				counttree(tr[tr[t].sib].child) > counttree(tr[c].child))
				break;
			t = tr[t].sib;
			}
		tmp = tr[t].sib;
		tr[t].sib = c;
		tr[c].sib = tmp;
		}

	}
	
/* set the value of a node and mark it evaluated */	
void setvalue(tree_t t, int prob, int result) {
	tr[t].val = TR_MAKEVAL(prob, result);
	tr[t].evaluated = TRUE;
	}

void setkocolor(tree_t t, int c) {
	tr[t].kocolor = c;
	}
	
void settrial(tree_t t, int val) {
	tr[t].trial = val;
}
