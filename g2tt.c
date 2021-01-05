/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */
#include "g2hd.h"
#include "g2tt.h"

/* transposition table for full board search 
 * 2 way associative tt for seach, and a separate 2 way associative tt for evaluations
 */

#define TTSIZE (1024 * 4)
#define KEYMASK (TTSIZE - 1)

#define TRUE 1
#define FALSE 0

/* search results */
struct ttnode {
	unsigned int key1;
	int value;
	float depth;
	sqr_t next;
	char flag;
};

/* evaluation results */
struct tenode {
	unsigned int key1;
	int value;
	sqr_t first;	/* first move in this line, for strat bonuses */ 
	char flag;
};

struct ttnode t1[TTSIZE + 1];		// replace with closest to root (biggest depth)
struct ttnode t2[TTSIZE + 1];		// replace with t1 victim

struct tenode e1[TTSIZE + 1];		// replace with closest to root (biggest depth)
struct tenode e2[TTSIZE + 1];		// replace with t1 victim


void tt_init() {
	int i;
	for (i = 0; i < TTSIZE+1; ++i) {
		t1[i].flag = t2[i].flag = e1[i].flag = e2[i].flag = TTUNUSED;
	}
}

int tt_lookup(__int64 hash, int *flag, float *depth, int *value, sqr_t *m) {
	unsigned int index = (int)(hash & KEYMASK);
	if (t1[index].flag != TTUNUSED &&
		t1[index].key1 == (unsigned int)(hash>>32)) {
		*flag = t1[index].flag;
		*depth = t1[index].depth;
		*value = t1[index].value;
		*m = t1[index].next;
		return TRUE;
	}
	else if (t2[index].flag != TTUNUSED &&
		t2[index].key1 == (unsigned int)(hash>>32)) {
		*flag = t2[index].flag;
		*depth = t2[index].depth;
		*value = t2[index].value;
		*m = t2[index].next;
		return TRUE;
	}
	return FALSE;
}


void addentry(__int64 hash, int flag, float depth, int value, sqr_t m) {
	unsigned int index = (int)(hash & KEYMASK);
	if (t1[index].depth == depth && 
		t1[index].key1 == (unsigned int)(hash>>32)) {		// actual same position
												// more recent must be more accurate?
		t1[index].flag = flag;
		t1[index].depth = depth;
		t1[index].value = value;
		t1[index].next = m;
	}
	else if (t1[index].flag == TTUNUSED) {		// unused entry
		t1[index].key1 = (unsigned int)(hash>>32);
		t1[index].flag = flag;
		t1[index].depth = depth;
		t1[index].value = value;
		t1[index].next = m;
	}
	else if (t1[index].depth < depth) {		// replace and move victim
		if (t1[index].key1 != (unsigned int)(hash>>32)) {	// only move if different position
			t2[index].key1 = t1[index].key1;
			t2[index].flag = t1[index].flag;
			t2[index].depth = t1[index].depth;
			t2[index].value = t1[index].value;
			t2[index].next = t1[index].next;
		}

		t1[index].key1 = (unsigned int)(hash>>32);
		t1[index].flag = flag;
		t1[index].depth = depth;
		t1[index].value = value;
		t1[index].next = m;
	}
	else {								// just put in victim way
		t2[index].key1 = (unsigned int)(hash>>32);
		t2[index].flag = flag;
		t2[index].depth = depth;
		t2[index].value = value;
		t2[index].next = m;
	}
}

int te_lookup(__int64 hash, sqr_t s, int *value ) {
	unsigned int index = (unsigned int)(hash & KEYMASK);
	if (e1[index].flag == EXACT &&
		e1[index].key1 == (unsigned int)(hash>>32) &&
		e1[index].first == s) {
		*value = e1[index].value;
		return TRUE;
	}
	else if (e2[index].flag == EXACT &&
		e2[index].key1 == (unsigned int)(hash>>32) &&
		e2[index].first == s) {
		*value = e2[index].value;
		return TRUE;
	}
	return FALSE;
}


void te_addentry(__int64 hash, int value, sqr_t m) {
	unsigned int index = (unsigned int)(hash & KEYMASK);
	if (e1[index].key1 == (unsigned int)(hash>>32)) {		// actual same position
												// more recent must be more accurate?
		e1[index].value = value;
		e1[index].first = m;
	}
	else if (e1[index].flag == TTUNUSED) {		// unused entry
		e1[index].key1 = (unsigned int)(hash>>32);
		e1[index].value = value;
		e1[index].first = m;
		e1[index].flag = EXACT;
	}
	else {								// just put in victim way
		e2[index].key1 = (unsigned int)(hash>>32);
		e2[index].value = value;
		e2[index].first = m;
		e2[index].flag = EXACT;
	}
}

