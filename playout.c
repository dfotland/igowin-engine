
/*
 * playout.c
 *
 * playout a game for UCT-MC
 */

//#include <malloc.h>
#include <stdlib.h>
#include <memory.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#ifdef WIN32
#include <process.h>
#endif
#include "ui2g2.h"
#include "playout.h"
#include "learn.h"

#pragma warning(disable : 4996)

#define PO_BITS

/*
 * board layout is 19x19 embedded in a 20x21 array, with 1 extra on end for lower right diagaonal of last point
 * left point in each row is off the board.  1 more is added to make the size even so arrays of shorts are on int boundaries
 */

#define PO_UP_BIT 1
#define PO_LEFT_BIT 2
#define PO_RIGHT_BIT 4
#define PO_DOWN_BIT 8

#define PO_NONE 0

#define PO_TM_HASH 400
#define PO_KO_HASH 500

static int po_edge[PO_BSIZE];		/* distance to edge of the board (0 corner or off board, 1 edge, 2, ... to center */

extern const __int64 zobhash64[500][2];

/* how many bits are set? */
const char po_count_bits[16] = {
	0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
};

/* offset to the first empty point if one bit is set */
const int po_bit_offs1[16] = {
	0, -PO_SZ, -1, -PO_SZ, 1, -PO_SZ, -1, -PO_SZ, PO_SZ, -PO_SZ, -1, -PO_SZ, 1, -PO_SZ, -1, -PO_SZ
};

/* offset to the second empty point if two bits are set */
const int po_bit_offs2[16] = {
	0, 0, 0, -1, 0, 1, 1, -1, 0, PO_SZ, PO_SZ, -1, PO_SZ, 1, 1, -1 
};

/* offsets for each bit set in a bit map */
const int po_bit_offs[16][4] = 
{
0,		0,		0,		0,
-PO_SZ,	0,		0,		0,
-1,		0,		0,		0,
-PO_SZ,	-1,		0,		0,
1,		0,		0,		0,
-PO_SZ,	1,		0,		0,
-1,		1,		0,		0,
-PO_SZ,	-1,		1,		0,
PO_SZ,	0,		0,		0,
-PO_SZ,	PO_SZ,	0,		0,
-1,		PO_SZ,	0,		0,
-PO_SZ,	-1,		PO_SZ,	0,
1,		PO_SZ,	0,		0,
-PO_SZ,	1,		PO_SZ,	0,
-1,		1,		PO_SZ,	0,
-PO_SZ,	-1,		1,		PO_SZ,
};

/* does a move make a superko position? */
int po_superko(struct po_board *b)
{
	int m;

	for (m = b->mnum - 2; m >= 0; m--) {
		if (b->hash[m] == b->hash[b->mnum]) {		/* superko */
			return TRUE;
		}
	}

	return FALSE;
}

/* 
 * convert sqr_t to playout move
 */
int po_to_po(sqr_t s, int size) 
{
	int x = s % size;
	int y = s / size;
	if (s == PASS)
		return PO_PASS;
	return PO_SZ * (y + 1) + x + 1;
}
 
/* 
 * convert playout move to sqr_t
 */
sqr_t po_from_po(int s, int size) 
{
	int x = s % (PO_SZ) - 1;
	int y = s / (PO_SZ) - 1;
	if (s == PO_PASS)
		return PASS;
	return size * y + x;
}

short *po_getmoves(struct po_board *b, short *mnum) 
{
	*mnum = b->mnum;
	return b->moves;
}

/*
 * random number from 0 to range - 1
 */
int po_random(struct po_board *b, int range)
{
#if PO_RANDOM_64
	b->seed = b->seed * 2862933555777941757 + 3037000493;
	return ((unsigned int)(b->seed >> 16)) % range; 
#else
	b->seed = ((unsigned int)b->seed) * 392314069 + 1;
	return (((unsigned int)(b->seed) >> 16) % range);		/* upper bits are more random */
#endif
}

/*
 * one time initialize the playout system
 */
void po_init_all(void)
{
	po_pattern_init();
}

void po_init_edge(int size)
{
	int i;
	for (i = 0; i < PO_BSIZE; ++i) {
		po_edge[i] = i % PO_SZ;
		if (i / PO_SZ < po_edge[i]) {
			po_edge[i] = i / PO_SZ;
		}
		if (size + 1 - (i % PO_SZ) < po_edge[i]) {
			po_edge[i] = size + 1 - (i % PO_SZ);
		}
		if (size + 1 - (i / PO_SZ) < po_edge[i]) {
			po_edge[i] = size + 1 - (i / PO_SZ);
		}
		if (i == PO_SZ + 1 || i == PO_SZ + size || i == PO_SZ * size + 1 || i == PO_SZ * size + size) {
			po_edge[i] = 0;
		}
		if (i <= PO_SZ || i % PO_SZ == 0 || i % PO_SZ > size || i >= PO_SZ * (size + 1)) {
			po_edge[i] = 0;
		}
	}
}

/*
 * initialize a new empty board
 */
void po_init(struct po_board *b, int handicap, int komi, int size, __int64 seed)
{
	int i;
#if PO_RANDOM_INIT
	int r;
#endif
	b->hash[0] = 0;
	b->empty_num = 0;
	b->po_offs[1] = -PO_SZ;
	b->po_offs[2] = -1;
	b->po_offs[4] = 1;
	b->po_offs[8] = PO_SZ;
	b->one_lib_num[0] = b->one_lib_num[1] = 0;
	b->captured_num = 0;
	b->passcount[PO_BLACK] = 0; 
	b->passcount[PO_WHITE] = 0;
	b->seed = seed;
	for (i = 0; i < PO_BSIZE; ++i) {
		b->p_first[i] = PO_EMPTY;
		b->p_g[i] = PO_NONE;
		b->p_pat[i] = 0;
		if (i <= PO_SZ || i % PO_SZ == 0 || i % PO_SZ > size || i >= PO_SZ * (size + 1)) {
			b->p_c[i] = PO_NO_COLOR;
			b->po_onboard[i] = 0;
		} else {
			b->p_c[i] = PO_EMPTY;
#if PO_RANDOM_INIT
			if (b->empty_num) {
				r = po_random(b, b->empty_num);
				b->empty_idx[b->empty_num] = b->empty_idx[r];
				b->p_idx[b->empty_idx[r]] = b->empty_num;

				b->empty_idx[r] = i;
				b->p_idx[i] = r;

				b->empty_num++;
			} else {
				b->p_idx[i] = 0;
				b->empty_idx[b->empty_num++] = i;
			}
#else
			b->p_idx[i] = b->empty_num;
			b->empty_idx[b->empty_num++] = i;
#endif
			b->po_onboard[i] = 0xff;	/* all bits set */
		}
		b->p_next_g[i] = PO_NONE;
		b->g_last_g[i] = PO_NONE;
		b->g_libs[i] = 0;
		b->g_size[i] = 0;
		b->p_nbr_num[i][0] = b->p_nbr_num[i][1] = 0;
	}
	for (i = 0; i < PO_BSIZE; ++i) {
		b->p_empty_bits[i] = 0;
#ifdef PO_LIBS
		b->p_lib_bits[i] = 0;
#endif
		if (b->p_c[i] == PO_NO_COLOR) {
			continue;
		}
		if (b->p_c[i - PO_SZ] != PO_NO_COLOR) {
			b->p_empty_bits[i] |= PO_UP_BIT;
		}
		if (b->p_c[i - 1] != PO_NO_COLOR) {
			b->p_empty_bits[i] |= PO_LEFT_BIT;
		}
		if (b->p_c[i + 1] != PO_NO_COLOR) {
			b->p_empty_bits[i] |= PO_RIGHT_BIT;
		}
		if (b->p_c[i + PO_SZ] != PO_NO_COLOR) {
			b->p_empty_bits[i] |= PO_DOWN_BIT;
		}
	}
	b->stones[0] = b->stones[1] = 0;
	b->ko = PO_NONE;
	b->tm = PO_BLACK;
	b->komi = komi;
	b->boardsize = size;
	b->mnum = 0;
}

/*
 * alloc a new empty board
 * thread safe
 */
struct po_board *po_alloc(int komi, int size, __int64 seed)
{
	struct po_board *b = (struct po_board *)malloc(sizeof(struct po_board));
	if (!b) {
		return b;
	}
	po_init(b, 0, komi, size, seed);
	return b;
}

/*
 * copy a board, preserving the random seed
 */
void po_copy(struct po_board *dst, struct po_board *src)
{
	__int64 seed = dst->seed;
	memcpy(dst, src, sizeof(struct po_board));
	dst->seed = seed;
}


void po_free(struct po_board *b)
{
	free(b);
}

/*
 * make a psss move during a playout
 * since playout must take stones off the board it must count chinese style and there is no Japanese bonus for passing
 * passes by one side may be forced because there are no legal moves otherwise, while the other side is capturing
 */
static void po_makepass(struct po_board *b)
{
	b->reason[b->mnum] = PO_REASON_PO_PASS;
	b->moves[b->mnum] = (PO_PASS << 1) + b->tm;
/*	b->passcount[b->tm]++; DO NOT put this back!!!!  Chinese score is not changed by a pass in the playout, so they are irrelevant */
	b->mnum++;
	b->tm = 1 - b->tm;
	b->ko = PO_NONE;
}

/*
 * count liberties of g, just for debug
 */
#ifdef PO_LIBS
static int po_lib_count(struct po_board *b, int g)
{
	int p = g;
	int libs = 0;
	do {
		libs += po_count_bits[b->p_lib_bits[p]];
	} while ((p = b->p_next_g[p]) != PO_NONE);
	return libs;
}
#endif

/*
 * make a move m (not a pass and not a suicide) on board b
 * must be thread safe, so no references to globals allowed
 * rave to record first player for rave updates
 */
static void po_make_move(struct po_board *b, int m, int c, int rave)
{
	int i, j, n, p, p2, g, g1, g2, n1, n2, n3, n4;
	int capsize;
	int found, found_g2;
	int *pat;
	assert(b->p_g[m] == PO_NONE);
	assert(m >= 0 && m <= (PO_SZ) * (PO_SZ) && b->p_c[m] != PO_NO_COLOR);
	assert(m != b->ko);
	assert(c == 0 || c == 1);

	/* put the stone on the board */
	b->p_c[m] = c;
	b->stones[c]++;
	b->captured_num = 0;
	b->empty_idx[b->p_idx[m]] = b->empty_idx[--b->empty_num];
	b->p_idx[b->empty_idx[b->empty_num]] = b->p_idx[m];
	b->moves[b->mnum] = (m << 1) + c;

	/* for RAVE */
	if (rave && b->p_first[m] == PO_EMPTY && b->mnum < b->rave_moves) { 
		b->p_first[m] = c;
		b->p_first_pat[m] = b->p_pat[m];
	}

	/* fix up patterns */
	/* from center empty point pattern index p_pat[] gets 0 or 1 for stone to right, 3 or 6 to upper-right, then continues counter-clockwise */
	pat = po_pat_index[c];	/* constants */
	b->p_pat[m]++;		/* just to make it nonzero so pat value zero means 3x3 empty points */
	b->p_pat[m - PO_SZ - 1] += *pat++;
	b->p_pat[m - PO_SZ] += *pat++;
	b->p_pat[m - PO_SZ + 1] += *pat++;
	b->p_pat[m + 1] += *pat++;
	b->p_pat[m + PO_SZ + 1] += *pat++;
	b->p_pat[m + PO_SZ] += *pat++;
	b->p_pat[m + PO_SZ - 1] += *pat++;
	b->p_pat[m - 1] += *pat++;

	/* remove empty point and liberty from adjacent points */
	b->p_empty_bits[m - PO_SZ] &= ~PO_DOWN_BIT;
	b->p_empty_bits[m - 1] &= ~PO_RIGHT_BIT;
	b->p_empty_bits[m + 1] &= ~PO_LEFT_BIT;
	b->p_empty_bits[m + PO_SZ] &= ~PO_UP_BIT;

#ifdef PO_LIBS
	b->p_lib_bits[m - PO_SZ] &= ~PO_DOWN_BIT;
	b->p_lib_bits[m - 1] &= ~PO_RIGHT_BIT;
	b->p_lib_bits[m + 1] &= ~PO_LEFT_BIT;
	b->p_lib_bits[m + PO_SZ] &= ~PO_UP_BIT;
#endif

	/* adjust groups for new stone */
	if (b->p_nbr_num[m][c] == 0) {				/* new single stone group */
		assert(m != b->ko);
		b->p_g[m] = m;
		b->p_next_g[m] = PO_NONE;
		b->g_last_g[m] = m;
		b->g_libs[m] = po_count_bits[b->p_empty_bits[m]];
#ifdef PO_LIBS
		b->p_lib_bits[m] = b->p_empty_bits[m];	/* new group all empty nbrs are liberties */
#endif
		b->g_size[m] = 1;

		/* existing enemy neighbors are groups, and are already set up.  There are no existing friendly neighbors */
 		if (b->p_empty_bits[m] & PO_UP_BIT) {
			p = m - PO_SZ;
			b->p_nbr[p][c][b->p_nbr_num[p][c]++] = m;
		}
 		if (b->p_empty_bits[m] & PO_LEFT_BIT) {
			p = m - 1;
			b->p_nbr[p][c][b->p_nbr_num[p][c]++] = m;
		}
 		if (b->p_empty_bits[m] & PO_RIGHT_BIT) {
			p = m + 1;
			b->p_nbr[p][c][b->p_nbr_num[p][c]++] = m;
		}
 		if (b->p_empty_bits[m] & PO_DOWN_BIT) {
			p = m + PO_SZ;
			b->p_nbr[p][c][b->p_nbr_num[p][c]++] = m;
		}
	} else {									/* add stone to existing group */
		g = b->p_nbr[m][c][0];
		b->p_g[m] = g;
		b->p_next_g[b->g_last_g[g]] = m;
		b->g_last_g[g] = m;
		b->p_next_g[m] = PO_NONE;
		b->g_libs[g]--;
		for (i = 1; i < 16; i <<= 1) {
			if (!(b->p_empty_bits[m] & i)) {
				continue;
			}
			p = m + b->po_offs[i];
			found = 0;
			for (j = 0; j < b->p_nbr_num[p][c]; ++j) {
				if (b->p_nbr[p][c][j] == g) {
					found = 1;
					break;
				}
			}
			if (!found) {
				b->p_nbr[p][c][b->p_nbr_num[p][c]++] = g;
				b->g_libs[g]++;
#ifdef PO_LIBS
				b->p_lib_bits[m] |= i;
#endif
			}
		}

		b->g_size[g]++;
		for (n = 1; n < b->p_nbr_num[m][c]; ++n) {			/* merge up to 3 more groups */
			g1 = b->p_g[m];
			g2 = b->p_nbr[m][c][n];

			/* merge smaller into larger (g2 into g1) */
			if (b->g_size[g2] > b->g_size[g1]) {
				g = g1;
				g1 = g2;
				g2 = g;
				b->g_libs[g1]--;	/* need accurate libs since this group will stick around */
			}
			p2 = g2;
			do {
				b->p_g[p2] = g1;
#ifdef PO_LIBS
				b->p_lib_bits[p2] = 0;
#endif
				for (i = 1; i < 16; i <<= 1) {
					if (!(b->p_empty_bits[p2] & i)) {
						continue;
					}
					p = p2 + b->po_offs[i];
					found = 0;
					found_g2 = -1;
					for (j = 0; j < b->p_nbr_num[p][c]; ++j) {
						if (b->p_nbr[p][c][j] == g2) {
							found_g2 = j;
						}
						if (b->p_nbr[p][c][j] == g1) {
							found = 1;
						}
					}
					if (found) {	/* already a libery of g1 */
						if (found_g2 != -1) {
							b->p_nbr[p][c][found_g2] = b->p_nbr[p][c][--b->p_nbr_num[p][c]];	/* remove g2 */
						}
					} else {
						assert(found_g2 != -1);
						b->p_nbr[p][c][found_g2] = g1;
						b->g_libs[g1]++;
#ifdef PO_LIBS
						b->p_lib_bits[p2] |= i;
#endif
					}
				}
			} while ((p2 = b->p_next_g[p2]) != PO_NONE);  /* inf loop here once in 9x9 game with 60 min time limits */
		
			b->p_next_g[b->g_last_g[g1]] = g2;
			b->g_last_g[g1] = b->g_last_g[g2];
			b->g_size[g1] += b->g_size[g2];
		}
	}

	/* reduce liberty count and capture enemy neighbors */
	capsize = 0;
	for (i = 0; i < b->p_nbr_num[m][1 - c]; ++i) {
		if (--b->g_libs[b->p_nbr[m][1 - c][i]] != 0) {
#if PO_CAPTURE
			/* record one liberty group - no need to record when I make myslef one liberty since there is already code to capture last one-lib group */
			g = b->p_nbr[m][1 - c][i];
			if (b->g_libs[g] == 1 && b->one_lib_num[1-c] < PO_BSIZE) {
				b->one_lib[1-c][b->one_lib_num[1-c]++] = g;
			}
#endif
			continue;
		}
		p = b->p_nbr[m][1 - c][i];
		capsize += b->g_size[p];
		b->stones[b->p_c[p]] -= b->g_size[p];
		assert(b->stones[b->p_c[p]] >= 0);
		do {
			assert(p != m);		/* can't capture myself */
			b->p_g[p] = PO_NONE;
			b->p_c[p] = PO_EMPTY;
			b->empty_idx[b->empty_num] = p;
			b->p_idx[p] = b->empty_num++;
			b->captured[b->captured_num++] = p;

			/* add to neighbor's empty neighbors */
			b->p_empty_bits[p - PO_SZ] |= PO_DOWN_BIT;
			b->p_empty_bits[p - PO_SZ] &= b->po_onboard[p - PO_SZ];
			b->p_empty_bits[p + PO_SZ] |= PO_UP_BIT;
			b->p_empty_bits[p + PO_SZ] &= b->po_onboard[p + PO_SZ];
			b->p_empty_bits[p - 1] |= PO_RIGHT_BIT;
			b->p_empty_bits[p - 1] &= b->po_onboard[p - 1];
			b->p_empty_bits[p + 1] |= PO_LEFT_BIT;
			b->p_empty_bits[p + 1] &= b->po_onboard[p + 1];

			/* fix up patterns */
			pat = po_pat_index[1 - c];
			b->p_pat[p]--;
			b->p_pat[p - PO_SZ - 1] -= *pat++;
			b->p_pat[p - PO_SZ] -= *pat++;
			b->p_pat[p - PO_SZ + 1] -= *pat++;
			b->p_pat[p + 1] -= *pat++;
			b->p_pat[p + PO_SZ + 1] -= *pat++;
			b->p_pat[p + PO_SZ] -= *pat++;
			b->p_pat[p + PO_SZ - 1] -= *pat++;
			b->p_pat[p - 1] -= *pat++;

			/* neighbors for new empty point and liberties of adjacent groups */
			b->p_nbr_num[p][1-c] = 0;
			b->p_nbr_num[p][c] = 0;
			n1 = b->p_g[p - PO_SZ];
			n2 = b->p_g[p + PO_SZ];
			n3 = b->p_g[p - 1];
			n4 = b->p_g[p + 1];
			if (b->p_c[p - PO_SZ] == c) {
				b->p_nbr[p][c][b->p_nbr_num[p][c]++] = n1;
				b->g_libs[n1]++;
#ifdef PO_LIBS
				b->p_lib_bits[p - PO_SZ] |= PO_DOWN_BIT;
#endif
			}
			if (n2 != n1 && b->p_c[p + PO_SZ] == c) {
				b->p_nbr[p][c][b->p_nbr_num[p][c]++] = n2;
				b->g_libs[n2]++;
#ifdef PO_LIBS
				b->p_lib_bits[p + PO_SZ] |= PO_UP_BIT;
#endif
			}
			if (n3 != n2 && n3 != n1 && b->p_c[p - 1] == c) {
				b->p_nbr[p][c][b->p_nbr_num[p][c]++] = n3;
				b->g_libs[n3]++;
#ifdef PO_LIBS
				b->p_lib_bits[p - 1] |= PO_RIGHT_BIT;
#endif
			}
			if (n4 != n3 && n4 != n2 && n4 != n1 && b->p_c[p + 1] == c) {
				b->p_nbr[p][c][b->p_nbr_num[p][c]++] = n4;
				b->g_libs[n4]++;
#ifdef PO_LIBS
				b->p_lib_bits[p + 1] |= PO_LEFT_BIT;
#endif
			}

		} while ((p = b->p_next_g[p]) != PO_NONE);
	}

	/* no more nbr groups since this point is no longer empty */
	b->p_nbr_num[m][0] = b->p_nbr_num[m][1] = 0;

	/* set ko */
	if (capsize == 1 && b->g_size[b->p_g[m]] == 1 && b->g_libs[b->p_g[m]] == 1) {
		for (i = 1; i < 16; i <<= 1) {
			if (b->p_empty_bits[m] & i) {
				b->ko = m + b->po_offs[i];
				break;
			}
		}
		assert(b->p_empty_bits[b->ko] == 0);
	} else {
		b->ko = PO_NONE;
	}

	b->tm = 1 - c;
	b->mnum++;

#ifdef PO_LIBS
	assert(po_lib_count(b, b->p_g[m]) == b->g_libs[b->p_g[m]]);
#endif
	assert(b->stones[0] + b->stones[1] < b->boardsize * b->boardsize);
	assert(b->p_nbr_num[m][c] == 0 && b->p_nbr_num[m][1 - c] <= 4 && b->p_nbr_num[m][1 - c] >= 0);
	assert(b->g_libs[b->p_g[m]] > 0);		/* suicide not allowed */
	assert(b->g_libs[b->p_g[m]] <= b->g_size[b->p_g[m]] * 2 + 2);
}

/*
 * update the hash value and make the move or pass
 */
void po_make_uct_move(struct po_board *b, int m, int c)
{
	int i, p;

	/* pass */
	if (m == PO_PASS) {
		b->moves[b->mnum] = (PO_PASS << 1) + b->tm;
		b->mnum++;
		b->hash[b->mnum] = b->hash[b->mnum - 1];
		b->passcount[b->tm]++;
		b->tm = 1 - c;
		b->ko = PO_NONE;
		b->reason[b->mnum - 1] = PO_REASON_UCT;
		return;
	}

	/* move */
	b->hash[b->mnum + 1] = b->hash[b->mnum] ^ zobhash64[m][c];
	for (i = 0; i < b->p_nbr_num[m][1 - c]; ++i) {
		if (b->g_libs[b->p_nbr[m][1 - c][i]] != 1) {
			continue;
		}
		p = b->p_nbr[m][1 - c][i];
		do {
			b->hash[b->mnum + 1] ^= zobhash64[p][1 - c];
		} while ((p = b->p_next_g[p]) != PO_NONE);
	}
	b->reason[b->mnum] = PO_REASON_UCT;
	po_make_move(b, m, c, FALSE);
#if UCT_LEARN
	learn_hash_move(b->mnum);
	learn_hash_stone(po_from_po(m, b->boardsize), c, b->boardsize, b->mnum);
	for (i = 0; i < b->captured_num; ++i) {
		learn_hash_stone(po_from_po(b->captured[i], b->boardsize), 1 - c, b->boardsize, b->mnum);
	}
#endif
}

/***************************************************************************************************
 * Move generation
 **************************************************************************************************/

static int po_add_group(struct po_board *b, int m, int c, int count, int *groups)
{
	int i;
	if (b->p_c[m] == c) {
		for (i = 0; i < count; ++i) {
			if (groups[i] == b->p_g[m])break;
		}
		if (i == count) {
			groups[count++] = b->p_g[m];
		}
	}
	return count;
}

/* find the enemy neighbor groups adjacent to group at point m (or group m)
 * return the number of groups, and an array of the group numbers
 */
static int po_enemy_nbrs(struct po_board *b, int m, int *groups)
{
	int count = 0;
	int p;
	int c = 1 - b->p_c[m];
	
	for (p = b->p_g[m]; p != PO_NONE; p = b->p_next_g[p]) {
		count = po_add_group(b, p + 1, c, count, groups);
		count = po_add_group(b, p - 1, c, count, groups);
		count = po_add_group(b, p + PO_SZ, c, count, groups);
		count = po_add_group(b, p - PO_SZ, c, count, groups);
	}
	return count;
}


/* 
 * capture a one liberty group g.  Return the move to capture it, or PO_PASS 
 */
static int po_capture(struct po_board *b, int g)
{
	int p, p2;
	for (p = g; p != PO_NONE; p = b->p_next_g[p]) {
		if (b->p_empty_bits[p] == 0) {
			continue;
		}
		p2 = p + po_bit_offs1[b->p_empty_bits[p]];
		if (b->ko == p2) {
			return PO_PASS;
		}
		return p2;
#if 0
		/* replace with more efficient one above */
		if (b->p_empty_bits[p] & PO_UP_BIT && b->ko != p - PO_SZ) {
			return p - PO_SZ;
		}
		if (b->p_empty_bits[p] & PO_LEFT_BIT && b->ko != p - 1) {
			return p - 1;
		}
		if (b->p_empty_bits[p] & PO_RIGHT_BIT && b->ko != p + 1) {
			return p + 1;
		}
		if (b->p_empty_bits[p] & PO_DOWN_BIT && b->ko != p + PO_SZ) {
			return p + PO_SZ;
		}
#endif
	}
	return PO_PASS;
}

/* 
 * is m (a move to a point with zero or one adjacent empty point) a self atari move 
 */
static int po_self_atari(struct po_board *b, int m)
{
	int i, j, g, p, p2, empty_nbrs, two_lib_groups = 0, groups[4];
	int newlibs = 0, liberties[3];				/* liberties of the new group */

	empty_nbrs = po_count_bits[b->p_empty_bits[m]];	/* 0 or 1 */
	assert(empty_nbrs <= 1);

	/* ok if we capture an enemy nbr (TODO and it is not a snapback) */
	for (i = 0; i < b->p_nbr_num[m][1 - b->tm]; ++i) {
		g = b->p_nbr[m][1 - b->tm][i];
		if (b->g_libs[g] == 1) {
			if ((empty_nbrs == 1 || b->g_size[g] > 1 || newlibs)) {
				return FALSE;
			}
			newlibs++;	/* get a liberty when we capture */
		}
	}

	/* ok if adjacent friend has more than two liberties */
	
	/* handle easy cases */
	for (i = 0; i < b->p_nbr_num[m][b->tm]; ++i) {
		g = b->p_nbr[m][b->tm][i];
		if (b->g_libs[g] >= 3) {		/* must have at leat 2 liberties */
			return FALSE;
		}
		if (b->g_libs[g] == 2) {
			groups[two_lib_groups++] = g;
		}
	}

	if (two_lib_groups && newlibs) {
		return FALSE;	/* one liberty and one capture */
	}

	/* must have only one liberty */
	if (!two_lib_groups || !empty_nbrs && !newlibs && two_lib_groups == 1) {
		return TRUE;
	}

	/* find all the new liberties */
	if (empty_nbrs) {
		liberties[0] = m + po_bit_offs1[b->p_empty_bits[m]];
		newlibs++;
	}
	for (i = 0; i < two_lib_groups; ++i) {
		p = groups[i];
		do {
			if (!b->p_empty_bits[p]) {
				continue;
			}
			p2 = p + po_bit_offs1[b->p_empty_bits[p]];
			liberties[newlibs++] = p2;
			for (j = 0; j < newlibs-1; ++j) {
				if (liberties[j] == p2) {
					newlibs--;
					break;
				}
			}
			if (newlibs > 2) {		/* 2 - one for the empty filled by this move, and one for the single liberty */
				return FALSE;
			}
			p2 = p + po_bit_offs2[b->p_empty_bits[p]];
			if (p2 == p) {
				continue;
			}
			liberties[newlibs++] = p2;
			for (j = 0; j < newlibs-1; ++j) {
				if (liberties[j] == p2) {
					newlibs--;
					break;
				}
			}
			if (newlibs > 2) {		/* 2 - one for the empty filled by this move, and one for the single liberty */
				return FALSE;
			}
		} while ((p = b->p_next_g[p]) != PO_NONE);

	}
	return TRUE;
}

/*
 * return the number of pointy ends for the shape made by a move at m,
 * or return 10 if there is a connection to a friendly group, so no nakade
 */
static int count_ends(struct po_board *b, int m) 
{
	int corner = FALSE;
	int ends = 0;
	int c = b->tm;
	int p, p2, i;
	int nbr = 0;
	int liberty = 0;	/* the liberty of the nakade */
	int groups[4];
	int next_group = 0;

	if (b->p_c[m + 1] == c) nbr++;
	if (b->p_c[m - 1] == c) nbr++;
	if (b->p_c[m + PO_SZ] == c) nbr++;
	if (b->p_c[m - PO_SZ] == c) nbr++;
	if (nbr == 1) ends++;
	if (b->p_empty_bits[m]) 
		liberty = m + po_bit_offs1[b->p_empty_bits[m]];

	for (i = 0; i < b->p_nbr_num[m][c]; ++i) {
		groups[next_group++] = b->p_nbr[m][c][i];
		for (p = b->p_nbr[m][c][i]; p != PO_NONE; p = b->p_next_g[p]) {
			nbr = 0;
			if (b->p_c[p + 1] == c || p + 1 == m) nbr++;
			if (b->p_c[p - 1] == c || p - 1 == m) nbr++;
			if (b->p_c[p + PO_SZ] == c || p + PO_SZ == m) nbr++;
			if (b->p_c[p - PO_SZ] == c || p - PO_SZ == m) nbr++;
			if (nbr == 1) {
				ends++;
			} else if (po_edge[p] == 0) {
				corner = TRUE;
			}
			if (!liberty && b->p_empty_bits[p]) {
				if (p + po_bit_offs1[b->p_empty_bits[p]] != m)
					liberty = p + po_bit_offs1[b->p_empty_bits[p]];
				else if (po_count_bits[b->p_empty_bits[p]] > 1 && 
					p + po_bit_offs2[b->p_empty_bits[p]] != m)
					liberty = p + po_bit_offs2[b->p_empty_bits[p]];
			}
		}
	}
	if (corner && ends == 2) {
		ends++;			/* to recognize bent 4 in the corner */
	}
	/* does liberty have a neighbor that is not in groups?  if so there is a connection, and this is not a nakade */
	if (liberty) {
		for (p = 0; p < b->p_nbr_num[liberty][c]; ++p) {
			p2 = b->p_nbr[liberty][c][p];
			for (i = 0; i < next_group; ++i) {
				if (groups[i] == p2)
					break;
			}
			if (i == next_group) {
				return 10;	/* there is a connection avaiable, so not nakade */
			}
		}
	}

	return ends;
}

/*
 * does move m, a self-atari, make a nakade shape?  If TRUE, we can move here.  If FALSE, prune this move
 * we know it is a self-atari, so we know there is 0 or 1 empty adjaacent point, and at least one enemy nbr
 * and no capturable enemy nbr
 */
static int po_nakade_self_atari(struct po_board *b, int m)
{
	int i, size, ends, nbrs;
	int g;  /* one of the friendly nbr grops */
	int groups[PO_BSIZE];

	/* only self atari if it makes nakade shape.  Otherwise it might be a seki.  1,2, or 3 stones are usually ok. */
	size = 1;	/* number of my stones that will be capturable after my move */
	nbrs = b->p_nbr_num[m][b->tm];
	for (i = 0; i < nbrs; ++i) {
		g = b->p_nbr[m][b->tm][i];
		size += b->g_size[g];
	}
	if (size >= 6) {
		return FALSE;
	}
	switch(size) {
		case 5:
			ends = count_ends(b, m);
			if (ends != 4 && ends != 1) {
				return FALSE;
			}
			break;
		case 4:
			ends = count_ends(b, m);
			if (ends == 10 || ends == 2) {
				return FALSE;
			}
			break;
		case 3:
			if (count_ends(b, m) == 10) {
				return FALSE;
			}
			if (nbrs >= 2) {
				return TRUE;	/* connect to make the nakade */
			}
			if (po_enemy_nbrs(b, g, groups) > 1) {
				return FALSE;	/* stil an outside liberty, or maybe a seki, or he is already alive */
			}
			break;
		case 2:
			if (b->p_nbr_num[m][1 - b->tm] == 1 && b->g_libs[b->p_nbr[m][1 - b->tm][0]] > 2)  {		/* no atari or capture or cut, just a waste */
				return FALSE;
			}
			break;
		case 1:
			if (b->p_nbr_num[m][1 - b->tm] == 1 && b->g_libs[b->p_nbr[m][1 - b->tm][0]] > 2)  {		/* no atari or capture or cut, just a waste */
				return FALSE;
			}
			break;
	}

#if 0
	else if (size > 1) {	/* always allow small throwins */
		ends = count_ends(b, m);
		if (ends == 10)
			return TRUE;
	}
#endif

	return TRUE;
}

/*
 * add moves to attack g which has a small number of liberties
 */
static int po_attack(struct po_board *b, int g, short *movelist, int *move_value, int *move_reason, int nextmove, int val)
{
	int p, p2, i, count, count2;
	for (p = g; p != PO_NONE; p = b->p_next_g[p]) {
		count = po_count_bits[b->p_lib_bits[p]];
		if (count == 0) {
			continue;
		}
		for (i = 0; i < count; ++i) {
			p2 = p + po_bit_offs[b->p_lib_bits[p]][i];
			assert(b->p_c[p2] == PO_EMPTY);
			count2 = po_count_bits[b->p_empty_bits[p2]];
			if (count2 == 0) {		/* illegal? */
				continue;
			}
			if (count2 > 1 || !po_self_atari(b, p2)
			//	|| po_nakade_self_atari(b, p2)
				) {
				movelist[nextmove] = p2;
				move_value[nextmove] = val + PO_ATTACK_VAL;
				move_reason[nextmove] = PO_REASON_PO_ATTACK_NBR;
				nextmove++;
			}
		}
	}
	return nextmove;
}

/*
 * gen a move to save group g, which has one liberty
 */
static int po_gen_save(struct po_board *b, short g, short *move_list, int *move_value, int *move_reason, int nextmove, int for_uct)
{
	int p, p2, empty;
	int val = PO_SAVE_VAL + b->g_size[g] * PO_SAVE_STONE_VAL;

#if PO_SAVE_CAPTURE
	/* capture neighbor group with one liberty - weaker */
	int c = 1 - b->p_c[g];
	for (p = g; p != PO_NONE; p = b->p_next_g[p]) {
		if (b->p_c[p + 1] == c && b->g_libs[b->p_g[p + 1]] == 1) {
			move_list[nextmove] = po_capture(b, b->p_g[p + 1]);
			if (move_list[nextmove] != PO_PASS) {
				move_value[nextmove] = val + b->g_size[b->p_g[p + 1]] * PO_SAVE_CAP_VAL;
				move_reason[nextmove] = PO_REASON_PO_SAVE;
				nextmove++;
			}
		}
		if (b->p_c[p - 1] == c && b->g_libs[b->p_g[p - 1]] == 1) {
			move_list[nextmove] = po_capture(b, b->p_g[p - 1]);
			if (move_list[nextmove] != PO_PASS) {
				move_value[nextmove] = val + b->g_size[b->p_g[p + 1]] * PO_SAVE_CAP_VAL;
				move_reason[nextmove] = PO_REASON_PO_SAVE;
				nextmove++;
			}
		}
		if (b->p_c[p + PO_SZ] == c && b->g_libs[b->p_g[p + PO_SZ]] == 1) {
			move_list[nextmove] = po_capture(b, b->p_g[p + PO_SZ]);
			if (move_list[nextmove] != PO_PASS) {
				move_value[nextmove] = val + b->g_size[b->p_g[p + 1]] * PO_SAVE_CAP_VAL;
				move_reason[nextmove] = PO_REASON_PO_SAVE;
				nextmove++;
			}
		}
		if (b->p_c[p - PO_SZ] == c && b->g_libs[b->p_g[p - PO_SZ]] == 1) {
			move_list[nextmove] = po_capture(b, b->p_g[p - PO_SZ]);
			if (move_list[nextmove] != PO_PASS) {
				move_value[nextmove] = val + b->g_size[b->p_g[p + 1]] * PO_SAVE_CAP_VAL;
				move_reason[nextmove] = PO_REASON_PO_SAVE;
				nextmove++;
			}
		}
	}
#if 0
	if (nextmove && po_random(b, 100) < 80) {
		return nextmove;	/* bias toward capture - slower but no strogner */
	}
#endif
#endif

	/* play in its liberty */
	for (p = g; p != PO_NONE; p = b->p_next_g[p]) {
		if (b->p_empty_bits[p]) {
			p2 = p + b->po_offs[b->p_empty_bits[p]];	/* the liberty */
#if PO_SAVE_NO_SELF_ATARI
			empty = po_count_bits[b->p_empty_bits[p2]];
			/* extend one stone for 3 space dead shape */
			if (b->g_size[g] == 1 && empty == 1 && b->p_nbr_num[p2][b->tm] == 1 && 
				po_count_bits[b->p_empty_bits[p2 + b->po_offs[b->p_empty_bits[p2]]]] == 1 &&
				b->p_nbr_num[p2 + b->po_offs[b->p_empty_bits[p2]]][b->tm] == 0) {
					move_value[nextmove] = val;
					move_reason[nextmove] = PO_REASON_PO_NAKADE;
					move_list[nextmove++] = p2;
			}
			/* save small group unconditionally */
			else if (empty > 2 || 
				empty == 2 &&  b->g_size[g] <= 2 ||  
				empty <= 1 && !po_self_atari(b, p2)) {
#else
			if (b->g_size[g] <= 2 || po_count_bits[b->p_empty_bits[p2]] > 2 || b->p_nbr_num[p2][b->p_c[g]] > 1) {
#endif
				move_value[nextmove] = val;
				move_reason[nextmove] = PO_REASON_PO_SAVE;
				move_list[nextmove++] = p2;		/* play in liberty */
			}
			/* finish a ladder - changed for best-184! */
#if PO_FORCE_LADDER
			else if (!for_uct && nextmove == 0 && empty == 2 && b->p_g[b->moves[b->mnum - 2] >> 1] == g && b->p_g[b->moves[b->mnum - 4] >> 1] == g) {
				move_value[nextmove] = val + PO_LADDER_VAL;
				move_reason[nextmove] = PO_REASON_PO_LADDER;
				move_list[nextmove++] = p2;		/* play in liberty */
			}
#endif
			break;
		}
	}

	return nextmove;
}

/*
 * gen a move to strengthen group g, which has two libertie
 */
static int po_gen_save2(struct po_board *b, short g, short *move_list, int *move_value, int *move_reason, int nextmove, int for_uct)
{
	int p, p2, empty, i;
	int val = PO_SAVE_VAL + b->g_size[g] * PO_SAVE_STONE_VAL;
	int groups[PO_BSIZE];
	int count;

#if PO_SAVE_CAPTURE
	/* capture neighbor group with one liberty - weaker? */
	int c = 1 - b->p_c[g];
	for (p = g; p != PO_NONE; p = b->p_next_g[p]) {
		if (b->p_c[p + 1] == c && b->g_libs[b->p_g[p + 1]] == 1) {
			move_list[nextmove] = po_capture(b, b->p_g[p + 1]);
			if (move_list[nextmove] != PO_PASS) {
				move_value[nextmove] = val + b->g_size[b->p_g[p + 1]] * PO_SAVE2_CAP_VAL;
				move_reason[nextmove] = PO_REASON_PO_SAVE2;
				nextmove++;
			}
		}
		if (b->p_c[p - 1] == c && b->g_libs[b->p_g[p - 1]] == 1) {
			move_list[nextmove] = po_capture(b, b->p_g[p - 1]);
			if (move_list[nextmove] != PO_PASS) {
				move_value[nextmove] = val + b->g_size[b->p_g[p + 1]] * PO_SAVE2_CAP_VAL;
				move_reason[nextmove] = PO_REASON_PO_SAVE2;
				nextmove++;
			}
		}
		if (b->p_c[p + PO_SZ] == c && b->g_libs[b->p_g[p + PO_SZ]] == 1) {
			move_list[nextmove] = po_capture(b, b->p_g[p + PO_SZ]);
			if (move_list[nextmove] != PO_PASS) {
				move_value[nextmove] = val + b->g_size[b->p_g[p + 1]] * PO_SAVE2_CAP_VAL;
				move_reason[nextmove] = PO_REASON_PO_SAVE2;
				nextmove++;
			}
		}
		if (b->p_c[p - PO_SZ] == c && b->g_libs[b->p_g[p - PO_SZ]] == 1) {
			move_list[nextmove] = po_capture(b, b->p_g[p - PO_SZ]);
			if (move_list[nextmove] != PO_PASS) {
				move_value[nextmove] = val + b->g_size[b->p_g[p + 1]] * PO_SAVE2_CAP_VAL;
				move_reason[nextmove] = PO_REASON_PO_SAVE2;
				nextmove++;
			}
		}
	}

#endif

	/* capture neighbor group with two liberties */
	count = po_enemy_nbrs(b, g, groups);
	for (i = 0; i < count; i++) {
		if (b->g_libs[groups[i]] == 2) {
			nextmove = po_attack(b, groups[i], move_list, move_value, move_reason, nextmove, val);
		}
	}

	/* play in its liberties */
	for (p = g; p != PO_NONE; p = b->p_next_g[p]) {
		if (b->p_lib_bits[p]) {
			p2 = p + po_bit_offs1[b->p_lib_bits[p]];	/* a liberty */
			empty = po_count_bits[b->p_empty_bits[p2]];

			if (empty > 2 || empty == 2 && b->p_nbr_num[p2][b->tm] > 1) {
				move_value[nextmove] = val;
				move_reason[nextmove] = PO_REASON_PO_SAVE2;
				move_list[nextmove++] = p2;		/* play in liberty */
			}
		}
		if (po_count_bits[b->p_lib_bits[p]] == 2) {
			p2 = p + po_bit_offs2[b->p_lib_bits[p]];	/* a liberty */
			empty = po_count_bits[b->p_empty_bits[p2]];

			if (empty > 2) {
				move_value[nextmove] = val;
				move_reason[nextmove] = PO_REASON_PO_SAVE2;
				move_list[nextmove++] = p2;		/* play in liberty */
			}
		}
	}

	return nextmove;
}


/*
 * what is the score?  + good for white.  white wins ties
 */
int po_score(struct po_board *b, int rules)
{
	int i, m;
	int score = 0;
	for (i = 0; i < b->empty_num; ++i) {
		m = b->empty_idx[i];
		if (b->p_empty_bits[m]) {
			continue;
		}
		if (b->p_nbr_num[m][PO_BLACK] && b->p_nbr_num[m][PO_WHITE]) {	/* seki */
			continue;
		}
		if (b->p_nbr_num[m][PO_BLACK]) {
			score--;
		} else {
			score++;
		}
	}
	if (rules == JAPANESE) {
		score += b->passcount[PO_BLACK] - b->passcount[PO_WHITE];
	}
	return b->komi + b->stones[PO_WHITE] - b->stones[PO_BLACK] + score;
}

/* do we control the diagonals of m to make a potential eye? */
static int po_diag_control(struct po_board *b, int m) 
{
	int dcount, ocount;

	/* is this an eye? surrounded by one group, or control 3 diags */
	if (b->p_nbr_num[m][b->tm] == 1) {
		return 1;	/* eye */
	}
	dcount = 0;
	ocount = 0;
	if (b->p_c[m - PO_SZ - 1] == 1 - b->tm) {
		if (b->g_libs[b->p_g[m - PO_SZ - 1]] == 1 && (b->ko == PO_NONE || b->g_size[b->p_g[m - PO_SZ - 1]] != 1)) {		// TODO - what about ko !!!!
			ocount++;
		}
		dcount++;
	}
	if (b->p_c[m - PO_SZ + 1] == 1 - b->tm) {
		if (b->g_libs[b->p_g[m - PO_SZ + 1]] == 1 && (b->ko == PO_NONE || b->g_size[b->p_g[m - PO_SZ + 1]] != 1)) {
			ocount++;
		}
		dcount++;
	}
	if (b->p_c[m + PO_SZ - 1] == 1 - b->tm) {
		if (b->g_libs[b->p_g[m + PO_SZ - 1]] == 1 && (b->ko == PO_NONE || b->g_size[b->p_g[m + PO_SZ - 1]] != 1)) {
			ocount++;
		}
		dcount++;
	}
	if (b->p_c[m + PO_SZ + 1] == 1 - b->tm) {
		if (b->g_libs[b->p_g[m + PO_SZ + 1]] == 1 && (b->ko == PO_NONE || b->g_size[b->p_g[m + PO_SZ + 1]] != 1)) {
			ocount++;
		}
		dcount++;
	}
	if (ocount) {	/* if one enemy is in atari, prefer to take it over filling */
		dcount--;
	}

	/* must check for one enemy diagonal to prevent filling own eyes! */
	if (dcount == 0  || dcount == 1 && po_edge[m] > 1) {
		return 1;	/* don't fill eye */
	}
	return 0;
}

/* is a fully surrounded point a legal move?  Not suicide and no ko recapture and doesn't fill an eye */
static int po_legal(struct po_board *b, int m)
{
	int i, dcount, g;
	int ocount;

	assert(b->p_empty_bits[m] == 0);

	/* point has enemy neighbor */
	if (b->p_nbr_num[m][1 - b->tm]) {

		/* capture enemy ok in surrounded point unless it is a ko */
		for (i = 0; i < b->p_nbr_num[m][1 - b->tm]; ++i) {
			g = b->p_nbr[m][1 - b->tm][i];
			if (b->g_libs[g] == 1 && m != b->ko) {
				return 1;
			}
		}

		/* nonsuicide play next to enemy ok */
		for (i = 0; i < b->p_nbr_num[m][b->tm]; ++i) {
			g = b->p_nbr[m][b->tm][i];
			if (b->g_libs[g] > 1) {
				return 1;
			}
		}
		return 0;	/* suicide */
	}

	/* all adjacent points are my color */
	/* is this an eye? surrounded by one group, or control 3 diags */
	if (b->p_nbr_num[m][b->tm] == 1) {
		return 0;	/* eye */
	}
	dcount = 0;
	ocount = 0;
	if (b->p_c[m - PO_SZ - 1] == 1 - b->tm) {
		if (b->g_libs[b->p_g[m - PO_SZ - 1]] == 1 && (b->ko == PO_NONE || b->g_size[b->p_g[m - PO_SZ - 1]] != 1)) {		// TODO - what about ko !!!!
			ocount++;
		}
		dcount++;
	}
	if (b->p_c[m - PO_SZ + 1] == 1 - b->tm) {
		if (b->g_libs[b->p_g[m - PO_SZ + 1]] == 1 && (b->ko == PO_NONE || b->g_size[b->p_g[m - PO_SZ + 1]] != 1)) {
			ocount++;
		}
		dcount++;
	}
	if (b->p_c[m + PO_SZ - 1] == 1 - b->tm) {
		if (b->g_libs[b->p_g[m + PO_SZ - 1]] == 1 && (b->ko == PO_NONE || b->g_size[b->p_g[m + PO_SZ - 1]] != 1)) {
			ocount++;
		}
		dcount++;
	}
	if (b->p_c[m + PO_SZ + 1] == 1 - b->tm) {
		if (b->g_libs[b->p_g[m + PO_SZ + 1]] == 1 && (b->ko == PO_NONE || b->g_size[b->p_g[m + PO_SZ + 1]] != 1)) {
			ocount++;
		}
		dcount++;
	}
	if (ocount) {	/* if one enemy is in atari, prefer to take it over filling */
		dcount--;
	}

	/* must check for one enemy diagonal to prevent filling own eyes! */
	if (dcount == 0  || dcount == 1 && po_edge[m] > 1) {
		return 0;	/* don't fill eye */
	}

	/* not my point eye, ok if not a suicide */
	/* don't suicide */
	for (i = 0; i < b->p_nbr_num[m][b->tm]; ++i) {
		g = b->p_nbr[m][b->tm][i];
		if (b->g_libs[g] > 1) {
			return 1;
		}
	}

	/* suicide */
	return 0;
}

/* TRUE if the one stone suicide at m should be skipped
 * ok if throwin make a ko, or throwin is atari, throwin makes false eye of 2 point space, or makes a seki in a 2 stone space
 */
static int bad_one_stone(struct po_board *b, int m)
{
	int i, s;

	/* throw in is atari */
	for (i = 0; i < b->p_nbr_num[m][1 - b->tm]; ++i) {
		if (b->g_libs[b->p_nbr[m][1 - b->tm][i]] == 2) {
			return FALSE;
		}
	}
	s = m + po_bit_offs1[b->p_empty_bits[m]];

	/* more than 2 point space */
	if (po_count_bits[b->p_empty_bits[s]] > 1) {
		return TRUE;
	}

	/* ko throwin - no enemy nbrs */
	if (!b->p_nbr_num[s][1 - b->tm]) {
		return FALSE;
	}

	/* no friendly neightbors, 2 stone eye */
	if (!b->p_nbr_num[s][b->tm]) {
		return FALSE;	/* for now - TODO check the diagonals for killing eye */
	}

	return TRUE;
}


float uct_close_1[5] =
{
	0, .09f, .07f, .05f, .03f
};

float uct_close_2[5] =
{
	0, .05f, .04f, .03f, .00f
};

static float uct_rave_init(struct po_board *b, struct uct_parent *node, int m, unsigned int child, int distm1, int distm2)
{
#if UCT_RAVE_PRIOR_PATTERNS_ANYWHERE
	int start;
#endif
	float rave_win_rate, close = 0.0f;

	/* default */
	node->rave_plays[child] = UCT_RAVE_FPU_PLAYS;
	rave_win_rate = UCT_RAVE_FPU;
#if UCT_RAVE_NEW_BETA
	child->rave_beta = 1.0f;
#endif

	/* play close */
	if (distm1 <= 4) {
		close += uct_close_1[distm1];
	}
	if (distm2 <= 4) {
		close += uct_close_2[distm2];
	}
	if (close != 0.0f) {
		rave_win_rate += close;
		node->rave_plays[child] = UCT_RAVE_PRIOR_LOCAL_PLAYS;
	}


	/* pattern match anywhere */
#if UCT_RAVE_PRIOR_PATTERNS_ANYWHERE
	start = uct_po_patterns[b->tm][b->p_pat[m]];
#if PO_NO_PATTERN_SELF_ATARI
	if ((po_edge[m] > 1 || (start & 1)) && start >= 24 &&
		(po_count_bits[b->p_empty_bits[m]] > 1 || !po_self_atari(b, m))) {		/* pattern not a self atari */
			rave_win_rate += UCT_RAVE_PRIOR_PATTERN;
			node->rave_plays[child] = UCT_RAVE_PRIOR_LOCAL_PLAYS;
#if UCT_RAVE_PRIOR_CLOSE_ENABLE
			if (distm1 <= 2 || distm2 <= 2) {
				rave_win_rate += UCT_RAVE_PRIOR_CLOSE;
			}
#endif
	}
#else
	if ((po_edge[m] > 1 || (start & 1)) && start >= 24) {
		rave_win_rate += UCT_RAVE_PRIOR_PATTERN
	}
#endif
#endif

#if 0
	/* capture the last move */
	if (b->mnum != 0 && (m = b->moves[b->mnum - 1] >> 1) != PO_PASS && b->g_libs[b->p_g[m]] == 1) {
		prior_capture = po_capture(b, b->p_g[m]);
		if (prior_capture != PO_PASS) {
			node->rave_plays[child] += UCT_RAVE_PRIOR_CAPTURE_PLAYS;
			rave_win_rate += UCT_RAVE_PRIOR_CAPTURE;
			return;
		}
	}
#endif

#if UCT_RAVE_PRIOR_SELF_ATARI_ENABLE
	/* bad to self-atari unless it's a single stone throwin cut.  big nonnakade self atari are pruned elsewhere */
	if (po_count_bits[b->p_empty_bits[m]] <= 1 &&	
		(b->p_nbr_num[m][b->tm] || b->p_nbr_num[m][1 - b->tm] == 1) &&	/* not single stone group or not cut */ 
		po_self_atari(b, m)) {
		rave_win_rate += UCT_RAVE_PRIOR_SELF_ATARI;
	}
#endif
	return rave_win_rate;
}

static int uct_ofs[4][8] = 
{
	{	-PO_SZ - 1, -PO_SZ, -PO_SZ + 1, 1, PO_SZ + 1, PO_SZ, PO_SZ - 1, -1, },
	{	-PO_SZ + 1, 1, PO_SZ + 1, PO_SZ, PO_SZ - 1, -1, -PO_SZ - 1, -PO_SZ, },
	{	PO_SZ + 1, PO_SZ, PO_SZ - 1, -1, -PO_SZ - 1, -PO_SZ, -PO_SZ + 1, 1, },
	{	PO_SZ - 1, -1, -PO_SZ - 1, -PO_SZ, -PO_SZ + 1, 1, PO_SZ + 1, PO_SZ, },
};

/*
 * is there a group on diagonal of p of color c with one liberty?
 */
static int po_one_lib_diag(struct po_board *b, int p, int c)
{
	if (b->p_c[p - PO_SZ - 1] == c && b->g_libs[b->p_g[p - PO_SZ - 1]] == 1)
		return TRUE;
	if (b->p_c[p - PO_SZ + 1] == c && b->g_libs[b->p_g[p - PO_SZ + 1]] == 1)
		return TRUE;
	if (b->p_c[p + PO_SZ - 1] == c && b->g_libs[b->p_g[p + PO_SZ - 1]] == 1)
		return TRUE;
	if (b->p_c[p + PO_SZ + 1] == c && b->g_libs[b->p_g[p + PO_SZ + 1]] == 1)
		return TRUE;
	return FALSE;
}

/*
 * generate local responses to the last move m with priorities
 */
int po_gen_local_moves(struct po_board *b, short *move_list, int *move_value, int *move_reason, int nextmove, int for_uct)
{
	int p, c, i, j, start, m, g;

#if PO_PRESERVE_NAKADE
	int count, corner;
#endif
#if PO_PEEP
	int p2, p3;
#endif
#if PO_ATTACK_LAST
	int l1, l2, l1libs, l2libs;
#endif
	if (b->mnum < 2) {
		return nextmove;
	}
	m = b->moves[b->mnum - 1] >> 1;
	if (m != PO_PASS) {
#if PO_CAPTURE_LAST
		/*
		 * capture the last move.  priority 1.
		 */
		if (b->g_libs[b->p_g[m]] == 1) {
			p = po_capture(b, b->p_g[m]);
			if (p != PO_NONE && 
				(po_count_bits[b->p_empty_bits[p]] > 1 || !po_self_atari(b, p))) {		/* capture is not a self-atari */
				move_reason[nextmove] = PO_REASON_PO_CAPTURE_LAST;
				move_list[nextmove] = p;
				move_value[nextmove] = PO_CAPTURE_LAST_VAL;
				nextmove++;

			}
		} else {	/* capture 1 lib group adjacent to last move */
			for (i = 0; i < po_count_bits[b->p_empty_bits[m]]; ++i) {
				p = m + po_bit_offs[b->p_empty_bits[m]][i];
				for (j = 0; j < b->p_nbr_num[p][1 - b->tm]; ++j) {
					g = b->p_nbr[p][1 - b->tm][j];
					if (b->g_libs[g] == 1) {
						move_reason[nextmove] = PO_REASON_PO_CAPTURE_LAST;
						move_list[nextmove] = p;
						move_value[nextmove] = PO_CAPTURE_LAST_VAL;
						nextmove++;
						break;
					}
				}
				if (nextmove)
					break;
			}
		}
#endif

		/*
		 * save adjacent to last move with one liberty
		 */
		c = b->tm;
		if (b->p_c[m - PO_SZ] == c && b->g_libs[b->p_g[m - PO_SZ]] == 1) {
			nextmove = po_gen_save(b, b->p_g[m - PO_SZ], move_list, move_value, move_reason, nextmove, for_uct);
		}
		if (b->p_c[m - 1] == c && b->g_libs[b->p_g[m - 1]] == 1 && b->p_g[m - 1] != b->p_g[m - PO_SZ]) {
			nextmove = po_gen_save(b, b->p_g[m - 1], move_list, move_value, move_reason, nextmove, for_uct);
		}
		if (b->p_c[m + 1] == c && b->g_libs[b->p_g[m + 1]] == 1 && b->p_g[m + 1] != b->p_g[m - PO_SZ] && b->p_g[m + 1] != b->p_g[m - 1]) {
			nextmove = po_gen_save(b, b->p_g[m + 1], move_list, move_value, move_reason, nextmove, for_uct);
		}
		if (b->p_c[m + PO_SZ] == c && b->g_libs[b->p_g[m + PO_SZ]] == 1 && b->p_g[m + PO_SZ] != b->p_g[m - PO_SZ] && b->p_g[m + PO_SZ] != b->p_g[m - 1] && b->p_g[m + PO_SZ] != b->p_g[m + 1]) {
			nextmove = po_gen_save(b, b->p_g[m + PO_SZ], move_list, move_value, move_reason, nextmove, for_uct);
		}

#if PO_SAVE2
		/*
		 * strengthen adjacent to last move with two liberties
		 */
		if (b->p_c[m - PO_SZ] == c && b->g_libs[b->p_g[m - PO_SZ]] == 2) {
			nextmove = po_gen_save2(b, b->p_g[m - PO_SZ], move_list, move_value, move_reason, nextmove, for_uct);
		}
		if (b->p_c[m - 1] == c && b->g_libs[b->p_g[m - 1]] == 2 && b->p_g[m - 1] != b->p_g[m - PO_SZ]) {
			nextmove = po_gen_save2(b, b->p_g[m - 1], move_list, move_value, move_reason, nextmove, for_uct);
		}
		if (b->p_c[m + 1] == c && b->g_libs[b->p_g[m + 1]] == 2 && b->p_g[m + 1] != b->p_g[m - PO_SZ] && b->p_g[m + 1] != b->p_g[m - 1]) {
			nextmove = po_gen_save2(b, b->p_g[m + 1], move_list, move_value, move_reason, nextmove, for_uct);
		}
		if (b->p_c[m + PO_SZ] == c && b->g_libs[b->p_g[m + PO_SZ]] == 2 && b->p_g[m + PO_SZ] != b->p_g[m - PO_SZ] && b->p_g[m + PO_SZ] != b->p_g[m - 1] && b->p_g[m + PO_SZ] != b->p_g[m + 1]) {
			nextmove = po_gen_save2(b, b->p_g[m + PO_SZ], move_list, move_value, move_reason, nextmove, for_uct);
		}
#endif



		/* preserve a nakade */
#if PO_PRESERVE_NAKADE
		if (b->captured_num == 3) {
			for (i = 0; i < b->captured_num; ++i) {
				if (po_count_bits[b->p_empty_bits[b->captured[i]]] == 2) {
					move_reason[nextmove] = PO_REASON_PO_NAKADE;
					move_value[nextmove] = PO_NAKADE_VAL;
					move_list[nextmove++] = b->captured[i];
					break;
				}
			}
		} else if (b->captured_num == 4) {
			count = 0;
			corner = FALSE;
			for (i = 0; i < b->captured_num; ++i) {
				if (po_count_bits[b->p_empty_bits[b->captured[i]]] == 1) {	/* count pointy ends */
					count++;
				} else if (po_edge[b->captured[i]] == 0) {
					corner = TRUE;
				}
			}
			if (count != 2 || corner) {
				for (i = 0; i < b->captured_num; ++i) {
					if (po_count_bits[b->p_empty_bits[b->captured[i]]] > 1
						&& (count != 2 || po_edge[b->captured[i]])
						) {
						move_reason[nextmove] = PO_REASON_PO_NAKADE;
						move_value[nextmove] = PO_NAKADE_VAL;
						move_list[nextmove++] = b->captured[i];
					}
				}
			}
			
		} else if (b->captured_num == 5) {
			count = 0;
			for (i = 0; i < b->captured_num; ++i) {
				if (po_count_bits[b->p_empty_bits[b->captured[i]]] == 1) {	/* count pointy ends */
					count++;
				}
			}
			if (count == 1 || count == 4) {
				for (i = 0; i < b->captured_num; ++i) {
					if (po_count_bits[b->p_empty_bits[b->captured[i]]] > 2) {
						move_reason[nextmove] = PO_REASON_PO_NAKADE;
						move_value[nextmove] = PO_NAKADE_VAL;
						move_list[nextmove++] = b->captured[i];
					}
				}
			}
		}
#endif
#if PO_ATTACK_LAST
		/* try to kill the last move if it is a 2 liberty group and both liberties are on the last stone (a short ladder), to preserve eye space */
		if (nextmove == 0 && b->g_libs[b->p_g[m]] == 2 && po_count_bits[b->p_empty_bits[m]] == 2) {
			l1 = m + po_bit_offs1[b->p_empty_bits[m]];
			l2 = m + po_bit_offs2[b->p_empty_bits[m]];
			l1libs = po_count_bits[b->p_empty_bits[l1]];
			l2libs = po_count_bits[b->p_empty_bits[l2]];
			/* certain capture in one move */
			if (l1libs <= 1 && b->p_nbr_num[l1][1 - c] == 1 &&
					(po_count_bits[b->p_empty_bits[l2]] > 1 || !po_self_atari(b, l2))) {
				move_reason[nextmove] = PO_REASON_PO_ATTACK;
				move_value[nextmove] = PO_ATK2LIB_VAL;
				move_list[nextmove++] = l2;
			}
			if (l2libs <= 1 && b->p_nbr_num[l2][1 - c] == 1 &&
					(l1libs > 1 || !po_self_atari(b, l1))) {
				move_reason[nextmove] = PO_REASON_PO_ATTACK;
				move_value[nextmove] = PO_ATK2LIB_VAL;
				move_list[nextmove++] = l1;
			}
			/* prevent his atari */
			if (l1libs && b->p_nbr_num[l1][c] && b->g_libs[b->p_nbr[l1][c][0]] == 2) {
				move_reason[nextmove] = PO_REASON_PO_ATTACK;
				move_value[nextmove] = PO_ATK2LIB_VAL;
				move_list[nextmove++] = l1;
			}
			if (l2libs && b->p_nbr_num[l2][c] && b->g_libs[b->p_nbr[l2][c][0]] == 2) {
				move_reason[nextmove] = PO_REASON_PO_ATTACK;
				move_value[nextmove] = PO_ATK2LIB_VAL;
				move_list[nextmove++] = l2;
			}
		}
#endif

		/*3x3 patterns at 8 adjacent points near the last move */
#if PO_SAVE_NO_PATTERNS
		if (nextmove == 0) {
#endif
			for (i = 0; i < 8; ++i) {
				p = m + uct_ofs[0][i];
				if (b->p_c[p] == PO_EMPTY && po_edge[p] != 0) {
#if PO_PEEP
					/* kill 3 pt eye */
					if (b->p_nbr_num[p][c] == 0 && po_count_bits[b->p_empty_bits[p]] == 1) {
						p2 = p + po_bit_offs1[b->p_empty_bits[p]];
						if (po_count_bits[b->p_empty_bits[p2]] == 2 && b->p_nbr_num[p][c] == 0) {
							p3 = p2 + po_bit_offs1[b->p_empty_bits[p2]];
							if (p3 == p) {
								p3 = p2 + po_bit_offs2[b->p_empty_bits[p2]];
							}
							if (b->p_nbr_num[p3][c] == 0 && po_count_bits[b->p_empty_bits[p3]] == 1) {
								move_reason[nextmove] = PO_REASON_PO_PEEP;
								move_value[nextmove] = PO_PEEP_VAL;
								move_list[nextmove++] = p2;
							}
						} else if (b->p_nbr_num[p2][c] == 1 && b->g_size[b->p_nbr[p2][c][0]] == 1 && b->g_libs[b->p_nbr[p2][c][0]] == 1) {
							move_reason[nextmove] = PO_REASON_PO_PEEP;
							move_value[nextmove] = PO_PEEP_VAL;
							move_list[nextmove++] = p2;
						}
					}
#endif
					start = uct_po_patterns[b->tm][b->p_pat[p]];
					if ((po_edge[p] > 1 || (start & 1)) && start >= 24 
#if PO_NO_PATTERN_SELF_ATARI
						&& (po_count_bits[b->p_empty_bits[p]] > 1 || !po_self_atari(b, p))
#endif
#if PO_NO_PATTERN_ONE_LIB
						&& !po_one_lib_diag(b, p, c)
#endif
						) {		/* pattern not a self atari */
						move_reason[nextmove] = PO_REASON_PO_PATTERN;
						move_value[nextmove] = start;
						move_list[nextmove++] = p;
					}
				}
			}
#if PO_SAVE_NO_PATTERNS
		}
#endif
	}

#if UCT_TRIP
	m = b->moves[b->mnum - 2] >> 1;
	if (m != PO_PASS) {
		/* save 2nd to last move */
		if (b->g_libs[b->p_g[m]] == 1) {
			nextmove = po_gen_save(b, b->p_g[m], move_list, move_value, move_reason, nextmove, for_uct);
		}
	}
#endif

	return nextmove;
}

/*
 * generate all legal moves, and maybe a pass.  always generate at least one move.
 * if there is a potential seki and no other moves, generate a pass
 * thread safe
 */
void po_uct_generate(struct po_board *b, struct uct_parent *node, int rules)
{
	int i, m, count = 0, can_pass = TRUE;
	int x, y, xm1, ym1, xm2, ym2, distm1, distm2, sum, best_prior;
	float prior[PO_SZ * PO_SZ];
	float p;
	float rave_win_rate;
	short move_list[PO_BSIZE];
	int move_value[PO_BSIZE];
	int move_reason[PO_BSIZE];
	int nextmove = 0;
	unsigned short *move_p = node->move;

	/* only generate one of the symetric moves */
	if (node->mnum == 1 && b->boardsize == 9 && (b->moves[0] >> 1) == 5 * PO_SZ + 5) {
		node->move[0] = 3 * PO_SZ + 5;
		node->move[1] = 3 * PO_SZ + 4;
		node->num_moves = 2;
		for (i = 0; i < node->num_moves; ++i) {
			node->rave_plays[i] = UCT_RAVE_FPU_PLAYS;
			node->rave_wins[i] = UCT_RAVE_FPU_WINS;
#ifdef UCTDEBUGOUTPUT
			node->rave_prior[i] = 256 * node->rave_wins[i] / node->rave_plays[i];
#endif
			node->prior_mfgo[i] = 0;
			uct_make_child(node, i, node->num_children);
		}
		return;
	}
#if 0
	/* always play first 9x9 move in the center, immediately */
	if (node->mnum == 0 && b->boardsize == 9) {
		node->num_moves = 1;
		node->move[0] = 5 * PO_SZ + 5;
		node->rave_plays[0] = UCT_RAVE_FPU_PLAYS;
		node->rave_wins[0] = UCT_RAVE_FPU_WINS;
#ifdef UCTDEBUGOUTPUT
		node->rave_prior[0] = 128;
#endif
		node->prior_mfgo[0] = 0;
		uct_make_child(node, 0, node->num_children);
		return;
	}
#endif

	memset(prior, 0, PO_SZ *(b->boardsize + 1) * sizeof(float));
#if 0
	for (i = 0; i < PO_SZ *(b->boardsize + 1); ++i) {
		prior[i] = 0.0f;
	}
#endif

	nextmove = po_gen_local_moves(b, move_list, move_value, move_reason, 0, TRUE);
	for (i = 0; i < nextmove; ++i) {
		assert(b->p_c[move_list[i]] == PO_EMPTY);
		p = 0.1f + move_value[i] / 1000.f;
		if (p > prior[move_list[i]])
			prior[move_list[i]] = p;
	}

#if !UCT_TRIP
	/* this can't work since objectives are reversed for 2nd to last move */
	if (b->mnum > 1 && (m = b->moves[b->mnum - 2] >> 1) != PO_PASS) {
		nextmove = po_gen_local_moves(m, b, move_list, move_value, move_reason, 0, TRUE);
		for (i = 0; i < nextmove; ++i) {
			prior[move_list[i]] = 0.1f + move_value[i] / 1000.f;
		}
	}
#endif

	if (b->mnum) {
		xm1 = (b->moves[b->mnum - 1] >> 1) % PO_SZ;
		ym1 = (b->moves[b->mnum - 1] >> 1) / PO_SZ;
	} else {
		xm1 = 100;
		ym1 = 100;
	}

	if (b->mnum > 1) {
		xm2 = (b->moves[b->mnum - 2] >> 1) % PO_SZ;
		ym2 = (b->moves[b->mnum - 2] >> 1) / PO_SZ;
	} else {
		xm2 = 100;
		ym2 = 100;
	}

	assert(node->tm == b->tm);
	assert(node->visits == 0);

	for (i = 0; i < b->empty_num; ++i) {
		m = b->empty_idx[i];

		/* skip illegal suicide and pure eye-filling move */
		if (!b->p_empty_bits[m] && !po_legal(b, m)) {
				continue;
		}
#if UCT_PRUNE
		if (po_count_bits[b->p_empty_bits[m]] <= 1 && 
			b->p_nbr_num[m][b->tm] != 0 &&
			po_self_atari(b, m) &&
			!po_nakade_self_atari(b, m)) {
			continue;
		}
#endif
		if (b->p_empty_bits[m]) {	/* points with adjacent empty points - BROKEN */
			can_pass = FALSE;
		}

#if UCT_PRUNE_EDGE
		if ((po_edge[m] < 3 || po_edge[m] > 4) && m != PO_SZ * 5 + 5 && 
			b->p_pat[m + PO_SZ] + b->p_pat[m - 1] + b->p_pat[m - PO_SZ] + b->p_pat[m + 1] == 0) {
				sum = 0;
				if (po_edge[m + PO_SZ]) sum += b->p_pat[m + PO_SZ + PO_SZ];
				if (po_edge[m - PO_SZ]) sum += b->p_pat[m - PO_SZ - PO_SZ];
				if (po_edge[m + 1]) sum += b->p_pat[m + 2];
				if (po_edge[m -1]) sum += b->p_pat[m -2];
				if (sum == 0)
					continue;
		}
#endif

		*move_p = m;

		x = m % PO_SZ;
		y = m / PO_SZ;
		distm1 = abs(xm1 - x) + abs(ym1 - y);
		distm2 = abs(xm2 - x) + abs(ym2 - y);
		rave_win_rate = uct_rave_init(b, node, m, count, distm1, distm2);
		if (prior[m] != 0.0f) {
			rave_win_rate += prior[m];
			node->rave_plays[count] = UCT_RAVE_PRIOR_LOCAL_PLAYS;
		}
		if (rave_win_rate > 0.95f) {
			rave_win_rate = 0.95f;
		}
		node->rave_wins[count] = (int)(node->rave_plays[count] * rave_win_rate);
#ifdef UCTDEBUGOUTPUT
		node->rave_prior[count] = (int)(256 * node->rave_wins[count] / node->rave_plays[count]);
#endif
		node->prior_mfgo[count] = 0;
		move_p++;
		count++;
	}

	/* try to pass if wins the game */
	/* generate a pass (to avoid filling in a seki */
	if (can_pass ||
		rules == JAPANESE && b->mnum * 3 > b->boardsize * b->boardsize ||
		rules == JAPANESE && b->mnum > 1 && (b->moves[b->mnum - 1] >> 1) == PO_PASS) {
		node->rave_plays[count] = UCT_RAVE_FPU_PLAYS;
		node->rave_wins[count] = (int)(UCT_RAVE_PRIOR_PASS * node->rave_plays[count]);
		node->prior_mfgo[count] = 0;
#ifdef UCTDEBUGOUTPUT
		node->rave_prior[count] = (int)(256 * node->rave_wins[count] / node->rave_plays[count]);
#endif
		*move_p = PO_PASS;
		count++;
		move_p++;
	}

	node->num_moves = count;
	for (i = 0; i < UCT_PROGRESSIVE_START_MOVES; ++i) { 
		best_prior = uct_best_rave(node);
		if (best_prior == UCT_MAX_MOVES)
			break;
		uct_make_child(node, best_prior, node->num_children);
	}
}


/* 
 * get a single non-pass move to make in this position.
 * if there are no moves, make a pass, and continue
 * if there are two passes in a row, return PO_NONE
 */
int po_genmove(struct po_board *b)
{
	int m;	/* the last move played, if it is not a pass */
	int mnum, i, r;
	int start;
	int my_terr = 0, his_terr = 0, no_terr = 0;
	int passcount = 0;
	int trycount = 0;

#if PO_CAPTURE
	int g;
#endif

	short move_list[PO_BSIZE];
	int move_value[PO_BSIZE];		/* cumulative values to this point */
	int move_reason[PO_BSIZE];
	int nextmove;
	int sum;

#ifdef G2DEBUGOUTPUT
	for (i = 0; i < PO_BSIZE; ++i)
		move_value[i] = -1;
#endif

	nextmove = po_gen_local_moves(b, move_list, move_value, move_reason, 0, FALSE);

	/* special case fast for only one move generated */
	if (nextmove == 1) {
		b->reason[b->mnum] = move_reason[0];
		return move_list[0];
	}

	/* return weighted random legal local move */
	while (nextmove) {
		sum = 0;
		for (i = 0; i < nextmove; ++i) {
#ifdef G2DEBUGOUTPUT
			assert(move_value[i] != -1);
#endif
			sum += move_value[i];
		}
		r = po_random(b, sum);
		sum = 0;
		for (i = 0; i < nextmove; ++i) {
			sum += move_value[i];
			if (sum > r)
				break;
		}
		m = move_list[i];

		/* any point with liberty or legal ok */
		if (b->p_empty_bits[m] == 0 && !po_legal(b, m)) {
			nextmove--;
			move_list[i] = move_list[nextmove];
			move_value[i] = move_value[nextmove];
			move_reason[i] = move_reason[nextmove];
			continue;
		}

		b->reason[b->mnum] = move_reason[i];
		return m;
	}

#if PO_CAPTURE
	/*
	 * capture a group
	 */
	for (i = 0; i < b->one_lib_num[1 - b->tm]; ) {
		g = b->one_lib[1 - b->tm][i];
		if (b->g_libs[g] == 1 && b->p_g[g] == g && b->g_size[g] > 2) {
			m = po_capture(b, g);
			i++;
			if (m != PO_NONE && po_random(b, 100) < PO_CAPTURE_STONE * b->g_size[g] && (b->p_empty_bits[m] || po_legal(b, m))) {
				b->reason[b->mnum] = PO_REASON_PO_CAPTURE;
				return m;
			}
		} else {
			b->one_lib[1 - b->tm][i] = b->one_lib[1 - b->tm][b->one_lib_num[1 - b->tm] - 1];
			b->one_lib_num[1 - b->tm]--;
		}
	}
#endif

	/*
	 * generate a pure random legal move not in an eye
	 */
	start = po_random(b, b->empty_num);
	mnum = start;
	b->reason[b->mnum] = PO_REASON_PO_RANDOM;
	do {
		m = b->empty_idx[mnum];

		/* illegal move */
		if (!b->p_empty_bits[m] && !po_legal(b, m)) {
			goto po_next;
		}
#if PO_RANDOM_PRUNE_CENTER
		if (po_edge[m] > 4 && b->p_pat[m + PO_SZ - 1] + b->p_pat[m + PO_SZ + 1] + b->p_pat[m - PO_SZ - 1] + b->p_pat[m - PO_SZ + 1] == 0) {
			goto po_next;
		}
#endif

#if PO_RANDOM_PRUNE_SELF_ATARI
		/* only self atari if it makes nakade shape.  Otherwise it might be a seki.  1,2, or 3 stones are usually ok. */
		if (po_count_bits[b->p_empty_bits[m]] <= 1 /*&& b->p_nbr_num[m][b->tm] != 0*/ && po_self_atari(b, m)) {
			if (!po_nakade_self_atari(b, m)) {
				goto po_next;
			}
			/* self atari that makes a nakade here 
			 * if it is an atari, make it always, otherwise, make it less often
			 */
			if (!b->p_nbr_num[m][1 - b->tm] || b->g_libs[b->p_nbr[m][1 - b->tm][0]] > 2) {
				if (po_random(b, 100) > 50)
					goto po_next;
			}
			b->reason[b->mnum] = PO_REASON_PO_RANDOM_SELF_ATARI;
		}
#endif

#if PO_RANDOM_PRUNE_SEKI
		if (po_count_bits[b->p_empty_bits[m]] == 0 && b->p_nbr_num[m][0] == 1 && b->p_nbr_num[m][1] == 1) {
			g = b->p_nbr[m][1 - b->tm][0];
			if (b->g_size[g] == 3 && b->g_libs[g] == 2 && po_random(b, 100) < 90) {
				goto po_next;
			}
		}
#endif

#if PO_RANDOM_PRUNE_SINGLE_DEAD
		/* don't waste move on point with one empty nbr, no friends, and only one enemy */
		if (po_count_bits[b->p_empty_bits[m]] == 1 && !b->p_nbr_num[m][b->tm] && b->p_nbr_num[m][1 - b->tm] == 1 && b->g_libs[b->p_nbr[m][1 - b->tm][0]] > 2) {
			goto po_next;
		}
#endif

#if PO_RANDOM_PRUNE_EYE
		if (b->p_nbr_num[m][1 - b->tm] == 0 && 
			po_count_bits[b->p_empty_bits[m]] == 1 && 
			b->p_nbr_num[m + po_bit_offs1[b->p_empty_bits[m]]][1 - b->tm] == 0 &&
			po_diag_control(b, m)) {
				goto po_next;
		}
#endif

		return m;

		
po_next:	/* that move was no good, get another one */
		/* do several random tries before giving up and just trying all moves in order */
		if (trycount < 3) {
			start = po_random(b, b->empty_num);
			mnum = start;
			trycount++;
			continue;
		}

		if (b->p_nbr_num[m][1 - b->tm] && b->p_nbr_num[m][b->tm]) {
			no_terr++;
		} else if (b->p_nbr_num[m][1 - b->tm]) {
			his_terr++;
		} else if (b->p_nbr_num[m][b->tm]) {
			my_terr++;
		} else { 
			no_terr++;
		}

		++mnum;
		if (mnum >= b->empty_num) {
			mnum = 0;
		}

		/* need to pass if no legal moves */
		if (mnum == start) {
			if (passcount) {
//				po_makepass(b);
//				if (b->tm == PO_WHITE) {
//					b->passcount[PO_WHITE]++;	/* black made second pass, so white must make another impied pass to make the chinese and japanese scores match */
//				}
				break;
			}
			passcount++;
			po_makepass(b);
			b->reason[b->mnum] = PO_REASON_PO_RANDOM;
			my_terr = his_terr = no_terr = 0;
			trycount = 0;
		}
	} while (1);

	b->terr[b->tm] = my_terr;
	b->terr[1 - b->tm] = his_terr;
	b->terr[NOCOLOR] = no_terr;
	return PO_NONE;
}

/*
 * do a playout from the current position
 * return TRUE if the color opposite to move wins
 * for scoring, the playout must take dead stones off the board and score chinese style.  That means passes only when no legal moves are available.
  * japanese rules give a point for passing
 */
int po_playout(struct po_board *b, int rules, int handicap)
{
	int m, komi;
	int mercy = (b->boardsize * b->boardsize) / 3;
	int c = b->tm;
	int maxmove = b->boardsize * b->boardsize * 2;
	int score;	/* score better for side to move */

	komi = b->komi;
	if (handicap && rules != CHINESE)
		handicap--;		/* AGA and Japense have one less jhandicap compensation poitn since white moves first */
	if (c == PO_BLACK) {
		komi = -komi - 1;
		handicap = -handicap;
	}

	while ((m = po_genmove(b)) != PO_NONE) {
		po_make_move(b, m, b->tm, TRUE);

		if (b->mnum >= maxmove) {
			if (rules == JAPANESE)
				score = komi + b->stones[c] + b->terr[c] - b->stones[1 - c] - b->terr[1 - c] - b->passcount[c] + b->passcount[1 - c];
			else
				score =  komi + b->stones[c] + b->terr[c] - b->stones[1 - c] - b->terr[1 - c];
			if (rules == JAPANESE || rules == CHINESE || rules == AMERICAN)
				score += handicap;
			return score < 0;
		}

#if PO_MERCY
		/* Mercy rule. */
		if (rules == JAPANESE) {
			if (b->stones[c] - b->passcount[c] > b->stones[1 - c] - b->passcount[1 - c] + mercy) {
				return 0;
			}
			if (b->stones[1 - c] - b->passcount[1 - c]  > b->stones[c] - b->passcount[c] + mercy) {
				return 1;
			}
		} else {
			if (b->stones[c] > b->stones[1 - c] + mercy) {
				return 0;
			}
			if (b->stones[1 - c] > b->stones[c] + mercy) {
				return 1;
			}
		}
#endif
	}
	assert(b->stones[0] + b->stones[1] + b->terr[0] + b->terr[1] + b->terr[2] == (b->boardsize ) * (b->boardsize));
	score = komi + b->stones[c] + b->terr[c] - b->stones[1 - c] - b->terr[1 - c];
	if (rules == JAPANESE)
		score += b->passcount[1 - c] - b->passcount[c];
	if (handicap && rules != GOE) {
		score += handicap;
	}
	return score < 0;
}

/*
 * update the rave counters at node, from the complete playout in b (b is the position at the end of the game)
 */
void po_update_rave(struct po_board *b, struct uct_parent *node, int the_winner)
{
	int c = node->tm;
	int i;
	char first;
	struct uct_child *child = node->child;
	unsigned short *wins = node->rave_wins, *plays = node->rave_plays;
#if UCT_RAVE_ALL
	int winner;
#endif

	for (i = 0; i < node->num_children; ++i) {
		first = b->p_first[node->move[i]];
#if UCT_RAVE_ALL
		if (first <= PO_WHITE) {
			winner = first == c && the_winner || first == 1 - c && !the_winner;
			*wins += winner;
			*plays += 1;
			child->rave_win_rate = (float)(*wins)/(float)(*plays);
#if UCT_RAVE_NEW_BETA
			node->child[i].rave_beta = (float)node->child[i].rave_plays / (node->child[i].rave_plays + node->child[i].visits + node->child[i].rave_plays * node->child[i].visits * UCT_RAVE_NEW_BETA_K);
#endif
#if UCT_RAVE_PAT_MATCH
			if (b->p_first_pat[node->child[i].move] == pat) {
				node->child[i].rave_plays++;
				node->child[i].rave_win_rate += (wins - node->child[i].rave_win_rate)/node->child[i].rave_plays;
			}
#endif
		}
#else
		if (first == c) {
#if UCT_RAVE_PAT_MATCH
			if (b->p_first_pat[node->child[i].move] == pat) {
				node->child[i].rave_plays++;
				node->child[i].rave_win_rate += (winner - node->child[i].rave_win_rate)/node->child[i].rave_plays;
			}
#else
			node->child[i].rave_plays++;
			node->child[i].rave_win_rate += (winner - node->child[i].rave_win_rate)/node->child[i].rave_plays;
#endif
		}
#endif
		wins++;
		plays++;
		child++;
	}

	for (i = node->num_children; i < node->num_moves; ++i) {
		first = b->p_first[node->move[i]];
		if (first <= PO_WHITE) {
			winner = first == c && the_winner || first == 1 - c && !the_winner;
			*wins += winner;
			*plays += 1;
		}
		wins++;
		plays++;
	}
}
