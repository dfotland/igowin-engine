
#include "ui2g2.h"
#include "playout.h"
#include <assert.h>

struct po_pat {
	char *p[3];
	char val;
};

/* @ - black stone
 * O - white stone
 * + - empty
 * x - any
 * center point + either color plays here, not +, black plays here only.
 *
 * val 24 is nominal, higher are good, lower are bad
 * during playout, only moves 24 or over will be considered
 * odd means it can match on the edge (and lower edge is not matched)
 */
struct po_pat po_raw_patterns[] = 
{
	/* new patterns */
#if PO_NEW_PATTERNS
	/* these patterns make it stronger */
	{
		"@+@",
		"++O",
		"+++",
		PO_PAT_VAL + 1
	},
	{
		"+O+",
		"++@",
		"+++",
		PO_PAT_VAL + 1
	},
	{
		"O+@",
		"+++",
		"+++",
		PO_PAT_VAL + 1
	},
	{
		"O+@",
		"+++",
		"++O",
		PO_PAT_VAL + 1 + 20
	},
	{
		"+Ox",
		"@b@",
		"@++",
		PO_PAT_VAL
	},
	{
		"xO@",
		"@++",
		"x+x",
		PO_PAT_VAL * 2 + 1
	},
#endif
	/* mogo patterns below here */
	{
		"@O@",
		"+++",
		"xxx",
		PO_PAT_VAL * 2 + 1
	},
	{
		"@O+",
		"+++",
		"x+x",
		PO_PAT_VAL + 1
	},
	{
		"@Ox",
		"@++",
		"x+x",
		PO_PAT_VAL * 2 + 1
	},
	{
		"@OO",
		"+b+",
		"x+x",
		PO_PAT_VAL + 1
	},
	{
		"@Ox",
		"O+O",
		"x@x",
		PO_PAT_VAL + 1
	},
	{
		"@Ox",
		"O+@",
		"x@x",
		PO_PAT_VAL + 1
	},
	{
		"@Ox",
		"O++",
		"x@x",
		PO_PAT_VAL * 2 + 1
	},
	{
		"@Ox",
		"O++",
		"x+x",
		PO_PAT_VAL * 2
	},
	{
		"x@x",
		"O+O",
		"+++",
		PO_PAT_VAL + 1
	},
	{
		"x@x",
		"O+O",
		"@+x",	/* modified - made right + and x */
		PO_PAT_VAL * 2 + 1
	},
	{
		"x@x",
		"O+O",
		"x@x",	/* modified - mad right and left x */
		PO_PAT_VAL * 2 + 1
	},
#if !UCT_TRIP
	{
		"x@x",
		"O+O",
		"@@+",
		PO_PAT_VAL * 2 + 1
	},
	{
		"x@x",
		"O+O",
		"@@@",
		PO_PAT_VAL * 2 + 1
	},
	{
		"x@x",
		"O+O",
		"@+@",
		PO_PAT_VAL * 2 + 1
	},
#endif
};


/* 
 * pattern values, by 3x3 shape and color to move 
 */

unsigned char uct_po_patterns[2][PO_3_TO_8];

/* values to sum to make a pattern index */
int po_pat_index[2][8];

static void po_make_pats(int start, int pat, int rpat, int *vector, int vec, int black, int white, int val)
{
	int i;
	if (vec == 3) {
		po_make_pats(start, pat, rpat, vector, 0, black, white, val);
		po_make_pats(start, pat, rpat, vector, 1, black, white, val);
		po_make_pats(start, pat, rpat, vector, 2, black, white, val);
		return;
	}
	pat *= 3;
	pat += vec;
	rpat *= 3;
	if (vec)
		rpat += 3 - vec;
	for (i = start; i < 8; ++i) {
		if (vector[i] == 3) {
			po_make_pats(i + 1, pat, rpat, vector, 0, black, white, val);
			po_make_pats(i + 1, pat, rpat, vector, 1, black, white, val);
			po_make_pats(i + 1, pat, rpat, vector, 2, black, white, val);
			return;
		}
		pat *= 3;
		pat += vector[i];
		rpat *= 3;
		if (vector[i])
			rpat += 3 - vector[i];
	}
	assert(pat < PO_3_TO_8 && rpat < PO_3_TO_8);
	if (black) {	/* black move legal in pat, or white move in color reversed pat */
		uct_po_patterns[0][pat] = val;
		uct_po_patterns[1][rpat] = val;
	}
	if (white) {  /* white legal in pat, or black in reversed pat */
		uct_po_patterns[1][pat] = val;
		uct_po_patterns[0][rpat] = val;
	}
}

static int po_lookup(char c) 
{
	if (c =='+') return 0;
	if (c =='@') return 1;
	if (c =='O') return 2;
	if (c =='x') return 3;
	assert(0);
	return 0;
}

void po_pattern_init(void)
{
	int i, j, k, val = 1, vector[8], vec2[8], t;
	int numpats = sizeof(po_raw_patterns) / sizeof (struct po_pat);

	for (i = 7; i >= 0; --i) {
		po_pat_index[0][i] = val;
		po_pat_index[1][i] = val * 2;
		val *= 3;
	}

	for (i = 0; i < numpats; ++i) {
		vector[0] = po_lookup(po_raw_patterns[i].p[0][0]); 
		vector[1] = po_lookup(po_raw_patterns[i].p[0][1]); 
		vector[2] = po_lookup(po_raw_patterns[i].p[0][2]); 
		vector[3] = po_lookup(po_raw_patterns[i].p[1][2]); 
		vector[4] = po_lookup(po_raw_patterns[i].p[2][2]); 
		vector[5] = po_lookup(po_raw_patterns[i].p[2][1]); 
		vector[6] = po_lookup(po_raw_patterns[i].p[2][0]); 
		vector[7] = po_lookup(po_raw_patterns[i].p[1][0]);
		po_make_pats(1, 0, 0, vector, vector[0], 1, po_raw_patterns[i].p[1][1] == '+', po_raw_patterns[i].val);
		/* rotate */
		for (j = 2; j <= 8; j += 2) {
			for (k = 0; k < 8; ++k) {
				vec2[k] = vector[(k + j) % 8];
			}
			po_make_pats(1, 0, 0, vec2, vec2[0], 1, po_raw_patterns[i].p[1][1] == '+', po_raw_patterns[i].val);
		}
		/* mirror */
		t = vector[0]; vector[0] = vector[2]; vector[2] = t;
		t = vector[7]; vector[7] = vector[3]; vector[3] = t;
		t = vector[6]; vector[6] = vector[4]; vector[4] = t;
		po_make_pats(1, 0, 0, vector, vector[0], 1, po_raw_patterns[i].p[1][1] == '+', po_raw_patterns[i].val);
		for (j = 2; j <= 8; j += 2) {
			for (k = 0; k < 8; ++k) {
				vec2[k] = vector[(k + j) % 8];
			}
			po_make_pats(1, 0, 0, vec2, vec2[0], 1, po_raw_patterns[i].p[1][1] == '+', po_raw_patterns[i].val);
		}
	}
}
