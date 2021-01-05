/* Copyright 2006-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#include "g2hd.h"
#include "learn.h"
#include "g2rldef.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern __int64 zobhash64[1000][2];
extern int zobhash[361][2];
 

/*
 * fuseki, joseki, and other learning.
 *
 * replaces the older g2fuslrn.c
 *
 * all games are stored in a single file, gamedb.dat, with compressed move data
 * this file starts with a fixed size array of game hash keys for detecting duplicate games
 * game data in the file is encrypted, using the zobhash random numbers.
 *
 * learned patterns are stored in learn.dat.  It contains a file header, hash table, pattern nodes, and moves 
 *
 */

/*
 * hash table, indexed by the canonical position hash
 * contains list of hash nodes
 */

#define LEARN_VERSION 1

/* 
 * how many pattern sizes
 * 19x19, 11x11, 11x19, 6x11
 */
#define LEARN_NUM_PATS 4

/*
 * bits used to turn a hash into an index
 * 256K entry hash table for position patterns
 */
#define LEARN_HT_BITS 18
#define LEARN_HT_SIZE (1 << LEARN_HT_BITS)
#define LEARN_HT_MASK (LEARN_HT_SIZE - 1)

/*
 * 256K max games
 */
#define LEARN_DUP_BITS 18
#define LEARN_DUP_SIZE (1<<LEARN_DUP_BITS)
#define LEARN_DUP_MASK (LEARN_DUP_SIZE-1)

/* default capacity */
#define LEARN_GAMES 40000

/* nodes in the hash data structure - any position in omre than one game */
#define LEARN_HT_NODES (LEARN_GAMES * LEARN_NUM_PATS * 10)

#define LEARN_HT_MOVES (LEARN_GAMES * 4)

#define LEARN_HT_END 0

#define LEARN_DUP_EMPTY 0x7fffffff

// maximum 16M learned patterns
#define LEARN_HT_NEXT_BITS 24
#define LEARN_HT_NEXT_MASK (((__int64)1 << LEARN_HT_NEXT_BITS) - 1)
#define LEARN_HT_KEY_MASK (~LEARN_HT_NEXT_MASK) 

/* 64 bit zobrist hash values for this position, 
 * for fuseki/joseki learning, repetition check, and search transposition table
 * indexed by msptr value after the move. entry 0 is hash value for the empty board.
 * the initial empty board value is the same for each rotation, and depends on the board size.
 * this prevents false fuseki matches between different board sizes.
 *
 * for fuseki, 8 values are used (4 rotations and 1 relection).  The index value is the 
 * minimum of the 8 values.  Positions match with color, there is no color-reversed match.
 * fuseki moves are stored with their color.  The color to move is not hashed, since it is
 * included in the move list.
 *
 * for joseki, 4 values are used (reflection and color reversal).  The index value is the minimum
 * of the 4 values.  stored move color indicates if the move goes with no color reverse, 
 * or color reverse.  The color to move is not hashed, since it is irrelevant.
 *
 * for adding game repetition check, the first many moves are hashed together, making a single
 * value.  There is no check for rotation or reflection.
 *
 * for the search, there is no rotation or reflection, but the color to move is included
 *
 * it is also used to detect repetition for super ko
 */
unsigned __int64 hashval[NUMMOVES][HASH_ARRAY_SIZE];

/*
 * list of moves attached to a pattern
 */

struct learn_move {
	unsigned int next;					/* index of next move in the list - 0 for end of list */
	unsigned int count:22;				/* number of times this move was seen */
	unsigned int color:1;
	unsigned int move:9;				/* orientation is to position with min hash number zero */
};

struct file_hdr {
	int version;
	unsigned int ht_size;				/* size of the hash table */
	unsigned int next_game;				/* number of games in the gamedb.dat file */
	unsigned int next_node;				/* next available learn_node */
	unsigned int next_move;				/* next available learn_move */
};

/* game hash value, LEARN_DUP_EMPTY for empty.  1 entry per game.  No list through games to avoid disk reads */
static unsigned int dups[LEARN_DUP_SIZE];

static unsigned int hash_table[LEARN_HT_SIZE];		/* heads of lists of hash_nodes, one node per position */
static struct learn_node *ht_nodes;					/* learned positions - all visitd more than once and one layer of leafs */
static struct learn_move *ht_moves;					/* moves linked from positions */

static bool_t hash_valid = FALSE;	/* have a valid database, so ok to save it */

unsigned int learn_numnodes;				/* total learn_nodes - one per duplicate position seen */
unsigned int learn_nummoves;				/* total learn_moves - list of moves attached to eac hash node */

struct file_hdr fh;				/* sizes of elements of the database */

#define MAX_FILENAME 1024
static char hash_filename[MAX_FILENAME];	/* holds file header, hash_table, nodes, and moves */
static char game_filename[MAX_FILENAME];	/* holds dups and games */
static char log_filename[MAX_FILENAME];		/* log of games added */

static void learn_log(char *msg)
{
	FILE *f = fopen(log_filename, "a");
	if (f == NULL)
		return;
	fputs(msg, f);
	fclose(f);
}

/* init for new game 
 * need to init different for each board size to avoid collisions on empty board, on first few moves.
 * init each dir the same for rotation invariance
 */
void learn_init_game(int boardsize)
{
	int i;
	for (i = 0; i < HASH_ARRAY_SIZE; ++i) {
		hashval[0][i] = boardsize * 123456789;
	}
}


static bool_t hash_alloc(void) {
	learn_numnodes = fh.next_node + 2000;
	learn_nummoves = fh.next_move + 2000;
	if (!(ht_nodes = (struct learn_node *)malloc(learn_numnodes * sizeof(struct learn_node)))) {
		return FALSE;
	}
	if (!(ht_moves = (struct learn_move *)malloc(learn_nummoves * sizeof(struct learn_move)))) {
		free(ht_nodes);
		return FALSE;
	}
	return TRUE;
}

void learn_get_stats(struct learn_stats *ls)
{
	unsigned int i;
	ls->mem_used = 
		(fh.next_node + 2000) * sizeof(struct learn_node) +
		(fh.next_move + 2000) * sizeof(struct learn_move);
	ls->mem_used += sizeof(hashval);
	ls->mem_used += sizeof(dups);
	ls->mem_used += sizeof(hash_table);
	ls->dupsize = LEARN_DUP_SIZE;
	ls->dupused = 0;
	for (i = 0; i < LEARN_DUP_SIZE; ++i) {
		if (dups[i] != LEARN_DUP_EMPTY)
			ls->dupused++;
	}
	for (i = 0; i < LEARN_NUM_TYPES; ++i) {
		ls->types[i] = 0;
	}
	ls->games = fh.next_game;
	ls->moves = fh.next_move;
	ls->nodes = fh.next_node;
	ls->multinodes = 0;
	for (i = 1; i < fh.next_node; ++i) {
		if (ht_nodes[i].wins[0] + ht_nodes[i].wins[1] > 1)
			ls->multinodes++;
		ls->types[ht_nodes[i].type]++;
	}
}

void outlearnstats()
{
	struct learn_stats ls;
	char buf[200];
	learn_get_stats(&ls);
	sprintf(buf, "Games %d.  Duplicates used %d of %d.  Memory used %d bytes.\r\n", ls.games, ls.dupused, ls.dupsize, ls.mem_used);
	outerr(buf);
	sprintf(buf, "Patterns: %d multiple games, %d total.  Moves %d\r\n", ls.multinodes, ls.nodes, ls.moves);
	outerr(buf);
	sprintf(buf, "Fuseki %d, Joseki %d, half board %d\r\n", ls.types[LEARN_FUS], ls.types[LEARN_JOS], ls.types[LEARN_HB]);
	outerr(buf);
}

/*
 * read the files
 * gamebd.dat contains the duplicate hash array and the games
 * learn.dat has file header, then pattern data and move data
 */

/* TODO: save to a backup after successful load */


bool_t learn_init(char *cwd) {
#ifdef G2DEBUGOUTPUT
	int i;
#endif
	FILE *f;
	strcpy(log_filename, cwd);
	strcat(log_filename, "learnlog.txt");
	
	strcpy(game_filename, cwd);
	strcat(game_filename, "gamedb.dat");

	f = fopen(game_filename, "rb");
	if (f == NULL) {
#ifdef G2DEBUGOUTPUT
		for (i = 0; i < LEARN_DUP_SIZE; ++i) {
			dups[i] = LEARN_DUP_EMPTY;
		}
		f = fopen(game_filename, "wb");
		if (f != NULL) {
			fwrite(dups, sizeof(int), LEARN_DUP_SIZE, f);
			fclose(f);
		}
#else
		outerr("Can't open gamedb.dat file\n");
		return FALSE;
#endif
	} else {
		fread(dups, sizeof(int), LEARN_DUP_SIZE, f);
		fclose(f);
	}
	
	strcpy(hash_filename, cwd);
	strcat(hash_filename, "learn.dat");
	f = fopen(hash_filename, "rb");
	if (f == NULL) {	/* no file, make empty database */
#ifdef G2DEBUGOUTPUT
		fh.ht_size = LEARN_HT_SIZE;
		fh.next_node = 1;  /* zero means end of list */
		fh.next_move = 1;
		fh.next_game = 0;	/* starts at zero since it's a file offset */
		if (!hash_alloc()) {
			return FALSE;
		}

		// empty hash table - all searches start with a linked list on hash_table
		for (i = 0; i < LEARN_HT_SIZE; ++i) 
			hash_table[i] = LEARN_HT_END;

		hash_valid = TRUE;
		return TRUE;
#else
		outerr("Can't open learn.dat file\n");
		return FALSE;
#endif
	}
	if (fread(&fh, sizeof(struct file_hdr), 1, f) != 1 || fh.ht_size != LEARN_HT_SIZE) {
		fclose(f);
		return FALSE;
	}

	if (!hash_alloc()) {
		fclose(f);
		return FALSE;
	}

	fread(hash_table, sizeof(int), LEARN_HT_SIZE, f);
	fread(ht_nodes, sizeof(struct learn_node), fh.next_node, f);
	fread(ht_moves, sizeof(struct learn_move), fh.next_move, f);

	fclose(f);
	hash_valid = TRUE;
	return TRUE;
}

/*
 * save duplicate hash table in fromt of gamedb.dat.
 * save learn.dat
 */

bool_t learn_save(void) {
	FILE *f;
	if (!hash_valid)
		return FALSE;
	f = fopen(hash_filename, "wb");
	if (f == NULL) {
		outerr("Can't open learn.dat");
		return FALSE;
	}
	fwrite(&fh, sizeof(struct file_hdr), 1, f);
	fwrite(hash_table, sizeof(int), LEARN_HT_SIZE, f);
	fwrite(ht_nodes, sizeof(struct learn_node), fh.next_node, f);
	fwrite(ht_moves, sizeof(struct learn_move), fh.next_move, f);

	fclose(f);

	f = fopen(game_filename, "r+b");
	if (f == NULL) {
		outerr("Can't open gamedb.dat");
		return FALSE;
	}
	fwrite(dups, sizeof(int), LEARN_DUP_SIZE, f);	/* write dups in front of existing file */
	fclose(f);
	
	return TRUE;
}

void learn_free(void) {
	if (!hash_valid)
		return;
	free(ht_nodes);
	free(ht_moves);
}

extern char xval[], yval[];

/* 
 * return the point modified for a symmetry 
 * bits 4 and 8 change the key corner, and bits 1 and 2 color reverse and flip x:y for joseki symmetry
 */ 
static sqr_t hash_rot(sqr_t s, int h, int boardsize, int *color) {
	int x = xval[s];
	int y = yval[s];
	int tmp;
	if (h & 8) {
		 	y = boardsize-1-y;
	}
	if (h & 4) {
		x = boardsize-1-x;
	}
	if (h & 2) {
		tmp = x;
		x = y;
		y = tmp;
	}
	if (h & 1) {
		*color = 1 - *color;
	}
	return y * boardsize + x;
}

/* return the point modified for a symmetry */ 
static sqr_t hash_unrot(sqr_t s, int h, int boardsize, int *color) {
	int x = xval[s];
	int y = yval[s];
	int tmp;
	if (h & 1) {
		*color = 1 - *color;
	}
	if (h & 2) {
		tmp = x;
		x = y;
		y = tmp;
	}
	if (h & 4) {
		x = boardsize-1-x;
	}
	if (h & 8) {
		y = boardsize-1-y;
	}
	return y*boardsize+x;
}

/*
 * make a move, updating hash from previous position.
 * must also call learn_hash_stone for each stone added or removed, including the stone played
 */
void learn_hash_move(int upptr) {
	int h;
	for (h = 0; h < HASH_ARRAY_SIZE; ++h)
		hashval[upptr][h] = hashval[upptr-1][h];
	return;
}

/*
 * get the current hash array
 */
void learn_get_hash(int upptr, unsigned __int64 *hash)
{
	int h;
	for (h = 0; h < HASH_ARRAY_SIZE; ++h)
		hash[h] = hashval[upptr][h];
	return;
}

/*
 * put the current hash array
 */
void learn_put_hash(int upptr, unsigned __int64 *hash)
{
	int h;
	for (h = 0; h < HASH_ARRAY_SIZE; ++h)
		hashval[upptr][h] = hash[h];
	return;
}

/*
 * update hash values when add or remove a stone to this position.  Do not call if s == PASS!
 */
void learn_hash_stone(sqr_t s, int c, int boardsize, int upptr) {
	int hsym, h;
	int x, y, ctmp;
#ifdef G2DEBUGOUTPUT
	if (s == PASS) {
		outerror("learn_hash_stone for PASS!\n");
	}
#endif
	/* 16 full board hashes */
	for (h = 0; h < 16; ++h) {
		ctmp = c;
		hsym = hash_rot(s, h, boardsize, &ctmp);
		hashval[upptr][h] ^= zobhash64[hsym][ctmp];
	}

	/* 
	 * 4 halfboard hashes for each edge, 4 joseki hashes for each 11x11 corner,  2 1/8 hashes for each half corner 
	 * include moves out to line 12 in joseki and halfboard patterns
	 */
	if (boardsize == 19) {
		for (h = 16; h < 64; ++h) {
			ctmp = c;
			hsym = hash_rot(s, h, boardsize, &ctmp);
			x = xval[hsym];
			y = yval[hsym];
			if (y > 11 || x > 11 && h >= 32 || y > 6 && h >= 48) {
				continue;
			}
			hashval[upptr][h] ^= zobhash64[hsym][ctmp];
		}
	}
}

/* is move s, color c a valid fuseki move in this position? */
int learn_is_fuseki(sqr_t s, int c)
{
	struct learn_mv lm[361];
	int strongest, wins[3], game;
	int count = getfuseki(c, lm, &strongest, wins, &game);
	int i;
	for (i = 0; i < count; ++i) {
		if (lm[i].s == s) {
			return TRUE;
		}
	}
	return FALSE;
}

/* get the current game, return FALSE if no game at this position */
int learn_get_game(int color, struct learn_game *lg)
{
	int wins[3], strongest;
	struct learn_mv lm[361];
	int game;
	int count = getfuseki(color, lm, &strongest, wins, &game);
	if (count == 0) {
		return FALSE;
	}
	getlearngame(game, lg);
	return TRUE;
}

void learn_lookup(unsigned __int64 *hash, struct learn_node **fuseki, struct learn_node *joseki[4], struct learn_node *side[4]) {
	unsigned __int64 min = 0xffffffffffffffff;
	unsigned int i, index;
	*fuseki = NULL;
	for (i = 0; i < 4; ++i) {
		joseki[i] = NULL;
		side[i] = NULL;
	}
	if (!hash_valid)
		return;
	for (i = 0; i < 8; ++i) {
		if (hash[i] < min)
			min = hash[i];
	}
	index = hash_table[min & LEARN_HT_MASK];
	while (index != LEARN_HT_END) {
		if ((ht_nodes[index].key_next & LEARN_HT_KEY_MASK) == (min & LEARN_HT_KEY_MASK)) {
			*fuseki = &ht_nodes[index];
			break;
		}
		index = (unsigned int)(ht_nodes[index].key_next & LEARN_HT_NEXT_MASK);
	}
}

static gomove_t learn_get_move(struct learn_game_file *gm, int m)
{
	int i = m/3;
	return (gm->move[i] >> ((m%3)*10)) & 1023;
}

/*
 * encrypt or decrypt a buffer (size in bytes)
 */
static void learn_crypt(unsigned int *ptr, size_t size)
{
	unsigned int i;
	for (i = 0; i < size/4; ++i) {
		*ptr++ ^= zobhash[i % 361][0];
	}
}

/* 
 * convert a game to file format
 */
static void game_to_file(struct learn_game *game, struct learn_game_file *gm) {
	int i, mask, move;
	int m;
	strcpy(gm->event, game->event);
	strcpy(gm->filename, game->filename);
	strcpy(gm->names[0], game->names[0]);
	strcpy(gm->names[1], game->names[1]);

	gm->result = game->result;
	gm->strength[0] = game->strength[0];
	gm->strength[1] = game->strength[1];
	gm->boardsize = game->boardsize;
	gm->year = game->year;
	gm->month = game->month;
	gm->day = game->day;
	gm->move_count = game->move_count;
	for (m = 0; m < game->move_count && m < LEARN_MAX_MOVES; ++m) {
		i = m/3;
		mask = 1023 << ((m%3)*10);
		move = game->move[m];
		if (game->color[m] == WHITECOLOR) {
			move += 512;
		}
		move = move << ((m%3)*10);
		gm->move[i] = (gm->move[i] & ~mask) | move;
	}	
}

/* 
 * convert a game from file format
 */
static void game_from_file(struct learn_game_file *game, struct learn_game *gm) {
	int m;
	strcpy(gm->event, game->event);
	strcpy(gm->filename, game->filename);
	strcpy(gm->names[0], game->names[0]);
	strcpy(gm->names[1], game->names[1]);

	gm->result = game->result;
	gm->strength[0] = game->strength[0];
	gm->strength[1] = game->strength[1];
	gm->boardsize = game->boardsize;
	gm->year = game->year;
	gm->month = game->month;
	gm->day = game->day;
	gm->move_count = game->move_count;
	for (m = 0; m < game->move_count && m < LEARN_MAX_MOVES; ++m) {
		gm->move[m] = learn_get_move(game, m);
	}	
}

/* read game from file, with decryption */
static bool_t learn_read_game(int index, struct learn_game_file *gm)
{
	FILE *f = fopen(game_filename, "rb");
	if (f != NULL) {
		fseek(f, LEARN_DUP_SIZE * sizeof(int) + index * sizeof(*gm), SEEK_SET);
		fread(gm, sizeof(struct learn_game_file), 1, f);
		learn_crypt((unsigned int *)gm, sizeof(*gm));
		fclose(f);
		return TRUE;
	}
	return FALSE;
}

/*
 * convert game to file format, encrypt, and save it
 */
static void learn_save_game(struct learn_game *game)
{
	struct learn_game_file gm;
	FILE *f = fopen(game_filename, "ab");
	if (f != NULL) {
		game_to_file(game, &gm);
		learn_crypt((unsigned int *)&gm, sizeof(gm));
		fwrite(&gm, sizeof(gm), 1, f);
		fclose(f);
		fh.next_game++;
	}
}

#define NUMLEARNGROUPS 9

static int gvals[NUMLEARNGROUPS][8] = 
{
	{ 0, 2, 4, 6, 8, 10, 12, 14 },

	{ 16, 17, 20, 21 },
	{ 18, 19, 26, 27 },
	{ 22, 23, 30, 31 },
	{ 24, 25, 28, 29 },

	{ 32, 33, 34, 35 },
	{ 36, 37, 38, 39 },
	{ 40, 41, 42, 43 },
	{ 44, 45, 46, 47 },
};

static unsigned int gcount[NUMLEARNGROUPS] = 
{
	8, 4, 4, 4, 4, 4, 4, 4, 4
};

static char *groupnames[NUMLEARNGROUPS] = {
	"full board",
	"upper half board",
	"left half board",
	"right half board",
	"lower half board",
	"upper left corner",
	"upper right corner",
	"lower left corner",
	"lower right corner",
};

/*
 * add a node to the database for a particular type of entry, for move m
 * return TRUE if this full board position has been seen before, or a new non-pass joseki
 * if enable, allow a new move to be added
 */
static int add_learn_node(int game, int m, int wins, int strength, int opp_strength, int group, int type, int enable) 
{
	unsigned __int64 min = 0xffffffffffffffff;
	unsigned int i, j, index, minind = 0, mp;
	unsigned short mv, mvtmp;
	int c = mvcolor[m];		// will be rotated to the correct color
	int ctmp;

	/*
	 * find the canonical hash - the lowest value.  There is only one min value, but there might be
	 * several indexes with the same value in a symetrical position.
	 */
	for (i = 0; i < gcount[group]; i++) {
		int index = gvals[group][i];
		if (hashval[m][index] < min) {
			min = hashval[m][index];
			minind = index;
		}
	}
	mv = hash_rot(mvs[m], minind, boardsize, &c);

	if (minind >= 16 && minind < 32 && yval[mv] > 7 ||	/* half board only out through line 8 */ 
		minind >= 32 && (xval[mv] > 10 || yval[mv] > 10) || 
		minind >= 48 && yval[mv] > 6) {
		mv = PASS;	/* this move is a tenuki */
	}
	index = (unsigned int)(min & LEARN_HT_MASK);	/* index into the hash table */

	/* seen this position already? */
	for (i = hash_table[index]; i != LEARN_HT_END; i = (unsigned int)(ht_nodes[i].key_next & LEARN_HT_NEXT_MASK)) {
		if ((ht_nodes[i].key_next & LEARN_HT_KEY_MASK) == (min & LEARN_HT_KEY_MASK) &&
			ht_nodes[i].type == type) {

			if (strength > ht_nodes[i].strongest || strength == ht_nodes[i].strongest && opp_strength == 19) {	/* if same strength, choose 9 dan opponent */
				ht_nodes[i].strongest = strength;
				ht_nodes[i].orient = minind;  
				ht_nodes[i].next_move = m;
				ht_nodes[i].game = game;  
			}
			if (wins < 3 && ht_nodes[i].wins[wins] < 65500) {
				ht_nodes[i].wins[wins]++;
			}
			if (mv == PASS)  {
				ht_nodes[i].tenuki[c]++;
				return FALSE;
			}

			/* add the move to the move list, checking for duplicates */
			for (mp = ht_nodes[i].moves; mp != LEARN_HT_END; mp = ht_moves[mp].next) {
				if (ht_moves[mp].color != c) {
					continue;
				}
				for (j = 0; j < gcount[group]; j++) {
					int index = gvals[group][j];
					if (hashval[m][index] != min) {
						continue;
					}
					mvtmp = hash_rot(mvs[m], index, boardsize, &ctmp);
					if (ht_moves[mp].move == mvtmp) {	/* already have this move */
						ht_moves[mp].count++;		/* increment count */
						return TRUE;
					}
				}
			}

			/* new move, add it */
			ht_moves[fh.next_move].next = ht_nodes[i].moves;
			ht_moves[fh.next_move].move = mv;
			ht_moves[fh.next_move].color = c;
			ht_moves[fh.next_move].count = 1;
			ht_nodes[i].moves = fh.next_move;
			fh.next_move++;
			return  TRUE;
		}
	}

	/* new position */
	ht_nodes[fh.next_node].key_next = (min & LEARN_HT_KEY_MASK) | (hash_table[index] & LEARN_HT_NEXT_MASK);
	hash_table[index] = fh.next_node;

	/* fill in the node values */
	ht_nodes[fh.next_node].orient = minind;  
	ht_nodes[fh.next_node].next_move = m;
	ht_nodes[fh.next_node].game = game;  
	ht_nodes[fh.next_node].strongest = strength;
	ht_nodes[fh.next_node].type = type;
	if (wins < 2) {
		ht_nodes[fh.next_node].wins[wins] = 1;
		ht_nodes[fh.next_node].wins[1-wins] = 0;
		ht_nodes[fh.next_node].wins[2] = 0;
	} else {
		ht_nodes[fh.next_node].wins[0] = 0;
		ht_nodes[fh.next_node].wins[1] = 0;
		ht_nodes[fh.next_node].wins[2] = 1;
	}

	if (mv == PASS) {
		ht_nodes[fh.next_node].tenuki[c] = 1;
		ht_nodes[fh.next_node].tenuki[1-c] = 0;
		ht_nodes[fh.next_node].moves = 0;
	} else {
		ht_nodes[fh.next_node].tenuki[0] = 0;
		ht_nodes[fh.next_node].tenuki[1] = 0;

		/* add the move to the empty move list */
		ht_moves[fh.next_move].next = LEARN_HT_END;
		ht_moves[fh.next_move].move = mv;
		ht_moves[fh.next_move].color = c;
		ht_moves[fh.next_move].count = 1;
		ht_nodes[fh.next_node].moves = fh.next_move;
		fh.next_move++;
	}
	fh.next_node++;
	return mv != PASS && group != 0;
}

/*
 * add a game node for msptr m in the current game
 *     return TRUE if this fuseki was seen before (and add one new fuseki move), 
 *		or a joseki move in the same corner
 *		(we should add another move)
 *		wins is the color of the winner (or 2 or 3 for tie or unknown)
 *		strength is the strongest player moving to this position
 */
static int add_game_node(int game, int m, int wins, int strength, int opp_strength, int lastjos, int *thisjos) 
{
	int ret = FALSE;
	int i;
	*thisjos = 0;
	ret = add_learn_node(game, m, wins, strength, opp_strength, 0, LEARN_FUS, TRUE);

	/* small boards only get fuseki added */
	if (boardsize != 19) {
		return ret;
	}

	/*
	 * only add halfboard positions when a full board position is seen more than once
	 */
	if (ret) {
		for (i = 1; i < 5; ++i) {
			add_learn_node(game, m, wins, strength, opp_strength, i, LEARN_HB, TRUE);
		}
	}

	/*
	 * for joseki, return true if we saw this move before or it's a new move in the same corner
	 */
	for (i = 5; i < 9; ++i) {
		if (add_learn_node(game, m, wins, strength, opp_strength, i, LEARN_JOS, ret || i == lastjos)) {
			if (lastjos == i) {
				ret = TRUE;
			}
			*thisjos = i;
		}
	}
	return ret;
}

/* return array of up to num fuseki moves by color in moves, and number of moves */
int getfuseki(int color, struct learn_mv *moves, int *strongest, int *wins, int *game)
{
	unsigned __int64 min = 0xffffffffffffffff;
	unsigned int i, j, minind = 0, index, mp;
	sqr_t s, lasts = PASS;
	int n = 0;
	int c;
	int num = 361;

	/* get the hash index (minimuim of symetric values */
	for (i = 0; i < 16; i += 2) {
		if (hashval[msptr][i] < min) {
			min = hashval[msptr][i];
			minind = i;
		}
	}
	index = (unsigned int)(min & LEARN_HT_MASK);	// index into the hash table

	/* seen this position? */
	*strongest = -30;
	for (i = hash_table[index]; i != LEARN_HT_END; i = (unsigned int)(ht_nodes[i].key_next & LEARN_HT_NEXT_MASK)) {
		if ((ht_nodes[i].key_next & LEARN_HT_KEY_MASK) == (min & LEARN_HT_KEY_MASK)) {
			if (ht_nodes[i].type != LEARN_FUS)
				continue;
			/* should find this only once */
			*strongest = ht_nodes[i].strongest;
			wins[0] = ht_nodes[i].wins[0];
			wins[1] = ht_nodes[i].wins[1];
			wins[2] = ht_nodes[i].wins[2];
			*game = ht_nodes[i].game;
			for (mp = ht_nodes[i].moves; mp != LEARN_HT_END; mp = ht_moves[mp].next) {
				for (j = 0; j < 16; j += 2) {
					if (hashval[msptr][j] != min) {
						continue;
					}
					c = color;
					s = hash_unrot((sqr_t)(ht_moves[mp].move), j, boardsize, &c);
					if (ht_moves[mp].color != c) {
						continue;
					}
					if (s == lasts) {	/* symetry produces identical move */
						continue;
					}
					moves->s = s;
					moves->count = ht_moves[mp].count;
					moves++;
					n++;
					if (n >= num) {
						return n;
					}
					lasts = s;
				}
			}
		}
	}
	return n;
}

char *getstr(int str, char *buf)
{
	if (str >= 11)
		sprintf(buf, "%d Dan Pro", str-10);
	else if (str > 0)
		sprintf(buf, "%d Dan Amateur", str);
	else if (str != -100)
		sprintf(buf, "%d Kyu", -str);
	else 
		sprintf(buf, "Unknown strength");
	return buf;
}

int getlearngame(int game, struct learn_game *lg)
{
	struct learn_game_file gm;
	if (!learn_read_game(game, &gm)) {
		return FALSE;
	}
	game_from_file(&gm, lg);
	return TRUE;
}

static void getgamejoscorner(int color, int group, char marks[363], char mk, int type)
{
	unsigned __int64 min = 0xffffffffffffffff, tmp;
	unsigned int i, j, minind = 0, index, mp;
	char buf[200], stbuf[50];
	struct learn_game_file gm;
	int c, ctm = color;
	unsigned int max;
	sqr_t s;
	int totmoves;

	for (i = 0; i < gcount[group]; i ++) {
		int ind = gvals[group][i];
		if (hashval[msptr][ind] < min) {
			min = hashval[msptr][ind];
			minind = ind;
		}
	}
	if (minind & 1)
		ctm = 1-ctm;
	index = (unsigned int)(min & LEARN_HT_MASK);	/* index into the hash table */

	/* seen this position? */
	for (i = hash_table[index]; i != LEARN_HT_END; i = (unsigned int)(ht_nodes[i].key_next & LEARN_HT_NEXT_MASK)) {
		tmp = ht_nodes[i].key_next & LEARN_HT_KEY_MASK;
		if (tmp == (min & LEARN_HT_KEY_MASK)) {
			if (ht_nodes[i].type != type)
				continue;
			if (ht_nodes[i].wins[0] + ht_nodes[i].wins[1] + ht_nodes[i].wins[2] == 1) {
				learn_read_game(ht_nodes[i].game, &gm);  /* add missing move to move list */
				sprintf(buf, "%s:\r\nBlack: %s, %s\r\n", groupnames[group], gm.names[0], getstr(gm.strength[0], stbuf));
				outerr(buf);
				sprintf(buf, "White: %s, %s\r\n", gm.names[1], getstr(gm.strength[1], stbuf));
				outerr(buf);
				sprintf(buf, "Event: %s: %d/%d/%d\r\n", gm.event, gm.month, gm.day, gm.year);
				outerr(buf);
				sprintf(buf, "File: %s\r\n", gm.filename);
				outerr(buf);
				if (ht_nodes[i].wins[0])
					sprintf(buf, "Black wins\r\n");
				else
					sprintf(buf, "White wins\r\n");
				outerr(buf);
			} else {
				sprintf(buf, "This %s position seen %d times.  Tenuki %d times, Player to move wins %3.0f%%. Strongest player making last move is %s\r\n", 
					groupnames[group],
					ht_nodes[i].wins[0] + ht_nodes[i].wins[1] + ht_nodes[i].wins[2], 
					ht_nodes[i].tenuki[ctm],
					100.*ht_nodes[i].wins[color]/(ht_nodes[i].wins[0] + ht_nodes[i].wins[1] + ht_nodes[i].wins[2]), 
					getstr(ht_nodes[i].strongest, 
					stbuf)
					);
				outerr(buf);
			}
			max = 0;
			totmoves = 0;
			for (mp = ht_nodes[i].moves; mp != LEARN_HT_END; mp = ht_moves[mp].next) {
				if (ht_moves[mp].color != ctm)
					continue;
				totmoves += ht_moves[mp].count;
				for (j = 0; j < gcount[group]; j ++) {
					int ind = gvals[group][j];
					if (hashval[msptr][ind] != min) {
						continue;
					}
					c = color;
					s = hash_unrot((sqr_t)(ht_moves[mp].move), ind, boardsize, &c);
					if (ht_moves[mp].count > max) { 
						max = ht_moves[mp].count;
					}
				}
			}
			for (mp = ht_nodes[i].moves; mp != LEARN_HT_END; mp = ht_moves[mp].next) {
				if (ht_moves[mp].color != ctm)
					continue;
				for (j = 0; j < gcount[group]; j ++) {
					int ind = gvals[group][j];
					if (hashval[msptr][ind] != min) {
						continue;
					}
					c = color;
					s = hash_unrot((sqr_t)(ht_moves[mp].move), ind, boardsize, &c);
					if (ht_moves[mp].count > max / 10) { 
						marks[s] = mk;
						ssqr(s, stbuf);
						sprintf(buf, "%s: %d of %d.  Urgency %d%%\r\n", 
							stbuf, 
							ht_moves[mp].count, 
							totmoves,
							100 * ht_moves[mp].count / (totmoves + ht_nodes[i].tenuki[ctm]));
						outerr(buf);
					}
				}
			}
		}
	}
}

void getgamejoseki(int color, char marks[363])
{
	int i;
	char *p = getstring(GOPAGE,54);
	for (i = 0; i < 363; ++i)
		marks[i] = 0;

	if (msptr == 0)
		return;

	getgamejoscorner(color, 5, marks, *(p+11), LEARN_JOS);
	getgamejoscorner(color, 6, marks, *(p+11), LEARN_JOS);
	getgamejoscorner(color, 7, marks, *(p+11), LEARN_JOS);
	getgamejoscorner(color, 8, marks, *(p+11), LEARN_JOS);

	getgamejoscorner(color, 1, marks, 'h', LEARN_HB);
	getgamejoscorner(color, 2, marks, 'h', LEARN_HB);
	getgamejoscorner(color, 3, marks, 'h', LEARN_HB);
	getgamejoscorner(color, 4, marks, 'h', LEARN_HB);
}

/*
 * is this game a duplicate?  If not, insert an entry.
 * hash the moves, then lookup many times to see if there is a match
 * 32 bit comparison
 */
static bool_t duplicate(struct learn_game *game)
{
	__int64 dh = 0, hvals[16];
	unsigned int i, j;
	int c;
	int free;

	for (i = 0; i < 16; ++i) {
		hvals[i] = 0;
	}

	/* make the hash, of the first 1/2 board moves (120 on a full board) */
	for (i = 0; i < (unsigned)(boardsize * boardsize / 2) && i < msptr; ++i) {
		for (j = 0; j < 16; ++j) {
			hvals[j] ^= zobhash64[hash_rot(mvs[i], j, game->boardsize, &c)][mvcolor[i]]; 
		}
	}

	/* rotation invariant hash */
	for (i = 0; i < 16; ++i) {
		if (hvals[i] > dh) {
			dh = hvals[i];
		}
	}

	/* check for duplicate */
	free = 0;
	for (i = 0; i < 31 - LEARN_DUP_BITS; ++i) {  /* 12 probes, using all bits */
		unsigned int index = (unsigned int)(dh >> i) & LEARN_DUP_MASK;
		if (dups[index] == dh >> 31) {
			return TRUE;
		}
		if (dups[index] == LEARN_DUP_EMPTY) {
			free = 1;
		}
		if (dups[index ^ 1] == dh >> 31) {
			return TRUE;
		}
		if (dups[index ^ 1] == LEARN_DUP_EMPTY) {
			free = 1;
		}
	}
	if (!free) {
		return TRUE;
	}

	/* find a empty slot for insertion */
	for (i = 0; i < 31 - LEARN_DUP_BITS; ++i) {  /* 12 probes, using all bits */
		unsigned int index = (unsigned int)(dh >> i) & LEARN_DUP_MASK;
		if (dups[index] == LEARN_DUP_EMPTY) {
			dups[index] = (unsigned int)(dh >> 31);
			break;
		}
		if (dups[index ^ 1] == LEARN_DUP_EMPTY) {
			dups[index ^ 1] = (unsigned int)(dh >> 31);
			break;
		}
	}
	return FALSE;
}

/*
 * add game to the game database, using the moves in game
 * if all, add all the moves
 */
bool_t learn_add_game(struct learn_game *game, int all)
{
	int i;
	char buf[500];
	if (!hash_valid)
		return FALSE;
	/*
	 * send all the moves to the go engine
	 */
	initvars(game->boardsize, komi, 64, 5000);
	for (i = 0; i < game->move_count; ++i) {
		if (game->move[i] == NOPOINT) {
			sprintf(buf, "Empty move %d in game %s\n", i, game->filename);
			learn_log(buf);
			outerr(buf);
			return FALSE;
		}
		if (update(game->move[i], game->color[i], FALSE) != TRUE) {
			sprintf(buf, "illegal move %d in game %s\n", i, game->filename);
			learn_log(buf);
			outerr(buf);
			return FALSE;
		}
	}
	learn_add_current_game(game, all);
	return TRUE;
}

/*
 * add game to the game database, using moves that have alerady been played
 * if all, add all the moves
 */
bool_t learn_add_current_game(struct learn_game *game, int all)
{
	int i, start, thisnode = 0, lastnode = 0;
	unsigned int count;		/* number of added nodes */
	struct learn_node *new_node;
	struct learn_move *new_move;
	char buf[500];
	int nodes = fh.next_node;
	int moves = fh.next_move;

	if (!hash_valid)
		return FALSE;

	/* bad strengths */
	if (game->strength[0] > 19 || game->strength[1] > 19 || game->strength[0] == 0 || game->strength[1] == 0) {
		sprintf(buf, "Bad strength %d %d in game %s\n", game->strength[0], game->strength[1], game->filename);
		learn_log(buf);
		outerr(buf);
		return FALSE;
	}

	/* check for duplicates */
	if (duplicate(game)) {
		sprintf(buf, "Ignoring duplicate game %s\n", game->filename);
		learn_log(buf);
		outerr(buf);
		return FALSE;
	}

	if (game->result == 3) {
		sprintf(buf, "Unknown result: %s\n", game->filename);
		learn_log(buf);
		outerr(buf);
	}

	/* add to the game library */
	learn_save_game(game);

	/* add to the hash structure */

	/* increase size if needed */
	if (fh.next_node >= learn_numnodes - 500) {
		learn_numnodes = learn_numnodes + learn_numnodes / 4;
		new_node = (struct learn_node *)realloc(ht_nodes, learn_numnodes * sizeof(struct learn_node));
		if (new_node == NULL) {
			hash_valid = FALSE;
			return FALSE;
		}
		ht_nodes = new_node;
	}
	if (fh.next_move >= learn_nummoves - 2000) {
		learn_nummoves = learn_nummoves + learn_nummoves / 4;
		new_move = (struct learn_move *)realloc(ht_moves, learn_nummoves * sizeof(struct learn_move));
		if (new_move == NULL) {
			free(ht_nodes);
			free(ht_moves);
			hash_valid = FALSE;
			return FALSE;
		}
		ht_moves = new_move;
	}

	/* the nodes.  leave out last position since it has no moves */
	count = 0;
	start = game->handicap;

	/*
	 * always add the first few moves, and add more moves if add_game_node says we should
	 */
	for (i = start; i < (int)msptr - 1; ++i) {
		count++;
		if (!add_game_node(fh.next_game - 1, i, game->result, game->strength[i == 0? WHITECOLOR : mvcolor[i - 1]], game->strength[mvcolor[i]], 
			lastnode, &thisnode) && 
			i > 4 + game->handicap && !all) {
			break;
		}
		lastnode = thisnode;
	}
	
	sprintf(buf, "Added %d positions from %d moves in %s.  %d new patterns, %d new moves.  Total games %d, positions %d, moves %d\n", 
		count, i, game->filename, fh.next_node - nodes, fh.next_move - moves, fh.next_game, fh.next_node, fh.next_move);
	learn_log(buf);
	return TRUE;
}
