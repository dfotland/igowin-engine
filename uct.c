
/* 
 * uct.c - uct-mc 
 */
#include <stdlib.h>
#include <assert.h>
#include "ui2g2.h"
#include "g2hd.h"
#include "g2proto.h"
#include "uct.h"
#include "playout.h"
#include "learn.h"
#include "g2rldef.h"
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <string.h>

#ifdef WIN32
/* required for TryEnterCriticalSection */
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x410
#endif
#include <windows.h>
#include <process.h>
#include <winternl.h>
#else
static int GetTickCount(void)
{
	return (int)(1000 * time(NULL));
}
#endif

#if USE_MPI
#include <mpi.h>
#endif

#pragma warning(disable : 4996)

#define UCT_MPI_TAG 12345

/*
 * new data since the last send, about 400 bytes
 */
struct uct_search_state {
	__int64 hash;
	volatile struct uct_search_state *next;
	int used;
	int threads;	/* threads that have seen this data - for MPI, 1 is valid, 0 means last one */
	int num_children;
	int num_moves;
	int tm;
	int ko;
	int mnum;
	unsigned int visits[UCT_MAX_CHILDREN];
	unsigned int wins[UCT_MAX_CHILDREN];
	unsigned short move[UCT_MAX_MOVES];
};

static int uct_max_node = UCT_MAX_NODE;						/* uct search nodes for each thread */
static int uct_max_state_node = UCT_MAX_STATE_NODE;			/* shared state nodes total */

static struct uct_search_state *uct_search_state_nodes;		/* search data to send from one thread or node to another */
static volatile struct uct_search_state *uct_state_free_nodes;					/* free list of nodes per thread */
static volatile int uct_state_free_node_count;

static volatile struct uct_search_state *uct_state_sent[UCT_MAX_THREADS];					/* each thread's sent states to other threads */
static volatile struct uct_search_state *uct_state_last[UCT_MAX_THREADS];					/* each thread's last sent state - to add more */
static volatile struct uct_search_state *uct_state_next[UCT_MAX_THREADS][UCT_MAX_THREADS];	/* each thread's next point to look for input from each other thread */

static struct uct_parent *uct_parent_nodes[UCT_MAX_THREADS];				/* the nodes for the search by thread.  Nodes are not shared */
static struct uct_parent *uct_hash_table[UCT_MAX_THREADS][UCT_HASH_SIZE];	/* heads of lists of nodes per thread. hash tables are not shared */
static struct uct_parent *uct_free_nodes[UCT_MAX_THREADS];					/* free list of nodes per thread.  Nodes are not shared */
static int uct_free_node_count[UCT_MAX_THREADS];
static int uct_alloc_count[UCT_MAX_THREADS];
static int uct_alloc_fails[UCT_MAX_THREADS];
static int uct_alloc_collect[UCT_MAX_THREADS];
static int uct_collectable[UCT_MAX_THREADS];
static int uct_nodes_sent[UCT_MAX_THREADS];
static int uct_nodes_received[UCT_MAX_THREADS];
static int uct_nodes_dropped[UCT_MAX_THREADS];
static int uct_nodes_alloced[UCT_MAX_THREADS];
static int uct_state_min_free;
static int uct_state_fail_count;

static struct uct_parent *uct_shared_nodes[UCT_MAX_THREADS][UCT_MAX_SHARED_NODE];	/* states with mfgo calls, to share */
static int uct_next_shared[UCT_MAX_THREADS];

#if UCT_LEARN
static unsigned __int64 learn_hash[HASH_ARRAY_SIZE];			/* for openings */
#endif

static volatile int uct_mnum;
static sqr_t uct_moves[MAXMOVE];								/* moves made in the game before the search */
static int uct_color[MAXMOVE];

unsigned int uct_time_limit[PO_MAXMOVE];						/* how much time for this move, in millisecodns */
unsigned int uct_time_used[PO_MAXMOVE];							/* how much time used by this move, in millisecodns */
static volatile int uct_playouts[PO_MAXMOVE];					/* total playouts used by this move */
static int uct_thread_playouts[UCT_MAX_THREADS];				/* playouts by thread */
static int uct_komi = 6;
static int uct_size = 19;
static int uct_max_depth_seen;
static volatile int uct_mfgo_calls;
#define UCT_MAX_VISIT 32
static volatile int uct_visited_children;
static volatile int uct_used_children;
static int uct_num_threads = 1;		
static unsigned int uct_max_playouts = UCT_MAX_PLAYOUTS; 
static int uct_partner;					/* offset to who to send to */
static int uct_next_garbage[UCT_MAX_THREADS];

#ifdef WIN32
static volatile HANDLE uct_ponder_complete;		/* pondering thread is done working */
static volatile HANDLE uct_search_complete[UCT_MAX_THREADS];
static CRITICAL_SECTION uct_lock;
static CRITICAL_SECTION uct_mfgo_lock;
static CRITICAL_SECTION uct_state_lock;
#define UCT_LOCK(x) EnterCriticalSection(&x)
#define UCT_UNLOCK(x) LeaveCriticalSection(&x)
#define UCT_TRYLOCK(x) TryEnterCriticalSection(&x)
#else
#define UCT_LOCK(x) 
#define UCT_UNLOCK(x)
#define UCT_TRYLOCK(x) 1
#endif

int uct_mpi_send_count, uct_mpi_recv_count;	/* number of MPI messages sent and received */
int uct_mpi_send_msg_count, uct_mpi_recv_msg_count;
int uct_mpi_fwd_count, uct_mpi_fwd_msg_count;
int uct_mpi_max_message_count;
int uct_mpi_max_recv_count;
int uct_mpi_recv_missing;
#if USE_MPI
int uct_my_rank;	
int uct_num_nodes;	/* number of nodes in the total MPI cluster */
int uct_group_size;	/* number of nodes participating in BSEND group */
int uct_pair;		/* the mask to xor to get my paired node (provided it is lower than uct_num_nodes) */

#if UCT_MPI_ALLREDUCE
MPI_Op uct_mpi_reduce_op;
_stdcall uct_mpi_reduce(struct uct_search_state *in, struct uct_search_state *inout, int *count, MPI_Datatype *dptr);
#define UCT_MPI_BUFFER_COUNT 2048
#else
#define UCT_MPI_BUFFER_COUNT 1024
#endif

struct uct_search_state uct_mpi_msg_buffer[UCT_MPI_BUFFER_COUNT];

#if !UCT_MPI_ALLREDUCE
/* enough space for two pending messages to each target in a 256 node cluster for the BSEND comm code */
#define MPI_MAX_BSEND UCT_MPI_BUFFER_COUNT * 8
#define UCT_MPI_BUFFER_SIZE (MPI_MAX_BSEND  * (sizeof(struct uct_search_state) + MPI_BSEND_OVERHEAD))
static char uct_mpi_buffer[UCT_MPI_BUFFER_SIZE];
static int uct_mpi_attached = FALSE;
#endif

#endif

static float uct_root_bias[UCT_MAX_THREADS][PO_BSIZE];

/*
 * set before start the thread.  clear to cause the thread to stop
 */
static volatile int uct_pondering;
static volatile int uct_searching[UCT_MAX_THREADS];

static void uct_dump_stats(struct uct_parent *uct_root, int time_left, int stones_left, int handicap, int rules, int show_lookahead, int color);

int uct_get_playout_count(void)
{
	if (uct_mnum == 0)
		return 0;
	return uct_playouts[uct_mnum - 1];
}

#define UCT_STATE_LOCK_SPIN_COUNT 4000

/* max MB memory for UCT search (no more than 2 GB) */
static int uct_max_memory = 200;

void uct_set_memory(int mem) 
{
	if (mem > 2048)
		mem = 2048;
	uct_max_memory = mem;
}

int uct_init_all(void)
{
#ifdef WIN32
	int i;
	for (i = 0; i < UCT_MAX_THREADS; ++i) {
		uct_search_complete[i] = CreateEvent(NULL, FALSE, FALSE, NULL);
		if (uct_search_complete[i] == NULL) {
			return FALSE;
		}
	}
	uct_ponder_complete = CreateEvent(NULL, FALSE, FALSE, NULL);
	if (uct_ponder_complete == NULL) {
		return FALSE;
	}
	uct_pondering = FALSE;
	InitializeCriticalSectionAndSpinCount(&uct_lock, UCT_STATE_LOCK_SPIN_COUNT);
	InitializeCriticalSection(&uct_mfgo_lock);
	InitializeCriticalSectionAndSpinCount(&uct_state_lock, UCT_STATE_LOCK_SPIN_COUNT);
#endif

#if USE_MPI && UCT_MPI_ALLREDUCE
	MPI_Op_create(uct_mpi_reduce, TRUE, &uct_mpi_reduce_op); 
#endif
	return TRUE;
}

void uct_free_all(void) {
	int i;
#if USE_MPI && !UCT_MPI_ALLREDUCE
	int size;
	if (uct_mpi_attached) {
		uct_mpi_attached = FALSE;
		MPI_Buffer_detach(uct_mpi_buffer, &size);
	}
#endif
#ifdef WIN32
	if (uct_pondering) {
		uct_pondering = FALSE;
		WaitForSingleObject(uct_ponder_complete, INFINITE);
	}
	for (i = 0; i < UCT_MAX_THREADS; ++i) {
		if (uct_searching[i]) {
			uct_searching[i] = FALSE;
			WaitForSingleObject(uct_search_complete[i], INFINITE);
		}
		CloseHandle(uct_search_complete[i]);
	}
	CloseHandle(uct_ponder_complete);
	DeleteCriticalSection(&uct_lock);
	DeleteCriticalSection(&uct_mfgo_lock);
	DeleteCriticalSection(&uct_state_lock);
#endif
	for (i = 0; i < UCT_MAX_THREADS; ++i) {
		if (uct_parent_nodes[i]) {
			free(uct_parent_nodes[i]);
			uct_parent_nodes[i] = 0;
			uct_free_nodes[i] = 0;
			uct_free_node_count[i] = 0;
		}
	}
}


/* 
 * komi in points, white wins ties.  
 * Init for a new game
 */
void uct_init(int bsize, int max_threads, int max_playouts)
{
	int num_threads = 8;

#ifdef WIN32
	/* max one thread per logical processor */
	SYSTEM_INFO SysInfo;
	GetSystemInfo(&SysInfo);
	num_threads = SysInfo.dwNumberOfProcessors;
	assert(num_threads > 0);

	/* stop pondering */
	if (uct_pondering) {
		uct_pondering = FALSE;
		WaitForSingleObject(uct_ponder_complete, INFINITE);
	}
#else
	num_threads = 1;
#endif
	if (num_threads > max_threads) {
		num_threads = max_threads;
	}
	if (num_threads < 1) {
		num_threads = 1;
	}
	if (num_threads > UCT_MAX_THREADS) {
		num_threads = UCT_MAX_THREADS;
	}
	uct_num_threads = num_threads;
	uct_max_playouts = max_playouts;
	uct_size = bsize;
	uct_mnum = 0;

	po_init_edge(bsize);
}

static int uct_init_complete = 0;


static void uct_init_data()
{
	int i, t, mem;
	struct uct_parent *parent;

	if (uct_init_complete) {
		return;
	}
	uct_init_complete = 1;
	mem = uct_max_memory * 1024 * 1024;
	mem -= sizeof(uct_hash_table);
	mem -= sizeof(uct_search_state_nodes);
	uct_max_node = mem / (sizeof(struct uct_parent) * uct_num_threads);

	if (uct_num_threads == 1) {
		uct_max_state_node = 0;
	} else {
		uct_max_state_node = uct_max_node / 10;
		uct_search_state_nodes = (struct uct_search_state *)malloc(sizeof(struct uct_search_state) * uct_max_state_node);
		mem -= sizeof(struct uct_search_state) * uct_max_state_node;
		uct_max_node = mem / (sizeof(struct uct_parent) * uct_num_threads);
	}

	/* clear per thread node pool */
	for (t = 0; t < uct_num_threads; ++t) {
		if (!uct_parent_nodes[t]) {
			uct_parent_nodes[t] = (struct uct_parent *)malloc(uct_max_node * sizeof(struct uct_parent));
		}
		uct_next_shared[t] = 0;
		parent = uct_parent_nodes[t];
		uct_free_nodes[t] = parent;
		for (i = 0; i <  uct_max_node; ++i) {
			parent->used = FALSE;
			parent->next = parent + 1;
			++parent;
		}
		uct_parent_nodes[t][uct_max_node - 1].next = NULL;
		uct_free_node_count[t] = uct_max_node;

		uct_partner = UCT_MAX_THREADS;
		while (uct_partner >= uct_num_threads) {
			uct_partner /= 2;
		}
	}
	memset(uct_hash_table, 0, UCT_HASH_SIZE * uct_num_threads * sizeof(int *));
}

void uct_init_komi(int komi)
{
	uct_komi = komi;
}

void uct_makeamove(sqr_t s, int c)
{
	uct_moves[uct_mnum] = po_to_po(s, uct_size);
	uct_color[uct_mnum] = c;
	uct_mnum++;
}
 
void uct_takebackmove()
{
	if (uct_mnum > 0) {
		uct_mnum--;
	}
}


static void uct_garbage_inc(unsigned int thread)
{
	int i = uct_next_garbage[thread];
	struct uct_parent **p, *tmp;
	int count;
	for (count = 0; count < UCT_GARBAGE_INC_SIZE; ++count) {
		p = &uct_hash_table[thread][i];
		i++;
		if (i == UCT_HASH_SIZE) {
			i = 0;
		}
		while (*p) {
			if ((*p)->visits <= UCT_GC_VISITS) {
				tmp = *p;
				*p = (*p)->next;
				tmp->next = uct_free_nodes[thread];
				assert(tmp->used);
				assert(!tmp->shared);
				tmp->used = FALSE;
				uct_free_nodes[thread] = tmp;
				uct_free_node_count[thread]++;
				uct_collectable[thread]--;
			} else {
				p = &((*p)->next);
			}
		}
	}
	uct_next_garbage[thread] = i;
}

/* allocate a parent node
 */
static struct uct_parent *uct_alloc_node(int thread)
{
	struct uct_parent *t;
	uct_alloc_count[thread]++;
	if (uct_free_nodes[thread] == 0) {
		assert(uct_free_node_count[thread] == 0);
		uct_garbage_inc(thread);
		uct_alloc_collect[thread]++;
		if (uct_free_nodes[thread] == 0) {
			uct_alloc_fails[thread]++;
			return 0;
		}
	}
	uct_collectable[thread]++;
	t = uct_free_nodes[thread];
	assert(!t->used);
	uct_free_nodes[thread] = t->next;
	uct_free_node_count[thread]--;
	t->used = TRUE;
	t->mfgo_called = FALSE;
	t->shared = FALSE;
	return t;
}

/*
 * get a new parent node to hold the position in b and generate moves for it
 * add the node to the hash table
 */
static struct uct_parent *uct_create_node(struct po_board *b, int thread, int rules)
{
	struct uct_parent *t = uct_alloc_node(thread);
	unsigned int index = ((unsigned int)b->hash[b->mnum]) & UCT_HASH_MASK;

	if (!t) {
		return 0;
	}
	t->hash = b->hash[b->mnum];
	t->mnum = b->mnum;
	t->tm = b->tm;
	t->ko = b->ko;

	/* TODO: move to garbage collection */
	t->num_moves = 0;
	t->num_children = 0;
	t->add_use = UCT_PROGRESSIVE_START;
	t->visits = 0;
	t->last_visits = 0;
#if USE_MPI
	t->sent_mpi_visits = 0;
#endif
	t->parent_val = 0;				// so initial bias just comes from FPU

	po_uct_generate(b, t, rules);
	t->next = uct_hash_table[thread][index];
	uct_hash_table[thread][index] = t;
	return t;
}

static int isurgent(int s)
{
	list_t ptr;
	int reason;
	for (ptr = stratreasons[s]; ptr != EOL; ptr = link[ptr]) {
		reason = strat[list[ptr]].reason;
		if (reason == PLAY_IN_EMPTY_CORNER ||
			reason == JOSEKI_NO_TENUKI ||
			reason == FUSEKI_LIB && strat[list[ptr]].value >= 250) {
			return TRUE;
		}
	}
	return FALSE;
}

/*
 * delete move m from node.
 */
static int uct_delete_move(struct uct_parent *node, int m)
{
	if (m < node->num_children) {
		node->child[m].visits = UCT_LEAF;
		node->child[m].wins = 0;
		node->child[m].win_rate = 0.f;
		node->child[m].last_wins = 0;
		node->child[m].rave_win_rate = 0.f;
		node->rave_wins[m] = 0;
		node->prior_mfgo[m] = 0;
		return 0;
	}
	node->move[m] = node->move[node->num_moves - 1];
	node->rave_plays[m] = node->rave_plays[node->num_moves - 1];
	node->rave_wins[m] = node->rave_wins[node->num_moves - 1];
#ifdef UCTDEBUGOUTPUT
	node->rave_prior[m] = node->rave_prior[node->num_moves - 1];
#endif
	node->prior_mfgo[m] = node->prior_mfgo[node->num_moves - 1];
	node->num_moves--;
	return 1;
}

/* 
 * return the number of moves suggested by many faces.  0 means game won, should pass.  -1 means error.
 * set the mfgo_prior values for each child move
 * save a pointer to this node in uct_next_shared
 * might be called more than once on same position since always caleld at root, and position might already exist
 * 
 */
static int uct_prior_mfgo(struct po_board *b, struct uct_parent *node, int handicap, int rules, int thread)
{
	int i, passval, haveurgent = FALSE, ret;
	float val;
	sqr_t s;
	unsigned int oldmsptr = msptr;
	int num_dame = 0;
	int count = 0;
#if !UCT_SAVE_MFGO_PRIOR
	float win_rate;
#endif

	assert(node->used);
	while (msptr < b->mnum) {
		ret = makeuctmove(po_from_po(b->moves[msptr] >> 1, uct_size), b->moves[msptr] & 1, rules);
		assert(ret == G2OK);
		if (ret != G2OK) {	
			while (msptr > oldmsptr) {
				takebackauctmove(rules, &s);
			}
			/* illegal move */
			return -1;
		}
	}

	/* get starting moves from Many faces */
	node->mfgo_called = TRUE;
	uct_mfgo_calls++;
	getobaval();
	passval = strategy(node->tm, handicap, rules) - obaval;
	num_dame = get_reasons_for_moves(passval, node->tm, handicap, rules, FALSE);

	while (msptr > oldmsptr) {
		takebackauctmove(rules, &s);
	}

	/* won position, force a pass and return 0 */
	if (num_dame == 0 && passval > 0 && mvs[msptr - 1] == PASS) {
		node->num_children = 1;
		node->num_moves = 1;
		node->move[0] = PO_PASS;
		node->child[0].win_rate = PO_PASS;
		node->child[0].visits = UCT_LEAF;
		node->child[0].win_rate = (float)(0.9 + passval / 5000.);	/* 0.01 per point */
		node->child[0].wins = (unsigned int)(UCT_LEAF * node->child[0].win_rate);
		return 0;
	}
	
	/* check for urgent opening moves - joseki, big fuseki */
	for (i = 0; i < boardsquare; ++i) {
		if (isurgent(i)) {
			haveurgent = TRUE;
			break;
		}
	}

#if UCT_SAVE_MFGO_PRIOR
	for (i = 0; i < node->num_moves; ++i) {
		s = po_from_po(node->move[i], uct_size);
//		val = 0.5f + UCT_PRIOR_MFGO_OBAVAL * passval / obaval;
		val = (float)node->rave_wins[i] / (float)node->rave_plays[i];
		if (s == PASS) {
			if (num_dame && (b->moves[b->mnum - 2] >> 1) != PO_PASS) {
				val -= 0.2f;
			}
		}
		else if (stratguess[0][s] == 0) {
			val += UCT_PRIOR_NON_MFGO;
		}
		else if (haveurgent &&  !isurgent(s)) {
			if (i >= node->num_children) {
				i -= uct_delete_move(node, i);
			} else if (uct_size != 9) {
				val += UCT_PRIOR_NON_URGENT;
			}
		}
		else {
//			val += stratguess[0][s] * UCT_PRIOR_MFGO_OBAVAL / obaval;
			val += stratguess[0][s] / UCT_PRIOR_MFGO_VAL;		/* 1% per point */
//			count++;
		}
		if (val > 0.9f) {
			val = 0.9f;
		} else if (val < 0.1) {
			val = 0.1f;
		}
		node->prior_mfgo[i] = (int)(256 * val);
	}
#else
	for (i = 0; i < node->num_moves; ++i) {
		s = po_from_po(node->move[i], uct_size);
		win_rate = (float)node->rave_wins[i] / (float)node->rave_plays[i];
		if (s == PASS) {
			/* too early to pass, so reduce it's priority */
			if (num_dame && (b->moves[b->mnum - 2] >> 1) != PO_PASS) {
				win_rate -= 0.2f;
				if (win_rate < 0.0) {
					win_rate = 0.0;
				}
				node->rave_wins[i] = (int)(win_rate * node->rave_plays[i]);
				if (i < node->num_children) {
					node->child[i].rave_win_rate = win_rate;
				}
			} else {
				/* make sure pass is tried */
				node->rave_plays[i] += UCT_PRIOR_MFGO_PLAYS;  /* CHANGE:  = to  +=: 2% stronger */
				win_rate += 0.2f;
				if (win_rate > 0.99f) {
					win_rate = 0.99f;
				}
				node->rave_wins[i] = (int)(win_rate * node->rave_plays[i]);
				if (i < node->num_children) {
					node->child[i].rave_win_rate = win_rate;
				} else {
					if (node->num_children < UCT_MAX_CHILDREN) {
						uct_make_child(node, i, node->num_children);
					}
				}
			}
			node->prior_mfgo[i] = 256 * node->rave_wins[i] / node->rave_plays[i];
			continue;
		}
		if (haveurgent &&  !isurgent(s) ) {	/* change - was delete all moves, not just unplayed moves - delete all is weaker */
#if UCT_PRIOR_KILL_NON_URGENT
			if (i >= node->num_children) {
				i -= uct_delete_move(node, i);
			} else {
				if (uct_size == 9) {
					node->child[i].rave_win_rate /= 2;
// weaker					node->rave_wins[i] /= 2;
					node->child[i].win_rate /= 2;
					node->child[i].wins /= 2;
					node->child[i].last_wins /= 2;
				} else {
					i -= uct_delete_move(node, i);
#if 0
					node->child[i].rave_win_rate /= 4;
// weaker					node->rave_wins[i] /= 4;
					node->child[i].win_rate /= 4;
					node->child[i].wins /= 4;
					node->child[i].last_wins /= 4;
#endif
				}
			}
#else
			TODO: broken! 
			node->child[i].rave_win_rate += UCT_PRIOR_NON_URGENT;
			node->prior_mfgo[i] = 256 * node->rave_wins[i] / node->rave_plays[i];
#endif
			continue;
		}
		node->rave_plays[i] += UCT_PRIOR_MFGO_PLAYS;  /* CHANGE:  = to  +=: 2% stronger */
		if (stratguess[0][s] <= 0) {
			win_rate += UCT_PRIOR_NON_MFGO;
			if (win_rate < 0) {
				win_rate = 0;
			}
			node->rave_wins[i] = (int)(win_rate * node->rave_plays[i]);
			node->prior_mfgo[i] = 256 * node->rave_wins[i] / node->rave_plays[i];
			continue;
		}
		val = stratguess[0][s] / UCT_PRIOR_MFGO_VAL;
		win_rate += val;
		if (win_rate > UCT_PRIOR_MAX) {
			win_rate = UCT_PRIOR_MAX;
		}
		else if (win_rate < 0) {
			win_rate = 0;
		}
		node->rave_wins[i] = (int)(win_rate * node->rave_plays[i]);
		if (i < node->num_children) {
			node->child[i].rave_win_rate = (float)node->rave_wins[i] / (float)node->rave_plays[i];
			assert(node->child[i].rave_win_rate < 1.0);
		}
		node->prior_mfgo[i] = 256 * node->rave_wins[i] / node->rave_plays[i];
		count++;
	}
#endif

#if UCT_SEND_MFGO
	/* add node to list of nodes to be shared between threads */
	uct_shared_nodes[thread][uct_next_shared[thread]] = node;
	node->shared = TRUE;
	if (uct_next_shared[thread] < UCT_MAX_SHARED_NODE - 1) {
		uct_next_shared[thread]++;
	}
#endif

	/* no moves from many faces, so must pass now */
	if (count == 0) {
		node->num_children = 1;
		node->num_moves = 1;
		node->move[0] = PO_PASS;
		node->child[0].win_rate = PO_PASS;
		node->child[0].visits = UCT_LEAF;
		if (passval > 0) {
			node->child[0].win_rate = (float)(0.9 + passval / 5000.);	/* 0.01 per point */
			node->child[0].wins = (unsigned int)(UCT_LEAF * node->child[0].win_rate);
		} else {
			node->child[0].win_rate = (float)(0.1 + passval / 5000.);	/* 0.01 per point */
			node->child[0].wins = (unsigned int)(UCT_LEAF * node->child[0].win_rate);
		}
	}

	/* always add one child now */
	if (node->num_children < node->num_moves && node->num_children < UCT_MAX_CHILDREN) {
			uct_make_child(node, uct_best_rave(node), node->num_children);
	}
	/* add another one soon */
	node->add_use = UCT_PROGRESSIVE_START + node->visits / 2;
	return count;
}

/* 
 * find the best child among the children in use (from unpruning)
 * thread safe
 */
unsigned int uct_best_child(struct uct_parent *node, int depth, int thread)
{
	int i;
	double uct_val;
	double best;
	unsigned int best_child;
	double rave_val, beta;
#if !UCT_RAVE_NEW_BETA
	beta = sqrt(UCT_BETA_K/(UCT_BETA_K + 3 * node->visits));
#endif
	/* get the best child to try */
	best = -1.;
	best_child = UCT_MAX_CHILDREN;
	for (i = 0; i < node->num_children; i++) {
		if (node->child[i].visits == UCT_LEAF && node->child[i].win_rate == 0.0)
			continue;
		if (node->child[i].visits == UCT_LEAF) {
			uct_val = node->child[i].win_rate + 0.1f;
		} else {
			uct_val = node->child[i].win_rate + node->parent_val * node->child[i].child_val;
			if (depth == 0) {
				uct_val += uct_root_bias[thread][node->move[i]];
			}	

#if UCT_RAVE_NEW_BETA
			beta = node->child[i].rave_beta;
#endif
			rave_val = (double)node->child[i].rave_win_rate;
#if UCT_SAVE_MFGO_PRIOR
			if (node->mfgo_called) {
				rave_val = rave_val * (1.0 - UCT_PRIOR_MFGO_WEIGHT) + node->prior_mfgo[i] * UCT_PRIOR_MFGO_WEIGHT / 256.;
			}
#endif
			uct_val = ((1 - beta) * uct_val + beta * rave_val);
		}
		if (uct_val > best) {
			best_child = i;
			best = uct_val;
		}
	}
	return best_child;
}

/*
 * find the best child to unprune - a child that has never been visited
 */
unsigned int uct_best_rave(struct uct_parent *node)
{
	int i;
	unsigned int best_child;
	float best, rave;

	/* get the best child to try */
	best = -1;
	best_child = UCT_MAX_MOVES;
	for (i = node->num_children; i < node->num_moves; i++) {
		rave = (float)node->rave_wins[i] / (float)node->rave_plays[i];
		if (rave > best) {
			best = rave;
			best_child = i;
		}
	}
	return best_child;
}

/*
 * promote move m to be a child node, if there is room for it.
 * either in an empty slot or the last slot
 * return index of the new child
 */
int uct_make_child(struct uct_parent *node, int m, int slot) 
{
	struct uct_child *uc;
	unsigned short tmp;
	unsigned char tc;
	int i, leaf = FALSE;
#if USE_MPI
	int j;
#endif
	assert(m >= node->num_children);

#if UCT_EMPTY_SLOT
	/*
	 * find a slot for the move 
	 */
	for (i = 0; i < node->num_children; ++i) {
		if (node->child[i].visits == UCT_LEAF && node->child[i].wins == 0) {
			leaf = TRUE;
			break;
		}
	}
	if (i >= UCT_MAX_CHILDREN) {
		return 0;		/* no room left */
	}
#else
	i = node->num_children;
#endif

	/* init the new child */
	uc = &node->child[i];
	uc->visits = 0;
	uc->wins = 0;
	uc->last_wins = 0;
	uc->last_visits = 0;
#if USE_MPI
	for (j = 0; j < UCT_MPI_TARGETS; ++j) {
		uc->sent_mpi_wins[j] = 0;
		uc->sent_mpi_visits[j] = 0;
	}
#endif
	uc->win_rate = UCT_FPU;
	uc->rave_win_rate = (float)node->rave_wins[m] / (float)node->rave_plays[m];
	assert(uc->rave_win_rate <= 1.0);
	uc->child_val = 0.0f;

	/*
	 * swap move m up to num_children or a leaf, then bump num_children.  if overwriting a leaf, remove the old move
	 */
	if (m != i) {
		if (leaf) {
			node->move[i] = node->move[m];
			node->move[m] = node->move[node->num_moves - 1];
			node->rave_plays[i] = node->rave_plays[i];
			node->rave_plays[m] = node->rave_plays[node->num_moves - 1];
			node->rave_wins[i] = node->rave_wins[i];
			node->rave_wins[m] = node->rave_wins[node->num_moves - 1];
			node->prior_mfgo[i] = node->prior_mfgo[i];
			node->prior_mfgo[m] = node->prior_mfgo[node->num_moves - 1];
#ifdef UCTDEBUGOUTPUT
			node->rave_prior[i] = node->rave_prior[i];
			node->rave_prior[m] = node->rave_prior[node->num_moves - 1];
#endif
			node->num_moves--;
		} else {
			tmp = node->move[m];
			node->move[m] = node->move[i];
			node->move[i] = tmp;

			tmp = node->rave_plays[m];
			node->rave_plays[m] = node->rave_plays[i];
			node->rave_plays[i] = tmp;

			tmp = node->rave_wins[m];
			node->rave_wins[m] = node->rave_wins[i];
			node->rave_wins[i] = tmp;

#ifdef UCTDEBUGOUTPUT
			tc = node->rave_prior[m];
			node->rave_prior[m] = node->rave_prior[i];
			node->rave_prior[i] = tc;
#endif
			tc = node->prior_mfgo[m];
			node->prior_mfgo[m] = node->prior_mfgo[i];
			node->prior_mfgo[i] = tc;
		}

	}
	if (i == node->num_children) {
		node->num_children++;	
	}
	return i;
}

/* 
 * do one playout using uct
 * select the child with the highest value and make that move
 * TODO: two passes game over in UCT tree?
 * return TRUE if tree->color (side that made last move) wins this game
 */
static int uct_playout(struct po_board *b, int depth, int handicap, int rules, int thread, unsigned int uct_po_per_node)
{
	int winner;
	int windex;
	struct uct_parent *node, *tmp, *path[UCT_MAX_DEPTH];
	struct uct_child *best_child_p, *best_p[UCT_MAX_DEPTH];
	unsigned int best_child, best[UCT_MAX_DEPTH], best_rave;
	unsigned int index;
	int moves[UCT_MAX_DEPTH];	/* moves made at each depth, for rave */

	depth = 0;

	/* descend to a leaf and make a playout */
	while(1) {

		/* find the node */
		node = NULL;
		index = ((unsigned int)b->hash[b->mnum]) & UCT_HASH_MASK;
		for (tmp = uct_hash_table[thread][index]; tmp != NULL; tmp = tmp->next) {
			if (tmp->hash == b->hash[b->mnum] && tmp->tm == b->tm && tmp->ko == b->ko) {
				node = tmp;
				break;
			}
		}

		/* if not found, create a new node and its moves */
		if (!node) {
			node = uct_create_node(b, thread, rules);
			if (!node) {
				winner =  po_playout(b, rules, handicap);
				break;
			}
		}

		if (node->visits >= UCT_PRIOR_MFGO_VISITS && !node->mfgo_called) {	
			if (UCT_TRYLOCK(uct_mfgo_lock)) {		
				uct_prior_mfgo(b, node, handicap, rules, thread);
				UCT_UNLOCK(uct_mfgo_lock);
			}
		}
#ifdef UCTDEBUGOUTPUT
		else if (node->visits == UCT_GC_VISITS) {
			uct_collectable[thread]--;
		}
#endif

		/* progressive unprune */
		assert(node->visits < UCT_LEAF);
		assert(node->add_use < 1000000);	/* increase this if change max children above 30 */
		if (node->visits >= node->add_use && node->num_children < node->num_moves && node->num_children < UCT_MAX_CHILDREN) {
			node->add_use += UCT_PROGRESSIVE_MIN + node->add_use / 2;
			best_rave = uct_best_rave(node);
			assert(best_rave != UCT_MAX_MOVES);
			best_child = uct_make_child(node, best_rave, node->num_children);	/* get at least one playout with new move */
			assert(b->p_g[node->move[best_child]] == 0);
#ifdef UCTDEBUGOUTPUT
			uct_used_children++;
#endif
		} else {
			best_child = uct_best_child(node, depth, thread);
			if (best_child == UCT_MAX_CHILDREN) {	/* no legal child moves */
				best_child = 0;
				node->add_use = node->visits;
			}
	//		assert(best_child != UCT_MAX_CHILDREN);
		}
		best_child_p = &node->child[best_child];

		path[depth] = node;
		best_p[depth] = best_child_p;
		best[depth] = best_child;
		moves[depth] = node->move[best_child];
		depth++;

		if (best_child_p->visits == UCT_LEAF) {
			winner = best_child_p->win_rate > 0.5;
			break;
		} 

		/* make the best move */
		po_make_uct_move(b, node->move[best_child], b->tm);

		/* check for superko violation, only on first visit. (1.5% slowdown) TODO: this is wrong since might get here by a different path later where this move is legal */
		if (best_child_p->visits == 0 && node->move[best_child] != PO_PASS && po_superko(b)) {
			winner = 0;
			best_child_p->win_rate = 0;
			best_child_p->wins = 0;
			best_child_p->last_wins = 0;
			best_child_p->rave_win_rate = 0.0;
			node->rave_wins[best_child] = 0;
			best_child_p->visits = UCT_LEAF;
			break;
		}

		/* 
		 * not enough visits to add a new child node yet, or
		 * no more UCT search after two passes.
		 */
		if (best_child_p->visits < uct_po_per_node || depth >= UCT_MAX_DEPTH) {
			winner = po_playout(b, rules, handicap);
			if (best_child_p->visits == 0) {
				best_child_p->win_rate = 0;	/* remove FPU so the running average will be correct */
				best_child_p->wins = 0;
				best_child_p->last_wins = 0;
			}
			break;
		}
		/* two passes, so game over, just playout to get the score */
		if (b->mnum > 1 && (b->moves[b->mnum - 2] >> 1) == PO_PASS && node->move[best_child] == PO_PASS) {
			if (b->tm == PO_WHITE) {
				b->passcount[PO_WHITE]++;	/* black made second pass, so white must make another impied pass to make the chinese and japanese scores match */
			}
			winner = po_playout(b, rules, handicap);
			break;
		}
	}

	if (depth > uct_max_depth_seen) {
		uct_max_depth_seen = depth;
	}

	/* here, winner is the po_playout value after move at best[depth] is made, unless depth is zero */
	windex = 0;
	while (depth > 0) {
		depth--;
#ifdef UCTDEBUGOUTPUT
		if (best_p[depth]->visits == 0) {
			uct_visited_children++;
		}
#endif
#if !UCT_SEND_MFGO
		/* add node to list of nodes to be shared between threads */
		if (path[depth]->visits >= UCT_SHARED_VISITS && !path[depth]->shared) {
			path[depth]->shared = TRUE;
			uct_shared_nodes[thread][uct_next_shared[thread]] = path[depth];
			if (uct_next_shared[thread] < UCT_MAX_SHARED_NODE - 1) {
				uct_next_shared[thread]++;
			}
		}
#endif
		if (best_p[depth]->visits != UCT_LEAF) {
			best_p[depth]->visits++;
			best_p[depth]->wins += winner;
			assert(best_p[depth]->visits != 0);
			best_p[depth]->win_rate = ((float)best_p[depth]->wins) / best_p[depth]->visits;
			best_p[depth]->child_val = sqrtf(1.0f/best_p[depth]->visits);
		} else if (depth > 0 && best_p[depth]->win_rate != 0.0) {
			/* back up legal terminal node to parent */
			if (best_p[depth]->win_rate > 0.5) {
				/* this move wins, so parent move loses */
				best_p[depth - 1]->visits = UCT_LEAF;
				best_p[depth - 1]->win_rate = (float)(1.0 - best_p[depth]->win_rate);
				best_p[depth - 1]->wins = (unsigned int)(UCT_LEAF * best_p[depth - 1]->win_rate);
			} else if (path[depth]->visits > 200) {
				/* best move loses, so parent move wins */
				best_p[depth - 1]->visits = UCT_LEAF;
				best_p[depth - 1]->win_rate = (float)(1.0 - best_p[depth]->win_rate);
				if (best_p[depth - 1]->win_rate < 0.001)
					best_p[depth - 1]->win_rate = 0.001f;
				best_p[depth - 1]->wins = (unsigned int)(UCT_LEAF * best_p[depth - 1]->win_rate);
			}
		}
		path[depth]->visits++;
		path[depth]->parent_val = (UCT_K * sqrtf((float)log(path[depth]->visits)));
		b->p_first[moves[depth]] = (char)path[depth]->tm;
		if (
#if UCT_MFGO_NO_RAVE
			!path[depth]->mfgo_called &&
#endif
			path[depth]->visits < UCT_RAVE_MAX_UPDATE) {
			po_update_rave(b, path[depth], winner);
//			if (path[depth]->move[best[depth]] == PO_PASS) {
//				path[depth]->rave_plays[best[depth]]++;
//				best_p[depth]->rave_win_rate += (winner - best[depth]->rave_win_rate)/path[depth]->rave_plays[best];
//			}
		}

		winner = !winner;
		windex = 1 - windex;
	}

	return winner;
}

/* return the best child in the uct tree.  if a tie, return the first one. 
 * most visits unless UCT_LEAF
 */
static unsigned int uct_best(struct uct_parent *node)
{
	unsigned int t, best = 0;
	unsigned int visits = node->child[0].visits;
	float best_rate = 0.0;

	/* return a winning leaf */
	for (t = 0; t < node->num_children; ++t) {
		assert(node->visits <= UCT_LEAF);
		if (node->child[t].visits == UCT_LEAF && node->child[t].win_rate > best_rate) {
			best_rate = node->child[t].win_rate;
			best = t;
		}
	}

	if (node->child[best].visits == UCT_LEAF && node->child[best].win_rate >= 0.9)
		return best;

	/* else return the non-leaf with the most visits */
	if (visits == UCT_LEAF) {
		visits = 0;
	}
	for (t = 1; t < node->num_children; t++) {
		if (node->child[t].visits >= visits && node->child[t].visits < UCT_LEAF) {
			best = t;
			visits = node->child[t].visits;
		}
	}
	return best;
}

/* return the next best interior child in the uct tree */
unsigned int uct_next_best(struct uct_parent *node, unsigned int max, int best_index)
{
	unsigned int best = UCT_MAX_CHILDREN;
	unsigned int visits = 0;
	int t;
	for (t = 0; t < node->num_children; t++) {
		if (node->child[t].visits > max || node->child[t].visits == max && t <= best_index) {
			continue;
		}
		if (node->child[t].visits > visits) {
			best = t;
			visits = node->child[t].visits;
		}
	}
	return best;
}

/* 
 * keep search data around for statistics output 
 */
static int uct_num_moves;

/*
 * initialize a new search
 * allocate the board for the start of the search and make the moves to the curretn position on it
 */
static struct po_board *uct_init_search(int handicap, int color)
{
	int i, t;
	struct po_board *uct_base;
	__int64 po_seed = 12345678901234567;
#ifndef G2DEBUGOUTPUT
#ifdef WIN32
	FILETIME tm;
	GetSystemTimeAsFileTime(&tm);
	po_seed += tm.dwLowDateTime;
	po_seed += ((__int64)tm.dwHighDateTime) << 32;
#endif
	po_seed += (int)time(NULL);
#ifdef WIN32
	po_seed += getpid();
#endif
#endif
	uct_mpi_send_count = uct_mpi_recv_count = 0;
	uct_mpi_send_msg_count = uct_mpi_recv_msg_count = 0;
	uct_mpi_fwd_count = uct_mpi_fwd_msg_count = 0;
	uct_mpi_max_message_count = 0;
	uct_mpi_max_recv_count = 0;
	uct_mpi_recv_missing = 0;
	uct_base = po_alloc(uct_komi, uct_size, po_seed);
	uct_max_depth_seen = 0;
	uct_mfgo_calls = 0;
	uct_visited_children = 0;
	uct_used_children = 0;
	uct_playouts[uct_mnum] = 0;
	uct_num_moves = 0;
	uct_state_min_free = uct_max_state_node;
	uct_state_fail_count = 0;
	po_init(uct_base, handicap, uct_komi, uct_size, uct_base->seed);

	for (i = 0; i < uct_mnum; ++i) {
		po_make_uct_move(uct_base, uct_moves[i], uct_color[i]);
		uct_base->reason[uct_base->mnum - 1] = PO_REASON_GAME;
	}
	/* reset the rave values */
	for (i = 0; i < PO_BSIZE; ++i) {
		uct_base->p_first[i] = PO_EMPTY;
	}
	uct_base->tm = color;
	for (i = 0; i < uct_num_threads; ++i) {
		uct_thread_playouts[i] = 0;
		uct_alloc_count[i] = 0;
		uct_alloc_fails[i] = 0;
		uct_alloc_collect[i] = 0;
		uct_collectable[i] = 0;
		uct_nodes_sent[i] = 0;
		uct_nodes_received[i] = 0;
		uct_nodes_dropped[i] = 0;
		uct_nodes_alloced[i] = 0;
	}
	uct_base->rave_moves = uct_base->mnum + (uct_base->boardsize * uct_base->boardsize) * 3 / 4;
	for (t = 0; t < uct_num_threads; ++t) {
		uct_state_sent[t] = 0;
		for (i = 0; i < uct_num_threads; ++i) {
			uct_state_next[t][i] = 0;
		}
	}
	uct_state_free_nodes = uct_search_state_nodes;
	for (i = 0; i < uct_max_state_node - 1; ++i) {
		uct_search_state_nodes[i].next = &uct_search_state_nodes[i + 1];
		uct_search_state_nodes[i].used = 0;
	}
	if (uct_max_state_node) {
		uct_search_state_nodes[uct_max_state_node - 1].used = 0;
		uct_search_state_nodes[uct_max_state_node - 1].next = NULL;
	}
	uct_state_free_node_count = uct_max_state_node;
#if UCT_LEARN
	assert(uct_mnum == msptr);
	learn_get_hash(uct_mnum, learn_hash);
#endif
	return uct_base;
}

static struct uct_search_state *uct_state_alloc(void)
{
	volatile struct uct_search_state *s;
	UCT_LOCK(uct_state_lock);
	s = uct_state_free_nodes;
	if (s) {
		uct_state_free_nodes = s->next;
		uct_state_free_node_count--;
		assert(!s->used);
		s->used = TRUE;
	}
	UCT_UNLOCK(uct_state_lock);
	return (struct uct_search_state *)s;
}

/* merge the data back from the other threads */
static void uct_merge_receive(int thread, struct uct_parent *uct_root)
{
	int t, i, m, visits;
#if USE_MPI && UCT_ADD_SHARED
	int j;
#endif
#if UCT_ADD_BEST
	int best_move;
	unsigned int best_visits;
#endif
	volatile struct uct_search_state *us;
	unsigned int index;
	struct uct_parent *node, *tmp;

	/* receive from thread t */
	for (t = 0; t < uct_num_threads; ++t) {
		if (t == thread) {
			continue;
		}
		us = uct_state_next[thread][t];
		assert(!us || us->threads & (1 << thread));
		if (!us) {
			us = uct_state_sent[t];
		}
		for (; us; us = us->next) {
			assert(us->used);
			if (us->threads & (1 << thread)) {
				continue;
			}
			/* find the node */
			node = NULL;
			index = ((unsigned int)us->hash) & UCT_HASH_MASK;
			for (tmp = uct_hash_table[thread][index]; tmp != NULL; tmp = tmp->next) {
				if (tmp->hash == us->hash && tmp->tm == us->tm && tmp->ko == us->ko) {
					node = tmp;
					break;
				}
			}
			if (!node) {
#if UCT_ADD_SHARED
				node = uct_alloc_node(thread);
				if (!node) {
					uct_nodes_dropped[thread]++;
					UCT_LOCK(uct_state_lock);
					uct_state_next[thread][t] = us;
					us->threads |= 1 << thread;	/* TODO - find a way not to throw away this data */
					UCT_UNLOCK(uct_state_lock);
					continue;
				}
				uct_nodes_alloced[thread]++;
				node->hash = us->hash;
				node->mnum = us->mnum;
				node->tm = us->tm;
				node->ko = us->ko;
				node->num_moves = us->num_moves;
				node->num_children = us->num_children;
				assert(node->num_children);
				node->visits = 0;
				for (i = 0; i < us->num_moves; ++i) {
					node->move[i] = us->move[i];
				}
				for (i = 0; i < us->num_moves; ++i) {
					node->rave_plays[i] = UCT_RAVE_FPU_PLAYS;
					node->rave_wins[i] = UCT_RAVE_FPU_WINS;
#ifdef UCTDEBUGOUPTPUT
					node->rave_prior[i] = 128;
#endif
				}
				visits = node->visits;
				for (i = 0; i < us->num_children; ++i) {
					node->visits += us->visits[i];  			/* since every move is sent, moves can come in with no visits */
					node->child[i].visits = us->visits[i];
					node->child[i].last_visits = us->visits[i];
					node->child[i].wins = us->wins[i];
					node->child[i].last_wins = us->wins[i];
#if UCT_SEND_ADJUST_RAVE
					if (visits < UCT_RAVE_MAX_UPDATE) {
						node->rave_wins[i] += us->wins[i];
						node->rave_plays[i] += us->visits[i];
					}
#endif
					node->child[i].win_rate = (float)node->child[i].wins / (float)node->child[i].visits;
					node->child[i].child_val = sqrtf(1.0f/node->child[i].visits);
					node->child[i].rave_win_rate = (float)node->rave_wins[i] / (float)node->rave_plays[i];
#if USE_MPI
					for (j = 0; j < UCT_MPI_TARGETS; ++j) {
						node->child[i].sent_mpi_wins[j] = 0;
						node->child[i].sent_mpi_visits[j] = 0;
					}
#endif
				}
				node->add_use = node->visits + 100;
				node->mfgo_called = FALSE;
				node->parent_val = (UCT_K * sqrtf((float)log(node->visits)));
				node->next = uct_hash_table[thread][index];
				uct_hash_table[thread][index] = node;
#else
				uct_nodes_dropped[thread]++;
				UCT_LOCK(uct_state_lock);
				uct_state_next[thread][t] = us;
				us->threads |= 1 << thread;	/* TODO - find a way not to throw away this data */
				UCT_UNLOCK(uct_state_lock);
				continue;
#endif
			}
#if UCT_ADD_BEST
			/* add the best incoming child if it is not already */
			uct_nodes_received[thread]++;
			if (node->num_children < UCT_MAX_CHILDREN) {
				best_visits = 0;
				best_move = 0;
				for (i = 0; i < us->num_children; ++i) {
					if (us->visits[i] > best_visits) {
						best_visits = us->visits[i];
						best_move = us->move[i];
					}
				}
				for (m = 0; m < node->num_children; ++m) {
					if (node->move[m] == best_move) {
						break;
					}	
				}
				if (m == node->num_children) {
					for (; m < node->num_moves; ++m) {
						if (node->move[m] == best_move) {
							uct_make_child(node, m, node->num_children);
							break;
						}
					}
				}
			}
#endif

			/* add the data to the children */
			visits = node->visits;
			for (i = 0; i < us->num_children; ++i) {
				for (m = 0; m < node->num_children; ++m) {
					if (node->move[m] == us->move[i]) {
						if (node->child[m].visits == UCT_LEAF) {
							continue;
						}
						if (us->visits[i] == UCT_LEAF) {
							node->child[m].visits = UCT_LEAF;
							node->child[m].wins = us->wins[i];
							node->child[m].child_val = 0.0f;
							node->child[m].win_rate = ((float)node->child[m].wins) / node->child[m].visits;
							continue;
						}
						node->visits += us->visits[i];
						node->child[m].visits += us->visits[i];
						node->child[m].last_visits += us->visits[i];
						node->child[m].wins += us->wins[i];
						node->child[m].last_wins += us->wins[i];
#if UCT_SEND_ADJUST_RAVE
						if (visits < UCT_RAVE_MAX_UPDATE) {
							node->rave_wins[m] += us->wins[i];
							node->rave_plays[m] += us->visits[i];
							node->child[m].rave_win_rate = (float)node->rave_wins[m] / (float)node->rave_plays[m];
						}
#endif
						if (node->child[m].visits != 0) {
							node->child[m].win_rate = ((float)node->child[m].wins) / node->child[m].visits;
							node->child[m].child_val = sqrtf(1.0f / node->child[m].visits);
						}
						break;
					}
				}
			}
			assert(node->visits < UCT_LEAF);
			node->parent_val = (UCT_K * sqrtf((float)log(node->visits)));
			UCT_LOCK(uct_state_lock);
			uct_state_next[thread][t] = us;
			us->threads |= 1 << thread;
			UCT_UNLOCK(uct_state_lock);
		}
	}
}

/* 
 * send delta for every node with enough visits to all other thread
 */
static void uct_merge_send(int thread, struct uct_parent *node)
{
	/* send node to all threads by putting it in our output list */
	volatile struct uct_search_state *us;
	int i;
#if UCT_SEND_NO_EMPTY
	if (node->visits == node->last_visits) {
		return;
	}
#endif
	assert(node->used);
	us = uct_state_alloc();
	if (!us) {
		uct_state_fail_count++;
		return;
	}
	node->last_visits = node->visits;
	us->tm = node->tm;
	us->ko = node->ko;
	us->hash = node->hash;
	us->threads = 1 << thread;
	us->next = 0;
	us->mnum = node->mnum;
	us->num_children = node->num_children;
	us->num_moves = node->num_moves;
	for (i = 0; i < node->num_moves; ++i) {
		us->move[i] = node->move[i];
	}
	for (i = 0; i < node->num_children; ++i) {
		if (node->child[i].visits == UCT_LEAF) {
			us->visits[i] = UCT_LEAF;
			us->wins[i] = node->child[i].wins;
		} else {
			us->visits[i] = node->child[i].visits - node->child[i].last_visits;
			us->wins[i] = node->child[i].wins - node->child[i].last_wins;
		}
		node->child[i].last_visits = node->child[i].visits;
		node->child[i].last_wins = node->child[i].wins;
	}
	if (!uct_state_sent[thread]) {
		uct_state_sent[thread] = us;	/* must be last so other threads get correct link */
	} else {
		assert(uct_state_last[thread]->next == 0);
		uct_state_last[thread]->next = us;
	}
	uct_state_last[thread] = us;
	uct_nodes_sent[thread]++;
}

/*
 * free up the nodes already sent and seen by all other threads, but leave one to prevent race
 */
static void uct_merge_free(int thread)
{
	volatile struct uct_search_state *us, *tmp;
	int count = 0;
	for (us = uct_state_sent[thread]; us != 0 && us->next != 0; us = us->next) {
		count++;
		/* put the nodes from uct_state_sent to us in the free list 
		 * leave one node with all threads seen to avoid race condition
		*/
		if (us->next->threads == (1 << uct_num_threads) - 1 &&			/* next node seen by all threads */
			(us->next->next == 0 ||										/* followed by no node */
			 us->next->next->threads != (1 << uct_num_threads) - 1)) {	/* or a node not seen by all */
			UCT_LOCK(uct_state_lock);

			/* clear the used bit in all states being freed */
			for (tmp = uct_state_sent[thread]; tmp != us->next; tmp = tmp->next) {
				assert(uct_state_next[1 - thread][thread] != tmp); 
				assert(tmp->used);
				tmp->used = FALSE;
			}
			tmp = uct_state_sent[thread];
			uct_state_sent[thread] = us->next;
			us->next = uct_state_free_nodes;
			uct_state_free_nodes = tmp;
			uct_state_free_node_count += count;
			UCT_UNLOCK(uct_state_lock);
			break;
		}
	}
}

/*
 * send all mfgo nodes to the other threads
 */
static void uct_merge_search(int thread, struct uct_parent *uct_root) 
{
	int i;
	/* free sent data after it is read by the other threads */
	uct_merge_free(thread);
	/* send all mfgo nodes, only children with reasonable number of visits */
	for (i = 0; i < uct_next_shared[thread]; ++i) {
		uct_merge_send(thread, uct_shared_nodes[thread][i]);
	}
	if (uct_state_free_node_count < uct_state_min_free) {
		uct_state_min_free = uct_state_free_node_count;
	}
	/* get all input data from other threads */
	uct_merge_receive(thread, uct_root);
}

#if USE_MPI

/* add one parent node to the mpi send buffer
 * track what we have already seen (sent or received) and just send new data
 */
void uct_mpi_add_send(int index, struct uct_parent *node, struct uct_search_state *us, struct uct_parent *uct_root)
{
	int i, s;
#ifdef MPIDEBUGOUTPUT
	char buf[400];
#endif

	us->tm = node->tm;
	us->ko = node->ko;
	us->hash = node->hash;
	us->threads = 1;
	us->next = 0;
	us->num_children = node->num_children;
	for (i = 0; i < node->num_children; ++i) {
		s = node->move[i];
		us->move[i] = s;

		assert(node->child[i].visits >= node->child[i].sent_mpi_visits[index]);
		if (node->child[i].visits == UCT_LEAF) {
			us->visits[i] = node->child[i].visits;
			us->wins[i] = node->child[i].wins;
		} else {
			us->visits[i] = node->child[i].visits - node->child[i].sent_mpi_visits[index];
			us->wins[i] = node->child[i].wins - node->child[i].sent_mpi_wins[index];
		}
#if !UCT_MPI_ALLREDUCE
		node->child[i].sent_mpi_visits[index] += us->visits[i];
		node->child[i].sent_mpi_wins[index] += us->wins[i];
#endif
	}

#ifdef MPIDEBUGOUTPUT
	if (node == uct_root) {
		sprintf(buf, "Root %d total visits, %d/%d, %d/%d two moves.  Sending %d/%d, %d/%d, sent %d/%d %d/%d", 
			uct_root->visits, uct_root->child[0].wins, uct_root->child[0].visits, uct_root->child[1].wins, uct_root->child[1].visits,
			us->wins[0], us->visits[0], us->wins[1], us->visits[1],
			uct_root->child[0].sent_mpi_wins[0], uct_root->child[0].sent_mpi_visits[0], uct_root->child[1].sent_mpi_wins[0], uct_root->child[1].sent_mpi_visits[0]
		);
		outerr(buf);
	}
#endif
}

#if UCT_MPI_ALLREDUCE
struct uct_search_state uct_mpi_msg_recv_buffer[UCT_MPI_BUFFER_COUNT];

/* merge function for the reduce computation */
void uct_mpi_merge(struct uct_search_state *in, struct uct_search_state *out)
{
	int i, m;
	/* add the data to the children */
	for (i = 0; i < in->num_children; ++i) {
		for (m = 0; m < out->num_children; ++m) {
			if (in->move[m] == out->move[i]) {
				out->visits[m] += in->visits[i];
				out->wins[m] += in->wins[i];
				break;
			}
		}
		if (m == out->num_children && out->num_moves < UCT_MAX_CHILDREN) {
			out->num_children++;
			out->visits[m] = in->visits[i];
			out->wins[m] = in->wins[i];
		}
	}
}

/* reduce operation called by allreduce */
_stdcall uct_mpi_reduce(struct uct_search_state *in, struct uct_search_state *inout, int *count, MPI_Datatype *dptr)
{
	int outcount = 0;
	struct uct_search_state *inptr = in, *outptr = inout;
	struct uct_search_state *merge[UCT_MPI_BUFFER_COUNT];		/* sorted list of merged nodes */
	int next = 0;
	while( outcount < UCT_MPI_BUFFER_COUNT && inout[outcount].threads != 0) {
		outcount++;
	}
	assert(outcount != UCT_MPI_BUFFER_COUNT);
	while ((inptr->threads != 0 || outptr->threads != 0) && next < UCT_MPI_BUFFER_COUNT) {
		if (inptr->threads == 0) {
			merge[next++] = outptr;
			outptr++;
		}
		else if (outptr->threads == 0) {
			merge[next++] = inptr;
			inptr++;
		}
		else if (inptr->hash == outptr->hash) {	/* merge */
			uct_mpi_merge(inptr, outptr);
			merge[next++] = outptr;
			outptr++;
			inptr++;
		}
		else if (inptr->hash > outptr->hash) {
			merge[next++] = outptr;
			outptr++;
		} else {
			merge[next++] = inptr;
			inptr++;
		}
	}
	inout[next].threads = 0;
	while (next > 0) {
		next--;
		if (inout[next].hash != merge[next]->hash) {
			memcpy(&inout[next], merge[next], sizeof(struct uct_search_state));
		}
	}
}

int uct_mpi_qsort_cmp(const void *a, const void *b)
{
	struct uct_parent *left = *(struct uct_parent **)a;
	struct uct_parent *right = *(struct uct_parent **)b;
	if (left->hash < right->hash) {
		return -1;
	}
	return 1;
}

/* merge the allreduce result back into my local data 
*/
void uct_mpi_reduce_merge(struct uct_parent *uct_root)
{
#ifdef MPIDEBUGOUTPUT
	char buf[400];
#endif
#if UCT_ADD_BEST
	int best_move;
	unsigned int best_visits;
#endif	
	int i, m, msg, visits, wins;
	unsigned int index;
	struct uct_parent *node, *tmp;
	struct uct_search_state *us;
	for (msg = 0; msg < UCT_MPI_BUFFER_COUNT; ++msg) {
		us = &uct_mpi_msg_recv_buffer[msg];
		if (us->threads == 0) {
			if (msg > uct_mpi_max_recv_count) {
				uct_mpi_max_recv_count = msg;
			}
			break;
		}
		node = NULL;
		index = ((unsigned int)us->hash) & UCT_HASH_MASK;
		for (tmp = uct_hash_table[0][index]; tmp != NULL; tmp = tmp->next) {
			if (tmp->hash == us->hash && tmp->tm == us->tm && tmp->ko == us->ko) {
				node = tmp;
				break;
			}
		}
		if (!node) {
			uct_mpi_recv_missing++;
			continue;
		}

#if UCT_ADD_BEST
		/* add the best incoming child if it is not already */
		if (node->num_children < UCT_MAX_CHILDREN) {
			best_visits = 0;
			best_move = 0;
			for (i = 0; i < us->num_children; ++i) {
				if (us->visits[i] == UCT_LEAF) {
					continue;
				}
				if (us->visits[i] > best_visits) {
					best_visits = us->visits[i];
					best_move = us->move[i];
				}
			}
			for (m = 0; m < node->num_children; ++m) {
				if (node->move[m] == best_move) {
					break;
				}	
			}
			if (m == node->num_children) {
				for (; m < node->num_moves; ++m) {
					if (node->move[m] == best_move) {
						uct_make_child(node, m);
						break;
					}
				}
			}
		}
#endif

		/* reduced value includes my contribution.  subtract that off befor doing the sum. */
		for (i = 0; i < us->num_moves; ++i) {
			for (m = 0; m < node->num_children; ++m) {
				if (node->move[m] == us->move[i]) {
					/* new visits from other nodes - reduce result, less the ones I sent */
					if (node->mfgo_called) {		// TODO - wrong!  Add code for UCT_LEAF
						visits = us->visits[i] - (node->child[m].visits - node->child[m].sent_mpi_visits[0]);
						wins = us->wins[i] - (node->child[m].wins - node->child[m].sent_mpi_wins[0]);
					} else {
						visits = us->visits[i];
						wins = us->wins[i];
					}
					assert(visits >= 0);
					assert(wins >= 0);
					if (visits < 0 || wins < 0) {
						break;
					}

					node->visits += visits;
					node->child[m].visits += visits;
					node->child[m].wins += wins;

					node->child[m].win_rate = ((float)node->child[m].wins) / node->child[m].visits;
					node->child[m].child_val = sqrtf(1.0f/node->child[m].visits);

					node->child[m].sent_mpi_visits[0] = node->child[m].visits;
					node->child[m].sent_mpi_wins[0] = node->child[m].wins;
					break;
				}
			}
		}
		node->sent_mpi_visits = node->visits;
#ifdef MPIDEBUGOUTPUT
		if (node == uct_root) {
			sprintf(buf, "Root reduce to: %d total visits, %d/%d, %d/%d two moves (received %d/%d, %d/%d).", 
				uct_root->visits, uct_root->child[0].wins, uct_root->child[0].visits, uct_root->child[1].wins, uct_root->child[1].visits,
				us->wins[0], us->visits[0], us->wins[1], us->visits[1]);
			outerr(buf);
		}
#endif
	}
}

/*
 * merge search data across the MPI cluster
 * use MPI_ALLREDUCE
 * inefficient because I have to send the maximum size buffer rather than just the changes.  the bsend version does much less communications
 * only call in thread 0!
 */
static void uct_mpi_merge_search(struct uct_parent *uct_root)
{
	int sent, i;

	/* send all interesting nodes to my everyone */
	qsort(uct_shared_nodes[0], uct_shared_mfgo[0], sizeof(int *), uct_mpi_qsort_cmp);
	sent = 0;
	for (i = 0; i < uct_next_shared[0]; ++i) {
		if (uct_shared_nodes[0][i]->sent_mpi_visits == uct_shared_nodes[0][i]->visits ||
			uct_shared_nodes[0][i]->visits == 0) {
			continue;	/* no new data to send at this node */
		}
		uct_mpi_add_send(0, uct_shared_nodes[0][i], &uct_mpi_msg_buffer[sent], uct_root);
		sent++;
		uct_shared_nodes[0][i]->sent_mpi_visits = uct_shared_nodes[0][i]->visits;
		if (sent >= UCT_MPI_BUFFER_COUNT - 1) {
			break;  
		}
	}
	uct_mpi_msg_buffer[sent].threads = 0;	/* to mark the end of the buffer */

	uct_mpi_send_count++;
	if (sent > uct_mpi_max_message_count) {
		uct_mpi_max_message_count = sent;
	}
	if (MPI_Allreduce(uct_mpi_msg_buffer, uct_mpi_msg_recv_buffer, UCT_MPI_BUFFER_COUNT * sizeof(struct uct_search_state), MPI_CHAR, uct_mpi_reduce_op, MPI_COMM_WORLD) != MPI_SUCCESS) {
		outerr("MPI_Allreduce error");
		return;
	}
	uct_mpi_reduce_merge(uct_root);
}

#else  /* bsend based allreduce code here */

/* complete overlap of reduction with computation.  but delay is getting results communicated.
*/

/*
 * receive a message and forward it
 */
static int uct_mpi_recv(struct uct_parent *uct_root)
{
	unsigned int index;
	struct uct_parent *node, *tmp;
	struct uct_search_state *us;
	int i, m, flag, delta, ind, from_ind, to_ind;
	MPI_Status status;
	int source, size, msg, missing = 0, visits, wins, forward;
#if UCT_ADD_BEST
	int best_move;
	unsigned int best_visits;
#endif
#ifdef MPIDEBUGOUTPUT
	char buf[200];
#endif

	/* any messages available? */
	if (MPI_Iprobe(MPI_ANY_SOURCE, UCT_MPI_TAG, MPI_COMM_WORLD, &flag, &status) != MPI_SUCCESS) {
		outerr("MPI_IProbe failed");
		return 0;
	}
	if (!flag) {
		return 0;	/* no messages waiting */
	}
	if (MPI_Recv(uct_mpi_msg_buffer, sizeof(struct uct_search_state) * UCT_MPI_BUFFER_COUNT, MPI_CHAR, MPI_ANY_SOURCE, UCT_MPI_TAG, MPI_COMM_WORLD, &status) != MPI_SUCCESS) {
		outerr("MPI_Recv failed");
		return 0;
	}
	uct_mpi_recv_count++;
	source = status.MPI_SOURCE;

	MPI_Get_count(&status, MPI_CHAR, &size);
	size /= sizeof(struct uct_search_state);
	delta = status.MPI_SOURCE ^ uct_my_rank;
	for (from_ind = 0; from_ind < UCT_MPI_TARGETS; ++from_ind) {
		if ((1 << from_ind) == delta)
			break;
	}
	assert(from_ind != UCT_MPI_TARGETS);
	delta >>= 1;
	if (delta) {
		forward = uct_my_rank ^ delta;
		for (to_ind = 0; to_ind < UCT_MPI_TARGETS; ++to_ind) {
			if ((1 << to_ind) == delta)
				break;
		}
		assert(to_ind != UCT_MPI_TARGETS);
	}

	for (msg = 0; msg < size; ++msg) {
		us = &uct_mpi_msg_buffer[msg];
		node = NULL;
		index = ((unsigned int)us->hash) & UCT_HASH_MASK;
		for (tmp = uct_hash_table[0][index]; tmp != NULL; tmp = tmp->next) {
			if (tmp->hash == us->hash && tmp->tm == us->tm && tmp->ko == us->ko) {
				node = tmp;
				break;
			}
		}
		if (!node) {
			missing++;
			continue;
		}

#if UCT_ADD_BEST
		/* add the best incoming child if it is not already */
		if (node->num_children < UCT_MAX_CHILDREN) {
			best_visits = 0;
			best_move = 0;
			for (i = 0; i < us->num_children; ++i) {
				if (us->visits[i] > best_visits) {
					best_visits = us->visits[i];
					best_move = us->move[i];
				}
			}
			for (m = 0; m < node->num_children; ++m) {
				if (node->move[m] == best_move) {
					break;
				}	
			}
			if (m == node->num_children) {
				for (; m < node->num_moves; ++m) {
					if (node->move[m] == best_move) {
						uct_make_child(node, m);
						break;
					}
				}
			}
		}
#endif

		/* add the data to the children, and update the message for forwarding */
#ifdef MPIDEBUGOUTPUT
		if (node == uct_root) {
			sprintf(buf, "receive root, from %d index %d, current %d/%d, %d/%d, got %d/%d, %d/%d", source, from_ind,
				node->child[0].wins, node->child[0].visits, node->child[1].wins, node->child[1].visits,
				us->wins[0], us->visits[0], us->wins[1], us->visits[1]);
			outerr(buf);
			if (delta) {
				sprintf(buf, "forwarding to %d, index %d", forward, to_ind);
			}
		}
#endif
		for (i = 0; i < us->num_children; ++i) {
			for (m = 0; m < node->num_children; ++m) {
				if (node->child[m].visits == UCT_LEAF) {
					continue;
				}
				if (node->move[m] == us->move[i]) {
					visits = us->visits[i];
					wins = us->wins[i];

					node->visits += visits;
					node->child[m].visits += visits;
					node->child[m].wins += wins;

					/* don't send node's data back to him or anyone further away */
					for (ind = from_ind; ind < UCT_MPI_TARGETS; ++ind) {
						node->child[m].sent_mpi_visits[ind] += visits;
						node->child[m].sent_mpi_wins[ind] += wins;
					}

					/* don't send again to forwarded node */
					if (delta) {
						/* add my data before sending it on */
						assert(node->child[m].visits >= node->child[m].sent_mpi_visits[from_ind]);

						us->visits[i] = node->child[m].visits - node->child[m].sent_mpi_visits[to_ind];
						us->wins[i] = node->child[m].wins - node->child[m].sent_mpi_wins[to_ind];

						node->child[m].sent_mpi_visits[to_ind] += us->visits[i];
						node->child[m].sent_mpi_wins[to_ind] += us->wins[i];
					}

					node->child[m].win_rate = ((float)node->child[m].wins) / node->child[m].visits;
					node->child[m].child_val = sqrtf(1.0f/node->child[m].visits);
					break;
				}
			}
			node->parent_val = (UCT_K * sqrtf((float)log(node->visits)));
		}
		if (!node->shared && node->visits >= UCT_SHARED_VISITS) {
			node->shared = TRUE;
			uct_shared_nodes[0][uct_next_shared[0]] = node;
			if (uct_next_shared[0] < UCT_MAX_SHARED_NODE - 1) {
				uct_next_shared[0]++;
			}
		}
#ifdef MPIDEBUGOUTPUT
		if (node == uct_root) {
			sprintf(buf, "receive root merge, current %d/%d, %d/%d, updated %d/%d, %d/%d",
				node->child[0].wins, node->child[0].visits, node->child[1].wins, node->child[1].visits,
				us->wins[0], us->visits[0], us->wins[1], us->visits[1]);
			outerr(buf);
		}
#endif
	}
#ifdef MPIDEBUGOUTPUT
	sprintf(buf, "MPI recv %d from rank %d, can't find %d nodes", size, source, missing);
	outerr(buf);
#endif
	/* send the data onwards, except from last input (nearest neightbor) */
	if (delta) {
		uct_mpi_fwd_count++;
		uct_mpi_fwd_msg_count += size;
		if (MPI_Bsend(uct_mpi_msg_buffer, sizeof(struct uct_search_state) * size, MPI_CHAR, forward, UCT_MPI_TAG, MPI_COMM_WORLD) != MPI_SUCCESS) {
			outerr("MPI_Bsend failed");
		}
	} 
	return size;
}

/*
 * merge search data across the MPI cluster
 * each node sends to one other node
 * when it receives from a node, it forwards the data to one other node, unless it is received from the adjacent node
 * only call in thread 0!
 */
static void uct_mpi_merge_search(struct uct_parent *uct_root)
{
	int i, size;
	int index;
	int sent, received, rcv_count = 0, total_sent = 0;
#ifdef MPIDEBUGOUTPUT
	char buf[200];
#endif
	int mpi_node = uct_my_rank ^ uct_pair;

	/* receive data, merge it, and forward it */
	received = 0;
	while (size = uct_mpi_recv(uct_root)) {
		received += size;
		rcv_count++;
	}

	/* send all interesting nodes to my partner */
	if (mpi_node >= uct_num_nodes)
		return;	/* this node won't participate */
	for (index = 0; index < UCT_MPI_TARGETS; ++index) {
		if ((1 << index) == uct_pair)
			break;
	}
	assert(index != UCT_MPI_TARGETS);	sent = 0;
	for (i = 0; i < uct_next_shared[0]; ++i) {
		if (uct_shared_nodes[0][i]->sent_mpi_visits == uct_shared_nodes[0][i]->visits) {
			continue;	/* no new data to send at this node */
		}
		uct_mpi_add_send(index, uct_shared_nodes[0][i], &uct_mpi_msg_buffer[sent], uct_root);
		sent++;
		uct_shared_nodes[0][i]->sent_mpi_visits = uct_shared_nodes[0][i]->visits;
		if (sent >= UCT_MPI_BUFFER_COUNT) {
			uct_mpi_send_count++;
			uct_mpi_send_msg_count += sent;
			if (sent > uct_mpi_max_message_count) {
				uct_mpi_max_message_count = sent;
			}
			if (MPI_Bsend(uct_mpi_msg_buffer, sizeof(struct uct_search_state) * sent, MPI_CHAR, mpi_node, UCT_MPI_TAG, MPI_COMM_WORLD) != MPI_SUCCESS) {
				outerr("MPI_Bsend failed");
				sent = 0;
				break;
			}
			total_sent += sent;
			sent = 0;
		}
	}

	/* if it fails we don't care.  will add data again next time.  Must be a BSend to avoid deadlock since messages are exchanged 
	 * MPI_Send is no good, because it might block until there is a matching receive.  In this case we would get a deadlock since me and my partner are exchanging data
	 * MPI_BSend is required so the send will always complete even if there is no receive.
	*/
	if (sent) {
		uct_mpi_send_count++;
	}
	if (sent && MPI_Bsend(uct_mpi_msg_buffer, sizeof(struct uct_search_state) * sent, MPI_CHAR, mpi_node, UCT_MPI_TAG, MPI_COMM_WORLD) != MPI_SUCCESS) {
		outerr("MPI_Bsend failed");
		sent = 0;
	}
	total_sent += sent;

#ifdef MPIDEBUGOUTPUT
	sprintf(buf, "MPI received %d, sent %d of %d mfgo nodes\n", received, total_sent, uct_next_shared[0]);
	outerr(buf);
#endif

}

/* eat any pending MPI messages */
static void uct_mpi_eat(void) 
{
	int flag, size, source;
	MPI_Status status;
#ifdef MPIDEBUGOUTPUT
	char buf[200];
#endif

	return;

	/* eat any pending MPI messages */
	/* any messages available? */
	while(1) {
		if (MPI_Iprobe(MPI_ANY_SOURCE, UCT_MPI_TAG, MPI_COMM_WORLD, &flag, &status) != MPI_SUCCESS) {
			outerr("MPI_IProbe failed");
			return;
		}
		if (!flag) {
			break;	/* no messages waiting */
		}

		if (MPI_Recv(uct_mpi_msg_buffer, sizeof(struct uct_search_state) * UCT_MPI_BUFFER_COUNT, MPI_CHAR, MPI_ANY_SOURCE, UCT_MPI_TAG, MPI_COMM_WORLD, &status) != MPI_SUCCESS) {
			outerr("MPI_Recv failed");
			return;
		}
		source = status.MPI_SOURCE;

		MPI_Get_count(&status, MPI_CHAR, &size);
		size /= sizeof(struct uct_search_state);
#ifdef MPIDEBUGOUTPUT
		sprintf(buf, "MPI recv discard %d from rank %d", size, source);
		outerr(buf);
#endif
	}
}

#endif /* UCT_MPI_ALLREDUCE */

#endif  /* USE_MPI */

/*
 * set the bias values for the root node - permanent, nondecreasing
 */
static void uct_set_root_bias(int thread, int size)
{
	int i;
	sqr_t s;
	for (i = 0; i < PO_BSIZE; ++i) {
		uct_root_bias[thread][i] = 0.0f;
	}
	for (s = 0; s < boardsquare; ++s) {
		if (stratreasons[s] == EOL) {
			uct_root_bias[thread][po_to_po(s, size)] = UCT_ROOT_BIAS_NO_MFGO;
		}
	}
}

/* gather data from all other MPI ranks 
*/
void uct_playout_merge(int thread, struct uct_parent *uct_root, int using_mpi, unsigned int playouts)
{


#if USE_MPI
#ifdef MPIDEBUGOUTPUT
	char buf[200];
#endif
	/* merge data fronm other nodes */
	if (thread == 0 && using_mpi) {
#ifdef MPIDEBUGOUTPUT
		sprintf(buf, "%d playouts, calling mpi_merge_search", playouts);
		outerr(buf);
#endif
		uct_mpi_merge_search(uct_root);
	}
#endif
}

/* return TRUE if time is done 
*/
int uct_playout_time_check(int thread, unsigned int playouts, unsigned int start_time, unsigned int target_time, struct uct_parent *uct_root, int wins)
{
	unsigned int best, second;
	unsigned int time_used = GetTickCount() - start_time;

	if (playouts >= uct_max_playouts || thinking() || !uct_searching[thread]) {
		return TRUE;
	}
#if UCT_TIMED
	if (time_used > target_time) {
		return TRUE;
	}
	/* stop really early if result is pretty certain */
	if (playouts >= UCT_MIN_PLAYOUTS && time_used > target_time / 4 &&( wins * 100 / playouts >= 95 || wins * 100 / playouts <= 5)) {
		return TRUE;
	}
	best = uct_best(uct_root);
	if (playouts >= UCT_MIN_PLAYOUTS * 2 && uct_root->child[best].visits == UCT_LEAF && uct_root->child[best].win_rate >= 0.9f) {
		return TRUE;
	}
	if (playouts >= UCT_MIN_PLAYOUTS && time_used > target_time / 2) {
		/* stop early if can't grow the tree */
		if (uct_free_node_count[thread] == 0) {
			return TRUE;
		}
		second = uct_next_best(uct_root, uct_root->child[best].visits, best);
		/* even if all playouts go to another move, can't change the result, so stop early */
		if (second != UCT_MAX_CHILDREN && uct_root->child[best].visits >= uct_root->child[second].visits + playouts * (target_time - time_used) / time_used) {
			return TRUE;
		}
	}
#endif
	return FALSE;
}

struct comp_params {
	int rules;
	int handicap;
	unsigned int target_time;
	int thread;
	struct po_board *base;
	int mnum;
	int using_mpi;
	int show_lookahead;
};


/* return the pv in pv and its depth */
static int uct_get_pv(struct po_board *uct_base, struct uct_parent *uct_root, sqr_t *pv, double *win_rate, int *visits)
{
	int next = 0;
	struct po_board *uct_working;
	struct uct_parent *node, *tmp;
	unsigned int index, best;

	*win_rate = 0.5;
	uct_working = po_alloc(uct_komi, uct_size, uct_base->seed);
	po_copy(uct_working, uct_base);
	while (next < UCT_MAX_DEPTH) {
		index = ((unsigned int)uct_working->hash[uct_working->mnum]) & UCT_HASH_MASK;
		node = NULL;
		for (tmp = uct_hash_table[0][index]; tmp != NULL; tmp = tmp->next) {
			if (tmp->hash == uct_working->hash[uct_working->mnum] && tmp->tm == uct_working->tm && tmp->ko == uct_working->ko) {
				node = tmp;
				break;
			}
		}
		if (!node || node->num_moves == 0) {
			break;
		}
		best = uct_best(node);
//		if (node->child[best].visits == UCT_LEAF)
//			break;
		if (next == 0) {
			*win_rate = (double)node->child[best].wins / node->child[best].visits;
			*visits = node->visits;
		}
		po_make_uct_move(uct_working, node->move[best], uct_working->tm);
		pv[next] = po_from_po(node->move[best], uct_size);
		if (pv[next] == PO_PASS) {
			break;
		}
		next++;
	}
	po_free(uct_working);
	return next;
}

#ifdef SHOW_LOOKAHEAD

extern void sendlookahead(sqr_t *, int);
extern void clearerror(void);
extern void flusherror(void);

#endif

/*
 * entry point for a thread to do some searching
 */
static void uct_do_compmove(void *param)
{
	int i;
#ifdef SHOW_LOOKAHEAD
	sqr_t pv[UCT_MAX_DEPTH];  /* pv being displayed */
	int next_pv = 0;
#endif
	unsigned int start_time = GetTickCount();
	int wins = 0, win;
#ifdef G2DEBUGOUTPUT
	int posgf[2] = { FALSE, FALSE };
#endif
	struct comp_params *p = (struct comp_params *)param;
	int handicap = p->handicap;
	int rules = p->rules;
	unsigned int target_time = p->target_time;
	int thread = p->thread;
	struct po_board *uct_base = p->base;
	int using_mpi = p->using_mpi;
	int show_lookahead = p->show_lookahead;
#ifdef G2DEBUGOUTPUT
	char buf[1000];
#endif
#ifdef SHOW_LOOKAHEAD
	char sbuf[200];
	double win_rate;
	int visits;
#endif

	/* each thread gets its own working board */
	struct po_board *uct_working;
	unsigned int playouts = 0;
	int total_moves = 0;
	struct uct_parent *uct_root;
	unsigned int index;
	struct uct_parent *tmp;
	int time_over = FALSE;
	unsigned int uct_po_per_node;
	int time_check_playouts = 0;

	if (thread != 0) {
		show_lookahead = FALSE;
	}
#ifdef WIN32
	SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
#endif

	/* find the root node (probably exists due to pondering) */
	uct_root = NULL;
	index = ((unsigned int)uct_base->hash[uct_base->mnum]) & UCT_HASH_MASK;
	for (tmp = uct_hash_table[thread][index]; tmp != NULL; tmp = tmp->next) {
		if (tmp->hash == uct_base->hash[uct_base->mnum] && tmp->tm == uct_base->tm && tmp->ko == uct_base->ko) {
			uct_root = tmp;
			break;
		}
	}
	if (!uct_root) {
		/* generate moves at root */
		uct_root = uct_create_node(uct_base, thread, rules);

		/* if only one move generated, just play it */
		if (uct_root->num_moves == 1) {
			assert(uct_root->num_children == 1);
			uct_root->child[0].visits++;
			uct_root->visits++;
#ifdef WIN32
			SetEvent(uct_search_complete[thread]);
#endif
			return;
		}
	}

#if UCT_PRIOR_MFGO
	/* make sure root node always has Many Faces' suggestions 
	 * must always do prior to check for pass ends game and set root bias
	 */
	UCT_LOCK(uct_mfgo_lock);
	if (uct_prior_mfgo(uct_base, uct_root, handicap, rules, thread) == 0) {
		/* game over, play a pass */
		UCT_UNLOCK(uct_mfgo_lock);
#ifdef WIN32
		SetEvent(uct_search_complete[thread]);
#endif
		return;
	}
	if (uct_size != 9) {
		uct_set_root_bias(thread, uct_size);
	}
	UCT_UNLOCK(uct_mfgo_lock);
#endif	

#if USE_MPI && !UCT_MPI_ALLREDUCE
	if (using_mpi) {
		uct_mpi_eat();
	}
#endif

	uct_working = po_alloc(uct_komi, uct_size, uct_base->seed + p->thread * 3141596);
	/* do playouts for this search */
	for (;;) {
		if (uct_size == 9) {
			uct_po_per_node = UCT_PO_PER_NODE_9x9;
		} else {
			uct_po_per_node = UCT_PO_PER_NODE;
		}

		/* don't grow the tree as agressively if it gets large */
		if (uct_alloc_fails[thread]) {
			uct_po_per_node *= 8;
		}
		else if (uct_alloc_collect[thread]) {
			uct_po_per_node *= 4;
		}
		else if (uct_free_node_count[thread] < uct_max_node / 4) {
			uct_po_per_node *= 2;
		}
		for (i = 0; i < UCT_PLAYOUT_BATCH; i++) {
			po_copy(uct_working, uct_base);
#if UCT_LEARN
			assert(msptr == uct_base->mnum);
			learn_put_hash(uct_base->mnum, learn_hash);	/* horribly not thread-safe */
#endif
			win = !uct_playout(uct_working, 0, handicap, rules, thread, uct_po_per_node);
			wins += win;
#ifdef UCTDEBUGOUTPUT
			total_moves += uct_working->mnum - uct_base->mnum;
#endif
		}
		playouts += UCT_PLAYOUT_BATCH;

#ifdef SHOW_LOOKAHEAD
		if (show_lookahead && thread == 0) {
			next_pv = uct_get_pv(uct_base, uct_root, pv, &win_rate, &visits);
			if (show_lookahead & LOOKAHEADMOVES) {
				sendlookahead(pv, next_pv);
			}
			if (show_lookahead & LOOKAHEADSTATS) {
				sprintf(sbuf, "%d trial games: Win: %2.1f%%\n", visits, 100. * win_rate);
				clearerror();
				outerr(sbuf);
				flusherror();
			}
		}
#endif

		/* merge data from other threads */
		if (uct_num_threads > 1) {
			uct_merge_search(thread, uct_root);
		}
		time_check_playouts += UCT_PLAYOUT_BATCH;

		if (time_check_playouts < UCT_PLAYOUT_TIME_CHECK) {
			continue;
		}
		time_check_playouts -= UCT_PLAYOUT_TIME_CHECK;

		/* merge data from other threads and nodes */
		uct_playout_merge(thread, uct_root, using_mpi, playouts);



#ifdef G2DEBUGOUTPUT
		/* save playouts for analysis */
		if (thread == 0 && playouts >= UCT_MIN_PLAYOUTS) {
			if (win) {
				sprintf(buf, "c:\\go\\uctplayoutwin%d.sgf", playouts);
				po_save_sgf(uct_working, handicap, buf);
			} else {
				sprintf(buf, "c:\\go\\uctplayoutlose%d.sgf", playouts);
				po_save_sgf(uct_working, handicap, buf);
			}
		}
#endif

		time_over = uct_playout_time_check(thread, playouts, start_time, target_time, uct_root, wins);

		if (thread != 0 && time_over) {
			break;
		}
		if (thread == 0) {
#if USE_MPI && UCT_MPI_ALLREDUCE
			MPI_Bcast(&time_over, 1, MPI_INT, 0, MPI_COMM_WORLD);	/* MPI must all stop together to keep comm synchronized */
#endif
			if (time_over) {
				break;
			}
		}
	}

#ifdef SHOW_LOOKAHEAD

	/* remove any lookahead */
	if (show_lookahead & LOOKAHEADMOVES) {
		sendlookahead(pv, 0);
	}
#endif

#if USE_MPI && !UCT_MPI_ALLREDUCE
	/* merge data fronm other nodes */
	if (thread == 0 && using_mpi) {
		uct_mpi_eat();
	}
#endif
	uct_thread_playouts[thread] += playouts;
	UCT_LOCK(uct_lock);
	uct_playouts[uct_mnum] += playouts;
	uct_num_moves += total_moves;
	UCT_UNLOCK(uct_lock);

	po_free(uct_working);
	uct_working = 0;

#ifdef WIN32
	SetEvent(uct_search_complete[thread]);
#endif
}


/*
 * single threaded garbage collection
 */
static void uct_garbage_collect(unsigned int mnum)
{
	int i;
	struct uct_parent **p, *tmp;
	int t = 0;
	/* free up nodes that are ld or small visits */
	for (t = 0; t < uct_num_threads; ++t) {
		for (i = 0; i < UCT_HASH_SIZE; ++i) {
			p = &uct_hash_table[t][i];
			while (*p) {
				if ((*p)->visits < UCT_REUSE_VISITS || (*p)->mnum <= mnum) {
					if ((*p)->visits <= UCT_GC_VISITS) {
						uct_collectable[t]--;
					}
					tmp = *p;
					*p = (*p)->next;
					tmp->next = uct_free_nodes[t];
					assert(tmp->used);
					tmp->used = FALSE;
					uct_free_nodes[t] = tmp;
					uct_free_node_count[t]++;
				} else {
					p = &((*p)->next);
				}
			}
		}
	}

	/* after nodes are freed, clear out unused uct_shared_nodes values */
	for (t = 0; t < uct_num_threads; ++t) {
		for (i = 0; i < uct_next_shared[t]; ++i) {
			if (!uct_shared_nodes[t][i]->used) {
				uct_shared_nodes[t][i] = uct_shared_nodes[t][uct_next_shared[t] - 1];
				uct_next_shared[t]--;
				i--;
			}
		}
	}
}


/*
 * ponder
 */
static void uct_do_ponder(void *param)
{
	struct comp_params *p = (struct comp_params *)param;
	struct po_board *uct_base = p->base;
	struct po_board *uct_working = po_alloc(uct_komi, uct_size, uct_base->seed + p->thread * 3141596);
	int max_nodes = (uct_max_node / uct_num_threads) / 2;

	while (uct_pondering && uct_free_node_count[p->thread] > max_nodes) {
			po_copy(uct_working, uct_base);
			uct_playout(uct_working, 0, p->handicap, p->rules, p->thread, UCT_PO_PER_NODE);
	}

	po_free(uct_working);

#ifdef WIN32
	SetEvent(uct_search_complete[p->thread]);
#endif
}

/*
 * free unused nodes and do some pondering
 */
static void uct_ponder(void *param)
{
	struct comp_params *p = (struct comp_params *)param;
	int mnum = p->mnum;
#ifdef WIN32
#if UCT_ENABLE_PONDER
	struct comp_params pp[UCT_MAX_THREADS];
	int i;
	uintptr_t thread_handle;
#endif
#endif

	uct_garbage_collect(mnum);
#ifdef WIN32
#if UCT_ENABLE_PONDER
	for (i = 0; i < uct_num_threads; ++i) {
		pp[i].handicap = p->handicap;
		pp[i].rules = p->rules;
		pp[i].target_time = p->target_time;
		pp[i].thread = i;
		pp[i].base = uct_base;
		uct_searching[i] = TRUE;
		thread_handle = _beginthread(uct_do_ponder, 0, &pp[i]);
		assert(thread_handle != -1L);
	}
	for (i = 0; i < uct_num_threads; ++i) {
		WaitForSingleObject(uct_search_complete[i], INFINITE);
		uct_searching[i] = FALSE;
	}
#endif

	SetEvent(uct_ponder_complete);
#endif
}

/* find the best move among all threads and all nodes.
 * called after all threads have completed searching
 * return the best move
 */
static int uct_merge_best(struct po_board *uct_base, int using_mpi, float *win_rate)
{
	int i;
	int best = 0;	/* best index into moves array */
	unsigned int best_visits = 0;
	struct uct_parent *uct_root;
	unsigned int index;
	struct uct_parent *tmp;

	/* find the root node for thread 0 */
	uct_root = NULL;
	index = ((unsigned int)uct_base->hash[uct_base->mnum]) & UCT_HASH_MASK;
	for (tmp = uct_hash_table[0][index]; tmp != NULL; tmp = tmp->next) {
		if (tmp->hash == uct_base->hash[uct_base->mnum] && tmp->tm == uct_base->tm && tmp->ko == uct_base->ko) {
			uct_root = tmp;
			break;
		}
	}
	assert(uct_root);
	if (!uct_root) {
		*win_rate = 0.5f;
		return PO_PASS;
	}
	for (i = 0;  i < uct_root->num_children; ++i) {
		if (uct_root->child[i].visits >= UCT_LEAF && uct_root->child[i].win_rate < 0.1)
			continue;
//		if (uct_root->child[i].visits > best_visits) {  using most wins is a little better than using most visits
		if (uct_root->child[i].wins > best_visits) {
			best = i;
//			best_visits = uct_root->child[best].visits;
			best_visits = uct_root->child[best].wins;
		}
	}

	*win_rate = uct_root->child[best].win_rate;
	return uct_root->move[best];
}

static struct comp_params p[UCT_MAX_THREADS];


#if USE_MPI
static struct uct_search_state all_us[MPI_MAX_NODES];
sqr_t uct_mpi_gather(sqr_t s, struct uct_parent *uct_root) 
{
	struct uct_search_state us;	/* for final results gather */
	unsigned int visit_counts[PO_BSIZE];
	int i, m;
	char buf2[20], buf3[20];
	int best_move;
	unsigned int best_val;
#ifdef UCTDEBUGOUTPUT
	char buf[600];
#endif

	/* combine all of the results from all of the ranks */
	us.num_children = uct_root->num_children;
	for (i = 0; i < uct_root->num_children; ++i) {
		us.move[i] = uct_root->move[i];
		us.visits[i] = uct_root->child[i].visits;
		us.wins[i] = uct_root->child[i].wins;
	}
	MPI_Gather(&us, sizeof(struct uct_search_state), MPI_CHAR, all_us, sizeof(struct uct_search_state), MPI_CHAR, 0, MPI_COMM_WORLD);
	if (uct_my_rank == 0) {
		for (i = 0; i < PO_BSIZE; ++i) {
			visit_counts[i] = 0;
		}
		for (i = 0; i < uct_num_nodes; ++i) {
			for (m = 0; m < all_us[i].num_children; ++m) {	
				if (all_us[i].visits[m] != UCT_LEAF) {
					visit_counts[all_us[i].move[m]] += all_us[i].visits[m];
				}
			}
		}
		best_val = 0;
		best_move = s;
		for (i = 0; i < PO_BSIZE; ++i) {
			if (visit_counts[i] > best_val) {
				best_val = visit_counts[i];
				best_move = i;
			}
		}
#ifdef UCTDEBUGOUTPUT
		sprintf(buf, "UCT gather from %d nodes changes move from %s to %s with %d visits", uct_num_nodes,
			ssqr(po_from_po(s, uct_size), buf2),
			ssqr(po_from_po(best_move, uct_size), buf3), best_val);
		outerr(buf);
#endif
		s = best_move;
	}
	return s;
}
#endif

/*
 * entry point to get a computer move
 */
sqr_t uct_compmove(int color, int handicap, int rules, unsigned int time_left, unsigned int stones_left, int show_lookahead, int using_mpi, int ranks_per_group, float *win_rate)
{
	unsigned int target_time;
	int stones;
	sqr_t s;
	unsigned int start_time = GetTickCount();
	struct uct_parent *uct_root;
	unsigned int index;
	struct uct_parent *tmp;
#ifdef UCTDEBUGOUTPUT
	char buf[600];
	sqr_t sold;
#endif
	int num_threads = uct_num_threads;	/* how many threads to schedule */
	struct po_board *uct_base;
	int i;
#ifdef WIN32
	uintptr_t thread_handle;
#endif

	uct_init_data();

#if USE_MPI
	if (using_mpi) {
		MPI_Comm_size(MPI_COMM_WORLD, &uct_num_nodes);  // number of nodes involved in run
		MPI_Comm_rank(MPI_COMM_WORLD, &uct_my_rank);    // my process id: 0 <= myRank < numProcs
		uct_group_size = ranks_per_group;
		if (uct_group_size <= 1) {
			uct_group_size = uct_num_nodes;
		}
#ifdef UCTDEBUGOUTPUT
		sprintf(buf, "my_rank %d, nodes %d, group size %d, uct_pair %d\n", uct_my_rank, uct_num_nodes, uct_group_size, uct_pair);
		outerr(buf);
#endif

#if !UCT_MPI_ALLREDUCE
		if (!uct_mpi_attached) {
			if (MPI_Buffer_attach(uct_mpi_buffer, UCT_MPI_BUFFER_SIZE) != MPI_SUCCESS) {
				outerr("MPI_Buffer_attach failed, search aborted");
				return PASS;
			}
			uct_mpi_attached = TRUE;
		}
		uct_pair = 1;
		while (uct_pair < uct_group_size) {
			uct_pair <<= 1;
		}
		uct_pair >>= 1;	/* highest power of 2 lower than total nodes */
#endif

	}
#endif

	/* how much time to use in this search, millisecoonds */
	if (stones_left > 0) {
		/* in overtime */
		target_time = (time_left * 7 / 8) / stones_left;
	} else {
		stones = uct_size * uct_size / 6;						/* lots of time in the opening, and will get extra time for early exits, especially late in search */
		target_time = time_left / stones;						/* nominal time left per move */
/*		target_time = target_time * 9 / 8;						 since often stop early, and want to spend more time in the opening and less in endgame */
		if (uct_size > 9 && uct_mnum < 16) {						/* play first few moves faster on big boards */
			target_time  = target_time * (uct_mnum + 2) / (16 + 2);
		}
		if (uct_size == 9 && uct_mnum == 0) {
			target_time /= 4;
		}
	}
	if (target_time > 3000)
		target_time -= 1000;	/* allow time for communication */
	if (target_time < 3000) {
		target_time = target_time * 3 / 4;
	}
	uct_time_limit[uct_mnum] = target_time;

#ifdef UCTDEBUGOUTPUT
	sprintf(buf, "Start search move %d using %d threads, %d ms, with %d+%d free (%d+%d used) nodes, %d+%d mfgo used nodes, %d po/thread share, %d po/MPI share\n", msptr + 1, 
		num_threads, target_time, uct_free_node_count[0], uct_free_node_count[1], uct_max_node - uct_free_node_count[0], uct_max_node - uct_free_node_count[1], uct_next_shared[0], uct_next_shared[1], UCT_PLAYOUT_BATCH, UCT_PLAYOUT_TIME_CHECK);
	outerr(buf);
#endif

#ifdef WIN32
	/* stop pondering */
	if (uct_pondering) {
		uct_pondering = FALSE;
		WaitForSingleObject(uct_ponder_complete, INFINITE);
	}
#else
	uct_garbage_collect(uct_mnum - 1);
#endif

	fixplaylevel(UCTLEVEL);

	/* uct_base is never modified after this point - it is read only to all compute threads */
	uct_base = uct_init_search(handicap, color);

	for (i = 0; i < num_threads; ++i) {
		p[i].handicap = handicap;
		p[i].rules = rules;
		p[i].target_time = target_time;
		p[i].thread = i;
		p[i].base = uct_base;
		p[i].using_mpi = using_mpi;
		p[i].show_lookahead = show_lookahead;
		uct_searching[i] = TRUE;
#ifdef WIN32
		thread_handle = _beginthread(uct_do_compmove, 0, &p[i]);
		assert(thread_handle != -1L);
#else
		uct_do_compmove(&p[i]);
#endif
	}

	for (i = 0; i < num_threads; ++i) {
#ifdef WIN32
		WaitForSingleObject(uct_search_complete[i], INFINITE);
#endif
		uct_searching[i] = FALSE;
	}

	/* find the root node. */
	uct_root = NULL;
	index = ((unsigned int)uct_base->hash[uct_base->mnum]) & UCT_HASH_MASK;
	for (tmp = uct_hash_table[0][index]; tmp != NULL; tmp = tmp->next) {
		if (tmp->hash == uct_base->hash[uct_base->mnum] && tmp->tm == uct_base->tm && tmp->ko == uct_base->ko) {
			uct_root = tmp;
			break;
		}
	}
	assert(uct_root);

	/* gather the final data back to thread 0 from the other threads. */
	uct_merge_receive(0, uct_root);

	/* TODO: for MPI send out the final data, then a compete marker, then read all inputs blocking, until all complete markers arrive
	 * it will work without this since the missing data will be delivered to the next search, but completing comms for each search before returning is better
	 */

	s = uct_merge_best(uct_base, using_mpi, win_rate);


#if USE_MPI
	if (using_mpi) {
		s = uct_mpi_gather(s, uct_root);
	}
#endif

#ifdef UCTDEBUGOUTPUT
	sold = uct_root->move[uct_best(uct_root)];
	if (s != sold) {
		sprintf(buf, "2 threads found different move %d %d\n", s, sold);
		outerr(buf);
	}
#endif
	uct_time_used[uct_mnum] = GetTickCount() - start_time;
	if (uct_root->visits > 1) {
		uct_dump_stats(uct_root, time_left, stones_left, handicap, rules, show_lookahead, color);
	}

	po_free(uct_base);
#ifdef WIN32
	/* start the pondering thread to do garbage collection and some pondering */
	p[0].handicap = handicap;
	p[0].rules = rules;
	p[0].mnum = uct_mnum;
	uct_pondering = TRUE;
	thread_handle = _beginthread(uct_ponder, 0, &p[0]);
	assert(thread_handle != -1L);
#endif
	return po_from_po(s, uct_size);
}

#ifdef G2DEBUGOUTPUT
static char *po_reason[PO_REASON_COUNT] = {
	"Game",
	"UCT",
	"PO capture last",
	"PO random",
	"PO save adjacent",
	"PO pass",
	"PO pattern",
	"PO last move pattern",
	"PO capture any group",
	"PO nakade self atari",
	"PO attack last group",
	"PO nakade key point",
	"PO peep",
	"PO ladder",
	"PO save2 adjacent",
	"PO attack2 adjacent",
};
#endif

/* 
 * save information from one playout into a file for debug
 */
#ifdef G2DEBUGOUTPUT
void po_save_sgf(struct po_board *b, int handicap, char *name)
{
	int i, j, ch;
	sqr_t s;
	char mv[10];
	struct po_board brd;
	struct uct_parent *node = NULL, *tmp;
	unsigned int index;
	FILE *f = fopen(name, "w");
	if (f == NULL) {
		return;
	}

	po_init(&brd, 0, b->komi, b->boardsize, 12345678);
	
	fprintf(f, "(;GM[1]FF[4]CA[UTF-8]AP[gtpmfgo]PW[mfgoMC]PB[mfgoMC]SZ[%d]HA[%d]KM[%d.5]\n", b->boardsize, handicap, b->komi);
	for (i = 0; i < b->mnum; ++i) {
		s = po_from_po(b->moves[i] >> 1, b->boardsize);
		if (s == PASS) {
			fprintf(f, ";%c[tt]", (b->moves[i] & 1) ? 'W' : 'B');
		} else { 
			fprintf(f, ";%c[%c%c]", (b->moves[i] & 1) ? 'W' : 'B', 'a' + s % (b->boardsize), 'a' + s / (b->boardsize));
		}
		if (b->reason[i] == PO_REASON_UCT) {

			/* find the node */
			index = ((unsigned int)brd.hash[brd.mnum]) & UCT_HASH_MASK;
			for (tmp = uct_hash_table[0][index]; tmp != NULL; tmp = tmp->next) {
				if (tmp->hash == brd.hash[brd.mnum] && tmp->tm == brd.tm && tmp->ko == brd.ko) {
					node = tmp;
					break;
				}
			}
			if (!node) {
				fprintf(f, "C[Used %d of %d ms, %s, can't find node]\n", uct_time_used[i], uct_time_limit[i], po_reason[b->reason[i]]);
			} else {
				fprintf(f, "C[Used %d of %d ms, %s, %d parent visits.  ", 
					uct_time_used[i], uct_time_limit[i], po_reason[b->reason[i]], node->visits);
				ch = 0;
				for (j = 0; j < node->num_children; j++) {
					if (node->move[j] == b->moves[i] >> 1) {
						fprintf(f, "This move visits %d, win %4.2f",
							node->child[j].visits, node->child[j].win_rate);
						fprintf(f, ", rave visits %d, rave win %4.2f, prior %4.2f, prior_mfgo %4.2f\n", 
							node->rave_plays[j], (float)node->rave_wins[j] / (float)node->rave_plays[j], node->rave_prior[j] / 256., node->prior_mfgo[j] / 256.);
						ch = j;
						break;
					}
				}
				for (j = 0; j < node->num_children; j++) {
					ssqr(po_from_po(node->move[j], uct_size), mv);
					fprintf(f, "%s: visits %d, win %4.2f", 
						mv, node->child[j].visits, node->child[j].win_rate);
					fprintf(f, ", rave visits %d, win %4.2f, prior %4.2f, prior_mfgo %4.2f\n", 
						node->rave_plays[j], (float)node->rave_wins[j] / node->rave_plays[j], 
						node->rave_prior[j] / 256., node->prior_mfgo[j] / 256.);
				}
				fprintf(f, "Untried moves:\n");
				for (j = node->num_children; j < node->num_moves; j++) {
					ssqr(po_from_po(node->move[j], uct_size), mv);
					fprintf(f, "%s: rave visits %d, win %4.2f, prior %4.2f, mfgo_prior %4.2f\n", 
						mv, node->rave_plays[j], (float)node->rave_wins[j] / node->rave_plays[j], node->rave_prior[j] / 256., node->prior_mfgo[j] / 256.);
				}
				fprintf(f, "]\n");
			}

		} else {
			fprintf(f, "C[%s]\n", po_reason[b->reason[i]]);
		}
		po_make_uct_move(&brd, b->moves[i] >> 1, b->moves[i] & 1);
	}
	fprintf(f, ")\n");
	fclose(f);
}
#endif

static void uct_dump_stats(struct uct_parent *uct_root, int time_left, int stones_left, int handicap, int rules, int show_lookahead, int color)
{
	char buf[2000];
	int s2;
	int t, i, depth;
	struct uct_parent *node = 0, *last_node;
	struct uct_parent *tmp;
	char mv[10];
	int pass;
	unsigned int best, b;
	struct uct_child *ch;
	struct po_board *uct_working;

#ifndef UCTDEBUGOUTPUT
	if (!show_lookahead & LOOKAHEADSTATS) {
		return;
	}
#endif

#ifdef UCTDEBUGOUTPUT
	sprintf(buf, "%d (%d %d %d %d) playouts, %5.1f move/po, %d+%d of %d free nodes, %d+%d+%d+%d allocs, %d+%d+%d+%d GC, %d+%d+%d+%d collectable, %d+%d+%d+%d failed allocs, %d visited children, %d used children.\n", 
		uct_playouts[uct_mnum], uct_thread_playouts[0], uct_thread_playouts[1], uct_thread_playouts[2], uct_thread_playouts[3], 
		(double)uct_num_moves / uct_playouts[uct_mnum], uct_free_node_count[0], uct_free_node_count[1], uct_max_node, 
		uct_alloc_count[0], uct_alloc_count[1], uct_alloc_count[2], uct_alloc_count[3],
		uct_alloc_collect[0], uct_alloc_collect[1], uct_alloc_collect[2], uct_alloc_collect[3],
		uct_collectable[0], uct_collectable[1], uct_collectable[2], uct_collectable[3],
		uct_alloc_fails[0], uct_alloc_fails[1], uct_alloc_fails[2], uct_alloc_fails[3], 
		uct_visited_children, uct_used_children);
	outerr(buf);
	sprintf(buf, "%d ms (target %d ms [%d msec time for %d stones]), %5.1fK po/sec, %d max depth, %d (%3.0f/sec) mfgo\n", 
		uct_time_used[uct_mnum], uct_time_limit[uct_mnum], time_left, stones_left, (double)uct_playouts[uct_mnum] / uct_time_used[uct_mnum], uct_max_depth_seen, 
		uct_mfgo_calls, 1000. * uct_mfgo_calls / uct_time_used[uct_mnum]);
	outerr(buf);
	sprintf(buf, "Thread: %d+%d+%d+%d (of %d)shared, %d+%d+%d+%d send, %d+%d+%d+%d recv, %d+%d+%d+%d alloced, %d+%d+%d+%d dropped, free states %d of %d, min free states %d, state alloc fails %d\n",
		uct_next_shared[0], uct_next_shared[1], uct_next_shared[2], uct_next_shared[3],UCT_MAX_SHARED_NODE,
		uct_nodes_sent[0], uct_nodes_sent[1], uct_nodes_sent[2], uct_nodes_sent[3],
		uct_nodes_received[0], uct_nodes_received[1], uct_nodes_received[2], uct_nodes_received[3],
		uct_nodes_alloced[0], uct_nodes_alloced[1], uct_nodes_alloced[2], uct_nodes_alloced[3],
		uct_nodes_dropped[0], uct_nodes_dropped[1], uct_nodes_dropped[2], uct_nodes_dropped[3],
		uct_state_free_node_count, uct_max_state_node, uct_state_min_free, uct_state_fail_count);
	outerr(buf);
#if USE_MPI
	sprintf(buf, "MPI: %d send (%d msg), %d recv, %d forward (%d msg), %d max msg per send, %d max msg per recv, %d msg dropped \n", 
		uct_mpi_send_count, uct_mpi_send_msg_count, uct_mpi_recv_count, uct_mpi_fwd_count, uct_mpi_fwd_msg_count, uct_mpi_max_message_count, uct_mpi_max_recv_count, uct_mpi_recv_missing);
	outerr(buf);
#endif
#else 
	outerr("Board shows winning percentages for popular moves.\n");
	sprintf(buf, "Analyzed %d full games in %4.1f seconds, %4.0f games/second.\n",
		uct_root->visits, uct_time_used[uct_mnum]/ 1000., uct_playouts[uct_mnum] * 1000. / uct_time_used[uct_mnum]); 
	outerr(buf);
#endif

	for (t = 0; t < uct_root->num_children; t++) {
		s2 = po_from_po(uct_root->move[t], uct_size);
		if (s2 == PASS) {
#ifdef G2DEBUGOUTPUT
			sprintf(buf, "PASS move: %4.1f%% win, %d visits\n", 100. * uct_root->child[t].win_rate, uct_root->child[t].visits);
			outerr(buf);
#endif
		} else {
			sprintf(buf, "%d", (int)(0.5 + 100. * uct_root->child[t].win_rate));
			outstone(s2, buf);
		}
	}

	outerr("\n");
	outerr("Principal variation:\n");
	uct_working = uct_init_search(handicap, color);
	pass = FALSE;
	depth = 0;
	for (t = 0; t < UCT_MAX_DEPTH; ++t) {
		/* find the node */
		unsigned int index = ((unsigned int)uct_working->hash[uct_working->mnum]) & UCT_HASH_MASK;
		last_node = node;
		node = NULL;
		for (tmp = uct_hash_table[0][index]; tmp != NULL; tmp = tmp->next) {
			if (tmp->hash == uct_working->hash[uct_working->mnum] && tmp->tm == uct_working->tm && tmp->ko == uct_working->ko) {
				node = tmp;
				break;
			}
		}
		if (!node) {
			for (tmp = uct_hash_table[1][index]; tmp != NULL; tmp = tmp->next) {
				if (tmp->hash == uct_working->hash[uct_working->mnum] && tmp->tm == uct_working->tm && tmp->ko == uct_working->ko) {
					node = tmp;
					break;
				}
			}
		}
		if (!node || node->num_moves == 0) {
#ifdef UCTDEBUGOUTPUT
			if (!node) {
				outerr("Can't find position in hash table.\n");
			}
#endif
			sprintf(buf, "End of search, %d ply PV.\n\n", depth);
			outerr(buf);
			break;
		}

#ifdef UCTDEBUGOUTPUT
		for (i = 0; i < node->num_children; ++i) {
			if (node->move[i] == PO_PASS) {
				sprintf(buf, "PASS: %4.1f%% win, %d visits, %4.1f%% rave, %d rave visits, %3.0f prior mfgo.\n", 100. * node->child[i].win_rate, node->child[i].visits, 
					100. * node->rave_wins[i] / (float)node->rave_plays[i], node->rave_plays[i], 100. * node->prior_mfgo[i] / 256);
				outerr(buf);
			}
		}
#endif
		
		depth++;
		best = uct_best(node);
		b = best;
		for (i = 0; i < node->num_children; ++i) {
			if (b == UCT_MAX_CHILDREN || node->child[b].visits == UCT_LEAF)
				break;
			ssqr(po_from_po(node->move[b], uct_size), mv);
			ch = &node->child[b];
#ifdef UCTDEBUGOUTPUT
			sprintf(buf, "%s%c %s: %4.1f%% win %d/%d(%2.0f%%) po %4.1f%% of %d rave %3.0f%% prior %3.0f%% prior_mfgo %d/%d\n", i == 0? " ": "    ",
				node->tm?'w':'b', mv, ch->win_rate * 100., 
				ch->visits, node->visits, 100. * ch->visits / node->visits, 100. * node->rave_wins[b] / (float)node->rave_plays[b], node->rave_plays[b], 
				100. * node->rave_prior[b] / 256, 100. * node->prior_mfgo[b] / 256, node->num_children, node->num_moves); 
			outerr(buf);
#else
			sprintf(buf, "%s%c %s: Tried %d full games, and won %4.1f%%.\n", i == 0? " ": "    ",
				node->tm?'w':'b', mv, ch->visits, ch->win_rate * 100.); 
			outerr(buf);
			break;
#endif
			if (ch->visits < 0.05 * node->visits) {
				break;
			}
			b = uct_next_best(node, ch->visits, b);
		}
		if (pass && node->move[best] == PO_PASS) {
			outerr("Two passes, game over.\n");
			break;	// 2 passes in a row
		}
		po_make_uct_move(uct_working, node->move[best], uct_working->tm);
		pass = node->move[best] == PO_PASS;
	}
	po_free(uct_working);
	uct_working = 0;
}

void po_test_generate(int handicap, int color)
{
	int i;
	sqr_t s;
	char buf[20];
	int made[PO_BSIZE];
	struct po_board *uct_base;

	uct_base = uct_init_search(handicap, color);
	uct_base->seed = (int)time(NULL);

	for (i = 0; i < PO_BSIZE; ++i) {
		made[i] = 0;
	}
	for (i = 0; i < 10000; ++i) {
		made[po_genmove(uct_base)]++;
	}
	for (i = 0; i < PO_BSIZE; ++i) {
		if (made[i] == 0) continue;
		s = po_from_po(i, uct_size);
		sprintf(buf, "%d", (made[i] + 5) / 10);
		outstone(s, buf);
	}
	outerr("plays per thousand genmoves\n");
	po_free(uct_base);
}

void uct_show_prior(int handicap, int color, int rules, int mfgo)
{
#ifdef G2DEBUGOUTPUT
	int i;
	sqr_t s;
	char buf[20];
	struct po_board *uct_base;
	struct uct_parent *uct_root;
	uct_base = uct_init_search(handicap, color);
	uct_root = uct_create_node(uct_base, 0, rules);
	if (mfgo)
		uct_prior_mfgo(uct_base, uct_root, handicap, rules, 0);
	for (i = 0; i < uct_root->num_moves; ++i) {
		s = po_from_po(uct_root->move[i], uct_size);
		if (mfgo)
			sprintf(buf, "%d", 100 * uct_root->prior_mfgo[i] / 256);
		else
			sprintf(buf, "%d", 100 * uct_root->rave_prior[i] / 256);
		outstone(s, buf);
	}
	po_free(uct_base);
#endif
}
