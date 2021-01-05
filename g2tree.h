
# ifndef TREE_INCLUDED
# define TREE_INCLUDED
 
# include "g2types.h"

/* max of 64K since unsigned short indexes! */
# define MAXTREE 10000

# define NONE MAXTREE-1

# define V_LOSE 0  			/* loses fight */
# define V_LOSELIKELY 1		/* likely to lose fight */
# define V_UNKNOWNRES 2		/* too many nodes - unknown result  */
# define V_WINKO 3			/* need external ko threat to win fight */
# define V_WINLIKELY 4		/* likely to win fight */
# define V_WIN 5          	/* wins fight */ 


typedef listval_t tree_t; /* so can be stored in a list */

/* a node in the search tree.
 * represents a position on the board and the move leading to it
 * the root of the tree has no move associated with it
 */
struct tree {
	tree_t sib;				/* next sibling or NONE*/
	tree_t child;			/* child node or NONE */
	short trial;			/* which scout trial made this node */
	sqr_t  s;      			/* the move that leads to this node */
	short val;    			/* value of move (probability of success, result), from point of view of the mover */
	char genval;			/* probability generated with move */
	char evaluated;			/* TRUE if this node has been evaluated (leaf or internal) */
	char ob;				/* count of number of times this is the same as opponent best refute of another move */
	char bf;				/* best probability in an unevaluated failing node below this node 
							 * for success node, it's the best unevaluated move for opponent, since it takes
							 * one opponent move to work to change the status of this node
							 * for failure node, it's the combination of the bf for the opponent's winning move,
							 * and 100-bf for the opponent's best unevaluted move, since both must change to make me
							 * a success node.
							 */
	char kocolor;			/* which color has taken ko with threat */ 
	};


extern struct tree tr[MAXTREE];

/* Result at his node (one of V_LOSE to V_WIN */
# define TR_RESULT(x) ((tr[x].val)&0xf)
/* probability of success (0-100) at a node */
# define TR_PROB(x) (((tr[x].val)>>4)&0xff)

void initree(void);  		/* initialize tree structure */
void deletetree(tree_t parent, tree_t child);
void deletechildren(tree_t parent);
tree_t newtree(void);		/* return a new tree, init with s,val */
void freetree(tree_t t);    /* free a whole tree */
//tree_t addfirstchild(tree_t node, sqr_t s, int val); /* add a new child */
tree_t addlastchild(tree_t node, sqr_t s, int prob, int result);
/*tree_t addsortchild(tree_t node, sqr_t s, int prob, int result); */
tree_t bestchild(tree_t node, int *which);	/* return the best child move for this tree */
void setvalue(tree_t t, int prob, int result);
void settrial(tree_t t, int trial);
void setkocolor(tree_t t, int c);
void mrgsortchild(tree_t p, tree_t c);  /* make tree c a child of tree p */
int checktree(void);
int usedtreenodes(void);
int counttree(tree_t t);
int countevaltree(tree_t t);
void dumptree(tree_t, int color);
char *treesummary(tree_t t, char *buf);
int treealprob(tree_t t, int alprob, int live);
char *treeres(tree_t t, int *prob);
char *opptreeres(tree_t t, int *prob);
int treeresval(tree_t t);	/* result value for this tree (V_WIN etc) */
#endif
