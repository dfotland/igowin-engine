/*  This file contains prototypes and defines for the interface
 * to the go playing engine "g2".  It should be included in the
 * user interface source files.
 */

/* for compiling, the following defines should be set:
 *
 * For the release version:
 *   WIN95 for a PC windows 95 or higher version, to use the _fastcall convention
 *
 *   No defines are required for any other 32-bit int version
 *
 * leave out the file g2check.c
 *
 * Set the define SMALLCODE to leave out some of the user interface
 *   code not not needed just to play go
 * leave out the file g2why.c
 *
 * set ISHIFORMAT to save and restore pattern matches in ishi format
 *
 * For debugging, set the following defines:
 *   G2DEBUGOUTPUT - to allow debug commands to output internal data values
 *   G2DEBUG	   - to enable debugging code
 *   CHECK		   - to add range checking code (slows execution about 2X)
 *   TEST		   - add testing code that slows execution
 *
 * and compile with the file g2check.c
 *
 */

#include "g2types.h"

#ifndef UI2G2_INCLUDED
#define UI2G2_INCLUDED 

/* true and false constants */

#define TRUE 1
#define FALSE 0

/* 
 * levels 0 to MAXLEVEL are normal levels (0-18k, 1-15K, 2-12K, 3-9K, 4-6K, 5-4K, 6-2K kyu)
 * use UCTLEVEL to run the monte carlo player
 */
#define MAXLEVEL 5
#define UCTLEVEL 6
#define GTPLEVEL 7
#define NUMLEVELS 7

#define UCT 1
 
/* colors for moves or points on the board */

#define BLACKCOLOR 0
#define WHITECOLOR 1
#define NOCOLOR 2

/* maximum number of nodes in a supercomputer MPI run */

#define MPI_MAX_COMM 8
#define MPI_MAX_NODES (1 << MPI_MAX_COMM)

/* moves */

#define MAXBOARDSIZE 19
#define NUMPOINTS (MAXBOARDSIZE * MAXBOARDSIZE)
#define NOPOINT (unsigned)(MAXBOARDSIZE * MAXBOARDSIZE)
#define PASS (unsigned)(MAXBOARDSIZE * MAXBOARDSIZE + 1)

/* page numbers for strings */

#define WHYPAGE 24
#define GOPAGE 25
#define ERRPAGE 26

/* status returns */

#define G2OK 0				/* return OK */
#define G2FAIL 1			/* failure */
#define G2BADFILEOPEN 2	/* error in file open */
#define G2BADFILEACC 3		/* error in file access */
#define G2OCCUPIED 4		/* move illegal - stone there already */
#define G2SUICIDE 5		/* move illegal suicide */
#define G2KO 6				/* move illegal ko capture */
#define G2NOROOM 7		/* out of room in data structures */
#define G2SUPERKO 8		/* repeat prior position with same player to move */
#define G2RESIGN 9		/* a desire to resign rather than play the move returned */

/* for showing lookahead */
#define LOOKAHEADSTATS 1
#define LOOKAHEADMOVES 2

/* suggestion flags */

/* maximum number of suggestion letters to include in getsuggestions */
#define NUMSUG 3

#define SHOWJOSEKI 1
#define SHOWRUN 2
#define SHOWSURROUND 4
#define SHOWLIVE 8
#define SHOWKILL 16
#define SHOWCUT 32
#define SHOWDEFEND 64
#define SHOWINVADE 128
#define SHOWOBVIOUS 256
#define SHOWSHAPE 512
#define SHOWENDGAME 1024
#define SHOWGROUPS 2048
#define SHOWLIBERTIES 4096
#define SHOWFUSEKI 8192
#define SHOWJOSEKIGAME 16384

/* rules to use */

#define JAPANESE 0
#define AMERICAN 1
#define CHINESE 2
#define GOE 3
#define CGOS 4

/* joseki types returned by getjostype() */

/* normal joseki.  should not tenuki at this move */
#define J_NORMAL 0
/* urgent joseki.  do not tenuki here */
#define J_URGENT 1
/* bad move */
#define J_BAD 2
/* complex joseki with many hard variations */
#define J_COMPLEX 3
/* this move is the first move in a corner, at the 3-4 point */
#define J_THREEFROUR 4
/* this move is the first move in a corner, at the 4-4 point */
#define J_FOURFOUR 5
/* this move makes a shimari */
#define J_SHIMARI 6
/* this is a followup move.  tenuki is possible/likely before this move */
#define J_FOLLOWUP 7
/* this is a trick move */
#define J_TRICK 8
/* ignore this move - it is an error in the joseki database */
#define J_IGNORE 9
/* this move is a kakari */
#define J_KAKARI 10
/* this move is the first move in a corner, at the 3-3 point */
#define J_THREETHREE 11
/* this is a kakari against a 4-4 point stone */
#define J_FOURKAKARI 12
/* this is a pincer against a kakari */
#define J_PINCER 13
/* this is move that the program learned (not from a joseki book) */
#define J_LEARNNORMAL 14
/* this is move that the program learned (not from a joseki book) */
#define J_LEARNFOLLOWUP 15

#define LEARN_MAX_STRING 36
#define MAXMOVE 2000

/* a game to add to the game database and be learned from */
struct learn_game {
	char names[2][LEARN_MAX_STRING];			/* player names Black is [0], and white is [1] */
	char filename[LEARN_MAX_STRING];			/* file where game is saved, or "none" */
	char event[LEARN_MAX_STRING];				/* name of event, round, etc, or "none" */
	char boardsize;								/* 9 to 19 */
	char handicap;								/* nummber of handicap stones - patterns are learned after handicap */
	char result;								/* 0 - black wins, 1 - white wins, 2 - tie, 3 - unknown,  */
	char strength[2];							/* strength of black, white -10 10 kyu, +3 3 dan, +12, 2 dan pro */
	short year;									/* date played */
	char month;
	char day;
	int move_count;								/* how many moves in the game (max of MAXMOVE) - set to zero for learn_add_current_game */
	point_t move[MAXMOVE];						/* moves in the game */
	short color[MAXMOVE];						/* color of each move (BLACKCOLOR, WHITECOLOR) */
};

/*
 * used to return moves and values from the fuseki database
 */
struct learn_mv {
	point_t s;
	int count;
};


/***********************************************************************

	Initialization functions

************************************************************************/	
 
/*
 * initialize the go engine
 * must be called once before making any moves
 * max_memory in Megabytes that the engine can use (although there is a minimum memory size no matter what you ask for)
 * returns:
 *	G2OK is successful
 *	G2BADFILEOPEN if a file can't be opened
 *	G2BADFILEACC is error in reading file
 *	G2FAIL if not enough memory
 *
 * It uses fopen, fread, and fclose to read the files.
 * It uses malloc to get memory.
 * It uses getstring() and outerr() to report any problems.
 *
 * The pattern.dat and gamedb.dat files are endian-sensitive.  Be sure you have the right
 * version of this file.
 */
g2status initmfgo(char *cwd, int max_memory);
 
 
/*
 * initialize all constants and variable data before a new game
 * boardsize is the size of the grid (odd number 9-19)
 * set the komi, in 50ths of a point.
 * num_threads is the number of threads to use for any searches in this game.  Search will ue this number or one thread per logical processor, whichever is less.
 * max_playouts is maximum number of random games per move. 1000000 is a good value.
 */
void initvars(int boardsize, int komi, int max_threads, int max_playouts);

/*
 * set lookahead display to show lookahead
 *
 * on = TRUE to turn on lookahead display
 * on = FALSE to turn off lookahead display
 *
 * if lookahead is turned on, the go engine will call lookmove() and
 * looktakeback() during lookahead (fom inside compmove()), so the user
 * interface can update the screen to show the computer thinking.
 */
 void showtac(int on);
 
 
 /*
  * set the komi, in 50ths of a point.
  */
 void initkomi(int komi);
 
/*************************************************************************

	Game playing functions

*************************************************************************/	

/*
 * Make a move at point of color (BLACKCOLOR or WHITECOLOR)
 * point is in the range of 0..boardsize*boardsize-1, or PASS
 * rules specifies which rule set to use (JAPANESE, AMERICAN, etc)
 *     suicide is legal in GOE rules.
 *     PASS costs a prisoner in AMERICAN rules.
 *
 * atari is TRUE if this move puts a group into atari (only one liberty left).
 *
 * use for initializing a position, moving in move tree.  Last
 * makeamove should have fast==FALSE, to do a complete evaluation
 * koisok is TRUE to allow illegal ko captures
 *
 * return TRUE in *atari if the move is an atari
 * returns G2OK if move was made, or an error and does not make the move.
 *	G2OCCUPIED is point already has a stone on it
 *	G2KO is point is illegal ko or other repetition
 *	G2SUICIDE for illegal suicide
 *  G2NOROOM if out of space in data structures, so can't make this move
 *  G2SUPERKO if there is a superko violation
 */
g2status makeamove(point_t point, int color, int rules, int *atari, int koisok); 


/* 
 * take back the last move made.  Specify which rule set in rules.
 *    American rules will subtract a prisoner for taking back a pass.
 *
 * returns G2FAIL if there are no stones on the board.  returns the move
 * move taken back or NOPOINT in *s
 */

g2status takebackamove(int rules, point_t *s);


/*
 * calculate the best move in the current position
 * if (random) randomize the moves a little.
 * color is the color to move
 * handicap is the handicap used for this game (to give the program an
 * idea of the relative strength of white and black).
 * rules is the ruleset to use (JAPANESE, GOE, etc.)
 * time_left is the number of milliseconds to make stones_left moves
 * stones_left is the number of stones left to play.  0 means time_left is the time to compelte the full game, without overtime.
 *   if the game is in overtime, for example 20 moves in 5 minutes, time_left would be 300 and stones_left would be 20)
 * the play level (0-6) controls the strength of the search.
 * show_lookahead causes the engine to show the move sequence it is looking at (if 2, or just search stats if 1, or both if 3)
 * target_time and max_time can cause the search to stop early when time is low.
 * level is the playing level, higher values are stronger, from 0 to NUMLEVELS-1
 * allow_resign it TRUE if the computer can resign (returning G2RESIGN)
 *
 * returns the move in *point
 * returns G2FAIL if there was an internal error,
 * returns G2RESIGN to recommend resignation (but *point still has the best move)
 * returns G2NOROOM if data structure space is low to generate a move
 * retuens G2OK otherwise
 *
 * set tryfirst to PASS when getting a computer move.  For analyzing
 * a move, set tryfirst to that move.  tryfirst is evaluated first, so it
 * will have an accurate score value
 *
 * set skip_count and skip_points to ignore some moves during the search.  This can
 * be used to find the second best, third best, etc moves with repeated searches.
 * if skip_count is 0, skip_points is ignored, so it can be NULL
 *
 * If USE_MPI is defined, using_mpi directs the engine to use MPI protocol to get a better move
 * when using MPI, mpi_ranks_per_group controls the size of the sharing groups.  Must be a power of 2
 */
extern g2status compmove(int random, int color, int handicap, int rules, point_t *point, point_t tryfirst, unsigned int time_left, unsigned int stones_left, int level, int show_lookahead, int skip_count, point_t *skip_points, int using_mpi, int mpi_ranks_per_group, int allow_resign);

/*
 * get a set of computer moves, for hints.  It does a separate search for each move, completing all
 * within the time allocated. 
 * *num_moves input is the number of moves to try to generate.  On output it is the number of moves actually generated
 * points holds the returned moves, best first
 * values holds the estimated value of each move
 * time_left is the total time spend on all of the searches together, in milliseconds
 */
extern g2status compmove_multi(int random, int color, int handicap, int rules, int *num_moves, point_t *points, int *values, unsigned int time_left, int level); 

/* 
 *solve a life and death problem as a test
 */
void solveld(int color, point_t *point);

/*
 * add game to the game database - call once when the game is finished
 * learn_add_game uses the moves in game (the learn_game struct)
 * learn_add_current_game uses the position already in the engine, so it ignores the moves in game.  
 * all is TRUE to add all moves from the game
 * returns TRUE if sucessful, FALSE if not
 */
int learn_add_game(struct learn_game *game, int all);
int learn_add_current_game(struct learn_game *game, int all);

/* 
 * save the learing database.
 * returns TRUE if successful
 */
int learn_save(void);


/***********************************************************************

     Access to internal go engine data structures

************************************************************************/

/*
 * in all cases, sets of points, groups, or armies are passed in arrays
 * terminated with the value -1.
 */

#ifndef SMALLCODE

 /*
 * get a string corresponding to a point in Korschelt notation
 * (A-T, 1-19, for example:  D4, Q16, etc) or PASS or NO
 *
 * buf must be at least 5 characters and is used to return the string.
 * the return value is a pointer to buf.
 *
 */
char *ssqr(point_t s, char *buf);

 
/* use getstring() outerr() to display the reasons for the move at s of color
 * which was generated by the last call to compmove();
 */
void printreasons(point_t s, int color);


/* solve a go life/death problem at point s, with tm to move 
 * uses lookmove, lookbackup, and addcomment to send the commented
 * move tree to the user interface
 * level is the evaluation leve to use (9 recommended)
 * maxnodes is the maximum size of the search (about 500 recommended)
 * maxdepth is maximum depth of search in ply (about 10 recommended)
 *
 * the program will read life and death for the group at point s.
 * it calls thinking() so that it can be interrupted early (thinking returned FALSE)
 * it calls problemprogress() about every 25 nodes so you can display progress on the
 * problem if you like.  This lets you make a maximum solving mode, where the user
 * watches the progress and interrupts the solver whenever he wants to see the best
 * answer so far.
 */
void solveproblem(point_t s, int tm, int level, int maxnodes, int maxdepth);


/* 
 * use outerr and outstone to show analysis of the last move played 
 */
void analyzepos(int rules, int handicap);

/* 
 * should color resign? rules is the rules to evaluate with (JAPANESE, etc.)
 */
int should_resign(int color, int rules);


/* 
 * output moves to attack or defend group at p1.  If p2 is a different group,
 * show moves for semeai between p1 and p2
 */
void outsemmoves(point_t p1, point_t p2, int tm);

/* 
 * return the current evaluation of the board.  50 per point, color is positive
 */
int getcurrenteval(int color, int handicap, int rules);

/* 
 * use getstring() outerr() and outstone() to display status of group at s
 */
void outgstat(point_t s);

/* get move suggestions.
 * color is BLACKCOLOR or WHITECOLOR (color to move)
 * flag or's together flag bits defined above, for which kind of
 * suggestions to return.
 * move suggestions are returned in ideas[][].  Each point on the
 * board gets a string (0 terminated) of letters defining the move
 * suggestions.  Letters are taken from go.txt, page 25, string 54.
 * If the SHOWGROUPS bit is set, every point with a stone on it will
 * have a string set in ideas[][] showing the strength of the that group.
 *
 * getsuggestions does no reading so it is quite fast.
 */
void getsuggestions(int flag, int color, char ideas[361][NUMSUG+1]);

/* output the best move sequence found after a computer move
 * generation, from the point p
 */
void outbestseq(point_t p);

#endif /* SMALLCODE */

  
/*
 * calculate and return the score on the board including komi.
 * rules is CHINESE, JAPANESE, AMERICAN, or GOE
 * swap is point where there is a group to complement the life/death of
 *  use -1 to clear the list of swapped groups.  Each new call with swap
 *  not -1, adds a new group to be swapped.
 * -2 means leave the swapped list alone
 * returns color[] is color controlling each point.  
 *    0-black, 1-white, 2-unknown, 3-stone
 * returns dead[] are TRUE at points containing groups that the program 
 * thinks are dead
 *
 * return value is score as whole number of points, with positive meaning
 * white is ahead.  Komi is included, but rounded down to the nearest
 * whole point (white wins ties)
 */
int getthescore(int rules, int handicap, int swap, int color[NUMPOINTS+1], int dead[NUMPOINTS+1]);


/* 
 * return the army that corresponds to a point 
 */
army_t point2army(point_t point);


/* return the group that corresponds to a point */

group_t point2group(point_t point);


/* return the set of points contained in a group */

void group2points(group_t group, point_t points[NUMPOINTS+1]);


/* return the set of groups contained in an army */

void army2groups(army_t army, int groups[NUMPOINTS+1]);


/* return the state of the board. colors of each point */

void getboard(char pointcolors[NUMPOINTS]);


/* return the number of prisoners for each color */

void getprisoners(int *blackpris, int *whitepris);


/* 
 * get all the joseki moves in a corner
 * joseki are moves from published joseki dictionaries
 * corner specifies one of 4 corners:
 *   0 - upper left (at point 0)
 *   1 - upper right (at point 19)
 *   2 - lower right (at point 360)
 *   3 - lower left (at point 361-19)
 *
 * color is the color to move (BLACKCOLOR, WHITECOLOR)
 *
 * marks is a single character returned for each point on the
 * board indicating what type of joseki move is there for the specified
 * corner:
 *   \0 (null) for no joseki
 *   J for normal Joseki
 *   T for trick joseki
 *   j for joseki followup (a j move is not required, can be played later)
 *   X for a bad move
 * These letters are specified in GO.TXT, page 25, string 54, as part of
 * the outsuggestions string.
 *
 * getjoseki returns the number of joseki was found.
 */
int getjoseki(int corner, int color, char marks[363]);

/* get joseki move number num in corner, or PASSMOVE */

point_t getonejoseki(int corner, int color, int num);
 
/* 
 * is s a joseki move for color in corner (0-3) 
 * returns TRUE or FALSE.
 */
int isjoseki(point_t s, int corner, int color);


/* 
 * get the type of JOSEKI for the last joseki move made in corner 
 *   (note that this might not be the last move actually made)
 * corner is 0 to 3 as specified above.
 * return value is one of 16 constants giving the type of the last
 * move in the corner.
 */
unsigned int getjostype(int corner);

/* 
 * access the game database for learned patterns, for fuseki, joseki, and half board
 *
 * getfuseki gets information about the current position - it returns the number of fuseki moves from the position, an array of data about these moves, the
 * strongest player and win rates in this position, and the index of a game with the strognest player who played to this position.
 *
 * getlearngame gets the full game information about the game from the database on disk.  Returns FALSE if the game is not found
 */
int getfuseki(int color, struct learn_mv *lm, int *strongest, int *wins, int *game);
int getlearngame(int game, struct learn_game *lg);
void getgamejoseki(int color, char marks[363]);
void getgamehalfboard(int color, char marks[363]);
                                  
/* 
 * get marks on stones in g indicating the strength of the group 
 * each point in marks that is part of g gets a one character
 * null terminated string indicating the strength of the group.
 * Other points are set to null strings.
 */                                
void markgroupstrength(group_t g, char marks[361][NUMSUG+1]);

/* 
 * get statistics on the most recent search
 * iterations is the depth of the main search completed
 * nodes is the number of full board evaluations in the search
 */
void getsearchstats(int *iterations, int *nodes);

/*************************************************************************

	Calls out from the go engine to the user interface

*************************************************************************/

/*
 * called periodically while the program is calculating a move
 * UI can blink status or move clock to indicate progress.
 * return TRUE to direct the engine to stop as soon as possible
 * and return the best move found so far.
 */
int thinking(void);


/* 
 * called by solveproblem() to show progress toward the solution.
 * size is the number of nodes searched so far.
 * result is the current evaluation of the result:
 *  0 - failure
 *  1 - likely failure
 *  2 - unknown
 *  3 - ko
 *  4 - likely success
 *  5 - success
 * confidence is the confidence in the result (0 to 100 percent)
 */
void problemprogress(int size, int result, int confidence);

/*
 * get a string (from file or resource), by page and string number(from 0)
 */
char *getstring(int page, int num); 

/*
 * output a string s to the tail of the text output window
 */
void outerr(const char *s);

/* add a comment to the current position - currently only called by
 * solveproblem()
 */

/* clear the error output display */

void clearerror(void);


#ifndef SMALLCODE
 
void addcomment(char *s);

#endif

/*
 * put string c as mark on stone at point s.
 * c is typically 1 to 3 characters long.
 *
 */
void outstone(point_t s, char *c);

/*
 * highlight point s in some way
 */
void histone(point_t s);

/*
 * showlookahead is TRUE, and the engine has made a move at point.
 * UI should display the changed board, and mark point.
 */
void lookmove(point_t s, int color);

/*
 * showlookahead is TRUE, and the engine has just taken back a move.
 * UI should display the changed board.  s has the move that was
 * just taken off.
 */
void looktakeback(point_t s);


/* make the UI back up a move without removing it from the move tree */
void lookbackup(point_t s);

/* show lookahead while thinking */

void fixsd(int mvnumber, int);  /* called after a move is taken back the board */
void fixlm(sqr_t s, int color);      /*add this move to the board (callback form inside compmove) */

#ifdef G2DEBUGOUTPUT
/*********************************************************************
 *
 * interface for debug information
 *
 *********************************************************************/

/*********************************************************************
 *
 * calls the user interface can make to the go engine 
 *
 ********************************************************************/
 
/* execute a debug command.  c is the command name.  cursorpos is
 * the point of interest, tm is the color to move, level si the computer level for evaluations.
 */
void debugcommand(char c, point_t cursorpos, int tm, int handicap, int rules, int level);

/* check the integrity of the data stuctures (link with g2check.c) 
 * if saypassed is true, will output a passed message if check passed.
 */


#ifdef TEST
int dscheck(int saypassed);
#endif
 
/*********************************************************************
 *
 * debug calls out from engine 
 *
 *********************************************************************/
 
/* wait for a key press or mouse click */                      
                      
void waitaction(void);

/* highlight a point in some special way (different from histone) */

void hispecstone(point_t s);

/* output an error message */

void outerror(char *message);

/* remove all marks from board */

void unhighlight(void);

/* turn off the computer player */

void turnoffcplay(void);

/* wait for a mouse click or similar I/O.  Return the point clicked on */

point_t waitclick(void);

# endif
# endif
