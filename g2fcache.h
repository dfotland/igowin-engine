/* Copyright 1984-1995 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

#ifndef FCACHE_INCLUDED
#define FCACHE_INCLUDED

# define TRYTOLIVE ((group_t)(NUMGROUPS+1))
# define TRYTOKILL ((group_t)(NUMGROUPS+2))

int countfights(void);	/* how many nodes in all the trees for fights */
void clearfights(void);		/* clear out the fight cache */
group_t biggestarmygroup(list_t grouplist);	/* cache by biggest group in army */
void markfight(group_t goodgroup, group_t badgroup, int tm, tree_t t);
int foughtalready(group_t g, group_t nbgroup, int tm, tree_t *movetree);
int checkliferesult(group_t g, int live);
list_t fightmoves(army_t army, int tm);
#endif


