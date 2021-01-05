/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

# include "g2def.h"
/* reasons for moves 
 * value added when rule passes (50 per point), guess value, weakest group can be made, modifier
 *
 * modifiers are:
 *
 * if no modifier, then fire_strat_rule doesn't have to specify a param.
 * the rule will pass if the move made ends up equal or stronger than the 3rd value.
 *
 * ATTACK or DEFEND. Then fire_strat_rule must specify a square
 * and the rule will only apply if the group on that square gets weaker or
 * stronger respectively (not the stone played).  Reasons for groups marked with ATTACK or DEFEND
 * must end with "group %".  if DEFEND, the group specified must be at least
 * as alive as specified, not the stone played.
 *
 * PATTERNMV, param is the pattern matched, not point required by ATKVAL or DEFVAL
 * 
 * RUN indicates that this is a running move.  the parameter is the
 * key point for this group.  after the move, the stone played must have more
 * running potential.
 *
 * NAMEGROUP: specify a point in param to have why? display a group name substtuted for the %
 *
 * JOSEKIMOVE indicates that this rule indicates a joseki move, so we will read joseki lookahead
 *
 * DEFVAL1 indicates that the strat[].value for this reason is the defense
 * value of this group.  This value will only be added once.  The defval1 rule must not specify more
 * than defval, since it will be used as olddefval for other rules
 * DEFVAL1 rule must specify group.
 * if add DEFVAL1 weakest is weakest aliveness of group after 
 * lookahead that contains stone played
 * DEFVAL2 indicates that this strat[].value contains a def_val, and will only be added once
 * ATKVAL1 indicates that this strat[].value is the atk_val() for this group.
 * this value will only be added once.  param must indicate the group.
 * ATKVAL2 indicates that this pattern strat[].val contains atk_val.
 *  (and won't be counted if another reason has ATKVAL1 set)
 * PATBETTER indicates that if there is a pattern match, that the pattern value is more
 * accurate and this rule should be ignored.
 * BIGMOVE is a big move and only the biggest bonus will be added

 */

# define JOS_GUESS 300
	
struct rulestruct rules[] = {
	{ 0,	0,		ALIVE, BIGMOVE },	/* 0 */
	{ 0,	0,		ALIVE },
	{ 0,	0,		ALIVE },
	{ 0,	0,		ALIVE },
	{ 0,	0,		ALIVE },
	{ 0,	-50,	ALIVE },
	{ 0,	JOS_GUESS,DEAD, JOSEKIMOVE + BIGMOVE },
	{ 0,	JOS_GUESS+400,ALIVE },
	{ 0,	100,	UNSETTLED },
	{ 0,	200,	ALIVE },
	{ 0,	JOS_GUESS, ALIVE + BIGMOVE },  /* 10 */
	{ 0,	100,	RUNNING_FIGHT },
	{ 0,	100,	WEAK_POTENTIAL },
	{ 0,	200,	UNSETTLED },
	{ 200,	200,	WEAK_POTENTIAL },
	{ 0,	0,		DEAD },
	{ 20,	0,		WEAK_POTENTIAL },
	{ 0,	0,		WEAK_POTENTIAL,DEFEND },
	{ 0,	0,		ALIVE, BIGMOVE },  /* no need for defval here, since group is already alive */
	{ 200,	300,	WEAK_POTENTIAL },
	{ 0,	0,		WEAK_POTENTIAL },  /* 20 */
	{ 0,	0,		WEAK_POTENTIAL },
	{ 0,	150,	DEAD },
	{ 0,	250,	UNSETTLED, BIGMOVE },
	{ 0,	0,		WEAK_POTENTIAL,ATKVAL1 + BIGMOVE },
	{ 0,	0,		WEAK_POTENTIAL, BIGMOVE },
	{ 0,	0,		WEAK_POTENTIAL },
	{ 0,	0,		WEAK_POTENTIAL, PATBETTER + BIGMOVE },
	{ 0,	50,		STRONG_SEMEAI },
	{ 0,	0,		WEAK_POTENTIAL },
	{ 0,	0,		DEAD }, /* 30 */
	{ 0,	0,		VERY_WEAK },
	{ -1,	-1,		WEAK },
	{ 0,	150,	ALIVE },
	{ 0,	200,	WEAK_POTENTIAL },
	{ 0,	0,		DEAD },
	{ -1000,-1000,	DEAD },
	{ -1000,-1000,	DEAD },
	{ 0,	0,		DEAD,ATTACK },
	{ 0,	0,		WEAK,DEFVAL1 },
	{ 0,	0,		WEAK },  /* 40 */
	{ 0,	0,		DEAD,ATTACK+ATKVAL1 },
	{ 0,	50,		ALIVE },
	{ 0,	0,		DEAD },
	{ 0,	50,		WEAK_POTENTIAL },
	{ 0,	0,		WEAK_POTENTIAL },
	{ 0,	100,	WEAK_POTENTIAL },
	{ 0,	50,		ALIVE },
	{ 50,	50,		ALIVE },
	{ 20,	150,	ALIVE },
	{ 100,	250,	ALIVE },  /* 50 */
	{ 0,	400,	WEAK },
	{ 50,	150,	WEAK },
	{ 35,	100,	WEAK_POTENTIAL },
	{ 0,	0,		DEAD },
	{ 0,	0,		DEAD },
	{ -500	,-500,	VERY_WEAK },
	{ 0,	100,	DEAD },
	{ 0,	100,	RUNNING_FIGHT, DEFEND+DEFVAL1 },
	{ 0,	150,	RUNNING_FIGHT, DEFEND },
	{ 50,	200,	DEAD },  /* 60 */
	{ 0,	80,		DEAD },	/* 11/96 was WEAK, but want to allow atari leaving group threatened */
	{ 11,	100,	DEAD,ATTACK+ATKVAL1 },
	{ 0,	100,	DEAD,ATTACK },
	{ 0,	0,		DEAD },
	{ 0,	50,		WEAK },
	{ 0,	100,	WEAK },
	{ 0,	50,	WEAK_POTENTIAL },
	{ 0,	200,	DEAD },
	{ -500,	-500,	VERY_WEAK },
	{ 0,	400,	RUNNING_FIGHT },  /* 70 */
	{ 0,	0,		WEAK_POTENTIAL },
	{ 0,	0,		WEAK_POTENTIAL },
	{ 0,	150,	UNSETTLED, BIGMOVE },
	{ 0,	200,	WEAK_POTENTIAL,ATKVAL1 },
	{ 0,	200,	DEAD,ATKVAL1 },
	{ 0,	400,	ALIVE },
	{ 25,	50,		ALIVE },
	{ 25,	25,		ALIVE },
	{ 0,	200,	ALIVE,DEFVAL2 },
	{ 0,	100,	ALIVE,DEFEND },  /* 80 */
	{ 0,	0,		DEAD },
	{ 0,	250,	ALIVE,DEFEND+DEFVAL2 },
	{ 300,	300,	UNSETTLED,DEFEND+DEFVAL2 },
	{ 0,	200,	UNSETTLED, ATKVAL1 },
	{ 0,	300,	UNSETTLED,ATKVAL1 + BIGMOVE },
	{ 0,	0,		UNSETTLED },
	{ 400,	JOS_GUESS+400, DEAD, JOSEKIMOVE + BIGMOVE},
	{ 0,0,	ALIVE,	BIGMOVE },
	{ -3000,-3000,	DEAD },
	{ 0,	200,	DEAD,ATTACK+ATKVAL1 },  /* 90 */
	{ 0,	0,		ALIVE },
	{ 0,	50,		DEAD,JOSEKIMOVE + BIGMOVE },
	{ 0,	50,		DEAD },
	{ 0,	200,	DEAD },
	{ 0,	0,		DEAD,ATTACK },
	{ 100,	400,	ALIVE },
	{ 0,	300,	ALIVE },
	{ 0,	0,		DEAD },
	{ 0,	200,	WEAK_POTENTIAL,RUN+DEFVAL2 },
	{ 5,	5,		DEAD },  /* 100 */
	{ 0,	0,		DEAD },
	{ 0,	200,	WEAK_POTENTIAL },
	{ 0,	50,		WEAK_POTENTIAL },
	{ 0,	0,		DEAD },
	{ 0,	0,		DEAD },
	{ 0,	200,	UNSETTLED },
	{ 0,	0,		DEAD },
	{ 200,	250,	WEAK_POTENTIAL },
	{ 0,	300,	WEAK_POTENTIAL },
	{ 100,	400,	WEAK_POTENTIAL,ATKVAL1 + BIGMOVE },  /* 110 */
	{ 0,	0,		UNSETTLED },
	{ 100,	400,	WEAK_POTENTIAL, BIGMOVE },
	{ 0,	300,	DEAD,JOSEKIMOVE + BIGMOVE},
	{ 150,	0,		WEAK_POTENTIAL },
	{ 0,	0,		DEAD },
	{ 0,	150,	UNSETTLED },
    { 0,	300,	ALIVE },
    { 40,	40,		DEAD },
    { 0,	0,		UNSETTLED,DEFVAL2 },
	{ 200,	200,	SEMEAI,DEFVAL1 },  /* 120 */
	{ 175,	175,	SEMEAI },
	{ 150,	150,	WEAK_SEMEAI },
	{ 0,	125,	UNSETTLED },
	{ 0,	100,	SEMEAI },
	{ 0,	200,	UNSETTLED_LIMP,DEFVAL1 },
	{ 0,	100,	UNSETTLED },
	{ 0,	300,	WEAK,ATKVAL1 },
	{ 0,	700,	ALIVE },
	{ 0,	200,	UNSETTLED, BIGMOVE },  
	{ 200,	400,	UNSETTLED, BIGMOVE },  /* 130 */
	{ 0,	100,	DEAD },
	{ 100,	100,	DEAD },
	{ 100,	100,	WEAK_POTENTIAL },
	{ 100,	100,	WEAK_POTENTIAL },
	{ 25,	100,	WEAK_POTENTIAL },  /* 1/2 point bonus for making bamboo joint */
	{ 0,	150,	ALIVE },
	{ 0,	50,		ALIVE },
	{ 200,	400,	ALIVE },
	{ 0,	400,	WEAK_POTENTIAL },
	{ 0,	100,	WEAK_POTENTIAL },  /* 140 */
	{ 0,	200,	WEAK_POTENTIAL },
	{ 0,	0,		WEAK_POTENTIAL },
	{ 0,	200,	WEAK_POTENTIAL, BIGMOVE },
	{ 0,	0,		WEAK_POTENTIAL },
	{ 0,	400,	WEAK_POTENTIAL,ATKVAL1 + BIGMOVE },
	{ 0,	100,	WEAK_POTENTIAL },
	{ 0,	0,		WEAK_POTENTIAL },
	{ 0,	0,		DEAD },
	{ 0,	0,		DEAD,/*ATTACK+*/ATKVAL1 },
	{ 0,	200,	WEAK_POTENTIAL },  /* 150 */
	{ 0,	0,		0 },  /* weakest alive 0 so this reason is not enough by itself */
	{ 0,	200,	ALIVE,DEFEND+DEFVAL1 },
	{ 0,	0,		ALIVE,DEFEND+DEFVAL1 },
	{ 0,	0,		DEAD },
	{ 0,	400,	WEAK_POTENTIAL },
	{ 0,	150,	ALIVE },
	{ 0,	0,		UNSETTLED },
	{ 0,	0,		WEAK,ATTACK+ATKVAL1 },
	{ 0,	0,		SEMEAI },
	{ 100,	100,	WEAK },  /* 160 */
	{ 100,	100,	DEAD,ATTACK+ATKVAL1 },
	{ 0,	0,		DEAD,ATTACK },
	{ 0,	0,		SEMEAI },
	{ 0,	300,	UNSETTLED },
	{ 0,	0,		DEAD },
	{ 0,	0,		WEAK },
	{ 0,	0,		WEAK },
	{ 0,	0,		DEAD,PATTERNMV },
	{ 0,	0,		WEAK },
	{ 0,	0,		DEAD },  /* 170 */
	{ 0,	0,		DEAD },
	{ 0,	0,		DEAD, DEFEND },
	{ 0,	0,		DEAD, DEFEND },
	{ 0,	0,	RUNNING_FIGHT,DEFEND+DEFVAL1 },  /* have to improve the life */
	{ 0,	300,	WEAK },
	{ 300,	500,	WEAK },
	{ 0,	100,	DEAD,	PATTERNMV },
	{ 0,	100,	DEAD,	PATTERNMV },
	{ 0,	100,	DEAD,	PATTERNMV/*+DEFVAL2 no defval conflict anymore*/ },
	{ 0,	100,	DEAD,	PATTERNMV },  /* 180 */
	{ 0,	100,	DEAD,	PATTERNMV+ATKVAL2 },
	{ 0,	100,	WEAK_LIMP,PATTERNMV+DEFVAL2 },
	{ 0,	100,	DEAD,	PATTERNMV+ATKVAL2 },
	{ 0,	0,		WEAK,	PATTERNMV+DEFVAL2 },
	{ 0,	100,	DEAD,	PATTERNMV },
	{ 0,	0,		DEAD,	PATTERNMV+DEFVAL2 },
	{ 100,	300,	WEAK,	ATTACK+ATKVAL1 },
	{ 0,	0,		STRONG_SEMEAI,DEFEND },
	{ 0,	0,		DEAD },
	{ 0,	0,		DEAD },  /* 190 */
	{ 0,	100,	DEAD,	ATTACK+ATKVAL1 },
	{ 0,	0,		UNSETTLED_DEAD,DEFEND },
	{ 0,	0,		DEAD,	ATKVAL1 },
	{ 0,	400,	UNSETTLED},
	{ 0,	0,		WEAK_SEMEAI, DEFEND },
	{ 0,	0,		DEAD,	PATTERNMV },  
	{ 0,	200,	DEAD,	PATTERNMV },
	{ 0,	0,		DEAD,	DEFVAL1},  /* no defend since already read it - know that it works */
	{ 0,	0,		DEAD,	DEFVAL1},
	{ -50,	-200,	DEAD,	NAMEGROUP},  /* 200 */
	{ 0,	0,		DEAD,	ATKVAL1 },
	{ 0,	0,		DEAD,	ATKVAL1 },
	{ 0,	-200,	DEAD,	NAMEGROUP },
	{ 0,	200,	DEAD,	PATTERNMV/*+DEFVAL2 no defval conflict anymore*/ },
	{ 0,	0,		DEAD,	DEFVAL1 },
	{ 0,	0,		DEAD, },
	{ 0,	0,		DEAD, },
	{ 0,	0,		DEAD, },
	{ 0,	0,		DEAD, },
	{ 0,	0,		WEAK_POTENTIAL,DEFEND+DEFVAL1 }, /* 210 */
	{ 0,	0,		ALIVE, },
	{ 0,	0,		DEAD, },
	{ 0,	0,		DEAD, },
	{ 0,	0,		DEAD, },
	{ 0,	0,		DEAD, },
	};
