/* Copyright 1984-2000 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */

/* Code to track location and distance of nearest stone of each color 
 * only code in this file can touch the data structures
 */

#include "g2hd.h"
#ifdef G2DEBUGOUTPUT
#include <stdio.h>
#endif

#define MAXREACH 40
#define HIGHEST 10

/* at each point with a stone, a list of the closest points to that point at maximum distance
 * from that point.  EOL for empty points.
 */
list_t boundary[NUMSQUARES];

/* point of closest non-DEAD stone of each color to each point, or NOSQUARE */
sqr_t closest[NUMSQUARES][2];

/* distance from each point to closest non-DEAD stone point of each color, or MAXREACH 
 * adjacent points are distnace 1, so distance zero is never seen
 */
unsigned char distance[NUMSQUARES][2];

void initdistance() {
	sqr_t s;
	for (s = 0; s < NUMSQUARES; ++s) {
		boundary[s] = EOL;
		closest[s][0] = closest[s][1] = NOSQUARE;
		distance[s][0] = distance[s][1] = MAXREACH;
		}
	}

void fixdistance() {
	sqr_t s;
	list_t open = EOL, newb = EOL, ptr, ptr2, nbr;
	list_t finished = EOL;
	int dist = 0;
	int c;
	int bigeyedist[361], olddist[361];	/* this point is in a big eye due to distance */

	for (s = 0; s < boardsquare; ++s) {
		bigeyedist[s] = eyerec[s] != 0 &&	/* was a valid big eye here */
			distance[s][eyecolor[eyerec[s]]] <= 2 && 
			distance[s][1-eyecolor[eyerec[s]]] >= BIGEYEDIST;
		if (bigeyedist[s])
			olddist[s] = distance[s][1-eyecolor[eyerec[s]]];
		if (boundary[s] != EOL)
			killist(&boundary[s]);
		closest[s][0] = closest[s][1] = NOSQUARE;
		distance[s][0] = distance[s][1] = MAXREACH;
		if (board[s] != NOGROUP && gralive[board[s]] != DEAD) {
			adflist(s, &open);
			adflist(s, &boundary[s]);
		}
	}

	while(open != EOL && dist < HIGHEST) {  /* still stones left to expand */
		dist++;
		for (ptr = open; ptr != EOL; ptr = link[ptr]) {
			s = list[ptr];  /* expand s's boundary by one point */
			c = S_COLOR(s);
			for (ptr2 = boundary[s]; ptr2 != EOL; ptr2 = link[ptr2]) {
				if (dist > 3 && board[list[ptr2]] == NOGROUP && lnbf[list[ptr2]][1-c])
					continue;		/* don't expand through enemy liberty unless adjacent to stone */
				if (dist > 2 && board[list[ptr2]] == NOGROUP && 
					(lnbf[list[ptr2]][1-c] > 1 ||
					 lnbf[list[ptr2]][1-c] == 1 && edge[list[ptr2]] == 1))
					continue;		/* don't expand through 1 point jump between stones or from edge */
				for (nbr = nblbp[list[ptr2]]; nbr != EOL; nbr = link[nbr]) {
					if (dist <= distance[list[nbr]][c]) {
						distance[list[nbr]][c] = dist;
						closest[list[nbr]][c] = s;  /* s is closest to this point */
						if (dist == 2 && lnbf[list[nbr]][1-c] && lnbf[list[ptr2]][1-c])
							continue;	/* don't expand through two liberties of enemy groups */
						adflist(list[nbr], &newb);
					}
				}
			}
			killist(&boundary[s]);
			boundary[s] = newb;
			if (newb == EOL)
				adflist(s, &finished);
			newb = EOL;
		}
		for (ptr = finished; ptr != EOL; ptr = link[ptr])
			dlflist(list[ptr], &open);
		killist(&finished);
	}
	killist(&open);
	for (s = 0; s < boardsquare; ++s) {
		if (!bigeyedist[s] &&			/* new point in a big eye */
			(distance[s][0] <= 2 && distance[s][1] >= BIGEYEDIST ||
			distance[s][0] >= BIGEYEDIST && distance[s][1] <= 2)) {
				adflist(s, &eyelist);
		}
		else if (bigeyedist[s] &&		/* point no longer in a big eye */
			(distance[s][eyecolor[eyerec[s]]] > 2 || distance[s][1-eyecolor[eyerec[s]]] < BIGEYEDIST ||
			 distance[s][1-eyecolor[eyerec[s]]] != olddist[s])) {
				adflist(s, &eyelist);
		}
#ifdef NEVER
		else {
			if (eyerec[s] != 0 && ld[s] == NOLD)
				adflist(s, &eyelist);
		}
#endif
	}
}

#ifdef G2DEBUGOUTPUT

void outdist(int c) {
	sqr_t s;
	char buf[12];
	for (s = 0; s < boardsquare; ++s)
		if (board[s] == NOGROUP) {
			sprintf(buf, "%d", distance[s][c]);
			outstone(s, buf);
			}
	}

#endif

