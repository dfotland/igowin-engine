/* interface to the distance from stones data structures - read only! */


/* at each point with a stone, a list of the closest points to that point at maximum distance
 * from that point.  EOL for empty points.
 */
extern list_t boundary[NUMSQUARES];

/* point of closest stone of each color to each point, or NOSQUARE */
extern sqr_t closest[NUMSQUARES][2];

/* distance from each point to closest point of each color, or MAXREACH */
extern unsigned char distance[NUMSQUARES][2];

void initdistance();
void fixdistance();
void outdist(int c);

