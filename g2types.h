/* types for go program data structures */

# ifndef GOTYPES
# define GOTYPES

#ifndef _MSC_VER 
#define __int64 long long int 
#endif

/* allow visual studio 2005 deprecat3ed functions */
typedef unsigned short list_t;		/* index of a list element */
typedef unsigned short listval_t;	/* storage for a list element */

typedef unsigned short group_t;		/* stones in a single string/block */
typedef unsigned short army_t;		/* groups with unbreakable connections */
typedef unsigned short conn_t;		/* index into connection data structures */
typedef unsigned short eye_t;		/* index into eye data structures */
typedef short sqr_t, point_t;		/* points on board signed since sometimes I subtract them and get negative differences */
typedef short g2status;				/* go engine status returns */
typedef short gomove_t;				/* a sqr_t with 1 bit (512) for color (set is white) 10 bits per move to fit 3 per int*/

#endif
