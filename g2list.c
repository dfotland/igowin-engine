/* Copyright 1984-2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */
 
#include <assert.h>
#include "g2def.h"
#include "g2list.h"

#ifdef CHECK
extern void outerror(char *);
extern void turnoffcplay(void);
#endif

listval_t list[NUMLIST];
list_t link[NUMLIST];  /* lists values and links */

list_t freelist;			/* freelist head for list elements */

int numlistused(void) {
	int count = 0;
	int ptr;
	for (ptr = freelist; ptr != EOL; ptr = link[ptr])
		count++;
	return NUMLIST-count-1;
}

#ifdef ALLOCLISTS

void _fastcall alloclists() {
	list = calloc(NUMLIST,2);
	link = calloc(NUMLIST,2);
	if (list == NULL || link == NULL) {
		outerror("Fatal error!  Not enough memory for lists\n");
		g2exit(0);
	}
}

#endif	

void _fastcall cpylistm(list_t list1, list_t *list2, int mask) {  /* copy list1 to list2. list2 must be empty mask off values as copy them */
   register int ptr,ptr2;
   ASSERT(*list2 < NUMLIST);
   ASSERT(list1 < NUMLIST);
#ifdef CHECK
   if (*list2 != EOL) {
      outerror("cpylist, input not empty\n");
      return;
      }
#endif
   if (list1 == EOL)return;
   *list2 = freelist; 
   ptr2 = freelist; 
   ptr = list1; 
   while(TRUE) {
#ifdef CHECK
      if (ptr2 == EOL) {
         outerror("cpylist\n"); 
         outerror("list overflow\n");
		 ASSERT(0);
         }
#endif
      list[ptr2] = list[ptr]&mask; 
      ptr = link[ptr];
      if (ptr == EOL)break;
      ptr2 = link[ptr2];
      } 
   freelist = link[ptr2]; 
   link[ptr2] = EOL;
   }


void _fastcall cpylist(list_t list1, list_t *list2) {  /* copy list1 to list2. list2 must be empty */
   register int ptr,ptr2;
   ASSERT(*list2 < NUMLIST);
   ASSERT(list1 < NUMLIST);
#ifdef CHECK
   if (*list2 != EOL) {
      outerror("cpylist, input not empty\n");
      return;
      }
#endif
   if (list1 == EOL)return;
   *list2 = freelist; 
   ptr2 = freelist; 
   ptr = list1; 
   while(TRUE) {
#ifdef CHECK
      if (ptr2 == EOL) {
         outerror("cpylist\n"); 
         outerror("list overflow\n");
 		 ASSERT(0);
        }
#endif
      list[ptr2] = list[ptr]; 
      ptr = link[ptr];
      if (ptr == EOL)break;
      ptr2 = link[ptr2];
      } 
   freelist = link[ptr2]; 
   link[ptr2] = EOL;
   }


/* return first element in list and return it to free list */

listval_t _fastcall gtflist(list_t *head) { 
   int tmp;
   ASSERT(*head < NUMLIST);
   if (*head == EOL)return(G2ERROR);
   tmp = *head;
   *head = link[*head];
   link[tmp] = freelist;
   freelist = tmp;
   return(list[freelist]);
   }


/* return TRUE if sorted or unsorted lists have any elements in common */

int _fastcall cmflist(list_t l1, list_t l2) {
	int ptr;
	for (;l1 != EOL; l1 = link[l1]) {
		for (ptr = l2; ptr != EOL; ptr = link[ptr])
			if (list[ptr] == list[l1])return(TRUE);
		}
	return(FALSE);
	}

/* count number of common elements in l1 and l2 (sorted lists) under mask */

int _fastcall comlistm(list_t l1, list_t l2, int mask) {
	int count= 0;
	while(l1 != EOL && l2 != EOL) {
		if ((list[l1]&mask) < (list[l2]&mask))
			l1 = link[l1];
		else if ((list[l1]&mask) > (list[l2]&mask))
			l2 = link[l2];
		else if ((list[l1]&mask) == (list[l2]&mask)) {
			++count;
			l1 = link[l1];
			l2 = link[l2];
			}
		}
	return count;
	}

  
/* count number of common elements in l1 and l2 (sorted lists) */

int _fastcall comlist(list_t l1, list_t l2) {
	int count= 0;
	while(l1 != EOL && l2 != EOL) {
		if (list[l1] < list[l2])
			l1 = link[l1];
		else if (list[l1] > list[l2])
			l2 = link[l2];
		else if (list[l1] == list[l2]) {
			++count;
			l1 = link[l1];
			l2 = link[l2];
			}
		}
	return count;
	}


/* make the list at *head contain all unique elements */
/* keep the first instance of each duplicate */
   
list_t _fastcall unflist(list_t head) {
	int ptr,ptr2,optr;
   ASSERT(head < NUMLIST);
	if (head == EOL)return(head);
	for (ptr = head; link[ptr] != EOL; ptr = link[ptr]) {
		optr = ptr;
		ptr2 = link[ptr];
		while(ptr2 != EOL) {
			if (list[ptr2] == list[ptr]) {
				link[optr] = link[ptr2];
				link[ptr2] = freelist;
				freelist = ptr2;
				ptr2 = link[optr];
				}
			else {
				optr = ptr2;
				ptr2 = link[ptr2];
				}
			}
		}
	return(head);
	}
   
/* merge unsorted lists list1 and list2.  Elements of list1 are
 * interleaved into list2 starting after first+skip elements.
 * list1 is left empty.
 */

void _fastcall mrflist(list_t *list1,list_t *list2,int skip) {
	int ptr1,ptr2,tmp1,tmp2;
	if (*list2 == EOL) {
		*list2 = *list1;
		*list1 = EOL;
		return;
		}
	ptr1 = *list1;
	ptr2 = *list2;
	while(skip > 0 && link[ptr2] != EOL) {
		skip--;
		ptr2 = link[ptr2];
		}
	while(ptr1 != EOL) {
		tmp2 = link[ptr2];
		tmp1 = link[ptr1];
		link[ptr1] = tmp2;
		link[ptr2] = ptr1;
		ptr1 = tmp1;
		ptr2 = link[ptr2];
		if (link[ptr2] != EOL)
			ptr2 = link[ptr2];
		}
	*list1 = EOL;
	}

   
/* concatenate list1 on the front of list2, leaving list1 empty.
 * list1 and list2 are not sorted
 */

void _fastcall catlist(list_t *list1, list_t *list2) {
	int ptr;
	if (*list1 == EOL)return;
	for (ptr = *list1; ptr != EOL; ptr = link[ptr])
		if (link[ptr] == EOL) {
				link[ptr] = *list2;
				break;
				}
	*list2 = *list1;
	*list1 = EOL;
	}

/* concatenate list1 on the end of list2, leaving list1 empty.
 * list1 and list2 are not sorted
 */

void _fastcall ecatlist(list_t *list1, list_t *list2) {
	int ptr;
	if (*list1 == EOL)return;
	if (*list2 == EOL) {
		*list2 = *list1;
		*list1 = EOL;
		return;
		}
	for (ptr = *list2; ptr != EOL; ptr = link[ptr])
		if (link[ptr] == EOL) {
				link[ptr] = *list1;
				break;
				}
	*list1 = EOL;
	}

  
/* merge sorted list1 into sorted list2, leaving list1 unchanged.  return
 * number of elements added to list2
 */


int _fastcall mrglist(list_t list1, list_t *list2) {
   register int ptr1,ptr2, count,temp;
   unsigned int temp2;
	ASSERT(list[EOL] == 0xffff);
   count = 0;
   if (list1 == EOL)return(0);
   if (*list2 == EOL) {
      cpylist(list1,list2);
      ptr1 = *list2;
      while(ptr1 != EOL) {
        ++count;
        ptr1 = link[ptr1];
        }
      return(count);
      }

   if (list[list1] < list[*list2]) {
      temp = *list2;
      *list2 = freelist; 
      freelist = link[freelist];
#ifdef CHECK
      if (freelist == EOL) {
         outerror("mrglist list overflow\n"); 
 		 ASSERT(0);
        }
#endif
      link[*list2] = temp;
      list[*list2] = list[list1];
      ptr1 = link[list1];
      ++count;
      }
   else if (list[list1] == list[*list2])
      ptr1 = link[list1];
   else ptr1 = list1;

   ptr2 = *list2;

   while(ptr1 != EOL) {

/* guaranteed that list[ptr1] > list[ptr2] */
#ifdef CHECK
	   if (list[ptr1] <= list[ptr2]) {
		   outerror("mrglist error\n");
		   turnoffcplay();
		   }
#endif

      if (link[ptr2] == EOL) {         /* end case */
         link[ptr2] = freelist;
         while(ptr1 != EOL) {
            list[freelist] = list[ptr1];
            ptr2 = freelist;
            freelist = link[freelist];
#ifdef CHECK
            if (freelist == EOL) {
               outerror("mrg 1 freelist overflow\n");
	 		 ASSERT(0);
              }
#endif
            ptr1 = link[ptr1];
            ++count;
            }
         link[ptr2] = EOL;
         return(count);
         }

      temp2 = list[link[ptr2]];
      if (list[ptr1] < temp2) {
         temp = link[ptr2];
         link[ptr2] = freelist;
         ptr2 = freelist;
         freelist = link[freelist];
#ifdef CHECK
         if (freelist == EOL) {
            outerror("mrg 2 freelist overflow\n");
	 		 ASSERT(0);
           }
#endif
         link[ptr2] = temp;
         list[ptr2] = list[ptr1];
         ++count;
         ptr1 = link[ptr1];
         }
      else if (list[ptr1] == temp2)
         ptr1 = link[ptr1];
      else ptr2 = link[ptr2];
      }

   return(count);
   } 

  
int _fastcall andlist(list_t list1, list_t list2, list_t *head) {
				/* find elements in both list1 and list2 and
				 * add them to list at *head returns number 
                 * added to head */
   register int count = 0;
   while(list1 != EOL && list2 != EOL) {
      if (list[list1] == list[list2]) {
         addlist(list[list1],head);
         list1 = link[list1];
         list2 = link[list2];
         ++count;
         }
      else if (list[list1] < list[list2])
         list1 = link[list1];
      else
         list2 = link[list2];
      }
   return(count);
   }


/* add value to sorted list (smallest to largest) at head */
/* returns TRUE if new value added */
/* returns FALSE if value is duplicate */

int _fastcall addlist(listval_t value, list_t *head) {
	register int ptr,optr;
	ASSERT(*head < NUMLIST);
	ASSERT(list[EOL] == 0xffff);
	if (list[*head] > value) { 		/* add to front of list */
		ptr = *head;
		*head = freelist; 
		freelist = link[freelist];
#ifdef CHECK
		if (freelist == EOL) {
			outerror("addlist list overflow\n"); 
			 ASSERT(0);
			}
#endif
		link[*head] = ptr;
		list[*head] = value;
		return(TRUE); 
		} 
	if (list[*head] == value)return(FALSE);
	optr = *head; 
	while(TRUE) {
		ptr = link[optr];
		if (list[ptr] >= value) {
			if (list[ptr] == value)return(FALSE);
			link[optr] = freelist;
			list[freelist] = value;
			freelist = link[freelist];
#ifdef CHECK
			if (freelist == EOL) {
				outerror("addlist 2 list overflow\n");
				 ASSERT(0);
				}
#endif
			link[link[optr]] = ptr;
			return(TRUE);
			}
		optr = link[ptr];
		if (list[optr] >= value) {
			if (list[optr] == value)return(FALSE);
			link[ptr] = freelist;
			list[freelist] = value;
			freelist = link[freelist];
#ifdef CHECK
			if (freelist == EOL) {
     			outerror("addlist 3 list overflow\n");
		 		 ASSERT(0);
    			}
#endif
			link[link[ptr]] = optr;
			return(TRUE);
			}
		ptr = link[optr];
		if (list[ptr] >= value) {
			if (list[ptr] == value)return(FALSE);
			link[optr] = freelist;
			list[freelist] = value;
			freelist = link[freelist];
#ifdef CHECK
			if (freelist == EOL) {
     			outerror("addlist 4 list overflow\n");
		 		 ASSERT(0);
    			}
#endif
			link[link[optr]] = ptr;
			return(TRUE);
			}
		optr = link[ptr];
		if (list[optr] >= value) {
			if (list[optr] == value)return(FALSE);
			link[ptr] = freelist;
			list[freelist] = value;
			freelist = link[freelist];
#ifdef CHECK
			if (freelist == EOL) {
     			outerror("addlist 5 list overflow\n");
		 		 ASSERT(0);
    			}
#endif
			link[link[ptr]] = optr;
			return(TRUE);
   			}
		}
	}
  

/* add value to end of unsorted list *head */
  
int _fastcall aeflist(listval_t value, list_t *head) {
   register int ptr;
   ASSERT(*head < NUMLIST);
   if (*head == EOL) {
      *head = freelist;
      freelist = link[freelist];
#ifdef CHECK
      if (freelist == EOL) {
        outerror("aeflist overflow\n");
			 ASSERT(0);
        }
#endif
      link[*head] = EOL;
      list[*head] = value;
      return(TRUE);
      }
#ifdef CHECK
   if (freelist == EOL) {
      outerror("aeflist 2 overflow\n");
 		 ASSERT(0);
     }
#endif
   for (ptr = *head; link[ptr] != EOL; ptr = link[ptr])
		;
   link[ptr] = freelist;
   freelist = link[freelist];
   ptr = link[ptr];
   link[ptr] = EOL;
   list[ptr] = value;
   return(TRUE);
   }


/* add value to front of unsorted list at head */
/* always returns TRUE */

int _fastcall adflist(listval_t value,list_t *head) {
   register int ptr;
   ASSERT(*head < NUMLIST);
   if (*head == EOL) {
      *head = freelist;
      freelist = link[freelist];
#ifdef CHECK
      if (freelist == EOL) {
        outerror("adflist overflow\n");
 		 ASSERT(0);
       }
#endif
      link[*head] = EOL;
      list[*head] = value;
      return(TRUE);
      }
   ptr = *head;
   *head = freelist;
   freelist = link[freelist];
#ifdef CHECK
   if (freelist == EOL) {
      outerror("adflist overflow\n");
 		 ASSERT(0);
     }
#endif
   link[*head] = ptr;
   list[*head] = value;
   return(TRUE);
   }


/* delete a value from an unsorted or sorted list */
/* MUST NOT CHANGE ORDER OF ENTRIES IN LIST! */

int _fastcall dlflist(listval_t value,list_t *head) {
   register int ptr,optr;
   ASSERT(*head < NUMLIST);
   if (list[*head] == value) {  /* found on head of list */
      ptr = *head;
      *head = link[*head];
      link[ptr] = freelist; 
      freelist = ptr; 
      return(TRUE); 
      } 
   optr = *head; 
   while(TRUE) { 
      ptr = link[optr];
      if (ptr == EOL)return(FALSE);
      if (list[ptr] == value) {
   	link[optr] = link[ptr];
   	link[ptr] = freelist;
   	freelist = ptr;
   	return(TRUE);
	}
      optr = link[ptr];
      if (optr == EOL)return(FALSE);
      if (list[optr] == value) {
   	link[ptr] = link[optr];
   	link[optr] = freelist;
   	freelist = optr;
   	return(TRUE);
	}
      }
   } 


/* delete ALL values under a mask from an unsorted or sorted list */
/* MUST NOT CHANGE ORDER OF ENTRIES IN LIST! return TRUE if entry deleted */
/* big changes 10/94 */

int _fastcall dellistm(listval_t value, list_t *head, unsigned int mask) {
	register int ptr,optr,rval = FALSE;
	ASSERT(*head < NUMLIST);
	while((list[*head]&mask) == value) {  /* found on head of list */
		ptr = *head;
		*head = link[*head];
		link[ptr] = freelist; 
		freelist = ptr; 
		rval = TRUE;
		} 
	if (*head == EOL)
		return rval;
	optr = *head; 
	ptr = link[optr];
	while(ptr != EOL) { 
		if ((list[ptr]&mask) == value) {
			link[optr] = link[ptr];
			link[ptr] = freelist;
			freelist = ptr;
			ptr = link[optr];
			rval = TRUE;
			}
		else {
			ptr = link[ptr];
			optr = link[optr];
			}
		}
	return rval;
	} 

  
/* delete value from sorted list at *head.  return true if successful */

int _fastcall dellist(listval_t value, list_t *head) { 
   register int ptr,optr;
   ASSERT(*head < NUMLIST);
   if (list[*head] >= value) {  /* found on head of list */
      if (list[*head] != value)return(FALSE);
      ptr = *head;
      *head = link[*head];
      link[ptr] = freelist; 
      freelist = ptr; 
      return(TRUE); 
      } 
   optr = *head; 
   while(TRUE) { 
      ptr = link[optr];
      if (list[ptr] >= value) {
   	if (list[ptr] != value)return(FALSE); 
   	link[optr] = link[ptr];
   	link[ptr] = freelist;
   	freelist = ptr;
   	return(TRUE);
	}
      optr = link[ptr];
      if (list[optr] >= value) {
   	if (list[optr] != value)return(FALSE); 
   	link[ptr] = link[optr];
   	link[optr] = freelist;
   	freelist = optr;
   	return(TRUE);
	}
      ptr = link[optr];
      if (list[ptr] >= value) {
   	if (list[ptr] != value)return(FALSE); 
   	link[optr] = link[ptr];
   	link[ptr] = freelist;
   	freelist = ptr;
   	return(TRUE);
	}
      optr = link[ptr];
      if (list[optr] >= value) {
   	if (list[optr] != value)return(FALSE); 
   	link[ptr] = link[optr];
   	link[optr] = freelist;
   	freelist = optr;
   	return(TRUE);
	}
      } 
   }
  



/* return entire list at head to the free list */

void _fastcall killist(list_t  *head)
{
   register int ptr;
   ASSERT(*head < NUMLIST);
   if (*head == EOL)
	   return;
   ptr = *head;
   for (ptr = *head; link[ptr] != EOL; ptr = link[ptr]) {
	   ; /* find last element*/
   }
   link[ptr] = freelist;
   freelist = *head;
   *head = EOL;
}

/* return TRUE if val is in sorted list at *head
 */


int _fastcall inlist(listval_t val, list_t *head)
{
	int l;
   ASSERT(*head < NUMLIST);
	l = *head;
	while (l != EOL) {
#ifdef CHECK
		if (list[l] >= list[link[l]]) {
			outerror("Inlist error");
			turnoffcplay();
		}
#endif
		if (list[l] == val)
			return TRUE;
		if (list[l] > val)
			return FALSE;
		l = link[l];
	}
	return FALSE;
}



/* returns the number of elements in sorted or unsorted list at head
 */

int _fastcall cntlist(list_t *head)
{
	register int ptr,cnt;
	ASSERT(*head < NUMLIST);
	cnt = 0;
	for (ptr = *head; ptr != EOL; ptr = link[ptr]) {
		++cnt;
		assert(cnt < NUMLIST + 1);
	}
	return cnt;
}


/* newlist returns the number of values in sorted list l2 which are not in
 * sorted list l1.  The number of new values.
 */
int _fastcall newlist(list_t l1, list_t l2)
{
	int sum;
	sum = 0;
	while (l2 != EOL) {
		while (list[l1] < list[l2])
			l1 = link[l1];
		if (list[l1] != list[l2])
			++sum;
		l2 = link[l2];
	}
	return sum;
}

/* return TRUE if val is in unsorted list at *head
 */
int _fastcall inflist(listval_t val, list_t *head) {
	int l;
   ASSERT(*head < NUMLIST);
	l = *head;
	while(l != EOL) {
		if (list[l] == val)return(TRUE);
		l = link[l];
		}
	return(FALSE);
	}


/* return TRUE if val is in unsorted list at *head under mask
 */


int _fastcall inlistm(listval_t val, list_t *head, unsigned int mask) {
	int l;
   ASSERT(*head < NUMLIST);
	l = *head;
	while(l != EOL) {
		if ((list[l]&mask) == val)return(TRUE);
		l = link[l];
		}
	return(FALSE);
	}

/* reverse list head in place and return head */
	
list_t revlist(list_t *head) {
	int tmp,ptr,rlist= EOL;
   ASSERT(*head < NUMLIST);
	ptr = *head;
	while(ptr != EOL) {
		tmp = link[ptr];
		link[ptr] = rlist;
		rlist = ptr;
		ptr = tmp;
		}
	*head = rlist;
	return(rlist);
	}
