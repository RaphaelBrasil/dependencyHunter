#include <stdlib.h>
#include <stdio.h>



int main()
{
	
  int i, j,t;
  unsigned short *x;
  unsigned short lfsr = &x;
  unsigned bit;
  unsigned int r;

  unsigned rand()
  {
    bit  = ((lfsr >> 0) ^ (lfsr >> 2) ^ (lfsr >> 3) ^ (lfsr >> 5) ) & 1;
    return lfsr =  (lfsr >> 1) | (bit << 15);
  }

r = rand() % 1000;
printf("%u", r);

}
