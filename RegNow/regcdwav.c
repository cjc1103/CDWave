/* MiLoPhoto registration key generator 
 * Note: 'int' and 'unsigned int' must be 32-bit!
*/
#include <stdio.h>

const char hex[] = "0123456789ABCDEF";

// $A3B75123, $CD3F5681, $0918F543

#define KEYA 0xA3B75123
#define KEYB 0xCD3F5681
#define KEYC 0x0918F543

unsigned int hash(const char* s, unsigned int mul, unsigned int start)
{
  unsigned int sum = start;
  while (*s) 
    {
      sum = (mul * sum) + *s;
      s++;
    }
  return sum;
}

int main(int argc, char** argv)
{
  if (argc > 1) 
    {
      char* user = argv[1];
      unsigned int h1 = hash(user, KEYA, KEYB);
      unsigned int h2 = hash(user, KEYC, h1);
      //char buf[17];
	  printf("%.8X%.8X", h1, h2);
      //toHex(buf + toHex(buf, h1), h2);
      //printf("%s\n", buf);
      return 0;
    }
  else
    {
      printf("usage: %s <username>\n", argv[0]);
      return 1;
    }
}

