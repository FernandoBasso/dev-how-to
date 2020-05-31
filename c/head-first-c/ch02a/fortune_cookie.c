#include <stdio.h>

void fortune_cookie (char *msg)
{
  // Size of storage needed for a pointer (to char in this case).
  // 4 on 32-bit systems, 8 on 64-bit systems.
  printf ("In Fn: %lx\n", sizeof (msg));
  printf ("Message reads: “%s”\n", msg);
}

int main ()
{
  // char quote[] = "The force is strong with you.";
  char quote[] = "Yoda";

  printf ("In main: %ld\n", sizeof (quote));
  fortune_cookie (quote);

  return 0;
}

/* vim: set syn=off ft=text ai: */

