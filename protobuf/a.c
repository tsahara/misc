#include "pt.pb-c.h"

#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char **argv)
{
  Person *p;
  unsigned char buf[4096];

  FILE *fp = fopen("person.saved", "r");
  size_t len = fread(buf, 1, sizeof(buf), fp);
  fclose(fp);

  p = person__unpack(NULL, len, buf);
  printf("name=%s, id=%d\n", p->name, p->id);

  exit(0);
}
