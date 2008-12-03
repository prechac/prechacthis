#include <stdio.h>
#include <SWI-Prolog.h>


int main(int argc, char **argv)
{ 
	/* initialise Prolog */
	
	if ( !PL_initialise(argc, argv) ) PL_halt(1);
	char str [80];
	do {
		scanf("%s", str);
	} while (!str == "halt");
	PL_halt(0);
	return 0;
}
