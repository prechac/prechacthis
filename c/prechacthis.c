#include <stdio.h>
#include <SWI-Prolog.h>


int main(int argc, char **argv)
{ 
	/* initialise Prolog */
	
	if ( !PL_initialise(argc, argv) )
		PL_halt(1);
	
	/* Lookup calc/1 and make the arguments and call */
	
	{
		predicate_t pred = PL_predicate("server", 0, "user");
		int rval;
		
		rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, NULL);
		
		PL_halt(rval ? 0 : 1);
	}
	
	return 0;
}
