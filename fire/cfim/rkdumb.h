//----------
//  $Id: rkdumb.h 767 2013-04-10 22:29:22Z rhavis@msn.com $
//----------


#ifndef RKDUMB
#define RKDUMB

void rkdumb(double vstart[], int nvar, double x1, double x2, int nstep,
	void (*derivs)(double, double [], double []));


#endif     // RKDUMB


