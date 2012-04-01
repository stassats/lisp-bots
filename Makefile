# $Id: Makefile,v 1.1.1.1 2003/11/03 17:17:54 eenge Exp $
# $Source: /project/lisppaste/cvsroot/lisppaste2/Makefile,v $

clean:
	find -name "*.fasl" -o -name "*.faslmt" -o -name "*~" -o -name "*.err" -o -name "*.x86f" | xargs rm 

commit:
	make clean; cvs up; cvs ci

