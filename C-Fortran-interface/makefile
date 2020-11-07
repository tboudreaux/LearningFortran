FC="gfortran"
FFLAGS="-g"

CC="gcc"
CFLAGS="-g"

default: all

all: cexec fexec

cexec: cobj circle 
	$(FC) mainC.o circle.o -o cExec

cobj:
	$(CC) $(CFLAGS) -c mainC.c 

fexec: fmain circle
	$(FC) $(FFLAGS) mainF.o circle.o -o fortranExec

fmain:
	$(FC) $(FFLAGS) -c mainF.f

circle:
	$(FC) $(FFLAGS) -c circle.f

clean:
	rm cExec
	rm fortranExec
	rm *.o
