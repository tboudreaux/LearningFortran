FC="gfortran"
FFLAGS="-g"

default: all

all: main subA subB
	$(FC) $(FFLAGS) main.o subA.o subB.o -o exec

main:
	$(FC) $(FFLAGS) -c main.f

subA:
	$(FC) $(FFLAGS) -c subA.f

subB:
	$(FC) $(FFLAGS) -c subB.f
