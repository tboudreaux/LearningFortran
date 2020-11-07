FC="gfortran"
FFLAGS="-O2"
NAME="executable"

default: all

all: main sub
	$(FC) -o $(NAME) main.o sub.o

main:
	$(FC) $(FFLAGS) -c main.f

sub:
	$(FC) $(FFLAGS) -c sub.f

clean:
	rm *.o
	rm $(NAME)
