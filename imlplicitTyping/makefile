FC="gfortran"
FFLAGS="-g"
NAME="executable"

default: all

all: main
	$(FC) -o $(NAME) main.o

main:
	$(FC) $(FFLAGS) -c main.f

clean:
	rm *.o
	rm $(NAME)
