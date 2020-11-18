FC="gfortran"
FFLAGS="-g"

NAME="exec"

default: $(NAME)

$(NAME): main.o sub.o
	$(FC) $(FFLAGS) main.o sub.o -o $(NAME)

main.o: main.f
	$(FC) $(FFLAGS) -c main.f

sub.o: sub.f
	$(FC) $(FFLAHS) -c sub.f

execTest: main.o
	$(FC) $(FFLAGS) main.o -o execTest

clean:
	rm *.o
	rm $(NAME)
