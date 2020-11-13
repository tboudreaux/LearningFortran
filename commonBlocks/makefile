FC="gfortran"
FFLAGS="-g"
NAME=exec

default: $(NAME)

$(NAME): main.o subA.o subB.o
	$(FC) $(FFLAGS) main.o subA.o subB.o -o $(NAME)

main.o:
	$(FC) $(FFLAGS) -c main.f

subA.o:
	$(FC) $(FFLAGS) -c subA.f

subB.o:
	$(FC) $(FFLAGS) -c subB.f

clean:
	rm *.o
	rm $(NAME)
