.SUFFIXES: .o .f90

FC  =gfortran
LIBS=-lproj 
INC =-I/usr/include

FCFLAGS=-O2 -ffree-line-length-none -w #-Wunused #-Wall 
LDFLAGS=-O2

OBJS=proj.o test.o
EXE =a.out

$(EXE): $(OBJS)
	$(FC) $(LDFLAGS) $(OBJS) $(LIBS) -o $(EXE)
%.o: %.f90
	$(FC) -c $(FCFLAGS) $(INC) $< -o $@
clean:
	rm -f *.o *.mod
