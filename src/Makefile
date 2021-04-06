#  Automatic Time-stamp: <Last changed by martino on Sunday 04 April 2021 at CEST 15:04:00>
NAME    = nested_fit4.0alpha.exe
.SUFFIXES : .f90 .f .o .c .mod
CFLAGS  =
############## FLAG TO COSTUMIZE #############################
#### Uncomment next line to run in parallel in LINUX
#FFLAGS  = -fopenmp
#### Uncomment next line to optimize in MAC
#FFLAGS   = -framework accelerate
#### Uncomment next line to run in parallel and optimize in MAC
FFLAGS   = -framework accelerate -fopenmp
###############################################################
### ATTENTION!!!!
### If you do not want to run in parallel,
### modify the variable "parallel_on" in nested fit file
###############################################################

CFILES =

# In the order: my stuff, Num. Rec., SLATEC, FITPACK, others stuff
FFILES  =  USERFCN.f USERFCN_SET.f init_interpolation.f \
	\
	dpsort.f \
	xermsg.f fdump.f j4save.f xercnt.f \
	xerhlt.f xerprn.f xersve.f xgetua.f i1mach.f\
	\
	splev.f	 curfit.f fprati.f fprota.f fpknot.f \
	fpdisc.f fpgivs.f fpcurf.f fpbspl.f fpchec.f fpback.f \
	\
	rinteg.f WOFZ.f \
#	cw.f
#	splint.f fpintb.f

# Change Mod_likelihood.f90 in Mod_likelihood_tests.f90 below for tests

F90FILES = Mod_parameters.f90 Mod_likelihood.f90 Mod_timestamp.f90 \
	Mod_cluster_analysis.f90 Mod_search_new_point.f90 \
	nested_fit.f90 nested_sampling.f90 \
	USERFCN_2D.f90 \
	dlog_fac.f90 sortn_slatec.f90 meanvar.f90 randn.f90 shirley_fitpack.f90


# for gfortran
OPT = -O2 -static
# For Intel fortran
#OPT = -O2 -no-wrap-margin

# other options
#OPT = -g  -C -static
#OPT = -O0 -g
#OPT = -O0 -g  -fbounds-check -Wall  -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -ftrapv
#OPT = -g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan
#OPT = -g -Wall -Wextra -Warray-temporaries -Wconversion -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow -finit-real=nan


#LDFLAGS = $(FFLAGS)
#LDFLAGS = -fopenmp
#LDFLAGS = -framework accelerate
#LDFLAGS = -framework accelerate -fopenmp

# Rules...
FC  = gfortran
#FC  = ifort
CC  = gcc
FC90= $(FC)
SRCFILES =  $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o) $(F90FILES:.f90=.o)

.f.o:
	$(FC) -c $(FFLAGS) ${OPT} $<

.c.o:
	$(CC) -c $(CFLAGS) $<


.f90.o:
	$(FC90) -c $(FFLAGS) ${OPT} $<
# actual build
$(NAME): $(OBJFILES) $(FFILES)   $(LIBG)
	$(FC) -o $@  $(FFLAGS)  $(OBJFILES) $(LIBG) $(LDFLAGS)
	chmod 640 $@
	chmod g+x $@
	chmod u+x $@
	cp -p $@ $(HOME)/bin

clean:
	rm -f *.o
	rm -f *.mod
