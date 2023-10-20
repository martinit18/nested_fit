#  Automatic Time-stamp: <Last changed by martino on Tuesday 08 June 2021 at CEST 15:08:06>
# Copyright M. Trassinelli
# Last modified: 2023/10/20 14:51:31

VERSION_FULL = 4.5.6
VERSION = 4.5

NAME    = nested_fit$(VERSION_FULL)

NAME_FUNC    = nested_fit_func$(VERSION_FULL)

#Choose option for makefile. Comment if not used
COMP = g #g for gfortran, i for ifort
#DEBUG = ON #Enable debug mode.
#NORNG = ON #Uses the same set seed. Useful for testing
OPENMP = ON #Enable/Disable OpenMP.
#OPENMPI = ON #Enable/Disable OpenMPI.
#AUTOTESTS = ON #Automatically run tests after compiling.
#LAPACK = ON #Enable/Disable LAPACK.


.SUFFIXES : .f90 .f .o .c .mod

CFLAGS  =

ifeq ($(OPENMPI),ON )
   ifeq ($(OPENMP),ON )
      OPENMP =
      $(info Compiling against OpenMP and OpenMPI at the same time is currently not supported.)
      $(info Disabling OpenMP.)
      $(info Please enable only one feature.)
   else
   endif
else
endif

ifeq ($(NORNG),ON )
   ifeq ($(OPENMP),ON )
      OPENMP =
      $(info Having a set seed while enabling OpenMP does not work.)
      $(info Disabling OpenMP.)
      $(info Please enable only one feature.)
   else
   endif
else
endif

#Choose compiler, create flags and Mod_metadata.f90 based on options selected

ifeq ($(COMP),g )
    $(info Compiler = gfortran.)
    ifeq ($(DEBUG),ON )
        FFLAGS = -DDEBUG_ON -cpp -O0 -Wall -g -ffree-line-length-0 -fbounds-check 
    else
        FFLAGS = -cpp -O2 -ffree-line-length-0
    endif
    FC  = gfortran
else ifeq ($(COMP),i )
    $(info Compiler = ifort.)
    ifeq ($(DEBUG),ON )
        FFLAGS = -DDEBUG_ON -cpp -heap-arrays -O0 -g -no-wrap-margin -check bounds -warn all -nogen-interfaces
    else
        FFLAGS = -cpp -heap-arrays -O2 -no-wrap-margin
    endif
     
    FC  = ifort
endif



$(file > Mod_metadata.f90,MODULE MOD_METADATA)
$(file >> Mod_metadata.f90,  IMPLICIT NONE)
$(file >> Mod_metadata.f90,  ! Module for program runtime metadata)
$(file >> Mod_metadata.f90,  )
$(file >> Mod_metadata.f90,  CHARACTER(LEN=5), PARAMETER :: version_full = '$(VERSION_FULL)')
$(file >> Mod_metadata.f90,  CHARACTER(LEN=3), PARAMETER :: version = '$(VERSION)')
$(file >> Mod_metadata.f90,  CHARACTER(LEN=18), PARAMETER :: nf_child_proc_name = 'nf_child_proc$(VERSION_FULL)') 
$(file >> Mod_metadata.f90,  )
$(file >> Mod_metadata.f90,#ifdef FUNC_TARGET) 
$(file >> Mod_metadata.f90,  CHARACTER(LEN=20), PARAMETER :: exec_target_name = 'nested_fit_func$(VERSION_FULL)')
$(file >> Mod_metadata.f90,#else)  
$(file >> Mod_metadata.f90,  CHARACTER(LEN=15), PARAMETER :: exec_target_name = 'nested_fit$(VERSION_FULL)')
$(file >> Mod_metadata.f90,#endif) 
$(file >> Mod_metadata.f90,  )
$(file >> Mod_metadata.f90,  )
$(file >> Mod_metadata.f90,END MODULE MOD_METADATA)

ifeq (4.0,$(firstword $(sort $(MAKE_VERSION) 4.0)))
    $(info File Mod_metadata.f90 created)
else
    $(info File Mod_metadata.f90 not updated. The version already existing will be used.)
endif

ifeq ($(OPENMP),ON )
   ifeq ($(COMP),g )
      FFLAGS += -DOPENMP_ON -fopenmp
   else ifeq ($(COMP),i )
      FFLAGS += -DOPENMP_ON -qopenmp
   endif 
else
endif

ifeq ($(OPENMPI),ON )
   FFLAGS += -DOPENMPI_ON
else
endif  

ifeq ($(NORNG),ON )
   FFLAGS += -DNORNG_ON
else
endif

ifeq ($(LAPACK),ON )
   FFLAGS += -DLAPACK_ON
   LIBG = -llapack -lblas
else
endif

ifeq ($(AUTOTESTS),ON )
   FFLAGS += -DAUTOTESTS_ON
else
endif

CFILES =

# In the order: my stuff, Num. Rec., SLATEC, FITPACK, others stuff
FFILES  = USERFCN.f USERFCN_SET.f init_interpolation.f \
	\
	SLATEC/dpsort.f \
	SLATEC/xermsg.f SLATEC/fdump.f SLATEC/j4save.f SLATEC/xercnt.f \
	SLATEC/xerhlt.f SLATEC/xerprn.f SLATEC/xersve.f SLATEC/xgetua.f SLATEC/i1mach.f\
	\
	DIERCKX/splev.f	 DIERCKX/curfit.f DIERCKX/fprati.f DIERCKX/fprota.f DIERCKX/fpknot.f \
	DIERCKX/fpdisc.f DIERCKX/fpgivs.f DIERCKX/fpcurf.f DIERCKX/fpbspl.f DIERCKX/fpchec.f DIERCKX/fpback.f \
	\
	rinteg.f CALGO.ACM/WOFZ.f \

F90FILES_LIKE = Mod_parameters.f90 Mod_timestamp.f90 Mod_likelihood.f90 \
	Mod_cluster_analysis.f90 Mod_search_new_point.f90 \
	Mod_options.f90 Mod_mpi.f90 Mod_math.f90 \
	Mod_metadata.f90 \
	argparse.f90 \
	nested_fit.f90 nested_sampling.f90 \
	USERFCN_2D.f90 \
	dlog_fac.f90 sortn_slatec.f90 meanvar.f90 randn.f90 shirley_fitpack.f90 \

F90FILES_FUNC = Mod_parameters.f90 Mod_timestamp.f90 Mod_likelihood_func.f90 \
	Mod_cluster_analysis.f90 Mod_search_new_point.f90 \
	Mod_options.f90 Mod_mpi.f90 Mod_math.f90 \
	Mod_metadata.f90 \
        argparse.f90 \
	nested_fit.f90 nested_sampling.f90 \
	USERFCN_2D.f90 \
	dlog_fac.f90 sortn_slatec.f90 meanvar.f90 randn.f90 shirley_fitpack.f90 \


# Rules...
#FC  = gfortran
#FC  = ifort
CC  = gcc
FC90= $(FC)
SRCFILES =  $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o) $(F90FILES_LIKE:.f90=.o) 
OBJFILES_FUNC = $(CFILES:.c=.o) $(FFILES:.f=.o) $(F90FILES_FUNC:.f90=.o)
$(info Flags --> $(FFLAGS))
$(info Libraries --> $(LIBG))

#
# actual build
all: $(NAME) $(NAME_FUNC) 

like: $(NAME)

func: $(NAME_FUNC) 

$(NAME): $(OBJFILES) $(FFILES)   $(LIBG)
	$(FC) -o $@  $(FFLAGS)  $(OBJFILES) $(LIBG) $(LDFLAGS)
	chmod 640 $@
	chmod g+x $@
	chmod u+x $@
	rm -f $(HOME)/bin/$@
	cp -p $@ $(HOME)/bin
	mv $@ ../bin/.

$(NAME_FUNC):  $(OBJFILES_FUNC) $(FFILES)   $(LIBG)
	$(FC) -o $@  $(FFLAGS) $(OBJFILES_FUNC) $(LIBG) $(LDFLAGS)
	chmod 640 $@
	chmod g+x $@
	chmod u+x $@
	rm -f $(HOME)/bin/$@
	cp -p $@ $(HOME)/bin
	mv $@ ../bin/.


%.o: $(dir $(FFILES))/%.f
	$(FC) -c $(FFLAGS) ${OPT} $^

.f90.o:
	$(FC90) -c $(FFLAGS) ${OPT} $<


#f90getopt.o: cli/f90getopt.f90
#	$(FC) -c $(FFLAGS) ${OPT} $<

argparse.o: cli/argparse.f90
	$(FC) -c $(FFLAGS) ${OPT} $<

clean:
	rm -f *.o
	rm -f */*.o
	rm -f *.mod
