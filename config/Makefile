.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

# PATH options
srcdir = src
objdir = libesmbc/include
bindir = libesmbc/bin
libdir = libs

# Command-line options at make call
debug  ?= 0
openmp ?= 0 

## COMPILER CONFIGURATION ##
# (should be loaded from config directory)

<COMPILER_CONFIGURATION>

# Determine whether to use normal flags or debugging flags
DFLAGS   = $(DFLAGS_NODEBUG)
ifeq ($(debug), 1)
	DFLAGS   = $(DFLAGS_DEBUG)
endif

# Debugging flags with profiling output enabled
ifeq ($(debug), 2)
	DFLAGS   = $(DFLAGS_PROFILE)
endif

###############################################
##
## List of rules and source files
##
###############################################

$(objdir)/ismip6.o: $(srcdir)/ismip6.f90 $(objdir)/nml.o \
							$(objdir)/ncio.o $(objdir)/varslice.o
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/ncio.o: $(srcdir)/ncio.f90
	$(FC) $(DFLAGS) $(FFLAGS) $(INC_NC) -c -o $@ $<

$(objdir)/nml.o: $(srcdir)/nml.f90
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/varslice.o: $(srcdir)/varslice.f90 $(objdir)/nml.o $(objdir)/ncio.o $(objdir)/mapping_scrip.o
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

# coordinates-light

$(objdir)/grid_to_cdo.o: $(srcdir)/coordinates-light/grid_to_cdo.f90
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/gaussian_filter.o: $(srcdir)/coordinates-light/gaussian_filter.f90
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/index.o: $(srcdir)/coordinates-light/index.f90
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

#$(objdir)/interp1D.o: $(srcdir)/coordinates-light/interp1D.f90 $(objdir)/yelmo_defs.o
#	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/interp2D.o: $(srcdir)/coordinates-light/interp2D.f90
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/mapping_scrip.o: $(srcdir)/coordinates-light/mapping_scrip.f90 $(objdir)/ncio.o $(objdir)/interp2D.o \
								$(objdir)/gaussian_filter.o $(objdir)/index.o $(objdir)/grid_to_cdo.o
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

esmbc_libs = 	$(objdir)/ismip6.o \
				$(objdir)/nml.o \
				$(objdir)/ncio.o \
				$(objdir)/varslice.o \
				$(objdir)/grid_to_cdo.o \
				$(objdir)/gaussian_filter.o \
				$(objdir)/index.o \
				$(objdir)/interp2D.o \
				$(objdir)/mapping_scrip.o

#				$(objdir)/interp1D.o \

###############################################
##
## Compilation of complete programs
##
###############################################

test : $(esmbc_libs)
		$(FC) $(DFLAGS) $(FFLAGS) -o $(bindir)/test.x test.f90 \
			$(esmbc_libs) $(LFLAGS)
		@echo " "
		@echo "    $(bindir)/test.x is ready."
		@echo " "

.PHONY : usage
usage:
	@echo ""
	@echo "    * USAGE * "
	@echo ""
	@echo " make test       : compiles test.x"
	@echo " make clean      : cleans object files"
	@echo ""

clean:
	rm -f $(bindir)/*.x
	rm -f  *.x gmon.out $(objdir)/*.o $(objdir)/*.mod $(objdir)/*.a $(objdir)/*.so
	rm -rf *.x.dSYM
