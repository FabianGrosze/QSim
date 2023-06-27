# --------------------------------------------------------------------------- #
#  QSim - Programm zur Simulation der Wasserqualität                          #
#                                                                             #
#  Copyright (C) 2022                                                         #
#  Bundesanstalt für Gewässerkunde                                            #
#  Koblenz (Deutschland)                                                      #
#  http://www.bafg.de                                                         #
#                                                                             #
#  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    #
#  der GNU General Public License, Version 3, wie von der Free Software       #
#  Foundation veröffentlicht, weitergeben und/oder modifizieren.              #
#                                                                             #
#  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     #
#  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  #
#  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        #
#  bestimmten Zweck.                                                          #
#                                                                             #
#  Details finden Sie in der GNU General Public License.                      #
#  Sie sollten ein Exemplar der GNU General Public License zusammen mit       #
#  diesem Programm erhalten haben.                                            #
#  Falls nicht, siehe http://www.gnu.org/licenses/.                           #
#                                                                             #
#  Programmiert von                                                           #
#  1979 bis 2018   Volker Kirchesch                                           #
#  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  #
#  seit 2021       Michael Schoenung, schoenung@bafg.de                       #
#                  Fabian Große, grosse@bafg.de                               #
# --------------------------------------------------------------------------- #

# Makefile for QSim1D

# set shell
SHELL=/usr/bin/bash

# get QSim version from version_string.f90
QSim_version=`grep -i versionstext ../metabolism/version_string.f90 | tail -n1 | cut -d"'" -f2`

# set executable name
PV=qsim1d_$(QSim_version)_dev

#====================================================================

# set compiler:
FC_MODULE=produktiv/gcc-7.3.0
FC_LOAD=module add $(FC_MODULE)
FC = gfortran

# ===================================================================

Q1 = gerris_definitions/write_e_extnct.o \
    gerris_definitions/write_e_extnct_param.o \
    gerris_definitions/write_ereigg_param.o \
    gerris_definitions/write_ereigh_param.o \
    gerris_definitions/write_ergeb2d_param.o \
    gerris_definitions/write_ergebm_param.o \
    gerris_definitions/write_ergebt_param.o \
    gerris_definitions/write_modellg_param.o \
    gerris_definitions/write_wetter_param.o \
    gerris_definitions/write_gerris_definitions.o \
    module_model_settings.o \
    advdiff.o \
    albedo.o \
    basispoint.o \
    cip.o \
    coliform_bacteria_inflow_1d.o \
    crank_nicolson.o \
    cra_nickoeff.o\
    tracer_inflow_1d.o \
    init_result_files.o \
    km_sys.o \
    lax_wendroff.o \
    lin_interpolation.o \
    maccorm.o \
    nitrogen_inflow_1d.o \
    organic_carbon_inflow_1d.o \
    oxygen_inflow_1d.o \
    phosphate_inflow_1d.o \
    ph_inflow_1d.o \
    qerror.o \
    qsim.o \
    quickest.o \
    randbedingungen.o \
    silicate_inflow_1d.o \
    sysgen.o \
    sys_gitterStrang.o \
    sys_z_gitter.o \
    thomas.o\
    transport.o \
    trimat.o \
    wehr.o \
    wehrles.o \
    zooplankton_inflow_1d.o
    

-include ../metabolism/metabol.mk

# ===================================== COMPILER OPTIONS =================================================
# DEBUG MODE (complete list and details: https://gcc.gnu.org/onlinedocs/gfortran/Invoking-GNU-Fortran.html)
# -O0                     ... No optimisation
# -g                      ... Write additional output for error tracking in case of runtime error
# -c                      ... Create an object file
# -ffree-line-length-none ... Full line is interpreted
# -fcheck=all,array-temps ... Enable all run time checks except for temporary array creation; includes:
#                             bits      ... Check for invalid arguments to the bit manipulation intrinsics
#                             bounds    ... Check if array subscripts violate the declared minimum and maximum values.
#                             do        ... Check for invalid modification of loop iteration variables.
#                             mem       ... Check for memory allocation.
#                             pointer   ... Check for pointers and allocatables.
#                             recursion ... Check for recursively called subroutines and functions which are not marked as recursive.
# -fmax-errors=N          ... Abort compilation after N errors occurred
# -pedantic               ... Warn about use of extensions to Fortran
# -Wcharacter-truncation  ... Warn if a character string is truncated
# -Wcompare-reals         ... Warn about real comparisons
# -Wconversion            ... Warn if an implicit conversion likely changes the value of an expression
# -Wconversion-extra      ... Warn about implicit conversions between different types and kinds
# -Wrealloc-lhs-all       ... Warn if the compiler introduces code to (re)allocate variables
# -Wsurprising            ... Warn about suspicious code (e.g. INTEGER SELECT construct with a CASE that can never be matched)
# -Wtabs                  ... Warn if tabs are used as whitespace
# -Wunused-dummy-argument ... Warn about unused dummy arguments in subroutines/functions
# -Wunused-parameter      ... Warn about unused parameters
# -------------------------- Not available in currently installed gfortran version -----------------------
# -Wdo-subscript          ... Warn if an array subscript inside a DO loop could lead to an out-of-bounds access
# -Winteger-division      ... Warn if an integer division causes a truncation
# --------------------------------------------------------------------------------------------------------
DEBUG_FLAGS = -O0 -g -c -ffree-line-length-none -fcheck=all,array-temps -fmax-errors=50 -pedantic    \
              -Wcharacter-truncation -Wcompare-reals -Wconversion -Wconversion-extra                 \
              -Wrealloc-lhs-all -Wsurprising -Wtabs -Wunused-dummy-argument -Wunused-parameter
              #-Wdo-subscript -Winteger-division
# ========================================================================================================
# STANDARD MODE
CFLAGS = -O -g -fcheck=bounds
#CFLAGS = -O3 # run fast
FFLAGS = $(CFLAGS) -c -ffree-line-length-none -finit-local-zero
# ========================================================================================================

usual: check metabolism qsim1d

debug: set_debug_options clean usual

set_debug_options:
	$(eval PV=$(PV)_debug)
	$(eval FFLAGS=$(DEBUG_FLAGS))

clean_release: clean usual

clean_debug: clean debug

all: usual save

check:
	@if test -d "$$MY_BIN"; then echo "Directory MY_BIN=$(MY_BIN) exists"; else exit 1 ; fi;
	@if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) exists"; else exit 1 ; fi;
	@if test -d ../metabolism; then echo "Directory ../metabolism exists"; else echo "../metabolism missing" ; fi;

metabolism: $(META)
	ar ruv libmetabol.a $(META)
	ranlib libmetabol.a
	@echo "libmetabol.a done"

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@  $<

qsim1d:	$(Q1)
	$(FC) $(Q1) -L. -lmetabol -o $(PV)

exe:
	mv -pf ./$(PV) $(MY_BIN)/$(PV)

clean:
	@echo "cleaning ..."
	rm -f *.o $(PV) *.taz *~ *.mod *.a
	rm -f ../metabolism/*.o ../metabolism/*/*.o

FN=$(PV)_linux_source_`date +%d%b%y`.taz

save: taz store

taz:
	/bin/rm -f *_source_*.taz 
	tar cvfz $(FN) *.f90 *.rc *.h  Makefile* ../metabolism/*.f90 ../metabolism/*.mk

store:
	cp $(FN) $(MY_ARC)
	@echo "saved $(FN) to $(MY_ARC)"
	if test -d "$$sicherung"; then cp $(FN) $(sicherung); echo "additionally saved $(FN) to $(sicherung)"; fi;
