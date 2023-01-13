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
# --------------------------------------------------------------------------- #

# Makefile for QSim3D

SHELL=/usr/bin/bash

PV='qsim3d_14.09.16_dev'

NETCDFF_HOME=/opt/produktiv/netcdf-fortran-4.4.2
NETCDF_HOME=/opt/produktiv/netcdf-4.5.0

MPI=produktiv/mvapich2-2.3-mlnx
MPII=/opt/$(MPI)
MPI_BIN=$(MPII)/bin
MPI_MOD=module add $(MPI)
# modulecmd bash add produktiv/mvapich2-2.3-mlnx ?

# compiler
#/opt/produktiv/mvapich2-2.3-mlnx/bin/mpif90
CC   = mpic++
MCC = $(MPI_BIN)/$(CC)
FC  = mpif90              #FC = gfortran
MFC = $(MPI_BIN)/$(FC)


# ifeq ($(MPI_HOME),)
# $(error Die Umgebungsvariable MPI_HOME (Bibliotheken für Parallelisierung) fehlt.)
# endif
# MPI_INSTALL=$(MPI_HOME)  #  Variable wird auf dem hpc von module gesetzt
# MPI_INSTALL_DIR = $(MPI_INSTALL) 
# MPICH2_INCLUDE  = -I$(MPI_HOME)/include
# MPICH2_LIB      = -L$(MPI_HOME)/lib -lmpich -lmpichf90 -lmpl -lpthread
# CDFLIBS = -L$(NETCDF_HOME)/lib -lnetcdf -L$(NETCDFF_HOME)/lib -lnetcdff
# CDFINC = -I$(NETCDFF_HOME)/include # modules for netcdf 

# compiler flags
CFLAGS2  = -O -g -fbounds-check # develop
#CFLAGS2 = -O3 # run fast
FFLAGS   =  $(CFLAGS2) -c -ffree-line-length-none -finit-local-zero

# loader flags
LDFLAGS = -lc -lstdc++ -g -fbounds-check -Wl,-rpath=$(NETCDF_HOME)/lib -Wl,-rpath=$(NETCDFF_HOME)/lib  # develop
# production ## LDFLAGS = -lc -lstdc++  $(MPICH2_LIB)  $(CDFLIBS)

HIER=`pwd -P`
JETZT=`date +"%-d%m%Y"`

SCHISM_HYDRO = ../../Hydro
SCHISM_CORE  = ../../Core
QSIM_LIB     = -L../metabolism -lqsim
SCHISM_LIB   = -L. -lschism

# list of all qsim3d files
Q3 = version.co trans_read.co \
	module_modell.o module_QSimDatenfelder.o nachricht.o\
	benthische_verteilungen.o uebergabe_werte.o schwebstoff_salz.o\
	zonen.o modellg.o wetter.o ganglinien.o schnitt.o\
	planktische_variablen.o module_suspendedMatter.o parallel.o \
	temperw_huelle.o alter.o\
	module_isotope.o \
	nitrogen_wrapper_3d.o \
	oxygen_wrapper_3d.o \
	phosphate_wrapper_3d.o \
	silicate_wrapper_3d.o \
	organic_carbon_wrapper_3d.o \
	ph_wrapper_3d.o \
	algae_huelle.o konsum_huelle.o hnf_huelle.o dreissen_huelle.o \
	ausgeben_casu.o ausgeben_untrim.o ausgabe.o \
	eingabe.o initialisieren.o randbedingungen.o \
	schwermetalle_huelle.o erosion_huelle.o schweb_huelle.o\
	stofftransport_casu.o \
	stofftransport_untrim.o stofftransport.o\
	sedflux_huelle.o coliform_huelle.o albenth_huelle.o mphyt_huelle.o\
	stoffumsatz.o \
	QSim3D.o

# list of all metabolism files
-include ../metabolism/metabol.mk


#--------------------------------------------------------------------

usual: check date metabolism qsim3d

check:
	@if [ `hostname` != "oss-cln51" ]; then \
	   echo "QSim3D cannot be compiled on" `hostname`;\
	   echo "Use ssh oss-cln51"; \
	   exit 1; \
	fi
	@if test -d "$$MY_BIN"; then echo "Directory MY_BIN=$(MY_BIN) vorhanden"; else exit 1 ; fi;
	@if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) vorhanden"; else exit 1 ; fi;
	@if test -d ../metabolism; then echo "Directory ../metabolism ../metabolism vorhanden"; else echo "../metabolism" ; fi;

#	@if test -d "$$NETCDFF_HOME"; then echo "Directory NETCDFF_HOME=$(NETCDFF_HOME) vorhanden"; else exit 1 ; fi;
#	@if test -d "$$NETCDF_HOME"; then echo "Directory NETCDF_HOME=$(NETCDF_HOME) vorhanden"; else exit 1 ; fi;
#	@if test -d "$$MPI_HOME"; then echo "Directory MPI_HOME=$(MPI_HOME) vorhanden"; else exit 1 ; fi;
#	@if test -d $(SCHISM_HYDRO); then echo "Directory $(SCHISM_HYDRO) vorhanden"; else exit 1 ; fi;
#	@if test -d $(SCHISM_CORE); then echo "Directory $(SCHISM_CORE) vorhanden"; else exit 1 ; fi;

all: usual exe save
# all: usual exe kombit save

kombi: date kombit

date: 
	@echo "int version=$(JETZT);">version.h
	@rm -f version.co nachricht.o
	@echo 'write(codesource,*) "'$(HIER)'" ' >code_source.h

SC = $(SCHISM_CORE)/schism_glbl.o  $(SCHISM_CORE)/schism_msgp.o  $(SCHISM_CORE)/misc_modules.o \
     $(SCHISM_CORE)/schism_io.o  \
     $(SCHISM_HYDRO)/solver_subs.o  $(SCHISM_HYDRO)/transport_TVD.o  $(SCHISM_HYDRO)/transport_TVD_imp.o

%.o: %.F90
	$(MFC) $(FFLAGS) -I$(MPII)/include -I$(NETCDFF_HOME)/include -c -o $@  $<

schism: $(SC)
	ar ruv libschism.a $(SC)
	ranlib libschism.a
	@echo "libschism.a done"
	rm -f $(SCHISM_HYDRO)/*.o $(SCHISM_CORE)/*.o

%.co: %.c
	$(MCC) -c  $(CCLAGS) $< -o $@


%.o: %.f90
	$(MFC) $(FFLAGS) -I$(MPII)/include -I$(NETCDFF_HOME)/include -c -o $@  $<

%.o: %.f95
	$(MFC) $(FFLAGS) -I$(MPII)/include -I$(NETCDFF_HOME)/include -c -o $@  $<
#	gfortran-4.3 $(FFLAGS) -c -o $@  $<

#  screen_schism_nc.o stofftransport_schism.o 
#	 read_mesh_nc_sc.o  screen_schism_nc.o  get_schism_step.o  stofftransport_schism.o ausgeben_schism.o \


metabolism: $(META)
	ar ruv libmetabol.a $(META)
	ranlib libmetabol.a
	@echo "libmetabol.a done"

qsim3d: $(Q3)
	$(MFC) $(Q3) -L$(MPII)/lib -lmpich -lmpichf90 -lmpl -lpthread -L$(NETCDF_HOME)/lib -lnetcdf -L$(NETCDFF_HOME)/lib -lnetcdff $(LDFLAGS) -L. -lmetabol -o $(PV)
#	$(FC) $(Q3) $(LDFLAGS) $(QSIM_LIB) $(SCHISM_LIB) -o $(PV)

exe:
	mv -f $(PV) $(MY_BIN)/$(PV)
	cp -f ./scripte/* $(MY_BIN)

KT = version.co trans_read.co \
     kombitransi.o

kombit: $(KT)
	$(FC) $(KT) -L$(MPI_HOME)/lib -lmpich -lmpichf90 -lmpl -lpthread $(CDFLIBS) $(LDFLAGS) -o kombi
	cp -f kombi $(MY_BIN)

clean: 
	@echo "cleaning ..."
	rm -f *.o *.co *.mod *.a $(PV) version.h code_source.h *~
	rm -f ../metabolism/*.o ../metabolism/*/*.o ../metabolism/*.mod ../metabolism/*.a
	rm -f $(SCHISM_HYDRO)/*.o $(SCHISM_CORE)/*.o

FN=$(PV)_source_`date +%d%b%y`.taz 

save: taz store

taz:
	/bin/rm -f *_source_*.taz ../*_source_*.taz
	tar cvfz ../$(FN)  ../qsim3d/README \
	../qsim3d/*.f95 ../qsim3d/*.h ../qsim3d/*.c ../qsim3d/*.txt ../qsim3d/Makefile* ../qsim3d/scripte/* \
	../qsim1d/*.f90  ../qsim1d/*.rc  ../qsim1d/*.txt ../qsim1d/*.xml \
	../metabolism/*.f90 ../metabolism/*/*.f90 ../metabolism/*.txt ../metabolism/*.mk \
	../doku/docmd/*  ../doku/Makefile   ../doku/confi   ../doku/*.html \
	../license.txt ../readme.md
	cp ../$(FN) .

store:
	cp $(FN) $(MY_ARC)
	@echo "saved $(FN) to $(MY_ARC)"
	if test -d "$$sicherung"; then cp $(FN) $(sicherung); echo "additionally saved $(FN) to $(sicherung)"; fi;
	if test -d "./doku/taz"; then cp $(FN) ./doku/taz/qsim3d_source.taz; echo "Quellcode in Dokumentationsportal übertragen"; fi;

test: usual veri save

veri:
	cp -f $(PV) $(MY_BIN)/$(PV)_test
	cp -f ./scripte/* $(MY_BIN)
	@echo "TEST - QSim-3D executable  - $(PV)_test -  wurde erzeugt und in $(MY_BIN) bereitgestellt + scripte"
	@echo "--"


#untrim:
#	qsim3d /scratch/jwyrwa/qmodelle/untrim/ > ./untrim.test
#	/usr/bin/gedit ./untrim.test
