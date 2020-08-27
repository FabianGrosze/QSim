#====================================================================
# Makefile für mehrdimensionales Gewässergütemodell QSim3D
# wyrwa seit 01aug11

# 02aug18:
# Softlink  QSim -> /home/Wyrwa/QSim1D/src/qsim1d auf 1D-Stoffumsatz (metabolism) benötigt

#====================================================================

# PV=qsim3d
# PV=qsim3d_nurtemp
# PV=qsim3d_oxy
# PV=qsim3d_silikat
# PV=qsim3d_po4s
# PV=qsim3d_po4s_12
# PV=qsim3d_ph
# PV=qsim3d_ph_mintief
# PV=qsim3d_ncyc
# ~/qsource/qsim3d> diff stoffumsatz.f95 stoffumsatz_seflux.f95
# ~/qsource/qsim3d> cp stoffumsatz_nurtemp.f95 stoffumsatz.f95
# PV=qsim3d_nurtempsed
# PV=qsim3d_sedflux
# PV=qsim3d_ncyc_pp
# PV=qsim3d_orgc_pp
# PV=qsim3d_algae_pp
# PV=qsim3d_algae_pp_amor
# PV=qsim3d_alter
# PV=qsim3d_konsum_slurm
# PV=qsim3d_coroph
# PV=qsim3d_aug19_slurm
# PV=qsim3d_rumpf_03sep19
# PV=qsim3d_dreissen_preprocess
# PV=qsim3d_zone
# PV=qsim3d_hnf
# PV=qsim3d_coli
# PV=qsim3d_rumpf_03sep19i
# PV=qsim3d_dreissen_nan
# PV=qsim3d_coli2
# PV=qsim3d_ohnehnf
# PV=qsim3d_albenth
# PV=qsim3d_mphyt
PV=qsim3d_voll2

# NETCDFF_HOME=/opt/produktiv/netcdf-fortran-4.5.0

#compiler:
CC              = mpic++
FC              = mpif90
#CC              = g++
#FC              = gfortran

ifeq ($(MPI_HOME),)
  $(error Die Umgebungsvariable MPI_HOME (Bibliotheken für Parallelisierung) fehlt.)
endif
MPI_INSTALL=$(MPI_HOME)  #  Variable wird auf dem hpc von module gesetzt
MPI_INSTALL_DIR	    = $(MPI_INSTALL) # /home/sdietrich/bin auf voss-devel
#MPI_INSTALL_DIR	    = /mreferate/wyrwa/casulli/mpich2/install
#MPI_INSTALL_DIR	    = /mreferate/wyrwa/casulli/mpich2/mpich2-1.3.2p1
MPICH2_INCLUDE=-I$(MPI_HOME)/include  #  Variable wird auf dem hpc von module gesetzt
#MPICH2_INCLUDE      = -I$(MPI_INSTALL)/include
#MPICH2_INCLUDE      = -I${MPI_INSTALL_DIR}/src/include
#MPICH2_INCLUDE      = -I${MPI_INSTALL_DIR}/bin/include -I${MPI_INSTALL_DIR}/include 
MPICH2_LIB	   = -L$(MPI_HOME)/lib -lmpich -lmpichf90 -lmpl -lpthread
#MPICH2_LIB	   = -L$(MPI_INSTALL)/lib -lmpich -lmpichf90 -lmpl -lpthread
#MPICH2_LIB	   = -L${MPI_INSTALL_DIR}/lib -lmpich -lmpichf90 -lmpl -lpthread
##LDFLAGS            = $(LDFLAGS2) $(GEN_LIBS) $(LIB_LINUX) $(PETSC_LIB) $(MPICH2_LIB) $(GFORTRAN_LIBS) $(INTEL_LIBS) $(AZTEC_LIBS)

# netcdf  # NETCDF_HOME und NETCDFF_HOME werden in ./modules/qsim3d_modules gesetzt
# NETCDF_CFLAGS="-I/opt/produktiv/netcdf-c-4.6.1/include -I/opt/produktiv/netcdf-fortran-4.5.0/include"
# NETCDF_LIBS="-L/opt/produktiv/netcdf-fortran-4.5.0/lib -lnetcdff -L/opt/produktiv/netcdf-c-4.6.1/lib -lnetcdf"

# CDFLIBS = -L/usr/lib64 -lnetcdf -L$(NETCDF_HOME)/lib -lnetcdff
# CDFLIBS = -lnetcdf -L/opt/produktiv/netcdf-4.5.0 -lnetcdff -L$(NETCDF_HOME)/lib
# CDFLIBS = -lnetcdf -L$(NETCDF_HOME)/lib -lnetcdff
# CDFLIBS = -L/home/Wyrwa/weichwaren/netcdf/local/lib -lnetcdf -L$(NETCDFF_HOME)/lib -lnetcdff
# CDFLIBS = -L$(NETCDF_HOME)/lib -lnetcdf -L/opt/produktiv/netcdf-c-4.6.1/lib -lnetcdf -L$(NETCDFF_HOME)/lib -lnetcdff ############################# cln51_slurm
CDFLIBS = -L$(NETCDF_HOME)/lib -lnetcdf -L$(NETCDFF_HOME)/lib -lnetcdff  ############################# preprocess

# CDFINC = -I/usr/include -I$(NETCDF_HOME)/include # modules for netcdf 
# CDFINC = -I/usr/include -I/include # modules for netcdf 
# CDFINC = -I/opt/produktiv/netcdf-c-4.6.1/include -I$(NETCDFF_HOME)/include # modules for netcdf 
CDFINC = -I$(NETCDFF_HOME)/include # modules for netcdf 

#compiler-flags:
CFLAGS2             = -O -g -fbounds-check ## develop ## 
## production ## CFLAGS2             = -O
# GEN_CFLAGS          = -I../lib -I/usr/include
CCLAGS       = $(CFLAGS2) $(MPICH2_INCLUDE)
#FFLAGS       = $(CFLAGS2) -ffixed-line-length-none $(MPICH2_INCLUDE) -finit-local-zero ## develop ##
FFLAGS       = $(CFLAGS2) -ffixed-line-length-none $(MPICH2_INCLUDE) $(CDFINC) -finit-local-zero ## develop ##
## production ## FFLAGS       = $(CFLAGS2) -ffixed-line-length-none $(MPICH2_INCLUDE) $(CDFMOD)

#loader-flags:
LDFLAGS = $(MPICH2_LIB) $(CDFLIBS) -lc -lstdc++ -g -fbounds-check ## develop ## 
## production ## LDFLAGS = -lc -lstdc++  $(MPICH2_LIB)  $(CDFLIBS)

QSIM_LIB           = -L../metabolism -lqsim

LANGUAGE = GERMAN # ESPANOL

ifeq ($(MY_BIN),)
  $(error Die Umgebungsvariable MY_BIN (Verzeichnis für ausführbares Programm) fehlt.)
endif
ifeq ($(MY_ARC),)
  $(error Die Umgebungsvariable MY_ARC (Archiv-Verzeichnis) fehlt.)
endif

#--------------------------------------------------------------------

#usual: modu check date qsim qsim3d
usual: check date qsim qsim3d

modu:
	/usr/bin/modulecmd bash purge
	/usr/bin/modulecmd bash add ./module/qsim3d_modules
	/usr/bin/modulecmd bash list

check:
	if test -d "$$MPI_HOME"; then echo "Directory MPI_HOME=$(MPI_HOME) vorhanden"; else exit 1 ; fi;
	if test -d "$$MY_BIN"; then echo "Directory MY_BIN=$(MY_BIN) vorhanden"; else exit 1 ; fi;
	if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) vorhanden"; else exit 1 ; fi;
	if test -d "$$NETCDFF_HOME"; then echo "Directory NETCDFF_HOME=$(NETCDFF_HOME) vorhanden"; else exit 1 ; fi;
	if test -d "$$NETCDF_HOME"; then echo "Directory NETCDF_HOME=$(NETCDF_HOME) vorhanden"; else exit 1 ; fi;

all: usual exe save
# all: usual exe kombit save

kombi: date kombit

HIER=`pwd -P`
JETZT=`date +"%-d%m%Y"`
date: 
	@echo "int version=$(JETZT);">version.h
	rm -f version.co nachricht.o
	@echo 'write(codesource,*) "'$(HIER)'" ' >code_source.h

#	@echo 'print*,"code_source.h sagt:","'$(HIER)'"'>code_source.h
#	@echo 'sprintf(code_source,"'$(HIER)'");'>code_source.h

qsim:	
	@cd ../metabolism; make metabol; cd ../qsim3d

%.co: %.c
	$(CC) -c  $(CCLAGS) $< -o $@

# preprocessing for language change in doxygen
#%.F95: %.f95
#	cpp -D$(LANGUAGE) $< $@
#%.o: %.F95
#	gfortran-4.3 $(FFLAGS) -c -o $@  $<

%.o: %.f95
	$(FC) $(FFLAGS) -c -o $@  $<
#	gfortran-4.3 $(FFLAGS) -c -o $@  $<

Q3 = version.co trans_read.co \
     module_modell.o module_QSimDatenfelder.o nachricht.o \
     benthische_verteilungen.o uebergabe_werte.o schwebstoff_salz.o\
     zonen.o wetter.o ganglinien.o schnitt.o\
     planktische_variablen.o parallel.o \
     temperw_huelle.o alter.o\
     po4s_huelle.o silikat_huelle.o module_isotope.o ncyc_huelle.o \
     oxygen_huelle.o orgc_huelle.o ph_huelle.o \
     algae_huelle.o konsum_huelle.o hnf_huelle.o dreissen_huelle.o \
     ausgabe.o eingabe.o initialisieren.o randbedingungen.o\
     stofftransport_casu.o stofftransport_schism.o stofftransport_untrim.o stofftransport.o\
     sedflux_huelle.o coliform_huelle.o albenth_huelle.o mphyt_huelle.o\
     stoffumsatz.o \
     QSim3D.o

#

qsim3d:	$(Q3)
	$(FC) $(Q3) $(LDFLAGS) $(QSIM_LIB) -o $(PV)

exe:
	cp -f $(PV) $(MY_BIN)/$(PV)

#	cp -f ./scripte/* $(MY_BIN)
#	cp -f ./module/qsim3d_modules $(MY_BIN)
#	@echo "QSim-3D executable  - $(PV) -  wurde erzeugt und in $(MY_BIN) bereitgestellt + scripte + modules"
#	@echo "--"

KT = version.co trans_read.co \
     kombitransi.o

kombit: $(KT)
	$(FC) $(KT) $(LDFLAGS) -o kombi
	cp -f kombi $(MY_BIN)

#clean:	modu

clean:	
	@echo "cleaning ..."
	rm -f *.o *.co $(PV) version.h code_source.h *~ *.mod
	rm -f ../metabolism/*.o ../metabolism/*.a

# systemaufruf='cp '//trim(adjustl(codesource))//'/*source*.taz'
FN=$(PV)_source_`date +%d%b%y`.taz 

save: taz store

qusa:	
	@cd ./QSim; make save; cd ..

taz:
	/bin/rm -f *_source_*.taz ../*_source_*.taz
	tar cvfz ../$(FN) ../qsim3d/README ../qsim3d/Anmerkungen \
	../qsim3d/*.f95 ../qsim3d/*.h ../qsim3d/*.c ../qsim3d/*.txt ../qsim3d/Makefile* ../qsim3d/scripte/* \
	../qsim1d/*.f90  ../qsim1d/*.rc  ../qsim1d/*.txt ../qsim1d/*.xml ../qsim1d/convert.exe \
	../metabolism/*.f90 ../metabolism/*.txt ../metabolism/Makefile*
	cp ../$(FN) .

store:
	cp $(FN) $(MY_ARC)
	@echo "saved $(FN) to $(MY_ARC)"
	if test -d "$$sicherung"; then cp $(FN) $(sicherung); echo "additionally saved $(FN) to $(sicherung)"; fi;


##################################################################################
