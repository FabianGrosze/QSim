#!---------------------------------------------------------------------------------------
#!
#!   QSim - Programm zur Simulation der Wasserqualität
#!
#!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
#!
#!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
#!   GNU General Public License, Version 3,
#!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
#!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
#!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
#!   Details finden Sie in der GNU General Public License.
#!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
#!   Falls nicht, siehe http://www.gnu.org/licenses/.  
#!   
#!	Programmiert von:
#!	1979 bis 2018 Volker Kirchesch
#!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
#!
#!---------------------------------------------------------------------------------------
#====================================================================
# Makefile for QSim3D
# wyrwa seit 01aug11
#====================================================================

SHELL=/usr/bin/bash

# PV=qsim3d_sc21
# PV=qsim3d_14.06entw
# PV=qsim3d_14.07
# PV=qsim_14.06git05
# PV='qsim14.06doku'
# PV='qsim3d14.07doku'
# PV='qsim3d14.07Cu'
# PV='qsim3d14.07.01'
# PV='qsim3d_schnitt14.14'
# PV='qsim3d_schwer14.09.00'
# PV='qsim3d_schwer11new'
# PV='qsim3d_dev06sep22'
# PV='qsim3d_schwer17'
PV='qsim3d_schiqu22_12'


NETCDFF_HOME=/opt/produktiv/netcdf-fortran-4.4.2
NETCDF_HOME=/opt/produktiv/netcdf-4.5.0

MPI=produktiv/mvapich2-2.3-mlnx
MPII=/opt/$(MPI)
MPI_BIN=$(MPII)/bin
MPI_MOD=module add $(MPI)
# modulecmd bash add produktiv/mvapich2-2.3-mlnx  ????

#compiler: 
#/opt/produktiv/mvapich2-2.3-mlnx/bin/mpif90
CC              = mpic++
MCC             = $(MPI_BIN)/$(CC)
FC              = mpif90
MFC             = $(MPI_BIN)/$(FC)
#FC              = gfortran

Q3 = version.co trans_read.co \
     module_modell.o module_QSimDatenfelder.o nachricht.o \
     read_mesh_schism.o screen_schism.o ausgeben_schism.o read_zone_gr3.o \
     mesh_output.o get_schism_step.o stofftransport_schism.o \
     benthische_verteilungen.o uebergabe_werte.o schwebstoff_salz.o \
     zonen.o modellg.o wetter.o ganglinien.o schnitt.o \
     planktische_variablen.o module_suspendedMatter.o parallel.o \
     temperw_huelle.o alter.o \
     po4s_huelle.o silikat_huelle.o module_isotope.o ncyc_huelle.o \
     oxygen_huelle.o orgc_huelle.o ph_wrapper_3d.o \
     algae_huelle.o konsum_huelle.o hnf_huelle.o dreissen_huelle.o \
     ausgeben_casu.o ausgeben_untrim.o ausgabe.o \
     eingabe.o initialisieren.o randbedingungen.o \
     schwermetalle_huelle.o erosion_huelle.o schweb_huelle.o\
     stofftransport_casu.o transinfo_schritte.o \
     stofftransport_untrim.o read_mesh_nc.o read_elemente_gerris.o holen_trans_untrim.o untrim_nc_sichten.o\
     stofftransport.o allo_trans.o\
     sedflux_huelle.o coliform_huelle.o albenth_huelle.o mphyt_huelle.o\
     stoffumsatz.o \
     QSim3D.o

-include ../metabolism/metabol.mk

#ifeq ($(MPI_HOME),)
#  $(error Die Umgebungsvariable MPI_HOME (Bibliotheken für Parallelisierung) fehlt.)
#endif
#MPI_INSTALL=$(MPI_HOME)                #  Variable wird auf dem hpc von module gesetzt
#MPI_INSTALL_DIR	    = $(MPI_INSTALL) 
#MPICH2_INCLUDE=-I$(MPI_HOME)/include
#MPICH2_LIB	   = -L$(MPI_HOME)/lib -lmpich -lmpichf90 -lmpl -lpthread
#CDFLIBS = -L$(NETCDF_HOME)/lib -lnetcdf -L$(NETCDFF_HOME)/lib -lnetcdff
#CDFINC = -I$(NETCDFF_HOME)/include # modules for netcdf 

#compiler-flags:
CFLAGS2         = -O -g -fbounds-check -DMPIVERSION=2 ## -Wuninitialized ## -fbounds-check ## develop ##  -fcheck=all
#CFLAGS2             = -O3 ## run fast ## 
FFLAGS          =  $(CFLAGS2) -c -ffree-line-length-none -finit-local-zero

#loader-flags:
LDFLAGS = -lc -lstdc++ -g -fcheck=all  -Wl,-rpath=$(NETCDF_HOME)/lib -Wl,-rpath=$(NETCDFF_HOME)/lib  ## develop ## -fbounds-check
## production ## LDFLAGS = -lc -lstdc++  $(MPICH2_LIB)  $(CDFLIBS)

HIER=`pwd -P`
JETZT=`date +"%-d%m%Y"`

SCHISM_HYDRO = ../../Hydro
SCHISM_CORE  = ../../Core
QSIM_LIB           = -L../metabolism -lqsim
SCHISM_LIB           = -L. -lschism

LANGUAGE = GERMAN # ESPANOL

ifeq ($(MY_BIN),)
  $(error Die Umgebungsvariable MY_BIN (Verzeichnis für ausführbares Programm) fehlt.)
endif
ifeq ($(MY_ARC),)
  $(error Die Umgebungsvariable MY_ARC (Archiv-Verzeichnis) fehlt.)
endif

#--------------------------------------------------------------------

#usual: check date qsim qsim3d
#usual: date metabo qsim3d
usual: date metabo schism qsim3d
#usual: check date qsim qsim3d

check:
	if test -d "$$MY_BIN"; then echo "Directory MY_BIN=$(MY_BIN) vorhanden"; else exit 1 ; fi;
	if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) vorhanden"; else exit 1 ; fi;
	if test -d ../metabolism; then echo "Directory ../metabolism ../metabolism vorhanden"; else echo "../metabolism" ; fi;

	#if test -d "$$NETCDFF_HOME"; then echo "Directory NETCDFF_HOME=$(NETCDFF_HOME) vorhanden"; else exit 1 ; fi;
	#if test -d "$$NETCDF_HOME"; then echo "Directory NETCDF_HOME=$(NETCDF_HOME) vorhanden"; else exit 1 ; fi;
	#if test -d "$$MPI_HOME"; then echo "Directory MPI_HOME=$(MPI_HOME) vorhanden"; else exit 1 ; fi;
	#if test -d $(SCHISM_HYDRO); then echo "Directory $(SCHISM_HYDRO) vorhanden"; else exit 1 ; fi;
	#if test -d $(SCHISM_CORE); then echo "Directory $(SCHISM_CORE) vorhanden"; else exit 1 ; fi;
	#@echo $(HIER) $(JETZT) $(METABO)

all: usual exe save
# all: usual exe kombit save

kombi: date kombit

date: 
	@echo "int version=$(JETZT);">version.h
	rm -f version.co nachricht.o
	@echo 'write(codesource,*) "'$(HIER)'" ' >code_source.h


SC = $(SCHISM_CORE)/schism_glbl.o  $(SCHISM_CORE)/schism_msgp.o  $(SCHISM_CORE)/misc_modules.o \
     $(SCHISM_CORE)/schism_io.o  \
     $(SCHISM_HYDRO)/solver_subs.o  $(SCHISM_HYDRO)/aquire_hgrid.o  $(SCHISM_HYDRO)/misc_subs_QSIM.o\
     $(SCHISM_HYDRO)/transport_TVD.o  $(SCHISM_HYDRO)/transport_TVD_imp.o

%.o: %.F90
	$(MFC) $(FFLAGS) -I$(MPII)/include -I$(NETCDFF_HOME)/include -c -o $@  $<

schism: $(SC)
	ar ruv libschism.a $(SC)
	ranlib libschism.a
	@echo "libschism.a done"
   
#	rm -f $(SCHISM_HYDRO)/*.o $(SCHISM_CORE)/*.o

%.co: %.c
	$(MCC) -c  $(CCLAGS) $< -o $@

# preprocessing for language change in doxygen
#%.F95: %.f95
#	cpp -D$(LANGUAGE) $< $@
#%.o: %.F95
#	gfortran-4.3 $(FFLAGS) -c -o $@  $<

%.o: %.f90
	$(MFC) $(FFLAGS) -I$(MPII)/include -I$(NETCDFF_HOME)/include -c -o $@  $<

%.o: %.f95
	$(MFC) $(FFLAGS) -I$(MPII)/include -I$(NETCDFF_HOME)/include -c -o $@  $<
#	gfortran-4.3 $(FFLAGS) -c -o $@  $<

#  screen_schism_nc.o 
#	 read_mesh_nc_sc.o  screen_schism_nc.o  get_schism_step.o  stofftransport_schism.o ausgeben_schism.o \

metabo: $(META)
	ar ruv libmetabol.a $(META)
	ranlib libmetabol.a
	@echo "libmetabol.a done"

qsim3d:	$(Q3)
	$(MFC) $(Q3) -L$(MPII)/lib -lmpich -lmpichf90 -lmpl -lpthread \
   -L. -lschism\
   -L$(NETCDF_HOME)/lib -lnetcdf -L$(NETCDFF_HOME)/lib -lnetcdff \
   $(LDFLAGS) -L. -lmetabol -o $(PV)

#	$(FC) $(Q3) $(LDFLAGS) $(QSIM_LIB) $(SCHISM_LIB) -o $(PV)

exe:
	cp -f $(PV) $(MY_BIN)/$(PV)
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
	../license.txt ../readme.md \
	$(SCHISM_HYDRO)/*0  $(SCHISM_CORE)/*0
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

	

##################################################################################

doc:	
	rm -rf ./dox
	doxygen ./doku/confi
	cp ./dox/html/index.html ./dox/html/main.html
	cp -rp ./doku/pdf ./dox/html/pdf
	cp -rp ./doku/exp ./dox/html/exp
	cp -rp ./doku/img ./dox/html/img
	cp -rp ./doku/html ./dox/html/html
	cp -rp ./doku/taz ./dox/html/taz
	cp ./dox/html/img/bfg_logo.png ./dox/html/logo.png
	cp ./dox/html/img/qsim_logo.png ./dox/html/qsim.png
	cp ./dox/html/img/qsim_t.ico ./dox/html/qsim.ico
	rsync -ua ./dox/html/* /home/Wyrwa/public_html/dokuport

# 	@echo "type > fm to view new build html-doku , or:"
# 	@echo "type >"
# 	@echo "rsync -ua ./dox/html/* Wyrwa@hpc-service:/home/Wyrwa/public_html/dokuport"
# 	@echo "to publish doku on hpc-service (local webserver)"

# Wyrwa@hpc-worker.bafg.de:~/qsource> module load i4/applications/doxygen-1.8.13
#	/opt/i4/doxygen-1.8.13/bin/doxygen ./doku/confi
# 	/opt/produktiv/doxygen-1.8.12/bin/doxygen ./doku/confi
#
# 	doxygen ./doku/confi
# Wyrwa@hpc-worker.bafg.de:~/qsource> doxygen --version  :  1.8.5 ; date= Mi 30. Nov 10:41:03 CET 2016
# 	/opt/produktiv/doxygen-1.7.4/bin/doxygen ./doku/confi  !! bisher

# scp -r ~/qsource/dox/html Wyrwa@hpc-service:/home/Wyrwa/public_html/dokuport
# geht weil:
# Wyrwa@hpc-worker.bafg.de:~/qsource> ssh-copy-id Wyrwa@hpc-service
# gemacht wurde

#	if test -d "$$MY_DOC"; then rm -rf $(MY_DOC); mv ./dox/html $(MY_DOC); fi;
#	@echo "surf to: http://oss-hpc01.bafg.de/wyrwa/dokuport/index.html"
#	doxygen ./doku/confi

FD=qsim3d_doku_`date +%d%b%y`.taz

savedoku:
	/bin/rm -f qsim3d_doku_*.taz
	rm -rf ./doku/taz/qsim3d_doku.taz
	tar cvfz $(FD) ./doku Makefile
	cp $(FD) $(MY_ARC)
	if test -d "$$sicherung"; then cp $(FD) $(sicherung); echo "additionally saved $(FD) to $(sicherung)"; fi;
	cp -rp $(FD) ./doku/taz/qsim3d_doku.taz
	@echo "saved $(FD) to $(archiv), doku/taz and $(sicherung)"

# Wyrwa@hpc02head:/usr/bin> ls dox*
# doxygen  doxygen.1.7.4  doxygen.1.8.12  doxygen.1.8.5  doxygtag.1.7.4  doxytag
# lrwxrwxrwx. 1 root 13 21. Sep 13:51 doxygen -> doxygen.1.7.4
# Wyrwa@hpc02head:/opt/produktiv> ls doxy* = doxygen-1.7.4  ,  doxygen-1.8.12

# taz:
# 	/bin/rm -f *_source_*.taz 
# 	@cd ./QSim; make save; cd ..
# 	tar cvfz $(FN) README *.f95 *.h *.c *.txt Anmerkungen Makefile* ./QSim/*.taz ./scripte/*

