
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
# Makefile für Gewässergütemodell QSim1D
# wyrwa seit 17dez19

#====================================================================

VERSION = 14.10.07

# PV= qsim1d_voll
# PV= qsim1d_ohnesedflux
# PV= qsim1d_formatfrei
# PV= qsim1d_ohne_sedflux
# PV= qsim1d_mit_sedflux
# PV= qsim1d_entwicklung
# PV= qsim1d_final_v
# PV= qsim1d_14.06
# PV= qsim1d14.10.01
PV= qsim1d_schwer$(VERSION)

#====================================================================

#compiler:
FC              = gfortran
# FC              = mpif90
# aus einem *.f ein *.o compilieren implizt mit dem Kommando:
# $(FC) -c $(FFLAGS)

#compiler-flags:
CFLAGS2             = -O -g -fbounds-check ## develop ## 
#CFLAGS2             = -O3 ## run fast ## 
FFLAGS          =  $(CFLAGS2) -c -ffree-line-length-none -finit-local-zero
CCLAGS       = $(CFLAGS2)

#loader-flags:
LDFLAGS = -lc -lstdc++ -g -fbounds-check ## develop ## 
#LDFLAGS = -lc -lstdc++ ## run fast ## 

#Stoffumsatz als library
QSIM_LIB           = -L../metabolism -lqsim

#check: Bereitstellungsverzeichnis für executable und Archiv
ifeq ($(MY_BIN),)
  $(error Die Umgebungsvariable MY_BIN (Verzeichnis für ausführbares Programm) fehlt.)
endif
ifeq ($(MY_ARC),)
  $(error Die Umgebungsvariable MY_ARC (Archiv-Verzeichnis) fehlt.)
endif


#--------------------------------------------------------------------

usual: date qsim qsim1d

all:   usual save

HIER=`pwd -P`
JETZT=`date +"%-d%m%Y"`
date: 
	@echo "JETZT=$(JETZT)"
	@echo "HIER=$(HIER)"

#	@echo "int version=$(JETZT);">version.h
#	rm -f version.co nachricht.o
#	@echo 'write(codesource,*) "'$(HIER)'" ' >code_source.h


qsim:	
	@cd ../metabolism; cp makefile1 Makefile; make metabol; rm Makefile; cp *.mod ../qsim1d; cd ../qsim1d

%.o: %.f90
	$(FC) $(FFLAGS) $<

Q1= AdvDiff.o ALBEDO.o aparamles.o AParamParam.o ctracer.o E_extnctParam.o EreigGParam.o EreigHParam.o ergeb2DFormat.o  \
Ergeb2DParam.o ergebMFormat.o ErgebMParam.o ergebTFormat.o ErgebTParam.o fehlermeldungen.o k_eps.o km_sys.o ModellGParam.o  \
qsim.o randbedingungen.o sysgen.o sys_gitterStrang.o sys_gitterTrans.o sys_z_gitter.o transport.o transportz.o trimat_z.o  \
van_veen.o v_konz.o wehr.o wehrles.o WetterParam.o wind_stroemung.o z_gitter_einl.o Masse_neu_QSim.o lax_wen.o

qsim1d:	$(Q1)
	$(FC) $(Q1) $(QSIM_LIB) -o $(PV)
	cp -pf ./$(PV) $(MY_BIN)/$(PV)

clean:
	@echo "cleaning ..."
	rm -f *.o $(PV) *.taz *~ *.mod
	rm -f ../metabolism/*.o ../metabolism/*.a ../metabolism/*.mod

FN=$(PV)_linux_source_`date +%d%b%y`.taz

save: taz store

taz:
	/bin/rm -f *_source_*.taz 
	tar cvfz $(FN) *.f90 *.xml *.txt  Makefile* ../metabolism/*.f90 ../metabolism/makefile*

store:
	cp $(FN) $(MY_ARC)
	@echo "saved $(FN) to $(MY_ARC)"
	if test -d "$$sicherung"; then cp $(FN) $(sicherung); echo "additionally saved $(FN) to $(sicherung)"; fi;
