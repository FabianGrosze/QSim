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

# Makefile for QSim1D

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

Q1= module_model_settings.o AdvDiff.o ALBEDO.o aparamles.o ctracer.o \
    E_extnctParam.o EreigGParam.o EreigHParam.o ergeb2DFormat.o  \
    Ergeb2DParam.o ergebMFormat.o ErgebMParam.o ergebTFormat.o  \
    ErgebTParam.o fehlermeldungen.o k_eps.o km_sys.o ModellGParam.o  qsim.o  \
    randbedingungen.o sysgen.o sys_gitterStrang.o sys_gitterTrans.o s  \
    ys_z_gitter.o transport.o transportz.o trimat_z.o  \
    van_veen.o v_konz.o wehr.o wehrles.o WetterParam.o wind_stroemung.o  \
    z_gitter_einl.o Masse_neu_QSim.o lax_wen.o lin_interpolation.o  \
    write_gerris_definitions.o

-include ../metabolism/metabol.mk

#compiler-flags:
CFLAGS2             = -O -g -fbounds-check ## develop ## 
#CFLAGS2             = -O3 ## run fast ## 
FFLAGS          =  $(CFLAGS2) -c -ffree-line-length-none -finit-local-zero
CCLAGS       = $(CFLAGS2)

#--------------------------------------------------------------------

usual: date check metabo qsim1d

all: usual save

HIER=`pwd -P`
JETZT=`date +"%-d%m%Y"`
date: 
	@echo "JETZT=$(JETZT)"
	@echo "HIER=$(HIER)"

#	@echo "int version=$(JETZT);">version.h
#	rm -f version.co nachricht.o
#	@echo 'write(codesource,*) "'$(HIER)'" ' >code_source.h

check:
	if test -d "$$MY_BIN"; then echo "Directory MY_BIN=$(MY_BIN) available"; else exit 1 ; fi;
	if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) available"; else exit 1 ; fi;
	if test -d ../metabolism; then echo "Directory ../metabolism available"; else echo "../metabolism missing" ; fi;

metabo: $(META)
	ar ruv libmetabol.a $(META)
	ranlib libmetabol.a
	@echo "libmetabol.a done"

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@  $<

qsim1d:	$(Q1)
	$(FC) $(Q1) -L. -lmetabol -o $(PV)
	cp -pf ./$(PV) $(MY_BIN)/$(PV)

clean:
	@echo "cleaning ..."
	rm -f *.o $(PV) *.taz *~ *.mod
	rm -f ../metabolism/*.o ../metabolism/*/*.o ../metabolism/*.a ../metabolism/*.mod

FN=$(PV)_linux_source_`date +%d%b%y`.taz

save: taz store

taz:
	/bin/rm -f *_source_*.taz 
	tar cvfz $(FN) *.f90 *.xml *.txt  Makefile* ../metabolism/*.f90 ../metabolism/makefile*

store:
	cp $(FN) $(MY_ARC)
	@echo "saved $(FN) to $(MY_ARC)"
	if test -d "$$sicherung"; then cp $(FN) $(sicherung); echo "additionally saved $(FN) to $(sicherung)"; fi;
