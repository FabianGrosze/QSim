#====================================================================
# Makefile für Gewässergütemodell QSim1D
# wyrwa seit 17dez19

#====================================================================

# PV= qsim1d_voll
# PV= qsim1d_ohnesedflux
PV= qsim1d_formatfrei

#====================================================================

#compiler:
FC              = gfortran
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
	@cd ../metabolism; make metabol; cd ../qsim1d

%.o: %.f90
	$(FC) $(FFLAGS) $<

Q1= AdvDiff.o ALBEDO.o aparamles.o AParamParam.o ctracer.o E_extnctParam.o EreigGParam.o EreigHParam.o ergeb2DFormat.o  \
Ergeb2DParam.o ergebMFormat.o ErgebMParam.o ergebTFormat.o ErgebTParam.o fehlermeldungen.o k_eps.o km_sys.o ModellGParam.o  \
qsim.o randbedingungen.o sysgen.o sys_gitterStrang.o sys_gitterTrans.o sys_z_gitter.o transport.o transportz.o trimat_z.o  \
van_veen.o verteilungskoeff.o v_konz.o wehr.o wehrles.o WetterParam.o wind_stroemung.o z_gitter_einl.f90

qsim1d:	$(Q1)
	$(FC) $(Q1) $(QSIM_LIB) -o $(PV)
	cp -pf ./$(PV) $(MY_BIN)/$(PV)

clean:
	@echo "cleaning ..."
	rm -f *.o $(PV) *.taz *~ *.mod
	rm -f ../metabolism/*.o ../metabolism/*.a

FN=$(PV)_linux_source_`date +%d%b%y`.taz

save: taz store

taz:
	/bin/rm -f *_source_*.taz 
	tar cvfz $(FN) *.f90 *.xml *.txt  Makefile*

store:
	cp $(FN) $(MY_ARC)
	@echo "saved $(FN) to $(MY_ARC)"
	if test -d "$$sicherung"; then cp $(FN) $(sicherung); echo "additionally saved $(FN) to $(sicherung)"; fi;
