#====================================================================
# Makefile Compilation von
# QSim1D 
# qsim13.30vom27sep2017_aktuell.zip
# auf Linux
# mit gfortran
# wyrwa 21feb18
#====================================================================

# Library-name:
PV= metabol_qsim

# compiler
#FC              = gfortran
FC              = mpif90

# aus einem *.f ein *.o compilieren implizt mit dem Kommando:
# $(FC) -c $(FFLAGS)
CFLAGS2         = -O -g -fbounds-check
#FFLAGS          =  -c -g -ffree-line-length-none
#FFLAGS          =  $(CFLAGS2) -c -ffixed-line-length-none -finit-local-zero
FFLAGS          =  $(CFLAGS2) -c -ffree-line-length-none -finit-local-zero

METABOL_LIB           = -L. -lqsim

#-------------------------------------------------

usual: all

all:   check metabol save

check:
	if test -d "$$MY_BIN"; then echo "Directory MY_BIN=$(MY_BIN) vorhanden"; else exit 1 ; fi;
	if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) vorhanden"; else exit 1 ; fi;


QSIM = albenth.o  algaesbl.o  algaesgr.o  algaeski.o  cbsb5.o  C_Chla.o  coliform.o  COROPH.o  Dichte.o \
       dreissen.o  e_extnct_lesen.o  einleiter_Misch.o  erosion.o  funkstar.o  hnf.o  konsum.o  lichthemmung.o \
       lin_spline.o  mphyt.o  ncyc.o  orgc.o  oxygen.o ph.o pweinl.o pwert.o phstart.o \
       po4s.o  pro_Runge_Kutta.o  runge_kutta.o  sasu.o  schiff.o  schweb.o  Sed_DiffK.o  SedFlux.o \
       Sedimentation.o  sediment.o  sed_pom.o  silikat.o  strahlg.o  tage.o  temperl.o  TEMPERW.o  uptake.o ANZTAG.o \
       zuflussrand.o  belueftung_K2.o dreissen.o wettles.o Schwermetalle.o

metabol:$(QSIM)
	ar ruv libqsim.a $(QSIM)
	ranlib libqsim.a
	@echo "libqsim.a done"

%.o: %.f90
	$(FC) $(FFLAGS) $<

clean:
	@echo "cleaning ..."
	rm -f *.o *.taz *~ *.mod *.a

FN=$(PV)_linux_source_`date +%d%b%y`

save: taz store

taz:
	/bin/rm -f *_source_*.taz 
	tar cvfz $(FN).taz *.f90 *.xml *.txt  Makefile*

store:
	cp $(FN).taz $(MY_ARC)
	@echo "saved $(FN).taz to $(MY_ARC)"
	if test -d "$$sicherung"; then cp $(FN).taz $(sicherung); echo "additionally saved $(FN).taz to $(sicherung)"; fi;

git:
	git add *.f90 *.txt Makefile
	git commit -m "`date +%d%m%y`"
	git push -u origin master

