#====================================================================
# Makefile Compilation von
# QSim1D 
# qsim13.30vom27sep2017_aktuell.zip
# auf Linux
# mit gfortran
# wyrwa 21feb18
#====================================================================

# Programm-name:
PV= qsim1d_z3

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

all:   check metabol qsim save

check:
	if test -d "$$MY_BIN"; then echo "Directory MY_BIN=$(MY_BIN) vorhanden"; else exit 1 ; fi;
	if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) vorhanden"; else exit 1 ; fi;


QSIM = albenth.o  algaesbl.o  algaesgr.o  algaeski.o  cbsb5.o  C_Chla.o  coliform.o  COROPH.o  Dichte.o \
       dreissen.o  e_extnct_lesen.o  einleiter_Misch.o  erosion.o  funkstar.o  hnf.o  konsum.o  lichthemmung.o \
       lin_spline.o  mphyt.o  ncyc.o  orgc.o  oxygen.o ph.o pweinl.o pwert.o phstart.o \
       po4s.o  pro_Runge_Kutta.o  runge_kutta.o  sasu.o  schiff.o  schweb.o  Sed_DiffK.o  SedFlux.o \
       Sedimentation.o  sediment.o  sed_pom.o  silikat.o  strahlg.o  tage.o  temperl.o  TEMPERW.o  uptake.o ANZTAG.o \
       zuflussrand.o  belueftung_K2.o dreissen.o

metabol:$(QSIM)
	ar ruv libqsim.a $(QSIM)
	ranlib libqsim.a
	@echo "libqsim.a done"


Q=  AdvDiff.o  ALBEDO.o  AParamParam.o  ctracer.o  E_extnctParam.o  EreigGParam.o  EreigHParam.o  \
    ergeb2DFormat.o  Ergeb2DParam.o  ergebMFormat.o  ErgebMParam.o  ergebTFormat.o  ErgebTParam.o  fehlermeldungen.o  \
    k_eps.o  km_sys.o  ModellGParam.o  qsim.o   randbedingungen.o  sub_Chla.o  sysgen.o    \
    sys_gitterStrang.o  sys_gitterTrans.o  sys_z_gitter.o  transport.o  transportz.o  trimat_z.o     \
    van_veen.o  v_konz.o  wehr.o  wehrles.o  WetterParam.o  wettles.o  wind_stroemung.o  z_gitter_einl.o \
    belueftung_K2.o

#ALBEDO.o   ANZTAG.o   AParamParam.o   AdvDiff.o   COROPH.o   C_Chla.o   Dichte.o   E_extnctParam.o  \
#EreigGParam.o   EreigHParam.o   Ergeb2DParam.o   ErgebMParam.o   ErgebTParam.o   ModellGParam.o  \
#SedFlux.o   Sed_DiffK.o   Sedimentation.o   TEMPERW.o   WetterParam.o   albenth.o   algaesbl.o  \
#algaesgr.o   algaeski.o   cbsb5.o   coliform.o   ctracer.o   dreissen.o   einleiter_Misch.o  \
#ergeb2DFormat.o   ergebMFormat.o   ergebTFormat.o   erosion.o   fehlermeldungen.o   funkstar.o  \
#hnf.o   k_eps.o   km_sys.o   konsum.o   lichthemmung.o   lin_spline.o   mphyt.o  \
#ncyc.o   orgc.o   ph.o   phstart.o   po4s.o   pro_Runge_Kutta.o   pweinl.o   pwert.o  \
#qsim.o   randbedingungen.o   runge_kutta.o   sasu.o   schiff.o   schweb.o   sed_pom.o   sediment.o  \
#silikat.o   strahlg.o   sub_Chla.o   sys_gitterStrang.o   sys_gitterTrans.o   sys_z_gitter.o   sysgen.o  \
#tage.o   temperl.o   transport.o   transportz.o   trimat_z.o   uptake.o   v_konz.o   van_veen.o   wehr.o  \
#wehrles.o   wettles.o   wind_stroemung.o   z_gitter_einl.o

%.o: %.f90
	$(FC) $(FFLAGS) $<

qsim:	$(Q)
	$(FC) $(Q) $(METABOL_LIB) -o $(PV)
	cp -pf ./$(PV) $(MY_BIN)/$(PV)

clean:
	@echo "cleaning ..."
	rm -f *.o $(PV) *.taz *~ *.mod *.a

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

