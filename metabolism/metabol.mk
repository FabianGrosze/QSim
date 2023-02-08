#====================================================================
# to include in Makefiles of QSim1D and QSim3D
# Linux
# Wyrwa 02.September 2022
#====================================================================

META= ../metabolism/module_allodim.o \
	../metabolism/module_aparam.o \
	../metabolism/coliform_bacteria/module_coliform_bacteria.o \
	../metabolism/organic_carbon/module_organic_carbon.o \
	../metabolism/oxygen/module_oxygen.o \
	../metabolism/ph/module_ph.o\
	../metabolism/phosphate/module_phosphate.o \
	../metabolism/silicate/module_silicate.o \
	../metabolism/nitrogen/module_nitrogen.o \
	../metabolism/module_metabolism.o \
	../metabolism/set_cloud_reflectance.o \
	../metabolism/albenth.o \
	../metabolism/algaesbl.o \
	../metabolism/algaesgr.o \
	../metabolism/algaeski.o \
	../metabolism/C_Chla.o \
	../metabolism/coroph.o \
	../metabolism/dichte.o \
	../metabolism/dreissen.o \
	../metabolism/e_extnct_lesen.o \
	../metabolism/einleiter_misch.o \
	../metabolism/funkstar.o \
	../metabolism/hnf.o \
	../metabolism/konsum.o \
	../metabolism/lichthemmung.o \
	../metabolism/lin_spline.o \
	../metabolism/lin_sys.o \
	../metabolism/mphyt.o \
	../metabolism/runge_kutta.o \
	../metabolism/sasu.o \
	../metabolism/schiff.o \
	../metabolism/Sed_DiffK.o \
	../metabolism/SedFlux.o \
	../metabolism/Sedimentation.o \
	../metabolism/sediment.o \
	../metabolism/sed_pom.o \
	../metabolism/set_cloud_reflectance.o \
	../metabolism/strahlg.o \
	../metabolism/tage.o \
	../metabolism/temperl.o \
	../metabolism/temperw_kern.o \
	../metabolism/temperw.o \
	../metabolism/uptake.o \
	../metabolism/anztag.o \
	../metabolism/zuflussrand.o \
	../metabolism/belueftung_K2.o \
	../metabolism/dreissen.o \
	../metabolism/wettles.o \
	../metabolism/version_string.o \
	../metabolism/verteilungskoeff.o \
	../metabolism/Sedimentbelastung.o \
	../metabolism/schwermetalle.o \
	../metabolism/schwermetalle_kern.o \
	../metabolism/erosion.o \
	../metabolism/erosion_kern.o \
	../metabolism/schweb.o \
	../metabolism/schweb_kern.o
