!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

!> subroutine schwermetalle_kern : balance of heavy metal concentrations \n\n
!! file: schwermetalle_kern.f90 zurück: \ref lnk_schwermetalle
	  subroutine schwermetalle_kern(SSalgs,vphs,SSeross,iformVert                      &
	             ,anzZeits,sedsss,sedalks,sedalgs,sedalbs                                     &
				 ,gsZns,glZns,gsCads,glCads,gsCus,glCus,gsNis,glNis,gsAss,glAss,gsPbs,glPbs   &
				 ,gsCrs,glCrs,gsFes,glFes,gsHgs,glHgs,gsMns,glMns,gsUs,glUs                   &
				 ,kontroll,jjj)
	  
      implicit none
      logical :: kontroll
      integer :: jjj, anzZeits,iformVert
	  
      real    :: SSalgs,vphs,SSeross  
      real    :: sedsss,sedalks,sedalbs,sedalgs  
	  real    :: glZns, gsZns, glCads, gsCads, glCus, gsCus, glNis, gsNis
      real    :: glAss, gsAss, glPbs, gsPbs, glCrs, gsCrs, glFes, gsFes
      real    :: glHgs, gsHgs, glMns, gsMns, glUs, gsUs

      real    :: VTKoeffZn, VTKoeffCad, VTKoeffCu, VTKoeffNi
      real    :: VTKoeffAs, VTKoeffPb, VTKoeffCr, VTKoeffFe
      real    :: VTKoeffHg, VTKoeffMn, VTKoeffU

      real    :: ZnSeds,CadSeds,CuSeds,NiSeds,AsSeds,PbSeds
      real    :: CrSeds,FeSeds,Hgseds,MnSeds,USeds	

      real    :: hcSS,hcph	  

      !if(kontroll)print*,jjj,' schwermetalle_kern anfa glZns,gsZns,glPbs,gsPbs=',glZns,gsZns,glPbs,gsPbs

      ! Minimum von Schwebstoffkonzentration und pH-Wert für Berechnung von Verteilungskoeffizient und Sedimentbelastung ... 
      hcSS = min(100.,SSalgs)
      hcph = max(4.,vphs)

!     ########################################################################################
!     Berechnung der Verteilungskoeffizienten VTKoeffZn, VTKoeffCu, VTKoeffCad, VTKoeffNi
!                                             VTKoeffAs, VTKoeffPb, VTKoeffCr, VTKoeffFe 
!                                             VTKoeffHg, VTKoeffMn, VTKoeffU 
!     VTKoff.. in l/g aus dem Verhältnis gesamt- und gelöster Schwermetall-Konz.
!     ########################################################################################

      call Verteilungskoeff(hcSS,hcph  &
             ,VTKoeffZn,VTKoeffCu,VTKoeffCad,VTKoeffNi,VTKoeffAs,VTKoeffPb   &
			 ,VTKoeffCr,VTKoeffFe,VTKoeffHg ,VTKoeffMn,VTKoeffU               &  
			 ,iformVert,kontroll ,jjj)                              

!     ########################################################################################
!     Berechnung der Sedimentbelastung ZnSeds bis USeds
!     Hochzählen der erosionslosen Zeitschritte anzZeits
!     ########################################################################################
      call Sedimentbelastung(SSalgs  						&
                               ,gsZns,glZns,ZnSeds		&
							   ,gsCads,glCads,CadSeds	    &
                               ,gsCus,glCus,CuSeds		&
							   ,gsNis,glNis,NiSeds		&
							   ,gsAss,glAss,AsSeds		&
							   ,gsPbs,glPbs,PbSeds        &
                               ,gsCrs,glCrs,CrSeds		&
							   ,gsFes,glFes,FeSeds		&
							   ,gsHgs,glHgs,HgSeds		&
							   ,gsMns,glMns,MnSeds        &
                               ,gsUs,glUs,USeds			&
							   ,anzZeits,SSeross            &
							   ,kontroll ,jjj)

!     ########################################################################################
!     +++++ Verringerung der Gesamt-Schwermetallkonz. durch Sedimentation +++++++
!     ########################################################################################

      if(gsZns>0.0) gsZns = gsZns-(gsZns - glZns)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
      if(gsCads>0.0) gsCads = gsCads-(gsCads - glCads)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
      if(gsCus>0.0) gsCus = gsCus-(gsCus - glCus)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
      if(gsNis>0.0) gsNis = gsNis-(gsNis - glNis)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
      if(gsAss>0.0) gsAss = gsAss-(gsAss - glAss)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
      if(gsPbs>0.0) gsPbs = gsPbs-(gsPbs - glPbs)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
	     !if(kontroll)print*,'schwermetalle_kern Sedimentation glPbs,gsPbs,hcSS=',glPbs,gsPbs,hcSS
      if(gsCrs>0.0) gsCrs = gsCrs-(gsCrs - glCrs)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
      if(gsFes>0.0) gsFes = gsFes-(gsFes - glFes)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
      if(gsHgs>0.0) gsHgs = gsHgs-(gsHgs - glHgs)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
      if(gsMns>0.0) gsMns = gsMns-(gsMns - glMns)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)
      if(gsUs>0.0) gsUs = gsUs-(gsUs - glUs)*((sedSSs+sedalks+sedalbs+sedalgs)/hcSS)

!     ########################################################################################
!     +++++ Erhöhung der Gesamt-Schwermetallkonz. durch Erosion +++++++
!     ########################################################################################

      if(SSeross>0.0) then
        gsZns = gsZns + SSeross*ZnSeds
        gsCads= gsCads+ SSeross*CadSeds
        gsCus = gsCus + SSeross*CuSeds
        gsNis = gsNis + SSeross*NiSeds
        gsAss = gsAss + SSeross*AsSeds
        gsPbs = gsPbs + SSeross*PbSeds
		   !if(kontroll)print*,'schwermetalle_kern Erosion gsPbs,SSeross=',gsPbs,SSeross
        gsCrs = gsCrs + SSeross*CrSeds
        gsFes = gsFes + SSeross*FeSeds
        gsHgs = gsHgs + SSeross*Hgseds
        gsMns = gsMns + SSeross*MnSeds
        gsUs  = gsUs  + SSeross*USeds
      endif ! Erosion takeing Place
	  
!     #############################################################
!     Clipping negativer Konzentrationen:
!     ##############################################################
      if(gsZns<0.0)gsZns = 0.0
	  if(glZns<0.0)glZns = 0.0
      if(gsCads<0.0)gsCads = 0.0
	  if(glCads<0.0)glCads = 0.0
      if(gsCus<0.0)gsCus = 0.0
	  if(glCus<0.0)glCus = 0.0
      if(gsNis<0.0)gsNis = 0.0
	  if(glNis<0.0)glNis = 0.0
      if(gsAss<0.0)gsAss = 0.0
	  if(glAss<0.0)glAss = 0.0
      if(gsPbs<0.0)gsPbs = 0.0
	  if(glPbs<0.0)glPbs = 0.0
      if(gsCrs<0.0)gsCrs = 0.0
	  if(glCrs<0.0)glCrs = 0.0
      if(gsFes<0.0)gsFes = 0.0
	  if(glFes<0.0)glFes = 0.0
      if(gsHgs<0.0)gsHgs = 0.0
	  if(glHgs<0.0)glHgs = 0.0
      if(gsMns<0.0)gsMns = 0.0
	  if(glMns<0.0)glMns = 0.0
      if(gsUs <0.0)gsUs =  0.0
	  if(glUs <0.0)glUs =  0.0

!     #############################################################
!     Neuverteilung gelöste <-> gesamte Konzentration nach dem Zeitschritt tflie 
!     ##############################################################

      hcSS = SSalgs ! hier jetzt mit der realen Schwebstoffkonzentration

      if(gsZns>0.0) glZns = gsZns/(1.+VTKoeffZn*hcSS/1000.)
      if(gsCads>0.0) glCads = gsCads/(1.+VTKoeffCad*hcSS/1000.)
      if(gsCus>0.0) glCus = gsCus/(1.+VTKoeffCu*hcSS/1000.)
      if(gsNis>0.0) glNis = gsNis/(1.+VTKoeffNi*hcSS/1000.)
      if(gsAss>0.0) glAss = gsAss/(1.+VTKoeffAs*hcSS/1000.)
      if(gsPbs>0.0) glPbs = gsPbs/(1.+VTKoeffPb*hcSS/1000.)
      if(gsCrs>0.0) glCrs = gsCrs/(1.+VTKoeffCr*hcSS/1000.)
      if(gsFes>0.0) glFes = gsFes/(1.+VTKoeffFe*hcSS/1000.)
      if(gsHgs>0.0) glHgs = gsHgs/(1.+VTKoeffHg*hcSS/1000.)
      if(gsMns>0.0) glMns = gsMns/(1.+VTKoeffMn*hcSS/1000.)
      if(gsUs>0.0) glUs = gsUs/(1.+VTKoeffU*hcSS/1000.)
 
      !if(kontroll)print*,jjj,' schwermetalle_kern ende glZns,gsZns,glPbs,gsPbs=',glZns,gsZns,glPbs,gsPbs
 
      end subroutine schwermetalle_kern
