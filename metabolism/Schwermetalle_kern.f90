! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !
!> balance of heavy metal concentrations
!! file: schwermetalle_kern.f90 zurück: \ref lnk_schwermetalle
subroutine schwermetalle_kern(hssalgs,SSalgs,hphs,vphs,SSeross,iformVert             &
                              ,anzZeits,sedsss,sedalks,sedalgs,sedalbs                                     &
                              ,gsZns,glZns,gsCads,glCads,gsCus,glCus,gsNis,glNis,gsAss,glAss,gsPbs,glPbs   &
                              ,gsCrs,glCrs,gsFes,glFes,gsHgs,glHgs,gsMns,glMns,gsUs,glUs                   &
                              ,kontroll,jjj)
   
   implicit none
   integer               :: anzZeits,iformVert
   real                  :: hssalgs,SSalgs,hphs,vphs,SSeross
   real                  :: sedsss,sedalks,sedalbs,sedalgs
   real                  :: glZns, gsZns, glCads, gsCads, glCus, gsCus, glNis, gsNis
   real                  :: glAss, gsAss, glPbs, gsPbs, glCrs, gsCrs, glFes, gsFes
   real                  :: glHgs, gsHgs, glMns, gsMns, glUs, gsUs
   ! 1-previous timestep(hssalgs,hphs) 2-current timestep(SSalgs,vphs) 3-Formula (17)
   real, dimension(3)    :: VTKoeffZn, VTKoeffCad, VTKoeffCu, VTKoeffNi
   real, dimension(3)    :: VTKoeffAs, VTKoeffPb, VTKoeffCr, VTKoeffFe
   real, dimension(3)    :: VTKoeffHg, VTKoeffMn, VTKoeffU
   real                  :: ZnSeds,CadSeds,CuSeds,NiSeds,AsSeds,PbSeds
   real                  :: CrSeds,FeSeds,Hgseds,MnSeds,USeds
   real                  :: Css,ph
   real                  :: factor
   
   logical, intent(in)   :: kontroll !< debugging
   integer, intent(in)   :: jjj      !< debugging
   
   ! kontrolle
   if(gsZns<glZns)print*,jjj,'gsZns<glZns',gsZns,glZns
   if(gsCads<glCads)print*,jjj,'gsCads<glCads',gsCads,glCads
   if(gsCus<glCus)print*,jjj,'gsCus<glCus',gsCus,glCus
   if(gsNis<glNis)print*,jjj,'gsNis<glNis',gsNis,glNis
   if(gsAss<glAss)print*,jjj,'gsAss<glAss',gsAss,glAss
   if(gsPbs<glPbs)print*,jjj,'gsPbs<glPbs',gsPbs,glPbs
   if(gsCrs<glCrs)print*,jjj,'gsCrs<glCrs',gsCrs,glCrs
   if(gsFes<glFes)print*,jjj,'gsFes<glFes',gsFes,glFes
   if(gsHgs<glHgs)print*,jjj,'gsHgs<glHgs',gsHgs,glHgs
   if(gsMns<glMns)print*,jjj,'gsMns<glMns',gsMns,glMns
   if(gsUs<glUs)print*,jjj,'gsUs<glUs',gsUs,glUs
   
   ! Berechnung der Verteilungskoeffizienten
   ! VTKoeffZn, VTKoeffCu, VTKoeffCad, VTKoeffNi
   ! VTKoeffAs, VTKoeffPb, VTKoeffCr, VTKoeffFe
   ! VTKoeffHg, VTKoeffMn, VTKoeffU
   ! VTKoff.. in l/g aus dem Verhältnis gesamt- und gelöster Schwermetall-Konz.
   ! Minimum von Schwebstoffkonzentration und pH-Wert für Berechnung von Verteilungskoeffizient und Sedimentbelastung
   Css = min(100.,hSSalgs)
   ph = max(4.,hphs)
   call Verteilungskoeff(Css,ph  &
                         ,VTKoeffZn(1),VTKoeffCu(1),VTKoeffCad(1),VTKoeffNi(1),VTKoeffAs(1),VTKoeffPb(1)    &
                         ,VTKoeffCr(1),VTKoeffFe(1),VTKoeffHg(1) ,VTKoeffMn(1), VTKoeffU(1)                 &
                         ,iformVert,kontroll ,jjj)
   factor= 1.0/(1.+VTKoeffAs(1)*Css)
   if((factor .gt. 1.0).or.(factor .le. 0.0))print*,jjj,' 1 falsch',VTKoeffAs(1),Css

   Css = min(100.,SSalgs)
   ph = max(4.,vphs)
   call Verteilungskoeff(Css,ph  &
                         ,VTKoeffZn(2),VTKoeffCu(2),VTKoeffCad(2),VTKoeffNi(2),VTKoeffAs(2),VTKoeffPb(2)    &
                         ,VTKoeffCr(2),VTKoeffFe(2),VTKoeffHg(2) ,VTKoeffMn(2), VTKoeffU(2)                 &
                         ,iformVert,kontroll ,jjj)
   factor= 1.0/(1.+VTKoeffAs(2)*Css)
   if((factor .gt. 1.0).or.(factor .le. 0.0))print*,jjj,' 2 falsch',VTKoeffAs(2),Css
   
   ! Formel (17) zugeflossene Verteilung
   Css = SSalgs/1000.
   ! wenn gel. Null ist, VTKoeff(2) nehmen
   VTKoeffZn(3) = VTKoeffZn(1)*Css
   VTKoeffCad(3)= VTKoeffCad(1)*Css
   VTKoeffCu(3) = VTKoeffCu(1)*Css
   VTKoeffNi(3) = VTKoeffNi(1)*Css
   VTKoeffAs(3) = VTKoeffAs(1)*Css
   VTKoeffPb(3) = VTKoeffPb(1)*Css
   VTKoeffCr(3) = VTKoeffCr(1)*Css
   VTKoeffFe(3) = VTKoeffFe(1)*Css
   VTKoeffHg(3) = VTKoeffHg(1)*Css
   VTKoeffMn(3) = VTKoeffMn(1)*Css
   VTKoeffU(3)  = VTKoeffU(1)*Css
   
   if(glZns>0.0)VTKoeffZn(3) = (gsZns/glZns)-1.
   if(glCads>0.0)VTKoeffCad(3) = (gsCads/glCads)-1.
   if(glCus>0.0)VTKoeffCu(3) = (gsCus/glCus)-1.
   if(glNis>0.0)VTKoeffNi(3) = (gsNis/glNis)-1.
   if(glAss>0.0)VTKoeffAs(3) = (gsAss/glAss)-1.
   if(glPbs>0.0)VTKoeffPb(3) = (gsPbs/glPbs)-1.
   if(glCrs>0.0)VTKoeffCr(3) = (gsCrs/glCrs)-1.
   if(glFes>0.0)VTKoeffFe(3) = (gsFes/glFes)-1.
   if(glHgs>0.0)VTKoeffHg(3) = (gsHgs/glHgs)-1.
   if(glMns>0.0)VTKoeffMn(3) = (gsMns/glMns)-1.
   if(glUs>0.0)VTKoeffU(3) = (gsUs/glUs)-1.
   
   if(VTKoeffZn(1)>0.0)VTKoeffZn(3) = VTKoeffZn(3)*(VTKoeffZn(2)/VTKoeffZn(1)) +1.0
   if(VTKoeffCad(1)>0.0)VTKoeffCad(3) = VTKoeffCad(3)*(VTKoeffCad(2)/VTKoeffCad(1)) +1.0
   if(VTKoeffCu(1)>0.0)VTKoeffCu(3) = VTKoeffCu(3)*(VTKoeffCu(2)/VTKoeffCu(1)) +1.0
   if(VTKoeffNi(1)>0.0)VTKoeffNi(3) = VTKoeffNi(3)*(VTKoeffNi(2)/VTKoeffNi(1)) +1.0
   if(VTKoeffAs(1)>0.0)VTKoeffAs(3) = VTKoeffAs(3)*(VTKoeffAs(2)/VTKoeffAs(1)) +1.0
   if(VTKoeffPb(1)>0.0)VTKoeffPb(3) = VTKoeffPb(3)*(VTKoeffPb(2)/VTKoeffPb(1)) +1.0
   if(VTKoeffCr(1)>0.0)VTKoeffCr(3) = VTKoeffCr(3)*(VTKoeffCr(2)/VTKoeffCr(1)) +1.0
   if(VTKoeffFe(1)>0.0)VTKoeffFe(3) = VTKoeffFe(3)*(VTKoeffFe(2)/VTKoeffFe(1)) +1.0
   if(VTKoeffHg(1)>0.0)VTKoeffHg(3) = VTKoeffHg(3)*(VTKoeffHg(2)/VTKoeffHg(1)) +1.0
   if(VTKoeffMn(1)>0.0)VTKoeffMn(3) = VTKoeffMn(3)*(VTKoeffMn(2)/VTKoeffMn(1)) +1.0
   if(VTKoeffU(1)>0.0)VTKoeffU(3) = VTKoeffU(3)*(VTKoeffU(2)/VTKoeffU(1)) +1.0
      
   if(VTKoeffZn(3)>0.0)VTKoeffZn(3) = 1.0/VTKoeffZn(3) 
   if(VTKoeffCad(3)>0.0)VTKoeffCad(3) = 1.0/VTKoeffCad(3) 
   if(VTKoeffCu(3)>0.0)VTKoeffCu(3) = 1.0/VTKoeffCu(3) 
   if(VTKoeffNi(3)>0.0)VTKoeffNi(3) = 1.0/VTKoeffNi(3) 
   if(VTKoeffAs(3)>0.0)VTKoeffAs(3) = 1.0/VTKoeffAs(3) 
   if(VTKoeffPb(3)>0.0)VTKoeffPb(3) = 1.0/VTKoeffPb(3) 
   if(VTKoeffCr(3)>0.0)VTKoeffCr(3) = 1.0/VTKoeffCr(3) 
   if(VTKoeffFe(3)>0.0)VTKoeffFe(3) = 1.0/VTKoeffFe(3) 
   if(VTKoeffHg(3)>0.0)VTKoeffHg(3) = 1.0/VTKoeffHg(3) 
   if(VTKoeffMn(3)>0.0)VTKoeffMn(3) = 1.0/VTKoeffMn(3) 
   if(VTKoeffU(3)>0.0)VTKoeffU(3) = 1.0/VTKoeffU(3) 
   ! Wertebereich beschränken
   VTKoeffZn(3) = max(0.0,min(1.0,VTKoeffZn(3)))
   VTKoeffCad(3)= max(0.0,min(1.0,VTKoeffCad(3)))
   VTKoeffCu(3) = max(0.0,min(1.0,VTKoeffCu(3)))
   VTKoeffNi(3) = max(0.0,min(1.0,VTKoeffNi(3)))
   VTKoeffAs(3) = max(0.0,min(1.0,VTKoeffAs(3)))
   VTKoeffPb(3) = max(0.0,min(1.0,VTKoeffPb(3)))
   VTKoeffCr(3) = max(0.0,min(1.0,VTKoeffCr(3)))
   VTKoeffFe(3) = max(0.0,min(1.0,VTKoeffFe(3)))
   VTKoeffHg(3) = max(0.0,min(1.0,VTKoeffHg(3)))
   VTKoeffMn(3) = max(0.0,min(1.0,VTKoeffMN(3)))
   VTKoeffU(3)  = max(0.0,min(1.0,VTKoeffU(3)))
      
   ! Berechnung der Sedimentbelastung ZnSeds bis USeds
   ! Hochzählen der erosionslosen Zeitschritte anzZeits
   call Sedimentbelastung(SSalgs,                 &
                          gsZns,glZns,ZnSeds,     &
                          gsCads,glCads,CadSeds,  &
                          gsCus,glCus,CuSeds,     &
                          gsNis,glNis,NiSeds,     &
                          gsAss,glAss,AsSeds,     &
                          gsPbs,glPbs,PbSeds,     &
                          gsCrs,glCrs,CrSeds,     &
                          gsFes,glFes,FeSeds,     &
                          gsHgs,glHgs,HgSeds,     &
                          gsMns,glMns,MnSeds,     &
                          gsUs,glUs,USeds,        &
                          anzZeits,SSeross,       &
                          kontroll ,jjj)
   ! Verringerung der Gesamt-Schwermetallkonz. durch Sedimentation
   Css = SSalgs
   if (gsZns > 0.0) gsZns = gsZns-(gsZns - glZns)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   if (gsCads > 0.0) gsCads = gsCads-(gsCads - glCads)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   if (gsCus > 0.0) gsCus = gsCus-(gsCus - glCus)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   if (gsNis > 0.0) gsNis = gsNis-(gsNis - glNis)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   if (gsAss > 0.0) gsAss = gsAss-(gsAss - glAss)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   if (gsPbs > 0.0) gsPbs = gsPbs-(gsPbs - glPbs)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   !if(kontroll)print*,'schwermetalle_kern Sedimentation glPbs,gsPbs,Css=',glPbs,gsPbs,Css
   if (gsCrs > 0.0) gsCrs = gsCrs-(gsCrs - glCrs)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   if (gsFes > 0.0) gsFes = gsFes-(gsFes - glFes)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   if (gsHgs > 0.0) gsHgs = gsHgs-(gsHgs - glHgs)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   if (gsMns > 0.0) gsMns = gsMns-(gsMns - glMns)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   if (gsUs > 0.0) gsUs = gsUs-(gsUs - glUs)*((sedSSs+sedalks+sedalbs+sedalgs)/Css)
   
   ! Erhöhung der Gesamt-Schwermetallkonz. durch Erosion
   if (SSeross > 0.0) then
      gsZns = gsZns + SSeross*ZnSeds
      gsCads = gsCads+ SSeross*CadSeds
      gsCus = gsCus + SSeross*CuSeds
      gsNis = gsNis + SSeross*NiSeds
      gsAss = gsAss + SSeross*AsSeds
      gsPbs = gsPbs + SSeross*PbSeds
      !if(kontroll)print*,'schwermetalle_kern Erosion gsPbs,SSeross=',gsPbs,SSeross
      gsCrs = gsCrs + SSeross*CrSeds
      gsFes = gsFes + SSeross*FeSeds
      gsHgs = gsHgs + SSeross*Hgseds
      gsMns = gsMns + SSeross*MnSeds
      gsUs = gsUs  + SSeross*USeds
   endif
   
   ! Clipping negativer Konzentrationen:
   if (gsZns < 0.0)gsZns = 0.0
   if (gsCads < 0.0)gsCads = 0.0
   if (gsCus < 0.0)gsCus = 0.0
   if (gsNis < 0.0)gsNis = 0.0
   if (gsAss < 0.0)gsAss = 0.0
   if (gsPbs < 0.0)gsPbs = 0.0
   if (gsCrs < 0.0)gsCrs = 0.0
   if (gsFes < 0.0)gsFes = 0.0
   if (gsHgs < 0.0)gsHgs = 0.0
   if (gsMns < 0.0)gsMns = 0.0
   if (gsUs  < 0.0)gsUs  = 0.0
   
   ! Neuverteilung gelöste <-> gesamte Konzentration nach dem Zeitschritt tflie
   glZns = gsZns*VTKoeffZn(3)
   glCads = gsCads*VTKoeffCad(3)
   glCus = gsCus*VTKoeffCu(3)
   glNis = gsNis*VTKoeffNi(3)
   glAss = gsAss*VTKoeffAs(3)
   glPbs = gsPbs*VTKoeffPb(3)
   glCrs = gsCrs*VTKoeffCr(3)
   glFes = gsFes*VTKoeffFe(3)
   glHgs = gsHgs*VTKoeffHg(3)
   glMns = gsMns*VTKoeffMn(3)
   glUs  = gsUs *VTKoeffU(3)
   
end subroutine schwermetalle_kern
