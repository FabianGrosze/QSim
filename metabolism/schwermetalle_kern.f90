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
                              ,kontroll,jjj,meinrang)
   
   implicit none
   integer               :: anzZeits,iformVert,meinrang
   real                  :: hssalgs,SSalgs,hphs,vphs,SSeross
   real                  :: sedsss,sedalks,sedalbs,sedalgs
   real                  :: glZns, gsZns, glCads, gsCads, glCus, gsCus, glNis, gsNis
   real                  :: glAss, gsAss, glPbs, gsPbs, glCrs, gsCrs, glFes, gsFes
   real                  :: glHgs, gsHgs, glMns, gsMns, glUs, gsUs
   ! 1-previous timestep(hssalgs,hphs) 2-current timestep(SSalgs,vphs) 3-Formula (17)
   real, dimension(2)    :: VTKoeffZn, VTKoeffCad, VTKoeffCu, VTKoeffNi
   real, dimension(2)    :: VTKoeffAs, VTKoeffPb, VTKoeffCr, VTKoeffFe
   real, dimension(2)    :: VTKoeffHg, VTKoeffMn, VTKoeffU
   real                  :: ZnSeds,CadSeds,CuSeds,NiSeds,AsSeds,PbSeds
   real                  :: CrSeds,FeSeds,Hgseds,MnSeds,USeds
   real                  :: Css,ph
   
   logical, intent(in)   :: kontroll !< debugging
   integer, intent(in)   :: jjj      !< debugging
   
   logical  ,parameter  :: siebzehn = .FALSE. ! .TRUE. ! Formel 17 für Verteilungskoeffizienten aus Einleitungen ! .FALSE.

   if(kontroll.and.(jjj==316))then
      print*,jjj,'schwermetalle_kern vorher SSalg,pH=',hSSalgs,vphs,'Zn,Cad,Cu,Ni,As,Pb,Cr,Fe,Hg,Mn,U'
      print*,'gs=',gsZns,gsCads,gsCus,gsNis,gsAss,gsPbs,gsCrs,gsFes,gsHgs,gsMns,gsUs
      print*,'gl=',glZns,glCads,glCus,glNis,glAss,glPbs,glCrs,glFes,glHgs,glMns,glUs
   endif
   
   !external :: sedimentbelastung, verteilungskoeff
   
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
                         ,iformVert,kontroll,jjj,meinrang)
   Css = min(100.,SSalgs)
   ph = max(4.,vphs)
   call Verteilungskoeff(Css,ph  &
                         ,VTKoeffZn(2),VTKoeffCu(2),VTKoeffCad(2),VTKoeffNi(2),VTKoeffAs(2),VTKoeffPb(2)    &
                         ,VTKoeffCr(2),VTKoeffFe(2),VTKoeffHg(2) ,VTKoeffMn(2), VTKoeffU(2)                 &
                         ,iformVert,kontroll,jjj,meinrang)
                         
   if(kontroll.and.(jjj==316))then
      print*,jjj,'schwermetalle_kern Verteilungskoeff: hSSalgs,hphs,SSalgs,vphs=',hSSalgs,hphs,SSalgs,vphs
      print*,'VTKoeffZn(1),VTKoeffCu(1)VTKoeffFe(1)',VTKoeffZn(1),VTKoeffCu(1),VTKoeffFe(1)
      print*,'VTKoeffZn(2),VTKoeffCu(2)VTKoeffFe(2)',VTKoeffZn(2),VTKoeffCu(2),VTKoeffFe(2)
   endif

   ! formula (17) take into account inflowing distribution total/dissolved
   if (siebzehn) then
      Css = SSalgs/1000.0
      if((glZns>0.0).and.(Css>0.0))                  VTKoeffZn(2) = (((gsZns/glZns)-1.)/Css)
      if((VTKoeffZn(1)>0.0).and.(VTKoeffZn(2)>0.0))  VTKoeffZn(2) = VTKoeffZn(2)*(VTKoeffZn(2)/VTKoeffZn(1))
      if((glCads>0.0).and.(Css>0.0))                 VTKoeffCad(2)= (((gsCads/glCads)-1.)/Css)
      if((VTKoeffCad(1)>0.0).and.(VTKoeffCad(2)>0.0))VTKoeffCad(2)= VTKoeffCad(2)*(VTKoeffCad(2)/VTKoeffCad(1))
      if((glCus>0.0).and.(Css>0.0))                  VTKoeffCu(2) = (((gsCus/glCus)-1.)/Css)
      if((VTKoeffCu(1)>0.0).and.(VTKoeffCu(2)>0.0))  VTKoeffCu(2) = VTKoeffCu(2)*(VTKoeffCu(2)/VTKoeffCu(1))
      if((glNis>0.0).and.(Css>0.0))                  VTKoeffNi(2) = (((gsNis/glNis)-1.)/Css)
      if((VTKoeffNi(1)>0.0).and.(VTKoeffNi(2)>0.0))  VTKoeffNi(2) = VTKoeffNi(2)*(VTKoeffNi(2)/VTKoeffNi(1))
      if((glAss>0.0).and.(Css>0.0))                  VTKoeffAs(2) = (((gsAss/glAss)-1.)/Css)
      if((VTKoeffAs(1)>0.0).and.(VTKoeffAs(2)>0.0))  VTKoeffAs(2) = VTKoeffAs(2)*(VTKoeffAs(2)/VTKoeffAs(1))
      if((glPbs>0.0).and.(Css>0.0))                  VTKoeffPb(2) = (((gsPbs/glPbs)-1.)/Css)
      if((VTKoeffPb(1)>0.0).and.(VTKoeffPb(2)>0.0))  VTKoeffPb(2) = VTKoeffPb(2)*(VTKoeffPb(2)/VTKoeffPb(1))
      if((glCrs>0.0).and.(Css>0.0))                  VTKoeffCr(2) = (((gsCrs/glCrs)-1.)/Css)
      if((VTKoeffCr(1)>0.0).and.(VTKoeffCr(2)>0.0))  VTKoeffCr(2) = VTKoeffCr(2)*(VTKoeffCr(2)/VTKoeffCr(1))
      if((glFes>0.0).and.(Css>0.0))                  VTKoeffFe(2) = (((gsFes/glFes)-1.)/Css)
      if((VTKoeffFe(1)>0.0).and.(VTKoeffFe(2)>0.0))  VTKoeffFe(2) = VTKoeffFe(2)*(VTKoeffFe(2)/VTKoeffFe(1))
      if((glHgs>0.0).and.(Css>0.0))                  VTKoeffHg(2) = (((gsHgs/glHgs)-1.)/Css)
      if((VTKoeffHg(1)>0.0).and.(VTKoeffHg(2)>0.0))  VTKoeffHg(2) = VTKoeffHg(2)*(VTKoeffHg(2)/VTKoeffHg(1))
      if((glMns>0.0).and.(Css>0.0))                  VTKoeffMn(2) = (((gsMns/glMns)-1.)/Css)
      if((VTKoeffMn(1)>0.0).and.(VTKoeffMn(2)>0.0))  VTKoeffMn(2) = VTKoeffMn(2)*(VTKoeffMn(2)/VTKoeffMn(1))
      if((glUs >0.0).and.(Css>0.0))                  VTKoeffU(2)  = (((gsUs/glUs)-1.)/Css)
      if((VTKoeffU(1)>0.0).and.(VTKoeffU(2)>0.0))    VTKoeffU(2)  = VTKoeffU(2)*(VTKoeffU(2)/VTKoeffU(1))
   endif
   
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
   if (gsZns <= 0.0)gsZns = 0.0
   if (glZns <= 0.0)glZns = 0.0
   if (gsCads <= 0.0)gsCads = 0.0
   if (glCads <= 0.0)glCads = 0.0
   if (gsCus <= 0.0)gsCus = 0.0
   if (glCus <= 0.0)glCus = 0.0
   if (gsNis <= 0.0)gsNis = 0.0
   if (glNis <= 0.0)glNis = 0.0
   if (gsAss <= 0.0)gsAss = 0.0
   if (glAss <= 0.0)glAss = 0.0
   if (gsPbs <= 0.0)gsPbs = 0.0
   if (glPbs <= 0.0)glPbs = 0.0
   if (gsCrs <= 0.0)gsCrs = 0.0
   if (glCrs <= 0.0)glCrs = 0.0
   if (gsFes <= 0.0)gsFes = 0.0
   if (glFes <= 0.0)glFes = 0.0
   if (gsHgs <= 0.0)gsHgs = 0.0
   if (glHgs <= 0.0)glHgs = 0.0
   if (gsMns <= 0.0)gsMns = 0.0
   if (glMns <= 0.0)glMns = 0.0
   if (gsUs <= 0.0)gsUs = 0.0
   if (glUs <= 0.0)glUs = 0.0
   
   ! Neuverteilung gelöste <-> gesamte Konzentration nach dem Zeitschritt tflie
   Css = SSalgs/1000.0
   if (gsZns > 0.0) glZns = gsZns/(1.+VTKoeffZn(2)*Css)
   if (gsCads > 0.0) glCads = gsCads/(1.+VTKoeffCad(2)*Css)
   if (gsCus > 0.0) glCus = gsCus/(1.+VTKoeffCu(2)*Css)
   if (gsNis > 0.0) glNis = gsNis/(1.+VTKoeffNi(2)*Css)
   if (gsAss > 0.0) glAss = gsAss/(1.+VTKoeffAs(2)*Css)
   if (gsPbs > 0.0) glPbs = gsPbs/(1.+VTKoeffPb(2)*Css)
   if (gsCrs > 0.0) glCrs = gsCrs/(1.+VTKoeffCr(2)*Css)
   if (gsFes > 0.0) glFes = gsFes/(1.+VTKoeffFe(2)*Css)
   if (gsHgs > 0.0) glHgs = gsHgs/(1.+VTKoeffHg(2)*Css)
   if (gsMns > 0.0) glMns = gsMns/(1.+VTKoeffMn(2)*Css)
   if (gsUs > 0.0) glUs = gsUs/(1.+VTKoeffU(2)*Css)
   
   if(kontroll.and.(jjj==316))then
      print*,jjj,'schwermetalle_kern Ende VTKoeffZn(2),VTKoeffCu(2),VTKoeffFe(2)=',VTKoeffZn(2),VTKoeffCu(2),VTKoeffFe(2)
      print*,'Zn,Cad,Cu,Ni,As,Pb,Cr,Fe,Hg,Mn,U:'
      print*,'gs=',gsZns,gsCads,gsCus,gsNis,gsAss,gsPbs,gsCrs,gsFes,gsHgs,gsMns,gsUs
      print*,'gl=',glZns,glCads,glCus,glNis,glAss,glPbs,glCrs,glFes,glHgs,glMns,glUs
   endif

end subroutine schwermetalle_kern
