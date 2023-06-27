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

subroutine sedflux_huelle(i)

   use module_alloc_dimensions
   use module_aparam
   use modell
   use QSimDatenfelder
   
   implicit none
   
   integer i,j,nk
   ! für Sediment
   integer, dimension(azStrs)            :: mStas, mSs, abfr, mStra, nbuhn
   real, dimension(azStrs,20)            :: aschif, eschif
   real, dimension(azStrs,1000)          :: raua, Stakm, vmq, Hmq, bvmq, bHmq, SedOMb, dKornb, w2, w2b
   iglob = (i+meinrang*part)
   nk = (i-1)*number_plankt_vari
   control = iglob == kontrollknoten
   if (control)print*,'sedflux: nk,i,iglob = ',nk,i,iglob
   !!(\ref tiefe,\ref vmitt,\ref rau,\ref sedalg_mq,\ref hsedom,\ref hw2,\ref hbedgs,\ref hsedvvert
   tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
   vmitt(1:2) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag; randbedingungen.h
   rau(1:2) = strickler( zone(point_zone(iglob))%reib , tiefe(1) )
   sedAlg_MQ(1,1:2) = benthic_distribution_p(52+(i-1)*number_benth_distr) !! noch nicht verwendet ???
   hsedom(1,1:2) = zone(point_zone(iglob))%sediflux%sedom! SedOM <- POMz Anteil des organischen Materials im Sediment (0-1)
   hw2(1,1:2) = 0.0
   !#    ****************************************
   !#    Berechnung der SedimentKenngroessen
   !#    ****************************************
   !#    lesen von modellg.txt: aPOM(mstr,mZ),ePOM(mstr,mZ),POMz(mstr,mZ),BedGSz(mstr,mz),Sedvvertz(mstr,mz)
   !#    call Sediment(abfr,mStra,Stakm,mStas,mSs,aschif,eschif &
   !#                 ,SedOM,SedOMb,dKorn,dKornb    &
   !#                 ,raua,vmq,Hmq,nbuhn,bvmq,bHmq &
   !#                 ,jsed,w2,w2b) ... ruft ...
   !#    subroutine Sed_POM(tiefe1,ust,n,BSBC,PhytoC,GesSS,SedOM,dKorn,SedOMb,dKornb,fsch,fOM_OC,mstr,mSta,jsed,w2,w2b)
   !#    subroutine Sed_POM:    w2(mstr,mSta) = 0.74e-5
   !#vmitt1 = vmq(mstr,mSta)
   !#tiefe1 = Hmq(mstr,mSta)
   !#ust = ((raun*g)/(tiefe1**0.16667))*vmitt1
   abfr(1) = 1
   !mstr=1
   mStra(1) = 1
   Stakm(1,1) = 1.0
   mStas(1) = 1
   mSs(1) = zone(point_zone(iglob))%schiff%schifffahrts_zone! Schiffe in dieser zone?
   aschif(1,1) = 0.0
   eschif(1,1) = 2.0
   nbuhn(1) = 0
   SedOMb(1,1:2) = hsedom(1,1:2)
   !hdkorn(1,1) = 0.02! ##### ???,dKorn
   raua(1,1:2) = rau(1)
   vmq(1,1:2) = 1.0 !#### woher MQ-Verhältinsse in 3D Tide???
   Hmq(1,1:2) = 2.0
   bvmq(1,1:2) = 1.0
   bHmq(1,1:2) = 2.0
   if (control)print*,'vor Sediment: hsedom(1,1:2),hw2(1,1:2),hdkorn(1,1:2) = ',hsedom(1,1:2),hw2(1,1:2),hdkorn(1,1:2)
   call Sediment(1,mStra,Stakm,mStas,mSs,aschif,eschif          &
                 ,hsedom,SedOMb,hdkorn,dKornb                   &
                 ,raua,vmq,Hmq,nbuhn,bvmq,bHmq                  &
                 ,0,hw2,w2b)
   hsedom(1,2) = hsedom(1,1)
   hw2(1,2) = hw2(1,1)
   hdkorn(1,2) = hdkorn(1,1)
   if (control)print*,'nach Sediment: hsedom(1,1:2),hw2(1,1:2),hdkorn(1,1:2) = ',hsedom(1,1:2),hw2(1,1:2),hdkorn(1,1:2)
   if (control)print*,'nach Sediment: raua,vmq,Hmq = ',raua(1,1:2),vmq(1,1:2),Hmq(1,1:2)
   if (zone(point_zone(iglob))%sediflux%kornd > 0.0)  hdkorn(1,1:2) = zone(point_zone(iglob))%sediflux%kornd ! einlesen optional, in 3D neu
   !if(burial(point_zone(iglob)).gt. 0.0) hw2(1,1:2) = burial(point_zone(iglob))   ! von sed_pom() auf 0.74e-5 gesetzt !!!
   hbedgs(1,1:2) = zone(point_zone(iglob))%sediflux%bedgs ! BedGSz  Bedeckungsgrad der Sohle mit Sediment (0-1)
   hsedvvert(1,1:2) = zone(point_zone(iglob))%sediflux%sedvvert ! Sedvvertz  volumenbezogene Eindringgeschwindigkeit ins Sediment mm/h | ust???
   if (control)print*,'sedflux_huelle: hdkorn,hbedgs,hsedom,hw2 = ',hdkorn(1,1:2),hbedgs(1,1:2),hsedom(1,1:2),hw2(1,1:2)
   !!,\ref hdkorn,\ref vo2,\ref vno3,\ref vnh4,\ref gelP,\ref tempw,\ref anze,\ref mstr           &\n
   !#  subroutine Sed_POM(tiefe1,ust,n,BSBC,PhytoC,GesSS,SedOM,dKorn,SedOMb,dKornb,fsch,fOM_OC,mstr,mSta,jsed,w2,w2b)
   !   Berechnung des Mittleren Korndurchmessers in mm
   !   dKorn(mstr,mSta) = min(100.,0.0047*exp(64.89*ust))
   !hdkorn(1,1) = 0.02! ##### ???,dKorn
   vo2(1) = planktonic_variable_p( 2+nk) ! Sauerstoffgehalt tiefengemittelt
   vo2(2) = vo2(1)
   vno3(1) = planktonic_variable_p(5+nk)  ! nitrat
   vno3(2) = vno3(1)
   vNH4(1) = planktonic_variable_p(3+nk)  ! ammonium
   vNH4(2) = vNH4(1)
   gelp(1) = planktonic_variable_p(6+nk)  ! gelöster ortho-Phosphat-Phosphor tiefengemittelt
   gelp(2) = gelp(1)
   tempw(1) = planktonic_variable_p(1+nk)    ! Wasser-Temperatur
   tempw(2) = tempw(1)
   anze = 1          ! Anzahl der Profile im aktuellen Strang
   mstr = 1          ! Strangzähler | nur ein Profil in einem Strang
   !!,\ref hjno3,\ref hjnh4,\ref hjpo4,\ref hjo2,\ref hjn2,\ref sedalk,\ref sedalg,\ref sedalb
   hJNO3(1,1) = benthic_distribution_p(35+(i-1)*number_benth_distr) ! Nitrat-Freisetzung aus dem Sediment
   hJNO3(1,2) = hJNO3(1,1)
   hJNH4(1,1) = benthic_distribution_p(36+(i-1)*number_benth_distr) ! Ammonium-Freisetzung aus dem Sediment
   hJNH4(1,2) = hJNH4(1,1)
   hJPO4(1,1) = benthic_distribution_p(32+(i-1)*number_benth_distr) ! Phosphat-Freisetzung aus dem Sediment
   hJPO4(1,2) = hJPO4(1,1)
   hJO2(1,1) = benthic_distribution_p(8+(i-1)*number_benth_distr) ! Sauerstoffzehrung des Sediments gO2/m² und Zeitschritt
   hJO2(1,2) = hJO2(1,1)
   hJN2(1,1) = benthic_distribution_p(47+(i-1)*number_benth_distr) ! N2 Freisetzung aus dem Sediment
   hJN2(1,2) = hJN2(1,1)
   sedalk(1) = benthic_distribution_p(26+(i-1)*number_benth_distr) ! Sedimentierte Menge an Kiesel-Algen
   sedalk(2) = sedalk(1)
   sedalg(1) = benthic_distribution_p(27+(i-1)*number_benth_distr) ! Sedimentierte Menge an Grün-Algen
   sedalg(2) = sedalg(1)
   sedalb(1) = benthic_distribution_p(28+(i-1)*number_benth_distr) ! Sedimentierte Menge an Blau-Algen
   sedalb(2) = sedalb(1)
   !!\ref sedss_mq,\ref knh4e,\ref kapn3e
   !! ##### sedss_mq
   sedss_mq(1,1) = sedAlg_MQ(1,1) ! 0.0 !! ##### noch nicht verwendet ???
   sedss_mq(1,2) = sedss_mq(1,1)
   ! direkt aus QSimDatenfelder knh4e   !  NH4-Umsatzgeschw. im Sediment
   ! direkt aus QSimDatenfelder kapn3e  !  Denitrifikationsgeschw. im Sediment
   !!\ref tflie,\ref ilbuhn,\ref itags,\ref monats,\ref uhrz,\ref vo2z,\ref vnh4z,\ref vno3z
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   ilbuhn = 0          ! keine Buhnen
   itags = tag           ! Tag im Monat module::modell zeitsekunde()
   monats = monat        ! Monat im Jahr module::modell zeitsekunde()
   uhrz = uhrzeit_stunde ! Uhrzeit module::modell zeitsekunde()
   do j = 1,num_lev
      vo2z(j,1) = plankt_vari_vert_p(j+(2-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
      vo2z(j,2) = vo2z(j,1)
      vNH4z(j,1) = plankt_vari_vert_p(j+(3-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
      vNH4z(j,2) = vNH4z(j,1) ! Ammonium-Stickstoffkonzentration g/m³ tiefenaufgelöst
      vno3z(j,1) = plankt_vari_vert_p(j+(5-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
      vno3z(j,2) = vno3z(j,1) ! Nitrat-Stickstoffkonzentration g/m³ tiefenaufgelöst
      !!\ref gelpz,\ref nkzs,\ref sorpcape,\ref klange                                &\n
      gelpz(j,1) = plankt_vari_vert_p(j+(6-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
      gelpz(j,2) = gelpz(j,1)
   enddo ! alle tiefenlevels
   nkzs(1) = 1         ! nur eine Tiefenschicht
   nkzs(2) = nkzs(1)
   ! direkt aus QSimDatenfelder sorpcape ! SorptionsKapazität für Phosphor
   ! direkt aus QSimDatenfelder klange   ! Langmuirkoeffizient für Phosphorsorption
   !!\ref kdnh3e,\ref fpoc1e,\ref fpoc2e,\ref orgcsd_abb,\ref hcd,\ref jdoc1,\ref jdoc2
   ! direkt aus QSimDatenfelder kdnh3e   ! Partitionskoeffizient für Ammonium
   ! direkt aus QSimDatenfelder fpoc1e   ! leichtabbaubarer Anteil d. Sedimentkohlenstoffs
   ! direkt aus QSimDatenfelder fpoc2e   ! schwerabbaubarer Anteil d. Sedimentkohlenstoffs
   orgCsd_abb(1,1) = benthic_distribution_p(51+(i-1)*number_benth_distr) ! sedimentiertes biologisch abbaubares organ. Material
   orgCsd_abb(1,2) = orgCsd_abb(1,1)
   hcd(1,1,1:2) = planktonic_variable_p(37+nk) ! leicht abbaubare gelöste organische C-Verbindungen, CD(1,1)
   hcd(1,2,1:2) = planktonic_variable_p(38+nk) ! schwer abbaubare gelöste organische C-Verbindungen, CD(2,1)
   JDOC1(1) = benthic_distribution_p(48+(i-1)*number_benth_distr) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, leicht abbaubar
   JDOC1(2) = JDOC1(1)
   JDOC2(1) = benthic_distribution_p(49+(i-1)*number_benth_distr) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, schwer abbaubar
   JDOC2(2) = JDOC2(1)
   !!\ref q_nk,\ref q_pk,\ref q_ng,\ref q_pg,\ref q_nb                                  &\n
   Q_NK(1) = planktonic_variable_p(30+nk) ! Stickstoffanteil der Algenbiomasse kiesel
   Q_NK(2) = Q_NK(1)
   Q_PK(1) = planktonic_variable_p(31+nk) ! Phosphoranteil der Kiesel-Algenbiomasse
   Q_PK(2) = Q_PK(1)
   Q_NG(1) = planktonic_variable_p(33+nk) ! Stickstoffanteil der Algenbiomasse gruen
   Q_NG(2) = Q_NG(1)
   Q_PG(1) = planktonic_variable_p(34+(i-1)*number_plankt_vari) ! Phosphornteil der Algenbiomasse gruen
   Q_PG(2) = Q_PG(1)
   Q_NB(1) = planktonic_variable_p(35+nk) ! Stickstoffanteil der Algenbiomasse blau
   Q_NB(2) = Q_NB(1)
   !!\ref q_pb,\ref pl0,\ref nl0,\ref si,\ref hsised,\ref hjsi,\ref aki,\ref agr,\ref abl
   Q_PB(1) = planktonic_variable_p(36+nk) ! Phosphoranteil der Blau-Algenbiomasse
   Q_PB(2) = Q_PB(1)
   pl0(1) = planktonic_variable_p(58+nk)  !  P/C Verhältnis von Phosphor zu Kohlenstoff in organischem Material
   pl0(2) = pl0(1)
   nl0(1) = planktonic_variable_p(57+nk) ! Verhältnis von Stickstoff zu Kohlenstoff in organischem Material
   nl0(2) = nl0(1)
   si(1) = planktonic_variable_p( 7+nk)  ! silikat-Silizium-Konzentration (tiefengemittelt)
   si(2) = si(1)
   hsised(1,1:2) = benthic_distribution_p(2+(i-1)*number_benth_distr) !  sised aus algaeski = Menge an Silikat an der Gewässersohle infolge sedimentierter Algen
   hJSi(1,1) = benthic_distribution_p(46+(i-1)*number_benth_distr) ! Silizium-Flux aus dem Sediment
   hJSi(1,2) = hJSi(1,1)
   aki(1) = planktonic_variable_p(8+nk) ! Biomasse an Kiesel-Algen
   aki(2) = aki(1)
   agr(1) = planktonic_variable_p(9+nk) ! Biomasse an gruen-Algen
   agr(2) = agr(1)
   abl(1) = planktonic_variable_p(10+nk) ! Biomasse an Blau-Algen
   abl(2) = abl(1)
   !!\ref chlaki,\ref chlagr,\ref chlabl,\ref hflun3,\ref ilang,\ref iwied,\ref ynmx1e,\ref stks1e     &\n
   Chlaki(1) = planktonic_variable_p(12+nk)  ! Chlorophyl in Kieselalgen muegchla/l
   Chlaki(2) = Chlaki(1)
   chlagr(1) = planktonic_variable_p(13+nk)  ! Chlorophyl in gruenalgen muegchla/l
   chlagr(2) = chlagr(1)
   chlabl(1) = planktonic_variable_p(14+nk)  ! Chlorophyl in Blaualgen muegchla/l
   chlabl(2) = chlabl(1)
   hFluN3(1,1) = benthic_distribution_p(37+(i-1)*number_benth_distr) ! Ausgabe NitratFlux Wasser/Sediment in mgN/(l*h) aus ncyc()
   hFluN3(1,2) = hFluN3(1,1)
   iwied = 0      ! unbenutzte Variable
   ! direkt aus QSimDatenfelder ynmx1e  !  Max. Wachstum Nitrosomonas
   ! direkt aus QSimDatenfelder ystks1e !  Halbsättigung Nitrosomonas
   !!\ref obsb,\ref ocsb,  \ref control ,*jjj* ) !!wy
   obsb(1) = planktonic_variable_p(17+(i-1)*number_plankt_vari) ! C-BSB5
   obsb(2) = obsb(1)
   ocsb(1) = planktonic_variable_p(18+(i-1)*number_plankt_vari) ! CSB
   ocsb(2) = ocsb(1)
   !if(control)print*,'sedflux vorher: vO2,hJSi,hJNO3,hJNH4,hJN2=',vo2(1),hJSi(1,1),hJNO3(1,1),hJNH4(1,1),hJN2(1,1)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   call sedflux (tiefe,vmitt,rau,sedAlg_MQ,hSedOM,hw2,hBedGS,hsedvvert,hdKorn,vO2,vNO3,vNH4,gelP,Tempw,anze,mstr  &
                 ,hJNO3,hJNH4,hJPO4,hJO2,hJN2,sedalk,sedalg,sedalb,sedSS_MQ,KNH4,KapN3                            &
                 ,tflie,ilbuhn,itags,monats,uhrz,vo2z,vnh4z,vno3z,gelPz,nkzs,SorpCap,Klang                        &
                 ,KdNh3,fPOC1,fPOC2,orgCsd_abb,hCD,JDOC1,JDOC2,Q_NK,Q_PK,Q_NG,Q_PG,Q_NB                           &
                 ,Q_PB,pl0,nl0,Si,hSised,hJSi,aki,agr,abl,Chlaki,Chlagr,Chlabl,hFluN3,ilang,iwied,YNMAX1,STKS1    &
                 ,obsb,ocsb,  control ,iglob ) !!wy
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe:
   if (control)print*,'sedflux nacher: ,hJO2,vo2(1),hJSi,hJNO3,hJNH4,hJPO4(1,1),hJN2 = ' &
       ,hJO2(1,1),vo2(1),hJSi(1,1),hJNO3(1,1),hJNH4(1,1),hJPO4(1,1),hJN2(1,1)
   benthic_distribution_p(35+(i-1)*number_benth_distr) = hJNO3(1,1) ! Nitrat-Freisetzung aus dem Sediment
   benthic_distribution_p(36+(i-1)*number_benth_distr) = hJNH4(1,1) ! Ammonium-Freisetzung aus dem Sediment
   benthic_distribution_p(32+(i-1)*number_benth_distr) = hJPO4(1,1) ! Phosphat-Freisetzung aus dem Sediment
   benthic_distribution_p( 8+(i-1)*number_benth_distr) = hJO2(1,1)  ! Sauerstoffzehrung des Sediments gO2/(m²*d) und Zeitschritt
   benthic_distribution_p(47+(i-1)*number_benth_distr) = hJN2(1,1)  ! N2 Freisetzung aus dem Sediment
   benthic_distribution_p(48+(i-1)*number_benth_distr) = JDOC1(1)   ! Flux gelöster org. Kohlenstoffe aus dem Sediment, leicht abbaubar
   benthic_distribution_p(49+(i-1)*number_benth_distr) = JDOC2(1)   ! Flux gelöster org. Kohlenstoffe aus dem Sediment, schwer abbaubar
   benthic_distribution_p(46+(i-1)*number_benth_distr) = hJSi(1,1)  ! Silizium-Flux aus dem Sediment
   benthic_distribution_p(2+(i-1)*number_benth_distr) = hsised(1,1) ! Siliziumgehalt im Sediment (wird auch von algaeski verändert) Reduktion durch hJSi-Freisetzung
   !kein Rckgabewert sedalk(1)= ! Sedimentierte Menge an Kiesel-Algen
   !kein Rckgabewert sedalg(1)= ! Sedimentierte Menge an Grün-Algen
   !kein Rckgabewert sedalb(1)= ! Sedimentierte Menge an Blau-Algen
   planktonic_variable_p( 8+nk) = aki(1) ! Biomasse an Kiesel-Algen
   planktonic_variable_p( 9+nk) = agr(1) ! Biomasse an gruen-Algen
   planktonic_variable_p(10+nk) = abl(1) ! Biomasse an Blau-Algen
   planktonic_variable_p(12+nk) = chlaki(1)  ! Chlorophyl in Kieselalgen muegchla/l
   planktonic_variable_p(13+nk) = chlagr(1)  ! Chlorophyl in gruenalgen muegchla/l
   planktonic_variable_p(14+nk) = chlabl(1)  ! Chlorophyl in Blaualgen muegchla/l
   return
end subroutine sedflux_huelle
!+++++ Stoffflüsse in den Wasserkörper++++++++++
!
! hJNO3(mstr,ior) = s * (NO3(1) - NO30) ! + Flux vom Sediment in den Wasserkörper
! hJNH4(mstr,ior) = s * (NH4(1) - NH40)
! hJPO4(mstr,ior) = s * (PO4(1) - PO40)
! JDOC1(ior) = s * (DOC1(1) - DOC0(1))
! JDOC2(ior) = s * (DOC1(2) - DOC0(2))
! hJSi(mstr,ior) = s *(Sipor(1) - Si0)
! hJO2(mstr,ior) = SOD
! JCH4aq(ior) = s * (CH41 - CH40)
! hJN2(mstr,ior) = s * (N2(1) - N20)
