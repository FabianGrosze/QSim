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
!> Das module ph_module widmet sich dem \ref lnk_ph (sauer <-> basisch)
!! \n\n
!! zurück: \ref index; Quelle: ph_huelle.f95
!
!      module ph_module
!      implicit none
!      save
!      PUBLIC :: ph_huelle , ini_ph
!      CONTAINS
!> Die Subroutine ph_huelle() dient dem Aufruf der QSim-subroutine ph(). \n\n
!! Diese dient der Berechnung des \ref lnk_ph
!! \n\n
!! Quelle: ph_huelle.f95
subroutine ph_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam, only : caki, cabl, cagr
   implicit none
   integer :: i
   real tiefes,raus,flaes
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe:
   ! planktische variablen
   !mw(1:2) = planktonic_variable_p(62+(i-1)*number_plankt_vari) ! m-Wert
   !pw(1:2) = planktonic_variable_p(63+(i-1)*number_plankt_vari) ! p-Wert
   !ca(1:2) = planktonic_variable_p(64+(i-1)*number_plankt_vari) ! Calium ?
   !lf(1:2) = planktonic_variable_p(65+(i-1)*number_plankt_vari) ! Leitfähigkeit
   !vph(1:2) = planktonic_variable_p(66+(i-1)*number_plankt_vari) ! der zu berechnende PH-Wert
   !vco2(1:2) = transfer_quantity_p(26+(i-1)*number_trans_quant) ! Kohlendioxyd
   !tempw(1:2) = planktonic_variable_p( 1+(i-1)*number_plankt_vari)  ! Wassertemperatur
   !tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   ! Hydraulik
   !rau(1:2)= strickler( zone(point_zone(iglob))%reib , tiefe(1) )
   !vmitt(1:2) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! vel_mag(i)
   !flae(1:2) = 1000.0 !! unbenutzt da keine Einleitung
   !rhyd(1:2) = tiefe(1) ! hydraulischer Radius | sinnvollste Annahme im mehrdimensionalen
   !tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! water_depth(i)
   !Wlage(1,1:2)=zone(point_zone(iglob))%wettstat%wetterstations_lage ! Höhenlage der zuständigen Wetterstation mü.NHN
   !hWS(1,1:2)= rb_hydraul_p(3+(i-1)*number_rb_hydraul) ! Wasserspiegellage, von holen_trans() gesetzt
   !wge(1:2)=wge_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer)        ! Windgeschwindigkeit  aus Wetterstationsdaten
   !susn(1:2) = transfer_quantity_p(29+(i-1)*number_trans_quant) ! Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE
   !!#bsbt(1:2) = transfer_quantity_p(1+(i-1)*number_trans_quant) !orgc Sauerstoffverbrauch
   !bsbct(1:2) = transfer_quantity_p(47+(i-1)*number_trans_quant)
   !dalgki(1:2) = transfer_quantity_p(20+(i-1)*number_trans_quant) ! Zuwachs Kiesel-Algen
   !dalggr(1:2) = transfer_quantity_p(21+(i-1)*number_trans_quant) ! Zuwachs Grün-Algen
   !dalgbl(1:2) = transfer_quantity_p(22+(i-1)*number_trans_quant) ! Zuwachs Blau-Algen
   !dalgak(1:2) = transfer_quantity_p(23+(i-1)*number_trans_quant) ! Respiration Kiesel-Algen
   !dalgag(1:2) = transfer_quantity_p(24+(i-1)*number_trans_quant) ! Respiration Grün-Algen
   !dalgab(1:2) = transfer_quantity_p(25+(i-1)*number_trans_quant) ! Respiration Blau-Algen
   !PO2P(1:2) = transfer_quantity_p(30+(i-1)*number_trans_quant) ! Sauerstoffproduktion durch Makrophyten
   !PO2R(1:2) = transfer_quantity_p(31+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Makrophyten
   !ssalg(1:2) = planktonic_variable_p(52+(i-1)*number_plankt_vari) !
   !stind(1:2) = planktonic_variable_p(59+(i-1)*number_plankt_vari) !
   !albewg(1:2)=benthic_distribution_p(13+(i-1)*number_benth_distr) ! Wachstum benthischer gruen-Algen
   !alberg(1:2)=benthic_distribution_p(11+(i-1)*number_benth_distr) ! Respiration benthischer gruen-Algen
   !albewk(1:2)=benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
   !alberk(1:2)=benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
   
   !resdr(1:2)=benthic_distribution_p(15+(i-1)*number_benth_distr) ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
   !dzres1(1:2) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
   !dzres2(1:2) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten
   if ((iphy < 1) .or. (iphy > 4)) then
      write(fehler,*)'ph_huelle: aeration flag iphy',iphy,' out of bounds i,meinrang = ',i,meinrang
      call qerror(fehler)
   endif
   !! ---
   iglob = (i+meinrang*part) ! i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   tflie = real(deltat)/86400
   tiefes = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   raus = strickler( zone(point_zone(iglob))%reib , tiefes )
   flaes = 1000.0
   ! Caki,Cagr,Cabl  ! set by   ini_algae() delivered by module_QSimDatenfelder.f95
   kontroll = iglob == kontrollknoten
   
   if (kontroll)print*,iglob,meinrang,i,part,"  vor ph_kern lf,ph = ",  &
       planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)
   !  subroutine ph_kern(mws,pws,cas,lfs,tempws,vphs,vco2s                 &
   !                    ,tflie,raus,vmitts,tiefes,rhyds,flaes              &
   !                    ,wges,WLages,hWSs,iphy                             &
   !                    ,bsbcts,resdrs,dzres1s,dzres2s                     &
   !                 ,dalgkis,dalggrs,dalgbls,dalgaks,dalgags,dalgabs   &
   !                ,Caki,Cagr,Cabl                                    &
   !                ,albergs,alberks,albewgs,albewks                   &
   !                ,susns,po2ps,po2rs,ssalgs,stinds                   &
   call ph_kern(planktonic_variable_p(62+(i-1)*number_plankt_vari) &
                ,planktonic_variable_p(63+(i-1)*number_plankt_vari) &
                ,planktonic_variable_p(64+(i-1)*number_plankt_vari) &
                ,planktonic_variable_p(65+(i-1)*number_plankt_vari) &
                ,planktonic_variable_p( 1+(i-1)*number_plankt_vari) &
                ,planktonic_variable_p(66+(i-1)*number_plankt_vari) &
                ,transfer_quantity_p(26+(i-1)*number_trans_quant)   &
                ,tflie                                   &
                ,raus                                    &
                ,rb_hydraul_p(1+(i-1)*number_rb_hydraul) &
                ,rb_hydraul_p(2+(i-1)*number_rb_hydraul) &
                ,rb_hydraul_p(2+(i-1)*number_rb_hydraul) &
                ,flaes                                   &
                ,wge_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer)  &
                ,zone(point_zone(iglob))%wettstat%wetterstations_lage           &
                ,rb_hydraul_p(3+(i-1)*number_rb_hydraul)                        &
                ,iphy                                                           &
                ,transfer_quantity_p(47+(i-1)*number_trans_quant)    &
                ,benthic_distribution_p(15+(i-1)*number_benth_distr) &
                ,transfer_quantity_p(27+(i-1)*number_trans_quant)    &
                ,transfer_quantity_p(28+(i-1)*number_trans_quant)    &
                ,transfer_quantity_p(20+(i-1)*number_trans_quant) &
                ,transfer_quantity_p(21+(i-1)*number_trans_quant) &
                ,transfer_quantity_p(22+(i-1)*number_trans_quant) &
                ,transfer_quantity_p(23+(i-1)*number_trans_quant) &
                ,transfer_quantity_p(24+(i-1)*number_trans_quant) &
                ,transfer_quantity_p(25+(i-1)*number_trans_quant) &
                ,Caki,Cagr,Cabl   &
                ,benthic_distribution_p(11+(i-1)*number_benth_distr) &
                ,benthic_distribution_p(12+(i-1)*number_benth_distr) &
                ,benthic_distribution_p(13+(i-1)*number_benth_distr) &
                ,benthic_distribution_p(14+(i-1)*number_benth_distr) &
                ,transfer_quantity_p(29+(i-1)*number_trans_quant) &
                ,transfer_quantity_p(30+(i-1)*number_trans_quant) &
                ,transfer_quantity_p(31+(i-1)*number_trans_quant) &
                ,planktonic_variable_p(52+(i-1)*number_plankt_vari) &
                ,planktonic_variable_p(59+(i-1)*number_plankt_vari) &
                ,kontroll ,iglob )
   if (kontroll)print*,iglob,meinrang,i,part," nach ph_kern lf,ph = ",  &
       planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)
   if ((iphy < 1) .or. (iphy > 4)) then
      write(fehler,*)'ph_huelle nachher: aeration flag iphy',iphy,' out of bounds i,meinrang = ',i,meinrang
      call qerror(fehler)
   endif
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Daten-rückgabe:
   ! Transportkonzentrationen zurückschreiben
   !planktonic_variable_p(62+(i-1)*number_plankt_vari) = mw(1) !
   !planktonic_variable_p(63+(i-1)*number_plankt_vari) = pw(1) !
   !planktonic_variable_p(64+(i-1)*number_plankt_vari) = ca(1) !
   !planktonic_variable_p(65+(i-1)*number_plankt_vari) = lf(1) !  ### wie geht das mit dem Salzgehalt zusammen????
   !planktonic_variable_p(66+(i-1)*number_plankt_vari) = vph(1) !
   !planktonic_variable_p(59+(i-1)*number_plankt_vari) = stind(1) ! ??? Minutenzähler Bedeutung sehr unklar; Versuch einer Altersvariablen?
   ! Übergabekonzentrationen Rückgabewerte
   !transfer_quantity_p(26+(i-1)*number_trans_quant) = vco2(1) ! Kohlendioxyd
   return
end subroutine ph_huelle
!----+-----+----
!> die Subroutine ini_ph() steht in der datei ph_huelle.f95 \n
!! sie schreibt zunächst Nullen (ph=7) in die ph-variablen (mw,pw,ca,lf). \n
!!
!! ### ausgeschaltet in initialisieren() ### Vorbelegung durch randwerte
subroutine ini_ph()
   use modell
   implicit none
   integer i
   do i = 1,number_plankt_point
      planktonic_variable(62+(i-1)*number_plankt_vari) = 0.0 ! 1.5 ! mw 2.5
      planktonic_variable(63+(i-1)*number_plankt_vari) = 0.0 ! pw
      planktonic_variable(64+(i-1)*number_plankt_vari) = 0.0 ! 41.0 ! ca  41.0
      planktonic_variable(65+(i-1)*number_plankt_vari) = 0.0 ! 402.0 ! lf #### momentan von ini_salz gesetzt ???402.0
      planktonic_variable(66+(i-1)*number_plankt_vari) = 7.0 ! vph
   end do
   return
end subroutine ini_ph
