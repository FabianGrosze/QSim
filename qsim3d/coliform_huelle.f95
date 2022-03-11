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

!> \page coliform coliforme Bakterien/Keime (Hygiene)
!!
!! \image html badestelle_rhein.png "Krankheitskeime im Wasser können für badenden Menschen gefährlich sein."
!!
!! <h2>Herkunft</h2>
!! Programm zur Berechnung der Konzentration E. Coli, faecal coliformer und coliformer      
!! Bakterien in Fliessgewässer  \n                                     
!! AUTOR:VOLKER KIRCHESCH \n                                           
!! STAND:15.08.2017   \n                                               
!!
!! <h2>Teilprozesse</h2>
!! Fäkalcoliforme Bakterien vermehren sich im Gewässer nicht, sondern sterben rasch ab.\n\n
!! Berücksichtigt wird dabei:\n
!! * Grundverlustrate (temperaturabhängig)\n
!! * Licht-Dosis abhängige Verlustrate\n\n
!! Nicht gesondert berücksichtigt ist bisher:\n
!! * Grazing / Sedimentation 
!!                                                                       
!! <h2>Dokumentation und Veröffentlichungen</h2>
!! Kapitel <a href="./pdf/QSimDoku_coliform.pdf" target="_blank">19. Hygiene (Fäkalcoliforme Bakterien)</a> Ausschnitt aus:\n
!! "QSim - Das Gewässergütemodell der Bundesanstalt für Gewässerkunde" (QSimDoku_Kap7bis8bearbeiten.docx)\n
!! Änderungsstand 20. Juni 2018
!! \n\n
!! <a href="./pdf/18_Becker_ReWaM_intern_BfG_FLUSSHYGIENE_Koblenz26_10_2018_AB_IH.pdf" target="_blank">Modellierung hygienischer Belastungen in Fließgewässern</a>\n
!! <a href="./pdf/19_Fischer_SAME16-Potsdam_Hygienemodellierung-Berlin.pdf" target="_blank">To swim or not to swim</a>\n
!! <a href="./pdf/18_Fischer_FLUSSHYGIENE_Abschluss_final_Mri.pdf" target="_blank">Simulation von Maßnahmen zur langfristigen Verbesserung der hygienischen Wasserqualität</a>\n
!!
!! <h2>Schnittstellenbeschreibung / IT-Realisierung</h2>
!! SUBROUTINE COLIFORM (\ref tiefe,\ref rau,\ref vmitt,\ref vabfl,\ref elen,\ref flae,\ref flag,\ref tflie
!! ,\ref schwi,\ref ss,\ref zooind,\ref grote,\ref chla,\ref tempw,\ref jiein,*ecoli*     &\n
!! ,\ref qeinl, *colil*, *qeinll*,\ref anze,\ref iorla,\ref iorle,\ref ieinls,\ref ilbuhn
!! ,\ref coli,\ref doscf,\ref extks,\ref mstr,\ref azstrs &\n
!! ,\ref ratecde,\ref etacde,\ref ratecie,\ref xnuece,\ref ratecge,\ref ratecse,\ref ifehl & \n                                                  
!! ,\ref kontroll ,\ref iglob )
!! \n\n
!! \ref globaleParameter :\n
!!  RateCde Grundmortalitätsrate coliformer Bakterien bei 20°C\n 
!!  etaCde  Temperaturkoeffizient \n
!!  RateCIe Inaktivierungskoeffizient im Licht"  \n
!!  xnueCe  dimensionsloser Parameter zur Beschreibung der Inaktiv. im Licht \n
!!  RateCGe Coliforme Verlustrate durch Grazing \n
!!  RateCSe Coliforme Verlustrate durch Sedimentation \n
!! \n
!! Quelle coliform_huelle.f95; zurück zu: \ref lnk_ueberblick

!> SUBROUTINE coliform_huelle() wird beschrieben in: \ref coliform \n
!! Quelle coliform_huelle.f95
      SUBROUTINE coliform_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none
      integer :: i,nk
      real, Dimension(100) :: ecoli, coliL

      if(RateCde .le. 0.0)then
         if(kontroll)print*,'coliform_huelle no simulation without parameters'
         RETURN 
      end if

      iglob=(i+meinrang*part)
      nk=(i-1)*number_plankt_vari
      kontroll = iglob.eq.kontrollknoten

      tiefe(1:2)= rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
      rau(1:2)= strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
      vmitt(1:2) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag; randbedingungen.h
      vabfl(1:2) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
      elen(1:2)=1    ! Elementlänge (nicht verwendet)
      FLAE(1:2)=tiefe(1)*500.0 !! Breite konstant 500 m ; wird nur für Linienquelle verwendet, die in 3D nicht existiert. wie oxygen_huelle.f95
      flag(1:2)=0    ! keine Einleitungen
      tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim3D) in real Tage (QSim)
      schwi(1:2)=schwi_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer)    ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
      ss(1:2) = planktonic_variable_p(53+nk) ! ORG. UND ANORG. SCHWEBSTOFFE(OHNE ALGEN UND ZOOPLANKTER) schweb()
      zooind(1:2) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien in Ind/l
      !GRote jetzt direkt aus QSimDatenfelder GRote=transfer_parameter_p(67) ! Gewicht einer Rotatorie µg  | Aparam.txt
      CHLA(1:2) = planktonic_variable_p(11+nk)  ! Chlorophyl-A
      tempw(1:2)=planktonic_variable_p(1+nk)    ! Wasser-Temperatur
      jiein(1:2)=0       ! keine Punkt-Einleitungen
      ecoli(1:2) = 0.0     ! keine Einleitung

      qeinl(1:2)= 0.0      ! kein Einleitung (Abfluss)
      coliL(1:2)= 0.0      ! kein Linienquelle 
      qeinlL(1:2)=0.0      ! für Linienquelle; nicht verwendet
      anze=1               ! Anzahl der Profile im aktuellen Strang
      iorLa(1:2)=0         ! zur Berücksichtigung der Linienquelle; nicht verwendet
      iorLe(1:2)=0         ! zur Berücksichtigung der Linienquelle; nicht verwendet
      ieinLs(1:2)=0        ! keine Linienquellen
      ilbuhn = 0           ! keine Buhnen
      coli(1:2)  = planktonic_variable_p(61+(i-1)*number_plankt_vari) ! Fäkalcoliforme Bakterien
       if(kontroll)print*,'coliform_huelle... start: iglob,i,nk,meinrang=',iglob,i,nk,meinrang,' coli=',coli(1)
     DOSCF(1:2)  = planktonic_variable_p(70+(i-1)*number_plankt_vari) ! 
      EXTKS(1,1:2) = zone(point_zone(iglob))%seditemp%extiks ! Extinktionskoeffizient für PARS ((nicht mehr)nur bei Temperaturmodellierung erforderlich!)       mstr
      ! integer , parameter :: azStrs=1

      ! RateCde ! aus APARAM.txt
      ! etaCde  ! aus APARAM.txt
      ! RateCIe ! aus APARAM.txt
      ! xnueCe  ! aus APARAM.txt
      ! RateCGe ! aus APARAM.txt
      ! RateCSe ! aus APARAM.txt
      ifehl=0 !initialisiert

!----------------------------------------------------------------------------------
      call COLIFORM(tiefe,rau,vmitt,vabfl,elen,flae,flag,tflie,schwi,ss,zooind,GRote,Chla,tempw,jiein,ecoli    &
                   ,qeinl,coliL,qeinlL,anze,iorLa,iorLe,ieinLs,ilbuhn,coli,DOSCF,extkS,mstr,azStrs             &
                   ,RateCde,etaCde,RateCIe,xnueCe,RateCGe,RateCSe,ifehl                                        &                                                   
                   ,kontroll , iglob )
!----------------------------------------------------------------------------------
      if(ifehl.gt.0)then
         print*,'ifehl=',ifehl,' in coliform_huelle: iglob,i,meinrang=',iglob,i,meinrang
         call qerror('COLIFORM ifehl.gt.0')
      end if

      planktonic_variable_p(61+(i-1)*number_plankt_vari) = coli(1)  ! Fäkalcoliforme Bakterien
      planktonic_variable_p(70+(i-1)*number_plankt_vari) = DOSCF(1) ! 

      if(kontroll)print*,'coliform_huelle ende: coli=',coli(1)

      RETURN 
      END subroutine coliform_huelle

