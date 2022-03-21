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

!> \page Dreissena Dreissena polymorpha (Muscheln)
!! 
!! \image html Dreissena_polymorpha3.jpg
!! \image latex Dreissena_polymorpha3.jpg
!! von: https://commons.wikimedia.org/wiki/File:Dreissena_polymorpha3.jpg?uselang=de (Okt. 2019)
!! 
!! <h2>Herkunft</h2>
!! EIN PROGRAMM zu Berechnung der Entwicklung von Dreissena polymorph\n
!! und deren Einfluss auf das Phytoplankton  \n                        
!! AUTOR : VOLKER KIRCHESCH    \n                                      
!! STAND : 15.01.1996    \n                                            
!!
!! <h2>Teilprozesse</h2>
!! *Liste möglicherweise unvollständig* \n
!! <ul>
!! <li> Reproduktion Larvenbildung </li>
!! <li> Schwebstoffaufnahme durch Dreissena </li>
!! <li> Respiration </li>
!! <li> Exkretion </li>
!! <li> filtrierbaren Futterkonzentration </li>
!! <li> Filtrierrate Aufnahmerate Assimilation</li>
!! <li> Einfluss von Corophium auf die Ingest.- und Filtrierrate </li>
!! <li>    rres  - Grundrespirationsrate 1/d                                 </li>
!! <li>    qres  - aktive Respirationsrate (abhaengig von der Assimilation) 1</li>
!! <li>    dmorg - natuerliche Mortalitaetsrate                              </li>
!! <li>    dmorma- maximale Mortalitaetsrate bei Sauerstoffschwund           </li>
!! <li> ???????????? </li>
!! </ul>
!! 
!! <h2>Dokumentation und Veröffentlichungen</h2>
!! <a href="./pdf/QSimDoku_dreissen.pdf" target="_blank"> Dokumentationsfragment </a>\n
!! <a href="./pdf/2002Schoel_etal_river_Rhine_QSIM_model_algae_dreissena_food-web.pdf" target="_blank"> Veröffentlichung 2002Schoel_etal</a>\n
!! 
!! <h2>Schnittstellenbeschreibung / IT-Realisierung</h2>
!! SUBROUTINE dreissen( \ref zdrei, \ref zdreis, \ref tempw, \ref flae, \ref elen, \ref anze              &\n
!! &, \ref ior, \ref volfdr, \ref akbcm, \ref agbcm, \ref aki, \ref agr, \ref algdrk, \ref algdrg         &\n
!! &, \ref tflie, \ref ro2dr, \ref lboem, \ref bsohlm, \ref ss, \ref vo2, \ref ssdr, \ref drfaek          &\n
!! &, \ref drfaeg, \ref drfaes, \ref gewdr, \ref dlarvn, \ref itags, \ref monats, \ref jahrs              &\n
!! &, \ref lait1, \ref laim1, \ref laid1, \ref ilang                                                      &\n
!! &, \ref resdr, \ref exdrvg, \ref exdrvk, \ref ssalg, \ref drpfec                                       &\n
!! &, \ref abl, \ref exdrvb, \ref abbcm, \ref algdrb, \ref drfaeb                                         &\n
!! &, \ref idras, \ref drmas, \ref drakr, \ref drbar, \ref drmor, *ffood*, \ref coroi, \ref corois     &\n
!! &, \ref chnf, \ref drhnf, *hnfdra*, \ref dlmax, \ref dlmaxs, \ref gwdmax                            &\n
!! &, \ref sgwmue, \ref fkm, \ref foptde, \ref mstr, *azStr*                                &\n                                                   
!! &, \ref kontroll , \ref iglob )
!! \n\n
!! \anchor bsohlm bsohlm Sohlbreite hier = 500.0 gesetzt\n
!! \anchor lboem  lboem Böschungslänge  im 3D nicht verwendbar hier=0.0 \n
!! \anchor volfdr volfdr unbenutzt\n
!! \n
!! aus der L-Zeile von <a href="./exp/ModellG.3D.txt" target="_blank">ModellG.3D.txt</a>:\n
!! \anchor lait1 lait1 =  \ref lait (point_zone(iglob)) ! Dreissena Laichperiode: Tag des Beginns der Laichperiode\n
!! \anchor laim1 laim1 =  \ref laim (point_zone(iglob)) ! Dreissena Laichperiode: Monat des Beginns der Laichperiode\n
!! \anchor laid1 laid1 =  \ref laid (point_zone(iglob)) ! Dreissena Laichperiode: Dauer der Laichperiode in Tagen\n
!! \n
!! Raten Kontrollausgabe (nur in dreissen_huelle() definiert):\n
!! \anchor drpfec drpfec Pseudofacesanteil durch Dreissena\n
!! \anchor idras idras Ingestionsrate Dreissena (0. +1. Koh.) \n
!! \anchor drmas drmas Wachstumsrate Dreissena (0. +1. Koh.)\n
!! \anchor drakr drakr aktive Respirationrate Dreissena (0. +1. Koh.)\n
!! \anchor drbar drbar Grundrespirationrate Dreissena  (0. +1. Koh.)\n
!! \anchor drmor drmor Mortalitätsrate Dreissena (0. +1. Koh.)\n
!! \n
!! Quelle dreissen_huelle.f95; zurück zu: \ref lnk_ueberblick
      
!> SUBROUTINE dreissen_huelle() wird beschrieben in: \ref Dreissena \n
!! Quelle dreissen_huelle.f95
      SUBROUTINE dreissen_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      use aparam                                                   
      implicit none

      integer :: i,j, azStr,k
      real, Dimension(1000) :: lboem, bsohlm
      real, Dimension(1000) :: volfdr
      integer lait1, laim1, laid1
      real drpfec(1000), idras(1000,2), drmas(1000,2), drakr(1000,2), drbar(1000,2), drmor(1000,2),ffood(1000)
      real HNFdra(1000)

      iglob=(i+meinrang*part)

      if(zone(point_zone(iglob))%dreissen%dreissena_aktiv .eq. 0)then
         if(kontroll)print*,'dreissen_huelle: keine muscheln in zone',point_zone(iglob)
         RETURN ! keine Muscheln in dieser Zone
      else
         if(kontroll)print*,'dreissen_huelle ... start',iglob
      endif

      ! Bilanzvariablen
      zdreis(1:2,1) = benthic_distribution_p(56+(i-1)*number_benth_distr) ! Dreissenabiomasse pro Fläche Sohle (0. Kohorte)
      zdreis(1:2,2) = benthic_distribution_p(57+(i-1)*number_benth_distr) ! Dreissenabiomasse pro Fläche Sohle (1. Kohorte)
      zdreis(1:2,3) = benthic_distribution_p(58+(i-1)*number_benth_distr) ! Dreissenabiomasse pro Fläche Sohle (2. Kohorte)
      zdreis(1:2,4) = benthic_distribution_p(59+(i-1)*number_benth_distr) ! Dreissenabiomasse pro Fläche Sohle (3. Kohorte)
      zdrei(1:2,1) = benthic_distribution_p(60+(i-1)*number_benth_distr)  ! Dreissenabiomasse pro Fläche Böschung (0. Kohorte)
      zdrei(1:2,2) = benthic_distribution_p(61+(i-1)*number_benth_distr)  ! Dreissenabiomasse pro Fläche Böschung (1. Kohorte)
      zdrei(1:2,3) = benthic_distribution_p(62+(i-1)*number_benth_distr)  ! Dreissenabiomasse pro Fläche Böschung (2. Kohorte)
      zdrei(1:2,4) = benthic_distribution_p(63+(i-1)*number_benth_distr)  ! Dreissenabiomasse pro Fläche Böschung (3. Kohorte)
      gewdr(1:2,1) = benthic_distribution_p(64+(i-1)*number_benth_distr)  ! Gewicht einer Dreissena-Muschel (0. Kohorte)
      gewdr(1:2,2) = benthic_distribution_p(65+(i-1)*number_benth_distr)  ! Gewicht einer Dreissena-Muschel (1. Kohorte)
      gewdr(1:2,3) = benthic_distribution_p(66+(i-1)*number_benth_distr)  ! Gewicht einer Dreissena-Muschel (2. Kohorte)
      gewdr(1:2,4) = benthic_distribution_p(67+(i-1)*number_benth_distr)  ! Gewicht einer Dreissena-Muschel (3. Kohorte)
      dlarvn(1:2) = planktonic_variable_p(60+(i-1)*number_plankt_vari)    ! Anzahl der Dreissena-Larven
      dlmax(1:2)  = benthic_distribution_p(68+(i-1)*number_benth_distr)   ! Dreissena Larven ??
      dlmaxs(1:2) = benthic_distribution_p(69+(i-1)*number_benth_distr)   ! Dreissena Larven ??
      gwdmax(1:2) = benthic_distribution_p(70+(i-1)*number_benth_distr)   ! Dreissena Larven ??
      sgwmue(1:2) = benthic_distribution_p(71+(i-1)*number_benth_distr)   ! Dreissena Larven ??
      !Übergabe
      tempw(1:2) = planktonic_variable_p( 1+(i-1)*number_plankt_vari)  ! Wassertemperatur
      aki(1:2) = planktonic_variable_p(8+(i-1)*number_plankt_vari)     ! Anteil kiesel-Algen
      agr(1:2) = planktonic_variable_p(9+(i-1)*number_plankt_vari)     ! Anteil gruen-Algen
      abl(1:2) = planktonic_variable_p(10+(i-1)*number_plankt_vari)    ! Anteil blau-Algen
      vo2(1:2) = planktonic_variable_p( 2+(i-1)*number_plankt_vari)    ! Sauerstoffgehalt tiefengemittelt
      ro2dr(1:2) = benthic_distribution_p(24+(i-1)*number_benth_distr) ! Respiration Dreissena-Muscheln pro Zeitschritt in mgO2/l je Zeitschritt
      algdrk(1:2)= benthic_distribution_p(38+(i-1)*number_benth_distr) ! kiesel Algen-Konsum-bentisch (Muscheln) in mg/l
      algdrg(1:2)= benthic_distribution_p(40+(i-1)*number_benth_distr) ! grün-Algen-Konsum-bentisch (Muscheln) in mg/l
      algdrb(1:2)= benthic_distribution_p(41+(i-1)*number_benth_distr) ! blau-Algen-Konsum-bentisch (Muscheln) in mg/l
      drfaek(1:2) = transfer_quantity_p(13+(i-1)*number_trans_quant)   ! Faecesbildung der Muscheln infolge Konsum Kieselalgen
      drfaeg(1:2) = transfer_quantity_p(14+(i-1)*number_trans_quant)   ! Faecesbildung der Muscheln infolge Konsum Grünlalgen
      drfaeb(1:2) = transfer_quantity_p(15+(i-1)*number_trans_quant)   ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen
      drfaes(1:2) = transfer_quantity_p(95+(i-1)*number_trans_quant)   ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen
      ssdr(1:2)=benthic_distribution_p(4+(i-1)*number_benth_distr)     ! Schwebstoffaufnahme durch Dreissena ??
      CHNF(1:2) = planktonic_variable_p(48+(i-1)*number_plankt_vari)   ! C-Masse der heterotrophen Nanoflagelaten
      if(CHNF(1).le. 0.0) CHNF(1:2) = 0.0 ! CHNF=-1 meint keine HNF
      akbcm(1:2) = planktonic_variable_p(24+(i-1)*number_plankt_vari)  ! Verhältnis Chlorophyll-a zu Kohlenstoff Kieselalgen
      agbcm(1:2) = planktonic_variable_p(25+(i-1)*number_plankt_vari)  ! Verhältnis Chlorophyll-a zu Kohlenstoff Gruenalgen
      abbcm(1:2) = planktonic_variable_p(26+(i-1)*number_plankt_vari)  ! Verhältnis Chlorophyll-a zu Kohlenstoff der blau-Algen
      ssalg(1:2) = planktonic_variable_p(52+(i-1)*number_plankt_vari)  ! GESAMTSCHWEBSTOFFE incl. lebender Organismen, messbar, Randwert
      coroi(1:2)= benthic_distribution_p(54+(i-1)*number_benth_distr)  ! Corophium Böschung
      corois(1:2)= benthic_distribution_p(55+(i-1)*number_benth_distr) ! Corophium Sohle

      elen(1:2)=1         ! Elementlänge (nicht verwendet)
      FLAE(1:2)=500.0*rb_hydraul_p(2+(i-1)*number_rb_hydraul) !500* tiefe(1) Wassertiefe !! Breite konstant 500 m  
      bsohlm(1:2)=500.0   ! Sohlbreite im 3D nicht verwendbar
      lboem(1:2)=0.01      ! Böschungslänge  im 3D nicht verwendbar
      ior=1             ! Laufindex
      anze=1            ! Anzahl der Profile im aktuellen Strang
      mstr = 1          ! Strangzähler | nur ein Profil in einem Strang
      azStr = 2         ! Strangnummer dient nur zum Hochzählen
      if(iglob.eq.1)azStr = 1
      volfdr(:)=0.0     ! unbenutzt

      lait1 = zone(point_zone(iglob))%laich%lait ! Dreissena Laichperiode: Tag des Beginns der Laichperiode
      laim1 = zone(point_zone(iglob))%laich%laim ! Dreissena Laichperiode: Monat des Beginns der Laichperiode
      laid1 = zone(point_zone(iglob))%laich%laid ! Dreissena Laichperiode: Dauer der Laichperiode in Tagen

!! <h2>Dreissena-Bewuchs</h2>
!! Im 3D gibt es keine Böschungen, daher wird dort die Muscheldichte Null gesetzt.
!! <code>\verbatim
!! <ParamSetDef Id="QD" Text="Dreissena" Help="Dreissena-Bewuchs in den Gewässer-Abschnitten" Scope="Abschnitt">
!!   <Parameter Ident="MBoesch0" Text="Biomasse 0.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte (Schalenlänge kl. 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />
!!   <Parameter Ident="MSohle0" Text="Biomasse 0.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />
!!   <Parameter Ident="Gewicht0" Text="Mittl. Muschelgewicht 0.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 0. Kohorte" Min="" Max="" Default="" />
!!   <Parameter Ident="MBoesch1" Text="Biomasse 1.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte (Schalenlänge gr.= 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />
!!   <Parameter Ident="MSohle1" Text="Biomasse 1.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />
!!   <Parameter Ident="Gewicht1" Text="Mittl. Muschelgewicht 1.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 1. Kohorte." Min="" Max="" Default="" />
!! \endverbatim</code>

      do k=1,number_benth_distr
         if(isnan(benthic_distribution_p(k+(i-1)*number_benth_distr)))then
            print*,'vor dreissen: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
            if(meinrang==0)print*,'benth_distr_name:',benth_distr_name(k)
         endif
      end do
      
!----------------------------------------------------------------------------------
      call dreissen(zdrei,zdreis,tempw,flae,elen,anze                   &
     &,ior,volfdr,akbcm,agbcm,aki,agr,algdrk,algdrg                     &
     &,tflie,ro2dr,lboem,bsohlm,ss,vo2,ssdr,drfaek                      &
     &,drfaeg,drfaes,gewdr,dlarvn,itags,monats,jahrs                    &
     &,lait1,laim1,laid1,ilang                                          &
     &,resdr,exdrvg,exdrvk,ssalg,drpfec                                 &
     &,abl,exdrvb,abbcm,algdrb,drfaeb                                   &
     &,idras,drmas,drakr,drbar,drmor,ffood,coroI,coroIs                 &
     &,CHNF,drHNF,HNFdra,dlmax,dlmaxs,gwdmax                            &
     &,sgwmue,fkm,FoptD,mstr,azStr                                     &
     &,kontroll ,iglob ) !!wy
!----------------------------------------------------------------------------------
      if(kontroll)print*,'dreissen_huelle ... zdreis(1,1), dlmax(1)=',zdreis(1,1),dlmax(1)

      !Übergabe
      benthic_distribution_p(24+(i-1)*number_benth_distr) = ro2dr(1) ! Respiration Dreissena-Muscheln in mgO2/l je Zeitschritt
      benthic_distribution_p(38+(i-1)*number_benth_distr) = algdrk(1) ! kiesel Algen-Konsum-bentisch (Muscheln) in mg/l
      benthic_distribution_p(40+(i-1)*number_benth_distr) = algdrg(1) ! grün-Algen-Konsum-bentisch (Muscheln) in mg/l
      benthic_distribution_p(41+(i-1)*number_benth_distr) = algdrb(1) ! blau-Algen-Konsum-bentisch (Muscheln) in mg/l
      transfer_quantity_p(13+(i-1)*number_trans_quant) = drfaek(1) ! Faecesbildung der Muscheln infolge Konsum Kieselalgen
      transfer_quantity_p(14+(i-1)*number_trans_quant) = drfaeg(1) ! Faecesbildung der Muscheln infolge Konsum Grünlalgen
      transfer_quantity_p(15+(i-1)*number_trans_quant) = drfaeb(1) ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen
      transfer_quantity_p(95+(i-1)*number_trans_quant) = drfaes(1) ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen
      benthic_distribution_p(4+(i-1)*number_benth_distr) = ssdr(1) ! Schwebstoffaufnahme durch Dreissena ??
      benthic_distribution_p(29+(i-1)*number_benth_distr)= exdrvk(1) ! exkretierte Biomasse der Muscheln beim Verzehr von Kiesel-algen
      benthic_distribution_p(30+(i-1)*number_benth_distr)= exdrvg(1) ! exkretierte Biomasse der Muscheln beim Verzehr von Grünalgen
      benthic_distribution_p(31+(i-1)*number_benth_distr)= exdrvb(1) ! exkretierte Biomasse der Muscheln beim Verzehr von Blau-algen
      benthic_distribution_p(15+(i-1)*number_benth_distr)= resdr(1) ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln) 	
      transfer_quantity_p(96+(i-1)*number_trans_quant) = drhnf(1) !  Dreissena-Muscheln fressen HNF
      ! Bilanz
      benthic_distribution_p(56+(i-1)*number_benth_distr) = zdreis(1,1) ! Dreissenabiomasse pro Fläche Sohle (0. Kohorte)
      benthic_distribution_p(57+(i-1)*number_benth_distr) = zdreis(1,2) ! Dreissenabiomasse pro Fläche Sohle (1. Kohorte)
      benthic_distribution_p(58+(i-1)*number_benth_distr) = zdreis(1,3) ! Dreissenabiomasse pro Fläche Sohle (2. Kohorte)
      benthic_distribution_p(59+(i-1)*number_benth_distr) = zdreis(1,4) ! Dreissenabiomasse pro Fläche Sohle (3. Kohorte)
      benthic_distribution_p(60+(i-1)*number_benth_distr) = 0.0 ! zdrei(1,1)  ! Dreissenabiomasse pro Fläche Böschung (0. Kohorte)
      benthic_distribution_p(61+(i-1)*number_benth_distr) = 0.0 ! zdrei(1,2)  ! Dreissenabiomasse pro Fläche Böschung (1. Kohorte)
      benthic_distribution_p(62+(i-1)*number_benth_distr) = 0.0 ! zdrei(1,3)  ! Dreissenabiomasse pro Fläche Böschung (2. Kohorte)
      benthic_distribution_p(63+(i-1)*number_benth_distr) = 0.0 ! zdrei(1,4)  ! Dreissenabiomasse pro Fläche Böschung (3. Kohorte)
      benthic_distribution_p(64+(i-1)*number_benth_distr) = gewdr(1,1)  ! Gewicht einer Dreissena-Muschel (0. Kohorte)
      benthic_distribution_p(65+(i-1)*number_benth_distr) = gewdr(1,2)  ! Gewicht einer Dreissena-Muschel (1. Kohorte)
      benthic_distribution_p(66+(i-1)*number_benth_distr) = gewdr(1,3)  ! Gewicht einer Dreissena-Muschel (2. Kohorte)
      benthic_distribution_p(67+(i-1)*number_benth_distr) = gewdr(1,4)  ! Gewicht einer Dreissena-Muschel (3. Kohorte)
      planktonic_variable_p(60+(i-1)*number_plankt_vari) = dlarvn(1)    ! Anzahl der Dreissena-Larven
      benthic_distribution_p(68+(i-1)*number_benth_distr) = dlmax(1)    ! Dreissena Larven ??
      benthic_distribution_p(69+(i-1)*number_benth_distr) = dlmaxs(1)   ! Dreissena Larven ??
      benthic_distribution_p(70+(i-1)*number_benth_distr) = gwdmax(1)   ! Dreissena Larven ??
      benthic_distribution_p(71+(i-1)*number_benth_distr) = sgwmue(1)   ! Dreissena Larven ??

      do k=1,number_benth_distr
         if(isnan(benthic_distribution_p(k+(i-1)*number_benth_distr)))then
            print*,'nach dreissen: isnan(benthic_distribution_p  node#',iglob,' variable# ',k
            if(meinrang==0)print*,'benth_distr_name:',benth_distr_name(k)
         endif
      end do

      if(kontroll)then
         print*,'Pseudofacesanteil durch Dreissena drpfec=',drpfec(1)
         print*,'Ingestionsrate Dreissena (0. +1. Koh.) idras=',idras(1,1:2)
         print*,'Wachstumsrate Dreissena (0. +1. Koh.) drmas=',drmas(1,1:2)
         print*,'aktive Respirationrate Dreissena (0. +1. Koh.) drakr=',drakr(1,1:2)
         print*,'Grundrespirationrate Dreissena  (0. +1. Koh.) drbar=',drbar(1,1:2)
         print*,'Mortalitätsrate Dreissena (0. +1. Koh.) drmor=',drmor(1,1:2)
         print*,'Verhältnis von Futterkonz. zu optimaler Futterkonzentration (Food-Faktor) für Dreissena ffood=',ffood(1)
         print*,'Flagellatenverlustrate durch Dreissena hnfdra=',hnfdra(1)
      end if ! kontroll

      RETURN 
      END subroutine dreissen_huelle

