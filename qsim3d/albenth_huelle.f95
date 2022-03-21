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

!> \page albenth benthische Algen
!!
!! \image html 20160317_162418_klein.jpg ""  <a href="20160317_162418.jpg">Bild groß</a> \n
!! Herkunft: https://prd-wret.s3-us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/thumbnails/image/20160317_162418.jpg\n
!! public domain gemäß: https://www.usgs.gov/media/images/close-view-periphyton-rocks-along-shore-lake-tahoe\n
!! heruntergeladen am 12. Nov. 2019\n
!!
!! <h2>Herkunft</h2>
!!
!! UNTERPROGRAMM ZUR BERECHNUNG DES DES EINFLUSSES BENTHISCHER ALGEN \n     
!! AUTOR :VOLKER KIRCHESCH \n                                           
!! STAND :20.02.1996 \n                                               
!! \n                                                                 
!! Kx        - Beruecksichtigt, dass mit zunehmender Schichtdicke \n   
!!             die unteren Schichten weniger Licht bekommen\n          
!! saetbk    - Saettigungslichtstärke für Kieselalgen\n                
!! saetbg    - Saettigungslichtstärke für Grünalgen\n                  
!! (Werte wurden dem Modell AQUATOX entnommen, Dim.: mueE/(m2*s)) \n   
!! roPeri    - Dichte des Periphytons in g/m3 (s. Uhlmann)\n           
!! zPeri     - Dicke des Periphytonrasens [m]  \n                         
!!                                                                       
!! <h2>Teilprozesse</h2>
!! <ul>
!! <li>Licht-, Nährstoff- und Temperaturabhängiges Wachstum</li> 
!! <li>Limitation des Wachstums durch die Fliessgeschwindigkeit</li> 
!! <li>Abnahme der Biomasse infolge von Respiration</li>
!! <li>?? Abspülung benthischer Algen ?? Implementierung unklar ?? </li>
!! </ul>         
!!                                                            
!! <h2>Dokumentation und Veröffentlichungen</h2>
!! 
!! <a href="./pdf/BenthischeAlgenAnnette2011.pdf" target="_blank">Dokumentationsentwurf</a> Annette Becker 2011\n
!! 
!! <h2>Schnittstellenbeschreibung / IT-Realisierung</h2>
!!      SUBROUTINE albenth(\ref schwi, \ref tflie, \ref tempw, \ref tiefe, \ref vmitt, \ref vno3, \ref vnh4
!!                         , \ref gelp, \ref albewg, \ref alberg, \ref elen, \ref flae, \ref ior, \ref anze         &\n
!!                         , \ref aggmax, \ref agksn, \ref agksp, \ref si, \ref akksn, \ref akksp, \ref akkssi
!!                         , \ref akgmax, \ref albewk, \ref alberk, \ref abegm2, \ref abekm2          &\n
!!                         , \ref vabfl, \ref cmatgr, \ref cmatki, \ref akchl, \ref agchl, \ref extk, \ref ilang, \ref mstr  & \n                                                  
!!                         , \ref kontroll , \ref iglob )
!!
!! \n
!! Zonenweise Vorbelegung der Biomasse der benthischen Algen in <a href="./exp/MODELLG.3D.txt" target="_blank">MODELLG.3D.txt</a>: \n
!! subroutine ModellGParam(cpfad1,j1) \n
!! Id=QB Text=Benth.Algen Help=Benth.Algen-Vorkommen in den Gewässer-Abschnitten \n
!! Ident="GGruen" Text="Gewicht Grünalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Grünalgen \n
!! Ident="GKiesel" Text="Gewicht Kieselalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Kieselalgen \n
!! \n
!! Quelle albenth_huelle.f95; zurück zu: \ref lnk_ueberblick

!> SUBROUTINE albenth_huelle() wird beschrieben in: \ref albenth \n
!! Quelle albenth_huelle.f95
      SUBROUTINE albenth_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      use aparam
      implicit none
      integer :: i,nk

      iglob=(i+meinrang*part)
      nk=(i-1)*number_plankt_vari
      kontroll = iglob.eq.kontrollknoten
      if(kontroll)print*,'albenth_huelle  iglob,i,nk,meinrang=',iglob,i,nk,meinrang

      schwi(1:2)=schwi_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer)    ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
      tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim3D) in real Tage (QSim)
      tempw(1:2)=planktonic_variable_p(1+nk)    ! Wasser-Temperatur
      tiefe(1:2)= rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
      vmitt(1:2) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag; randbedingungen.h
      vno3(1:2) = planktonic_variable_p( 5+nk)  ! nitrat (unbenutzt)
      vnh4(1:2) = planktonic_variable_p( 3+nk)  ! ammonium (unbenutzt)
      gelp(1:2) = planktonic_variable_p( 6+nk)  ! gelöster ortho-Phosphat-Phosphor tiefengemittelt
      albewg(1:2) = benthic_distribution_p(13+(i-1)*number_benth_distr) ! Wachstum benthischer gruen-Algen
      alberg(1:2) = benthic_distribution_p(11+(i-1)*number_benth_distr) ! Respiration benthischer gruen-Algen
      elen(1:2)=1    ! Elementlänge (nicht verwendet)
      flae(1:2)=tiefe(1)*500.0 !! breite konstant 500 m ; wird nur für linienquelle verwendet, die in 3d nicht existiert. wie oxygen_huelle.f95
      ior=1             ! Laufindex
      anze=1               ! Anzahl der Profile im aktuellen Strang

      ! aggmax ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
      ! agksn ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
      ! agksp ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
      si(1) = planktonic_variable_p( 7+nk)  ! silikat-Silizium-Konzentration (tiefengemittelt)
      ! akksn ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
      ! akksp ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
      ! akkssi ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
      ! akgmax ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
      albewk(1:2) = benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
      alberk(1:2) = benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
      abegm2(1:2) =benthic_distribution_p(72+(i-1)*number_benth_distr)
      abekm2(1:2) =benthic_distribution_p(73+(i-1)*number_benth_distr)

      vabfl(1:2) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
      cmatgr(1:2)=benthic_distribution_p(10+(i-1)*number_benth_distr) ! Abspülung benthischer gruen-Algen
      cmatki(1:2)=benthic_distribution_p(9+(i-1)*number_benth_distr) ! Abspülung benthischer kiesel-Algen
      ! akchl ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
      ! agchl ! jetzt direkt aus QSimDatenfelder ! APARAM.txt
      extk(1:2) = transfer_quantity_p(54+(i-1)*number_trans_quant) ! mittlerer Extinktionskoeffizient
      ! module_QSimDatenfelder.f95:    integer , parameter :: ilang=1
      mstr = 1        ! Strangzähler | nur ein Profil in einem Strang

!----------------------------------------------------------------------------------
      call albenth(schwi,tflie,tempw,tiefe,vmitt,vno3,vnh4,gelp,albewg,alberg,elen,flae,ior,anze         &
                  ,aggmax,agksn,agksp,si,akksn,akksp,akkssi,akgmax,albewk,alberk,abegm2,abekm2           &
                  ,vabfl,cmatgr,cmatki,akchl,agchl,extk,ilang,mstr                                       &                                                   
                  ,kontroll ,iglob )  
!----------------------------------------------------------------------------------

! albenth():					benthische Verteilungen
!      cmatki(ior) = albewk(ior)		9	Abspülung benthischer kiesel-Algen ??
      benthic_distribution_p(9+(i-1)*number_benth_distr)  = cmatki(1)
!      cmatgr(ior) = albewg(ior)		10	Abspülung benthischer gruen-Algen ??
      benthic_distribution_p(10+(i-1)*number_benth_distr) = cmatgr(1)
!      alberg(ior) = alberg(ior)/tiefe(ior)	11 	Respiration benthischer gruen-Algen 	mgBio/l je Zeitschritt
      benthic_distribution_p(11+(i-1)*number_benth_distr) = alberg(1)
!      alberk(ior) = alberk(ior)/tiefe(ior) 	12	Respiration benthischer kiesel-Algen 	mgBio/l je Zeitschritt
      benthic_distribution_p(12+(i-1)*number_benth_distr) = alberk(1)
!      albewg(ior) = albewg(ior)/tiefe(ior)	13  	Wachstum benthischer gruen-Algen 	mgBio/l je Zeitschritt
      benthic_distribution_p(13+(i-1)*number_benth_distr) = albewg(1)
!      albewk(ior) = albewk(ior)/tiefe(ior)	14 	Wachstum benthischer kiesel-Algen 	mgBio/l je Zeitschritt
      benthic_distribution_p(14+(i-1)*number_benth_distr) = albewk(1)
!      abegm2(ior) = abegrt-alberg(ior)-(cmatgr(ior)*tiefe(ior)) 
      benthic_distribution_p(72+(i-1)*number_benth_distr) = abegm2(1) ! Biomasse benthischer Grünalgen
!      abekm2(ior) = abekit-alberk(ior)-(cmatki(ior)*tiefe(ior)) 
      benthic_distribution_p(73+(i-1)*number_benth_distr) = abekm2(1) ! Biomasse benthischer Kieselalgen

!nicht hier ...
! 20 	abeowg 	Sauerstoffproduktion (Wachstum) benthischer Grünalgen 	mgO/l je Zeitschritt
! wird in oxygen.f90 berechnet:      abeowg(ior) = albewg(ior)*falgog
! 21 	abeorg 	Sauerstoffverbrauch (Respiration) benthischer Grünalgen 	mgO/l je Zeitschritt
! wird in oxygen.f90 berechnet:      abeorg(ior) = alberg(ior)*opgrmix 
! 22 	abeowk 	Sauerstoffproduktion (Wachstum) benthischer Kieselalgen 	mgO/l je Zeitschritt
! wird in oxygen.f90 berechnet:      abeowk(ior) = albewk(ior)*falgok 
! 23 	abeork 	Sauerstoffverbrauch (Respiration) benthischer Kieselalgen 	mgO/l je Zeitschritt 
! wird in oxygen.f90 berechnet:      abeork(ior) = alberk(ior)*opkimix 


      RETURN 
      END subroutine albenth_huelle

