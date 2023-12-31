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
! hier enthalten:
! randbedingungen_setzen() ; randwert_planktonic() ; randbedingungen_ergaenzen() ; randbedingungen_parallel() ;
! scatter_BC() ; RB_werte_aktualisieren() ; function randwert_gueltig() ; read_boundary_timeseries() ;
! read_extnct() ;  alloc_hydraul_BC()

subroutine randbedingungen_setzen()
   use modell
   use QSimDatenfelder
   use module_aparam
   implicit none
   
   integer :: j, rb_zaehl
   logical :: einmalig
   
   
   ! Hydraulik-Randbedingungen (Geschwindigkeit, Wassertiefe und WSP)
   call scatter_rb_hydraul()

   ! Wetter-Randbedingungen
   call update_weather()
   call mpi_barrier (mpi_komm_welt, ierr)
   ! Randverläufe und Wetterstationen haben alle Prozessoren alle Eingabedaten
   ! und wenden sie auf ihre Knoten an.
   
   if (meinrang == 0) then !! nur prozessor 0
      einmalig = .true. ! Fehlermeldung nur einmal ausgeben
      
      call RB_werte_aktualisieren(rechenzeit)
      
      do j = 1,number_plankt_point ! nur bei casu=knotenanzahl2D
         select case (hydro_trieb)
         case(1) ! casu-transinfo
            RB_zaehl = knoten_rand(j)
         case(2) ! Untrim² netCDF
            RB_zaehl = element_rand(j)
         case(3) ! SCHISM
            RB_zaehl = knoten_rand(j)
         case default
            call qerror('randbedingungen_setzen: Hydraulischer Antrieb unbekannt')
         end select
         
         if ( RB_zaehl > 0 .and. RB_zaehl < 100000 ) then !! Alle Knoten, deren RB's bedient werden:
            if (j == kontrollknoten) print*,'Konrollstelle #',j,' ist Rand mit RB_zaehl = ',RB_zaehl
            if (inflow(j)) then !! alle Zufluss-Knoten
               if (j == kontrollknoten)print*,'Konrollstelle #',j,' ist inflow Rand mit ',inflow(j)
               call randwert_planktonic(j,RB_zaehl)
               if (j == kontrollknoten) then
                   print*,'RB gesetzt tracer = ', planktonic_variable(71+(j-1)*number_plankt_vari),  &
                                     'tempw = ' , planktonic_variable(1+(j-1)*number_plankt_vari),   &
                                     'obsb = '  , planktonic_variable(17+(j-1)*number_plankt_vari)
               endif
               call randbedingungen_ergaenzen(j,einmalig)
               call tiefenprofil(j)
               
               
            endif ! Zufluss-Randknoten
         endif ! bedienter Randknoten
      enddo ! alle j Knoten
      
      ! meteorological Boundary Conditions
      ! templ(1)=temperatur_lu       ! Lufttemperatur aus Wetterstationsdaten
      ! ro(1)=luftfeuchte            ! relative Luftfeuchte in %
      ! wge(1)=wind                  ! Windgeschwindigkeit  aus Wetterstationsdaten
      ! schwi(1)=strahlung           ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
      ! cloud(1)=bewoelkung          ! Bewölkungsdichte  aus Wetterstationsdaten
      ! wtyp(1)=Cloud reflectance(?) ! Derived from cloud type (0-9)
      
      if (kontrollknoten > 0) then
          print*,'randbedingungen_setzen, prozessor 0, chla(kontrollknoten) = ',  &
                  planktonic_variable(11+(kontrollknoten-1)*number_plankt_vari)
      endif
      
   endif ! nur prozessor 0
   
   call mpi_barrier (mpi_komm_welt, ierr)
   call scatter_BC()
   call scatter_planktkon()
   
   j = kontrollknoten - meinrang * part
   if (j >= 1 .and. j <= part) then
      print*,'nach randbedingungen_setzen am Kontrollknoten:',kontrollknoten,meinrang,part,j
      print*,'Tiefe  = ', rb_hydraul_p(2+(j-1)*number_rb_hydraul)
      print*,'tempw  = ', planktonic_variable_p( 1+(j-1)*number_plankt_vari), 1+(j-1)*number_plankt_vari
      print*,'O2     = ', planktonic_variable_p( 2+(j-1)*number_plankt_vari), 2+(j-1)*number_plankt_vari ! Sauerstoffgehalt tiefengemittelt
      print*,'obsb   = ', planktonic_variable_p(17+(j-1)*number_plankt_vari)
      print*,'ocsb   = ', planktonic_variable_p(18+(j-1)*number_plankt_vari)
      print*,'cd1    = ', planktonic_variable_p(37+(j-1)*number_plankt_vari) ! leicht abbaubare gelöste organische C-Verbindungen    mg C / l
      print*,'cd2    = ', planktonic_variable_p(38+(j-1)*number_plankt_vari) ! schwer abbaubare gelöste organische C-Verbindungen    mg C / l
      print*,'cp1    = ', planktonic_variable_p(39+(j-1)*number_plankt_vari) ! leicht abbaubare partikuläre organische C-Verbindungen    mg C / l
      print*,'cp2    = ', planktonic_variable_p(40+(j-1)*number_plankt_vari) ! schwer abbaubare partikuläre organische C-Verbindungen    mg C / l
      print*,'cm     = ', planktonic_variable_p(41+(j-1)*number_plankt_vari) ! monomolekularen organischen C-Verbindungen    mg C / l
      print*,'bac    = ', planktonic_variable_p(42+(j-1)*number_plankt_vari) ! Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen    mg C / l
      print*,'o2bsb  = ', planktonic_variable_p(43+(j-1)*number_plankt_vari) ! Sauerstoff-Kohlenstoffverhältnis beim C-Abbau    mgO2/mgC
      print*,'bl01   = ', planktonic_variable_p(44+(j-1)*number_plankt_vari) ! schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent    mg O2 / l
      print*,'bl02   = ', planktonic_variable_p(45+(j-1)*number_plankt_vari) ! leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent    mg O2 / l
      print*,'vbsb   = ', planktonic_variable_p(46+(j-1)*number_plankt_vari) ! BSB5 incl. lebender Organismen    mg O2 / l
      print*,'vcsb   = ', planktonic_variable_p(47+(j-1)*number_plankt_vari) ! CSB incl. lebender Organismen    mg O2 / l
      print*,'chla   = ', planktonic_variable_p(11+(j-1)*number_plankt_vari) ! Chlorophyll-a
      print*,'vkigr  = ', planktonic_variable_p(19+(j-1)*number_plankt_vari)
      print*,'antbl  = ', planktonic_variable_p(20+(j-1)*number_plankt_vari)
      print*,'chlaki = ', planktonic_variable_p(12+(j-1)*number_plankt_vari)
      print*,'chlagr = ', planktonic_variable_p(13+(j-1)*number_plankt_vari)
      print*,'chlabl = ', planktonic_variable_p(14+(j-1)*number_plankt_vari)
      print*,'aki    = ', planktonic_variable_p( 8+(j-1)*number_plankt_vari)
      print*,'agr    = ', planktonic_variable_p( 9+(j-1)*number_plankt_vari)
      print*,'abl    = ', planktonic_variable_p(10+(j-1)*number_plankt_vari)
      print*,'akbcm  = ', planktonic_variable_p(24+(j-1)*number_plankt_vari)
      print*,'agbcm  = ', planktonic_variable_p(25+(j-1)*number_plankt_vari)
      print*,'abbcm  = ', planktonic_variable_p(26+(j-1)*number_plankt_vari)
      print*,'ph     = ', planktonic_variable_p(66+(j-1)*number_plankt_vari)
   endif 
   
end subroutine randbedingungen_setzen



subroutine scatter_rb_hydraul()
   use modell
   implicit none
   integer :: j, RB_zaehl
   ! Hydraulik-Randbedingungen (Geschwindigkeit, Wassertiefe und WSP)
   ! wurden von prozess 0 in holen_trans() gelesen und werden hier auf alle Prozesse gescattert.
   
   call MPI_Scatter(rb_hydraul, part*number_rb_hydraul, MPI_FLOAT,  &
                    rb_hydraul_p, part*number_rb_hydraul, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 14 MPI_Scatter(rb_hydraul failed :', ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
 end subroutine scatter_rb_hydraul


subroutine randwert_planktonic(jjj,zaehl)
   use modell
   use QSimDatenfelder
   use module_aparam
   implicit none
   
   integer :: jjj, zaehl, nk, i, l
   
   if (meinrang > 0) call qerror(' 789 subroutine randwert_planktonic darf nur auf prozess 0')
   if (jjj > number_plankt_point) call qerror('randwert_planktonic: jjj > number_plankt_point')
   
   nk = (jjj-1)*number_plankt_vari
   if (nk+number_plankt_vari > number_plankt_vari*number_plankt_point) then
      print*,'randwert_planktonic: jjj,zaehl = ',jjj,zaehl, number_plankt_vari, number_plankt_point
      call qerror('planktonic_variable unterdimensioniert')
   endif
   
   if (zaehl > 0) then !! Randbedingung zu dieser Nummer vorhanden
      !!if(rabe(zaehl)%zufluss)then !! z.Z. Zufluss über diesen Rand?
      planktonic_variable( 1+nk) = rabe(zaehl)%wert_jetzt(22) !  Tempw 
      planktonic_variable( 2+nk) = rabe(zaehl)%wert_jetzt(23) !  VO2 oxygen()
      planktonic_variable( 3+nk) = rabe(zaehl)%wert_jetzt( 4) !  VNH4 ncyc()
      planktonic_variable( 4+nk) = rabe(zaehl)%wert_jetzt( 5) !  VNO2 ncyc()
      planktonic_variable( 5+nk) = rabe(zaehl)%wert_jetzt( 6) !  VNO3 ncyc()
      planktonic_variable( 6+nk) = rabe(zaehl)%wert_jetzt(10) !  GELP po4s()
      planktonic_variable( 7+nk) = rabe(zaehl)%wert_jetzt(12) !  SI silikat()
      planktonic_variable( 8+nk) = 0.0 ! aki wird jetzt in algae_start() gesetzt
      planktonic_variable( 9+nk) = 0.0 ! agr wird jetzt in algae_start() gesetzt
      planktonic_variable(10+nk) = 0.0 ! abl wird jetzt in algae_start() gesetzt
      planktonic_variable(11+nk) = rabe(zaehl)%wert_jetzt(13) !  CHLA
      planktonic_variable(12+nk) = 0.0 ! chlaki wird jetzt in algae_start() gesetzt
      planktonic_variable(13+nk) = 0.0 ! chlagr wird jetzt in algae_start() gesetzt
      planktonic_variable(14+nk) = 0.0 ! chlabl wird jetzt in algae_start() gesetzt
      planktonic_variable(15+nk) = rabe(zaehl)%wert_jetzt( 8) !  VX0 ncyc()
      planktonic_variable(16+nk) = rabe(zaehl)%wert_jetzt( 9) !  VX02 ncyc()
      planktonic_variable(17+nk) = 0.0 !  OBSB  orgc_start() C-BSB5, kohlenstoffbürtig
      planktonic_variable(18+nk) = 0.0 !  OCSB  orgc_start() CSB, Kohlenstoffbürtig
      planktonic_variable(19+nk) = rabe(zaehl)%wert_jetzt(14) !  VKIGR
      planktonic_variable(20+nk) = rabe(zaehl)%wert_jetzt(15) !  ANTBL
      planktonic_variable(21+nk) = 0.01 ! svhemk 0.01 svhemk ### unklar ###
      planktonic_variable(22+nk) = 0.01 ! svhemg 0.01 svhemk ### unklar ###
      planktonic_variable(23+nk) = 0.01 ! svhemb 0.01 svhemk ### unklar ###
      planktonic_variable(24+nk) = 0.0 ! akbcm wird jetzt in algae_start() gesetzt
      planktonic_variable(25+nk) = 0.0 ! agbcm wird jetzt in algae_start() gesetzt
      planktonic_variable(26+nk) = 0.0 ! abbcm wird jetzt in algae_start() gesetzt
      planktonic_variable(27+nk) = 0.01 ! akiiv 0.01 svhemk ### unklar ###
      planktonic_variable(28+nk) = 0.01 ! agriv 0.01 svhemk ### unklar ###
      planktonic_variable(29+nk) = 0.01 ! abliv 0.01 svhemk ### unklar ###
      planktonic_variable(30+nk) = 0.0 ! Q_NK ncyc() wird jetzt in neahr_start() gesetzt
      planktonic_variable(31+nk) = 0.0 ! Q_PK po4s() wird jetzt in neahr_start() gesetzt
      planktonic_variable(32+nk) = 0.0 ! Q_SK silikat() wird jetzt in neahr_start() gesetzt
      planktonic_variable(33+nk) = 0.0 ! Q_NG ncyc() wird jetzt in neahr_start() gesetzt
      planktonic_variable(34+nk) = 0.0 ! Q_PG po4s() wird jetzt in neahr_start() gesetzt
      planktonic_variable(35+nk) = 0.0 ! Q_NB ncyc() wird jetzt in neahr_start() gesetzt
      planktonic_variable(36+nk) = 0.0 ! Q_PB po4s() wird jetzt in neahr_start() gesetzt
      planktonic_variable(37+nk) = 0.0 ! CD(1 orgc_start()
      planktonic_variable(38+nk) = 0.0 ! CD(2 orgc_start()
      planktonic_variable(39+nk) = 0.0 ! CP(1 orgc_start()
      planktonic_variable(40+nk) = 0.0 ! CP(2 orgc_start()
      planktonic_variable(41+nk) = 0.0 ! CM orgc_start()
      planktonic_variable(42+nk) = 0.0 ! BAC orgc_start()
      planktonic_variable(43+nk) = 0.0 ! O2BSB orgc_start()
      planktonic_variable(44+nk) = 0.0 ! BL01 orgc_start()
      planktonic_variable(45+nk) = 0.0 ! BL02 orgc_start()
      planktonic_variable(46+nk) = rabe(zaehl)%wert_jetzt( 2) ! vbsb  BSB5 incl. lebender Organismen, messbar, Randwert
      planktonic_variable(47+nk) = rabe(zaehl)%wert_jetzt( 3) ! vcsb  CSB incl. lebender Organismen , messbar, Randwert
      planktonic_variable(48+nk) = rabe(zaehl)%wert_jetzt(24) !  CHNF  orgc()
      planktonic_variable(49+nk) = rabe(zaehl)%wert_jetzt(25) !  BVHNF
      planktonic_variable(50+nk) = rabe(zaehl)%wert_jetzt(16) !  ZOOIND
      planktonic_variable(51+nk) = 0.0 ! abrzo1 ### wohl unbenutzt ###
      planktonic_variable(52+nk) = rabe(zaehl)%wert_jetzt(21) ! ssalg GESAMTSCHWEBSTOFFE incl. lebender Organismen, messbar, Randwert
      planktonic_variable(53+nk) = 0.0 ! SS suspendierte Sedimente ohne lebende Organismen
      planktonic_variable(54+nk) = 0.7 ! fssgr !! qsim.f90: fssgrs = 0.7
      planktonic_variable(55+nk) = 0.4 ! fbsgr !! qsim.f90: fbsgrs = 0.4
      planktonic_variable(56+nk) = 0.0 ! frfgr
      planktonic_variable(57+nk) = 0.04  !  nl0 Stickstoff zu Kohlenstoff in organischem Material  = 0.040 wenn nicht berechenbar
      planktonic_variable(58+nk) = 0.005 !  pl0 Phosphor zu Kohlenstoff in organischem Material    = 0.005 wenn nicht berechenbar
      planktonic_variable(59+nk) = 0.0 ! stind ph()
      planktonic_variable(60+nk) = 0.0 ! dlarvn dreissen.f90
      planktonic_variable(61+nk) = rabe(zaehl)%wert_jetzt(26) !  COLI
      planktonic_variable(62+nk) = rabe(zaehl)%wert_jetzt(18) !  MW
      planktonic_variable(63+nk) = 0.0 ! pw ph_aufteilung_einleitung()
      planktonic_variable(64+nk) = rabe(zaehl)%wert_jetzt(19) !  CA
      planktonic_variable(65+nk) = rabe(zaehl)%wert_jetzt(20) !  LF
      planktonic_variable(66+nk) = rabe(zaehl)%wert_jetzt(17) !  VPH
      planktonic_variable(67+nk) = rabe(zaehl)%wert_jetzt( 7) !  GESN ncyc()
      planktonic_variable(68+nk) = rabe(zaehl)%wert_jetzt(11) !  GESP po4s()
      planktonic_variable(69+nk) = 0.0 ! SKmor, Silizium in schwebenden, abgestorbenen Kieselalgen, algaeski<->silikat
      planktonic_variable(70+nk) = 0.0 ! DOSCF siehe COLIFORM()
      planktonic_variable(71+nk) = rabe(zaehl)%wert_jetzt(28) !  tracer (neu QSim-3D)
      planktonic_variable(72+nk) = 0.0 ! Salz (neu QSim-3D)
      planktonic_variable(73+nk) = 0.0 ! alter_decay
      planktonic_variable(74+nk) = 0.0 ! alter_arith
      planktonic_variable(75+nk) = 0.0 ! alter_growth
      planktonic_variable(76+nk) = GROT ! TGZoo
      planktonic_variable(77+nk) = 0.0 ! akmor_1
      planktonic_variable(78+nk) = 0.0 ! agmor_1
      planktonic_variable(79+nk) = 0.0 ! abmor_1
      planktonic_variable(80+nk) = rabe(zaehl)%wert_jetzt(48) ! Zink gesamt
      planktonic_variable(81+nk) = rabe(zaehl)%wert_jetzt(49) ! Zink gelöst
      planktonic_variable(82+nk) = rabe(zaehl)%wert_jetzt(32) ! Cadmium gesamt
      planktonic_variable(83+nk) = rabe(zaehl)%wert_jetzt(33) ! Cadmium gelöst
      planktonic_variable(84+nk) = rabe(zaehl)%wert_jetzt(38) ! Kupfer gesamt
      planktonic_variable(85+nk) = rabe(zaehl)%wert_jetzt(39) ! Kupfer gelöst
      planktonic_variable(86+nk) = rabe(zaehl)%wert_jetzt(42) ! Nickel gesamt
      planktonic_variable(87+nk) = rabe(zaehl)%wert_jetzt(43) ! Nickel gelöst
      planktonic_variable(88+nk) = rabe(zaehl)%wert_jetzt(50) ! Arsen gesamt
      planktonic_variable(89+nk) = rabe(zaehl)%wert_jetzt(51) ! Arsen gelöst
      planktonic_variable(90+nk) = rabe(zaehl)%wert_jetzt(30) ! Blei gesamt
      planktonic_variable(91+nk) = rabe(zaehl)%wert_jetzt(31) ! Blei gelöst
      planktonic_variable(92+nk) = rabe(zaehl)%wert_jetzt(34) ! Chrom gesamt
      planktonic_variable(93+nk) = rabe(zaehl)%wert_jetzt(35) ! Chrom gelöst
      planktonic_variable(94+nk) = rabe(zaehl)%wert_jetzt(36) ! Eisen gesamt
      planktonic_variable(95+nk) = rabe(zaehl)%wert_jetzt(37) ! Eisen gelöst
      planktonic_variable(96+nk) = rabe(zaehl)%wert_jetzt(44) ! Quecksilber gesamt
      planktonic_variable(97+nk) = rabe(zaehl)%wert_jetzt(45) ! Quecksilber gelöst
      planktonic_variable(98+nk) = rabe(zaehl)%wert_jetzt(40) ! Mangan gesamt
      if (isNaN(rabe(zaehl)%wert_jetzt(41))) then
         print*,'randwert_planktonic isNaN 41 jjj,zaehl = ',jjj,zaehl
         call qerror("isNaN(rabe(zaehl)%wert_jetzt(41))Mangan gelöst")
      endif
      planktonic_variable(99+nk) = rabe(zaehl)%wert_jetzt(41) ! Mangan gelöst
      planktonic_variable(100+nk) = rabe(zaehl)%wert_jetzt(46) ! Uran gesamt
      planktonic_variable(101+nk) = rabe(zaehl)%wert_jetzt(47) ! Uran gelöst
      
      if (nur_alter .and. (wie_altern == 2))call alter_rand(jjj)
   endif !! RandbedingungsNummer größer 0
   
end subroutine randwert_planktonic


!> bisher plankt_vari_vert konstant über die Tiefe d.h. 2D tiefengemittelt
subroutine tiefenprofil(jjj)
   use modell
   implicit none
   integer, intent(in) :: jjj
   integer             :: nk, i, l
   
   if (jjj > number_plankt_point)call qerror('tiefenprofil: jjj > number_plankt_point')
   nk = (jjj-1) * number_plankt_vari
   if (nk+number_plankt_vari > number_plankt_vari*number_plankt_point) then
      print*,'tiefenprofil: jjj,zaehl = ',jjj, number_plankt_vari, number_plankt_point
      call qerror('planktonic_variable unterdimensioniert 2')
   endif
   
   ! konstante Verteilung in der Vertikalen
   do i = 1,14 ! tiefenprofile der ersten 14 konzentrationen
      do l = 1,num_lev
         plankt_vari_vert(l+(i-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(i+nk)
      enddo
   enddo
   
   do l = 1,num_lev
      plankt_vari_vert(l+(16-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(67+nk) !  hgesNz = GESN
      plankt_vari_vert(l+(15-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(68+nk) !  hgesPz = GESP
      plankt_vari_vert(l+(17-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(30+nk) !  hQ_NKz = Q_NK
      plankt_vari_vert(l+(18-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(33+nk) !  hQ_NGz = Q_NG
      plankt_vari_vert(l+(19-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(35+nk) !  hQ_NBz = Q_NB
      plankt_vari_vert(l+(20-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(24+nk) !  hCChlkz = akbcm
      plankt_vari_vert(l+(21-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(25+nk) !  hCChlgz = agbcm
      plankt_vari_vert(l+(22-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = planktonic_variable(26+nk) !  hCChlbz = abbcm
   enddo
   
end subroutine tiefenprofil


subroutine randbedingungen_ergaenzen(j, einmalig)
   use modell
   use QSimDatenfelder
   use module_aparam
   use module_ph, only: pwert
   
   implicit none
   integer, intent(in) :: j
   logical, intent(in) :: einmalig
   
   integer :: nk
   real    :: cpges, cdges, cref, toc
   
   
   nk = (j-1)*number_plankt_vari
   
   control = .false.
   if (j == kontrollknoten) control = .true.
   
   ! ini_algae in initialisieren() initialisieren.f95
   call algae_start(planktonic_variable(11+nk),      & ! CHLA chlas(mstr,mRB)
                    planktonic_variable(19+nk),      & ! VKIGR vkigrs(mstr,mRB)
                    planktonic_variable(20+nk),      & ! ANTBL antbls(mstr,mRB)
                    planktonic_variable( 1+nk),      & ! tempws
                    planktonic_variable(24+nk),      & ! akbcm akbcms(mstr,mRB)
                    planktonic_variable(26+nk),      & ! abbcm abbcms(mstr,mRB)
                    planktonic_variable(25+nk),      & ! agbcm agbcms(mstr,mRB)
                    planktonic_variable( 8+nk),      & ! aki akis(mstr,mRB)
                    planktonic_variable(10+nk),      & ! abl abls(mstr,mRB)
                    planktonic_variable( 9+nk),      & ! agr agrs(mstr,mRB)
                    a1Ki,a1Bl,a1Gr,                  & ! globale Parameter direkt aus QSimDatenfelder
                    planktonic_variable(12+nk),      & ! chlaki chlaks(mstr,mRB)
                    planktonic_variable(14+nk),      & ! chlabl chlabs(mstr,mRB)
                    planktonic_variable(13+nk))        ! chlagr chlags(mstr,mRB)
   
   ! qsim.f90:!...Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser
   benthic_distribution(1+(j-1)*number_benth_distr) = planktonic_variable(1+((j-1)*number_plankt_vari))
   ! für orgC() : CSB(ocsb) und C-BSB5(obsb) in die Berechnungskonzentrationen aufteilen Bakterienmenge abschätzen
   
   if (control) then
      print*, 'randbedingungen_ergaenzen: before orgc_start'
      print*, ' aki    = ', planktonic_variable( 8+nk)
      print*, ' agr    = ', planktonic_variable( 9+nk)
      print*, ' abl    = ', planktonic_variable(10+nk)
      print*, ' OBSB   = ', planktonic_variable(17+nk)
      print*, ' OCSB   = ', planktonic_variable(18+nk)
      print*, ' CD1    = ', planktonic_variable(37+nk)
      print*, ' CD2    = ', planktonic_variable(38+nk)
      print*, ' CP1    = ', planktonic_variable(39+nk)
      print*, ' CP2    = ', planktonic_variable(40+nk)
      print*, ' CM     = ', planktonic_variable(41+nk)
      print*, ' BAC    = ', planktonic_variable(42+nk)
      print*, ' BSB    = ', planktonic_variable(46+nk)
      print*, ' CSB    = ', planktonic_variable(47+nk)
      print*, ' CHNF   = ', planktonic_variable(48+nk)
      print*, ' zooind = ', planktonic_variable(50+nk)
      print*, ' SSalg  = ', planktonic_variable(52+nk)
      print*, ' frfgr  = ', planktonic_variable(56+nk)
   endif
   
   call orgc_start(toc_csb,bsbzoo,                  & ! globale Parameter direkt aus qsimdatenfelder
                   planktonic_variable( 8+nk),      & ! aki
                   planktonic_variable(10+nk),      & ! abl
                   planktonic_variable( 9+nk),      & ! agr
                   planktonic_variable(50+nk),      & ! zooind 
                   planktonic_variable(46+nk),      & ! vbsb  bsb5 incl. lebender organismen
                   planktonic_variable(47+nk),      & ! vcsb  csb incl. lebender organismen
                   planktonic_variable(17+nk),      & ! obsb  c-bsb5, kohlenstoffbürtig
                   planktonic_variable(18+nk),      & ! ocsb  csb, kohlenstoffbürtig
                   planktonic_variable(41+nk),      & ! cm
                   planktonic_variable(37+nk),      & ! cd(1)
                   planktonic_variable(38+nk),      & ! cd(2)
                   planktonic_variable(39+nk),      & ! cp(1)
                   planktonic_variable(40+nk),      & ! cp(2)
                   planktonic_variable(52+nk),      & ! ssalg
                   planktonic_variable(56+nk),      & ! frfgr
                   planktonic_variable(42+nk),      & ! bac
                   planktonic_variable(48+nk),      & ! chnf
                   cpges, cdges, cref, toc )          ! Übergabewerte spez. für den jeweiligen Rand
   
   if (control) then
      print*, 'randbedingungen_ergaenzen: after orgc_start'
      print*, ' aki    = ', planktonic_variable( 8+nk)
      print*, ' agr    = ', planktonic_variable( 9+nk)
      print*, ' abl    = ', planktonic_variable(10+nk)
      print*, ' OBSB   = ', planktonic_variable(17+nk)
      print*, ' OCSB   = ', planktonic_variable(18+nk)
      print*, ' CD1    = ', planktonic_variable(37+nk)
      print*, ' CD2    = ', planktonic_variable(38+nk)
      print*, ' CP1    = ', planktonic_variable(39+nk)
      print*, ' CP2    = ', planktonic_variable(40+nk)
      print*, ' CM     = ', planktonic_variable(41+nk)
      print*, ' BAC    = ', planktonic_variable(42+nk)
      print*, ' BSB    = ', planktonic_variable(46+nk)
      print*, ' CSB    = ', planktonic_variable(47+nk)
      print*, ' CHNF   = ', planktonic_variable(48+nk)
      print*, ' zooind = ', planktonic_variable(50+nk)
      print*, ' SSalg  = ', planktonic_variable(52+nk)
      print*, ' frfgr  = ', planktonic_variable(56+nk)
   endif
   
   call naehr_start(planktonic_variable( 8+nk),      & ! aki
                    planktonic_variable(10+nk),      & ! abl
                    planktonic_variable( 9+nk),      & ! agr
                    planktonic_variable( 3+nk),      & ! VNH4
                    planktonic_variable( 5+nk),      & ! VNO3
                    planktonic_variable( 4+nk),      & ! VNO2
                    planktonic_variable(67+nk),      & ! GESN
                    planktonic_variable(50+nk),      & ! ZOOIND
                    planktonic_variable( 6+nk),      & ! GELP
                    planktonic_variable(68+nk),      & ! GESP
                    planktonic_variable(30+nk),      & ! Q_NK
                    planktonic_variable(31+nk),      & ! Q_PK
                    planktonic_variable(32+nk),      & ! Q_SK
                    planktonic_variable(33+nk),      & ! Q_NG
                    planktonic_variable(34+nk),      & ! Q_PG
                    planktonic_variable(35+nk),      & ! Q_NB
                    planktonic_variable(36+nk),      & ! Q_PB
                    CPges,CDges,Cref,                & ! Übergabewerte spez. für den jeweiligen Rand, nur hier definiert
                    planktonic_variable(42+nk),      & ! BAC 
                    planktonic_variable(41+nk),      & ! CM
                    planktonic_variable(57+nk),      & ! nl0 Stickstoff zu Kohlenstoff in organischem Material
                    planktonic_variable(58+nk),      & ! pl0 Phosphor zu Kohlenstoff in organischem Material
                    planktonic_variable(53+nk),      & ! SS suspendierte Sedimente ohne lebende Organismen
                    planktonic_variable(52+nk),      & ! ssalg GESAMTSCHWEBSTOFFE
                    0,0,                             & ! mstr,mRB für Kontrollausgaben in 1D benötigt
                    einmalig,control,j)
   
   if (control) print*,'randbedingungen_ergaenzen: nach algae_aufteilung',    &
                        ' chla = ',  planktonic_variable(11+nk),    &
                        ' vkigr = ', planktonic_variable(19+nk),    &
                        ' antbl = ', planktonic_variable(20+nk),    &
                        ' chlaki = ',planktonic_variable(12+nk),    &
                        ' aki = ',   planktonic_variable(8+nk),     &
                        ' akbcm = ', planktonic_variable(24+nk),    &
                        ' Caki = ',  Caki
   
   ! Berechnung des p-Wertes am Start (ohne Algen)
   call pwert(planktonic_variable(62+nk),      & ! mw
              planktonic_variable(66+nk),      & ! vph
              planktonic_variable(65+nk),      & ! lf
              planktonic_variable( 1+nk),      & ! tempw 
              planktonic_variable(63+nk))        ! pw 

   return
end subroutine randbedingungen_ergaenzen

!> Legt die Randbedingungs-Datenfelder an
!!
!! to be done PARALLEL
subroutine randbedingungen_parallel()
   use modell
   implicit none
   integer :: as
   
   allocate (rb_hydraul_p(part*number_rb_hydraul), source = 0.0, stat = as )
   if (as /= 0) call qerror("Error while allocating variable `rb_hydraul_p`.")
   
   call MPI_Scatter(rb_hydraul, part*number_rb_hydraul, MPI_FLOAT,  &
                    rb_hydraul_p, part*number_rb_hydraul, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)'randbedingungen_parallel MPI_Scatter(rb_hydraul failed :', ierr
      call qerror(fehler)
   endif
   
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Bcast(rb_extnct_ilamda,1,MPI_INT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) then
      write(fehler,*)meinrang, 'MPI_Bcast(rb_extnct_ilamda,  ierr = ', ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   
   if (meinrang /= 0) then
      allocate (rb_extnct_p(rb_extnct_ilamda*anz_extnct_koeff), source = 0.0, stat = as )
      if (as /= 0) then
         write(fehler,*)'rb_extnct_p konnte nicht allokiert werden ', as, ' prozess = ', meinrang
         call qerror(fehler)
      endif
   endif 
   
   call MPI_Bcast(rb_extnct_p,rb_extnct_ilamda*anz_extnct_koeff,MPI_FLOAT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) then
      write(fehler,*)'randbedingungen_parallel MPI_Bcast(rb_extnct_p failed :', ierr
      call qerror(fehler)
   endif
  
end subroutine randbedingungen_parallel


!> Verteilen der Datenstrukturen auf die parallelen Prozesse
!!
!! to be done PARALLEL\n
subroutine scatter_bc()
   use modell
   implicit none
   integer :: i
   
   call MPI_Scatter(rb_hydraul, part*number_rb_hydraul, MPI_FLOAT,  &
                    rb_hydraul_p, part*number_rb_hydraul, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' MPI_Scatter(rb_hydraul failed :', ierr
      call qerror(fehler)
   endif
end subroutine scatter_BC


!> Interpolate Boundary Conditions
!!
!! Linear temporal interpolation of boundary conditions.
subroutine RB_werte_aktualisieren(t)
   use modell
   implicit none
   
   integer(int64), intent(in) :: t
   
   integer :: n,j,k
   integer :: zeit_vor, zeit_nach
   logical :: randwert_gueltig, vor_da, nach_da
   real    :: a, wert_vor, wert_nach, wert

   do n = 1,ianz_rb !! alle Randbedingungen
      rabe(n)%wert_jetzt(:) = 0.0 ! initialize unused boundary concentrations
      do k = 1,n_active_concentrations !all active boundary concentrations
         vor_da = .false.
         nach_da = .false.
         do j = 1,rabe(n)%anz_rb ! alle Zeitpunkte dieser RB
            wert = rabe(n)%punkt(j)%zeil%werts(k)
            if (randwert_gueltig(wert,k)) then
               if (t >= rabe(n)%punkt(j)%zeit_sek) then ! letzter gültiger Wert vorher
                  vor_da = .true.
                  wert_vor = wert
                  zeit_vor = rabe(n)%punkt(j)%zeit_sek
               else
                  if ( .not. nach_da) then ! erster gültiger Wert danach
                     nach_da = .true.
                     wert_nach = wert
                     zeit_nach = rabe(n)%punkt(j)%zeit_sek
                  endif
               endif
               
            endif 
         enddo 
         
         if (vor_da .and. nach_da) then
            a = real(t-zeit_vor)/real(zeit_nach-zeit_vor)
            rabe(n)%wert_jetzt(k) = (1.0 - a)*wert_vor + a*wert_nach
            if (kontrollknoten > 0) then
               print*, 'Rand', n, ' t,zeit_vor,zeit_nach,wert_vor,wert_nach,a,k,rabe(n)%wert_jetzt(k) = ',  &
                       t, zeit_vor, zeit_nach, wert_vor, wert_nach, a, k, rabe(n)%wert_jetzt(k)
            endif
         
         elseif (vor_da) then
            rabe(n)%wert_jetzt(k) = wert_vor
            if (kontrollknoten > 0) then
               print*, 'Rand',n,' t,zeit_vor,wert_vor,k,rabe(n)%wert_jetzt(k) = ',  &
                       t, zeit_vor, wert_vor, k, rabe(n)%wert_jetzt(k)
            endif
         elseif (nach_da) then
            rabe(n)%wert_jetzt(k) = wert_nach
            if (kontrollknoten > 0) then
                print*, 'Rand', n,' t,zeit_nach,wert_nach,k,rabe(n)%wert_jetzt(k) = ',  &
                        t, zeit_nach, wert_nach, k, rabe(n)%wert_jetzt(k)
            endif
         
         else ! no valid value
            select case (k) ! which value

               case (1) ! Abfluss alle Werte gültig (in QSim-3D unbenutzt)
                  rabe(n)%wert_jetzt(k) = 0.0

               case (24) ! CHNF Heterotrophe Nanoflagelaten, not present
                  rabe(n)%wert_jetzt(k) = 0.0

               case (25) ! BVHNF Biovolumen der HNF, not present
                  rabe(n)%wert_jetzt(k) = 0.0

               case (26) ! COLI Fäkalcoliforme Bakterien, not present
                  rabe(n)%wert_jetzt(k) = 0.0
            
               case default ! alle anderen
                  print*,'rabe(n)%punkt(j)%zeil%werts(k)',(rabe(n)%punkt(j)%zeil%werts(k),j = 1,rabe(n)%anz_rb)
                  print*,'ianz_rb, anzrawe, rabe(n)%anz_rb, rabe(n)%t_guelt',ianz_rb, anzrawe, rabe(n)%anz_rb, rabe(n)%t_guelt
                  print*,'RB_werte_aktualisieren: Kein wert nirgends für Rand-variable ',k,' an Rand ',n, ' für Zeitpunkt ',t
                  write(fehler,*)'RB_werte_aktualisieren: Kein wert nirgends für Rand-variable '  &
                  ,k,' an Rand ',n, ' für Zeitpunkt ',t
                  call qerror(fehler)
            end select ! k
         endif
      enddo
   enddo

end subroutine RB_werte_aktualisieren

!> Randwert gueltig
logical function randwert_gueltig(wert, n)
   use modell
   implicit none
   real, intent(in)    :: wert
   integer, intent(in) :: n
  
  randwert_gueltig = .false.
   if (n > n_active_concentrations .or. n <= 0) call qerror("Invalid boundarynumber")
   
   select case (n) 
      case(1) ! Abfluss alle Werte gültig (in QSim-3D unbenutzt)
         randwert_gueltig = .TRUE.
      
      case(27) ! EWAERM Wärmeeinleitung
         randwert_gueltig = .TRUE.
      
      case(28) ! Tracer
         randwert_gueltig = .TRUE.
      
      case default ! alle anderen
         randwert_gueltig = (wert >= 0.0)
   end select
   
end function randwert_gueltig


!> Read boundary conditions from file EreigG.txt
!!
!! Details in \ref lnk_randbedingungen
subroutine read_boundary_timeseries()
   use modell
   use module_datetime
   implicit none
   
   type(datetime) :: datetime_boundary
   character(500) :: file_name
   real           :: pseudo_time
   logical        :: rb_vorhanden, randwert_gueltig
   integer        :: open_error, u_ereigg, read_error, alloc_status, ini
   integer        :: idummy, anzi, i, j, n, m, min_nr, nr, maxrandnr, nini
   integer        :: anzmax, year, month, day, hour, minute
   integer,        dimension(:), allocatable :: nr_vorhanden, rb_vorkommen
   type(rb_zeile), dimension(:), allocatable :: lesezeil


   print*
   print "(a)", repeat("-", 80)
   print "(a)", "EreigG.txt: boundary conditions"
   print "(a)", repeat("-", 80)
   
   if (ischwer == 0 .and. ikonss == 0) then
      n_active_concentrations = 28
   else
      n_active_concentrations = anzrawe
   endif
   
   
   file_name = trim(modellverzeichnis) // 'EREIGG.txt'
   open (newunit = u_ereigg , file = trim(file_name), status = 'old', action = 'read ', iostat = open_error)
   if (open_error /= 0) call qerror("Could not open " // trim(file_name))
   rewind (u_ereigg)
   
   
   ! skip file header and model settings
   do n = 1,6 
      if (.not. zeile(u_ereigg)) call qerror("Error while reading " // trim(file_name))
   enddo
   
   ! --------------------------------------------------------------------------
   ! determine timeseries length
   ! --------------------------------------------------------------------------
   ianz_rb = 0
   min_nr = 9999
   max_rand_nr = -9999
   anzmax = -1
   do
      if (.not. zeile(u_ereigg)) exit
      
      ! read block header
      read(ctext, "(i5,2x,i5,2x,i1,2x,i5)", iostat = read_error) nr, idummy, idummy, anzi
      if (read_error /= 0) call qerror("Error while reading " // trim(file_name))
      
      min_nr = min(min_nr, nr)
      max_rand_nr = max(max_rand_nr, nr)
      anzmax = max(anzi, anzmax)
      ianz_rb = ianz_rb + 1
      
      ! skip block body
      if (anzi == 0) cycle
      do n = 1,anzi
         if (.not. zeile(u_ereigg)) then
            write(fehler,"(a,i0,a,i0)") "Error while reading " // trim(file_name) // " at boundary ", &
                                       ianz_rb, " line ", n
            call qerror(fehler)
         endif 
      
      enddo
   
   enddo
   
   if ((max_rand_nr <= 0) .or. (max_rand_nr > 9999)) call qerror("Invalid boundary number in " // trim(file_name))
   
   print "(a,i0)", "ianz_rb = ", ianz_rb
   print "(a,i0)", "anzmax  = ", anzmax
   
   allocate(rabe(ianz_rb), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating variable `rabe`")
   
   rabe(:)%anz_rb = 0
   
  
   ! --------------------------------------------------------------------------
   ! read data
   ! --------------------------------------------------------------------------
   rewind(u_ereigg)
   ! skip file header
   do n = 1,6  
      if ( .not. zeile(u_ereigg)) call qerror("Error while reading " // trim(file_name))
   enddo 
   
   do n = 1,ianz_rb 
      if (.not. zeile(u_ereigg)) call qerror("Error while reading " // trim(file_name))
      
      ! --- read block header ---
      read(ctext, *, iostat = read_error) rabe(n)%nr_rb, idummy, rabe(n)%tagesmittelwert_flag, anzi
      if (read_error /= 0) call qerror("Error while reading " // trim(file_name))
   
      
      allocate (lesezeil(anzi), stat = alloc_status )
      if (alloc_status /= 0) call qerror("Error while allocating variable `lesezeil`.")
      
      do ini = 1,anzi
         lesezeil(ini)%itag = 0
      enddo
      
      i = 0
      
      ! --- read block body ---
      do m = 1,anzi !alle Zeitpunkte dieser Randlinie
         if (.not. zeile(u_ereigg)) call qerror("Error while reading " // trim(file_name))
         
         lesezeil(m)%werts(:) = -1.0
         read(ctext, *, iostat = read_error) lesezeil(m)%itag,     &               
                                             lesezeil(m)%imonat,   &
                                             lesezeil(m)%ijahrl,   &
                                             lesezeil(m)%uhrl,     &
                                             lesezeil(m)%werts(1:n_active_concentrations)
         
         if (read_error /= 0 .or. any(isnan(lesezeil(m)%werts(:)))) then
            write(fehler, "(a,i0,a,i0)") "Error while reading boundary ", rabe(n)%nr_rb, ", line ", m
            call qerror(fehler)
         endif
         
         if (randwert_gueltig(lesezeil(m)%werts(22), 22)) then
            i = i+1
         endif
      enddo
      
      
      
      rabe(n)%t_guelt = i
      rabe(n)%anz_rb = anzi
      allocate (rabe(n)%punkt(rabe(n)%anz_rb), stat = alloc_status )
      if (alloc_status /= 0) call qerror("Error while allocation variable `rabe(n)%punkt(i)`")

      do ini = 1,rabe(n)%anz_rb
         rabe(n) % punkt(ini) % zeit_sek = 0
      enddo
      
      
      ! convert time into unixtime
      do i = 1,rabe(n)%anz_rb 
         year        = lesezeil(i)%ijahrl
         day         = lesezeil(i)%itag
         month       = lesezeil(i)%imonat
         pseudo_time = lesezeil(i)%uhrl
         
         hour = int(pseudo_time)
         minute = int((pseudo_time - floor(pseudo_time)) * 100.)
         
         datetime_boundary = datetime(year, month, day, hour, minute, tz = tz_qsim)
         
         rabe(n)%punkt(i)%zeit_sek = datetime_boundary % seconds_since_epoch()
         rabe(n)%punkt(i)%zeil = lesezeil(i)
         
      enddo
   
      deallocate (lesezeil)
    
      if (rabe(n)%nr_rb <= 0) call qerror("Invalid boundary number in " // file_name)
   enddo
   
   close(u_ereigg)

   ! --- print summary to console ---
   print*
   
   ! table header
   print "(a)",   "boundary timeseries"
   print "(3x,a)", repeat("-", 45)
   print "(3x,a)", "        id  n_values  valid_values  averaged?"
   print "(3x,a)", repeat("-", 45)
   
   ! table body
   do n = 1,ianz_rb 
      print "(3x,i4,2x,i4,2x,i8,2x,i12,2x,l9)", &
            n, rabe(n) % nr_rb,                 &
               rabe(n) % anz_rb,                &
               rabe(n) % t_guelt,               &
              (rabe(n) % tagesmittelwert_flag == 0)
   enddo
   print "(3x,a)", repeat("-", 45)
   print*
   
   ! Check boundary ids
   do i = 1,ianz_rb
      do n = i+1,ianz_rb
         
         if (rabe(i)%nr_rb == rabe(n)%nr_rb) then
            write(fehler,*)'Randnummer von Rand', i, ' = ',rabe(i)%nr_rb,' ist gleich Randnummer von Rand ',n
            call qerror(fehler)
         endif
      enddo
   enddo
   
   
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         maxrandnr = 0
         do j = 1,knotenanzahl2D !! max Randnummer ermitteln:
            if (knoten_rand(j) > 0) then !! Randknoten
               if (knoten_rand(j) > maxrandnr)maxrandnr = knoten_rand(j)
            endif ! Randknoten
         enddo
         
         allocate (nr_vorhanden(maxrandnr), source = 0, stat = alloc_status )
         
         allocate (rb_vorkommen(maxrandnr), source = 0, stat = alloc_status )
         if (alloc_status /= 0) call qerror("Error while allocating variable `rb_vorkommen`")
         
         do j = 1,knotenanzahl2D
            if (knoten_rand(j) > 0) nr_vorhanden(knoten_rand(j)) = nr_vorhanden(knoten_rand(j)) + 1
         enddo 
         
         do j = 1,maxrandnr
            if (nr_vorhanden(j) > 0) then
               print*,'Randnummer ',j,' kommt an ',nr_vorhanden(j),' Knoten vor.'
            else
               print*,'Randnummer ',j,' kommt nie vor.'
            endif 
         enddo 
         
         do j = 1,maxrandnr !! Randbedingung für alle Randnummern vorhanden?
            rb_vorkommen(j) = 0
            do n = 1,ianz_rb !! alle Randbedingungen
               if (j == rabe(n)%nr_rb) then
                  print*,'Randnummer mit dem Zähler',n,' bedient die Randbedingung mit der Nummer ',rabe(n)%nr_rb
                  rb_vorkommen(j) = rb_vorkommen(j) + 1
               endif
            enddo
            
            if (rb_vorkommen(j) < 1)print*,'### Warnung ###: für Randnummer ',j,' wurde keine Randbedingung vorgegeben.'
            if (rb_vorkommen(j) > 1) then
               write(fehler,*)'### Abbruch 284 ### für Randnummer ',j,' wurden ',rb_vorkommen(j), ' Randbedingungen angegeben.'
               call qerror(fehler)
            endif
         enddo 
         
         !! Randzähler in Feld knoten_rand() schreiben
         do j = 1,knotenanzahl2D
            if (knoten_rand(j) > 0) then !! falls Randknoten
               if (rb_vorkommen(knoten_rand(j)) == 0) then !! nicht bediente Randnummern ausrangieren.
                  knoten_rand(j) = 999999
               else
                  nini = -7
                  do n = 1,ianz_rb !! alle Randbedingungen
                     if (knoten_rand(j) == rabe(n)%nr_rb) then
                        nini = n
                     endif
                  enddo !! alle n Randbedingungen
                  knoten_rand(j) = nini
               endif !! nicht bediente Randnummer
         
               if (j == kontrollknoten) then
                  print*,"Kontrollknoten #",j," ist Randknoten mit der Rand-zähler ", knoten_rand(j)
                  if (knoten_rand(j) <= ianz_rb) then
                     print*,"Diesem Randknoten ist keine Randbedingung #",rabe(knoten_rand(j))%nr_rb," zugeordnet."
                  else
                     print*,"Diesem Randknoten ist keine Randbedingung zugeordnet"
                  endif !Randbedingung zugeordnet
               endif ! kontrollknoten
            endif ! Randknoten
         enddo ! alle j Knoten
         
         deallocate (nr_vorhanden)
         deallocate (rb_vorkommen)
      
      case(2) ! Untrim² netCDF
         min_nr = 99999
         maxrandnr = 0
         do j = 1,n_elemente
            if (element_rand(j) > maxrandnr) maxrandnr = element_rand(j)
            if ( (element_rand(j) < min_nr) .and. (element_rand(j) > 0) )min_nr = element_rand(j)
            rb_vorhanden = .false.
            
            do n = 1,ianz_rb
               if (element_rand(j) == rabe(n)%nr_rb) rb_vorhanden = .true.
            enddo
      
            if (.not. rb_vorhanden .and. element_rand(j) /= 0)  then
               write(fehler, "(a,i0,a,i0)")                                                      &
                  "Error in read_boundary_timeseries. Missing boundary condition for element ",  &
                  j, " of bounday ", element_rand(j)
               call qerror(fehler)
            endif
         enddo
         
         allocate (nr_vorhanden(maxrandnr), source = 0, stat = alloc_status )
         allocate (rb_vorkommen(maxrandnr), source = 0, stat = alloc_status )
         
         
         ! check, if all boundary conditions are given
         do j = 1,maxrandnr 
            do n = 1,ianz_rb
               if (j == rabe(n)%nr_rb) then
                  rb_vorkommen(j) = rb_vorkommen(j) + 1
               endif
            enddo
            
            if (rb_vorkommen(j) > 1) then
               write(fehler, "(a,i0,a,i0,a)") "Boundary ", j, " has ",  rb_vorkommen(j), " boundary conditions."
               call qerror(fehler)
            endif
         enddo
         
         do j = 1,n_elemente !! vorhandene Randummern durchzählen.
            if (element_rand(j) > 0) nr_vorhanden(element_rand(j)) = nr_vorhanden(element_rand(j)) + 1
         enddo
        
         do j = 1,maxrandnr
            print "(a,i3,a,i5,a)", "Boundary ", j , " is assigned to ", nr_vorhanden(j), " elements."
         enddo 
        
         do n = 1,ianz_rb
            if (rabe(n)%nr_rb < min_nr .or. rabe(n)%nr_rb > maxrandnr)  then
               print "(a,i0,a,i0,a)", "Note: Boundary ", n, "(nr = ", rabe(n)%nr_rb, ") is not used."
            endif
         enddo
         
         !! Randnummer in Randzähler umwandeln.
         do j = 1,n_elemente
            if (element_rand(j) > 0) then !! falls Rand
               if (rb_vorkommen(element_rand(j)) == 0) then !! nicht bediente Randnummern ausrangieren.
                  element_rand(j) = 999999
               else
                  nini = -7
                  do n = 1,ianz_rb 
                     if (element_rand(j) == rabe(n)%nr_rb) then
                        nini = n
                     endif
                  enddo

                  element_rand(j) = nini
               endif 
               
               if (j == kontrollknoten) then
                  print*,"control-Element #",j," hat Rand-zähler ", element_rand(j)
                  if (element_rand(j) <= ianz_rb) then
                     print*,"Diesem Rand-Element ist keine Randbedingung #",rabe(element_rand(j))%nr_rb," zugeordnet."
                  else
                     print*,"Diesem RandElement ist keine Randbedingung zugeordnet"
                  endif !Randbedingung zugeordnet
               endif ! kontrollknoten
            endif ! Randknoten
         enddo ! alle j Elemente
         
         deallocate (nr_vorhanden)
         deallocate (rb_vorkommen)
      
      
      case(3) ! SCHISM
         maxrandnr = 0
         do j = 1,knotenanzahl2D !! max Randnummer ermitteln:
            if (knoten_rand(j) > 0) then !! Randknoten
               if (knoten_rand(j) > maxrandnr)maxrandnr = knoten_rand(j)
            endif ! Randknoten
         enddo ! alle j Knoten
         
         allocate (nr_vorhanden(maxrandnr), source = 0, stat = alloc_status )
         allocate (rb_vorkommen(maxrandnr), source = 0, stat = alloc_status )
         if (alloc_status /= 0) then
            write(fehler,*)'nr_vorhanden(maxrandnr) konnte nicht allokiert werden ', alloc_status
            call qerror(fehler)
         endif ! alloc_status .ne.0
         
         
         do j = 1,knotenanzahl2D !! vorhandene Randummern durchzählen.
            if (knoten_rand(j) > 0)nr_vorhanden(knoten_rand(j)) = nr_vorhanden(knoten_rand(j)) + 1
         enddo ! alle j Knoten
         
         do j = 1,maxrandnr !! alle Randnummern ausgeben:
            if (nr_vorhanden(j) > 0) then
               print*,'Randnummer ',j,' kommt an ',nr_vorhanden(j),' Knoten vor. -SCHISM'
            else
               print*,'Randnummer ',j,' kommt nie vor. -SCHISM'
            endif !
         enddo ! alle j Randnummern
         
         do j = 1,maxrandnr !! Randbedingung für alle Randnummern vorhanden?
            rb_vorkommen(j) = 0
            do n = 1,ianz_rb !! alle Randbedingungen
               if (j == rabe(n)%nr_rb) then
                  print*,'Randnummer mit dem Zähler',n,' bedient die Randbedingung mit der Nummer  -SCHISM',rabe(n)%nr_rb
                  rb_vorkommen(j) = rb_vorkommen(j) + 1
               endif
            enddo !! alle n Randbedingungen
            if (rb_vorkommen(j) < 1)print*,'### Warnung ###: für Randnummer ',j,' wurde keine Randbedingung vorgegeben. -SCHISM'
            if (rb_vorkommen(j) > 1) then
               write(fehler,*)'### Abbruch 284 ### für Randnummer ',j,' wurden ',rb_vorkommen(j)  &
                   , ' Randbedingungen angegeben. -SCHISM'
               call qerror(fehler)
            endif
         enddo ! alle j Randnummern
         
         !! Randnummer in Feld knoten_rand() durch Randzähler ersetzen !!
         do j = 1,knotenanzahl2D
            if (knoten_rand(j) > 0) then !! falls Randknoten
               if (rb_vorkommen(knoten_rand(j)) == 0) then !! nicht bediente Randnummern ausrangieren.
                  knoten_rand(j) = 999999
               else
                  nini = -7
                  do n = 1,ianz_rb !! alle Randbedingungen
                     if (knoten_rand(j) == rabe(n)%nr_rb) then
                        nini = n
                     endif
                  enddo !! alle n Randbedingungen
                  knoten_rand(j) = nini
               endif !! nicht bediente Randnummer
         
               if (j == kontrollknoten) then
                  print*,"Kontrollknoten #",j," ist Randknoten mit der Rand-zähler ", knoten_rand(j)
                  if (knoten_rand(j) <= ianz_rb) then
                     print*,"Diesem Randknoten ist keine Randbedingung #",rabe(knoten_rand(j))%nr_rb," zugeordnet."
                  else
                     print*,"Diesem Randknoten ist keine Randbedingung zugeordnet"
                  endif !Randbedingung zugeordnet
               endif ! kontrollknoten
            endif ! Randknoten
         enddo ! alle j Knoten
         
         deallocate (nr_vorhanden)
         deallocate (rb_vorkommen)
         
      case default
          call qerror("read_boundary_timeseries: invalid value for variable `hydro_trieb`.")
   end select
   
  
end subroutine read_boundary_timeseries


!> Read Parameters for Light Extinction
!!
!! Details in \ref lnk_extnct_rb in \ref lnk_datenmodell
subroutine read_extnct()
   use modell
   use QSimDatenfelder
   use module_aparam
   implicit none
   
   integer :: i, alloc_status
   
   if (meinrang /= 0) call qerror("subroutine read_extnct must only be called from processor 0")
   
   cpfad = trim(modellverzeichnis)
   call e_extnct_lesen(ilamda, eta, aw, ack, acg, acb, ah, as, al, cpfad)
   rb_extnct_ilamda = ilamda

   allocate(rb_extnct_p(rb_extnct_ilamda*anz_extnct_koeff), source = 0., stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating variable `rb_etnct_p`.")

   do i = 1,ilamda
      rb_extnct_p(1 + (i-1)*anz_extnct_koeff) = eta(i)
      rb_extnct_p(2 + (i-1)*anz_extnct_koeff) = aw(i)
      rb_extnct_p(3 + (i-1)*anz_extnct_koeff) = ack(i)
      rb_extnct_p(4 + (i-1)*anz_extnct_koeff) = acg(i)
      rb_extnct_p(5 + (i-1)*anz_extnct_koeff) = acb(i)
      rb_extnct_p(6 + (i-1)*anz_extnct_koeff) = ah(i)
      rb_extnct_p(7 + (i-1)*anz_extnct_koeff) = as(i)
      rb_extnct_p(8 + (i-1)*anz_extnct_koeff) = al(i)
   enddo
   
end subroutine read_extnct

!> Allocate data array for hydraulic boundary conditions
subroutine alloc_hydraul_BC(nk)
   use modell
   implicit none
   
   integer, intent(in) :: nk
   integer             :: as
   
   ! allocate (rb_hydraul(nk*number_rb_hydraul), stat = as )
   allocate(rb_hydraul(part*proz_anz*number_rb_hydraul), source = 0., stat = as)
   if (as /= 0) call qerror("Error while allocating variable `rb_hydraul`.")
   
end subroutine alloc_hydraul_BC

!> Volumenstrom und Tracerflüsse entlang aller ianz_rb Ränder ermitteln
!! 
!! ruft subroutine flux() aus schnitt.f95 auf
subroutine rand_flux(zeitzaehler)
   use modell
   implicit none
   
   integer, intent(in) :: zeitzaehler
   
   integer ::  n, i, k
   integer :: nbot, ntop, fall
   real    :: deltax, d1, d2, deltad, x_kreuz, u1, u2, v1x,v1y,v2x,v2y, vox2, volst2
   real    :: lang, flaeche, vol_strom, pot_ener_flux, kin_ener_flux
   real    :: la,flae,vox,pox,kix
   real    :: c1, c2, masx, massen_flux
   
   do n = 1,ianz_rb !! alle Randbedingungen
      lang      = 0.0
      flaeche   = 0.0
      vol_strom = 0.0
      volst2    = 0.0
      pot_ener_flux = 0.0
      kin_ener_flux = 0.0
      massen_flux   = 0.0
      
      do i = 1, rabe(n)%randlinie%anzkanten ! alle i Kanten aus randlinie n
         nbot = rabe(n)%randlinie%kante(i)%bottom
         if (nbot <= 0) then
            write(fehler,*)nbot,' = nbot = rabe(',n,')%randlinie%kante(',i,')%bottom <= 0 '
            call qerror(fehler)
         endif
      
         ntop = rabe(n) % randlinie % kante(i) % top
         deltax = ( (rabe(n)%randlinie%kante(i)%normal_x**2) + (rabe(n)%randlinie%kante(i)%normal_y**2) )**0.5
         d1 = p(nbot) - knoten_z(nbot)
         d2 = p(ntop) - knoten_z(ntop)
         v1x = u(nbot) * cos(dir(nbot))
         v1y = u(nbot) * sin(dir(nbot))
         v2x = u(ntop) * cos(dir(ntop))
         v2y = u(ntop) * sin(dir(ntop))
         u1 = rabe(n) % randlinie % kante(i) % normal_x * v1x   &
            + rabe(n) % randlinie % kante(i) % normal_y * v1y
         u2 = rabe(n) % randlinie % kante(i) % normal_x * v2x   &
            + rabe(n) % randlinie % kante(i) % normal_y * v2y
         
         c1 = planktonic_variable(71+(nbot-1)*number_plankt_vari) !! passiver alters-tracer
         c2 = planktonic_variable(71+(ntop-1)*number_plankt_vari)
         
         call flux_casu(deltax,d1,d2,u1,u2,c1,c2,p(nbot),p(ntop),u(nbot),u(ntop),la,flae,vox,pox,kix,masx)
         vox2 = 0.5*(u1*d1 + u2*d2) !! Volumentromermittlung stückweise zu Testzwecken
         lang = lang + la
         flaeche = flaeche + flae
         vol_strom = vol_strom + vox
         pot_ener_flux = pot_ener_flux + pox
         kin_ener_flux = kin_ener_flux + kix
         volst2 = volst2+vox2
         massen_flux = massen_flux + masx
      enddo
      
      
      print*,"rand_flux: Rand #",n,' mit ',rabe(n)%randlinie%anzkanten ,' Kanten'&
      ," pot_ener_flux(MW) = ",pot_ener_flux/1000000," kin_ener_flux(MW) = ",kin_ener_flux/1000000
      
      ! Flux-Felder randflux_gang(Randzähler,Zeitpunkt,??) :
      randflux_gang(n,zeitzaehler,1) = lang
      randflux_gang(n,zeitzaehler,2) = flaeche
      randflux_gang(n,zeitzaehler,3) = vol_strom
      randflux_gang(n,zeitzaehler,4) = pot_ener_flux * 1e-6 ! in Mega-Watt
      randflux_gang(n,zeitzaehler,5) = kin_ener_flux * 1e-6 ! in Mega-Watt
      randflux_gang(n,zeitzaehler,6) = massen_flux 
   enddo ! alle n Randbedingungen
 
 
end subroutine rand_flux


!Flux-Felder sollen mal: 1=Volumenrstrom, 2=potentieller und 3=kinetischer Energiefluss; n_pl=Anzahl der auszugebenden planktischen Variablen
!----+-----+----
!
!> Randknoten zu einer Randlinie zusammenstellen
!!
!! damit Durchfluss-Integrationen von rand_flux() ausgeführt werden können
!! läuft nur auf Prozess 0
subroutine randlinie_zusammenstellen()
   use modell
   implicit none
   
   integer :: j,k, n, anzranz, nexi, anzel, alloc_status , dreidrin, vierdrin,nzwi
   real    :: kx,ky,nx,ny,einwaerts,lang,kantlang
   logical :: top
   
   if (meinrang /= 0) call qerror('randlinie_zusammenstellen() darf nur auf Prozess 0')
   
   print*,"randlinie_zusammenstellen() auf 0"

   do n = 1,ianz_rb !! alle Randbedingungen
      anzranz = 0
      lang = 0.0
      do j = 1,knotenanzahl2D ! alle j Knoten
         if (knoten_rand(j) == n ) then
            anzranz = anzranz+1
         endif
      enddo 
      
      print*,"randlinie_zusammenstellen: Der ",n,"-te Rand mit Nummer = ",rabe(n)%nr_rb," hat ",anzranz," Knoten"
      
      rabe(n)%randlinie%anzkanten = anzranz-1
      
      allocate (rabe(n)%randlinie%kante(rabe(n)%randlinie%anzkanten+1), stat = alloc_status )
      if (alloc_status /= 0) call qerror('allocate (rabe(n)%randlinie%kante fehlgeschlagen')
      
      anzel = 0
      do j = 1,n_elemente ! alle j Elemente
         nexi = 0
         dreidrin = 0
         vierdrin = 0
         do k = 1,cornernumber(j) ! 3 oder 4
            if ( knoten_rand(elementnodes(j,k)) == n) nexi = nexi+1
         enddo ! alle k knoten im Element j
         
         if (nexi == 2) then 
            ! Normalfall, Element hat eine Kante auf dem Rand
            anzel = anzel+1
            rabe(n)%randlinie%kante(anzel)%element = j
            top = .true.
            do k = 1,cornernumber(j) ! alle 3 oder 4 Knoten im Element j
               if ( knoten_rand(elementnodes(j,k)) == n) then ! Randknoten
                  if ( .not. top) rabe(n)%randlinie%kante(anzel)%bottom = elementnodes(j,k)
                  if (top) then
                     rabe(n)%randlinie%kante(anzel)%top = elementnodes(j,k)
                     top = .false.
                  endif ! top
               else ! nicht Randknoten
                  if (dreidrin > 0)vierdrin = elementnodes(j,k)
                  if (vierdrin == 0)dreidrin = elementnodes(j,k)
               endif ! Randknoten
            enddo ! alle k Knoten im Element j (2-Knoten-Randelement)
            
            kx = knoten_x(rabe(n) % randlinie % kante(anzel) % top)     &
               - knoten_x(rabe(n) % randlinie % kante(anzel) % bottom)
            ky = knoten_y(rabe(n) % randlinie % kante(anzel) % top)     &
               - knoten_y(rabe(n) % randlinie % kante(anzel) % bottom)
               
            nx = -1*ky
            ny = kx
            kx = knoten_x(dreidrin) - knoten_x(rabe(n) % randlinie % kante(anzel) % bottom)
            ky = knoten_y(dreidrin) - knoten_y(rabe(n) % randlinie % kante(anzel) % bottom)
            
            einwaerts = kx*nx + ky*ny
            if (einwaerts >= 0.0) then ! Normalenvektor auswärts drehen + top-bottom tauschen
               rabe(n)%randlinie%kante(anzel)%normal_x = -1*nx
               rabe(n)%randlinie%kante(anzel)%normal_y = -1*ny
               nzwi = rabe(n)%randlinie%kante(anzel)%bottom
               rabe(n)%randlinie%kante(anzel)%bottom = rabe(n)%randlinie%kante(anzel)%top
               rabe(n)%randlinie%kante(anzel)%top = nzwi
            else ! Normalenvektor übernehmen
               rabe(n)%randlinie%kante(anzel)%normal_x = nx
               rabe(n)%randlinie%kante(anzel)%normal_y = ny
            endif ! einwärts
            kantlang = ( (rabe(n)%randlinie%kante(anzel)%normal_x**2) + (rabe(n)%randlinie%kante(anzel)%normal_y**2) )**0.5
            lang = lang+kantlang
         endif ! Normalfall
         
         if (nexi >= 3) then ! unerwünschter Sonderfall
            write(fehler,*),"Am Rand mit Nummer = ",rabe(n)%nr_rb," hat das Element #",j,  &
                            "hat mehr als eine Randkante, dies ist unerwünscht und wird zur Zeit nicht behandelt."  &
                           ," ACHTUNG dadurch sind an diesem Rand die Integrale unvollständig."

            print*,trim(fehler)
         endif ! unerwünscht
         !if(nexi.eq.4)anzel=anzel+3 ! unerwünschter Sonderfall: 3 Kanten eines Vierecks sind Rand
         !if(nexi.gt.1)then !! Element j hat Kante an Rand n
         !   print*,"Element #",j," hat ",nexi," Knoten am Rand ",n," Randkantenlänge=",kantlang
         !   print*,"top ", rabe(n)%randlinie%kante(anzel)%top, knoten_x(rabe(n)%randlinie%kante(anzel)%top)  &
         ! &                      , knoten_y(rabe(n)%randlinie%kante(anzel)%top)
         !   print*,"bottom ", rabe(n)%randlinie%kante(anzel)%bottom, knoten_x(rabe(n)%randlinie%kante(anzel)%bottom)  &
         !&                         , knoten_y(rabe(n)%randlinie%kante(anzel)%bottom)
         !endif ! Randkante
      enddo ! alle j Elemente
      
      print*,"randlinie_zusammenstellen: Der ",n,"-te Rand mit Nummer = ",rabe(n)%nr_rb," hat ",anzel  &
            ," Kanten und ist ",lang," m lang"
      
      if (anzel == rabe(n)%randlinie%anzkanten) then
         print*,"Rand mit Nummer = ",rabe(n)%nr_rb,"regulär"
      else
         if (anzel == rabe(n)%randlinie%anzkanten+1) then
            print*,"Rand mit Nummer = ",rabe(n)%nr_rb,"geschlossen oder defekt ???"
            rabe(n)%randlinie%anzkanten = anzel
         else
            write(*,*),"Am ",n,"-ten Rand mit Nummer = ",rabe(n)%nr_rb," passen Knotenanzahl = "  &
                      ,rabe(n)%randlinie%anzkanten+1 ," und Kantenanzahl",anzel," nicht zusammen"
         endif
      endif
   enddo
   ! Das Allocieren der Flux-Felder geschieht erst in ganglinien_parallel()
   ! weil dort erst die Anzahl der Konzentrationen für die Massenflüsse zusammengezählt wird.
   
end subroutine randlinie_zusammenstellen

