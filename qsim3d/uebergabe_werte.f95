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
!----------------------------------------------------------------------------------------- uebergabekonzentrationen
!----+-----+----
!> <h1>Verteilen der Datenstrukturen auf die parallelen Prozesse</h1>
!! .... to be done\n
subroutine ueber_parallel()
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: allostat
   call MPI_Bcast(number_trans_quant_points,1,MPI_INT,0,mpi_komm_welt,ierr)
   !      print*,meinrang,' ueber_parallel, number_trans_quant_points,number_trans_quant='  &
   !     &      ,number_trans_quant_points,number_trans_quant
   call broadcast_parameter()
   !      print*,meinrang,' broadcast_parameter'
   allocate (transfer_quantity_p(number_trans_quant*part), stat = allostat )
   if (allostat /= 0) then
      write(fehler,*)' Rueckgabewert   von   allocate transfer_quantity_p :', allostat
      call qerror(fehler)
   end if
   !      print*,meinrang,'allocate (transfer_quantity_p    number_trans_quant,number_trans_quant_points,part=' &
   !     &      ,number_trans_quant,part
   allocate (trans_quant_vert_p(number_trans_quant_vert*part*num_lev_trans), stat = allostat )
   if (allostat /= 0) then
      write(fehler,*)' allocate (trans_quant_vert_p failed :', allostat
      call qerror(fehler)
   end if
   !      print*,meinrang,' allocate (trans_quant_vert_p  -  number_trans_quant_vert,num_lev_trans,part'  &
   !     &      ,number_trans_quant_vert,num_lev_trans,part
   !call mpi_barrier (mpi_komm_welt, ierr)
   !print*,meinrang,' ueber_parallel vor scatter_ueber'
   call scatter_ueber()
   !call mpi_barrier (mpi_komm_welt, ierr)
   !print*,meinrang,' ueber_parallel nach scatter_ueber'
   return
end subroutine ueber_parallel
!----+-----+----
!> <h1>Verteilen der Datenstrukturen auf die parallelen Prozesse</h1>
!! .... to be done\n
subroutine scatter_ueber()
   use modell
   implicit none
   call MPI_Bcast(transfer_value_p,number_trans_val,MPI_FLOAT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) then
      write(fehler,*)' MPI_Bcast(transfer_value_p failed :',ierr
      call qerror(fehler)
   end if
   call mpi_barrier (mpi_komm_welt, ierr)
   !print*,meinrang,'scatter_ueber: MPI_Bcast(transfer_value_p,  number_trans_val,part=',number_trans_val,part
   !call mpi_barrier (mpi_komm_welt, ierr)
   !print*,meinrang,"size(transfer_quantity_p)=",size(transfer_quantity_p)
   !call mpi_barrier (mpi_komm_welt, ierr)
   !if(meinrang.eq.0)print*,"size(transfer_quantity)=",size(transfer_quantity)
   !call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Scatter(transfer_quantity, part*number_trans_quant, MPI_FLOAT,  &
                    transfer_quantity_p, part*number_trans_quant, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' MPI_Scatter(transfer_quantity failed :',ierr
      call qerror(fehler)
   end if
   call mpi_barrier (mpi_komm_welt, ierr)
   !print*,meinrang,' scatter_ueber nach MPI_Scatter(transfer_quantity'
   call MPI_Scatter(trans_quant_vert, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT,  &
                    trans_quant_vert_p, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' MPI_Scatter(trans_quant_vert failed :', ierr
      call qerror(fehler)
   end if
   !      print*,meinrang,'scatter_ueber: MPI_Scatter(trans_quant_vert,ierr,number_trans_quant_vert,part,num_lev_trans'  &
   !     &      ,ierr,number_trans_quant_vert,part,num_lev_trans
   call mpi_barrier (mpi_komm_welt, ierr)
   !print*,meinrang,' scatter_ueber nach MPI_Scatter(trans_quant_vert,'
   return
end subroutine scatter_ueber
!----+-----+----
!> <h1>wieder-einsammeln der Datenstrukturen von den parallelen Prozesse</h1>
!! \n
subroutine gather_ueber()
   use modell
   implicit none
   integer :: i
   call MPI_Gather(transfer_quantity_p, part*number_trans_quant, MPI_FLOAT,  &
                   transfer_quantity, part*number_trans_quant, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' MPI_Gather(transfer_quantity failed :', ierr
      call qerror(fehler)
   end if
   call MPI_Gather(trans_quant_vert_p, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT,  &
                   trans_quant_vert, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT, 0,mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' MPI_Gather(trans_quant_vert failed :', ierr
      call qerror(fehler)
   end if
   return
end subroutine gather_ueber
!----+-----+----
!> Initialisierung der nicht-transportierten Übergabe-Konzentrationen und Werte.
!! \n\n
subroutine ini_ueber(nk)
   use modell
   implicit none
   integer nk,i,n,as,j,l,k
   real, parameter  :: bk1 = 0.51 , bk2 = 0.02  !! Konstanten wie in orgc gesetzt
   if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten
      !--------------------------------------------- Übergabe Konzentrationen
      number_trans_quant_points = nk
      if (num_lev_trans /= num_lev) then
         write(fehler,*)'Anzahl der Levels num_lev_trans und num_lev(planktonic) müssen gleich sein',num_lev_trans,num_lev
         call qerror(fehler)
      endif
      !--------single (global) transfer values
      do j = 1,number_trans_val ! initialise
         write(trans_val_name(j),'(18x)')
      end do
      trans_val_name( 1) = "               nix" ! leer
      trans_val_name( 2) = "              nZoo"
      trans_val_name( 3) = "              pZoo"
      trans_val_name( 4) = "               bk1"
      trans_val_name( 5) = "               bk2"
      trans_val_name( 6) = "            Saettk"
      trans_val_name( 7) = "            tauscs" ! Schiffseinfluss     qsim.f90: tauscs = 1.25
      trans_val_name( 8) = "            saettg"
      trans_val_name( 9) = "            saettb"
      trans_val_name(10) = "              it_h" ! Anzahl der Zeitschritte während der Hellphase
      !algae_huelle.f95:      saettg = transfer_value_p(8)    ! ???
      !algae_huelle.f95:      saettb = transfer_value_p(9)    ! ???
      !trans_val_name()= "               " !
      do j = 1,number_trans_val ! initialise
         transfer_value_p(j) = 0.0 !!!####!0.0
      end do
      do j = 1,number_trans_val ! default: no output
         output_trans_val(j) = .false.
      end do
      !#! transfer_value_p(2)=
      !#! transfer_value_p(3)=
      transfer_value_p(4) = 0.51 !! real, parameter  :: bk1 = 0.51 , bk2 = 0.02
      transfer_value_p(5) = 0.02 !! Konstanten wie in orgc gesetzt
      !saettk    transfer_value_p(6)=  !! Rückgabewert Algen
      transfer_value_p(7) = 1.25 ! Schiffseinfluss     qsim.f90: tauscs = 1.25
      ! saettg   transfer_value_p(8)=
      ! saettb   transfer_value_p(9)=
      !#! transfer_value_p(10)=
      !------- depth averaged quantities
      do j = 1,number_trans_quant ! initialise
         write(trans_quant_name(j),'(18x)')
      end do
      trans_quant_name( 1) = "              bsbt"
      trans_quant_name( 2) = "            bsbctP" ! Phosphorfreisetzung orgc
      trans_quant_name( 3) = "               doN" ! Stickstofffreisetzung orgc
      trans_quant_name( 4) = "            BACmua" ! Ausgabekonzentration Summe Aufnahme+Respirationsrate heterotrophe Bakterien
      trans_quant_name( 5) = "        empty_five" !
      trans_quant_name( 6) = "             abszo" ! Absterberate Zooplankton
      trans_quant_name( 7) = "            dkimor" ! Absterberate Kieselalgen
      trans_quant_name( 8) = "            dgrmor" ! Absterberate Grünlalgen
      trans_quant_name( 9) = "            dblmor" ! Absterberate Blaualgen
      trans_quant_name(10) = "            BSBHNF" !
      trans_quant_name(11) = "            HNFBAC" !
      trans_quant_name(12) = "      empty_twelve" !
      trans_quant_name(13) = "            drfaek" !
      trans_quant_name(14) = "            drfaeg" !
      trans_quant_name(15) = "            drfaeb" !
      trans_quant_name(16) = "             zexki" !
      trans_quant_name(17) = "             zexgr" !
      trans_quant_name(18) = "             zexbl" !
      trans_quant_name(19) = "            dorgSS" !
      trans_quant_name(20) = "            dalgki" ! Zuwachs Kiesel-Algen
      trans_quant_name(21) = "            dalggr" ! Zuwachs Grün-Algen
      trans_quant_name(22) = "            dalgbl" ! Zuwachs Blau-Algen
      trans_quant_name(23) = "            dalgak" ! Respiration Kiesel-Algen
      trans_quant_name(24) = "            dalgag" ! Respiration Grün-Algen
      trans_quant_name(25) = "            dalgab" ! Respiration Blau-Algen
      trans_quant_name(26) = "              vco2" ! Konzentration Kohlendioxyd
      trans_quant_name(27) = "            dzres1" ! Grund-Respiration Konsumenten
      trans_quant_name(28) = "            dzres2" ! Fraßabhängige Respirationsrate Konsumenten
      trans_quant_name(29) = "              susn" ! Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE
      trans_quant_name(30) = "              PO2P" ! Sauerstoffproduktion durch Makrophyten in mgO2/(l*h)
      trans_quant_name(31) = "              PO2R" ! Sauerstoffverbrauch durch Makrophyten mgO2/(l*h)
      trans_quant_name(32) = "              go2n" ! FUER DIE STICKSTOFFOXYDATION VERBRAUCHTE SAUERSTOFFMENGE in mgO2/(l*h)
      trans_quant_name(33) = "            akinh4" ! Ammoniumaufnahme der kiesel-Algen, tiefengem.
      trans_quant_name(34) = "            agrnh4" ! Ammoniumaufnahme der gruen-Algen, tiefengem.
      trans_quant_name(35) = "            ablnh4" ! Ammoniumaufnahme der blau-Algen, tiefengem.
      trans_quant_name(36) = "            akino3" ! Nitrataufnahme der kiesel-Algen
      trans_quant_name(37) = "            agrno3" ! Nitrataufnahme der gruen-Algen
      trans_quant_name(38) = "            ablno3" ! Nitrataufnahme der blau-Algen
      trans_quant_name(39) = "             salgo" ! Summe Sauerstoffeintrag Algen
      trans_quant_name(40) = "             dalgo" ! Sauerstoffproduktion der Gruen-, Kiesel-, und Blaualgen
      trans_quant_name(41) = "            dalgao" ! Respiration (Sauerstoffverbrauch) der Gruen-, Kiesel-, und Blaualge
      trans_quant_name(42) = "                ir" ! Ingestionsrate der Rotatorien in mg/(l*h) | konsum()
      trans_quant_name(43) = "            zooro2" ! Sauerstoffverbrauch durch Zooplanktonrespiration
      trans_quant_name(44) = "            rO2HNF" ! Respiration HNF ???
      trans_quant_name(45) = "             SAETT" ! Sauerstoff Sättigungs-Konzentration in mgO2/l
      trans_quant_name(46) = "            FluxT1" ! Wärmefluss tiefenintegriert ??? wohl Rückgabewert
      trans_quant_name(47) = "             bsbct" ! mineralisierter Kohlenstoffgehalt in der Wassersäule | Rückgabewert
      trans_quant_name(48) = "            akitbr" !
      trans_quant_name(49) = "            agrtbr" !
      trans_quant_name(50) = "            abltbr" !
      trans_quant_name(51) = "             sgo2n" ! Aufsummierter Nitrifikationssauerstoffverbrauch, nur Ausgabe
      trans_quant_name(52) = "             susno" ! Sauerstoffverbr. d. Nitrifik. in der Wassersäule in mgO2/(l*h)
      trans_quant_name(53) = "            algzok" ! kiesel-Algen-Konsum Zoo-Plankton in mg/l
      trans_quant_name(54) = "           ???extk" ! mittlerer Extinktionskoeffizient
      trans_quant_name(55) = "              tpki" ! Ausgabeparameter algaeski()
      trans_quant_name(56) = "            akmuea" ! Ausgabeparameter algaeski()
      trans_quant_name(57) = "            ftaaus" ! Ausgabeparameter algaeski() fta
      trans_quant_name(58) = "             fiaus" ! Ausgabeparameter algaeski() Pmit/(Pmax*3.6)
      trans_quant_name(59) = "            fheaus" ! Ausgabeparameter algaeski() svhemk
      trans_quant_name(60) = "            akraus" ! Ausgabeparameter algaeski() F53
      trans_quant_name(61) = "              Dz2D" ! min. vertikalen Dispersionskoeffizient
      trans_quant_name(62) = "             templ" ! Lufttemperatur
      trans_quant_name(63) = "                RO" ! Luftfeuchte
      trans_quant_name(64) = "             SCHWI" ! Globalstrahlung
      trans_quant_name(65) = "               WGE" ! Windgeschwindigkeit
      trans_quant_name(66) = "             cloud" ! Bewölkungsdichte
      trans_quant_name(67) = "              typw" ! Wolkentyp
      trans_quant_name(68) = "  empty_sixtyeight" !
      trans_quant_name(69) = "          empty_69" !
      trans_quant_name(70) = "          empty_70" !
      trans_quant_name(71) = "          empty_71" !
      trans_quant_name(72) = "            algzog" ! gruen-Algen-Konsum Zoo-Plankton in mg/l
      trans_quant_name(73) = "            algzob" ! blau-Algen-Konsum Zoo-Plankton in mg/l
      trans_quant_name(74) = "              zHNF" ! Aufnahmerate der HNF
      trans_quant_name(75) = "             HNFza" ! HNFza(ior) = (zHNF(ior)/CHNF(ior))*24.
      trans_quant_name(76) = "             rmuas" !  = mueRot-respRg ! Nettowachstumsrate Rotatorien ?
      trans_quant_name(77) = "              rakr" !  = iras(ior)*respaR ! Fraßabhängige Respiration ?
      trans_quant_name(78) = "              rbar" !  = respRg ! GRund?-Respiration ?
      trans_quant_name(79) = "              iras" !  Ausgabe Ingestionsrate
      trans_quant_name(80) = "              tpgr" ! Ausgabeparameter algaesgr()
      trans_quant_name(81) = "              tpbl" ! Ausgabeparameter algaesbl()
      trans_quant_name(82) = "            figaus" ! Ausgabeparameter algaesgr() Pmit/(Pmax*3.6)
      trans_quant_name(83) = "            fibaus" ! Ausgabeparameter algaesbl() Pmit/(Pmax*3.6)
      trans_quant_name(84) = "            agmuea" ! Ausgabeparameter algaesgr()
      trans_quant_name(85) = "            abmuea" ! Ausgabeparameter algaesbl()
      trans_quant_name(86) = "            fhegas" ! Ausgabeparameter algaeski() svhemg
      trans_quant_name(87) = "            fhebas" ! Ausgabeparameter algaeski() svhemb
      trans_quant_name(88) = "            agreau" ! Ausgabe agbcm
      trans_quant_name(89) = "            abreau" ! Ausgabe von abbcm (Chlorophyll-a zu Kohlenstoff der blau-Algen) algaesbl()
      trans_quant_name(90) = "           dC_DenW" ! C-Abbau durch Denitrifikation in der Wassersäule
      trans_quant_name(91) = "              zBAC" ! Aufnahmerate der Bakterien
      trans_quant_name(92) = "          empty_92" !
      trans_quant_name(93) = "          empty_93" !
      trans_quant_name(94) = "          empty_94" !
      trans_quant_name(95) = "            drfaes" ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen
      trans_quant_name(96) = "             drHNF" ! Dreissena-Muscheln fressen HNF
      !trans_quant_name()= "              " !
      ! allocate transfer_quantity and initialise
      !!! allocate (transfer_quantity(number_trans_quant*number_trans_quant_points), stat = as )
      print*,"ini_ueber allocate (transfer_quantity part*proz_anz = ",part*proz_anz,part,proz_anz
      allocate (transfer_quantity(number_trans_quant*part*proz_anz), stat = as )
      if (as /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate transfer_quantity :', as
         call qerror(fehler)
      end if
      print*,meinrang,'allocate (transfer_quantity(    number_trans_quant,number_trans_quant_points = ' &
            ,number_trans_quant,number_trans_quant_points
      do i = 1,number_trans_quant_points ! alle knoten
         do j = 1,number_trans_quant ! initialisierung aller konzentrationen zunächt auf Null
            transfer_quantity(j+(i-1)*number_trans_quant) = 0.0 !!!####!0.0
         end do
      end do
      do j = 1,number_trans_quant ! default: no output
         output_trans_quant(j) = .false.
      end do
      !--------vertically distributed quantities
      do j = 1,number_trans_quant_vert ! initialise
         write(trans_quant_vert_name(j),'(18x)')
      end do
      trans_quant_vert_name( 1) = "            up_NKz"
      trans_quant_vert_name( 2) = "            up_NGz" ! N-Aufnahmerate der Algengruppe gruen
      trans_quant_vert_name( 3) = "            up_NBz" ! N-Aufnahmerate der Algengruppe blau
      trans_quant_vert_name( 4) = "            up_Siz" ! Si (Silizium) -Aufnahmerate der Kieselalgen
      trans_quant_vert_name( 5) = "            up_PKz" ! P (Phosphor) -Aufnahmerate der Algengruppen kiesel
      trans_quant_vert_name( 6) = "            up_PGz" ! P-Aufnahmerate der Algengruppen gruen
      trans_quant_vert_name( 7) = "            up_PBz" ! P-Aufnahmerate der Algengruppen blau
      trans_quant_vert_name( 8) = "            up_N2z" ! Aufnahmerate von Luftstickstoff durch Blaualgen
      trans_quant_vert_name( 9) = "            aknh4z" !
      trans_quant_vert_name(10) = "            agnh4z" !
      trans_quant_vert_name(11) = "            abnh4z" !
      trans_quant_vert_name(12) = "            dalgkz" !
      trans_quant_vert_name(13) = "            dalggz" !
      trans_quant_vert_name(14) = "            dalgbz" !
      trans_quant_vert_name(15) = "            akno3z" !
      trans_quant_vert_name(16) = "            agno3z" !
      trans_quant_vert_name(17) = "            abno3z" !
      trans_quant_vert_name(18) = "            algakz" ! Respirierte Algenbiomasse kiesel
      trans_quant_vert_name(19) = "            algagz" ! Respirierte Algenbiomasse grün
      trans_quant_vert_name(20) = "            algabz" ! Respirierte Algenbiomasse blau
      trans_quant_vert_name(21) = "               vz1" ! lokale Sauerstoffzehrung/produktion tiefenaufgelöst (tiefenprofil ausserhalb oxygen)
      trans_quant_vert_name(22) = "             dtemp" ! lokaler Wärmeeintrag tiefenaufgelöst (tiefenprofil ausserhalb temperw)
      trans_quant_vert_name(23) = "            akibrz" ! Wachstum? Kiesel-Algen-Biomasse
      trans_quant_vert_name(24) = "            agrbrz" ! Wachstum ? Grün-Algen-Biomasse
      trans_quant_vert_name(25) = "            ablbrz" ! Wachstum ? Blau-Algen-Biomasse
      trans_quant_vert_name(26) = "            algzkz" ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l
      trans_quant_vert_name(27) = "            algzgz" ! Grün-Algen-Konsum durch Zoo-Plankton in mg/l
      trans_quant_vert_name(28) = "            algzbz" ! Blau-Algen-Konsum durch Zoo-Plankton in mg/l
      allocate (trans_quant_vert(part*proz_anz*number_trans_quant_vert*num_lev_trans), stat = as )
      !allocate (trans_quant_vert(number_trans_quant*number_trans_quant_points*num_lev_trans), stat = as )
      if (as /= 0) then
         write(fehler,*)' allocate (trans_quant_vert failed :', as
         call qerror(fehler)
      end if
      do i = 1,number_trans_quant_points
         do j = 1,number_trans_quant_vert
            do k = 1,num_lev_trans
               trans_quant_vert(k+(j-1)*num_lev_trans+(i-1)*number_trans_quant_vert*num_lev_trans) = 0.0 !!!####!0.0 ! initialisierung aller konzentrationen zunächt auf Null
            end do ! alle k levels
         end do ! alle j quantities
         !do k=1,num_lev_trans
         !   trans_quant_vert(k+(1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)=7.7 ! up_NKz probehalber
         !end do ! alle k levels
      end do ! alle i Knoten
      do j = 1,number_trans_quant_vert ! default: no output
         output_trans_quant_vert(j) = .false.
      end do
   end if !! nur prozessor 0
end subroutine ini_ueber
!----+-----+----
!> \page globaleParameter Globale Modellparameter
!! Dies sind empirische Parameter, die als allgemeingültig für \n
!! <ul>
!! <li>das gesamte Modellgebiet und</li>
!! <li>den gesamten Berechnungs-Zeitraum</li>
!! </ul>
!! angesehen werden.
!! \n\n
!! <h2> Extinktionskoeffizienten </h2>
!! Die \ref extnct_rb sind in der Gerris-Benutzeroberfläche nicht bearbeitbar.\n
!! Die Datei <a href="./exp/e_extnct.dat" target="_blank">e_extnct.dat</a> ist Teil der QSim-Installation.
!!
!! <h2> Biologische Parameter </h2>
!! In der Gerris-Benutzeroberfläche werden sie unter der Schaltfläche "QSim-Parameter" geführt.\n
!! Diese globalen Biologischen Parameter werden von QSim aus der Datei APARAM.txt gelesen. Es handelt sich dabei um folgende:
!!<table >
!!<tr><th>Position in APARAM.txt </th><th>QSim-Name</th><th> Beschreibung </th><th> Einheit </th><th> Zitat aus AParamParam.xml </th></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 1 :</td><td>agchl,aggmax,IKge,agksn,agksp </td></tr>
!!<tr><td>  | </td><td> \anchor agchl agchl   </td><td> Kohlenstoff/Chlorophyll Gr▒nalgen (dunkeladaptiert) bei 20°C </td><td> mgC/mgChla </td><td>  </td></tr>
!!<tr><td>  | </td><td> \anchor aggmax aggmax   </td><td> Max. Wachstumsrate d. Grünalgen  </td><td> 1/d </td><td>   Format= F5.2  Null= -1  Help= maximale Produktionsrate für Grünalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor ikge ikge   </td><td> Lichtsättigung für Photosynthese der Grünalgen bei 20°C </td><td> µE/(m2*s) </td><td>   Format= F6.2  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor agksn agksn   </td><td> Halbsättigungskonstante Grünalgen N </td><td> mg/l </td><td>   Format= F5.3  Null= -1  Help= Halbsättigungskonstante für N bei Grünalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor agksp agksp   </td><td> Halbsättigungskonstante Grünalgen P </td><td> mg/l </td><td>   Format= F6.4  Null= -1  Help= Halbsättigungskonstante für P bei Grünalgen  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 2 :</td><td>agremi,frmuge,bsbgr,csbgr,Qmx_NG </td></tr>
!!<tr><td>  | </td><td> \anchor agremi agremi   </td><td> Grundrespiration d. Grünalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor frmuge frmuge   </td><td> Anteil der vom Wachstum abhängigigen Respiration (Grünalgen) </td><td>  - </td><td>   Format= F5.2  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor bsbgr bsbgr   </td><td> C-BSB5-Erhöhung Grünalgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= C-BSB5-Erhöhung durch Grünalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor csbgr csbgr   </td><td> CSB-Erhöhung Grünalgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= CSB-Erhöhung durch Grünalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_ng qmx_ng   </td><td> max. N-Gehalt der Grünalgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1  Help= Stickstoffgehalt der Grünalgenbiomasse  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 3 :</td><td>Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_pg qmx_pg   </td><td> max. P-Gehalt der Grünalgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1  Help= Phosphorgehalt der Grünalgenbiomasse  </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_ng qmn_ng   </td><td> min. N-Gehalt der Grünalgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_pg qmn_pg   </td><td> min. P-Gehalt der Grünalgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor upmxng upmxng   </td><td> max. N-Aufnahmerate der Grünalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1    </td></tr>
!!<tr><td>  | </td><td> \anchor upmxpg upmxpg   </td><td> max. P-Aufnahmerate der Grünalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 4 :</td><td>opgrmi,opgrma,asgre,ToptG,TmaxG</td></tr>
!!<tr><td>  | </td><td> \anchor opgrmi opgrmi   </td><td> Min. O2-Prod. Grünalgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= minimale Sauerstoffproduktion durch Grünalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor opgrma opgrma   </td><td> Max. O2-Prod. Grünalgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= maximale Sauerstoffproduktion durch Grünalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor asgre asgre   </td><td> Sediment Grünalgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Sedimentierbarer Anteil für Grünalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor toptg toptg   </td><td> optimal Temperatur für Grünalgenwachstum </td><td> °C </td><td>   Format= F5.2  Null= -1  Help= optimal Temperatur für Grünalgenwachstum   </td></tr>
!!<tr><td>  | </td><td> \anchor tmaxg tmaxg   </td><td> Letal-Temperatur für Grünalgenwachstum </td><td> °C </td><td>   Format= F5.2  Null= -1  Help= Letal-Temperatur für Grünalgenwachstum   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 5 :</td><td>akchl,akgmax,IKke,akksn,akksp </td></tr>
!!<tr><td>  | </td><td> \anchor akchl akchl   </td><td> Kohlenstoff/Chlorophyll Kieselalgen (dunkeladaptiert) bei 20°C </td><td> mgC/mgChla </td><td>    </td></tr>
!!<tr><td>  | </td><td> \anchor akgmax akgmax   </td><td> Max. Wachstumsate d. Kieselalgen  </td><td> 1/d </td><td>   Format= F5.2  Null= -1  Help= maximale Wachstumsrate für Kieselalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor ikke IKke   </td><td> Lichtsättigung für Photosynthese der Kieselalgen bei 20°C </td><td> µE/(m2*s) </td><td>   Format= F6.2  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor akksn akksn   </td><td> N-Halbsättigungskonstante Kieselalgen </td><td> mg/l </td><td>   Format= F5.3  Null= -1  Help= Halbsättigungskonstante für N bei Kieselalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor akksp akksp   </td><td> P-Halbsättigungskonstante Kieselalgen </td><td> mg/l </td><td>   Format= F6.4  Null= -1  Help= Halbsättigungskonstante für P bei Kieselalgen  </td></tr>
!!<tr><td bgcolor= "#888888" ></td>Zeile 6 :</td><td>akkssi,akremi,frmuke,bsbki,csbki </td></tr>
!!<tr><td>  | </td><td> \anchor akkssi akkssi   </td><td> Si-Halbsättigungskonstante Kieselalgen </td><td> mg/l </td><td>   Format= F5.3  Null= -1  Help= Halbsättigungskonstante für SI bei Kieselalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor akremi akremi   </td><td> Grundrespiration d. Kieselalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  Help= minimale Respirationsrate für Kieselalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor frmuke frmuke   </td><td> Anteil der vom Wachstum abhängigigen Respiration (Kieselalgen) </td><td>  - </td><td>   Format= F5.2  Null= -1  Help=   Default=  </td></tr>
!!<tr><td>  | </td><td> \anchor bsbki bsbki   </td><td> C-BSB5-Erhöhung Kieselalgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= C-BSB5-Erhöhung durch Kieselalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor csbki csbki   </td><td> CSB-Erhöhung Kieselalgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= CSB-Erhöhung durch Kieselalgen   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 7 :</td><td>Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_nk qmx_nk   </td><td> max. N-Gehalt der Kieselalgenzelle </td><td> mgN/mgBio </td><td>   Format= F7.5  Null= -1  Help= Stickstoffgehalt der Kieselalgenbiomasse   </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_pk qmx_pk   </td><td> max. P-Gehalt der Kieselalgenzelle </td><td> mgP/mgBio </td><td>   Format= F7.5  Null= -1  Help= Phosphorgehalt der Kieselalgenbiomasse  </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_sk qmx_sk   </td><td> max. Si-Gehalt der Kieselalgenzelle </td><td> mgSi/mgBio </td><td>   Format= F7.5  Null= -1  Help= Silikatgehalt der Kieselalgenbiomasse  </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_nk qmn_nk   </td><td> min. N-Gehalt der Kieselalgenzelle </td><td> mgN/mgBio </td><td>   Format= F7.5  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_pk qmn_pk   </td><td> min. P-Gehalt der Kieselalgenzelle </td><td> mgP/mgBio </td><td>   Format= F7.5  Null= -1   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 8 :</td><td>Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_sk qmn_sk   </td><td> min. Si-Gehalt der Kieselalgenzelle </td><td> mgSi/mgBio </td><td>   Format= F7.5  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor upmxnk upmxnk   </td><td> max. N-Aufnahmerate der Kieselalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor upmxpk upmxpk   </td><td> max. P-Aufnahmerate der Kieselalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor upmxsk upmxsk   </td><td> max. Si-Aufnahmerate der Kieselalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor opkimi opkimi   </td><td> Min. O2-Prod. Kieselalgen </td><td> mgO2/mgBio </td><td>   Format= F4.2  Null= -1  Help= minimale Sauerstoffproduktion durch Kieselalgen  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 9 :</td><td> opkima, askie, ToptK, kTemp_Ki, abchl </td></tr>
!!<tr><td>  | </td><td> \anchor opkima opkima   </td><td> Max. O2-Prod. Kieselalgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= maximale Sauerstoffproduktion durch Kieselalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor askie askie   </td><td> Sediment Kieselalgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Sedimentierbarer Anteil für Kieselalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor toptk toptk   </td><td> optimal Temperatur für Kieselalgenwachstum </td><td> °C </td><td> Cyclotella meneghiniana: 27.9°C  Default= 20  Min= 0  Max= 99.99 </td></tr>
!!<tr><td>  | </td><td> \anchor ktemp_ki ktemp_ki </td><td> empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent) </td><td> 1/°C </td><td> Format= F7.5  Null= -1
!! Help= Cyclotella meneghiniana: 0.003  Default= 0.0056  Min= 0  Max= 9.99999  Gruppe= Kieselalgen  Kategorie= Temperatur  </td></tr>
!!<tr><td>  | </td><td> \anchor abchl abchl   </td><td> "Kohlenstoff/Chlorophyll Blaualgen </td><td> mgC/mgChla </td><td>   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 10 :</td><td>abgmax,IKbe,abksn,abksp,abremi </td></tr>
!!<tr><td>  | </td><td> \anchor abgmax abgmax   </td><td> Max. Wachstumsrate d. Blaualgen  </td><td> 1/d </td><td>   Format= F5.2  Null= -1  Help= maximale Wachstumsrate für Blaualgen</td></tr>
!!<tr><td>  | </td><td> \anchor ikbe ikbe   </td><td> Lichtsättigung für Photosynthese der Blaualgen bei 20°C </td><td> µE/m2*s) </td><td>   Format= F6.2  Null= -1 </td></tr>
!!<tr><td>  | </td><td> \anchor abksn abksn   </td><td> N-Halbsättigung Blaualgen </td><td> mg/l </td><td>   Format= F5.3  Null= -1  Help= Halbsättigungskonstante für N bei Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor abksp abksp   </td><td> P-Halbsättigung Blaualgen </td><td> mg/l </td><td>   Format= F6.4  Null= -1  Help= Halbsättigungskonstante für P bei Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor abremi abremi   </td><td> Grundrespiration d. Blaualgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  Help= minimale Respirationsrate für Blaualgen  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 11 :</td><td>frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB </td></tr>
!!<tr><td>  | </td><td> \anchor frmube frmube   </td><td> Anteil der vom Wachstum abhängigigen Respiration (Blaulalgen) </td><td>  - </td><td>   Format= F5.2  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor bsbbl bsbbl   </td><td> C-BSB5-Erhöhung Blaualgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= C-BSB5-Erhöhung durch Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor csbbl csbbl   </td><td> CSB-Erhöhung Blaualgen </td><td> mg/mgCS </td><td>   Format= F6.4  Null= -1  Help= CSB-Erhöhung durch Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_nb qmx_nb   </td><td> max. N-Gehalt der Blaualgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1  Help= Stickstoffgehalt der Blaualgenbiomasse  </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_pb qmx_pb   </td><td> max. P-Gehalt der Blaualgenzelle </td><td> mgP/mgBio </td><td>   Format= F7.5  Null= -1  Help= Phosphorgehalt der Blaualgenbiomasse  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 12 :</td><td>Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_nb qmn_nb   </td><td> min. N-Gehalt der Blaualgenzelle </td><td> mgN/mgBio </td><td>   Format= F7.5  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_pb qmn_pb   </td><td> min. P-Gehalt der Blaualgenzelle </td><td> mgP/mgBio </td><td>   Format= F7.5  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor upmxnb upmxnb   </td><td> max. N-Aufnahmerate der Blaualgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor upmxpb upmxpb   </td><td> max. P-Aufnahmerate der Blaualgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor opblmi opblmi   </td><td> Min. O2-Prod. Blaualgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= minimale Sauerstoffproduktion durch Blaualgen  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 13 :</td><td>opblma,asble,ToptB,TmaxB,ifix</td></tr>
!!<tr><td>  | </td><td> \anchor opblma opblma   </td><td> Max. O2-Prod. Blaualgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= maximale Sauerstoffproduktion durch Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor asble asble   </td><td> Sediment Blaualgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Sedimentierbarer Anteil für Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor toptb toptb   </td><td> optimal Temperatur für Blaualgenwachstum </td><td> °C </td><td>   Format= F5.2  Null= -1  Help= optimal Temperatur für Blaualgenwachstum  </td></tr>
!!<tr><td>  | </td><td> \anchor tmaxb tmaxb   </td><td> Letal-Temperatur für Blaualgenwachstum </td><td> °C </td><td>   Format= F5.2  Null= -1  Help= Letal-Temperatur für Blaualgenwachstum  </td></tr>
!!<tr><td>  | </td><td> \anchor ifix ifix   </td><td> Luftstickstofffixierer (0/1) </td><td>  </td><td>   Format= I2  Null= -1  Help= Luftstickstofffixierer(0:Nein/1:Ja)  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 14 :</td><td>irmaxe,FopIRe,GROT,zresge,zakie </td></tr>
!!<tr><td>  | </td><td> \anchor irmaxe irmaxe   </td><td> max. Gewichtsspez. Algenaufnahmerate d. Rotatorien </td><td> µgC*µgC-2/3*d-1 ??? </td><td>   Format= F5.2  Null= -1  Help= Max. Ingestionsrate für Rotatorien  </td></tr>
!!<tr><td>  | </td><td> \anchor fopire fopire   </td><td> Halbsättigungskonstante für Futteraufnahme d. Rotatorien </td><td> mg/l </td><td>   Format= F5.2  Null= -1  Help= Optimale Futterkonzentration für Rotatorienwachstum  </td></tr>
!!<tr><td>  | </td><td> \anchor GROT GROT   </td><td> durchschnittliches Gewicht einer Rotatorie </td><td> µg </td><td>   Format= F5.2  Null= -1  Help= Gewicht einer Rotatorie  </td></tr>
!!<tr><td>  | </td><td> \anchor zresge zresge   </td><td> Grundrespiration Rotatorien </td><td> 1/d </td><td>   Format= F5.3  Null= -1  Help= Grundrespiration der Rotatorien  </td></tr>
!!<tr><td>  | </td><td> \anchor zakie zakie   </td><td> Filtrierbarkeit Kieselalgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Filtrierbarkeit der Kieselalgen durch Rotatorien  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 15 :</td><td>zagre,zable,ynmx1e,stks1e,anitrie </td></tr>
!!<tr><td>  | </td><td> \anchor zagre zagre   </td><td> Filtrierbarkeit Grünalgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Filtrierbarkeit der Grünalgen durch Rotatorien  </td></tr>
!!<tr><td>  | </td><td> \anchor zable zable   </td><td> Filtrierbarkeit Blaualgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Filtrierbarkeit der Blaualgen durch Rotatorien  </td></tr>
!!<tr><td>  | </td><td> \anchor ynmx1e ynmx1e (YNMAX1)   </td><td> Max. Wachstum Nitrosomonas </td><td> 1/d </td><td>   Format= F4.2  Null= -1  Help= Max. Wachstumsrate der Nitrosomonas  </td></tr>
!!<tr><td>  | </td><td> \anchor stks1e stks1e   </td><td> Halbsättigung Nitrosomonas </td><td> mgNH4-N/l </td><td>   Format= F5.2  Null= -1  Help= Halbsättigungskonstante für Nitrosomonas  </td></tr>
!!<tr><td>  | </td><td> \anchor anitrie anitrie   </td><td> Absterberate Nitrosomonas </td><td> 1/d </td><td>   Format= F4.2  Null= -1  Help= Absterberate für Nitrosomonas  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 16 :</td><td>bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e</td></tr>
!!<tr><td>  | </td><td> \anchor bnmx1e bnmx1e (BNMX1)   </td><td> Max. Umsatz Nitrosomonas </td><td> gNH4-N/(m²*l) </td><td>   Format= F5.2  Null= -1  Help= Max. Umsatzrate sessiler Nitrosomonas  </td></tr>
!!<tr><td>  | </td><td> \anchor bnks1e bnks1e   </td><td> Halbsätt. sessiler Nitrosomonas </td><td> mg/l </td><td>   Format= F5.2  Null= -1  Help= Halbsättigungskonstante der sessilen Nitrosomonas  </td></tr>
!!<tr><td>  | </td><td> \anchor ynmx2e ynmx2e (YNMAX2)   </td><td> Max. Wachstum Nitrobacter </td><td> 1/d </td><td>   Format= F4.2  Null= -1  Help= Max. Wachstumsrate der Nitrobacter  </td></tr>
!!<tr><td>  | </td><td> \anchor stks2e stks2e   </td><td> Halbsättigung Nitrobacter </td><td> mgNO2-N/l </td><td>   Format= F5.2  Null= -1  Help= Halbsättigungskonstante für Nitrobacter  </td></tr>
!!<tr><td>  | </td><td> \anchor anitri2e anitri2e</td><td> Absterberate Nitrobacter </td><td> 1/d </td><td>   Format= F4.2  Null= -1  Help= Absterberate für Nitrobacter  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 17 :</td><td>bnmx2e,bnks2e,KNH4e,KapN3e,hyPe </td></tr>
!!<tr><td>  | </td><td> \anchor bnmx2e bnmx2e (BNMX2)   </td><td> Max. Umsatz Nitrobacter </td><td> gNO2-N/(m2*l) </td><td>   Format= F5.2  Null= -1  Help= Max. Umsatzrate sessiler Nitrobacter  </td></tr>
!!<tr><td>  | </td><td> \anchor bnks2e bnks2e   </td><td> Halbsätt. sessiler Nitrobacter </td><td> mg/l </td><td>   Format= F5.2  Null= -1  Help= Halbsättigungskonstante der sessilen Nitrobacter  </td></tr>
!!<tr><td>  | </td><td> \anchor knh4e knh4e   </td><td> NH4-Umsatzgeschw. im Sediment </td><td> m/d </td><td>   Format= F5.2  Null= -1  Help= Saar: 0.28;  Havel: 0.19   </td></tr>
!!<tr><td>  | </td><td> \anchor kapn3e kapn3e   </td><td> Denitrifikationsgeschw. im Sediment </td><td> m/d </td><td>   Format= F5.2  Null= -1  Help= Saar: 0.06;  Havel: 0.15  </td></tr>
!!<tr><td>  | </td><td> \anchor hype hype   </td><td> Hydrolyserate für die leicht abbaubaren partikulären organischen C-Verbindungen  </td><td> 1/d </td><td>   Format= F6.3  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 18 :</td><td>hymxDe,KsD1e,KsD2e,KsMe,upBACe </td></tr>
!!<tr><td>  | </td><td> \anchor hymxde hymxde   </td><td> maximale Hydrolyserate für die leicht abbaubaren gelösten organischen C-Verbindungen  </td><td> 1/d </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor ksd1e ksd1e   </td><td> Halbsättigungskonstante für die Hydrolyse der leicht abbaubaren gelöster organischer C-Verbindungen </td><td> mgC/l </td><td>   Format= F6.3  Null= -1  H</td></tr>
!!<tr><td>  | </td><td> \anchor ksd2e ksd2e   </td><td> Halbsättigungskonstante für die Hydrolyse der schwer abbaubaren gelöster organischer C-Verbindungen </td><td> mgC/l </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor ksme ksme   </td><td> Halbsättigungskonst. für den Abbau monomerer C-Verbindungen (?? für die Aufnahme von Kohlenstoff durch heterotrophen Bakterien??) </td><td> mgC/l </td><td>
!!   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor upbace upbace   </td><td> max. Aufnahmerate monomerer C-Verbindungen d. Bakterien </td><td> 1/d </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 19 :</td><td>YBACe,rsGBACe,FoptDe,upHNFe,BACkse </td></tr>
!!<tr><td>  | </td><td> \anchor ybace ybace   </td><td> Ertragskoeffizient für Bakterienbiomasse </td><td>  - </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor rsgbace rsgbace   </td><td> Grundrespiration het. Bakterien </td><td> 1/d </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor foptde foptde   </td><td> Opt. Futterkonz. Dreissena </td><td> mgC/l </td><td>   Format= F5.2  Null= -1  Help= Optimale Futterkonzentration für Dreissena-Wachstum  </td></tr>
!!<tr><td>  | </td><td> \anchor uphnfe uphnfe   </td><td> max. Aufnahmerate der HNF </td><td> 1/d </td><td>   Format= F5.2  Null= -1  Help= Aufnahmerate heterotropher Nanoflagelaten  </td></tr>
!!<tr><td>  | </td><td> \anchor backse backse   </td><td> Halbsättigungsk. für BaK.-Aufnahme durch HNF </td><td> mgC/l </td><td>   Format= F6.4  Null= -1  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 20 :</td><td>alamda,fPOC1e,fPOC2e,SorpCape,Klange</td></tr>
!!<tr><td>  | </td><td> \anchor alamda alamda   </td><td> Absorptionskoeff. für Gelbstoffe bei 440 nm </td><td> - </td><td>   Format= F5.3  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor fpoc1e fpoc1e   </td><td> leichtabbaubarer Anteil d. Sedimentkohlenstoffs </td><td>  -  </td><td>   Format= F5.2  Null=   Help= (Literaturwert: 0.65  </td></tr>
!!<tr><td>  | </td><td> \anchor fpoc2e fpoc2e   </td><td> schwerabbaubarer Anteil d. Sedimentkohlenstoffs </td><td>  -  </td><td>   Format= F5.2  Null=   Help= (Literaturwert: 0.15  </td></tr>
!!<tr><td>  | </td><td> \anchor sorpcape sorpcape</td><td> SorptionsKapazität für Phosphor </td><td> mgP/gTG </td><td>   Format= F6.2  Null= -1  Help= (Literaturwerte: Maxmalwert: 2.5; Eingabe: -1 -> Wert wird berechnet  </td></tr>
!!<tr><td>  | </td><td> \anchor klange klange   </td><td> Langmuirkoeffizient für Phosphorsorption </td><td> l/mgP </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 21 :</td><td>KdNh3e,RateCde,etaCde,RateCIe,xnueCe</td></tr>
!!<tr><td>  | </td><td> \anchor kdnh3e kdnh3e   </td><td> Partitionskoeffizient für Ammonium </td><td> l/kg </td><td>   Format= F5.2  Null= -1  Help= -1.-> Wert wird berechnet  </td></tr>
!!<tr><td>  | </td><td> \anchor ratecde   ratecde   </td><td>Grundmortalitätsrate coliformer Bakterien bei 20°C </td><td> 1/d </td><td>  Help="Grundmortalitätsrate coliformer Bakterien bei 20°C" Min="0.0" Max="10." Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td>  | </td><td> \anchor etacde   etacde   </td><td>Temperaturkoeffizient </td><td> - </td><td>  Help="Temperaturkoeffizient" Min="1." Max="3." Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td>  | </td><td> \anchor ratecie   ratecie   </td><td>Inaktivierungskoeffizient im Licht </td><td> m2*MJ-1 </td><td>  Help="Inaktivierungskoeffizient im Licht" Min="0.0" Max="99.99" Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td>  | </td><td> \anchor xnuece   xnuece   </td><td>dimensionsloser Parameter </td><td> - </td><td>  Help="dimensionsloser Parameter zur Beschreibung der Inaktiv. im Licht" Min="1.0" Max="999.99" Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 22 :</td><td>RateCGe,RateCSe</td></tr>
!!<tr><td>  | </td><td> \anchor ratecge   ratecge   </td><td>Verlustrate durch Grazing </td><td> d-1 </td><td> Help="Coliforme Verlustrate durch Grazing" Min="0.0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td>  | </td><td> \anchor ratecse   ratecse   </td><td>Verlustrate durch Sedimentation </td><td> d-1 </td><td>  Help="Coliforme Verlustrate durch Sedimentation" Min="0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!</table>
!!\n\n
!! Beispiel:\n
!! <a href="./exp/APARAM_200314.txt" target="_blank">APARAM.txt, Version vom 20.03.2014</a>\n
!! <a href="./exp/AParam_kommentiert.txt" target="_blank">APARAM.txt (kommentiert) v12.40</a>\n
!! <a href="./exp/aparam_gerris.xls" target="_blank">Parameterliste Volker 19dez12</a>\n
!! <a href="./exp/aparam_gerris_13.10.xls" target="_blank">überarbeitete Prameterliste 20dez12 wy</a>\n\n
!! die aktuelle <a href="./exp/AParamParam.xml" target="_blank"> AParamParam.xml </a> enthält die Parameterdefinitionen im
!! xml-Format. Diese Datei dient der Synchronisation mit der Benutzeroberfläche Gerris \ref Gerris .\n\n
!!\n\n
!! <a href="./pdf/Schnittstelle_QSIM.pdf" target="_blank">Schnittstellenbeschreibung Gerris-QSim</a>\n
!!
!! \n\n zurück: \ref Modellerstellung  Quelle: uebergabe_werte.f95
!----+-----+----
!> <h1>broadcast_parameter() alle prozessoren bekommen die Parameter aus aparam.txt</h1>
!! nicht mehr über transfer_parameter_p sondern direkt aus module_QSimDatenfelder.f95
!! aus qsim13.3:
!! <code>\verbatim
!!       read(55,5500,iostat=read_error)agchl,aggmax,IKge,agksn,agksp
!!       read(55,5502,iostat=read_error)agremi,frmuge,bsbgr,csbgr,Qmx_NG
!!       read(55,5504,iostat=read_error)Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG
!!       read(55,5506,iostat=read_error)opgrmi,opgrma,asgre,ToptG,kTemp_Gr
!!       read(55,5507,iostat=read_error)akchl,akgmax,IKke,akksn,akksp
!!       read(55,5508,iostat=read_error)akkssi,akremi,frmuke,bsbki,csbki
!!       read(55,5510,iostat=read_error)Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK
!!       read(55,5512,iostat=read_error)Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi
!!       read(55,5514,iostat=read_error)opkima,askie,ToptK,kTemp_Ki,abchl
!!       read(55,5516,iostat=read_error)abgmax,IKbe,abksn,abksp,abremi
!!       read(55,5518,iostat=read_error)frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB
!!       read(55,5520,iostat=read_error)Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi
!!       read(55,5522,iostat=read_error)opblma,asble,ToptB,kTemp_Bl,ifix
!!       read(55,5524,iostat=read_error)irmaxe,FopIRe,GROT,zresge,zakie
!!       read(55,5526,iostat=read_error)zagre,zable,ynmx1e,stks1e,anitrie
!!       read(55,5530,iostat=read_error)bnmx1e,bnks1e,ynmx2e,stks2e,anitrie
!!       read(55,5528,iostat=read_error)bnmx2e,bnks2e,KNH4e,KapN3e,hyPe
!!       read(55,5533,iostat=read_error)hymxDe,KsD1e,KsD2e,KsMe,upBACe
!!       read(55,5535,iostat=read_error)YBACe,rsGBACe,FoptDe,upHNFe,BACkse
!!       read(55,5538,iostat=read_error)alamda,fPOC1e,fPOC2e,SorpCape,Klange
!!       read(55,5540,iostat=read_error)KdNh3e
!! \endverbatim</code>
!! aus module_uebergabe_werte.f95
subroutine broadcast_parameter()
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   !----------------------------------------------------------------- APARAM.txt
   call MPI_Bcast(agchl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(aggmax,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(IKge,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(agksn,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(agksp,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(agremi,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(frmuge,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(bsbgr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(csbgr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmx_NG ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmx_PG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmn_NG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmn_PG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(upmxNG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(upmxPG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   
   call MPI_Bcast(opgrmi,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(opgrma,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(asgre,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ToptG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(kTemp_Gr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(akchl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(akgmax,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(IKke,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(akksn,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(akksp ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(akkssi,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(akremi,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(frmuke,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(bsbki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(csbki ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmx_NK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmx_PK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmx_SK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmn_NK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmn_PK ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmn_SK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(upmxNK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(upmxPK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(upmxSK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(opkimi ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(opkima,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(askie,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ToptK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(kTemp_Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(abchl ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(abgmax,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(IKbe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(abksn,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(abksp,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(abremi ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(frmube,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(bsbbl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(csbbl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmx_NB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmx_PB ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmn_NB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Qmn_PB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(upmxNB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(upmxPB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(opblmi ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(opblma,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(asble,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ToptB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(kTemp_Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ifix,1,MPI_INT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(IRMAX,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(FOPTR,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(GROT,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ZRESG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ZAKI ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ZAGR,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ZABL,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(YNMAX1,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(STKS1,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ANITR1 ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(BNMX1,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(BNKS1,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(YNMAX2,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(STKS2,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(ANITR2,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(BNMX2,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(BNKS2,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(KNH4,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(KapN3,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(HyP1 ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(hymxD,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(KsD1,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(KsD2,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(KsM,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(upBAC ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(YBAC,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(rsGBAC,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(FoptD,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(upHNF,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(BACks ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(alamda,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(fPOC1,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(fPOC2,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(SorpCap,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Klang,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(KdNh3,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ratecd,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(etacd,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(rateci,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(xnuec,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ratecg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ratecs,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   !-----------------------------------------------------------------weitere (ini_algae)
   call MPI_Bcast(Cagr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Caki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(Cabl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(CZoo,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a1Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a2Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a3Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a1Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a2Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a3Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a1Gr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a2Gr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a3Gr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   !-----------------------------------------------------------------weitere (orgc_start)
   call MPI_Bcast(TOC_CSB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(bsbZoo,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   !-----------------------------------------------------------------weitere (naehr2_start)
   call MPI_Bcast(nZoo,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(pZoo,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   return
end subroutine broadcast_parameter
