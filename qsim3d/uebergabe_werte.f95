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

!> Verteilen der Datenstrukturen auf die parallelen Prozesse
subroutine ueber_parallel()
   use modell
   use QSimDatenfelder
   use module_aparam
   implicit none
   integer :: allostat

   call MPI_Bcast(number_trans_quant_points,1,MPI_INT,0,mpi_komm_welt,ierr)
   call broadcast_parameter()
   allocate (transfer_quantity_p(number_trans_quant*part), stat = allostat)
   if (allostat /= 0) then
      write(fehler, "(a,i0)") 'allocate(transfer_quantity_p()) returned: ', allostat
      call qerror(fehler)
   endif
   allocate (trans_quant_vert_p(number_trans_quant_vert*part*num_lev_trans), stat = allostat)
   if (allostat /= 0) then
      write(fehler,"(a,i0)")' allocate (trans_quant_vert_p failed :', allostat
      call qerror(fehler)
   endif
   call scatter_ueber()
   return
end subroutine ueber_parallel

!----+-----+----
!> Verteilen der Datenstrukturen auf die parallelen Prozesse
subroutine scatter_ueber()
   use modell
   implicit none

   call MPI_Bcast(transfer_value_p,number_trans_val,MPI_FLOAT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) then
      write(fehler,"(a,i0)")' MPI_Bcast(transfer_value_p failed :',ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Scatter(transfer_quantity, part*number_trans_quant, MPI_FLOAT,  &
                    transfer_quantity_p, part*number_trans_quant, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,"(a,i0)")' MPI_Scatter(transfer_quantity failed :',ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Scatter(trans_quant_vert, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT,  &
                    trans_quant_vert_p, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,"(a,i0)")' MPI_Scatter(trans_quant_vert failed :', ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine scatter_ueber

!----+-----+----
!> wieder-einsammeln der Datenstrukturen von den parallelen Prozesse
subroutine gather_ueber()
   use modell
   implicit none
   
   integer :: i
   
   call MPI_Gather(transfer_quantity_p, part*number_trans_quant, MPI_FLOAT,  &
                   transfer_quantity, part*number_trans_quant, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,"(a,i0)")' MPI_Gather(transfer_quantity failed :', ierr
      call qerror(fehler)
   endif
   
   call MPI_Gather(trans_quant_vert_p, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT,  &
                   trans_quant_vert, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT, 0,mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,"(a,i0)") ' MPI_Gather(trans_quant_vert failed :', ierr
      call qerror(fehler)
   endif
   return
end subroutine gather_ueber

!----+-----+----
!> Initialisierung der nicht-transportierten Übergabe-Konzentrationen und Werte.
subroutine ini_ueber(nk)
   use modell
   implicit none
   
   integer, intent(in) :: nk
   integer :: i, n, as, j, l, k
   
   if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten
   
      ! Übergabe Konzentrationen
      number_trans_quant_points = nk
      if (num_lev_trans /= num_lev) call qerror("`num_lev_trans` does not equal `num_lev`")
      
      
      ! --- single (global) transfer values ---
      ! initialize
      do j = 1,number_trans_val
         trans_val_name(j)   = "                  "
         transfer_value_p(j) = 0.0
         output_trans_val(j) = .false.
      enddo
      
      ! names
      trans_val_name( 1) = "          empty_01"
      trans_val_name( 2) = "          empty_02"
      trans_val_name( 3) = "          empty_02"
      trans_val_name( 4) = "          empty_03"
      trans_val_name( 5) = "          empty_04"
      trans_val_name( 6) = "            Saettk"
      trans_val_name( 7) = "            tauscs" ! Schiffseinfluss     qsim.f90: tauscs = 1.25
      trans_val_name( 8) = "            saettg"
      trans_val_name( 9) = "            saettb"
      trans_val_name(10) = "              it_h" ! Anzahl der Zeitschritte während der Hellphase
      
      trans_val_name = adjustl(trans_val_name)
      
      ! values
      transfer_value_p(7) = 1.25 ! Schiffseinfluss  [qsim.f90: tauscs = 1.25]
      
      
      ! --- depth averaged quantities ---
      do j = 1,number_trans_quant ! initialise
         trans_quant_name(j) = "                  "
      enddo
      
      trans_quant_name( 1) = "              bsbt"
      trans_quant_name( 2) = "            bsbctP" ! Phosphorfreisetzung orgc
      trans_quant_name( 3) = "               doN" ! Stickstofffreisetzung orgc
      trans_quant_name( 4) = "            BACmua" ! Ausgabekonzentration Summe Aufnahme+Respirationsrate heterotrophe Bakterien
      trans_quant_name( 5) = "          empty_05" !
      trans_quant_name( 6) = "             abszo" ! Absterberate Zooplankton
      trans_quant_name( 7) = "            dkimor" ! Absterberate Kieselalgen
      trans_quant_name( 8) = "            dgrmor" ! Absterberate Grünlalgen
      trans_quant_name( 9) = "            dblmor" ! Absterberate Blaualgen
      trans_quant_name(10) = "            BSBHNF" !
      trans_quant_name(11) = "            HNFBAC" !
      trans_quant_name(12) = "            SSeros" ! amount of resuspended matter
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
      trans_quant_name(45) = "          empty_45" ! 
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
      trans_quant_name(67) = "              wtyp" ! Cloud reflectane(?) derived from cloud type
      trans_quant_name(68) = "          empty_68" !
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
      trans_quant_name(79) = "              iras" ! Ausgabe Ingestionsrate
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
      trans_quant_name(97) = "             susn2" ! nitrite, which is oxidised to nitrate by nitrifiers
      trans_quant_name(98) = "             pfln1" ! ammonium, which is oxidised to nitrite by nitrifiers (on macrophytes)
      trans_quant_name(99) = "             pfln2" ! nitrite, which is oxidised to nitrate by nitrifiers(on macrophytes)
      
      trans_quant_name = adjustl(trans_quant_name)
      
      ! allocate transfer_quantity and initialise
      ! all concentrations are initialized as 0.0
      allocate (transfer_quantity(number_trans_quant*part*proz_anz), source = 0.0,  stat = as )
      if (as /= 0) then
         write(fehler,"(a,i0)") 'allocate transfer_quantity returned:', as
         call qerror(fehler)
      endif
      
      ! Initailize as no output
      do j = 1,number_trans_quant
         output_trans_quant(j) = .false.
      enddo
      
      !--------vertically distributed quantities
      do j = 1,number_trans_quant_vert ! initialise
         write(trans_quant_vert_name(j),'(18x)')
      enddo
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
      
      trans_quant_vert_name = adjustl(trans_quant_vert_name)
      
      allocate(trans_quant_vert(part*proz_anz*number_trans_quant_vert*num_lev_trans), source = 0.0, stat = as)
      
      if (as /= 0) then
         write(fehler,*)' allocate (trans_quant_vert failed :', as
         call qerror(fehler)
      endif
      
      do j = 1,number_trans_quant_vert 
         ! default: no output
         output_trans_quant_vert(j) = .false.
      enddo
   endif
   
   ! make names available to all processes (e.g. for logging)
   call mpi_barrier(mpi_komm_welt, ierr)
   
   do k = 1,number_trans_val
      call mpi_bcast(trans_val_name(k)       , len(trans_val_name(k))       , MPI_CHAR, 0, mpi_komm_welt, ierr)
   enddo
   
   do k = 1,number_trans_quant
      call mpi_bcast(trans_quant_name(k)     , len(trans_quant_name(k))     , MPI_CHAR, 0, mpi_komm_welt, ierr)
   enddo
   
   do k = 1,number_trans_quant_vert
      call mpi_bcast(trans_quant_vert_name(k), len(trans_quant_vert_name(k)), MPI_CHAR, 0, mpi_komm_welt, ierr)
   enddo
   
   call mpi_barrier(mpi_komm_welt, ierr)
   
end subroutine ini_ueber
!----+-----+----

subroutine broadcast_parameter()
   use modell
   use QSimDatenfelder
   use module_aparam
   implicit none
   
   ! APARAM.txt
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
   
   ! weitere (ini_algae)
   call MPI_Bcast(a1Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a1Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(a1Gr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !

   !  weitere (orgc_start)
   call MPI_Bcast(TOC_CSB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   call MPI_Bcast(bsbZoo,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
   
   return
end subroutine broadcast_parameter
