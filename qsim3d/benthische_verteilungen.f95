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
!----+-----+----
!> Verteilen der benthischen verteilungen auf die parallelen Prozesse.
!! \n\n
subroutine benthic_parallel()
   use modell
   implicit none
   integer :: i,j,as
   !print*,meinrang,'benthic_parallel'
   call MPI_Bcast(number_benthic_points,1,MPI_INT,0,mpi_komm_welt,ierr)
   allocate (benthic_distribution_p(number_benth_distr*part), stat = as )
   
   if (as /= 0) then
      write(fehler,*)' Rueckgabewert von benthic_distribution_p :', as
      call qerror(fehler)
   end if
   
   do i = 1,part ! i
      do j = 1,number_benth_distr ! initialisierung aller konzentrationen zunächt auf 0.0 minus 1
         benthic_distribution_p(j+(i-1)*number_benth_distr) = 0.0 !!!####!-1.0
      end do
   end do
   ! if(meinrang.eq.0)then ! prozess 0 only
   !    do i=1,number_benthic_points ! all i verticals
   !       do j=1,number_benth_distr ! initialise
   !          benthic_distribution(j+(i-1)*number_benth_distr)=i*100+j
   !       end do
   !    end do
   !  end if ! only prozessor 0
   call scatter_benthic()
   ! call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine benthic_parallel
!----+-----+----
!> Verteilen der benthischen verteilungen auf die parallelen Prozesse.
!! \n\n
subroutine scatter_benthic()
   use modell
   implicit none
   !print*,'scatter_benthic'
   
   call MPI_Scatter(benthic_distribution, part*number_benth_distr, MPI_FLOAT,  &
                    benthic_distribution_p, part*number_benth_distr, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 13 MPI_Scatter(benthic_distribution failed :', ierr
      call qerror(fehler)
   end if
   !call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine scatter_benthic
!----+-----+----
!> wieder zusammensammeln der benthischen verteilungen von den parallelen Prozesse.
!! \n\n
subroutine gather_benthic()
   use modell
   implicit none
   !print*,'gather_benthic'
   
   call MPI_Gather(benthic_distribution_p, part*number_benth_distr, MPI_FLOAT,  &
                   benthic_distribution, part*number_benth_distr, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 13b MPI_Gather(benthic_distribution_p failed :', ierr
      call qerror(fehler)
   end if
   
   !call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine gather_benthic
!----+-----+----
!> Initialisierung der lokalen Konzentrationen.
!! \n\n
subroutine ini_benthic0(nk)
   use modell
   implicit none
   integer nk,j,i, as
   if (meinrang == 0) then ! prozess 0 only
      number_benthic_points = nk
      do j = 1,number_benth_distr ! initialise
         write(benth_distr_name(j),'(18x)')
      end do
      benth_distr_name( 1) = "              tsed"
      benth_distr_name( 2) = "             sised"
      benth_distr_name( 3) = "               pfl"
      benth_distr_name( 4) = "              ssdr"
      benth_distr_name( 5) = "            Ks_rau"
      !!! ( 5)= "Nikuradse Sandrauheit in m, Sohlreibungsbeiwert (Rauheit)"
      benth_distr_name( 6) = "            orgCsd" ! Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert
      benth_distr_name( 7) = "            bsbbet" ! Ausgabekonzentration Sauerstoffverbrauch durch Organismen auf Makrophyten
      benth_distr_name( 8) = "              hJO2" ! Sauerstoffzehrung des Sediments gO2/m² und Zeitschritt
      benth_distr_name( 9) = "            cmatki" ! Abspülung benthischer kiesel-Algen
      benth_distr_name(10) = "            cmatgr" ! Abspülung benthischer gruen-Algen
      benth_distr_name(11) = "            alberg" ! Respiration benthischer gruen-Algen
      benth_distr_name(12) = "            alberk" ! Respiration benthischer kiesel-Algen
      benth_distr_name(13) = "            albewg" ! Wachstum benthischer gruen-Algen
      benth_distr_name(14) = "            albewk" ! Wachstum benthischer kiesel-Algen
      benth_distr_name(15) = "             resdr" ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
      benth_distr_name(16) = "            hschlr" ! Sauerstoffzehrung durch das Sediments, Ausgabe in mgO2/(l*h)
      benth_distr_name(17) = "            so2ein" ! potentieller Sauerstoffeintrag aus der Luft (Ausgabe) [mgO2/l/h]
      benth_distr_name(18) = "            dO2o2D" ! Beiwert Oberflächenbelüftung ? (oxygen) war bbei2D
      benth_distr_name(19) = "            o2ein1" ! Sauerstoffeintrag aus der Luft (Ausgabe)
      benth_distr_name(20) = "            abeowg" ! Sauerstoffproduktion benthischer Grünalgen
      benth_distr_name(21) = "            abeorg" ! Sauerstoffverbrauch benthischer Grünalgen
      benth_distr_name(22) = "            abeowk" ! Sauerstoffproduktion benthischer Kieselalgen
      benth_distr_name(23) = "            abeork" ! Sauerstoffverbrauch benthischer Kieselalgen
      benth_distr_name(23) = "            abeork" ! Sauerstoffverbrauch benthischer Kieselalgen
      benth_distr_name(24) = "             ro2dr" ! Respiration Dreissena-Muscheln pro Zeitschritt in mgO2/l je Zeitschritt
      benth_distr_name(25) = "            SiRuek" ! Rückgelöste Menge Silikat-Silizium
      benth_distr_name(26) = "            sedalk" ! Sedimentierte Menge an Kiesel-Algen
      benth_distr_name(27) = "            sedalg" ! Sedimentierte Menge an Grün-Algen
      benth_distr_name(28) = "            sedalb" ! Sedimentierte Menge an Blau-Algen
      benth_distr_name(29) = "            exdrvk" ! exkretierte Ammoniummenge der Muscheln beim Verzehr von Kiesel-Algen
      benth_distr_name(30) = "            exdrvg" ! exkretierte Ammoniummenge der Muscheln beim Verzehr von Grün-Algen
      benth_distr_name(31) = "            exdrvb" ! exkretierte Ammoniummenge der Muscheln beim Verzehr von Blau-Algen
      benth_distr_name(32) = "             hJPO4" ! Phosphat-Freisetzung aus dem Sediment
      benth_distr_name(33) = "             sedx0" ! sedimentierte Nitrosomonasbiomasse in µg/l, nur Ausgabewert
      benth_distr_name(34) = "             bettn" ! OXYDIERTE STICKSTOFFMENGE AM GEWAESSERBETT | AmmoniumFlux Wasser/Sediment in mgN/(l*h)
      benth_distr_name(35) = "             hJNO3" ! Nitrat-Freisetzung aus dem Sediment
      benth_distr_name(36) = "             hJNH4" ! Ammonium-Freisetzung aus dem Sediment
      benth_distr_name(37) = "            hFluN3" ! Ausgabe NitratFlux Wasser/Sediment in mgN/(l*h)
      benth_distr_name(38) = "            algdrk" ! \ref Algen-Konsum-bentisch (Muscheln) in mg/l
      benth_distr_name(39) = "            algcok" ! Kiesel-Algen Konsum durch Corophium ?
      benth_distr_name(40) = "            algdrg" ! \ref grün-Algen-Konsum-bentisch (Muscheln) in mg/l
      benth_distr_name(41) = "            algdrb" ! \ref blau-Algen-Konsum-bentisch (Muscheln) in mg/l
      benth_distr_name(42) = "            algcog" ! grün-Algen Konsum durch Corophium ?
      benth_distr_name(43) = "            algcob" ! blau-Algen Konsum durch Corophium ?
      benth_distr_name(44) = "               Kst" ! Strickler-Beiwert aus Nikuradse Sandrauheit umgerechnet
      benth_distr_name(45) = "              utau" ! Sohlschubspannung
      benth_distr_name(46) = "              hJSi" ! Silizium-Flux aus dem Sediment
      benth_distr_name(47) = "              hJN2" !
      benth_distr_name(48) = "             JDOC1" ! flux dissolved organic carbon aus dem sediment
      benth_distr_name(49) = "             JDOC2" ! flux dissolved organic carbon aus dem sediment
      benth_distr_name(50) = "           orgCsd0" ! teil des? sedimentierten organ. Material
      benth_distr_name(51) = "        orgCsd_abb" ! sedimentiertes biologisch abbaubares organ. Material
      benth_distr_name(52) = "         sedAlg_MQ" !
      benth_distr_name(53) = "           sedAlk0" !
      benth_distr_name(54) = "             coroi" ! Corophium Böschung
      benth_distr_name(55) = "            corois" ! Corophium Sohle
      benth_distr_name(56) = "           zdreis0" ! Dreissenabiomasse pro Fläche Sohle (0. Kohorte)
      benth_distr_name(57) = "           zdreis1" ! (1. Kohorte)
      benth_distr_name(58) = "           zdreis2" ! (2. Kohorte)
      benth_distr_name(59) = "           zdreis3" ! (3. Kohorte)
      benth_distr_name(60) = "            zdrei0" ! Dreissenabiomasse pro Fläche Böschung (0. Kohorte)
      benth_distr_name(61) = "            zdrei1" ! (1. Kohorte)
      benth_distr_name(62) = "            zdrei2" ! (2. Kohorte)
      benth_distr_name(63) = "            zdrei3" ! (3. Kohorte)
      benth_distr_name(64) = "            gewdr0" ! Gewicht einer Dreissena-Muschel (0. Kohorte)
      benth_distr_name(65) = "            gewdr1" ! (1. Kohorte)
      benth_distr_name(66) = "            gewdr2" ! (2. Kohorte)
      benth_distr_name(67) = "            gewdr3" ! (3. Kohorte)
      benth_distr_name(68) = "             dlmax" !  Dreissena Larven ??
      benth_distr_name(69) = "            dlmaxs" !  Dreissena Larven ??
      benth_distr_name(70) = "            gwdmax" !  Dreissena Larven ??
      benth_distr_name(71) = "            sgwmue" !  Dreissena Larven ??
      benth_distr_name(72) = "            abegm2" !  Ausgabewerte albenth()
      benth_distr_name(73) = "            abekm2" !  Ausgabewerte albenth()
      !benth_distr_name(44)= "            uedau0" ! Überstaudauer 0-15
      !benth_distr_name(45)= "           uedau15" ! Überstaudauer 15-25
      !benth_distr_name(46)= "           uedau25" ! Überstaudauer 25-35
      !benth_distr_name(47)= "           uedau35" ! Überstaudauer 35-unendl.
      !benth_distr_name()= "            " !
      !allocate (benthic_distribution(number_benth_distr*number_benthic_points), stat = as )
      allocate (benthic_distribution(number_benth_distr*part*proz_anz), stat = as )
      if (as /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate benthic_distribution :', as
         call qerror(fehler)
      end if
      do i = 1,number_benthic_points ! all i verticals
         do j = 1,number_benth_distr ! initialise
            benthic_distribution(j+(i-1)*number_benth_distr) = 0.0 !!!####!0.0
         end do
      end do
      do j = 1,number_benth_distr ! default no output
         output_benth_distr(j) = .false.
      end do
      ! vorbelegen
      !do i=1,number_benthic_points !
      !benthic_distribution(1,k)=  4.0 !! Sediment-Temperatur Elbe-Ästuar Jahresanfang 2006
      !benthic_distribution(2,k)=  7.0 !! Siliziumgehalt im Sediment
      !benthic_distribution(5+(i-1)*number_benth_distr)= 70.0 !! Strickler Reibungsbeiwert Kst_rau (Mannings n, here: Kst=1/n)
      !benthic_distribution(11+(i-1)*number_benth_distr)= tief(i) !! water depth
      !end do
      !print*, '### ACHTUNG ### Strickler Reibungsbeiwert Kst_rau wird noch nicht eingelesen sondern hilfsweise 70 gesetzt ????'
      
   end if ! only prozessor 0
end subroutine ini_benthic0
!----+-----+----
