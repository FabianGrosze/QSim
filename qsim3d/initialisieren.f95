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

subroutine initialisieren()

   use modell
   use QSimDatenfelder
   use module_aparam
   
   implicit none
   
   integer :: i, j, k, nini, nuzo, nt, irn
   logical :: vorhanden, einmal
   
   if (meinrang == 0) then 
      print "(a)",   repeat("-", 80) 
      print "(a)",   "initializing zones"
      print "(a)",   repeat("-", 80) 
      print "(a,i0)",'rechenzeit = ', rechenzeit
      
      ! Initialisierung aller transportierten planktischen Variablen (Konzentrationen) zunächt auf Null
      planktonic_variable = 0.0
      
      !> Initilisieren der Zone mit der zugeordneten Randbedingung ini_randnr aus MODELLG.3D.txt
      !! Dazu ein <a href="./exp/MODELLG.3D.txt" target="_blank">Beispiel</a>.
      !! Initialisierungs-Randnummer der Zone, ini_randnr, in Randzähler umwandeln...
      do i = 1,zonen_anzahl ! alle i zonen
         vorhanden = .false.
         nini = -7
         do j = 1,ianz_rb !! alle j Randbedingungen
            if (zone(i)%ini_randnr == rabe(j)%nr_rb) then
               if ( .not. vorhanden) then
                  nini = j
               else
                  write(fehler,"(a,*(i0))") 'Boundary is assigned multiple times: ',zone(i)%ini_randnr, i, rabe(j)%nr_rb,j
                  call qerror(fehler)
               endif
               vorhanden = .true.
            endif
         enddo 
         
         if (vorhanden .and. (nini > 0)) then
            zone(i)%ini_randnr = nini
            print "(4(a,i0),a)", "Zone ", i, " (id ", zone(i)%zonen_nummer,    &
                                 ") is initialized with boundary ",            &
                                 zone(i)%ini_randnr, ' (id ', rabe(zone(i)%ini_randnr)%nr_rb , ')'
         else
            write(fehler,*)'Zone ',i,' with id ',zone(i)%zonen_nummer,  &
                  ' cannot be initialized with boundary ', zone(i)%ini_randnr,  &
                  " nini = ",nini
            call qerror(fehler)
         endif
      enddo
      
      print*
      do i = 1,zonen_anzahl 
         print "(a,i0,a,i0)", 'ini_randnr(',i,') = ',zone(i)%ini_randnr
      enddo
      
      ! Konstanten für Algen belegen
      call ini_algae(akchl, abchl, agchl, a1Ki, a1Bl, a1Gr)
      
      print*
      print "(a)", "after ini_algae:"
      do i = 1,zonen_anzahl 
         print "(a,i0,a,i0)",'ini_randnr(',i,') = ', zone(i)%ini_randnr
      enddo
      
      call RB_werte_aktualisieren(rechenzeit)
      
      print*
      print "(a)", "after RB_werte_aktualisieren:"
      do i = 1,zonen_anzahl
         print "(a,i0,a,i0)",'ini_randnr(',i,') = ',zone(i)%ini_randnr
      enddo 
      
      einmal = .true.
      do j = 1,number_plankt_point ! knotenanzahl2D ! alle j knoten
         nuzo = point_zone(j)
         if ((nuzo <= 0) .or. (nuzo > zonen_anzahl)) then
            write(fehler,*)'initialisieren: error in zone number: ',point_zone(j),nuzo,' at point ',j
            call qerror(fehler)
         endif
         irn = zone(nuzo)%ini_randnr
         call randwert_planktonic(j, irn,einmal)
         call randbedingungen_ergaenzen(j,einmal)
         call tiefenprofil(j)
      enddo ! alle j knoten
      
      ! allocate arrays for hydraulic parameters
      call alloc_hydraul_BC(number_plankt_point)
     
      ! Initialise Boundary Conditions:
      ! Salz+Trübung+Leitfähigkeit:
      call ini_schwebstoff_salz()
      
      ! Aufenthaltszeitberechnung initialisieren
      if (nur_alter) call alter_ini()
      
      ! ausgeschaltet weil durch Anfangsbedingungen aus zonenweise zugeordneter Randbedingung
      ! call alter_ini()
      ! call ini_orgc()
      ! call ini_ncyc()
      ! call ini_ph()
      ! call ini_po4s()
      ! call ini_silikat()

      ! Strömungsfeld anlegen für ersten Schritt Stoffumsatz
      select case (hydro_trieb)
         case(1) ! casu-transinfo
            call holen_trans(na_transinfo)
       
         case(2) ! untrim
            call holen_trans_untrim(na_transinfo)
            print "(a,i0)",'holen_trans_untrim fetching step = ', na_transinfo
            
            ! Courrant-Zahl
            cu(:) = 0.0
            
         case(3) ! SCHISM
            nt = na_transinfo
            ! don't call get_schism_step(nt) here only on rank 0
         
         case default
            call qerror("Invalid value for variable `hydro_trieb`")
      end select
      
      
      ! Dreissena, Muschel Anfangs-Belegung aus Zonenvorgabe
      do i = 1,number_plankt_point ! i
         benthic_distribution(56+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%msohle0  ! Dreissenabiomasse pro Fläche Sohle (0. Kohorte) zdreis(1:2,1) =
         benthic_distribution(57+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%msohle1  ! Dreissenabiomasse pro Fläche Sohle (1. Kohorte) zdreis(1:2,2) =
         benthic_distribution(58+(i-1)*number_benth_distr) = 0.0                                   ! Dreissenabiomasse pro Fläche Sohle (2. Kohorte) zdreis(1:2,3) =
         benthic_distribution(59+(i-1)*number_benth_distr) = 0.0                                   ! Dreissenabiomasse pro Fläche Sohle (3. Kohorte) zdreis(1:2,4) =
         benthic_distribution(60+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%mboesch0 ! Dreissenabiomasse pro Fläche Böschung (0. Kohorte) zdrei(1:2,1) =
         benthic_distribution(61+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%mboesch1 ! Dreissenabiomasse pro Fläche Böschung (1. Kohorte) zdrei(1:2,2) =
         benthic_distribution(62+(i-1)*number_benth_distr) = 0.0                                   ! Dreissenabiomasse pro Fläche Böschung (2. Kohorte) zdrei(1:2,3) =
         benthic_distribution(63+(i-1)*number_benth_distr) = 0.0                                   ! Dreissenabiomasse pro Fläche Böschung (3. Kohorte) zdrei(1:2,4) =
         benthic_distribution(64+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%gewicht0 ! Gewicht einer Dreissena-Muschel (0. Kohorte) gewdr(1:2,1) =
         benthic_distribution(65+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%gewicht1 ! Gewicht einer Dreissena-Muschel (1. Kohorte) gewdr(1:2,2) =
         benthic_distribution(66+(i-1)*number_benth_distr) = 0.0                                   ! Gewicht einer Dreissena-Muschel (2. Kohorte) gewdr(1:2,3) =
         benthic_distribution(67+(i-1)*number_benth_distr) = 0.0                                   ! Gewicht einer Dreissena-Muschel (3. Kohorte) gewdr(1:2,4) =
      enddo
      
      
      ! Benthische Algen, zonenweise Anfangsbelegung
      do i = 1,number_plankt_point 
         benthic_distribution(72+(i-1)*number_benth_distr) = zone(point_zone(i))%albenthi%ggruen  ! Biomasse benthischer Grünalgen
         benthic_distribution(73+(i-1)*number_benth_distr) = zone(point_zone(i))%albenthi%gkiesel  ! Biomasse benthischer Kieselalgen
      enddo 
   endif ! nur prozessor 0
   
   
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Bcast(nt,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(hydro_trieb,1,MPI_INT,0,mpi_komm_welt,ierr)
   
   if (hydro_trieb == 3) then 
      ! get first schism flowfield for initialization in parallel
      ! print*,meinrang,' initialisieren(): get_schism_step fetching step= ',nt, ' in parallel'
      ! call get_schism_step(nt)
   endif
   call mpi_barrier(mpi_komm_welt, ierr)
   
end subroutine initialisieren
