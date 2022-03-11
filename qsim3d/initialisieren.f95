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

!> \page Anfangsbedingungen Anfangsbedingungen
!! Die Setzung der Startwerte für \ref planktische_variablen wird zonenweise als \ref lokaleParameter vorgenommen.\n
!! Zur Initilisierung werden die Werte aus einer, der jeweiligen Zone zugeordneten Randbedingung verwendet.\n
!! Diese Zuordnung geschieht mithilfe der I-Zeile in der Datei \ref qsimdateimodellg3. 
!! \n\n
!! aus Datei initialisieren.f95 ; zurück: \ref Modellerstellung oder \ref Datenmodell.
      SUBROUTINE initialisieren()
!
      use modell                                                   
      use QSimDatenfelder
           
      implicit none
      integer i,j,k,nini,nuzo, nt, irn
      logical vorhanden, einmal

if(meinrang.eq.0)then ! nur auf Prozessor 0 bearbeiten

      print*,'initialisieren mit rechenzeit=',rechenzeit

!     Initialisierung aller transportierten planktischen Variablen (Konzentrationen) zunächt auf Null
      do k=1,number_plankt_point ! i
         do j=1,number_plankt_vari ! initialisierung aller konzentrationen zunächt auf Null
               planktonic_variable(j+(k-1)*number_plankt_vari) = 0.0
         end do
         !planktonic_variable(57+(k-1)*number_plankt_vari)= 0.15 ! nl0 >> nL0(ior) = (hcQ*hcnl0+hcQE*hcnl0E)/(hcQ+hcQE) ncyc() ??? hcnl0E wird berechnet in qsim.f90
         !planktonic_variable(58+(k-1)*number_plankt_vari)= 0.03 ! pl0 >> pl0(ior) = (hcQ*hcpl0+hcQE*hcpl0E)/(hcQ+hcQE) po4s() ??? hcpl0E wird berechnet in qsim.f90
      end do

!!!!! Initilisieren der Zone mit der zugeordneten Randbedingung ini_randnr aus MODELLG.3D.txt
!! Dazu ein <a href="./exp/MODELLG.3D.txt" target="_blank">Beispiel</a>.\n\n
      !! Initialisierungs-Randnummer der Zone, ini_randnr, in Randzähler umwandeln...
      do i=1,zonen_anzahl ! alle i zonen
         vorhanden=.false.
         nini=-7
         do j=1,ianz_rb !! alle j Randbedingungen
            !print*,'zone i=',i,' zonen_nummer(i)=',zonen_nummer(i),' zone(i)%ini_randnr=',zone(i)%ini_randnr  &
            !      ,' j=',j,' rabe(j)%nr_rb=',rabe(j)%nr_rb
            if(zone(i)%ini_randnr.eq.rabe(j)%nr_rb)then
               if(.not.vorhanden)then
                  nini=j
                  !print*,"Zuordnung zone(i)%ini_randnr=j ",zone(i)%ini_randnr,"=",j
               else
                  write(fehler,*)' 333 doppelzuornung ini_randnr ### Abbruch',zone(i)%ini_randnr,i,rabe(j)%nr_rb,j
                  call qerror(fehler)
               endif
               vorhanden=.true.
            endif
         end do !! alle j Randbedingungen
         if(vorhanden.and.(nini.gt. 0))then
            zone(i)%ini_randnr=nini
            print*,'initialisieren: Zone ',i,' mit Nr. ',zone(i)%zonen_nummer  &
                  ,' wird initialisiert mit Rand ',zone(i)%ini_randnr,' mit Nr.',rabe(zone(i)%ini_randnr)%nr_rb
         else
            write(fehler,*)' 99 Zone ',i,' mit Nr. ',zone(i)%zonen_nummer  &
                  ,' möchte initialisieren mit Rand ', zone(i)%ini_randnr  &
                  ," nini=",nini,' Diese Vorgabe (Randnummer) ist unmöglich ### Abbruch'
            call qerror(fehler)
         endif
      end do ! alle i zonen
      do i=1,zonen_anzahl ! alle i zonen
         print*,'initialisieren: ini_randnr(',i,')=',zone(i)%ini_randnr
      end do ! alle i zonen

      call ini_algae(akchl,abchl,agchl,Cagr,Caki,Cabl,CZoo,a1Ki,a2Ki,a3Ki,a1Bl,a2Bl,a3Bl,a1Gr,a2Gr,a3Gr) ! Konstanten aus QSimDatenfelder für Algen belegen
      do i=1,zonen_anzahl ! alle i zonen
         print*,'initialisieren nach ini_algae: ini_randnr(',i,')=',zone(i)%ini_randnr
      end do ! alle i zonen

      call RB_werte_aktualisieren(rechenzeit)
      do i=1,zonen_anzahl ! alle i zonen
         print*,'initialisieren nach RB_werte_aktualisieren: ini_randnr(',i,')=',zone(i)%ini_randnr
      end do ! alle i zonen

      einmal=.true.
      do j=1,number_plankt_point ! knotenanzahl2D ! alle j knoten
         !select case (hydro_trieb)
         !case(1) ! casu-transinfo                                           
         !   nuzo=knoten_zone(j)
         !case(2) ! Untrim² netCDF
         !   nuzo=element_zone(j)
         !case(3) ! schism ????                                           
         !   nuzo=knoten_zone(j)
         !case default
         !   call qerror('Hydraulischer Antrieb unbekannt initialisieren nuzo')
         !end select
         nuzo=point_zone(j)
         if((nuzo.le. 0).or.(nuzo.gt. zonen_anzahl))then
            write(fehler,*)'initialisieren: error in zone number: ',point_zone(j),nuzo,' at point ',j
            call qerror(fehler)
         end if

         if(j.eq.kontrollknoten)then
            print*,'vor randwert_planctonic ini_randnr(',nuzo,')=',zone(nuzo)%ini_randnr
            print*,'rabe(',zone(nuzo)%ini_randnr,')%wert_jetzt(22)='  &
                  , rabe(  zone(nuzo)%ini_randnr  )%wert_jetzt(22)
         endif
         irn=zone(nuzo)%ini_randnr
         call randwert_planctonic(j, irn,einmal)
         call randbedingungen_ergaenzen(j,einmal)

         call tiefenprofil(j)

      end do ! alle j knoten
      !print*,'initialisieren: randbedingungen_ergaenzen done', meinrang

      if(kontrollknoten.gt.0)print*,'nach initialisieren mit Randbedingung: tempw,chla,obsb,ocsb=',  &
             planktonic_variable( 1+(kontrollknoten-1)*number_plankt_vari),  &
             planktonic_variable(11+(kontrollknoten-1)*number_plankt_vari),  &
             planktonic_variable(17+(kontrollknoten-1)*number_plankt_vari),  &
             planktonic_variable(18+(kontrollknoten-1)*number_plankt_vari) 

!     allocate arrays for hydraulic parameters
      call alloc_hydraul_BC(number_plankt_point)

!     Initialise Boundary Conditions:
!     Salz+Trübung+Leitfähigkeit:
      call ini_schwebstoff_salz()

!     Aufenthaltszeitberechnung initialisieren
      if(nur_alter) call alter_ini()
      !call alter_ini()

!! ausgeschaltet weil durch Anfangsbedingungen aus zonenweise zugeordneter Randbedingung
!     organischer Kohlenstoff:
      !call ini_orgc()
!     Stickstoff:
      !call ini_ncyc()
!     Ph-Wert
      !call ini_ph()
!     Phosphor:
      !call ini_po4s()
!     Silizium:
      !call ini_silikat()
!     Sauerstoff:
      !call ini_oxygen()

!     Strömungsfeld anlegen für ersten Schritt Stoffumsatz
      select case (hydro_trieb)
      case(1) ! casu-transinfo                                           
         call holen_trans(na_transinfo)
      case(2) ! untrim                                           
         call holen_trans_untrim(na_transinfo)
         print*,'initialisieren(): holen_trans_untrim fetching step= ',na_transinfo
      case(3) ! SCHISM                                        
         nt=na_transinfo
		 print*, "don't call get_schism_step(nt) here only on rank 0 | nt=", nt
      case default
         call qerror('initialisieren: Hydraulischer Antrieb unbekannt')
      end select
      !!call ganglinien_zeitschritt(1)  !! will be done by Program QSIM3D

      !do j=1,knotenanzahl2D ! an alle j knoten die Überstaudauern (zurück)-initialisieren
      !   benthic_distribution(44+(j-1)*number_benth_distr) =  0.0
      !   benthic_distribution(45+(j-1)*number_benth_distr) =  0.0
      !   benthic_distribution(46+(j-1)*number_benth_distr) =  0.0
      !   benthic_distribution(47+(j-1)*number_benth_distr) =  0.0
     ! end do ! alle j knoten

      if(kontrollknoten.gt.0)print*,'initialisieren(): tempw,chla=',  &
             planktonic_variable( 1+(kontrollknoten-1)*number_plankt_vari),  &
             planktonic_variable(11+(kontrollknoten-1)*number_plankt_vari)

      !print*,'initialisieren(): tracer testbelegung #########'
      !do j=1,number_plankt_point ! 
      !   planktonic_variable(71+(j-1)*number_plankt_vari)=0.0
      !end do !

      ! Dreissena, Muschel Anfangs-Belegung aus Zonenvorgabe
      do i=1,number_plankt_point ! i
         benthic_distribution(56+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%msohle0 ! Dreissenabiomasse pro Fläche Sohle (0. Kohorte) zdreis(1:2,1) = 
         benthic_distribution(57+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%msohle1 ! Dreissenabiomasse pro Fläche Sohle (1. Kohorte) zdreis(1:2,2) = 
         benthic_distribution(58+(i-1)*number_benth_distr) = 0.0 ! Dreissenabiomasse pro Fläche Sohle (2. Kohorte) zdreis(1:2,3) = 
         benthic_distribution(59+(i-1)*number_benth_distr) = 0.0 ! Dreissenabiomasse pro Fläche Sohle (3. Kohorte) zdreis(1:2,4) = 
         benthic_distribution(60+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%mboesch0  ! Dreissenabiomasse pro Fläche Böschung (0. Kohorte) zdrei(1:2,1) = 
         benthic_distribution(61+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%mboesch1  ! Dreissenabiomasse pro Fläche Böschung (1. Kohorte) zdrei(1:2,2) = 
         benthic_distribution(62+(i-1)*number_benth_distr) = 0.0  ! Dreissenabiomasse pro Fläche Böschung (2. Kohorte) zdrei(1:2,3) = 
         benthic_distribution(63+(i-1)*number_benth_distr) = 0.0  ! Dreissenabiomasse pro Fläche Böschung (3. Kohorte) zdrei(1:2,4) = 
         benthic_distribution(64+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%gewicht0  ! Gewicht einer Dreissena-Muschel (0. Kohorte) gewdr(1:2,1) = 
         benthic_distribution(65+(i-1)*number_benth_distr) = zone(point_zone(i))%dreissen%gewicht1  ! Gewicht einer Dreissena-Muschel (1. Kohorte) gewdr(1:2,2) = 
         benthic_distribution(66+(i-1)*number_benth_distr) = 0.0  ! Gewicht einer Dreissena-Muschel (2. Kohorte) gewdr(1:2,3) = 
         benthic_distribution(67+(i-1)*number_benth_distr) = 0.0  ! Gewicht einer Dreissena-Muschel (3. Kohorte) gewdr(1:2,4) = 
      end do ! alle k Berechnungspunkte

      ! Benthische Algen, zonenweise Anfangsbelegung
      do i=1,number_plankt_point ! i
         benthic_distribution(72+(i-1)*number_benth_distr) = zone(point_zone(i))%albenthi%ggruen  ! Biomasse benthischer Grünalgen
         benthic_distribution(73+(i-1)*number_benth_distr) = zone(point_zone(i))%albenthi%gkiesel  ! Biomasse benthischer Kieselalgen
      end do ! alle k Berechnungspunkte

end if !! nur prozessor 0
      !print*,'initialisieren(): kontrollpunkt', meinrang
      call mpi_barrier (mpi_komm_welt, ierror)
      call MPI_Bcast(nt,1,MPI_INT,0,mpi_komm_welt,ierror)
      call MPI_Bcast(hydro_trieb,1,MPI_INT,0,mpi_komm_welt,ierror)
      if(hydro_trieb.eq. 3)then ! get first schism flowfield for initialization in parallel
	     print*,meinrang,' initialisieren(): not yet get_schism_step fetching step= ',nt, ' in parallel'
		 stop
         !call get_schism_step(nt)
	  end if ! hydro_trieb=SCHISM,3
      call mpi_barrier (mpi_komm_welt, ierror)

      RETURN
      END subroutine initialisieren


