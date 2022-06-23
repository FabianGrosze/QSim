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
!> \page lokaleParameter lokale Modell-Eigenschaften
!!
!! Lokale Modelleigenschaften werden in QSim3D jeweils einer Zone zugeordnet.\n
!! Die zonenweise Verteilungen werden mithilfe der Datei \ref qsimdateimodellg3 vorgegeben.\n
!! Beispiel: <a href="./exp/MODELLG.3D.txt" target="_blank">MODELLG.3D.txt</a>
!! \n\n
!! Die Unterteilung des Modellgebiets in Zonen wird von GERRIS durchgeführt und in der Datei
!! Beispiel: <a href="./exp/ELEMENTE.txt" target="_blank">ELEMENTE.txt</a> abgelegt (UNTRIM-Antrieb).
!! Nach der Elementnummer enthält jede Datenzeile die Koordinaten des Elementzentrums, die Zonennummer und die Randnummer (0=kein Rand).
!! \n\n
!! In QSim1D erfolgt die Zuordnung zu Strängen oder Strangabschnitten.\n
!! Dazu wird die Datei MODELLG.txt verwendet.\n
!!
!! \n\n zurück: \ref Modellerstellung \ref Datenmodell ; Quelle: zonen.f95
!> Die Datei zonen.f95 dient dazu ... ??? die Informationen zu einer Zone
!! (räumlich abgegrenzter bereich mit gleichen Eigenschaften) zu speichern.
!! Jeder 2D-Knoten (3D-Vertikale) des HN-Modells bringt eine Zonennummer mit,
!! über die die Zuordnung der Eigenschaften erfolgt.
!!
!       module zonen
!       implicit none
!       save
!       integer :: zonen_anzahl
!       integer , allocatable , dimension (:) :: wetterstations_nummer, zonen_nummer, schifffahrts_zone, ini_randnr
!       real , allocatable , dimension (:)  :: wetterstations_lage, reib_ks
!       PUBLIC :: modellg, zonen_parallel
!       CONTAINS
!
!----+-----+----
!> verteilt Zoneninformationen auf die Prozesse \n
!! aus zonen.f95
subroutine zonen_parallel()
   use modell
   implicit none
   integer :: alloc_status, ini
   call MPI_Bcast(zonen_anzahl,1,MPI_INT,0,mpi_komm_welt,ierr)
   if (meinrang /= 0) then ! alle Prozesse ausser 0
      allocate (knoten_zone(knotenanzahl2D), stat = alloc_status )
      if (alloc_status /= 0) then
         print*,' allocate failed in zonen_parallel knoten_zone :', alloc_status
         call qerror(fehler)
      end if
      do ini = 1,knotenanzahl2D
         knoten_zone(ini) = 0
      end do
      allocate (element_zone(n_elemente), stat = alloc_status )
      if (alloc_status /= 0) then
         print*,' allocate failed in zonen_parallel element_zone :', alloc_status
         call qerror(fehler)
      end if
      do ini = 1,n_elemente
         element_zone(ini) = 0
      end do
      allocate (point_zone(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         print*,' allocate failed in zonen_parallel point_zone :', alloc_status
         call qerror(fehler)
      end if
      do ini = 1,number_plankt_point
         point_zone(ini) = 0
      end do
      allocate (zone(zonen_anzahl), stat = alloc_status )
      if (alloc_status /= 0) call qerror('allocate zone failed in zonen_parallel() zonen.f95')
   end if !! alle Prozesse ausser 0
   call MPI_Bcast(zone%zonen_nummer,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%ini_randnr,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%nr_zone,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%reib,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%sediflux%sedom,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%sediflux%bedgs,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%sediflux%sedvvert,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%sediflux%kornd,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%sediflux%burial,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%seditemp%spewks,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%seditemp%wuebk,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%seditemp%psrefs,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%seditemp%extiks,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%laich%lait,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%laich%laim,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%laich%laid,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%schiff%vschiff,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%schiff%uprop,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%schiff%schifffahrts_zone,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%wettstat%wetterstations_lage,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%wettstat%wetterstations_nummer,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%dreissen%mboesch0,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%dreissen%msohle0,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%dreissen%gewicht0,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%dreissen%mboesch1,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%dreissen%msohle1,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%dreissen%gewicht1,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%dreissen%dreissena_aktiv,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%albenthi%ggruen,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%albenthi%gkiesel,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%macrophyt%starttag,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%macrophyt%startmonat,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%macrophyt%maxtag,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%macrophyt%maxmonat,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%macrophyt%endtag,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%macrophyt%endmonat,zonen_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%macrodicht%pflmin,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%macrodicht%pflmax,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%erosi%tau_krit,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%erosi%M_eros,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%erosi%n_eros,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zone%erosi%sed_roh,zonen_anzahl,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(knoten_zone,knotenanzahl2D,MPI_INT,0,mpi_komm_welt,ierr)
   if (ierr /= 0)call qerror(' MPI_Bcast(knoten_zone, failed ')
   call MPI_Bcast(element_zone,n_elemente,MPI_INT,0,mpi_komm_welt,ierr)
   if (ierr /= 0)call qerror(' MPI_Bcast(element_zone, failed ')
   call MPI_Bcast(point_zone,number_plankt_point,MPI_INT,0,mpi_komm_welt,ierr)
   if (ierr /= 0)call qerror(' MPI_Bcast(point_zone, failed ')
   return
end subroutine zonen_parallel
