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
! \page Datentechnik Informationstechnische Umsetzung und Parallelisierung
! \page numerik Numerik und Datentechnik
!> \page Parallelisierung Parallelisierung
!!!
!! \section Parallel_Umsatz Parallelisierung der Stoffumsetzungsvorgänge
!! Die Parallelisierung von T-QSim nutzt den Umstand, dass die mathematischen Formulierungen,
!! welche die bio-chemischen Stoffumsetzungsvorgänge modellieren, keine Raumgradienten enthalten.
!! Z.B. ist das Algenwachstum nur vom lokalen Licht-, Nährstoffangebot, Temperatur etc. abhängig.
!! Die Verhältnisse im Nachbarwassertropfen "interessieren" die Alge im hiesigen Wassertropfen nicht.\n\n
!! Somit ist es möglich, die Berechnungsstützstellen (Knoten) einfach fortlaufend anhand ihrer Nummer auf die parallelen Prozesse
!! zu verteilen. Umständliche Gebietszerlegungen, wie sie bei der Parallelisierung von Approximationsverfahren zur Lösung
!! partieller Differentialgleichungen (die Raumgradienten enthalten) erforderlich sind, werden nicht benötigt.\n\n
!!
!! \section Parallel_Transport Parallelisierung der Transportvorgänge
!! T-QSim nutzt die Stofftransport-Lösung des vorgeschalteten hydraulischen Treibers indem es die Transportinformation
!! anhand einer Matrix einließt. Diese wird dann auf alle Vektoren multipliziert,
!! welche die Diskretisierungen des Felder jeweils einer Gütevariablen enthalten.
!! Diese Matrix-Vektor-Multiplikation könnte evt. mit PETSc parallelisiert werden.\n\n
!! Das o.g. Verfahren ist auf explizite Zeitdiskretisierungen beschränkt.
!! Implizite Diskretisierungen würden das Lösen von linearen Gleichungssystemen erfordern, was bei der Vielzahl
!! an transportierten Variablen-Feldern in einem Gütemodell sehr aufwändig wäre.
!!
!! \section Parallel_IO Ein- und Ausgabe
!! Die Datenein- und -ausgabe erfolgt in T-QSim zentral. Nur der Prozess 0 beschäftigt sich mit Lesen/Schreiben
!! Alle anderen parallelen Prozesse bekommen Daten nur via MPI (Message-Passing-Interface).\n\n
!! Die Subroutinen eingabe() un initialisieren() werden daher auch nur von Prozess 0 aufgerufen.
!! Die Verteilung der Variablenfelder auf die multiplen Prozesse erfogt wie im
!! Abschnitt \ref Datenstruktur erläutert.
!! \n
!! Variablendefinition der für die Parallelisierung benötigten Datenfelder in module_modell.f95\n
!! \n aus Datei parallel.f95; zurück: \ref index
!----+-----+----
!> parallel_ini()\n
!! startet mpi
!! \n\n
!! aus Datei parallel.f95; zurück: \ref lnk_Datentechnik
subroutine parallel_ini()
   use modell
   use QSimDatenfelder
   !!!####     use schism_msgp, only: myrank,parallel_abort !,nproc
   implicit none
   call mpi_init(ierr)
   if (ierr /= 0)call qerror('mpi_init(ierr) /= 0')
   mpi_komm_welt = MPI_COMM_WORLD
   call mpi_comm_size(mpi_komm_welt,proz_anz, ierr)
   if (ierr /= 0)call qerror('mpi_comm_size(mpi_komm_welt,proz_anz, ierr) /= 0')
   call mpi_comm_rank(mpi_komm_welt,meinrang,ierr)
   !!!####     myrank=meinrang
   if (ierr /= 0)call qerror('mpi_comm_rank(mpi_komm_welt,meinrang,ierr) /= 0')
   write(*,*)'Prozess #',meinrang,' started mit PID = ',getpid()
   return
end subroutine parallel_ini
!----+-----+----
!> parallel_vorbereiten()\n
!! ruft die subroutinen auf, die auf allen Processoren >0
!! die jeweiligen Felder allokieren und füllen.
!! \n\n
!! aus Datei parallel.f95; zurück: \ref lnk_Datentechnik
subroutine parallel_vorbereiten()
   use modell
   use QSimDatenfelder
   use mod_suspendedMatter, only: init_suspendedMatter
   !!!###    use schism_msgp, only: myrank,parallel_abort !,nproc
   implicit none
   integer kontroll_lokal
   ! prepare for parallel
   call modell_parallel()
   !print*,meinrang," modell_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call planktkon_parallel() !calls scatter_planktkon()
   !print*,meinrang," planktkon_parallel() ... fertig"
   call mpi_barrier (mpi_komm_welt, ierr)
   call benthic_parallel()
   !print*,meinrang," benthic_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call ueber_parallel()
   !print*,meinrang," ueber_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call zonen_parallel()
   !print*,meinrang," zonen_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call wetter_parallel()
   !print*,meinrang," wetter_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call randbedingungen_parallel()
   !print*,meinrang," randbedingungen_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   
   ! initialize SPM (and salinity)
   if (iEros>=0) then
      call schwebstoff_salz_parallel()
      !print*,meinrang," schwebstoff_salz_parallel() ... danach"
      call mpi_barrier (mpi_komm_welt, ierr)
   else
      call init_suspendedMatter
   end if
   
   call alter_parallel()
   !print*,meinrang," alter_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   kontroll_lokal = kontrollknoten-(meinrang*part)
   if ((kontroll_lokal > 0) .and. (kontroll_lokal <= part)) then
      print*,'meinrang,part,number_plankt_vari,kontrollknoten,kontroll_lokal = '  &
            , meinrang,part,number_plankt_vari,kontrollknoten,kontroll_lokal
      print*,'nach randbedingungen_parallel(): tempw,chla = ',                    &
            planktonic_variable_p( 1+(kontroll_lokal-1)*number_plankt_vari),      &
            planktonic_variable_p(11+(kontroll_lokal-1)*number_plankt_vari)
   else ! keine kontrollausgabe
      print*,'meinrang,part,number_plankt_vari = ',meinrang,part,number_plankt_vari
   endif
   call ganglinien_parallel()
   call ausgeben_parallel()
   if (meinrang == 0)print*,'parallel_vorbereiten done'
   call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine parallel_vorbereiten
!----+-----+----
!> zentrale Modellwerte an alle Prozesse verteilen.\n
!! und part berechnen. d.i. die Anzahl der Knoten, die jeder Prozess erhält.
!! \n\n
!! aus Datei parallel.f95; zurück: \ref lnk_Datentechnik
subroutine modell_parallel()
   use modell
   use QSimDatenfelder
   implicit none
   integer n, alloc_status
   !print*,meinrang," modell_parallel() ... startet"
   call MPI_Bcast(deltat,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(rechenzeit,1,MPI_INT,0,mpi_komm_welt,ierr)
   zeitpunkt = rechenzeit
   call MPI_Bcast(zeitpunkt,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zeitschrittanzahl,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(time_offset,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(knotenanzahl3D,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(knotenanzahl2D,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(n_elemente,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(kontrollknoten,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(modell_geob,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(modell_geol,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   ! start + end time
   call MPI_Bcast(itags,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(monats,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(jahrs,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(uhrs,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(startzeitpunkt,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(itage,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(monate,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(jahre,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(uhren,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(endzeitpunkt,1,MPI_INT,0,mpi_komm_welt,ierr)
   ! Berechnung- und Ausgabeflags
   !read(92,9220)imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren,ilongDis,FlongDis,iColi,ikonsS,iSchwer,iphy  aus EREIGG.txt
   call MPI_Bcast(imitt,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ipH,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(idl,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(itemp,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(itracer,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ieros,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ischwa,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iverfahren,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ilongDis,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(FlongDis,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iColi,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ikonsS,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iSchwer,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iphy,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iformVert,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iform_verdr,1,MPI_INT,0,mpi_komm_welt,ierr)
   !call MPI_Bcast(min_tief,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   !print*,meinrang," modell_parallel() min_tief,n_elemente=", min_tief,n_elemente
   call MPI_Bcast(number_plankt_point,1,MPI_INT,0,mpi_komm_welt,ierr)
   !print*,meinrang," modell_parallel() number_plankt_point=", number_plankt_point
   !     !partitioning of variable arrays needed earlier now in eingabe()
   !     part=number_plankt_point/proz_anz
   !     n=part*proz_anz
   !     !print*,'ini_par knotenanzahl=', nk,' proz_anz=', proz_anz, ' part=', part, ' part*proz_anz=', n
   !     if(n.lt.number_plankt_point)part=part+1
   !     print*,'part=', part, ' part*proz_anz=',part*proz_anz," meinrang=",meinrang  &
   ! &         ," modell_parallel() number_plankt_point=", number_plankt_point
   !!geländehöhe überall verfügbar machen
   if (meinrang /= 0) then
      allocate (knoten_z(knotenanzahl2D), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)'modell_parallel() Rueckgabewert von allocate knoten_z(knotenanzahl2D) :', alloc_status
         call qerror(fehler)
      end if ! allocate fehlgeschlagen
   end if !! nicht 0-Prozess
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Bcast(knoten_z,knotenanzahl2D,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call mpi_barrier (mpi_komm_welt, ierr)
   !if(meinrang .eq.0)print*,"modell_parallel fertig"
   !print*,"modell_parallel fertig ",meinrang
   return
end subroutine modell_parallel
