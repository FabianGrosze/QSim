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

!> Zonen und Randnummern von der Datei ELEMENTE.txt einlesen,
!! selbige wurde von Gerris erzeugt.
logical function read_elemente_gerris()
   use modell
   implicit none
   integer k, n, alloc_status, io_error, nelli, neln, nelz, nelr, errcode
   real , allocatable , dimension (:) :: rbc
   real elx, ely, dist
   character(longname) dateiname
   
   read_elemente_gerris = .true.

   write(dateiname,'(2A)',iostat = errcode)trim(modellverzeichnis),'ELEMENTE.txt'
   if (errcode /= 0)call qerror('read_elemente_gerris writing filename ELEMENTE.txt failed')
   open ( unit = 22 , file = dateiname, status = 'old', action = 'read ', iostat = io_error )
   if (io_error /= 0) then
      print*,'open_error ELEMENTE.txt,\nDatei wird aber benötigt .. daher Abbruch'
      read_elemente_gerris = .false.
      return
   end if ! open_error.ne.0
   
   if (zeile(22)) then
      print*,'ELEMENTE.txt erste Zeile:',ctext(1:50)
   else
      write(fehler,*)'Lesen erste Zeile von ELEMENTE.txt fehlgeschlagen'
      call qerror(fehler)
   end if
   if (zeile(22)) then
      print*,'ELEMENTE.txt zweite Zeile:',ctext(1:50)
   else
      write(fehler,*)'Lesen zweite Zeile von ELEMENTE.txt fehlgeschlagen'
      call qerror(fehler)
   end if
   if ( .not. zeile(22))call qerror('Zeile 2(3) in ELEMENTE.txt fehlt')
   read(ctext, *, iostat = io_error) nelli
   if ( (io_error /= 0) .or. ( nelli /= n_elemente) ) then
      print*,'Elementanzahl in ELEMENTE.txt falsch oder nicht lesbar',nelli,n_elemente,io_error
      read_elemente_gerris = .false.
      return
   else!Elementanzahl o.k.
      print*,'read_elemente_gerris: Elementanzahl in ELEMENTE.txt o.k.',nelli
   end if !Elementanzahl falsch
   do n = 1,n_elemente ! alle Elemente
      if ( .not. zeile(22))call qerror('Zeile in ELEMENTE.txt nicht lesbar')
      read(ctext, *, iostat = io_error) neln, elx, ely, nelz, nelr
      if (n /= neln)call qerror('Elementnummer in ELEMENTE.txt falsch')
      dist = (elx-element_x(n))**2 + (ely-element_y(n))**2
      if (dist > 25)call qerror('Elementzentren mehr als 5 Meter Entfernung')
      !integer , allocatable , dimension (:) :: element_rand
      element_zone(n) = nelz
      element_rand(n) = nelr
   end do ! alle n Elemente
   !     knoten_rand ebenfalls nur zu Darstellungszwecken ... zeigt kanten-randnummern
   !     allocate (cell_bound_length(kantenanzahl), stat = alloc_status )
   allocate (rbc(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (rbc failed')
   do n = 1,knotenanzahl2D
      rbc(n) = 0.0
   end do ! alle Knoten
   do n = 1,kantenanzahl
      rbc(top_node(n)) = rbc(top_node(n))+0.5*real(boundary_number(n))
      rbc(bottom_node(n)) = rbc(bottom_node(n))+0.5*real(boundary_number(n))
   end do ! alle kanten
   allocate (knoten_rand(knotenanzahl2D), stat = alloc_status )
   if (alloc_status /= 0) call qerror('allocate (knoten_rand failed')
   do n = 1,knotenanzahl2D
      knoten_rand(n) = int(rbc(n))
   end do ! alle Knoten
   deallocate (rbc, stat = alloc_status )
   ! Knoten-zonen zu Darstellungszwecken
   do n = 1,knotenanzahl2D
      knoten_zone(n) = 0
   end do ! alle n Knoten zunächst 0
   do n = 1,n_elemente ! alle Elemente
      do k = 1,cornernumber(n)
         !! höchste Zonennummer am Knoten, wenn verschiedene aus angrenzenden Elementen
         if ( element_zone(n) > knoten_zone(elementnodes(n,k)) ) knoten_zone(elementnodes(n,k)) = element_zone(n)
         !! Randknoten auf Element-nummern setzen:
         if ((knoten_rand(elementnodes(n,k)) > 0) .and. (element_rand(n) > 0))knoten_rand(elementnodes(n,k)) = element_rand(n)
      end do ! alle k Element-ecken
   end do ! alle n Elemente
   return
end function read_elemente_gerris
