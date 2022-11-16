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

!> allokieren der Felder für die Transportinformationen.
!! \n\n
subroutine allo_trans()
   use modell
   implicit none
   integer :: alloc_status,j
   if (meinrang == 0) then ! prozess 0 only
      allocate( p(number_plankt_point), stat = alloc_status ) !, tief(number_plankt_point)
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate p :', alloc_status
         call qerror(fehler)
      end if
      do j = 1,number_plankt_point ! alle j Berechnungsstützstellen
         p(j) = -777.777
      end do ! alle j Berechnungsstützstellen
      allocate( u(number_plankt_point), dir(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate u :', alloc_status
         call qerror(fehler)
      end if
      allocate( vel_x(number_plankt_point), vel_y(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate u :', alloc_status
         call qerror(fehler)
      end if
      allocate (inflow(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate inflow :', alloc_status
         call qerror(fehler)
      end if
      select case (hydro_trieb)
         case(1) ! casu-transinfo
            !call allo_trans(knotenanzahl2D) !! Felder für Transportinformationen und Strömungsfeld allocieren
            allocate (intereck(4*number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate  intereck(4* :', alloc_status
               call qerror(fehler)
            end if
            allocate (wicht(4*number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate  wicht(4* :', alloc_status
               call qerror(fehler)
            end if
            allocate (w(number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate w :', alloc_status
               call qerror(fehler)
            end if
            allocate (ur_x(number_plankt_point),ur_y(number_plankt_point),ur_z(number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0)call qerror('allocate (ur_ failed')
         case(2) ! Untrim² netCDF
            !call allo_trans(n_elemente) !! Felder für Transportinformationen und Strömungsfeld allocieren
            allocate( el_vol(number_plankt_point), el_area(number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0)call qerror('allocate (el_vol(number_plankt_point), el_area(number_plankt_point)) failed')
            allocate( ed_flux(kantenanzahl), ed_area(kantenanzahl), stat = alloc_status )
            if (alloc_status /= 0)call qerror('allocate( ed_area(kantenanzahl) ) failed')
            allocate( ed_vel_x(kantenanzahl), ed_vel_y(kantenanzahl), stat = alloc_status )
            if (alloc_status /= 0)call qerror('allocate( ed_vel(kantenanzahl) ) failed')
            allocate (wicht(5*number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate  wicht(5* :', alloc_status
               call qerror(fehler)
            end if
            allocate (cu(number_plankt_point), stat = alloc_status )
         case(3) ! SCHISM netCDF
            print*,'####### allo_trans SCHISM macht noch nix spezielles VORSICHT #######'
         case default
            call qerror('allo_trans: Hydraulischer Antrieb unbekannt')
      end select
   end if ! only prozessor 0
   return
end subroutine allo_trans
!----+-----+----
