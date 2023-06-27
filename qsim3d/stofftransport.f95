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


subroutine stofftransport()
   use modell
   implicit none
   
   real , allocatable , dimension(:) :: planktonic_variable_before   ! temporary copy of values before transport
   integer                           :: i, j, k, nk

   call mpi_barrier(mpi_komm_welt, ierr)
   if (kontrollknoten > 0) then
      do i = 1,part ! all i elements/nodes on this process
         iglob = i + meinrang * part
         if (kontrollknoten == iglob) then
            print*,iglob,meinrang,i,part," vor ph2hplus lf,ph = ",  &
                   planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)
            
            ! keep variable values from before transport calculations
            nk = (i - 1) * number_plankt_vari
            allocate ( planktonic_variable_before(number_plankt_vari) )
            planktonic_variable_before(:) = planktonic_variable_p([(k, k = nk + 1, nk + number_plankt_vari)])
            
            ! write 'before' values to log file
            write(*, '(a)') 'Planktonic variables before stofftransport'
            do k = 1,number_plankt_vari
               write(*, '(i3,": ",a10," = ",E17.10)') k, trim(planktonic_variable_name(k)), planktonic_variable_before(k)
            enddo
            
            ! control point reached
            exit
         endif
      enddo
   endif
   
   call ph2hplus()
   call mpi_barrier (mpi_komm_welt, ierr)
   
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         call gather_planktkon()
         if (meinrang == 0) then !! nur prozessor 0
            if (kontrollknoten >= 1) print*,'0  vor stofftransport_casu: ph,lf = ',  &
                planktonic_variable(66+(kontrollknoten-1)*number_plankt_vari),  &
                planktonic_variable(65+(kontrollknoten-1)*number_plankt_vari)
            call stofftransport_casu()
            if (kontrollknoten >= 1) print*,'0 nach stofftransport_casu: ph,lf = ',  &
                planktonic_variable(66+(kontrollknoten-1)*number_plankt_vari),  &
                planktonic_variable(65+(kontrollknoten-1)*number_plankt_vari)
         endif !! nur prozessor 0
         call scatter_planktkon()
      case(2) ! Untrim² netCDF
         call gather_planktkon()
         if (meinrang == 0) then !! nur prozessor 0
            call stofftransport_untrim()
            if (kontrollknoten >= 1) print*,'nach stofftransport_untrim: lf,ph = ',  &
                planktonic_variable(65+(kontrollknoten-1)*number_plankt_vari),  &
                planktonic_variable(66+(kontrollknoten-1)*number_plankt_vari)
         endif !! nur prozessor 0
         call scatter_planktkon()
      case(3) ! SCHISM netCDF
         !! call stofftransport_schism() !parallel and 3D
         case default
         call qerror('stofftransport: Hydraulischer Antrieb unbekannt')
   end select
   call mpi_barrier (mpi_komm_welt, ierr)
   call hplus2ph()
   call mpi_barrier (mpi_komm_welt, ierr)
   call gather_planktkon() ! syncronize non-parallel fields to paralell ones again

   if (kontrollknoten > 0) then
      do i = 1,part ! all i elements/nodes on this process
         iglob = i + meinrang * part
         if (kontrollknoten == iglob) then
            
            print*,iglob,meinrang,i,part," nach hplus2ph lf,ph = ",  &
                   planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)
            
            write(*, '(a)') 'Planktonic variables after stofftransport'
            do k = 1,number_plankt_vari
               write(*, '(i3,": ",a10," = ",E17.10,", delta = ",E17.10)') k, trim(planktonic_variable_name(k)), planktonic_variable_p(k + nk), &
                                                                          planktonic_variable_p(k + nk) - planktonic_variable_before(k)
            enddo
            deallocate( planktonic_variable_before )
            
            ! control point reached
            exit
         endif
      enddo
   endif
   
   call mpi_barrier (mpi_komm_welt, ierr)
   if (meinrang == 0) then !! nur prozessor 0
      do j = 1,number_plankt_point ! alle j Knoten
         call tiefenprofil(j)  !! 2D depth avaraged
      enddo ! alle j Berechnungsstützstellen
   endif !! nur prozessor 0
   call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine stofftransport


!----+-----+----
!> Allokieren der Felder für die Transportinformationen.
subroutine allo_trans()
   use modell
   implicit none
   integer :: alloc_status, j
   
   if (meinrang == 0) then ! prozess 0 only
      allocate( p(number_plankt_point), stat = alloc_status ) !, tief(number_plankt_point)
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate p :', alloc_status
         call qerror(fehler)
      endif
      do j = 1,number_plankt_point ! alle j Berechnungsstützstellen
         p(j) = -777.777
      enddo ! alle j Berechnungsstützstellen
      allocate( u(number_plankt_point), dir(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate u :', alloc_status
         call qerror(fehler)
      endif
      allocate( vel_x(number_plankt_point), vel_y(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate u :', alloc_status
         call qerror(fehler)
      endif
      allocate (inflow(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate inflow :', alloc_status
         call qerror(fehler)
      endif
      
      select case (hydro_trieb)
         case(1) ! casu-transinfo
            !call allo_trans(knotenanzahl2D) !! Felder für Transportinformationen und Strömungsfeld allocieren
            allocate (intereck(4*number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate  intereck(4* :', alloc_status
               call qerror(fehler)
            endif
            allocate (wicht(4*number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate  wicht(4* :', alloc_status
               call qerror(fehler)
            endif
            allocate (w(number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate w :', alloc_status
               call qerror(fehler)
            endif
            allocate (ur_x(number_plankt_point),ur_y(number_plankt_point),ur_z(number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0)call qerror('allocate (ur_ failed')
         
         case(2) ! Untrim² netCDF
            !call allo_trans(n_elemente) !! Felder für Transportinformationen und Strömungsfeld allocieren
            allocate( el_vol(number_plankt_point), el_area(number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) call qerror('allocate (el_vol(number_plankt_point), el_area(number_plankt_point)) failed')
            allocate( ed_flux(kantenanzahl), ed_area(kantenanzahl), stat = alloc_status )
            if (alloc_status /= 0) call qerror('allocate( ed_area(kantenanzahl) ) failed')
            allocate( ed_vel_x(kantenanzahl), ed_vel_y(kantenanzahl), stat = alloc_status )
            if (alloc_status /= 0) call qerror('allocate( ed_vel(kantenanzahl) ) failed')
            allocate (wicht(5*number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate  wicht(5* :', alloc_status
               call qerror(fehler)
            endif
            
            ! Courrant-Zahl
            allocate(cu(number_plankt_point), stat = alloc_status)
            if (alloc_status /= 0) then
               write(fehler,"(a,i0)") 'Rueckgabewert von allocate(cu):', alloc_status
               call qerror(fehler)
            endif
         
         case(3) ! SCHISM netCDF
            print*,'####### allo_trans SCHISM macht noch nichts Spezielles VORSICHT #######'
         
         case default
            call qerror('allo_trans: Hydraulischer Antrieb unbekannt')
      end select
   
   endif ! only prozessor 0
   return
end subroutine allo_trans
!----+-----+----
!> calculate proton concentration from pH
subroutine ph2hplus()
   use modell
   implicit none
   real*8 mue,ph,lf,hplus,hk,lgh
   integer i
   
   do i = 1,part ! all i elements/nodes on this process
      iglob = i + meinrang * part
      lf = planktonic_variable_p(65 + (i-1) * number_plankt_vari)
      ph = planktonic_variable_p(66 + (i-1) * number_plankt_vari)
      mue = sqrt(max(0.0, 1.7e-5 * lf))
      hk = mue / (2. + 2.8 * mue)
      hplus = 10**(hk - ph)
      planktonic_variable_p(66+(i-1)*number_plankt_vari) = hplus
      if (iglob == kontrollknoten) print*, meinrang, i, ' ph2hplus: ph, hplus, part, number_plankt_vari = ',  &
          ph, hplus, part, number_plankt_vari
   enddo ! all i elements/nodes on this process
   
   return
end subroutine ph2hplus
!----+-----+----
!> calculate pH from proton concentration
!! \n\n
subroutine hplus2ph()
   use modell
   implicit none
   real(8) :: mue, ph, lf, hplus, hk
   integer :: i
   
   do i = 1,part ! all i elements/nodes on this process
      iglob = i + meinrang * part
      lf = planktonic_variable_p(65 + (i-1) * number_plankt_vari)
      hplus = planktonic_variable_p(66 + (i-1) * number_plankt_vari)
      mue = sqrt(max(0.0, 1.7e-5 * lf))
      hk = mue / (2. + 2.8 * mue)
      ph = hk - log10(hplus)
      planktonic_variable_p(66+(i-1)*number_plankt_vari) = ph
      if (iglob == kontrollknoten) print*, meinrang, i, ' hplus2ph: ph, hplus, part, number_plankt_vari = ',  &
          ph, hplus, part, number_plankt_vari
   enddo ! alle j elements/nodes
   
   return
end subroutine hplus2ph
