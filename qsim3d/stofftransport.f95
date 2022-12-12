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
   integer :: i, n, j,ierr
   do i = 1,part ! all i elements/nodes on this process
      iglob = i + meinrang * part
      if (kontrollknoten == iglob)print*,iglob,meinrang,i,part," vor ph2hplus lf,ph = ",  &
          planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)
   end do
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
         end if !! nur prozessor 0
         call scatter_planktkon()
      case(2) ! Untrim² netCDF
         call gather_planktkon()
         if (meinrang == 0) then !! nur prozessor 0
            call stofftransport_untrim()
            if (kontrollknoten >= 1) print*,'nach stofftransport_untrim: lf,ph = ',  &
                planktonic_variable(65+(kontrollknoten-1)*number_plankt_vari),  &
                planktonic_variable(66+(kontrollknoten-1)*number_plankt_vari)
         end if !! nur prozessor 0
         call scatter_planktkon()
         
      case(3) ! SCHISM netCDF
         !call gather_planktkon()
         call schism_tracer_fields(1)
         
         call stofftransport_schism() !parallel and 3D
         
         call schism_tracer_fields(2)
         !call scatter_planktkon()
         
         if (meinrang==control_proc)
            print*,'nach stofftransport_schism: lf,ph = ',           &
                  planktonic_variable_p(65+(control_elem-1)*maxel),  &
                  planktonic_variable_p(66+(control_elem-1)*maxel)
         endif ! Kontrollknoten

      case default
         call qerror('stofftransport: Hydraulischer Antrieb unbekannt')
   end select
   call mpi_barrier (mpi_komm_welt, ierr)
   call hplus2ph()
   call mpi_barrier (mpi_komm_welt, ierr)
   call gather_planktkon() ! syncronize non-parallel fields to paralell ones again
   do i = 1,part ! all i elements/nodes on this process
      iglob = (i+meinrang*part)
      if (kontrollknoten == iglob)print*,iglob,meinrang,i,part," nach hplus2ph lf,ph = ",  &
          planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)
   end do
   call mpi_barrier (mpi_komm_welt, ierr)
   if (meinrang == 0) then !! nur prozessor 0
      do j = 1,number_plankt_point ! alle j Knoten
         call tiefenprofil(j)  !! 2D depth avaraged
      end do ! alle j Berechnungsstützstellen
   end if !! nur prozessor 0
   call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine stofftransport
!----+-----+----
!> calculate proton concentration from pH
!! \n\n
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
   end do ! all i elements/nodes on this process
   
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
   end do ! alle j elements/nodes
   
   return
end subroutine hplus2ph
