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
!> Anlegen der Datenfelder für die Prozesse > 0
!! und anschließend verteilen 
subroutine planktkon_parallel()
   use modell
   implicit none
   integer :: as, i_local
   
   if ((meinrang == 0) .and. (kontrollknoten > 0)) then
      print*,'0 planktkon_parallel starting GlMn = (',kontrollknoten,') = '   &
       ,planktonic_variable(99+(kontrollknoten-1)*number_plankt_vari)
   endif
   
   ! depth averaged
   allocate (planktonic_variable_p(number_plankt_vari*part), source = -2., stat = as )
   if (as /= 0) call qerror("Error while allocating variable `planktonic_variable_p`")
   
   ! vertical profiles i.e. full 3D
   allocate (plankt_vari_vert_p(num_lev*number_plankt_vari_vert*part), source = -1., stat = as )
   if (as /= 0) call qerror("Error while allocate variable `plankt_vari_vert_p`")
   
   call scatter_planktkon()
   call mpi_barrier (mpi_komm_welt, ierr)
   
   if (kontrollknoten > 0) then
      i_local = kontrollknoten - (meinrang*part)
      if (i_local > 0 .and. i_local <= part) then
         print*,meinrang,part,i_local,kontrollknoten,number_plankt_vari,  &
          'planktkon_parallel finish GlMn_p = ',planktonic_variable_p(99+(i_local-1)*number_plankt_vari)
      endif
   endif
   
end subroutine planktkon_parallel


!> Verteilen der transportierten Konzentrationen auf die parallelen Prozesse.
subroutine scatter_planktkon()
   use modell
   implicit none
   
   call MPI_Scatter(planktonic_variable, part*number_plankt_vari, MPI_FLOAT,  &
                    planktonic_variable_p, part*number_plankt_vari, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 13 MPI_Scatter(planktonic_variable failed :', ierr
      call qerror(fehler)
   endif
   
   call MPI_Scatter(plankt_vari_vert, part*number_plankt_vari_vert*num_lev, MPI_FLOAT,  &
                    plankt_vari_vert_p, part*number_plankt_vari_vert*num_lev, MPI_FLOAT, 0,mpi_komm_welt, ierr)
   
   if (ierr /= 0) then
      write(fehler,*)' 14 MPI_Scatter(plankt_vari_vert failed :', ierr
      call qerror(fehler)
   endif
   
end subroutine scatter_planktkon


!> wieder zusammensammeln der transportierten Konzentrationen von den parallelen Prozesse.
subroutine gather_planktkon()
   use modell
   implicit none

   call MPI_Gather(planktonic_variable_p, part*number_plankt_vari, MPI_FLOAT,  &
                   planktonic_variable, part*number_plankt_vari, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 15 MPI_Gather(planktonic_variable failed :', ierr
      call qerror(fehler)
   endif

   call MPI_Gather(plankt_vari_vert_p, part*number_plankt_vari_vert*num_lev, MPI_FLOAT,  &
                   plankt_vari_vert, part*number_plankt_vari_vert*num_lev, MPI_FLOAT, 0,mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 16 MPI_Gather(plankt_vari_vert failed :', ierr
      call qerror(fehler)
   endif

end subroutine gather_planktkon

!> Initialisierung der transportierten Konzentrationen auf 0.0.
subroutine ini_planktkon0(nk)
   use modell
   implicit none
   integer nk,k,n,as,j,l,ini
   if (meinrang == 0) then ! prozess 0 only
      number_plankt_point = nk
      
      !------- tiefengemittelte planktische variablen
      do j = 1,number_plankt_vari ! initialise
         write(planktonic_variable_name(j),'(18x)')
      enddo
      include "planktonic_variable_name.h"
      planktonic_variable_name = adjustl(planktonic_variable_name)
      
      do j = 1,number_plankt_vari ! zunächst nix ausgeben
         output_plankt(j) = .false.
      enddo
      
      ! allocate and initialize planktonic_variable
      allocate (planktonic_variable(number_plankt_vari*part*proz_anz), source = 0., stat = as)
      if (as /= 0) call qerror("Error while allocating variable `planktonic_variable`")
      
      
      ! ------- tiefenaufgelöst, planktonic variables
      do j = 1,number_plankt_vari_vert ! initialise
         write(plankt_vari_vert_name(j),'(18x)')
      enddo
      include "plankt_vari_vert_name.h"
      plankt_vari_vert_name = adjustl(plankt_vari_vert_name)
      
      ! allocate and initialize plankt_vari_vert
      allocate (plankt_vari_vert(num_lev*number_plankt_vari_vert*part*proz_anz), source = 0., stat = as )
      if (as /= 0) call qerror("Error while allocating variable `plankt_vari_vert`")
      output_plankt_vert(:) = .false.
      
      allocate (point_zone(number_plankt_point), stat = as )
      if (as /= 0) call qerror("Error while allocating variable `point_zone`")
      
      select case (hydro_trieb)
      case(1) ! casu-transinfo
         do ini = 1,number_plankt_point
            point_zone(ini) = knoten_zone(ini)
         enddo
      case(2) ! Untrim² netCDF
         do ini = 1,number_plankt_point
            point_zone(ini) = element_zone(ini)
         enddo
      case(3) ! SCHISM netCDF (doch noch von zone.gr3)
         do ini = 1,number_plankt_point
            point_zone(ini) = element_zone(ini)
         enddo
         !call qerror('ini_planktkon0: SCHISM zone not yet worked out')
      case default
         call qerror('ini_planktkon0: unknown hydraulic driver')
      end select
      
   endif !! nur prozessor 0
   
   ! make names available to all processes (e.g. for logging)
   call mpi_barrier(mpi_komm_welt, ierr)
   do k = 1,number_plankt_vari
      call mpi_bcast(planktonic_variable_name(k), len(planktonic_variable_name(k)), MPI_CHAR, 0, mpi_komm_welt, ierr)
   enddo
   do k = 1,number_plankt_vari_vert
      call mpi_bcast(plankt_vari_vert_name(k)   , len(plankt_vari_vert_name(k))   , MPI_CHAR, 0, mpi_komm_welt, ierr)
   enddo
   call mpi_barrier(mpi_komm_welt, ierr)
   
end subroutine ini_planktkon0
