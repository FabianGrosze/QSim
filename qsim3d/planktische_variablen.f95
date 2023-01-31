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
!> Anlegen der Datenfelder für die Prozesse > 0\n
!! und anschließend verteilen \n\n
subroutine planktkon_parallel()
   use modell
   use schism_glbl, only:tr_el,tr_nd,nea2,ntracers,nvrt,npa
   implicit none
   integer as,j,k,i,iloka,ierr
   !print*,meinrang, ' planktkon_parallel starting'
   if ((meinrang == 0) .and. (kontrollknoten > 0))print*,'0 planktkon_parallel starting 99:',  &
       planktonic_variable_name(99),' = (',kontrollknoten,') = ',                             &
       planktonic_variable(99+(kontrollknoten-1)*number_plankt_vari)
   ! depth averaged
   allocate (planktonic_variable_p(number_plankt_vari*part), stat = as )
   if (as /= 0) then
      write(fehler,*)' return value allocate planktonic_variable_p :', as
      call qerror(fehler)
   end if
   planktonic_variable_p=0.0
   !planktonic_variable_p(:)=-2.0
   
   ! vertical profiles i.e. full 3D
   allocate (plankt_vari_vert_p(num_lev*number_plankt_vari*part), stat = as )
   if (as /= 0) then
      write(fehler,*)' return value  plankt_vari_vert_p :', as
      call qerror(fehler)
   end if
   plankt_vari_vert_p=0.0
   
   !call mpi_barrier (mpi_komm_welt, ierr)
   call scatter_planktkon()
   call mpi_barrier (mpi_komm_welt, ierr)
   
   if (kontrollknoten > 0) then
      iloka = kontrollknoten-(meinrang*part)
      if ((iloka > 0) .and. (iloka <= part))print*,meinrang,part,iloka,kontrollknoten,number_plankt_vari,  &
          'planktkon_parallel finish GlMn_p = ',planktonic_variable_p(99+(iloka-1)*number_plankt_vari)
      if (meinrang == 0)print*,'0 planktkon_parallel finish GlMn = (',kontrollknoten,') = '   &
          ,planktonic_variable(99+(kontrollknoten-1)*number_plankt_vari)
   endif ! kontrollknoten
   
   
   if (hydro_trieb==3)then ! schism
      ! ntracers=number_plankt_vari ; nvrt=num_lev !! set in read_mesh_schism

      if(.not.allocated(tr_el))then
         allocate(tr_el(ntracers,nvrt,nea2), stat = as )
         if (as /= 0) then
            write(fehler,*)' return value allocate tr_el :', as
            call qerror(fehler)
         else
            print*,meinrang,'planktkon_parallel allocate(tr_el',ntracers,nvrt,nea2
         end if
         tr_el=0.0
      endif ! tr_el allocated
      allocate(tr_nd(ntracers,nvrt,npa), stat = as )
      if (as /= 0) then
         write(fehler,*)' return value allocate tr_nd :', as
         call qerror(fehler)
      end if
      tr_nd=0.0

   end if ! schism

   
   return
end subroutine planktkon_parallel
!----+-----+----
!> Verteilen der transportierten Konzentrationen auf die parallelen Prozesse.
!! \n\n
subroutine scatter_planktkon()
   use modell
   implicit none
   integer ierr

   !print*,'scatter_planktkon part,number_plankt_vari,meinrang=',part, number_plankt_vari, meinrang
   !if(allocated(planktonic_variable))then
   !   print*,meinrang,' scatter_planktkon: planktonic_variable allocated'
   !else
   !   print*,meinrang,' scatter_planktkon: planktonic_variable not allocated'
   !endif
   !call mpi_barrier (mpi_komm_welt, ierr)

   call MPI_Scatter(planktonic_variable, part*number_plankt_vari, MPI_FLOAT,  &
                    planktonic_variable_p, part*number_plankt_vari, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 13 MPI_Scatter(planktonic_variable failed :', ierr
      call qerror(fehler)
   end if
   call MPI_Scatter(plankt_vari_vert, part*number_plankt_vari*num_lev, MPI_FLOAT,  &
                    plankt_vari_vert_p, part*number_plankt_vari*num_lev, MPI_FLOAT, 0,mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 14 MPI_Scatter(plankt_vari_vert failed :', ierr
      call qerror(fehler)
   end if
   !print*,meinrang, ' scatter_planktkon finish'
   return
end subroutine scatter_planktkon
!----+-----+----
!> wieder zusammensammeln der transportierten Konzentrationen von den parallelen Prozesse.
!! \n\n
subroutine gather_planktkon()
   use modell
   implicit none
   integer ierr
   !print*,'gather_planktkon'
   call MPI_Gather(planktonic_variable_p, part*number_plankt_vari, MPI_FLOAT,  &
                   planktonic_variable, part*number_plankt_vari, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 15 MPI_Gather(planktonic_variable failed :', ierr
      call qerror(fehler)
   end if
   call MPI_Gather(plankt_vari_vert_p, part*number_plankt_vari*num_lev, MPI_FLOAT,  &
                   plankt_vari_vert, part*number_plankt_vari*num_lev, MPI_FLOAT, 0,mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 16 MPI_Gather(plankt_vari_vert failed :', ierr
      call qerror(fehler)
   end if
   
end subroutine gather_planktkon
!----+-----+----
!> Initialisierung der transportierten Konzentrationen auf 0.0.
!! \n\n
subroutine ini_planktkon0(nk)
   use modell
   implicit none
   integer nk,k,n,as,j,l,ini
   integer ierr
   if (meinrang == 0) then ! prozess 0 only
      print*,'ini_planktkon0'
      number_plankt_point = nk
      ! number_plankt_vari= s. o.
      !------- tiefengemittelte planktische variablen
      do j = 1,number_plankt_vari ! initialise
         write(planktonic_variable_name(j),'(18x)')
      end do
      include "planktonic_variable_name.h"
      
      do j = 1,number_plankt_vari ! zunächst nix ausgeben
         output_plankt(j) = .false.
      end do
      !!!!!!!!! allocate and initialize planktonic_variable
      print*,"ini_planktkon0 going to: allocate (planktonic_variable( "  &
      ,"part*proz_anz,part,proz_anz,number_plankt_point,number_plankt_vari = " &
      ,part*proz_anz,part,proz_anz,number_plankt_point,number_plankt_vari
      allocate (planktonic_variable(number_plankt_vari*part*proz_anz), stat = as )
      !allocate (planktonic_variable(number_plankt_vari*number_plankt_point), stat = as )
      if (as /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate planktonic_variable_ :', as
         call qerror(fehler)
      else
         print*,'planktonic_variable allocated to array size = ',  &
                                                               size(planktonic_variable)
      end if
      do k = 1,number_plankt_point ! i
         do j = 1,number_plankt_vari ! initialisierung aller konzentrationen zunächt auf Null
            planktonic_variable(j+(k-1)*number_plankt_vari) = 0.0 !!!####! 0.0
            !planktonic_variable(71+(k-1)*number_plankt_vari) = real(knoten_zone(k))  !  tracer test annu ####
            !planktonic_variable(72+(k-1)*number_plankt_vari) = 10*real(knoten_zone(k))  !  salz test annu ####
         end do
      end do
      ! ------- tiefenaufgelöst, planktonic variables
      !do j = 1,number_plankt_vari_vert ! initialise
      !   write(plankt_vari_vert_name(j),'(18x)')
      !end do
      !include "plankt_vari_vert_name.h"
      ! allocate and initialize plankt_vari_vert
      allocate (plankt_vari_vert(num_lev*number_plankt_vari*part*proz_anz), stat = as )
      !allocate (plankt_vari_vert(num_lev*number_plankt_vari_vert*number_plankt_point), stat = as )
      if (as /= 0) then
         write(fehler,*)' Rueckgabewert   von   plankt_vari_vert :', as
         call qerror(fehler)
      end if
      plankt_vari_vert=0.0
      do k = 1,number_plankt_point ! initialisierung aller konzentrationen zunächt auf Null
         do j = 1,number_plankt_vari !
            do l = 1,num_lev
               plankt_vari_vert(l+(j-1)*num_lev+(k-1)*number_plankt_vari*num_lev) = 0.0 !!!####! 0.0
               !plankt_vari_vert(i,j,k)=0.0
               !plankt_vari_vert(k)%level(j)%value(i)=0.0
            end do ! i alle
         end do ! j alle levels
      end do
      !do j = 1,number_plankt_vari_vert ! zunächst nix ausgeben
      !   output_plankt_vert(j) = .false.
      !end do
      allocate (point_zone(number_plankt_point), stat = as )
      if (as /= 0) then
         print*,' allocate failed in zonen_parallel point_zone :', as
         call qerror(fehler)
      end if
      select case (hydro_trieb)
         case(1) ! casu-transinfo
            do ini = 1,number_plankt_point
               point_zone(ini) = knoten_zone(ini)
            end do
         case(2) ! Untrim² netCDF
            continue
            !do ini = 1,number_plankt_point
            !   point_zone(ini) = element_zone(ini)
            !end do
         case(3) ! SCHISM netCDF (doch noch von zone.gr3)
            continue
            ! ??? if(.not. zonierung_schism())call verteilung_schism() ???
            !do ini = 1,number_plankt_point
            !   point_zone(ini) = element_zone(ini)
            !end do
            !call qerror('ini_planktkon0: SCHISM zone not yet worked out')
         case default
            call qerror('ini_planktkon0: unknown hydraulic driver')
            
      end select
   end if !! nur prozessor 0
   
end subroutine ini_planktkon0

!----+-----+----
!> harmonize QSim and schism tracer fields
!! hin_her==1 hin zu tr_el
!! hin_her==2 her zu plankt_vari_vert_p
!! \n\n
subroutine schism_tracer_fields(hin_her)
      use modell
      use schism_glbl, only:tr_el,nea2
      implicit none
      integer hin_her, i,ii,j,k,kk,nk
      real realnumlev
      
      if(num_lev<=0)then
         call qerror('schism_tracer_fields: num_lev<=0')
      else
         realnumlev=real(num_lev)
      endif
      
!   allocate (planktonic_variable_p(number_plankt_vari*part), stat = as )
!   allocate (plankt_vari_vert_p(num_lev*number_plankt_vari_vert*part), stat = as )
  !tracer concentration @ prism center; used as temp. storage. tr_el(ntracers,nvrt,nea2) but last index usually
  !is valid up to nea only except for TVD
  !real(rkind),save,allocatable,target :: tr_el(:,:,:) 
  !    allocate(tr_el(ntracers,nvrt,nea2),tr_nd0(ntracers,nvrt,npa),tr_nd(ntracers,nvrt,npa),stat=istat)

      ! 3D
      !!!###do i=1,nea2
      !!!###   ii=(i-1)*number_plankt_vari*num_lev
      !!!###   do k=1,num_lev
      !!!###      kk=(k-1)*number_plankt_vari+ii
      !!!###      do j=1,number_plankt_vari
      !!!###         !plankt_vari_vert_p(k+(j-1)*num_lev+(i-1)*number_plankt_vari*num_lev)=  &
      !!!###         !planktonic_variable_p(j + (i-1) * number_plankt_vari)
      !!!###         !! richtig: tr_el(j,k,i)=plankt_vari_vert_p(k+(j-1)*num_lev+(i-1)*number_plankt_vari*num_lev)
      !!!###         if(hin_her==1)tr_el(j,k,i)=plankt_vari_vert_p(j+kk) !! plankt_vari_vert_p falsch
      !!!###         if(hin_her==2)plankt_vari_vert_p(j+kk) = tr_el(j,k,i)
      !!!###      end do ! all k levels
      !!!###   end do ! all j concentrations
      !!!###end do ! all i elements (discretized points)

      ! 2D
      if(hin_her==1)then ! into tr_el
         do i=1,nea2
            nk=(i-1) * number_plankt_vari
            do j=1,number_plankt_vari
               tr_el(j,1:num_lev,i)=planktonic_variable_p(j + nk)
            end do ! all j concentrations
         end do ! all i elements (discretized points)
      endif ! hin
      if(hin_her==2)then ! from tr_el
         do i=1,nea2
            nk=(i-1) * number_plankt_vari
            do j=1,number_plankt_vari
               planktonic_variable_p(j + nk)=sum(tr_el(j,1:num_lev,i))/realnumlev
            end do ! all j concentrations
         end do ! all i elements (discretized points)
      endif ! her

      if(meinrang==0)print*,hin_her,  &
      'schism_tracer_fields:  ### warning #### full 3D + averaging needs to take layer thickness into account'
end subroutine schism_tracer_fields
