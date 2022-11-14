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

subroutine holen_trans_untrim(nt)
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   integer   :: start3(3), count3(3)
   integer   :: start2(2), count2(2)
   integer   :: nt, n,j,k, varid, alloc_status
   real      :: c
   !---------------elemente
   print*,'holen_trans_untrim: zeitpunkt,nt = ',transinfo_zeit(transinfo_zuord(nt)),nt
   start3 = (/ 1, 1, nt /)
   count3 = (/ n_elemente, 1, 1 /)
   !float Mesh2_face_Wasservolumen_2d(nMesh2_data_time, nMesh2_layer_2d, nMesh2_face) ;
   call check_err( nf_inq_varid(ncid,'Mesh2_face_Wasservolumen_2d', varid) )
   call check_err( nf90_get_var(ncid, varid, el_vol, start3, count3 ) )
   do n = 1,n_elemente !
      if ((el_vol(n) <= 0.0) .or. (el_vol(n) > 1.e+30)) el_vol(n) = 0.0
   end do ! alle n elemente
   !print*,'stofftransport_untrim: got Mesh2_face_Wasservolumen_2d'
   start2 = (/ 1, nt /)
   count2 = (/ n_elemente, 1 /)
   call check_err( nf_inq_varid(ncid,'Mesh2_face_Wasserstand_2d', varid) )
   call check_err( nf90_get_var(ncid, varid, p, start2, count2 ) )
   do n = 1,n_elemente !
      if ( abs(p(n)) > 1.e+30)p(n) = -999.9
   end do ! alle n elemente
   !print*,'stofftransport_untrim: got Mesh2_face_Wasserstand_2d'
   call check_err( nf_inq_varid(ncid,'Mesh2_face_Wasserflaeche_2d', varid) )
   call check_err( nf90_get_var(ncid, varid, el_area, start2, count2 ) )
   do n = 1,n_elemente !
      if ((el_area(n) <= 0.0) .or. (el_area(n) > 1.e+30)) el_area(n) = 0.0
   end do ! alle n elemente
   !print*,'stofftransport_untrim: got Mesh2_face_Wasserflaeche_2d'
   !---------------kanten
   ed_vel_x(:) = 0.0
   ed_vel_y(:) = 0.0
   start3 = (/ 1, 1, nt /)
   count3 = (/ kantenanzahl, 1, 1 /)
   !float Mesh2_edge_Durchflussflaeche_2d(nMesh2_data_time, nMesh2_layer_2d, nMesh2_edge) ;
   call check_err( nf_inq_varid(ncid,'Mesh2_edge_Durchflussflaeche_2d', varid) )
   !call check_err( nf_inq_varid(ncid,'Mesh2_edge_mit_hor_durchstroemte_Kantenflaeche_2d', varid) )
   call check_err( nf90_get_var(ncid, varid, ed_area, start3, count3 ) )
   !float Mesh2_edge_Stroemungsgeschwindigkeit_x_R_2d(nMesh2_data_time, nMesh2_layer_2d, nMesh2_edge) ;
   call check_err( nf_inq_varid(ncid,'Mesh2_edge_Stroemungsgeschwindigkeit_x_R_2d', varid) )
   call check_err( nf90_get_var(ncid, varid, ed_vel_x, start3, count3 ) )
   call check_err( nf_inq_varid(ncid,'Mesh2_edge_Stroemungsgeschwindigkeit_y_R_2d', varid) )
   call check_err( nf90_get_var(ncid, varid, ed_vel_y, start3, count3 ) )
   !call check_err( nf_inq_varid(ncid,'Mesh2_edge_hor_Wassertransport_Kantenflaeche_2d', varid) )
   !call check_err( nf90_get_var(ncid, varid, ed_flux, start3, count3 ) )
   !print*,"Mesh2_edge_Stroemungsgeschwindigkeit"
   do n = 1,kantenanzahl
      !Mesh2_edge_Durchflussflaeche_2d:_FillValue = 1.e+31f ;
      if ((ed_area(n) <= 0.0) .or. (ed_area(n) > 1.e+30)) then ! Kante trocken ?
         ed_area(n) = 0.0
         ed_vel_x(n) = 0.0
         ed_vel_y(n) = 0.0
      end if ! Kante trocken
      !print*,n,ed_vel_x(n),ed_vel_y(n)
      ! Mesh2_edge_Stroemungsgeschwindigkeit_x_R_2d:_FillValue = 1.e+31f ;
      if (ed_vel_x(n) > 100.0)ed_vel_x(n) = 0.0
      if (ed_vel_y(n) > 100.0)ed_vel_y(n) = 0.0
   end do ! alle n kanten
   do n = 1,n_elemente ! mean velocity magnitude in element
      u(n) = 0.0
      do k = 1,cornernumber(n)
         u(n) = u(n)+ (ed_vel_x(elementedges(n,k))**2 + ed_vel_y(elementedges(n,k))**2)**0.5
         if (u(n) > huge(u(n))) then
            print*,"ed_vel_x,y,elementedges,n,k = ",  &
                  ed_vel_x(elementedges(n,k)),ed_vel_y(elementedges(n,k)),elementedges(n,k),n,k
            call qerror("holen_trans_untrim u infinity")
         endif
      end do ! alle k Kanten im Element
      c = real(cornernumber(n))
      if (c <= 0.0)call qerror("cornernumber(n) ist null ???")
      u(n) = u(n)/c
      inflow(n) = .false.
      if (element_rand(n) > 0) inflow(n) = .true.
   end do ! alle n Elemente
   print*,'### stofftransport_untrim: all boundaries inflow ###'
   do j = 1,number_plankt_point ! all j elements
      rb_hydraul(1+(j-1)*number_rb_hydraul) = u(j)
      rb_hydraul(2+(j-1)*number_rb_hydraul) = 0.0 !  Tiefe zunächst null
      if ((el_area(j) > 0.0) .and. (el_vol(j) > 0.0)) &
          rb_hydraul(2+(j-1)*number_rb_hydraul) = el_vol(j)/el_area(j) ! Mittlere Tiefe im Element
      rb_hydraul(3+(j-1)*number_rb_hydraul) = p(j)
      benthic_distribution(44+(j-1)*number_benth_distr) = 0.1      ! ks ######### jetzt anders zone()%reib
      benthic_distribution(45+(j-1)*number_benth_distr) = 0.1*u(j) ! utau
   end do ! all j elements
   print*,'### stofftransport_untrim: ks and utau, only first guess ###'
   return
end subroutine holen_trans_untrim