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
!> Ausgabe der Ergebnisse auf schism-Basis
!! \n\n
!! aus: ausgeben_schism.f95 ; zurück: \ref lnk_ergebnisausgabe
subroutine ausgeben_schism(itime)
   use modell
   use schism_glbl, only:su2,sv2,tr_el,eta2  &
   ,npa, nsa, nea, nvrt, ns_global,ne_global,np_global  &
   ,ielg,iplg,islg
   use schism_msgp, only: myrank,nproc,parallel_abort
   implicit none
   character(len = longname) :: dateiname, systemaufruf, zahl
   integer :: i,j,k,n, istat, ion, errcode, open_error
   integer :: sysa, itime
   real :: t, nue_num, nue_elder, reibgesch, sandrauh, wati, dummy, vx, vy, vz
   real :: ubetr, infl, aus, relnumdiff, tr,al,aufenthaltszeit
   real , allocatable , dimension (:) :: output
   
if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten
   write(zahl,*)itime
   zahl = adjustl(zahl)
   print*,'ausgeben_schism aufgerufen für t, rechenzeit, itime = ',trim(zahl), rechenzeit, itime
   
!####### nodes ##########################################################################################   
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'nodes_',trim(zahl),'.vtk'
   if (errcode /= 0)call qerror('ausgeben_schism writing filename node_ failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('ausgeben_schism writing system call rm -rf dateiname failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error node_.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
   !! mesh
   call mesh_output(ion)
   !! hydraulic
   write(ion,'(A)')'SCALARS WSP float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') p(n)
   end do
   write(ion,'(A)')'SCALARS vel float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') u(n)
   end do
   write(ion,'(A)')'SCALARS bathymetry float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') knoten_z(n)
   end do
   write(ion,'(A)')'SCALARS dry float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') real(knoten_trocken(n))
   end do

   close (ion)
   print*,meinrang,myrank,'node output ausgeben_schism done'
   
!####### elements ##########################################################################################   
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'elements_',trim(zahl),'.vtk'
   if (errcode /= 0)call qerror('ausgeben_schism writing filename elemente_ failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('ausgeben_schism writing system call rm -rf dateiname failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error elemente_.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'Simlation QSim3D SCHISM'
   write(ion,'(A)')'ASCII'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',number_plankt_point, ' float'
   do n = 1,number_plankt_point
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') element_x(n), element_y(n), 0.0
   end do ! all elements
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,I12)')'CELLS ', number_plankt_point, number_plankt_point*2
   do n = 1,number_plankt_point
      write(ion,'(A,2x,I12)')'1',n
   end do ! all elements
   write(ion,'(A)')' ' ! vtk-vertex
   write(ion,'(A,2x,I12)')'CELL_TYPES ', number_plankt_point
   do n = 1,number_plankt_point
      write(ion,'(A)')'1'
   end do ! all elements
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', number_plankt_point
   
   write(ion,'(A)')'SCALARS Rang float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') real(element_rang(n))
   end do ! all elements
   write(ion,'(A)')'SCALARS zone float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') real(element_zone(n))
   end do ! all elements
   write(ion,'(A)')'SCALARS Rand float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') real(element_rand(n))
   end do ! all elements
   write(ion,'(A)')'SCALARS WSP_rb3 float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') rb_hydraul(3+(n-1)*number_rb_hydraul)
   end do ! all elements
   write(ion,'(A)')'SCALARS depth_rb2 float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') rb_hydraul(2+(n-1)*number_rb_hydraul)
   end do ! all elements
   write(ion,'(A)')'SCALARS vel_rb1 float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') rb_hydraul(1+(n-1)*number_rb_hydraul)
   end do ! all elements
   write(ion,'(A)')'SCALARS dry float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') real(element_trocken(n))
   end do ! all elements
   close (ion)
   print*,meinrang,myrank,'elements output ausgeben_schism done'
   
end if ! meinrang==0
   return !!!##### prematurely ####
   
   
if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten

!####### kanten edges sides ##########################################################################################   
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'kanten_',trim(zahl),'.vtk'
   if (errcode /= 0)call qerror('ausgeben_untrim writing filename kanten_ failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('ausgeben_untrim writing system call rm -rf dateiname kanten_ failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error kanten_.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'Simlation QSim3D SCHISM'
   write(ion,'(A)')'ASCII'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',kantenanzahl, ' float'
   do n = 1,kantenanzahl
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') 0.5*(knoten_x(top_node(n))+knoten_x(bottom_node(n)))  &
                                           , 0.5*(knoten_y(top_node(n))+knoten_y(bottom_node(n))), 0.0
   end do ! alle kanten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl, kantenanzahl*2
   do n = 1,kantenanzahl
      write(ion,'(A,2x,I12)')'1',n
   end do ! alle kanten
   write(ion,'(A)')' ' ! vtk-vertex
   write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl
   do n = 1,kantenanzahl
      write(ion,'(A)')'1'
   end do ! alle kanten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', kantenanzahl
   write(ion,'(A)')'SCALARS vel_mag float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,kantenanzahl
      write(ion,'(f27.6)') sqrt(ed_vel_x(n)**2 + ed_vel_y(n)**2)
   end do ! alle kanten
   write(ion,'(A)')'VECTORS vel float'
   !write(ion,'(A)')'LOOKUP_TABLE default' ! nicht bei Vektoren ??
   do n = 1,kantenanzahl
      write(ion,'(6x, f11.6, 2x, f11.6, A)') ed_vel_x(n), ed_vel_y(n),'   0.0'
   end do ! alle Knoten
   close (ion)
   print*,meinrang,myrank,'kanten output ausgeben_schism done'

   return !!!#########

!####### Element sides, edges=kanten ##########################################################################################   
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'kanten_',trim(zahl),'.vtk'
   if (errcode /= 0)call qerror('ausgeben_schism writing filename kanten_ failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('ausgeben_schism writing system call rm -rf dateiname kanten_ failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = istat )
   if (istat /= 0) then
      write(fehler,*)'open_error kanten.vtk'
      call qerror(fehler)
   end if ! istat.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'Simlation QSim3D untrim'
   write(ion,'(A)')'ASCII'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',kantenanzahl, ' float'

   
   write(ion,'(A)')'SCALARS tief float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') rb_hydraul(2+(n-1)*number_rb_hydraul)
   end do ! alle Knoten
   write(ion,'(A)')'SCALARS vel_norm float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') u(n)
   end do
   write(ion,'(A)')'VECTORS vel float'
   !write(ion,'(A)')'LOOKUP_TABLE default' ! nicht bei Vektoren ??
   do n = 1,knotenanzahl2D
      !   vel_x(j)=u(j)
      !   vel_y(j)=dir(j)
      !vx=u(n)*cos(dir(n))*(-1.0)
      !vy=u(n)*sin(dir(n))*(-1.0)
      vz = 0.0
      write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') vel_x(n), vel_y(n), vz
   end do ! alle Knoten
   write(ion,'(A)')'SCALARS rang float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') real( knoten_rang(n) )
   end do ! alle Knoten
   write(ion,'(A)')'SCALARS rand float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') real( knoten_rand(n) )
   end do ! alle Knoten
   write(ion,'(A)')'SCALARS zone float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') real( knoten_zone(n) )
   end do ! alle Knoten
   close (ion)
   print*,'node output ausgeben_schism done'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   close (ion)
   print*,meinrang,myrank,'edge output ausgeben_schism not yet done'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   return

!####### Element sides, edges=kanten ##########################################################################################   
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'kanten_',trim(zahl),'.vtk'
   if (errcode /= 0)call qerror('ausgeben_schism writing filename kanten_ failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('ausgeben_schism writing system call rm -rf dateiname kanten_ failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = istat )
   if (istat /= 0) then
      write(fehler,*)'open_error kanten.vtk'
      call qerror(fehler)
   end if ! istat.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'Simlation QSim3D untrim'
   write(ion,'(A)')'ASCII'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',kantenanzahl, ' float'
   do n = 1,kantenanzahl
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') 0.5*(knoten_x(top_node(n))+knoten_x(bottom_node(n)))  &
                                           , 0.5*(knoten_y(top_node(n))+knoten_y(bottom_node(n))), 0.0
   end do ! alle kanten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl, kantenanzahl*2
   do n = 1,kantenanzahl
      write(ion,'(A,2x,I12)')'1',n-1
   end do ! alle kanten
   write(ion,'(A)')' ' ! vtk-vertex
   write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl
   do n = 1,kantenanzahl
      write(ion,'(A)')'1'
   end do ! alle kanten
   
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', kantenanzahl
   write(ion,'(A)')'SCALARS flux_area float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,kantenanzahl
      write(ion,'(f27.6)') ed_area(n)
   end do ! alle kanten
   write(ion,'(A)')'SCALARS volume_flux float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,kantenanzahl
      write(ion,'(f27.6)') ed_flux(n)
   end do ! alle kanten
   write(ion,'(A)')'VECTORS ed_vel float'
   vz = 0.0
   do n = 1,kantenanzahl
      write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') ed_vel_x(n),ed_vel_y(n),vz
   end do ! alle kanten
   close (ion)

   print*,meinrang,myrank,'edge output ausgeben_schism not yet done'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end if ! meinrang==0

return
end subroutine ausgeben_schism
