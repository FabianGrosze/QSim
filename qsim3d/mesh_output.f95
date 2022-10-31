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

!----+-----+----+-----+----+-----+----+-----+----+-----+----
subroutine mesh_output(ioni)
   use modell
   implicit none
   character(len = longname) :: dateiname,systemaufruf
   integer ion,ioni,n,igr3,ielem
   integer io_error
   !print*,'mesh_output: starting'
   
   if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten
      !-------------------------------------------------------------------------------------------- nodes
      if(ioni==0)then
         write(dateiname,'(4A)',iostat = io_error)trim(modellverzeichnis),'mesh_node.vtk'
         if (io_error /= 0)call qerror('mesh_output writing filename mesh_node failed')
         write(systemaufruf,'(2A)',iostat = io_error)'rm -rf ',trim(dateiname)
         if (io_error /= 0)call qerror('mesh_output writing system call rm -rf dateiname mesh_node failed')
         call system(systemaufruf)
         ion = 106
         open ( unit = ion , file = dateiname, status = 'new', action = 'write ', iostat = io_error )
         if (io_error /= 0) then
            write(fehler,*)'io_error mesh_node.vtk'
            call qerror(fehler)
         end if ! io_error.ne.0
      endif !ioni==0

      !----------------------------------------------------------------- .vtk
      write(ion,'(A)')'# vtk DataFile Version 3.0'
      write(ion,'(A)')'Simlation QSim3D'
      write(ion,'(A)')'ASCII'
      !write(ion,'(A)')'DATASET POLYDATA'
      write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
      !
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
      do n = 1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
      end do ! alle Knoten
      write(ion,'(A)')' '
   
      summ_ne = 0 ! needed by vtk output
      do n = 1,n_elemente
         if(cornernumber(n)/=3 .and. cornernumber(n)/=4) then
            print*,meinrang,'mesh_output: cornernumber=',cornernumber(n),n
            call qerror('mesh_output: cornernumber wrong')
         endif
         summ_ne = summ_ne+cornernumber(n)+1
      end do ! all Elements
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', n_elemente, summ_ne
      do n = 1,n_elemente ! alle Elemente
         if (cornernumber(n) == 3) then
            write(ion,'(4(I8,2x))') &
                              cornernumber(n),elementnodes(n,1)-1,elementnodes(n,2)-1,elementnodes(n,3)-1
         end if
         if (cornernumber(n) == 4) then
            write(ion,'(5(I8,2x))') &
                              cornernumber(n),elementnodes(n,1)-1,elementnodes(n,2)-1,elementnodes(n,3)-1,elementnodes(n,4)-1
         end if
      end do ! alle Elemente
      write(ion,'(A)')' '
   
      write(ion,'(A,2x,I12)')'CELL_TYPES ', n_elemente
      do n = 1,n_elemente ! alle Elemente
         if (cornernumber(n) == 3)write(ion,'(A)') '5'
         if (cornernumber(n) == 4)write(ion,'(A)') '9'
      end do ! alle Elemente
      write(ion,'(A)')' '
   
      write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
      write(ion,'(A)')'SCALARS Gelaendehoehe float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,knotenanzahl2D
         write(ion,'(f27.6)') knoten_z(n)
      end do ! alle Knoten
      write(ion,'(A)')'SCALARS knoten_zone float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,knotenanzahl2D
         write(ion,'(f27.6)') real( knoten_zone(n) )
      end do ! alle Knoten
      write(ion,'(A)')'SCALARS knoten_rand float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,knotenanzahl2D
         write(ion,'(f27.6)') real(knoten_rand(n))
      end do ! alle Knoten
      write(ion,'(A)')'SCALARS knoten_rang float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,knotenanzahl2D
         write(ion,'(f27.6)') real(knoten_rang(n))
      end do ! alle Knoten
   
      print*,'mesh_output:mesh node output done'
      if(ioni==0)close (ion)

      if(ioni==0)then
      !----------------------------------------------------------------- .gr3
         write(dateiname,'(2A)')trim(modellverzeichnis),'mesh.gr3'
         igr3 = 107
         open ( unit = igr3 , file = dateiname, status = 'unknown', action = 'write ', iostat = io_error )
         if (io_error /= 0) then
            print*,'mesh.gr3, io_error = ',io_error
            close (igr3)
            return
         end if ! io_error.ne.0
   
         write(igr3,'(A,2x,A)') 'Grid written by QSim3D',modellverzeichnis
         write(igr3,*)n_elemente, knotenanzahl2D
   
         do n = 1,knotenanzahl2D
            ! write(igr3,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
            write(igr3,*)n, knoten_x(n), knoten_y(n), knoten_z(n)
         end do ! alle Knoten
   
         do n = 1,n_elemente ! alle Elemente
            if (cornernumber(n) == 3) then
               write(igr3,*) &
                           n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
            end if
            if (cornernumber(n) == 4) then
               write(igr3,*) &
                           n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
            end if
         end do ! alle Elemente
   
         close (igr3)
         print*,'written mesh.gr3'
         !----------------------------------------------------------------- element.vtk
         write(dateiname,'(2A)')trim(modellverzeichnis),'elemente.vtk'
         ielem = 108
         write(systemaufruf,'(2A)',iostat = io_error)'rm -rf ',trim(dateiname)
         if (io_error /= 0)call qerror('ausgeben_untrim writing system call rm -rf dateiname failed')
         call system(systemaufruf)
         open ( unit = ielem , file = dateiname, status = 'unknown', action = 'write ', iostat = io_error )
         if (io_error /= 0) then
            write(fehler,*)'io_error elemente_.vtk'
            call qerror(fehler)
         end if ! io_error.ne.0
         write(ielem,'(A)')'# vtk DataFile Version 3.0'
         write(ielem,'(A)')'Simlation QSim3D untrim'
         write(ielem,'(A)')'ASCII'
         write(ielem,'(A)')'DATASET UNSTRUCTURED_GRID'
         write(ielem,'(A)')' '
         write(ielem,'(A,2x,I12,2x,A)')'POINTS ',n_elemente, ' float'
         do n = 1,n_elemente
            write(ielem,'(f17.5,2x,f17.5,2x,f8.3)') element_x(n), element_y(n), 0.0
         end do ! alle Knoten
         write(ielem,'(A)')' '
         write(ielem,'(A,2x,I12,2x,I12)')'CELLS ', n_elemente, n_elemente*2
         do n = 1,n_elemente
            write(ielem,'(A,2x,I12)')'1',n
         end do ! alle Knoten
         write(ielem,'(A)')' ' ! vtk-vertex
         write(ielem,'(A,2x,I12)')'CELL_TYPES ', n_elemente
         do n = 1,n_elemente
            write(ielem,'(A)')'1'
         end do ! alle Knoten
         write(ielem,'(A)')' '
         write(ielem,'(A,2x,I12)')'POINT_DATA ', n_elemente
         write(ielem,'(A)')'SCALARS Rang float 1'
         write(ielem,'(A)')'LOOKUP_TABLE default'
         do n = 1,n_elemente
            write(ielem,'(f27.6)') real( element_rang(n) )
         end do ! all elements
         write(ielem,'(A)')'SCALARS element_rand float 1'
         write(ielem,'(A)')'LOOKUP_TABLE default'
         do n = 1,n_elemente
            write(ielem,'(6x,f27.6)') real( element_rand(n) )
         end do
         write(ielem,'(A)')'SCALARS element_zone float 1'
         write(ielem,'(A)')'LOOKUP_TABLE default'
         do n = 1,n_elemente
            write(ielem,'(6x,f27.6)') real( element_zone(n) )
         end do
         close (ielem)
         print*,'written elemente.vtk'

      endif !ioni==0
      print*,'mesh_output: finished'
   end if ! nur Prozessor 0
   return
end subroutine mesh_output