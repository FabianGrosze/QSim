!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

!> Ausgabe der Ergebnisse auf schism-Basis
!! \n\n
!! aus: ausgeben_schism.f95 ; zurück: \ref Ergebnisse
      SUBROUTINE ausgeben_schism(itime)
      use modell     
	  use schism_glbl, only:su2,sv2,tr_el,eta2  &
	 &                     ,npa, nsa, nea, nvrt, ns_global,ne_global,np_global  &
	 &                     ,ielg,iplg,islg
      use schism_msgp, only: myrank,nproc,parallel_abort	  
      use schism_msgp, only: myrank,nproc,parallel_abort	  
      implicit none
      character(len=longname) :: dateiname, systemaufruf, zahl
      integer :: i,j,k,n, istat, ion, errcode
      integer :: sysa, itime
      real :: t, nue_num, nue_elder, reibgesch, sandrauh, wati, dummy, vx, vy, vz
      real :: ubetr, infl, aus, relnumdiff, tr,al,aufenthaltszeit
      real , allocatable , dimension (:) :: output

      if(meinrang.ne.0)call qerror('ausgeben_schism() sollte eigentlich nur von Prozessor 0 aufgerufen werden')
      write(zahl,*)itime
      zahl=adjustl(zahl)
      print*,'ausgeben_schism aufgerufen für t, rechenzeit, itime=',trim(zahl), rechenzeit, itime

      write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'node',trim(zahl),'.vtk'
      if(errcode .ne. 0)call qerror('ausgeben_schism writing filename node_ failed')
      write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
      if(errcode .ne. 0)call qerror('ausgeben_schism writing system call rm -rf dateiname failed')
      call system(systemaufruf)
      ion=106
      open ( unit =ion , file = dateiname, status ='unknown', action ='write ', iostat = istat )
      if(istat.ne. 0) then
         write(fehler,*)'open_error node_.vtk'
         call qerror(fehler)
      end if ! open_error.ne.0

      !! mesh
      call mesh_output(ion)

      !! hydraulic
      write(ion,'(A)')'SCALARS WSP float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,number_plankt_point
         write(ion,'(f27.6)') p(n)
         !write(ion,'(f27.6)') rb_hydraul(3+(n-1)*number_rb_hydraul)
      end do
      write(ion,'(A)')'SCALARS tief float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,number_plankt_point
         write(ion,'(f27.6)') rb_hydraul(2+(n-1)*number_rb_hydraul)
      end do ! alle Knoten
      write(ion,'(A)')'SCALARS vel_norm float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,number_plankt_point
         write(ion,'(f27.6)') u(n)
      end do 
      write(ion,'(A)')'VECTORS vel float'
      !write(ion,'(A)')'LOOKUP_TABLE default' ! nicht bei Vektoren ??
      do n=1,number_plankt_point
         !   vel_x(j)=u(j)
         !   vel_y(j)=dir(j) 
         !vx=u(n)*cos(dir(n))*(-1.0)
         !vy=u(n)*sin(dir(n))*(-1.0)
         vz=0.0
         write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') vel_x(n), vel_y(n), vz
      end do ! alle Knoten

      write(ion,'(A)')'SCALARS rang float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,number_plankt_point
         write(ion,'(f27.6)') real( knoten_rang(n) )
      end do ! alle Knoten

      !! planktonic variables
      do j=1,number_plankt_vari ! all depth averaged planktonic variables
         if(output_plankt(j))then ! output requested
            write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(planktonic_variable_name(j))),' float 1'
            write(ion,'(A)')'LOOKUP_TABLE default'
            do n=1,number_plankt_point ! all nodes
               aus=planktonic_variable(j+(n-1)*number_plankt_vari)
               write(ion,'(f27.6)') aus
            end do ! all nodes
         end if ! output requested
      end do ! all planktonic_variable
      close (ion)
      print*,'node output ausgeben_schism done'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  
	  !!! Element sides, edges=kanten
      write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'kanten_',trim(zahl),'.vtk'
      if(errcode .ne. 0)call qerror('ausgeben_schism writing filename kanten_ failed')
      write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
      if(errcode .ne. 0)call qerror('ausgeben_schism writing system call rm -rf dateiname kanten_ failed')
      call system(systemaufruf)
      ion=106
      open ( unit =ion , file = dateiname, status ='unknown', action ='write ', iostat = istat )
      if(istat.ne.0) then
         write(fehler,*)'open_error kanten.vtk'
         call qerror(fehler)
      end if ! istat.ne.0

      write(ion,'(A)')'# vtk DataFile Version 3.0'
      write(ion,'(A)')'Simlation QSim3D untrim'
      write(ion,'(A)')'ASCII'
      write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'

      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',kantenanzahl, ' float'
      do n=1,kantenanzahl
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') 0.5*(knoten_x(top_node(n))+knoten_x(bottom_node(n)))  &
                                             , 0.5*(knoten_y(top_node(n))+knoten_y(bottom_node(n))), 0.0
      end do ! alle kanten

      write(ion,'(A)')' ' 
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl, kantenanzahl*2
      do n=1,kantenanzahl
         write(ion,'(A,2x,I12)')'1',n-1
      end do ! alle kanten

      write(ion,'(A)')' ' ! vtk-vertex
      write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl
      do n=1,kantenanzahl
         write(ion,'(A)')'1'
      end do ! alle kanten
	  
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'POINT_DATA ', kantenanzahl
      write(ion,'(A)')'SCALARS flux_area float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,kantenanzahl
         write(ion,'(f27.6)') ed_area(n)
      end do ! alle kanten
      write(ion,'(A)')'SCALARS volume_flux float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n=1,kantenanzahl
         write(ion,'(f27.6)') ed_flux(n)
      end do ! alle kanten
      write(ion,'(A)')'VECTORS ed_vel float'
      vz=0.0
      do n=1,kantenanzahl
         write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') ed_vel_x(n),ed_vel_y(n),vz
      end do ! alle kanten
	  	  	 
      close (ion)
      print*,meinrang,myrank,'edge output ausgeben_schism not yet done'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  
      RETURN
      END subroutine ausgeben_schism