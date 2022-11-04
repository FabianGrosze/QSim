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
!> Ausgabe der Ergebnisse auf untrim-Basis
!! \n\n
!! aus: ausgeben_untrim.f95 ; zurück: \ref lnk_ergebnisausgabe
subroutine ausgeben_untrim(itime)
   use modell
   implicit none
   character(len = longname) :: dateiname, systemaufruf, zahl
   integer :: i,j,k,n, open_error, ion, system_error, string_read_error, alloc_status
   integer :: sysa, itime, errcode
   real :: t, nue_num, nue_elder, reibgesch, sandrauh, wati, dummy
   real :: ubetr, infl, aus, nx, ny,nz, relnumdiff, tr,al,aufenthaltszeit
   real , allocatable , dimension (:) :: output
   if (meinrang /= 0)call qerror('ausgeben_untrim() sollte eigentlich nur von Prozessor 0 aufgerufen werden')
   write(zahl,*)itime
   zahl = adjustl(zahl)
   print*,'ausgeben_untrim aufgerufen für t, rechenzeit = ',trim(zahl), rechenzeit
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'elemente_',trim(zahl),'.vtk'
   if (errcode /= 0)call qerror('ausgeben_untrim writing filename elemente_ failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('ausgeben_untrim writing system call rm -rf dateiname failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error elemente_.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'Simlation QSim3D untrim'
   write(ion,'(A)')'ASCII'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',number_plankt_point, ' float'
   do n = 1,number_plankt_point
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') element_x(n), element_y(n), 0.0
   end do ! alle Knoten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,I12)')'CELLS ', number_plankt_point, number_plankt_point*2
   do n = 1,number_plankt_point
      write(ion,'(A,2x,I12)')'1',n
   end do ! alle Knoten
   write(ion,'(A)')' ' ! vtk-vertex
   write(ion,'(A,2x,I12)')'CELL_TYPES ', number_plankt_point
   do n = 1,number_plankt_point
      write(ion,'(A)')'1'
   end do ! alle Knoten
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', number_plankt_point
   write(ion,'(A)')'SCALARS Rang float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') real(n/part)
   end do ! all elements
   write(ion,'(A)')'SCALARS tiefe_rb2 float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') rb_hydraul(2+(n-1)*number_rb_hydraul)
   end do ! alle Knoten
   write(ion,'(A)')'SCALARS WSP float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') p(n)
      !write(ion,'(f27.6)') rb_hydraul(3+(n-1)*number_rb_hydraul)
   end do ! all elements
   write(ion,'(A)')'SCALARS vel float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') u(n)
   end do
   write(ion,'(A)')'SCALARS dt_max float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      dummy = 1.0-wicht((n-1)*5+1)
      if (dummy > 0.0) then
         dummy = real(deltat)/dummy
         write(ion,'(f27.6)') dummy
      else
         write(ion,'(f27.6)') 0.0
      end if
   end do ! alle n Elemente
   write(ion,'(A)')'SCALARS Cu float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(f27.6)') cu(n)
   end do ! alle n Elemente
   !dummy=0.0
   ! im Element vorhandenes Volumen geteilt durch den Ausfluss = Zeit innerhalb der das Elementvolumen ganz aus ihm entschwindet
   !if(wicht((n-1)*5+1).gt. 0.0) dummy=el_vol(n)/wicht((n-1)*5+1)
   !write(ion,'(f27.6)') dummy
   
   !write(ion,'(A)')'SCALARS Randnummer float 1'
   !write(ion,'(A)')'LOOKUP_TABLE default'
   !do n=1,number_plankt_point
   !   write(ion,'(f27.6)') real( rabe(element_rand(n))%nr_rb )
   !end do
   write(ion,'(A)')'SCALARS element_rand float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(6x,f27.6)') real( element_rand(n) )
   end do
   write(ion,'(A)')'SCALARS element_zone float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(6x,f27.6)') real( element_zone(n) )
   end do
   write(ion,'(A)')'SCALARS point_zone float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      write(ion,'(6x,f27.6)') real( point_zone(n) )
   end do
   write(ion,'(A)')'SCALARS rau float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      !write(ion,'(6x,f27.6)') zone(element_zone(n))%reib
      write(ion,'(6x,f27.6)') zone(point_zone(n))%reib
   end do
   write(ion,'(A)')'SCALARS strickler float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      !write(ion,'(6x,f27.6)') strickler( zone(element_zone(n))%reib , rb_hydraul(2+(n-1)*number_rb_hydraul) )
      write(ion,'(6x,f27.6)') strickler( zone(point_zone(n))%reib , rb_hydraul(2+(n-1)*number_rb_hydraul) )
   end do
   write(ion,'(A)')'SCALARS inflow float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,number_plankt_point
      if (inflow(n)) then
         write(ion,'(A)') '1.0'
      else
         write(ion,'(A)') '0.0'
      endif
   end do
   if (nur_alter) then ! Aufenthaltszeit in Tagen ! Altersberechnung wie in Shen&Wang 2007
      write(ion,'(A)')'SCALARS age_arith float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,number_plankt_point
         tr = planktonic_variable(71+(n-1)*number_plankt_vari)
         al = planktonic_variable(74+(n-1)*number_plankt_vari)
         aus = 0.0
         if (tr > 0.00001 ) aus = al / tr
         write(ion,'(6x,f27.6)') aus
      end do ! alle Knoten
   end if !nur_alter
   
   ! planktische, transportierte Konzentrationen entsprechend ausgabeflag
   do j = 1,number_plankt_vari ! alle tiefengemittelten
      if (output_plankt(j)) then ! zur ausgabe vorgesehen
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(planktonic_variable_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,number_plankt_point ! alle Knoten
            aus = planktonic_variable(j+(n-1)*number_plankt_vari)
            write(ion,'(f27.6)') aus
         end do ! alle Elemente
         !   print*,'ausgeben_untrim: done output for ',ADJUSTL(trim(planktonic_variable_name(j)))
         !else
         !   print*,'ausgeben_untrim: no output for ',ADJUSTL(trim(planktonic_variable_name(j)))
      end if ! zur ausgabe vorgesehen
   end do ! alle planktonic_variable
   do j = 1,number_plankt_vari_vert ! alle tiefenaufgelösten z.Z. nur level 1
      if (output_plankt_vert(j)) then ! zur ausgabe vorgesehen
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(plankt_vari_vert_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,number_plankt_point ! alle Knoten
            aus = plankt_vari_vert(1+(j-1)*num_lev+(n-1)*number_plankt_vari_vert*num_lev)
            write(ion,'(f27.6)') aus
         end do ! alle Knoten
      end if ! zur ausgabe vorgesehen
   end do ! done all plankt_vari_vert
   ! Übergabe_Konzentrationen entsprechend ausgabeflag
   do j = 1,number_trans_quant ! alle tiefengemittelten
      if (output_trans_quant(j)) then ! zur ausgabe vorgesehen
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(trans_quant_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,number_plankt_point
            write(ion,'(f27.6)') transfer_quantity(j+(n-1)*number_trans_quant)
         end do ! alle Knoten
      end if ! zur ausgabe vorgesehen
   end do ! alle transkon
   do j = 1,number_trans_quant_vert ! alle tiefenaufgelösten z.Z. nur level 1
      if (output_trans_quant_vert(j)) then ! zur ausgabe vorgesehen
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(trans_quant_vert_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,number_plankt_point
            aus = trans_quant_vert(1+(j-1)*num_lev_trans+(n-1)*num_lev_trans*number_trans_quant_vert)
            write(ion,'(f27.6)') aus
         end do ! alle Knoten
      end if ! zur ausgabe vorgesehen
   end do ! done all vertically distributed transfer quantities
   ! benthic distributions according to output flag
   if(number_benthic_points .ne. number_plankt_point)call qerror('number_benthic_points .ne. number_plankt_point') 
   do j = 1,number_benth_distr ! all benthic distributions
      if (output_benth_distr(j)) then ! flagged?
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(benth_distr_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,number_plankt_point ! all nodes number_benthic_points
            write(ion,'(f27.6)') benthic_distribution(j+(n-1)*number_benth_distr)
         end do ! all nodes
      end if ! flagged
   end do ! all benthic distributions
   close (ion) ! close vtk
   
   print*,'elemente_ ausgeben_untrim gemacht'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'kanten_',trim(zahl),'.vtk'
   if (errcode /= 0)call qerror('ausgeben_untrim writing filename kanten_ failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('ausgeben_untrim writing system call rm -rf dateiname kanten_ failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error elemente_.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
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
      write(ion,'(A,2x,I12)')'1',n
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
   write(ion,'(A)')'SCALARS boundary float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,kantenanzahl
      write(ion,'(f27.6)') real(boundary_number(n))
   end do ! alle kanten
   write(ion,'(A)')'VECTORS vel float'
   !write(ion,'(A)')'LOOKUP_TABLE default' ! nicht bei Vektoren ??
   do n = 1,kantenanzahl
      write(ion,'(6x, f11.6, 2x, f11.6, A)') ed_vel_x(n), ed_vel_y(n),'   0.0'
   end do ! alle Knoten
   write(ion,'(A)')'VECTORS normal float'
   !write(ion,'(A)')'LOOKUP_TABLE default' ! nicht bei Vektoren ??
   do n = 1,kantenanzahl
      write(ion,'(6x, f11.6, 2x, f11.6, A)') edge_normal_x(n), edge_normal_y(n),'   0.0'
   end do ! alle Knoten
   close (ion)
   print*,'kanten_ ausgeben_untrim gemacht'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(dateiname,'(4A)',iostat = errcode)trim(modellverzeichnis),'knoten_',trim(zahl),'.vtk'
   if (errcode /= 0)call qerror('ausgeben_untrim writing filename knoten_ failed')
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   if (errcode /= 0)call qerror('ausgeben_untrim writing system call rm -rf dateiname knoten_ failed')
   call system(systemaufruf)
   ion = 106
   open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error knoten_.vtk'
      call qerror(fehler)
   end if ! open_error.ne.0
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'Simlation QSim3D untrim'
   write(ion,'(A)')'ASCII'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
   do n = 1,knotenanzahl2D
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), 0.0
   end do ! alle knoten
   write(ion,'(A)')' '
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
   write(ion,'(A)')'SCALARS knot_ele float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') real(knot_ele(n))
   end do ! alle Knoten
   write(ion,'(A)')'SCALARS knoten_rand float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(6x,f27.6)') real( knoten_rand(n) )
   end do
   write(ion,'(A)')'SCALARS knoten_zone float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(6x,f27.6)') real( knoten_zone(n) )
   end do
   allocate (output(knotenanzahl2D), stat = alloc_status )
   ! planktische, transportierte Konzentrationen entsprechend ausgabeflag
   do j = 1,number_plankt_vari ! alle tiefengemittelten
      if (output_plankt(j)) then ! zur ausgabe vorgesehen
         do n = 1,knotenanzahl2D
            output(n) = 0.0
         end do ! alle Knoten
         do n = 1,n_elemente ! alle Elemente
            do k = 1,cornernumber(n)
               output(elementnodes(n,k)) = output(elementnodes(n,k))+planktonic_variable(j+(n-1)*number_plankt_vari)
            end do ! alle ecken
         end do ! alle Elemente
         write(ion,'(3A)')'SCALARS ',ADJUSTL(trim(planktonic_variable_name(j))),' float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D
            write(ion,'(f27.6)') output(n)/real(knot_ele(n))
         end do ! alle Knoten
      end if ! zur ausgabe vorgesehen
   end do ! alle planktonic_variable
   if (nur_alter) then ! Aufenthaltszeit in Tagen ! Altersberechnung wie in Shen&Wang 2007 mit real function aufenthaltszeit (tracer,alter)
      write(ion,'(A)')'SCALARS Tage_Aufenthalt float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,knotenanzahl2D
         output(n) = 0.0
      end do ! alle Knoten
      do n = 1,number_plankt_point
         tr = planktonic_variable(71+(n-1)*number_plankt_vari)
         al = planktonic_variable(73+(n-1)*number_plankt_vari)
         aus = 0.0
         if (tr > 0.00001 ) aus = al / tr
         do k = 1,cornernumber(n)
            output(elementnodes(n,k)) = output(elementnodes(n,k))+aus
         end do ! alle ecken
      end do ! alle Elemente
      do n = 1,knotenanzahl2D
         write(ion,'(f27.6)') output(n)/real(knot_ele(n))
      end do ! alle Knoten
   end if !nur_alter
   deallocate (output, stat = alloc_status )
   close (ion)
   print*,'knoten_ ausgeben_untrim gemacht'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   return
end subroutine ausgeben_untrim
