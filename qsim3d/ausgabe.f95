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

subroutine ausgeben()
   use modell
   implicit none

   call mpi_barrier (mpi_komm_welt, ierr)
   call gather_benthic()
   call gather_ueber()

   ! Aufruf immer nach stofftransport() daher ist gather_planktkon() immer schon gemacht
   if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten
      select case (hydro_trieb)
         case(1) ! casu-transinfo
            call ausgeben_casu()
   
         case(2) ! Untrim² netCDF
            call ausgeben_untrim(rechenzeit)
         
         case(3) ! SCHISM
            ! call ausgeben_schism(rechenzeit)
         
         case default
            print*,'hydro_trieb = ',hydro_trieb
           call qerror('ausgeben: Hydraulischer Antrieb unbekannt')
      end select
   endif 
   call mpi_barrier (mpi_komm_welt, ierr)
   
end subroutine ausgeben


!> Suboutine ausgabekonzentrationen() ließt aus der Datei
!! <a href="./exp/ausgabekonzentrationen.txt" target="_blank">ausgabekonzentrationen.txt</a>
!! welche variablen ausgegeben werden sollen.\n
!! als Beispiel-Datei, der entnehmbar ist, welche Variablen ausgegeben werden könnnen, schreibt ausgabekonzentrationen()
!! die Datei ausgabekonzentrationen_beispiel.txt \n
!! Die angekreuzten, gewählten Variablen werden sowohl bei den Ganglinien als auch bei den ausgabezeitpunkten verwendet.
!!\n\n
!! aus Datei ausgabe.f95 ; zurück zu \ref lnk_modellerstellung
subroutine ausgabekonzentrationen()
   use modell
   implicit none
   
   integer        :: ion, ibei, open_error, io_error, alloc_status, iscan, j, n, sysa
   logical        :: found
   character(200) :: dateiname, text
   character(300) :: systemaufruf
   
   output_plankt(:) = .false.
   output_plankt_vert(:) = .false.
   output_benth_distr(:) = .false.
   output_trans_val(:) = .false.
   output_trans_quant(:) = .false.
   output_trans_quant_vert(:) = .false.
   
   dateiname = trim(modellverzeichnis) // 'ausgabekonzentrationen.txt'
   ion = 103
   open(unit = ion , file = dateiname, status = 'old', action = 'read ', iostat = open_error)
   if (open_error /= 0) then
      close (ion)
      return
   else
      print*,'open ausgabekonzentrationen.txt'
   endif
   
   do while (zeile(ion)) ! read all lines and understand
      
      if (ctext(1:1) /= 'x' .and. ctext(1:1) /= 'X') cycle
      
      found = .false.
      
      ! all depth averaged planktic con.
      do j = 1,number_plankt_vari 
         write(text,'(A18)') trim(planktonic_variable_name(j))
         iscan = index(trim(ctext),trim(text))
         if (iscan > 0) then ! found
            print*, meinrang, iscan,' output for planktic concentration j = ',j,' parameter: ',trim(text)
            output_plankt(j) = .true.
            found = .true.
         endif
      enddo 
      
      ! all vertically distributed planktonic variables
      do j = 1,number_plankt_vari_vert 
         write(text,'(A18)')trim(plankt_vari_vert_name(j))
         iscan = index(trim(ctext),trim(text))
         if (iscan > 0) then ! found
            if (meinrang == 0)print*,'output only for level 1; plankt_vari_vert j = ',j,' parameter: ',trim(text)
            output_plankt_vert(j) = .true.
            found = .true.
         endif 
      enddo
      
      ! all benthic distributions
      do j = 1,number_benth_distr 
         write(text,'(a)')adjustl(trim(benth_distr_name(j)))
         iscan = index(trim(ctext),trim(text))
         if (iscan > 0) then ! found
            if (meinrang == 0)print*,'output for benthic distribution j = ',j,' parameter: ',trim(text)
            output_benth_distr(j) = .true.
            found = .true.
         endif
         ! ausgabe_bentver(j)=.true. ! überbrückt: ### alle
         ! ausgabe_bentver(j)=.false. ! überbrückt: ### keine
      enddo 
      
      ! alle globalen Übergabe Werte
      do j = 1,number_trans_val  
         write(text,'(a)')adjustl(trim(trans_val_name(j)))
         iscan = index(trim(ctext),trim(text))
         if (iscan > 0) then ! found
            print*,'ausgabe globaler uebergabe wert j = ',j,' parameter: ',trim(text)
            output_trans_val(j) = .true.
            found = .true.
         endif !! in string ctext
      enddo
      
      ! all exchange con.
      do j = 1,number_trans_quant 
         write(text,'(A)')ADJUSTL(trim(trans_quant_name(j)))
         iscan = index(trim(ctext),trim(text))
         if (iscan > 0) then ! found
            if (meinrang == 0)print*,'output for exchange concentration j = ',j,' parameter: ',trim(text)
            output_trans_quant(j) = .true.
            found = .true.
         endif !! in string ctext
         ! output_trans_quant(j)=.true.  überbrückt: alle
         ! output_trans_quant(j)=.false. überbrückt: keine
      enddo 
      
      ! all vertically distributed transfer quantities
      do j = 1,number_trans_quant_vert  
         write(text,'(A)')ADJUSTL(trim(trans_quant_vert_name(j)))
         iscan = index(trim(ctext),trim(text))
         if (iscan > 0) then ! found
            if (meinrang == 0)print*,'output only for level 1; trans_quant_vert j = ',j,' parameter: ',trim(text)
            output_trans_quant_vert(j) = .true.
            found = .true.
         endif
      enddo
      
      if (.not. found) print*,'no parameter found for choice:'
      
   enddo 
   close(ion)
   
   ! always write age concentrations in age simulation
   if (nur_alter) then 
      output_plankt(71) = .true. ! Tracer
      output_plankt(73) = .true. ! age_decay
      output_plankt(74) = .true. ! age_arith
      output_plankt(75) = .true. ! age_growth
   endif 
   
   n_bn = count(output_benth_distr)
   n_pl = count(output_plankt) + count(output_plankt_vert)
   n_ue = count(output_trans_val) + count(output_trans_quant) + count(output_trans_quant_vert)
   
   print*,'ausgabekonzentrationen n_pl,n_bn,n_ue = ',n_pl,n_bn,n_ue
   
end subroutine ausgabekonzentrationen

!> Write file `ausgabekonzentrationen_beispiel.txt`
!!
!! This file lists all available output variables.
subroutine ausgabekonzentrationen_beispiel()
   use modell
   implicit none
   
   integer             :: j,open_error
   character(longname) :: dateiname
   
   dateiname = trim(modellverzeichnis) // 'ausgabekonzentrationen_beispiel.txt'
   open(unit = 104 , file = dateiname, status = 'replace', action = 'write ', iostat = open_error)
   if (open_error /= 0) call qerror("Could not open file " // dateiname)

   print*,'ausgabekonzentrationen_beispiel.txt opened for write ...'
   
   ! depth averaged planktic variables
   write(104,'(A)')"# depth averaged, planctonic, transported concentrations"
   do j = 1,number_plankt_vari 
      write(104,'(A1,7x,I4,2x,A18)') "0", j, trim(planktonic_variable_name(j))
   enddo 
   
   ! vertically distributed planktonic variables
   write(104,'(A)')"# depth resolving, planctonic, transported concentrations"
   do j = 1,number_plankt_vari_vert 
      write(104,'(A1,7x,I4,2x,A18)') "0", j, trim(plankt_vari_vert_name(j))
   enddo 
   
   ! benthic distributions
   write(104,'(A)')"# bentic distributions"
   do j = 1,number_benth_distr 
      write(104,'(A1,7x,I4,2x,A18)') "0", j, trim(benth_distr_name(j))
   enddo 
   
   ! global transfer variables
   write(104,'(A)')"# global transfer variables"
   do j = 1,number_trans_val  
      write(104,'(A1,7x,I4,2x,A18)') "0", j, trim(trans_val_name(j))
   enddo
   
   ! depth averaged transfer variables
   write(104,'(A)')"# depth averaged transfer variables"
   do j = 1,number_trans_quant 
      write(104,'(A1,7x,I4,2x,A18)') "0", j, trim(trans_quant_name(j))
   enddo
   
   ! vertically distributed transfer quantities
   write(104,'(A)')"# depth resolving transfer variables"
   do j = 1,number_trans_quant_vert  
      write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(trans_quant_vert_name(j))
   enddo
   
   close(104)
   
end subroutine ausgabekonzentrationen_beispiel


!> Read file `ausgabezeitpunkte.txt` 
!!
!! Datetimes defined in `ausgabezeitpunkte.txt` are stored in variable 
!! `ausgabe_zeitpunkt`.
subroutine ausgabezeitpunkte()
   use modell
   use module_datetime
   implicit none
   
   integer        :: n, u_out, open_error, io_error, nba
   integer        :: day, month, year, hour, minute, second
   character(200) :: filename
   type(datetime), dimension(:), allocatable :: datetime_output
   
   
   filename = trim(modellverzeichnis) // 'ausgabezeitpunkte.txt'
   u_out = 103
   open(newunit = u_out , file = filename, status = 'old', action = 'read ', iostat = open_error)
   if (open_error /= 0) call qerror ("could not open " // trim(filename))
   
   
   ! determine number of output times
   n_output = 0
   do while (zeile(u_out))
      ! commented lines ('#') are skipped
      if (ctext(1:1) /= '#') then
         n_output = n_output + 1
         read(ctext,*, iostat = io_error) day, month, year, hour, minute, second
         if (io_error /= 0) call qerror("error while reading " // trim(filename))
      endif 
   enddo 
   
   allocate(ausgabe_zeitpunkt(n_output))
   allocate(ausgabe_bahnlinie(n_output))
   allocate(datetime_output(n_output))

   
   ! --- read dates for output ---
   rewind(u_out)
   n = 0
   do while (zeile(u_out))
      if (ctext(1:1) /= '#') then ! keine kommentarzeile
         n = n + 1
         read(ctext,*) day, month, year, hour, minute, second
         
         datetime_output(n) = datetime(year, month, day, hour, minute, tz = tz_qsim)
         ausgabe_zeitpunkt(n) = datetime_output(n) % seconds_since_epoch()
         
         ! check for trajectory output
         read(ctext,*, iostat = io_error) day, month, year, hour, minute, second, nba
         if (io_error == 0) then
            ausgabe_bahnlinie(n) = nba
         else
            ausgabe_bahnlinie(n) = 0
         endif 
      endif 
   enddo 
   close (u_out)
   
   
   ! --- print summary to console ---
   print* 
   print "(a)", repeat("-", 80)
   print "(a)", "output settings"
   print "(a)", repeat("-", 80)
   
   print "(a,i0)",  "n_output = ", n_output
   print "(3a,i0)", "first output = ", datetime_output(1) % date_string(),        " | ",  ausgabe_zeitpunkt(1) 
   print "(3a,i0)", "last output =  ", datetime_output(n_output) % date_string(), " | ",  ausgabe_zeitpunkt(n_output) 
   
   if (any(ausgabe_zeitpunkt < startzeitpunkt .or. ausgabe_zeitpunkt > endzeitpunkt)) then
      print*
      print "(a)", "note:"
      print "(a)", "  some output dates are outside of the simulated timeperiod and will "
      print "(a)", "  not be included in the model results."
   
   endif
   
end subroutine ausgabezeitpunkte

!> true if output required now
subroutine ausgeben_parallel()
   use modell
   implicit none
   
   integer :: alloc_status
   
   call MPI_Bcast(n_output, 1, MPI_INT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) call qerror("Error while mpi_bcast of variable `n_output`.")
      
   if (meinrang /= 0) then
      allocate(ausgabe_zeitpunkt(n_output), stat = alloc_status)
      allocate(ausgabe_bahnlinie(n_output), stat = alloc_status)
   endif

   call MPI_Bcast(ausgabe_zeitpunkt, n_output, MPI_INTEGER8, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) call qerror("Error while mpi_bcast of variable `ausgabe_zeitpunkt`.")
   
   call MPI_Bcast(ausgabe_bahnlinie,n_output,MPI_INT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) call qerror("Error while mpi_bcast of variable `ausgabe_bahnlinie`.")
   
end subroutine ausgeben_parallel


!> true if output required now
logical function jetzt_ausgeben()
   use modell
   use iso_fortran_env
   implicit none
   
   integer        :: n
   integer(int64) :: diff
   
   jetzt_ausgeben = .false.
   bali = .false.
   
   do n = 1,n_output,1
      diff = ausgabe_zeitpunkt(n) - rechenzeit
      if (diff >= (-1*(deltat/2)) .and. diff < (deltat/2)) then
         jetzt_ausgeben = .true.
         if (ausgabe_bahnlinie(n) /= 0) bali = .true.
      endif
   enddo
   
   if (jetzt_ausgeben) print*,'jetzt_ausgeben ,meinrang',meinrang
   
end function jetzt_ausgeben


!> Kontrollausgabe des Netzes
subroutine show_mesh()
   use modell
   implicit none
   character(longname) :: dateiname, systemaufruf
   integer             :: n, ion, open_error, nel, ner, alloc_status,errcode
   real                :: dummy
   
   
   if (meinrang /= 0) return ! nur auf Prozessor 0 bearbeiten
   
   ! --------------------------------------------------------------------------
   ! nodes
   ! --------------------------------------------------------------------------
   dateiname = trim(modellverzeichnis) // 'mesh_node.vtk'
   write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
   
   ion = 106
   open(unit = ion , file = dateiname, status = 'replace', action = 'write ', iostat = open_error)
   if (open_error /= 0) call qerror("Could not open " // trim(dateiname))
   call mesh_output(ion)
   print*,'show_mesh:mesh_node.vtk done'
   
   close (ion)
   
   ! --------------------------------------------------------------------------
   ! edges
   ! --------------------------------------------------------------------------
   if (hydro_trieb == 3) then ! schism
      dateiname = trim(modellverzeichnis) // 'mesh_midedge.vtk'
      open(unit = ion , file = dateiname, status = 'replace', action = 'write', iostat = open_error)
      if (open_error /= 0) call qerror("Could not open "// trim(dateiname))
      
      write(ion,'(A)') '# vtk DataFile Version 3.0'
      write(ion,'(A)') 'Simlation QSim3D SCHISM'
      write(ion,'(A)') 'ASCII'
      write(ion,'(A)') 'DATASET UNSTRUCTURED_GRID'
      write(ion,'(A)') ' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',kantenanzahl, ' float'
      
      do n = 1,kantenanzahl
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') 0.5 * (knoten_x(top_node(n))+knoten_x(bottom_node(n))), &
                                               0.5 * (knoten_y(top_node(n))+knoten_y(bottom_node(n))), &
                                               0.0
      enddo
      
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl, kantenanzahl*2
      do n = 1,kantenanzahl
         write(ion,'(A,2x,I12)')'1',n-1
      enddo
      
      write(ion,'(A)')' ' ! vtk-vertex
      write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl
      do n = 1,kantenanzahl
         write(ion,'(A)')'1'
      enddo
      
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'POINT_DATA ', kantenanzahl
      write(ion,'(A)')'SCALARS length float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,kantenanzahl
         write(ion,'(f27.6)') cell_bound_length(n) ! real(n) ! ed_area(n)
      enddo
      
      dummy = 0.0
      write(ion,'(A)') 'VECTORS normal float'
      do n = 1,kantenanzahl
         write(ion,'(6x, f11.6, 2x, f11.6, 2x, f11.6)') edge_normal_x(n),edge_normal_y(n),dummy
      enddo 
      
      close (ion)
      print*,'show_mesh:mesh_midedge.vtk schism done',kantenanzahl
      
      dateiname = trim(modellverzeichnis) // 'mesh_side.vtk'
      open(unit = ion , file = dateiname, status = 'replace', action = 'write', iostat = open_error )
      if (open_error /= 0) call qerror("Could not open " // trim(dateiname))

      write(ion,'(A)')'# vtk DataFile Version 3.0'
      write(ion,'(A)')'Simlation QSim3D SCHISM'
      write(ion,'(A)')'ASCII'
      write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
      do n = 1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
      enddo 
      
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl, kantenanzahl*3
      do n = 1,kantenanzahl
         write(ion,'(A,2x,I12,2x,I12)')'2',top_node(n)-1,bottom_node(n)-1
      enddo 
      
      write(ion,'(A)')' ' ! vtk-vertex
      write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl
      do n = 1,kantenanzahl
         write(ion,'(A)')'3'
      enddo 
      
      dummy = 123.4
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
      write(ion,'(A)')'SCALARS dummy float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,knotenanzahl2D
         write(ion,'(f27.6)') dummy ! real(n) ! ed_area(n)
      enddo
      
      close (ion)
      print*,'show_mesh:mesh_side.vtk schism done',kantenanzahl
   endif ! schism
   
   
   ! --------------------------------------------------------------------------
   ! faces
   ! --------------------------------------------------------------------------
   kanten_vorhanden = .false. !! geht schief bei casu ????
  
   if ((hydro_trieb == 2) .or. (kanten_vorhanden)) then ! untrim
      dateiname = trim(modellverzeichnis) // 'mesh_element.vtk'
      ion = 106
      open(unit = ion, file = dateiname, status = 'replace', action = 'write ', iostat = open_error )
      if (open_error /= 0) call qerror("Could not open " // dateiname)

      write(ion,'(A)')'# vtk DataFile Version 3.0'
      write(ion,'(A)')'mesh_element '
      write(ion,'(A)')'ASCII'
      write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,A)')'POINTS ',n_elemente+knotenanzahl2D, ' float'

      dummy = 0.0
      do n = 1,n_elemente
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') element_x(n), element_y(n), dummy
      enddo
      
      do n = 1,knotenanzahl2D
         write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
      enddo
      
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', kantenanzahl, 3*kantenanzahl
      do n = 1,kantenanzahl
         nel = left_element(n)
         ner = right_element(n)
         if (boundary_number(n) > 0 ) then
            ner = n_elemente+top_node(n)
            nel = n_elemente+bottom_node(n)
         endif
         write(ion,'(A,2x,I8,2x,I8)')'2', nel-1, ner-1
      enddo
      
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'CELL_TYPES ', kantenanzahl
      do n = 1,kantenanzahl
         write(ion,'(A)')'3'
      enddo
      
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'POINT_DATA ', n_elemente+knotenanzahl2D
      write(ion,'(A)')'SCALARS boundary float 1'
      write(ion,'(A)')'LOOKUP_TABLE default'
      do n = 1,n_elemente
         write(ion,'(f27.6)') real(element_rand(n))
      enddo
      
      do n = 1,knotenanzahl2D
         write(ion,'(f27.6)') real(knoten_rand(n))
      enddo
      
      print*,'show_mesh: mesh_element.vtk done'
      close (ion)
   endif! edges
   
end subroutine show_mesh


subroutine mesh_output(ion)
   use modell
   implicit none

   character(longname) :: dateiname
   integer             :: ion, n, igr3, io_error
   
   ! --------------------------------------------------------------------------
   ! .vtk
   ! --------------------------------------------------------------------------
   write(ion,'(A)')'# vtk DataFile Version 3.0'
   write(ion,'(A)')'Simlation QSim3D'
   write(ion,'(A)')'ASCII'
   write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
   
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
   do n = 1,knotenanzahl2D
      write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
   enddo 
   
   if (element_vorhanden) then
      ! Elemente ausgeben (Knotennummern in paraview wieder von 0 beginnend)
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', n_elemente, summ_ne
      do n = 1,n_elemente ! alle Elemente
         if (cornernumber(n) == 3) then
            write(ion,'(4(I8,2x))') cornernumber(n),elementnodes(n,1)-1,elementnodes(n,2)-1,elementnodes(n,3)-1
         endif
         
         if (cornernumber(n) == 4) then
            write(ion,'(5(I8,2x))') cornernumber(n),elementnodes(n,1)-1,elementnodes(n,2)-1,elementnodes(n,3)-1,elementnodes(n,4)-1
         endif
      enddo 
      
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'CELL_TYPES ', n_elemente
      
      do n = 1,n_elemente 
         if (cornernumber(n) == 3)write(ion,'(A)') '5'
         if (cornernumber(n) == 4)write(ion,'(A)') '9'
      enddo 
      
   else ! keine file.elements vorhanden
      
      ! Punkte als vtk-vertices
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12,2x,I12)')'CELLS ', knotenanzahl2D, 2*knotenanzahl2D
      do n = 1,knotenanzahl2D
         write(ion,'(A,2x,I8)')'1', n-1
      enddo
      
      write(ion,'(A)')' '
      write(ion,'(A,2x,I12)')'CELL_TYPES ', knotenanzahl2D
      do n = 1,knotenanzahl2D
         write(ion,'(A)')'1'
      enddo
   endif !! element_vorhanden
   
   
   write(ion,'(A)')' '
   write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
   write(ion,'(A)')'SCALARS Gelaendehoehe float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') knoten_z(n)
   enddo ! alle Knoten
   
   write(ion,'(A)')'SCALARS knoten_zone float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') real( knoten_zone(n) )
   enddo
   
   write(ion,'(A)')'SCALARS knoten_rand float 1'
   write(ion,'(A)')'LOOKUP_TABLE default'
   do n = 1,knotenanzahl2D
      write(ion,'(f27.6)') real(knoten_rand(n))
   enddo
   
   ! --------------------------------------------------------------------------
   ! .gr3
   ! --------------------------------------------------------------------------
   if (meinrang /= 0) call qerror("mesh_output may only be called from process 0") 
   
   dateiname = trim(modellverzeichnis) // 'mesh.gr3'
   open (newunit = igr3, file = dateiname, status = 'unknown', action = 'write ', iostat = io_error)
   if (io_error /= 0) call qerror("Could not open " // trim(dateiname))
   
   write(igr3,'(A,2x,A)') 'Grid written by QSim3D', modellverzeichnis
   write(igr3,*) n_elemente, knotenanzahl2D
   
   do n = 1,knotenanzahl2D
      ! write(igr3,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
      write(igr3,*)n, knoten_x(n), knoten_y(n), knoten_z(n)
   enddo
   
   do n = 1,n_elemente ! alle Elemente
      if (cornernumber(n) == 3) then
         write(igr3,*) &
                      n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
      endif
      if (cornernumber(n) == 4) then
         write(igr3,*) &
                      n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
      endif
   enddo
   
   close (igr3)
   
end subroutine mesh_output
