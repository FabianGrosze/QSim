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
   integer ierr
   
   call mpi_barrier (mpi_komm_welt, ierr)
   call gather_benthic()
   call gather_ueber()
   !! Aufruf immer nach stofftransport() daher ist gather_planktkon() immer schon gemacht
   if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten
      select case (hydro_trieb)
         case(1) ! casu-transinfo
            call ausgeben_casu()
         case(2) ! Untrim² netCDF
            call ausgeben_untrim(rechenzeit)
         case(3) ! SCHISM
            call ausgeben_schism(rechenzeit)
         case default
            print*,'hydro_trieb = ',hydro_trieb
            call qerror('ausgeben: Hydraulischer Antrieb unbekannt')
      end select
   end if ! nur Prozessor 0
   call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine ausgeben
!----+-----+----+-----+----+-----+----+-----+----
!> Suboutine tagesmittelwert() macht tagesmittelwerte
!! \n\n
subroutine tagesmittelwert()
   use modell
   implicit none
   integer j,n, ion, open_error, system_error, errcode,ierr
   real tagesanteil, null
   character(len = longname) :: dateiname, dateiname2, systemaufruf, zahl
   character(50) tm,tt,tj
   null = 0.0
   !if(.true.) then ! heute mittelwertausgabe
   if ((monat == 7) .and. (tag >= 5).and.(tag <= 25)) then ! heute mittelwertausgabe
      if (uhrzeit_stunde < uhrzeit_stunde_vorher) then ! Tageswechsel
         write(zahl,*)rechenzeit
         write(tj,'(I4.4)')jahr
         write(tm,'(I2.2)')monat
         write(tt,'(I2.2)')tag
         zahl = adjustl(zahl)
         tj = adjustl(tj)
         tm = adjustl(tm)
         tt = adjustl(tt)
         ion = 105
         write(dateiname,'(8A)',iostat = errcode)trim(modellverzeichnis),'mittelwert',trim(tj),'_',trim(tm),'_',trim(tt),'.vtk'
         if (errcode /= 0)call qerror('tagesmittelwert writing filename mittelwert failed')
         print*,'Ausgabe Mittelwert auf: ',trim(dateiname)
         write(systemaufruf,'(2A)',iostat = errcode)'rm -rf ',trim(dateiname)
         if (errcode /= 0)call qerror('tagesmittelwert writing system call rm -rf dateiname mittelwert failed')
         call system(trim(systemaufruf),system_error)
         if (system_error /= 0) then
            print*,'rm -rf mittelwert_*** failed.'
         end if ! system_error.ne.0
         open ( unit = ion , file = dateiname, status = 'new', action = 'write', iostat = open_error )
         if (open_error /= 0) then
            write(fehler,*)'open_error mittelwert_vtk open_error = ',open_error
            call qerror(fehler)
         end if ! open_error.ne.0
         if (knotenanzahl2D /= number_benthic_points) then
            write(fehler,*)'3D noch nicht vorgesehen hier'
            call qerror(fehler)
         end if !
         if (number_plankt_point /= knotenanzahl2D) then
            write(fehler,*)'number_plankt_point und knotenanzahl2D passen nicht zusammen ???'
            call qerror(fehler)
         end if !
         !write(ion,*)'huhu ausgabe'
         write(ion,'(A)')'# vtk DataFile Version 3.0'
         write(ion,'(A)')'Simlation tiqusim'
         write(ion,'(A)')'ASCII'
         !write(ion,'(A)')'DATASET POLYDATA'
         write(ion,'(A)')'DATASET UNSTRUCTURED_GRID'
         !
         write(ion,'(A)')' '
         write(ion,'(A,2x,I12,2x,A)')'POINTS ',knotenanzahl2D, ' float'
         do n = 1,knotenanzahl2D
            write(ion,'(f17.5,2x,f17.5,2x,f8.3)') knoten_x(n), knoten_y(n), knoten_z(n)
         end do ! alle Knoten
         if (element_vorhanden) then
            ! Elemente ausgeben
            write(ion,'(A)')' '
            write(ion,'(A,2x,I12,2x,I12)')'CELLS ', n_elemente, summ_ne
            do n = 1,n_elemente ! alle Elemente
               if (cornernumber(n) == 3) then
                  write(ion,'(4(I8,2x))') cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
               end if
               if (cornernumber(n) == 4) then
                  write(ion,'(5(I8,2x))') cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
               end if
            end do ! alle Elemente
            write(ion,'(A)')' '
            write(ion,'(A,2x,I12)')'CELL_TYPES ', n_elemente
            do n = 1,n_elemente ! alle Elemente
               if (cornernumber(n) == 3)write(ion,'(A)') '5'
               if (cornernumber(n) == 4)write(ion,'(A)') '9'
            end do ! alle Elemente
         else ! keine file.elements vorhanden
            ! Punkte als vtk-vertices
            write(ion,'(A)')' '
            write(ion,'(A,2x,I12,2x,I12)')'CELLS ', knotenanzahl2D, 2*knotenanzahl2D
            do n = 1,knotenanzahl2D
               write(ion,'(A,2x,I8)')'1', n-1
            end do ! alle Knoten
            write(ion,'(A)')' '
            write(ion,'(A,2x,I12)')'CELL_TYPES ', knotenanzahl2D
            do n = 1,knotenanzahl2D
               write(ion,'(A)')'1'
            end do ! alle Knoten
         end if !! element_vorhanden
         write(ion,'(A)')' '
         write(ion,'(A,2x,I12)')'POINT_DATA ', knotenanzahl2D
         write(ion,'(A)')'SCALARS Gelaendehoehe float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D
            write(ion,'(f27.6)') knoten_z(n)
         end do ! alle Knoten
         write(ion,'(A)')'SCALARS T_wass_mittel float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D
            write(ion,'(f27.6)') transfer_quantity_p(68+(n-1)*number_trans_quant)
         end do ! alle Knoten
         write(ion,'(A)')'SCALARS T_sed_mittel float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D
            write(ion,'(f27.6)') transfer_quantity_p(69+(n-1)*number_trans_quant)
         end do ! alle Knoten
         write(ion,'(A)')'SCALARS mittel_tief float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D
            if (transfer_quantity_p(71+(n-1)*number_trans_quant) > 0.0) then
               write(ion,'(f27.6)') transfer_quantity_p(70+(n-1)*number_trans_quant)  &
                                   / transfer_quantity_p(71+(n-1)*number_trans_quant)    ! tagesmittelwert Wassertiefe
            else
               write(ion,'(f27.6)') null
            end if
         end do ! alle Knoten
         write(ion,'(A)')'SCALARS Bedeckungsdauer float 1'
         write(ion,'(A)')'LOOKUP_TABLE default'
         do n = 1,knotenanzahl2D
            write(ion,'(f27.6)') transfer_quantity_p(71+(n-1)*number_trans_quant)
         end do ! alle Knoten
         do n = 1,knotenanzahl2D
            transfer_quantity_p(68+(n-1)*number_trans_quant) = 0.0
            transfer_quantity_p(69+(n-1)*number_trans_quant) = 0.0
            transfer_quantity_p(70+(n-1)*number_trans_quant) = 0.0
            transfer_quantity_p(71+(n-1)*number_trans_quant) = 0.0
         end do ! alle Knoten wieder null setzen
         close(ion)
      endif ! Tageswechsel
      tagesanteil = real(deltat)/real(86400)
      do n = 1,knotenanzahl2D  !!!!!!!!!!!  mittelwerte aufsummieren
         transfer_quantity_p(68+(n-1)*number_trans_quant) = transfer_quantity_p(68+(n-1)*number_trans_quant)  &
                                                          + (planktonic_variable_p(1+(n-1)*number_plankt_vari)  * tagesanteil) ! Wasser-Temperatur Rückgabewert
         transfer_quantity_p(69+(n-1)*number_trans_quant) = transfer_quantity_p(69+(n-1)*number_trans_quant)  &
                                                          + (benthic_distribution_p(1+(n-1)*number_benth_distr) * tagesanteil) ! Temperatur des Sediments - Rückgabewert
         if (rb_hydraul(2+(n-1)*number_rb_hydraul) > 0.02) then ! tief(n)
            transfer_quantity_p(70+(n-1)*number_trans_quant) = transfer_quantity_p(70+(n-1)*number_trans_quant)  &
                                                             + (rb_hydraul(2+(n-1)*number_rb_hydraul) * tagesanteil) ! TagesSumme Tiefe wenn bedeckt
            transfer_quantity_p(71+(n-1)*number_trans_quant) = transfer_quantity_p(71+(n-1)*number_trans_quant)  &
                                                             + (tagesanteil) ! Bedeckungsdauer (Tageanteil)
         endif
      end do ! alle Knoten
   endif ! heute mittelwertberechnung
   uhrzeit_stunde_vorher = uhrzeit_stunde
   return
end subroutine tagesmittelwert
!----+-----+----
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
   integer :: ion, ibei, open_error, io_error, alloc_status, iscan, j, n, sysa,ierr
   character (len = 200) :: dateiname, text
   logical :: found
   character(300) systemaufruf
   !>integer :: k_ausgabe
   !>integer , allocatable , dimension (:) :: ausgabe_konz
   output_plankt(:) = .false.
   output_plankt_vert(:) = .false.
   output_benth_distr(:) = .false.
   output_trans_val(:) = .false.
   output_trans_quant(:) = .false.
   output_trans_quant_vert(:) = .false.
   write(dateiname,'(2A)')trim(modellverzeichnis),'ausgabekonzentrationen.txt'
   ion = 103
   open ( unit = ion , file = dateiname, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0) then
      print*,'keine ausgabekonzentrationen, open_error = ',open_error
      close (ion)
      return
   else
      print*,'ausgabekonzentrationen.txt geoeffnet ...'
   end if ! open_error.ne.0
   do while ( zeile(ion)) !!  read all lines and understand
      if ((ctext(1:1) == 'x') .or. (ctext(1:1) == 'X')) then ! line marked ?
         found = .false.
         !print*,trim(ctext)
         do j = 1,number_plankt_vari ! all depth averaged planktic con.
            write(text,'(A18)')trim(planktonic_variable_name(j))
            iscan = index(trim(ctext),trim(text))
            if (iscan > 0) then ! found
               print*,meinrang,iscan,' output for planktic concentration j = ',j,' parameter: ',trim(text)
               !print*,trim(ctext)
               output_plankt(j) = .true.
               found = .true.
            end if !! in string ctext
         end do ! done all planktic con.
         do j = 1,number_plankt_vari_vert ! all vertically distributed planktonic variables
            write(text,'(A18)')trim(plankt_vari_vert_name(j))
            iscan = index(trim(ctext),trim(text))
            if (iscan > 0) then ! found
               if (meinrang == 0)print*,'output only for level 1; plankt_vari_vert j = ',j,' parameter: ',trim(text)
               !print*,trim(ctext)
               output_plankt_vert(j) = .true.
               found = .true.
            end if !! in string ctext
         end do ! done all plankt_vari_vert
         do j = 1,number_benth_distr ! all benthic distributions
            write(text,'(A)')ADJUSTL(trim(benth_distr_name(j)))
            iscan = index(trim(ctext),trim(text))
            if (iscan > 0) then ! found
               if (meinrang == 0)print*,'output for benthic distribution j = ',j,' parameter: ',trim(text)
               !print*,trim(ctext)
               output_benth_distr(j) = .true.
               found = .true.
            end if !! in string ctext
            ! ausgabe_bentver(j)=.true. ! überbrückt: ### alle
            ! ausgabe_bentver(j)=.false. ! überbrückt: ### keine
         end do ! done all all benthic distributions
         do j = 1,number_trans_val  ! alle globalen Übergabe Werte
            write(text,'(A)')ADJUSTL(trim(trans_val_name(j)))
            iscan = index(trim(ctext),trim(text))
            if (iscan > 0) then ! found
               print*,'ausgabe globaler uebergabe wert j = ',j,' parameter: ',trim(text)
               !print*,trim(ctext)
               output_trans_val(j) = .true.
               found = .true.
            end if !! in string ctext
         end do !
         do j = 1,number_trans_quant ! all exchange con.
            write(text,'(A)')ADJUSTL(trim(trans_quant_name(j)))
            iscan = index(trim(ctext),trim(text))
            if (iscan > 0) then ! found
               if (meinrang == 0)print*,'output for exchange concentration j = ',j,' parameter: ',trim(text)
               !print*,trim(ctext)
               output_trans_quant(j) = .true.
               found = .true.
            end if !! in string ctext
            ! output_trans_quant(j)=.true. ! überbrückt: ### alle
            ! output_trans_quant(j)=.false. ! überbrückt: ### keine
         end do ! done all exchange con.
         do j = 1,number_trans_quant_vert  ! all vertically distributed transfer quantities
            write(text,'(A)')ADJUSTL(trim(trans_quant_vert_name(j)))
            iscan = index(trim(ctext),trim(text))
            if (iscan > 0) then ! found
               if (meinrang == 0)print*,'output only for level 1; trans_quant_vert j = ',j,' parameter: ',trim(text)
               !print*,trim(ctext)
               output_trans_quant_vert(j) = .true.
               found = .true.
            end if !! in string ctext
         end do ! done all vertically distributed transfer quantities
         if ( .not. found) then
            print*,'no parameter found for choice:'
            !print*,trim(ctext)
         end if ! not found
      end if ! marked line
   end do ! no further line
   close (ion)
   if (nur_alter) then ! allways write age concentrations in age simulation
      output_plankt(71) = .true. ! Tracer
      output_plankt(73) = .true. ! age_decay
      output_plankt(74) = .true. ! age_arith
      output_plankt(75) = .true. ! age_growth
   end if ! nuralter
   
   n_pl = 0
   do j = 1,number_plankt_vari
      if (output_plankt(j))n_pl = n_pl+1
   end do
   do j = 1,number_plankt_vari_vert
      if (output_plankt_vert(j))n_pl = n_pl+1
   end do
   n_bn = 0
   do j = 1,number_benth_distr
      if (output_benth_distr(j))n_bn = n_bn+1
   end do
   n_ue = 0
   do j = 1,number_trans_val
      if (output_trans_val(j))n_ue = n_ue+1
   end do
   do j = 1,number_trans_quant
      if (output_trans_quant(j))n_ue = n_ue+1
   end do
   do j = 1,number_trans_quant_vert
      if (output_trans_quant_vert(j))n_ue = n_ue+1
   end do
   print*,'ausgabekonzentrationen n_pl,n_bn,n_ue = ',n_pl,n_bn,n_ue
   !     writing output variable list moved to SUBROUTINE eingabe()
   !text='ausgabekonzentrationen_beispiel.txt'
   !dateiname=trim(adjustl(modellverzeichnis))//trim(adjustl(text))
   !systemaufruf='cp '//trim(adjustl(codesource))//'/'//trim(adjustl(text))//' '//trim(dateiname)
   !call system(systemaufruf,sysa)
   !if(sysa.ne.0) Print*,'### kopieren von ',trim(adjustl(text)),' ausgabekonzentrationen_beispiel.txt fehlgeschlagen ###'
   return
end subroutine ausgabekonzentrationen
!----+-----+----
!> suboutine ausgabekonzentrationen_beispiel writes file ausgabekonzentrationen_beispiel.txt to inform about available output variables
!! \n\n
subroutine ausgabekonzentrationen_beispiel()
   use modell
   implicit none
   integer :: j,open_error,ierr
   character (len = 300) :: dateiname
   write(dateiname,'(2A)')trim(modellverzeichnis),'ausgabekonzentrationen_beispiel.txt'
   open ( unit = 104 , file = dateiname, status = 'replace', action = 'write ', iostat = open_error )
   if (open_error /= 0) then
      print*,'ausgabekonzentrationen_beispiel.txt open_error = ',open_error
      close (104)
      return
   else
      print*,'ausgabekonzentrationen_beispiel.txt opened for write ...'
   end if ! open_error.ne.0
   write(104,'(A)')"# depth averaged, planctonic, transported concentrations"
   do j = 1,number_plankt_vari ! all depth averaged planktic con.
      write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(planktonic_variable_name(j))
   end do ! done all planktic con.
   write(104,'(A)')"# depth resolving, planctonic, transported concentrations"
   do j = 1,number_plankt_vari_vert ! all vertically distributed planktonic variables
      write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(plankt_vari_vert_name(j))
   end do ! done all plankt_vari_vert
   write(104,'(A)')"# bentic distributions"
   do j = 1,number_benth_distr ! all benthic distributions
      write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(benth_distr_name(j))
   end do ! done all benthic distributions
   write(104,'(A)')"# global transfer variables"
   do j = 1,number_trans_val  ! alle globalen Übergabe Werte
      write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(trans_val_name(j))
   end do
   write(104,'(A)')"# depth averaged transfer variables"
   do j = 1,number_trans_quant ! all exchange con.
      write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(trans_quant_name(j))
   end do
   write(104,'(A)')"# depth resolving transfer variables"
   do j = 1,number_trans_quant_vert  ! all vertically distributed transfer quantities
      write(104,'(A1,7x,I4,2x,A18)')"0",j,trim(trans_quant_vert_name(j))
   end do
   close (104)
   return
end subroutine ausgabekonzentrationen_beispiel
!----+-----+----
!> Die suboutine ausgabezeitpunkte() ließt Datei ausgabezeitpunkte.txt und schreibt Feld ausgabe_zeitpunkt
!! \n\n
subroutine ausgabezeitpunkte()
   use modell
   implicit none
   integer :: n, ion, open_error, io_error, alloc_status, nba,ierr
   character (len = 200) :: dateiname
   !integer :: n_ausgabe
   !integer , allocatable , dimension (:) :: ausgabe_punkt
   write(dateiname,'(2A)')trim(modellverzeichnis),'ausgabezeitpunkte.txt'
   ion = 103
   open ( unit = ion , file = dateiname, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0) then
      print*,'keine Ausgabezeitpunkte, open_error = ',open_error
      n_ausgabe = 0
      close (ion)
      return
   end if ! open_error.ne.0
   n = 0
   do while ( zeile(ion))
      if (ctext(1:1) /= '#') then ! keine Kommentarzeile
         !print*,'1 ',trim(ctext)
         n = n+1
         read(ctext,*, iostat = io_error)tag, monat, jahr, stunde, minute, sekunde !, uhrzeit_stunde
         if (io_error /= 0) then
            print*,'unlesbare Angabe in ausgabezeitpunkte.txt'
            write(fehler,*)trim(ctext)
            call qerror(fehler)
         end if ! io_error.ne.0
         !call sekundenzeit()
      endif ! keine Kommentarzeile
   end do ! zeile
   n_ausgabe = n
   print*,n_ausgabe,' Ausgabezeitpunkte'
   allocate (ausgabe_zeitpunkt(n_ausgabe), stat = alloc_status )
   allocate (ausgabe_bahnlinie(n_ausgabe), stat = alloc_status )
   rewind (ion)
   n = 0
   do while ( zeile(ion))
      if (ctext(1:1) /= '#') then ! keine Kommentarzeile
         !print*,'2 ',trim(ctext)
         n = n+1
         read(ctext,*, iostat = io_error)tag, monat, jahr, stunde, minute, sekunde !, uhrzeit_stunde
         print*,"ausgabezeitpunkt = ",tag, monat, jahr, stunde, minute, sekunde
         call sekundenzeit(1)
         !call zeitsekunde() !! damit auch die Uhrzeit stimmt
         ausgabe_zeitpunkt(n) = zeitpunkt
         print*,'Ausgabezeitpunkt ',n,' Datum: ', tag, monat, jahr,' ; Uhrzeit', stunde, minute, sekunde,  &
         ' uhrzeit_stunde = ',uhrzeit_stunde,  &
         'Stunden |  ergibt: ',zeitpunkt,' Sekunden seit ', trim(time_offset_string)
         if (zeitpunkt < startzeitpunkt)print*,'### keine Ausgabe ### liegt vor dem startzeitpunkt. \n'
         if (zeitpunkt > endzeitpunkt)print*,'### keine Ausgabe ### liegt nach dem endzeitpunkt. \n'
         read(ctext,*, iostat = io_error)tag, monat, jahr, stunde, minute, sekunde, nba
         if (io_error == 0) then
            ausgabe_bahnlinie(n) = nba
            print*,'mit Bahnlinienausgabe ',ausgabe_bahnlinie(n)
         else
            ausgabe_bahnlinie(n) = 0
         end if ! io_error.ne.0
      endif ! keine Kommentarzeile
   end do ! zeile
   close (ion)
   return
end subroutine ausgabezeitpunkte
!-----+-----+-----+-----+
!> true if output required now
!! \n\n
subroutine ausgeben_parallel()
   use modell
   implicit none
   integer :: alloc_status,ierr
   !print*,meinrang,'ausgeben_parallel() n_ausgabe=',n_ausgabe
   call MPI_Bcast(n_ausgabe,1,MPI_INT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) then
      write(fehler,*)'14  ',meinrang, 'MPI_Bcast(n_ausgabe,  ierr = ', ierr
      call qerror(fehler)
   end if
   !print*,'MPI_Bcast(n_ausgabe gemacht',meinrang
   if (meinrang /= 0) then
      allocate (ausgabe_zeitpunkt(n_ausgabe), stat = alloc_status )
      allocate (ausgabe_bahnlinie(n_ausgabe), stat = alloc_status )
   end if
   call MPI_Bcast(ausgabe_zeitpunkt,n_ausgabe,MPI_INT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) then
      write(fehler,*)meinrang, 'MPI_Bcast(ausgabe_zeitpunkt,  ierr = ', ierr
      call qerror(fehler)
   end if
   !print*,'MPI_Bcast(ausgabe_zeitpunkt gemacht',meinrang
   call MPI_Bcast(ausgabe_bahnlinie,n_ausgabe,MPI_INT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) then
      write(fehler,*)meinrang, 'MPI_Bcast(ausgabe_bahnlinie,  ierr = ', ierr
      call qerror(fehler)
   end if
   !print*,'MPI_Bcast(ausgabe_bahnlinie gemacht',meinrang
   return
end subroutine ausgeben_parallel
!-----+-----+-----+-----+
!> true if output required now
!! \n\n
logical function jetzt_ausgeben()
   use modell
   implicit none
   integer :: n , diff,ierr
   jetzt_ausgeben = .false.
   bali = .false.
   !if(hydro_trieb.eq. 3)then
   !   jetzt_ausgeben=.FALSE.
   !   if(meinrang.eq. 0)print*,'SCHISM preliminary: no output for all timesteps'
   !   return
   !end if ! SCHISM
   
   do n = 1,n_ausgabe,1
      diff = ausgabe_zeitpunkt(n)-rechenzeit
      if ( (diff >= (-1*(deltat/2))) .and. (diff < (deltat/2)) ) then
         jetzt_ausgeben = .TRUE.
         if (ausgabe_bahnlinie(n) /= 0) bali = .TRUE.
      end if !
      !print*,'ausgeben? ', rechenzeit, ausgabe_punkt(n), deltat, (rechenzeit-ausgabe_punkt(n))
      !if(((rechenzeit-ausgabe_punkt(n)).lt.deltat).and.((rechenzeit-ausgabe_punkt(n)).ge.0))then
      !if(((rechenzeit-ausgabe_punkt(1)).lt.deltat).and.((rechenzeit-ausgabe_punkt(1)).ge.0))then
      !   print*,'jetzt jede Stunde ausgeben'
      !   ausgabe_punkt(1)=ausgabe_punkt(1)+3600
      !   jetzt_ausgeben=.TRUE.
      !end if !
   end do
   !if(.not.jetzt_ausgeben)jetzt_ausgeben=(zeitschrittanzahl.eq.izeit) !! Ausgabe am Ende
   if (jetzt_ausgeben)print*,'jetzt_ausgeben ,meinrang',meinrang
   return
end function jetzt_ausgeben
!----+-----+----
!> Initialisierung der transportierten Übergabe-Konzentrationen.
!! \n\n
subroutine ini_aus(nk)
   use modell
   implicit none
   integer nk,k,n,as,j,ierr
   if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten
      knotenanzahl_ausgabe = nk
      anzahl_auskonz = 1
      allocate (AusgabeKonzentrationsName(anzahl_auskonz), stat = as )
      if (as /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate AusgabeKonzentrationsName :', as
         call qerror(fehler)
      end if
      AusgabeKonzentrationsName( 1) = "            BACmua"
      !!!!!!!!! ausgabe_konzentration allokieren und initialisieren
      allocate (ausgabe_konzentration(anzahl_auskonz,knotenanzahl_ausgabe), stat = as )
      if (as /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate transfer_quantity :', as
         call qerror(fehler)
      end if
      do k = 1,knotenanzahl_ausgabe ! alle knoten
         do j = 1,anzahl_auskonz ! initialisierung aller konzentrationen zunächt auf Null
            ausgabe_konzentration(j,k) = 0.0
         end do
      end do
   end if !! nur prozessor 0
end subroutine ini_aus
!----+-----+----
!> ELCIRC .grd Format ausgabe momentan Sept15 Überstaudauern für Elbestabil
!! \n\n
subroutine aus_grd()
   use modell
   implicit none
   integer :: ion, open_error, io_error, n,ierr
   character (len = 200) :: dateiname
   if ( .not. uedau_flag) return !! Überstaudauern nur ausgeben wenn parameter in module_modell.f95 gesetzt
   if (uedau_flag) call qerror(" aus_grd() Überstaudauer nicht mehr implementiert")
   if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten
      write(dateiname,'(2A)')trim(modellverzeichnis),'uedau0.grd'
      ion = 107
      open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
      if (open_error /= 0) then
         print*,'uedau0.grd, open_error = ',open_error
         close (ion)
         return
      end if ! open_error.ne.0
      
      write(ion,'(A)') 'Grid written by QSim3D'
      write(ion,'(I9,2x,I9)')n_elemente, knotenanzahl2D
      
      do n = 1,knotenanzahl2D
         ! write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
         write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n),   &
                                                     benthic_distribution(44+(n-1)*number_benth_distr)  !! Überstaudauer
      end do ! alle Knoten
      
      do n = 1,n_elemente ! alle Elemente
         if (cornernumber(n) == 3) then
            write(ion,'(5(I8,2x))') &
                                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
         end if
         if (cornernumber(n) == 4) then
            write(ion,'(6(I8,2x))') &
                                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
         end if
      end do ! alle Elemente
      
      close (ion)
      print*,'Überstaudauer 0-15 cm (44) ausgegeben auf: uedau0.grd'
      !!!!!!!!!
      write(dateiname,'(2A)')trim(modellverzeichnis),'uedau15.grd'
      ion = 107
      open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
      if (open_error /= 0) then
         print*,'uedau15.grd, open_error = ',open_error
         close (ion)
         return
      end if ! open_error.ne.0
      
      write(ion,'(A)') 'Grid written by QSim3D'
      write(ion,'(I9,2x,I9)')n_elemente, knotenanzahl2D
      
      do n = 1,knotenanzahl2D
         ! write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
         write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n),   &
                                                     benthic_distribution(45+(n-1)*number_benth_distr)  !! Überstaudauer
      end do ! alle Knoten
      
      do n = 1,n_elemente ! alle Elemente
         if (cornernumber(n) == 3) then
            write(ion,'(5(I8,2x))') &
                                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
         end if
         if (cornernumber(n) == 4) then
            write(ion,'(6(I8,2x))') &
                                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
         end if
      end do ! alle Elemente
      
      close (ion)
      print*,'Überstaudauer 15-25 cm (45) ausgegeben auf: uedau15.grd'
      !!!!!!!!!
      write(dateiname,'(2A)')trim(modellverzeichnis),'uedau25.grd'
      ion = 107
      open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
      if (open_error /= 0) then
         print*,'uedau25.grd, open_error = ',open_error
         close (ion)
         return
      end if ! open_error.ne.0
      
      write(ion,'(A)') 'Grid written by QSim3D'
      write(ion,'(I9,2x,I9)')n_elemente, knotenanzahl2D
      
      do n = 1,knotenanzahl2D
         ! write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
         write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n),   &
                                                     benthic_distribution(46+(n-1)*number_benth_distr)  !! Überstaudauer
      end do ! alle Knoten
      
      do n = 1,n_elemente ! alle Elemente
         if (cornernumber(n) == 3) then
            write(ion,'(5(I8,2x))') n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
         end if
         if (cornernumber(n) == 4) then
            write(ion,'(6(I8,2x))') n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
         end if
      end do ! alle Elemente
      
      close (ion)
      print*,'Überstaudauer 25-35 cm (46) ausgegeben auf: uedau25.grd'
      
      
      write(dateiname,'(2A)')trim(modellverzeichnis),'uedau35.grd'
      ion = 107
      open ( unit = ion , file = dateiname, status = 'unknown', action = 'write ', iostat = open_error )
      if (open_error /= 0) then
         print*,'uedau35.grd, open_error = ',open_error
         close (ion)
         return
      end if ! open_error.ne.0
      
      write(ion,'(A)') 'Grid written by QSim3D'
      write(ion,'(I9,2x,I9)')n_elemente, knotenanzahl2D
      
      do n = 1,knotenanzahl2D
         ! write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n), p(n) !! Wasserspiegellage
         write(ion,'(I9,2x,f11.4,2x,f11.4,2x,f11.4)')n, knoten_x(n), knoten_y(n),   &
                                                     benthic_distribution(47+(n-1)*number_benth_distr)  !! Überstaudauer
      end do ! alle Knoten
      
      do n = 1,n_elemente ! alle Elemente
         if (cornernumber(n) == 3) then
            write(ion,'(5(I8,2x))') &
                                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
         end if
         if (cornernumber(n) == 4) then
            write(ion,'(6(I8,2x))') &
                                n, cornernumber(n),elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
         end if
      end do ! alle Elemente
      
      close (ion)
      print*,'Überstaudauer 35-undendl. cm (47) ausgegeben auf: uedau35.grd'
   end if !! nur prozessor 0
   return
end subroutine aus_grd

!----+-----+----+-----+----+-----+----+-----+----+-----+----
!> raus ist true, wenn in diesem Zeitschritt das Geschwindigkeitsfeld ausgegeben werden soll \n\n
!! \n\n
!      SUBROUTINE ausgabezeitpunkt(raus)
!      use modell
!      implicit none
!      logical :: raus
!      raus=.TRUE.
!      return
!      END SUBROUTINE ausgabezeitpunkt
