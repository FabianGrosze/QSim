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

!> Creates all files needed to store model results
!!
!! * ErgebM.txt
!! * ErgebT.txt
!! * Ergeb2D.txt
!! * ausgabe156.csv (optional)
!! * ausgabe157_schwermetalle.csv (optional)
!! * ausgabe158_algae.csv (optional)
!! @author Michael Schönung
!! @date 20.06.2022
subroutine init_result_files(cpfad, modell, cEreig, write_csv_files, output_strang, output_querprofil, anz_csv_output)

   implicit none
   
   ! --- dummy arguments ---
   character(len = 255), intent(in)   :: cpfad           !< path to directory for output
   character(len = *),   intent(in)   :: modell          !< modelname (Gerris)
   character(len = 255), intent(in)   :: cEreig          !< meta data (Gerris)
   logical, intent(in)                :: write_csv_files !< switch to turn of .csv-outputs
   integer, dimension(:), allocatable      :: output_strang, output_querprofil
   integer                                 :: anz_csv_output

   ! --- local variables ---
   character(len = 275)    :: pfadstring
   character(len = 8)      :: versionstext
   integer                 :: open_error
   
   print *, ''
   print *, repeat('=', 78)
   print *, repeat(' ', 33), 'init output'
   print *, repeat('=', 78)
   print *, 'creating files for results:'
   
   ! get current version number
   call version_string(versionstext)
   
   !--- ErgebM.txt ---
   print*, '> ErgebM.txt'
   pfadstring = trim(adjustl(cpfad)) // 'ERGEBM.txt'
   open(unit = 45, file = pfadstring, iostat = open_error)
   if (open_error /= 0) then
      print*,'unit = 45 open_error ERGEBM.txt ',cpfad,pfadstring
      stop 2
   end if
   
   
   write(45,'(a,a)')'*V  QSim  ERGEBM  ', versionstext
   call ergebMFormat()
   write(45,'(a50)')  modell
   write(45,'(a255)') cEreig
   
   ! --- ErgebT.txt ---
   print*, '> ErgebT.txt'
   pfadstring =  trim(adjustl(cpfad)) // 'ERGEBT.txt'
   open(unit = 155, file = pfadstring, iostat = open_error)
   if (open_error /= 0) then
      print*,'unit = 155 open_error ERGEBT.txt ',cpfad,pfadstring
      stop 2
   end if
   
   write(155,'(a,a)') '*V  QSim  ERGEBT  ', versionstext
   call ergebTFormat()
   write(155,'(a50)')modell
   write(155,'(a255)')cEreig
   
   ! --- Ergeb2D.txt ---
   print*, '> Ergeb2D.txt'
   pfadstring =  trim(adjustl(cpfad)) // 'ERGEB2D.txt'
   open(unit = 255, file = pfadstring, iostat = open_error)
   if (open_error /= 0) then
      print*,'unit = 255 open_error ERGEB2D.txt ',cpfad,pfadstring
      stop 2
   end if
   
   write(255,'(a,a)') '*V  QSim  ERGEB2D  ', versionstext
   call ergeb2DFormat()
   write(255,'(a50)')modell
   write(255,'(a255)')cEreig
   
   if (write_csv_files) then 
      !call ausgabe_querprofil(cpfad, modell, cEreig, write_csv_files, output_strang, output_querprofil, anz_csv_output)
      anz_csv_output=8
      allocate(output_strang(anz_csv_output), output_querprofil(anz_csv_output))
      output_strang(1)=1; output_querprofil(1)=9  ! Elbe-Km  4
      output_strang(2)=1; output_querprofil(2)=202  ! Elbe-Km 94,4 
      output_strang(3)=1; output_querprofil(3)=364  ! Elbe-Km 172,5
      output_strang(4)=1; output_querprofil(4)=450  ! Elbe-Km 214
      output_strang(5)=1; output_querprofil(5)=663  ! Elbe-Km 318
      output_strang(6)=2; output_querprofil(6)=810-663  ! Elbe-Km 389
      output_strang(7)=2; output_querprofil(7)=979-663  ! Elbe-Km 474,5
      output_strang(8)=2; output_querprofil(8)=1175-663  ! Elbe-Km 585,05

      ! --- Ausgabe 156 ---
      print*, '> ausgabe156.csv'
      pfadstring =  trim(adjustl(cpfad)) // 'ausgabe156.csv'
      open(unit = 156, file = pfadstring, iostat = open_error)
      write(156,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; Stakm ; STRID ; vbsb ; vcsb ; vnh4 ; vno2 ; vno3 ; gsN ; gelp ;  &
                       gsP ; Si ; chla ; zooin ; vph ; mw ; ca ; lf ; ssalg ; tempw ; vo2 ; CHNF ; coli ; Dl ; dsedH ; tracer'
      
      ! --- Ausgabe 157 Schwermetalle ---
      print*, '> ausgabe157_schwermetalle.csv'
      pfadstring =  trim(adjustl(cpfad)) // 'ausgabe157_schwermetall.csv'
      open(unit = 157, file = pfadstring, iostat = open_error)
      
      write(157,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; Stakm ; STRID ; gsPb ; glPb ; gsCad ; glCad ; gsCr ; glCr ; gsFe ; &
                       glFe ; gsCu ; glCu ; gsMn ; glMn ; gsNi ; glNi ; gsHg ; glHg ; gsU ; glU ; gsZn ; glZn ; gsAs ; glAs ;     &
                       SSeros; sedalk; sedalg; sedalb; sedss ; tau'
      
      ! --- Ausagbe 158 Algae ---
      print*, '> ausgabe158_algae.csv'
      pfadstring = trim(adjustl(cpfad)) // 'ausgabe158_algae.csv'
      open(unit = 158, file = pfadstring, iostat = open_error)
      
      write(158,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; Stakm ; STRID ; O2 ; chla ; aki ; agr ; abl ; chlak ; chlag ; chlab ; &
                       ssalg ; ss'
   endif
   
   print*, repeat('-', 78)
end subroutine init_result_files

subroutine ausgabe_querprofil(cpfad, modell, cEreig, write_csv_files, output_strang, output_querprofil, anz_csv_output)

   implicit none
   anz_gangl = 0
   nn = 0
   ionumber = 777
      pfadstring =  trim(adjustl(cpfad)) // 'ausgabe156.csv'
      open(unit = 156, file = pfadstring, iostat = open_error)
   write(dateiname,'(2A)')trim(modellverzeichnis),'ausgabe_querprofile.txt'
   open ( unit = ionumber , file = dateiname, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0) then
      print*,'keine ausgabe_querprofile.txt'
      return
   end if ! open_error.ne.0
   do while ( zeile(ionumber)) !! zunächst Anzahl der Ganglinien feststellen
      if (ctext(1:1) /= '#') then !! keine Kommentarzeile
         anz_gangl = anz_gangl+1
         read(ctext,*,iostat = io_error)knumm
         if (io_error /= 0) then
            write(fehler,*)'knumm nicht richtig aus ausgabe_querprofile.txt gelesen'
            call qerror(fehler)
         end if ! io_error.ne.0
         print*,'ausgabe_querprofile.txt  ',anz_gangl,' :', trim(ctext), ' #',knumm
      end if !! keine Kommentarzeile
   end do ! zeile
   rewind (ionumber) ! ausgabe_querprofile.txt zurückspulen
   allocate (knot_gangl(anz_gangl), stat = alloc_status )
   !allocate (c3Ammonium(anz_gangl), stat = alloc_status )
   !allocate (IntAmmonium(anz_gangl), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)'allocate knot_gangl(anz_gangl) fehlgeschlagen alloc_status = ', alloc_status
      call qerror(fehler)
   end if !
   do while ( zeile(ionumber)) !! alle Zeilen
      if (ctext(1:1) /= '#') then !! keine Kommentarzeile
         nn = nn+1
         if (nn > anz_gangl) then
            write(fehler,*)'Fehler bei ausgabe_querprofile.txt nochmal lesen ',nn,anz_gangl
            call qerror(fehler)
         end if
         read(ctext,*,iostat = io_error)knot_gangl(nn)
      end if !! keine Kommentarzeile
   end do ! alle zeilen
   close (ionumber) ! ausgabe_querprofile.txt wieder geschlossen
   print*,' ganglinien_lesen fertig | anz_gangl = ',anz_gangl
   
end subroutine ausgabe_querprofil
