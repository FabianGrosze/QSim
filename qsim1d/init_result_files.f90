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
subroutine init_result_files(cpfad, modell, cEreig, write_csv_output, output_strang, output_querprofil, anz_csv_output)
!subroutine init_result_files(cpfad, modell, cEreig, write_csv_files)

   use allodim
   implicit none
   
   ! --- dummy arguments ---
   character(len = 255), intent(in) :: cpfad           !< path to directory for output
   character(len = *),   intent(in) :: modell          !< modelname (Gerris)
   character(len = 255), intent(in) :: cEreig          !< meta data (Gerris)
   logical, intent(in)              :: write_csv_output !< switch to turn of .csv-outputs
   integer, dimension(output_crossections) :: output_strang, output_querprofil
   integer                                 :: anz_csv_output

   ! --- local variables ---
   character(275) :: pfadstring
   character(8)   :: versionstext
   integer        :: open_error, u_file1
   
   external :: version_string, qerror, ergebmformat, ergebtformat
  
   
   print *, ''
   print *, repeat('=', 78)
   print *, repeat(' ', 33), 'init output'
   print *, repeat('=', 78)
   print *, 'Creating files for results:'
   
   ! get current version number
   call version_string(versionstext)
   
   !--- ErgebM.txt ---
   print*, '   * ErgebM.txt'
   pfadstring = trim(adjustl(cpfad)) // 'ERGEBM.txt'
   open(unit = 45, file = pfadstring, iostat = open_error)
   if (open_error /= 0) call qerror("Could not open ERGEBM.txt")
      
   write(45,'(a,a)')'*V  QSim  ERGEBM  ', versionstext
   call ergebMFormat()
   write(45,'(a50)')  modell
   write(45,'(a255)') cEreig
   
   ! --- ErgebT.txt ---
   print*, '   * ErgebT.txt'
   pfadstring =  trim(adjustl(cpfad)) // 'ERGEBT.txt'
   open(unit = 155, file = pfadstring, iostat = open_error)
   if (open_error /= 0) call qerror("Could not open ERGEBT.txt")
   
   write(155,'(a,a)') '*V  QSim  ERGEBT  ', versionstext
   call ergebTFormat()
   write(155,'(a50)')modell
   write(155,'(a255)')cEreig
   
   ! --- Ergeb2D.txt ---
   print*, '   * Ergeb2D.txt'
   pfadstring =  trim(adjustl(cpfad)) // 'ERGEB2D.txt'
   open(unit = 255, file = pfadstring, iostat = open_error)
   close(255)

   
   if (write_csv_output) then 
      call ausgabe_querprofil(cpfad, modell, cEreig, write_csv_output, output_strang, output_querprofil, anz_csv_output)
   
      ! --- Ausgabe 156 ---
      print*, '> ausgabe156.csv'
      pfadstring =  trim(adjustl(cpfad)) // 'ausgabe156.csv'
      open(unit = 156, file = pfadstring, iostat = open_error)
      write(156,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; iior ; Stakm ; STRID ; vbsb ; vcsb ; vnh4 ; vno2 ; vno3 ; gsN ; gelp ;  &
                     & gsP ; Si ; chla ; zooin ; vph ; mw ; ca ; lf ; ssalg ; tempw ; vo2 ; CHNF ; coli ; Dl ; dsedH ; tracer'

      ! --- Ausgabe 157 Schwermetalle ---
      print*, '> ausgabe157_schwermetalle.csv'
      pfadstring =  trim(adjustl(cpfad)) // 'ausgabe157_schwermetall.csv'
      open(unit = 157, file = pfadstring, iostat = open_error)
      
      write(157,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; Stakm ; STRID ; gsPb ; glPb ; gsCad ; glCad ; gsCr ; glCr ; gsFe ; &
                     & glFe ; gsCu ; glCu ; gsMn ; glMn ; gsNi ; glNi ; gsHg ; glHg ; gsU ; glU ; gsZn ; glZn ; gsAs ; glAs ;     &
                     & SSeros; sedalk; sedalg; sedalb; sedss; tau; tausc'
      
      ! --- Ausagbe 158 Algae ---
      print*, '> ausgabe158_algae.csv'
      pfadstring = trim(adjustl(cpfad)) // 'ausgabe158_algae.csv'
      open(unit = 158, file = pfadstring, iostat = open_error)
      
      write(158,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; Stakm ; STRID ; O2 ; chla ;&
                      &aki ; agr ; abl ; chlak ; chlag ; chlab ; ssalg ; ss'
   endif
   
   ! remove file1.err, which might still exist from previous run
   pfadstring = trim(adjustl(cpfad)) // 'file1.err'
   open(newunit = u_file1, file = trim(adjustl(cpfad)) // 'file1.err')
   close(u_file1, status = "delete")
      
end subroutine init_result_files

!> reads ausgabe_querprofile.txt in order to restrict output to certain cross-sections
!! Jens Wyrwa 2022
!!
subroutine ausgabe_querprofil(cpfad, modell, cEreig, write_csv_output, output_strang, output_querprofil, anz_csv_output)

   use allodim
   implicit none
   character(275) :: pfadstring,fehler
   character(len = 255), intent(in) :: cpfad           !< path to directory for output
   character(len = *),   intent(in) :: modell          !< modelname (Gerris)
   character(len = 255), intent(in) :: cEreig          !< meta data (Gerris)
   logical, intent(in)              :: write_csv_output !< switch to turn of .csv-outputs
   character (len = 2000) :: ctext

   ! --- local variables ---
   integer        :: open_error, ionumber, nn, alloc_status, io_error
   integer, dimension(output_crossections) :: output_strang, output_querprofil
   integer                                 :: anz_csv_output
   logical zeile

   anz_csv_output=0
   nn = 0
   ionumber = 777

   pfadstring =  trim(adjustl(cpfad)) // 'ausgabe_querprofile.txt'
   open ( unit = ionumber , file = pfadstring, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0) then
      print*,'keine ausgabe_querprofile.txt'
      return
   end if ! open_error.ne.0
   
   do while ( zeile(ionumber,ctext)) !! zunächst Anzahl der Ganglinien feststellen
      anz_csv_output = anz_csv_output+1
      read(ctext,*,iostat = io_error)nn
      if (io_error /= 0) then
         write(fehler,*)'nn nicht richtig aus ausgabe_querprofile.txt gelesen'
         call qerror(fehler)
      end if ! io_error.ne.0
   end do ! zeile
   print*,anz_csv_output,' querprofile aus ausgabe_querprofile.txt gelesen'

   if(anz_csv_output > output_crossections)then
      write(fehler,*)'ausgabe_querprofil too many cross-sections anz_csv_output>output_crossections:',anz_csv_output,output_crossections
      call qerror(fehler)
   endif

   rewind (ionumber) ! ausgabe_querprofile.txt zurückspulen
   nn=0
   output_strang=0
   output_querprofil=0
   do while ( zeile(ionumber,ctext)) !! all lines
      nn = nn+1
      if (nn > anz_csv_output) then
         write(fehler,*)'Fehler bei ausgabe_querprofile.txt nochmal lesen ',nn,anz_csv_output
         call qerror(fehler)
      end if
      read(ctext,*,iostat = io_error)output_strang(nn), output_querprofil(nn)
      if (io_error /= 0) then
         write(fehler,*)'reading output_strang, output_querprofil failed  nn,alloc_status= ',nn,alloc_status
         call qerror(fehler)
      end if !
   end do ! alle zeilen
   close (ionumber) ! ausgabe_querprofile.txt wieder geschlossen
   print*,' ausgabe_querprofil finished | anz_csv_output = ',anz_csv_output

end subroutine ausgabe_querprofil

   !----+-----+----
   !> Dient dem Einlesen der nächsten nicht-#-Kommentar Zeile \n\n
   !! \n\n
   logical function zeile(ion,ctext)
      implicit none
      character (len = 2000) :: ctext
      integer :: io_error, ion
      zeile = .FALSE.
      do
         read(ion, '(A)', iostat = io_error ) ctext
         if (io_error /= 0) then
            !!print*,'io_error SUBROUTINE zeile'
            zeile = .FALSE.
            return
         end if ! io_error.ne.0
         if (ctext(1:1) /= '#') exit
      end do ! alle Zeilen
      zeile = .TRUE.
      return
   end function zeile
