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
subroutine init_result_files(cpfad, modell, cEreig, write_csv_files)
   implicit none
   
   ! --- dummy arguments ---
   character(len = 255), intent(in)   :: cpfad           !< path to directory for output
   character(len = 255), intent(in)   :: modell          !< modelname (Gerris)
   character(len = 255), intent(in)   :: cEreig          !< meta data (Gerris)
   logical, intent(in)                :: write_csv_files !< switch to turn of .csv-outputs
   
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
   
   
   write(45,*)'*V  QSim  ERGEBM  ', versionstext
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
   
   write(155,*) '*V  QSim  ERGEBT  ', versionstext
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
   
   write(255,*) '*V  QSim  ERGEB2D  ', versionstext
   call ergeb2DFormat()
   write(255,'(a50)')modell
   write(255,'(a255)')cEreig
   
   if (write_csv_files) then 
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
                       SSeros; sedalk; sedalg; sedalb; sedss'
      
      ! --- Ausagbe 158 Algae ---
      print*, '> ausgabe158_algae.csv'
      pfadstring = trim(adjustl(cpfad)) // 'ausgabe158_algae.csv'
      open(unit = 158, file = pfadstring, iostat = open_error)
      
      write(158,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; Stakm ; STRID ; O2 ; chla ; aki ; agr ; abl ; chlak ; chlag ; chlab ; &
                       ssalg ; ss'
   endif
   
   print*, repeat('-', 78)
end subroutine init_result_files