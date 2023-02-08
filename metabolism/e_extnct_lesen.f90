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

!> e_extnct_lesen
!!
!! ließt die 
!! Absorptionsspektren sigma(Lambda) fuer Wasser, Kiesel-,Gruen- und Blaualgen, 
!! Humin, susp. Schwebstoff,Sonnenlicht\n
!! aus <a href="../../exp/e_extnct.dat" target="_blank">e_extnct.dat</a>
!! 
!!
!! Quelle: e_extnct_lesen.f90 , zu: Stoffumsatz ; algae_huelle(), 
!! \ref lnk_randbedingungen , \ref lnk_extnct_rb

subroutine e_extnct_lesen(ilamda,eta,aw,ack,acg,acb,ah,as,al,cpfad)
   implicit none

   character(500)       :: dateiname, text
   character(255)       :: cpfad
   integer              :: io_error
   integer              :: ilamda, i
   real, dimension(40)  :: eta, aw, ack, acg, acb, ah, as, al
   
   external :: qerror
   
   ! Einlesen der e_extnct.dat
   ! wird wieder aktiviert wenn Datei in Gerris erzeugt wird
   ! open(unit=101, DEFAULTFILE=cpfad, file='e_extnct.dat')
   ! open(unit=101, DEFAULTFILE='/GERRIS/QSIM/', file='e_extnct.dat')
   
   write(dateiname,'(2A)')trim(cpfad),'e_extnct.dat'
   open(unit = 101 , file = dateiname, status = 'old', action = 'read ', iostat = io_error)
   if (io_error /= 0) call qerror("Could not open e_extnct.dat")
   
   rewind (101)
   ! read(101,'(A2)')ckenn_vers1
   ! if(ckenn_vers1/='*V')then
   ! else
   !    read(101,'(2x)')
   ! endif
   ! read(101,'(i2)') ilamda
   read(101,'(A)') text
   !print*,'e_extnct.dat: Kopfzeile ...'
   !print*,trim(text)
   
   ! Anzahl der Wellenlängen
   read(101,*, iostat = io_error) ilamda
   if (io_error /= 0) call qerror("Could not read ilamda from e_extnct.dat")
   
   if (ilamda > 40) call qerror("Number of wavelengths in e_extnct.dat exceeds 40")
   
   do i = 1,ilamda
      ! Wellenlängenabhängige Extinctionskoeffizienten
      read(101,*, iostat = io_error) eta(i),aw(i),ack(i),acg(i),acb(i),ah(i),as(i),al(i) 
      
      if (io_error /= 0) call qerror("Error while reading from e_extnct.dat")
      
   enddo
   !! print*,'e_extnct.dat: ',ilamda,' Zeilen gelesen'
   return
end subroutine e_extnct_lesen
