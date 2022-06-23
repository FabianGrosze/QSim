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

!> <h2> subroutine e_extnct_lesen </h2>
!! ließt die \n
!! Absorptionsspektren sigma(Lambda) fuer Wasser, Kiesel-,Gruen- und Blaualgen, Humin, susp. Schwebstoff,Sonnenlicht\n
!! aus <a href="../../exp/e_extnct.dat" target="_blank">e_extnct.dat</a>
!! \n\n
!! Quelle: e_extnct_lesen.f90 , zu: Stoffumsatz ; algae_huelle(), \ref Randbedingungen , \ref extnct_rb

subroutine e_extnct_lesen(ilamda,eta,aw,ack,acg,acb,ah,as,al,cpfad)
   !!write(cpfad,'(A)')trim(modellverzeichnis)

   implicit none

   character(500)       :: dateiname, text, modellverz
   character(255)       :: cpfad
   integer              :: io_error
   integer              :: ilamda, i
   real, dimension(40)  :: eta, aw, ack, acg, acb, ah, as, al
   
   ! Einlesen der e_extnct.dat
   ! wird wieder aktiviert wenn Datei in Gerris erzeugt wird
   ! open(unit=101, DEFAULTFILE=cpfad, file='e_extnct.dat')
   ! open(unit=101, DEFAULTFILE='/GERRIS/QSIM/', file='e_extnct.dat')
   
   write(dateiname,'(2A)')trim(cpfad),'e_extnct.dat'
   open(unit = 101 , file = dateiname, status = 'old', action = 'read ', iostat = io_error)
   if (io_error /= 0) then
      print*,'io_error open e_extnct.dat'
      print*, 'Does this file exist?'
      stop 100
   end if
   
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
   if (io_error /= 0) then
      print*,'io_error ilamda aus e_extnct.dat nicht gelesen'
      stop 101
   end if
   
   if (ilamda > 40) then
      print*,'Fehler: Mehr als 40 Wellenlängen in e_extnct.dat angegeben. ilamda = ',ilamda
      stop 102
   endif
   
   do i = 1,ilamda
      ! Wellenlängenabhängige Extinctionskoeffizienten
      read(101,*, iostat = io_error) eta(i),aw(i),ack(i),acg(i),acb(i),ah(i),as(i),al(i) 
      
      if (io_error /= 0) then
         print*,'io_error beim Lesen von eta(i),aw(i),ack(i),acg... aus e_extnct.dat'
         stop 103
      end if
      
      ! if(i == 1) print*,'e_extnct.dat: Wellenlänge eta(1)=',eta(1),' Nano-Meter'
      ! if(i == ilamda) print*,'e_extnct.dat: Wellenlänge eta(',ilamda,')=',eta(ilamda),' Nano-Meter'
   enddo
   !! print*,'e_extnct.dat: ',ilamda,' Zeilen gelesen'
   return
end subroutine e_extnct_lesen
