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
subroutine randbedingungen(cpfad, i_Rands, iw_max)
   implicit none
   
   ! --- dummy arguments ---
   character(len=255), intent(in) :: cpfad
   integer, intent(out)           :: i_rands
   integer, intent(out)           :: iw_max
   
   ! --- local varaibles ---
   character(len=275) :: pfadstring
   integer            :: read_error,open_error
   integer            :: mstr,i_hcon,iws_RB,itagl,iwe
   integer            :: RBNR
   
   external           :: qerror
   
   
   pfadstring =  trim(adjustl(cpfad)) // 'EREIGG.txt'
   open(unit = 92, file = pfadstring, iostat = open_error)
   if (open_error /= 0) call qerror("Could not open EreigG.txt")
   rewind (92)
   
   ! skip file header
   read(92,'(2x)')
   read(92,'(2x)')
   read(92,'(2x)')
   read(92,'(2x)')
   read(92,'(2x)')
   read(92,'(2x)')
   
   
   iw_max = 0
   i_rands = 0
   do 
      ! read boundary header 
      ! mstr      Nummer des Strangs der Randbedingung gemäß MODELLA.txt
      ! rbnr      Nummer der Randbedingung im Strang gemäß MODELLA.txt
      ! i_hcon    Art der Eingabewerte der Randbedingung: (0 = Tagesmittelwerte, 1 = uhrzeitbezogene Einzelwerte)
      ! iws_rb    Anzahl der folgenden Zeitpunkt-Wert-Zeilen der Randbedingung
      read(92,9230,iostat = read_error) mstr, RBNR, i_hcon, iws_RB
      ! eof
      if (read_error < 0.0) exit
      
      if (iws_RB == 0) cycle
      if (iws_RB > iw_max)iw_max = iws_RB
      ! Summenbildung der Randbedingungen
      i_Rands = i_Rands + 1
      
      do iwe = 1,iws_RB
          ! Einlesen der Randbedingungswerte für den Strang <mstr>
          read(92,'(I2)') itagl
      enddo
   enddo ! Randbedingungsschleife Ende
   
   9230 format(I5,2x,I5,2x,I1,2x,I5)
   
   close(92)
   
   print '(a)'    , 'subroutine randbedingungen():'
   print '(a, I0)', 'boundaries found in EREIGG.txt: ', i_rands
   
end subroutine randbedingungen
