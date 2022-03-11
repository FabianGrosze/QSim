!---------------------------------------------------------------------------- !
!   QSim - Programm zur Simulation der Wasserqualität                         !
!                                                                             !
!   Copyright (C) 2020                                                        !
!       Bundesanstalt für Gewässerkunde                                       !
!       Koblenz (Deutschland)                                                 !
!       http://www.bafg.de                                                    !
!                                                                             !
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen   !
!   der GNU General Public License, Version 3, wie von der Free Software      !
!   Foundation veröffentlicht, weitergeben und/oder modifizieren.             !
!                                                                             !
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es    !
!   Ihnen von Nutzen sein wird, aber OHNE IRGENDEINE GARANTIE, sogar ohne die !
!   implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN       !
!   BESTIMMTEN ZWECK.                                                         !
!                                                                             !
!   Details finden Sie in der GNU General Public License.                     !
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit      !
!   diesem Programm erhalten haben.                                           !
!   Falls nicht, siehe http://www.gnu.org/licenses/.                          !
!                                                                             !
!    Programmiert von:                                                        !
!    1979 bis 2018 Volker Kirchesch                                           !
!    seit 2011 Jens Wyrwa, Wyrwa@bafg.de                                      !
! -----------------------------------------------------------------------------

SUBROUTINE anzTag(monat, jahr, tage) 
                                                                        
    ! Ein Programm zur Bestimmung der Tageszahl eines Monats

    ! Autor: Volker Kirchesch
    ! Stand: 22.09.2010  
    ! Ergänzung Okt. 2021 Michael Schönung	
                                                                       

    implicit none
    
    integer, intent(in)     :: monat, jahr
    integer, intent(out)    :: tage
    
    integer                 :: schalttag
    
    
    ! Schaltjahr bestimmen:
    ! Ein Jahr ist ein Schaltjahr, wenn es durch 4 teilbar ist.
    ! Falls sich die Jahrzahl durch 100 teilen lässt, ist es kein Schaltjahr.
    ! Falls auch eine Teilung durch 400 möglich ist, dann ist es ein Schaltjahr.
    if (mod(jahr, 400) == 0) then
        schalttag = 1
    else if (mod(jahr,100) == 0) then
        schalttag = 0
    elseif (mod(jahr, 4) == 0) then
        schalttag = 1
    else
        schalttag = 0
    endif
    
    
    select case (monat)
        case(1)  
            tage = 31
        case(2)  
            tage = 28 + schalttag
        case(3)  
            tage = 31
       case(4)  
            tage = 30
       case(5)  
            tage = 31
       case(6)  
            tage = 30
       case(7)  
            tage = 31
       case(8)  
            tage = 31
       case(9)  
            tage = 30
       case(10) 
            tage = 31
       case(11) 
            tage = 30
       case(12) 
            tage = 31
       case default
            print*,'Fehlerhafte Monatsangabe im Datum.'
    end select

    return 
end subroutine anzTag
