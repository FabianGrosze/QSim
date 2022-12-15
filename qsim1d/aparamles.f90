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

!> Interpolation der Randbedingungen
!! @author Volker Kirchesch
!! @date 03.01.2013
subroutine aparamles(cpfad, itags, monats, jahrs, aggmax, akgmax, abgmax)
   
   implicit none
   
   character(len=255), intent(in) :: cpfad
   integer, intent(in)            :: itags
   integer, intent(in)            :: monats
   integer, intent(in)            :: jahrs
   real, intent(out)              :: aggmax
   real, intent(out)              :: akgmax
   real, intent(out)              :: abgmax
   
   character(len=275)         :: pfadstring
   integer                    :: anzDatum, nrs, nrsj, R_NR,  R_NRS, r__nrs, read_error
   integer                    :: i_datum, ip
   integer, dimension(1000)   :: itagp, monatp, jahrp
   real,    dimension(1000,3) :: wertp
   
   
   ! Einlesen aus aparamt.txt
   close (192)
   pfadstring = trim(adjustl(cpfad)) // 'aparamt.txt'
   open(unit = 192, file = pfadstring)
   rewind (192)
   
   if (monats > 2) then
      nrs = (itags+31*(monats-1)-int(0.4*monats+2.3))
   else
      nrs = itags+31*(monats-1)
   endif
   
   
   ! Tage seit 1900 (Berücksichtigung der Schaltjahre
   ! TODO (Schönung): Not all leap years are divisible by 4. So this calculation
   !                  is wrong for some years
   nrsj = (jahrs-1900)*365+int((jahrs-1900)/4) 
   
   r_nrs = nrs + nrsj
   read(192,'(i4)',iostat = read_error)anzdatum
   if (read_error < 0.0)anzdatum = 0
   
   if (anzdatum /= 0) then
      do i_datum = 1, anzdatum
         read(192,9245)itagp(i_datum),monatp(i_datum),jahrp(i_datum),(wertp(i_datum,ip),ip = 1,3)
         if (monatp(i_datum) > 2) then
            nrs = (itagp(i_datum)+31*(monatp(i_datum)-1)-int(0.4*monatp(i_datum)+2.3))
         else
            nrs = itagp(i_datum)+31*(monatp(i_datum)-1)
         endif
         nrsj = (jahrp(i_datum) - 1900)*365+int((jahrp(i_datum)-1900)/4)
         r_nr = nrs + nrsj
         write(89,*)itags,monats,jahrs,r__nrs
         if (r_nr <= r_nrs) then
            if (wertp(i_datum,1) > 0.0)aggmax = wertp(i_datum,1)
            if (wertp(i_datum,2) > 0.0)akgmax = wertp(i_datum,2)
            if (wertp(i_datum,3) > 0.0)abgmax = wertp(i_datum,3)
         endif
      enddo
   endif
   9245 format(i2,2x,i2,2x,i4,2x,f5.2,2x,f5.2,2x,f5.2)
end subroutine aparamles
