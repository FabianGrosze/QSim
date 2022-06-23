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
!> Berechnung der Lufttemperatur für eine gegebene Uhrzeit aus der Tagesmaximums-
!! und minimumstemperatur
!!
!! Literatur: W.J. Parton and J.A. Logan: A MOdel for Diurnal variation in Soil and Air
!!            Temperature. - Agricultural Meteorology, 23, S.205-216 (1981)
subroutine temperl(SA,SU,Uhrz,TEMPL,mstr,IDWe,TLMAX,TLMIN,anze,imet,azStrs)
  
   integer                             :: anze, azStrs
   integer, dimension(azStrs,1000)     :: IDWe
   real, dimension(1000), intent(out)  :: templ !< Temperatur zu einer bestimmten Uhrzeit
   real, dimension(20), intent(in)     :: tlMax !< Tagesmaximum der Temperatur
   real, dimension(20), intent(in)     :: tlMin !< Tagesminimum der Temperatur
  
   ! BBD = Zeit seit SA in h
   ! BBD = Zeit seit SU in h
   
   A = 2. ! zeiliche Verschiebung des Temperaturmaximums [h]
   B = 3. ! Koeffizient der die Temperaturabnahme während der Nacht beschreibt [-]
   
   ! Dauer des Lichttages[h]
   ADY = SU-SA
   ! Dauer der Nacht [h]
   ANI = 24.-SU+SA
   
   do ior = 1,anze+1
      if (imet == 1) then ! Werte der Lufttemperatur liegen im Berechnungszeittakt vor.
         templ(ior) = TLMAX(IDWe(mstr,ior))
      
      else
         TMX = TLMAX(IDWe(mstr,ior))
         TMN = TLMIN(IDWe(mstr,ior))
         if (Uhrz <= SU .and. Uhrz>=SA) then ! Festlegung ob Tag oder Nacht
            BBD = Uhrz-SA
            Templ(ior) = (TMX-TMN)*SIN((3.14*BBD)/(ADY + 2.*A)) +TMN
         else
            if (Uhrz > SA) then
               BBD = Uhrz-SU
            else
               BBD = 24.-SU+Uhrz
            endif
            TSN = (TMX-TMN)*SIN((3.14*ADY)/(ADY + 2.*A))+TMN
            Templ(ior) = TMN + (TSN-TMN)*EXP(-B*BBD/ANI)
         endif
      endif
   enddo
end subroutine temperl
