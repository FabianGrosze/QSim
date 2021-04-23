subroutine Temperl(SA,SU,Uhrz,TEMPL,mstr,IDWe,TLMAX,TLMIN,anze,imet,azStrs)
!
!
! Diese subroutine berechnet die Lufttemperatur f�r eine gegebene Uhrzeit
! Literatur: W.J. Parton and J.A. Logan: A MOdel for Diurnal variation in Soil and Air
!            Temperature. - Agricultural Meteorology, 23, S.205-216 (1981)
!
! aus der Tages Maximum und Minimum-Temperatur
! TMX = MAXIMUM TEMPERATUR
! TMN = MINIMUM TEMPERATUR
! Templ = TEMPERATUR zu einer bestimmten Uhrzeit
! A = zeiliche Verschiebung des Temperaturmaximums in h
! B = Koeffizient der die Temperaturabnahme w�hrend der Nacht beschreibt
! ADY = Dauer des Lichttages in h
! ANI = Dauer der Nacht in h
! BBD = Zeit seit SA in h 
! BBD = Zeit seit SU in h
!   
 
  integer                          :: anze, azStrs 
  integer, Dimension(azStrs,1000)  :: IDWe
  real, Dimension(1000)            :: templ
  real, Dimension(20)              :: TLMAX, TLMIN
!
!
    A = 2. !in h
    B = 3. ! Dimensionslos
!
!
  ADY = SU-SA
  ANI= 24.-SU+SA
!
 do ior = 1,anze+1
    if(imet==1)then ! Werte der Lufttemperatur liegen im Berechnungszeittakt vor.
   templ(ior) = TLMAX(IDWe(mstr,ior))
      else
      TMX = TLMAX(IDWe(mstr,ior))
      TMN = TLMIN(IDWe(mstr,ior))
       If(Uhrz<=SU.and.Uhrz>=SA)then ! Festlegung ob Tag oder Nacht
       BBD = Uhrz-SA
       Templ(ior) = (TMX-TMN)*SIN((3.14*BBD)/(ADY + 2.*A)) +TMN
         else
         if(Uhrz>SA)then
         BBD = Uhrz-SU
            else
            BBD = 24.-SU+Uhrz
         endif  
         TSN =(TMX-TMN)*SIN((3.14*ADY)/(ADY + 2.*A))+TMN
         Templ(ior) = TMN + (TSN-TMN)*EXP(-B*BBD/ANI)
       endif 
    endif
 enddo

     end subroutine Temperl