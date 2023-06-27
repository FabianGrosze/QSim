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

!> Berechnung des Makrophytenentwicklung im Jahresgang
subroutine macrophytes(pfl_s, pflmin_s, pflmax_s, tiefe_s,          &
                       tempw_s, schwi_s, extk_s,                    &
                       itags, monats, itstart, mstart, itmax, mmax, &
                       itend, mend, sa, su, tflie, po2p_s, po2r_s,  &
                       control, jjj)
                 
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout) :: pfl_s    !< macrophytes [g/m²]
   real,    intent(in)    :: pflmin_s !< minimum during winter [g/m²]
   real,    intent(inout) :: pflmax_s !< maximum during summer [g/m²]
   real,    intent(in)    :: tiefe_s  !< water depth
   real,    intent(in)    :: tempw_s  !< water temperature
   real,    intent(in)    :: schwi_s  !< global radiation [cal/(cm² h)]
   real,    intent(in)    :: extk_s   !< coefficient for light extinction
   integer, intent(in)    :: itags    !< current day of QSim simulation
   integer, intent(in)    :: monats   !< current month of QSim simulation
   integer, intent(in)    :: itstart  !<
   integer, intent(in)    :: mstart   !<
   integer, intent(in)    :: itmax    !<
   integer, intent(in)    :: mmax     !<
   integer, intent(in)    :: itend    !<
   integer, intent(in)    :: mend     !<
   real,    intent(in)    :: sa       !< time of sunrise
   real,    intent(in)    :: su       !< time of sunset
   real,    intent(in)    :: tflie    !< timestep [d]
   real,    intent(out)   :: po2p_s   !< oxygen production of macrophytes [mgO2/l]
   real,    intent(out)   :: po2r_s   !< oxygen consumption of macrophytes [mgO2/l]
   logical, intent(in)    :: control !< debugging
   integer, intent(in)    :: jjj      !< debugging
   
   ! --- local variables ---
   integer         :: nrs, nrsi
   integer         :: nrstart, nrmax, nrend, nrpmax
   real            :: tlip1,  pmaxpfl
   real            :: pfln,  pfln1
   real            :: obfli,  hconpfl, fim
   real            :: dpfl,  dpflmax,  alpha,  algip1
   real            :: mftd, mftd1, mgrmax, mgrow
   
   real, parameter :: mcona  = 2.0
   real, parameter :: mconb  = 0.5
   real, parameter :: pcona  = 2.0
   real, parameter :: pconb  = 1.0
   real, parameter :: phytmi = 0.01
   real, parameter :: phytmx = 0.20
   real, parameter :: miopt  = 160.0
   real, parameter :: grstr  = 20.0
   
   external        :: qerror
  
   ! convert dates into day of year
   if (mstart > 2) then
     nrstart = (itstart+31*(mstart-1)-int(0.4*mstart+2.3))
   else
      nrstart = itstart+31*(mstart-1)
   endif
   
   if (mmax > 2) then
      nrmax = (itmax+31*(mmax-1)-int(0.4*mmax+2.3))
   else
      nrmax = itmax+31*(mmax-1)
   endif
   nrpmax = nrmax
   
   if (mend > 2) then
      nrend = (itend+31*(mend-1)-int(0.4*mend+2.3))
   else
      nrend = itend+31*(mend-1)
   endif
   
   
   if (monats > 2) then
      nrs = (itags + 31 * (monats-1) - int(0.4 * monats + 2.3))
   else
      nrs = itags + 31 * (monats-1)
   endif
   
   nrsi = nrs + 1
   
   if (itstart <= 0 .and. pflmax_s > 0.0) then
      call qerror("macrophytes: Invalid dates for growth period.")
   endif
   
   if (pflmax_s <= 0.0) then
      pfl_s = 0.0
      return
   endif
   
   if (schwi_s == 0.0) then
      obfli = 0.0001
   else
      obfli = 8.2051 * (schwi_s * 4.2)
   endif

   po2p_s = 0.0
   po2r_s = 0.0
   
 
   tlip1 = log(grstr / obfli) / (-extk_s)
   if (tlip1 < 0.01) tlip1 = 0.01
   if (tlip1 > tiefe_s) tlip1 = tiefe_s
   algip1 = obfli*(1./(tlip1 * extk_s)) * (1. - exp(-extk_s * tlip1))
   
   if (nrs >= nrend .or. nrsi >= nrend) then
      pfl_s = pflmin_s
   
   elseif (nrs <= nrstart) then
      pfl_s = pflmin_s
   
   else if (obfli /= 0.0) then   
      mftd = ((nrs-nrstart)/(nrmax-nrstart))**mcona                     &
             *exp(mcona*(nrmax-nrs)/(nrmax-nrstart)*((nrend-nrmax)/     &
             (nrend-nrs))**mconb)
      mftd1 = ((nrsi-nrstart)/(nrmax-nrstart))**mcona                   &
              *exp(mcona*(nrmax-nrsi)/(nrmax-nrstart)*((nrend-nrmax)/   &
              (nrend-nrsi))**mconb)
      
      ! Berechnung der maximalen Wachstumsrate
      pfln  = pflmin_s + (pflmax_s - pflmin_s) * mftd
      pfln1 = pflmin_s + (pflmax_s - pflmin_s) * mftd1
      
      if (pflmax_s > 0.0 .and. pfl_s == 0.0) pfl_s = pfln
      
      mgrmax = log(pfln1 / pfln)
      mgrmax = mgrmax * 24. / (su-sa)
      
      if (mgrmax < 0.0) then
         mgrow = mgrmax
      else
         fim = (algip1/miopt) * exp(1. - algip1 / miopt)
         if (fim > 1.) fim = 1.
         mgrow = mgrmax*fim
      endif
      
      dpfl = pfln * (exp(mgrow * tflie) - 1.)
      dpflmax = pfln * (exp(mgrmax * tflie) - 1.)
      pflmax_s = pflmax_s - (dpflmax - dpfl)
      pfl_s = pfl_s + dpfl
   endif
   
   ! Berechnung des Aufwuchses auf den Makrophyten
   ! if (nrs < nrstart) then
   !    pftd = 0.0
   ! else
   !    pftd = ((nrs-nrstart)/(nrpmax-nrstart))**pcona                    &
   !           **exp(mcona*(nrpmax-nrs)/(nrpmax-nrstart)*((nrend-nrmax)/  &
   !          *(nrend-nrs))**pconb)
   ! endif
   ! phyt = (phytmi + (phytmx-phytmi) * pftd) * pfl_s
   
   
   ! Berechnung der Sauerstoffproduktion und -respiration durch Makrophyten
   
   ! max. Bruttoproduktion der Wasserpflanzen [mgO2/(gTG*h)]
   pmaxpfl = 5.66
   alpha = 0.11
   
   ! Umrechnung des Pflanzentrockengewichtes von g/m2 in g/l
   hconpfl = pfl_s / tiefe_s / 1000.
   po2p_s = (pmaxpfl * (1. - exp(-alpha * algip1 / pmaxpfl))) * hconpfl * tflie * 24.
   po2r_s = 2.0 * hconpfl * tflie * 24.
   
   ! Temperaturabhaengigkeit der Pflanzenphotosynthese
   po2p_s = po2p_s * (exp(-((tempw_s - 18.)**2) / 13.7**2))

end subroutine macrophytes
