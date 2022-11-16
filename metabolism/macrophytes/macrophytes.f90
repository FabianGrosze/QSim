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
subroutine macrophytes(pfl_s, pflmax_s, pflmin_s, tempw_s,        &
                       schwi_s, extk_s, tiefe_s,                  &
                       itstart, mstart, itmax, mmax, itend, mend, &
                       itags, monats, sa, su,                     &
                       tflie,                                     &
                       po2p_s, po2r_s,                            &
                       kontroll, jjj)
   
   ! --- dummy arguments ---
   real, intent(inout)  :: pfl_s       !< Trockengewicht Wasserpflanzen [g/m²]
   real, intent(inout)  :: pflmax_s    !< maximale Makrophytendichte im Jahresgang
   real, intent(inout)  :: pflmin_s    !< minimale Makrophytendichte im Jahresgang
   real, intent(in)     :: tempw_s     !< Wassertemperatur [°C]
   real, intent(in)     :: schwi_s     !< Globalstrahlung [cal/(cm2*h)]
   real, intent(in)     :: extk_s      !< Extinktionskoeffizient
   real, intent(in)     :: tiefe_s     !< Wassertiefe
   integer, intent(in)  :: itstart     !< Tag des Makropyhtenstarts
   integer, intent(in)  :: mstart      !< Monat des Makropyhtenstarts
   integer, intent(in)  :: itmax       !< Tag des Makrophytenmaximums
   integer, intent(in)  :: mmax        !< Monat des Makrophytenmaximums
   integer, intent(in)  :: itend       !< Tag des Makrophytenendes
   integer, intent(in)  :: mend        !< Monat des Makrophytenendes
   integer, intent(in)  :: itags       !< Tag des aktullen Simulationstages
   integer, intent(in)  :: monats      !< Monat des aktullen Simulationstages
   real, intent(in)     :: sa          !< Sonnenaufgang
   real, intent(in)     :: su          !< Sonnenuntergang
   real, intent(in)     :: tflie       !< Zeitschritt [d]
   real, intent(out)    :: po2p_s      !< Sauerstoffproduktion der Makrophyten [mgO2/l]
   real, intent(out)    :: po2r_s      !< Sauerstoffverbrauch der Makrophyten [mgO2/l]
   logical, intent(in)  :: kontroll    !< debugging
   integer, intent(in)  :: jjj         !< debugging

   ! --- local variables ---
   real     :: obfli, algip1, mftd, mftd1, pfln, pfln1, mgrmax, mgrow, fim
   real     :: alpha, hconpfl, dpfl, dpflmax, tlip1
   integer  :: nrsi, nrstart, nrmax, nrend, nrs

   real, parameter :: miopt = 160.
   real, parameter :: GRSTR = 20.0
   real, parameter :: pmaxpfl = 5.66 !  max. Bruttoproduktion der Wasserpflanzen [mgO2/(gTG*h)]
   real, parameter :: mcona = 2.0
   real, parameter :: mconb = 0.5
   
   ! -----------------------------------------------------------------------
   ! transform date into days since start of year
   ! -----------------------------------------------------------------------
   ! TODO (schoenung): write function to convert date into daynumber
   ! day of start
   if (mstart > 2) then
      NRStart = (ITstart+31*(Mstart-1)-INT(0.4*Mstart+2.3))
   else
      NRStart = ITstart+31*(Mstart-1)
   endif

   ! day of maximum
   if (mmax > 2) then 
      NRmax = (ITmax+31*(Mmax-1)-INT(0.4*Mmax+2.3))
   else
      NRmax = ITmax+31*(Mmax-1)
   endif

   ! day of end
   if (mend > 2) then 
      NRend = (ITend+31*(Mend-1)-INT(0.4*Mend+2.3))
   else
      NRend = ITend+31*(Mend-1)
   endif

   ! current day in simulation
   if (monats > 2) then
      NRS = (ITAGs+31*(MONATs-1)-INT(0.4*MONATs+2.3))
   else
      NRS = ITAGs+31*(Monats-1)
   endif
   nrsi = nrs + 1
   
   ! -----------------------------------------------------------------------
   ! metabolism
   ! -----------------------------------------------------------------------
   if (pflmax_s <= 0.0) then
      pfl_s = 0.0
      po2p_s = 0.0
      po2r_s = 0.0
      return
   endif
   
   OBFLI = 8.2051*(schwi_s * 4.2)
   if (OBFLI < 0.0001) obfli = 0.0001
   
   if (obfli == 0.0) then
      algip1 = 0.0
   else
      
      tlip1 = (log(grstr)-log(obfli))/(-extk_s)
      if (tlip1 < 0.01) tlip1 = 0.01
      tlip1 = min(tlip1, tiefe_s)
      algip1 = obfli*(1./(tlip1*extk_s)) * (1.-exp(-extk_s * tlip1))
   endif
   
   
   if (nrs <= nrstart .or. nrs >= nrend .or. nrsi >= nrend) then
      pfl_s = pflmin_s
   
   else
      mftd = ((nrs-nrstart) / (nrmax-nrstart))**mcona                     &
           * exp(mcona * (nrmax-nrs) / (nrmax-nrstart) * ((nrend-nrmax)/  &
             (nrend-nrs))**mconb)
      
      mftd1 = ((nrsi-nrstart)/(nrmax-nrstart))**mcona                   &
              *exp(mcona*(nrmax-nrsi)/(nrmax-nrstart)*((nrend-nrmax)/   &
              (nrend-nrsi))**mconb)
      
      ! Berechnung der maximalen Wachstumsrate
      pfln  = pflmin_s + (pflmax_s - pflmin_s) * mftd
      pfln1 = pflmin_s + (pflmax_s - pflmin_s) * mftd1
      
      if (pflmax_s > 0.0 .and. pfl_s == 0.0) pfl_s = pfln
      
      mgrmax = log(pfln1) - log(pfln)
      mgrmax = mgrmax * 24./(su-sa)
      
      if (mgrmax < 0.0) then
         mgrow = mgrmax
      else
         fim = (algip1 / miopt) * exp(1.- algip1/miopt)
         if (fim > 1.) fim = 1.
         mgrow = mgrmax * fim
      endif
      
      dpfl = pfln * (exp(mgrow * tflie) - 1.)
      dpflmax  = pfln*(exp(mgrmax*tflie)-1.)
      pflmax_s = pflmax_s - (dpflmax - dpfl)
      pfl_s = pfl_s + dpfl
   endif
   
   
   ! Berechnung der Sauerstoffproduktion und -Respiration durch Makrophyten
   alpha = 0.11
   
   ! Umrechnung des Pflanzentrockengewichtes von g/m2 in g/l
   hconpfl = pfl_s / tiefe_s / 1000.
   po2p_s = (pmaxpfl *(1.-exp(-alpha*algip1/pmaxpfl))) * hconpfl * tflie*24.
   po2r_s = 2.0 * hconpfl * tflie * 24.
   
   ! Temperaturabhaengigkeit der Pflanzenphotosynthese
   po2p_s = po2p_s * (exp(-((tempw_s - 18.)**2) / 13.7**2))


end subroutine macrophytes