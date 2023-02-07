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
subroutine mphyt(tiefe,tempw,anze,po2p,po2r,pfldalg,tflie,                    &
                 itags,monats,itstart,mstart,itmax,mmax,itend,mend,schwi,     &
                 pflmin,pflmax,pfl,sa,su,ilang,extk,mstr,                     &
                 kontroll,jjj)
                 
   implicit none
   
   integer         :: mstr, mstart, monats, mmax, mend, ilang
   integer         :: itstart, itmax, itend, itags, ior
   real            :: tlip1, tflie, su, sa, pmaxpfl
   real            :: phytmx, phytmi, pfln, pfln1, pconb
   real            :: pcona, obfli, hconpfl, grstr, fim
   real            :: dpfl, dpflmax, alpha, algip1
   logical         :: kontroll
   integer         :: jjj
   integer         :: anze
   real            :: mftd,mftd1,mcona,mconb,mgrmax,mgrow,miopt
   real            :: nrstart,nrmax,nrend,nrpmax,nrs,nrsi
   real            :: pfldalg(1000),po2p(1000),po2r(1000),pflmin(1000)
   real            :: pflmax(1000),schwi(1000)
   real            :: tiefe(1000),tempw(1000),pfl(1000),extk(1000)
   character(1000) :: message
  
   if (ilang == 0) return
   
   
   if (mstart > 2)goto 30
   NRStart = ITstart+31*(Mstart-1)
   goto 31
   30 NRStart = (ITstart+31*(Mstart-1)-INT(0.4*Mstart+2.3))
   !
   !
   31 if (mmax > 2)goto 32
   NRmax = ITmax+31*(Mmax-1)
   goto 33
   32 NRmax = (ITmax+31*(Mmax-1)-INT(0.4*Mmax+2.3))
   !
   33 nrpmax = nrmax
   !
   if (mend > 2)goto 34
   NRend = ITend+31*(Mend-1)
   goto 35
   34 NRend = (ITend+31*(Mend-1)-INT(0.4*Mend+2.3))
   !
   35 mcona = 2.0
   mconb = 0.5
   pcona = 2.
   pconb = 1.
   phytmi = 0.01
   phytmx = 0.20
   !
   if (monats > 2)goto 23
   NRS = ITAGs+31*(MONATs-1)
   goto 29
   23 NRS = (ITAGs+31*(MONATs-1)-INT(0.4*MONATs+2.3))
   
   29 continue
   nrsi = nrs+1.
   do ior = 1,anze
      
      if (itstart <= 0 .and. pflmax(ior) > 0.0) then
         write(message, "(a,i0)") "Invalid dates for macrophytes growth in stretch ", mstr
         call qerror(message)
      endif
      
      if (pflmax(ior) <= 0.0) then
         pfl(ior) = 0.0
         cycle
      endif
      
      OBFLI = 8.2051*(schwi(ior)*4.2)
      if (OBFLI.EQ.0.0)obfli = 0.0001
      
      po2p(ior) = 0.0
      po2r(ior) = 0.0
      pfldalg(ior) = 0.0
      if (pflmax(ior) == 0.0) cycle
      if (obfli == 0.0) then
         algip1 = 0.0
         goto 50
      endif
      
      miopt = 160.
      GRSTR = 20.0
      tlip1 = (log(grstr)-log(obfli))/(-extk(ior))
      if (tlip1 < 0.01)tlip1 = 0.01
      if (tlip1 > tiefe(ior))tlip1 = tiefe(ior)
      algip1 = obfli*(1./(tlip1*extk(ior)))*(1.-exp(-extk(ior)*tlip1))
      
      50 continue
      if (nrs >= nrend .or. nrsi >= nrend) then
         pfl(ior) = pflmin(ior)
         goto 56
      endif
      if (nrs <= nrstart) then
         pfl(ior) = pflmin(ior)
         goto 56
      endif
      if (obfli == 0.0)goto 56
      
      mftd = ((nrs-nrstart)/(nrmax-nrstart))**mcona                     &
             *exp(mcona*(nrmax-nrs)/(nrmax-nrstart)*((nrend-nrmax)/     &
             (nrend-nrs))**mconb)
      mftd1 = ((nrsi-nrstart)/(nrmax-nrstart))**mcona                   &
              *exp(mcona*(nrmax-nrsi)/(nrmax-nrstart)*((nrend-nrmax)/   &
              (nrend-nrsi))**mconb)
      ! Berechnung der maximalen Wachstumsrate
      pfln = pflmin(ior)+(pflmax(ior)-pflmin(ior))*mftd
      pfln1 = pflmin(ior)+(pflmax(ior)-pflmin(ior))*mftd1
      
      if (pflmax(ior) > 0.0 .and. Pfl(ior) == 0.0)pfl(ior) = pfln
      
      mgrmax = log(pfln1)-log(pfln)
      mgrmax = mgrmax*24./(su-sa)
      if (mgrmax < 0.0) then
         mgrow = mgrmax
         goto 59
      endif
      fim = (algip1/miopt)*exp(1.-algip1/miopt)
      if (fim > 1.)fim = 1.
      mgrow = mgrmax*fim
      
      59 dpfl = pfln*(exp(mgrow*tflie)-1.)
      dpflmax = pfln*(exp(mgrmax*tflie)-1.)
      pflmax(ior) = pflmax(ior)-(dpflmax-dpfl)
      pfl(ior) = pfl(ior)+dpfl
      56 continue
      
      ! Berechnung des Aufwuchses auf den Makrophyten
      !
      !     if(nrs.lt.nrstart)then
      !     pftd = 0.0
      !     goto 58
      !     endif
      !     pftd = ((nrs-nrstart)/(nrpmax-nrstart))**pcona
      !    **exp(mcona*(nrpmax-nrs)/(nrpmax-nrstart)*((nrend-nrmax)/
      !    *(nrend-nrs))**pconb)
      !  58 phyt = (phytmi+(phytmx-phytmi)*pftd)*pfl(ior)
      !     pfldalg(ior) = phyt*0.10
      !
      !     Berechnung der Sauerstoffproduktion und -Respiration durch
      !     Makrophyten
      !
      !     pmaxpfl   -       max. Bruttoproduktion der Wasserpflanzen
      !                       [mgO2/(gTG*h)]
      pmaxpfl = 5.66
      alpha = 0.11
      
      ! Umrechnung des Pflanzentrockengewichtes von g/m2 in g/l
      hconpfl = pfl(ior)/tiefe(ior)/1000.
      po2p(ior) = (pmaxpfl*(1.-exp(-alpha*algip1/pmaxpfl)))*hconpfl     &
                  *tflie*24.
      po2r(ior) = 2.0*hconpfl*tflie*24.
      ! Temperaturabhaengigkeit der Pflanzenphotosynthese
      po2p(ior) = po2p(ior)*(exp(-((tempw(ior)-18.)**2)/13.7**2))
      !
      
   enddo

end
