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

!> Entwicklung von Dreissena polymorph und deren Einfluss auf das Phytoplankton
!! @author Volker Kirchesch
!! @date 15.01.1996
subroutine dreissena(zdrei,zdreis,tempw,flae,elen,anze,                 &
                     ior,volfdr,akbcm,agbcm,aki,agr,algdrk,algdrg,      &
                     tflie,ro2dr,lboem,bsohlm,ss,vo2,ssdr,drfaek,       &
                     drfaeg,drfaes,gewdr,dlarvn,itags,monats,jahrs,     &
                     lait1,laim1,laid1,ilang,                           &
                     resdr,exdrvg,exdrvk,ssalg,drpfec,                  &
                     abl,exdrvb,abbcm,algdrb,drfaeb,                    &
                     idras,drmas,drakr,drbar,drmor,ffood,coroI,coroIs,  &
                     CHNF,drHNF,HNFdra,dlmax,dlmaxs,gwdmax,             &
                     sgwmue,fkm,FoptDe,mstr,azStr,                      &
                     kontroll,jjj)
   
   use aparam, only: Caki, Cabl, Cagr, FoptD
   implicit none
   
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(1000,4) :: zdrei    !< 
   real,    intent(inout), dimension(1000,4) :: zdreis   !<  
   real,    intent(in),    dimension(1000)   :: tempw    !< 
   real,    intent(in),    dimension(1000)   :: flae     !< 
   real,    intent(in),    dimension(1000)   :: elen     !< 
   integer, intent(in)                       :: anze     !< 
   integer, intent(inout)                    :: ior      !< 
   real,    intent(out),   dimension(1000)   :: volfdr   !< 
   real,    intent(in),    dimension(1000)   :: akbcm    !< 
   real,    intent(in),    dimension(1000)   :: agbcm    !< 
   real,    intent(in),    dimension(1000)   :: aki      !< 
   real,    intent(in),    dimension(1000)   :: agr      !< 
   real,    intent(out),   dimension(1000)   :: algdrk   !< 
   real,    intent(out),   dimension(1000)   :: algdrg   !< 
   real,    intent(in)                       :: tflie    !< 
   real,    intent(out),   dimension(1000)   :: ro2dr    !<  
   real,    intent(in),    dimension(1000)   :: lboem    !< 
   real,    intent(in),    dimension(1000)   :: bsohlm   !< 
   real,    intent(in),    dimension(1000)   :: ss       !< 
   real,    intent(in),    dimension(1000)   :: vo2      !< unused
   real,    intent(out),   dimension(1000)   :: ssdr     !<  
   real,    intent(out),   dimension(1000)   :: drfaek   !< 
   real,    intent(out),   dimension(1000)   :: drfaeg   !< 
   real,    intent(out),   dimension(1000)   :: drfaes   !< 
   real,    intent(inout), dimension(1000,4) :: gewdr    !< 
   real,    intent(inout), dimension(1000)   :: dlarvn   !< 
   integer, intent(in)                       :: itags    !< 
   integer, intent(in)                       :: monats   !< 
   integer, intent(in)                       :: jahrs    !< 
   integer, intent(in)                       :: lait1    !< 
   integer, intent(in)                       :: laim1    !< 
   integer, intent(in)                       :: laid1    !< 
   integer, intent(in)                       :: ilang    !< 
   real,    intent(out),   dimension(1000)   :: resdr    !< 
   real,    intent(out),   dimension(1000)   :: exdrvg   !< 
   real,    intent(out),   dimension(1000)   :: exdrvk   !< 
   real,    intent(in),    dimension(1000)   :: ssalg    !< 
   real,    intent(out),   dimension(1000)   :: drpfec   !<  
   real,    intent(in),    dimension(1000)   :: abl      !< 
   real,    intent(out),   dimension(1000)   :: exdrvb   !< 
   real,    intent(in),    dimension(1000)   :: abbcm    !< 
   real,    intent(out),   dimension(1000)   :: algdrb   !< 
   real,    intent(out),   dimension(1000)   :: drfaeb   !< 
   real,    intent(out),   dimension(1000,2) :: idras    !< 
   real,    intent(out),   dimension(1000,2) :: drmas    !< 
   real,    intent(out),   dimension(1000,2) :: drakr    !< 
   real,    intent(out),   dimension(1000,2) :: drbar    !< 
   real,    intent(out),   dimension(1000,2) :: drmor    !< 
   real,    intent(out),   dimension(1000)   :: ffood    !< 
   real,    intent(in),    dimension(1000)   :: coroI    !< 
   real,    intent(in),    dimension(1000)   :: coroIs   !< 
   real,    intent(in),    dimension(1000)   :: CHNF     !< 
   real,    intent(out),   dimension(1000)   :: drHNF    !< 
   real,    intent(out),   dimension(1000)   :: HNFdra   !< 
   real,    intent(inout), dimension(1000)   :: dlmax    !< 
   real,    intent(inout), dimension(1000)   :: dlmaxs   !< 
   real,    intent(out),   dimension(1000)   :: gwdmax   !< 
   real,    intent(inout), dimension(1000)   :: sgwmue   !< 
   real,    intent(in),    dimension(1000)   :: fkm      !< unused
   real,    intent(in)                       :: FoptDe   !< unused
   integer, intent(in)                       :: mstr     !<  unused
   integer, intent(in)                       :: azStr    !< 
   logical, intent(in)                       :: kontroll !< debugging
   integer, intent(in)                       :: jjj      !< debugging
   
   
   ! --- local variables ---
   integer            :: itime_hoch, jahr_tst1, ndr, nrs, nrka1a, nrla1e
   integer            :: nrla1a
   real               :: fco, fcos, fcom 
   real               :: hconc1, hconc2, exdrvz, ssorg, ssc
   real               :: agrc, akic, ablc, yc, ycs, fgr, fki, fbl, fss
   real               :: hconvg, hconvb, hconvk, hconvs, food, hconf, fr, up, ups
   real               :: rres, respc, respcs, dyc, dycs, fh2ovol, drmue, dgewdr, vol
   real               :: rescm3, excm3, respbio, respo2, upt, upst, uptm3, draup, ddlarn
   real               :: dlamor, dlaefes, drrt1, drrt3, drrt2, drrt33, drrt22
   real               :: dlafes, drrt11, spwmx, drrtt, fdrrt, dei, deimue, dgwmue
   real               :: stdpla, drftt, dfemue, dfmue, dfmues, ddrein, dreing, dreisn
   real               :: gewdts, dmorg, dreinm, hcond1, hcond2
   real               :: assr, assrs, exdr, exdrs, hcont, w, x
   real               :: idr, drrt, drft, qfec
   real, dimension(4) :: adrg, adrk, drss, drfeck, drfecg, drfecs
   real, dimension(4) :: dchlg, dchlk, filaki, filagr
   real, dimension(4) :: adrb, drfecb, filabl, dchlb
   real, dimension(4) :: filss, vofkop, filHNF, hcondb, hconds
   
   real, parameter    :: zqz10 = 3.1       !
   real, parameter    :: ztmax = 31.       !
   real, parameter    :: ztopt = 28.       !
   real, parameter    :: drCgeh = 0.38     !
   real, parameter    :: fweib = 0.5       !
   real, parameter    :: fgesund = 0.25    !
   real, parameter    :: F_lim = 0.01      ! Kohlenstoffgehalt, bei dem Dreissena die Futteraufnahme einstellt
   real, parameter    :: tdpla = 22.       ! tdpla = 2.
   real, parameter    :: klmorg = 8.26     !
   real, parameter    :: klmor = klmorg/tdpla
   real, parameter    :: flai = 0.52       ! 
   real, parameter    :: nndr = 2          !
   real, parameter    :: pgr = 1.0         ! Präferenzfaktor für Grünalgen
   real, parameter    :: pki = 1.0         ! Präferenzfaktor für Kieselalgen   
   real, parameter    :: pbl = 0.5         ! Präferenzfaktor für Blaualgen (ist 0.2)
   real, parameter    :: pss = 0.1         ! 
   real, parameter    :: rres0 = 0.0015    ! Grundrespirationsrate
   real, parameter    :: qres = 0.29       ! aktive Respirationsrate (abhaengig von der Assimilation) 1
   
   save jahr_tst1, drrt, drft, stdpla, itime_hoch
   
   if (ilang == 0)goto 999
   ! exdrvg(k,b) -  Anteil der Algenbiomasse die exkretiert wird (wird nur bei den Nährstoffen berücksichtigt)
   ! if (FoptD == 0.0)FoptD = 1.2
   
   ! Anteil des Futters, dass als Faeces wieder ausgeschieden wird
   qfec = 0.25
   
   if (azStr == 1) then
      drrt = drrt + tflie
      ! Schalter für Hochzählen der div. Zeiten
      itime_hoch = 1
   endif
   
   do  ior = 1,anze
      
      do ndr = 1,nndr
         if (zdrei(ior,ndr)  < 0.0) zdrei(ior,ndr)  = 0.0
         if (zdreis(ior,ndr) < 0.0) zdreis(ior,ndr) = 0.0
         if (gewdr(ior,ndr)  < 0.0) gewdr(ior,ndr)  = 0.0
      enddo
      
      ! Einfluss von Corophium auf die Ingestions- und Filtrierrate
      if (coroI(ior) == 0.0 .and. coroIs(ior) == 0.0) then
         fco = 1.
         fcos = 1.
         fcom = 1.
      else
         fco = (90000. - (coroI(ior)-10000.))/90000.
         if (fco > 1.)fco = 1.
         if (fco < 0.0)fco = 0.0
         
         fcos = (90000.-(coroIs(ior)-10000.))/90000.
         if (fcos > 1.)fcos = 1.
         if (fcos < 0.0)fcos = 0.0
         
         hconc1 = 2.*lboem(ior)*elen(ior)*coroI(ior)
         hconc2 = bsohlm(ior)*elen(ior)*coroIs(ior)
         fcom = (hconc1*fco+hconc2*fcos)/(hconc1+hconc2)
      endif
      
      if (elen(ior) == 0.0) then
         ro2dr(ior) = 0.0
         algdrg(ior) = 0.0
         algdrk(ior) = 0.0
         volfdr(ior) = 0.0
         resdr(ior) = 0.0
         exdrvz = 0.0
         exdrvg(ior) = 0.0
         exdrvk(ior) = 0.0
         cycle
      endif
      
      exdrvz = 0.0
      exdrvg(ior) = 0.0
      exdrvk(ior) = 0.0
      exdrvb(ior) = 0.0
      resdr(ior)  = 0.0
      ro2dr(ior)  = 0.0
      
      ! Berechnung der filtrierbaren Futterkonzentration
      ssorg = SS(ior)*0.1
      ssc = ssorg*0.4
      ssorg = 0.0
      
      agrc = agr(ior) * Cagr
      akic = aki(ior) * Caki
      ablc = abl(ior) * Cabl
      
      ! max. Dreissena-Dichte
      ! drpr = zdreis(ior)/(bsohlm(ior)*elen(ior))
      do ndr = 1,nndr
         zdrei(ior,ndr)  = zdrei(ior,ndr)  * (2.*lboem(ior) * elen(ior))
         zdreis(ior,ndr) = zdreis(ior,ndr) * (bsohlm(ior) * elen(ior))
         Yc = zdrei(ior,ndr)
         Ycs = zdreis(ior,ndr)
         
         Fgr = pgr*agrc
         Fki = pki*akic
         Fbl = pbl*ablc
         Fss = 0.0
         
         ! TODO (schoenung): use epsilon()
         if (aki(ior)+agr(ior)+abl(ior) == 0.0) then
            hconvk = 0.0
            hconvg = 0.0
            hconvb = 0.0
         else
            hconvk = aki(ior)*pki / (aki(ior)*pki + agr(ior)*pgr + abl(ior)*pbl)
            hconvg = agr(ior)*pgr / (aki(ior)*pki + agr(ior)*pgr + abl(ior)*pbl)
            hconvb = abl(ior)*pgr / (aki(ior)*pki + agr(ior)*pgr + abl(ior)*pbl)
         endif
         hconvs = 0.0
         
         ! Berechnung der Aufnahmerate gC/m2
         food = fgr + fki + fbl + fss
         hconf = food / FoptD
         if (food > FoptD)  hconf = 1.
         if (food <= F_lim) hconf = 0.0
         ffood(ior) = hconf
         
         if (gewdr(ior,ndr) == 0.0) then
            idr = 0.0
         else
            ! Walz
            ! idr = (0.1105*gewdr(ior,ndr)**(-0.213))**exp(-0.00605*(20.0-tempw(ior))**2)
            
            ! modifizierte Walz
            ! idr = (0.295*gewdr(ior,ndr)**(-0.636))**exp(-0.00605*(20.0-tempw(ior))**2)
             
            ! nach Schneider
            ! idr = (0.1271*gewdr(ior,ndr)**(-0.39))**exp(-0.00605*(20.0-tempw(ior))**2)
            
            idr = (0.249*gewdr(ior,ndr)**(-0.615))*exp(-0.00605*(20.0-tempw(ior))**2)
         endif
         
         ! Filtrierrate
         if (gewdr(ior,ndr) == 0.0) then
            fr = 0.0
         else
            fr = 9.24 * gewdr(ior,ndr)**(-0.392)
            fr = fr * 3.267 * exp(-0.037 * SSalg(ior))
            fr = fr * exp(-0.00605 * (20.-tempw(ior))**2)
            ! Umrechnung in m3/g*d
            fr = (fr * 24./1000.) * fcom
         endif
         
         up  = idr * hconf * fco  * Yc
         ups = idr * hconf * fcos * Ycs
         idr = idr * hconf * fcom
         
         idras(ior,ndr) = idr
         
         ! Berechnung der Assimilationsrate
         qfec = 0.315*exp(0.88*hconf)
         assr = (1.-qfec)*up
         assrs = (1.-qfec)*ups
         
         ! Berechnung der Exkretionsrate
         exdr = 0.064*(1.-qfec)*up
         exdrs = 0.064*(1.-qfec)*ups
         
         ! Temperaturabhaengigkeit
         if (tempw(ior) >= ztmax) then
            hcont = 0.0
         else
            w = log(zqz10)*(ztmax-ztopt)
            x = ((w/20.)*(1.+sqrt(1.+40./w)))**2
            hcont = (((ztmax-tempw(ior))/(ztmax-ztopt))*exp(1.-(ztmax-tempw(ior))/(ztmax-ztopt)))**x
         endif
         
         ! Berechnung der Respirationsrate in gC/(m2Gewaesserbod.*d)
         if (gewdr(ior,ndr) == 0.0) then
            rres = 0.0
         else
            rres = rres0 * gewdr(ior,ndr)**(-0.25)
         endif
         respc  = rres * hcont * Yc  + qres * assr
         respcs = rres * hcont * Ycs + qres * assrs
         
         ! zeitliche Änderung der Dreissena-Biomasse [gC]
         dYc = (assr - respc - exdr) * tflie
         Yc = Yc + dYc
         dYcs = (assrs - respcs - exdrs) * tflie
         Ycs = Ycs + dYcs
         zdrei(ior,ndr)  = Yc
         zdreis(ior,ndr) = Ycs
         
         FH2oVol = FR * Yc + FR * Ycs
         FH2oVOL = FH2oVOL * tflie
         
         ! Gewichtsänderung einer Muschel
         drmue = idr - (idr * qfec) - (idr*(1.-qfec)*0.064) - ((1.-qfec) * qres * idr) - rres * hcont
         dgewdr = gewdr(ior,ndr) * drmue * tflie
         gewdr(ior,ndr) = gewdr(ior,ndr) + dgewdr
         drmas(ior,ndr) = drmue
         drakr(ior,ndr) = (1.-qfec) * qres * idr
         drbar(ior,ndr) = rres * hcont
         
         ! Umrechnung in mg/l
         ! Annahme: 1 mg respirierte Biomasse verbraucht 5.59 mg O2(Schneider)
         vol = flae(ior)*elen(ior)
         if (vol == 0.0)goto 999
         rescm3 = (respc+respcs)/vol
         excm3 = (exdr+exdrs)/vol
         
         respbio = rescm3 / drCgeh ! Respiration Umrechnung von C in Biomasse
         resdr(ior) = resdr(ior) + respbio * tflie
         
         exdrvz = exdrvz + excm3 * tflie
         exdrvg(ior) = exdrvz * hconvg/Cagr
         exdrvk(ior) = exdrvz * hconvk/Caki
         exdrvb(ior) = exdrvz * hconvb/Cabl         
         respo2 = respbio*5.59
         ro2dr(ior) = ro2dr(ior) + respo2*tflie
         
         upt = up*tflie
         upst = ups*tflie
         uptm3 = (upt+upst)/vol
         
         adrg(ndr) = uptm3 * hconvg/Cagr
         adrk(ndr) = uptm3 * hconvk/Caki
         adrb(ndr) = uptm3 * hconvb/Cabl
         ! drss(ndr) = uptm3*hconvs/0.4
         drfecg(ndr) = qfec * adrg(ndr)
         drfeck(ndr) = qfec * adrk(ndr)
         drfecb(ndr) = qfec * adrb(ndr)
         drfecs(ndr) = qfec * drss(ndr)
         vofkop(ndr) = (fh2ovol/vol)*100.
         
         Filaki(ndr) = aki(ior) * pki * fh2ovol/vol
         Filagr(ndr) = agr(ior) * pgr * fh2ovol/vol
         Filabl(ndr) = abl(ior) * pbl * fh2ovol/vol
         filss(ndr)  = ss(ior)  *       fh2ovol/vol
         
         if (adrk(ndr) > 0.0 .and. adrk(ndr) > Filaki(ndr)) then
            Filaki(ndr) = adrk(ndr)
            filagr(ndr) = adrg(ndr)
            filabl(ndr) = adrb(ndr)
            vofkop(ndr) = (Filaki(ndr)/(aki(ior)*pki))*100.
         
         else if (adrg(ndr) > 0.0 .and. adrg(ndr) > Filagr(ndr)) then
            Filaki(ndr) = adrk(ndr)
            filagr(ndr) = adrg(ndr)
            filabl(ndr) = adrb(ndr)
            vofkop(ndr) = (Filagr(ndr)/(agr(ior)*pgr))*100.
         
         else if (adrb(ndr) > 0.0 .and. adrb(ndr) > Filabl(ndr)) then
            Filaki(ndr) = adrk(ndr)
            filagr(ndr) = adrg(ndr)
            filabl(ndr) = adrb(ndr)
            vofkop(ndr) = (Filabl(ndr)/(abl(ior)*pbl))*100.
         endif
         
         dchlg(ndr) = 0.0
         dchlk(ndr) = 0.0
         dchlb(ndr) = 0.0
         if (agbcm(ior) > 0.0) dchlg(ndr) = filagr(ndr) * 1000. * Cagr / agbcm(ior)
         if (akbcm(ior) > 0.0) dchlk(ndr) = filaki(ndr) * 1000. * Caki / akbcm(ior)
         if (abbcm(ior) > 0.0) dchlb(ndr) = filabl(ndr) * 1000. * Cabl / abbcm(ior)
         
         filHNF(ndr) = CHNF(ior)*fh2ovol/vol
         
      enddo
      
      algdrg(ior) = 0.0
      algdrk(ior) = 0.0
      algdrb(ior) = 0.0
      ssdr(ior) = 0.0
      drfaek(ior) = 0.0
      drfaeg(ior) = 0.0
      drfaeb(ior) = 0.0
      drfaes(ior) = 0.0
      draup = 0.0
      volfdr(ior) = 0.0
      drHNF(ior) = 0.0
      
      do ndr = 1,nndr
         algdrg(ior) = algdrg(ior) + filagr(ndr)
         algdrk(ior) = algdrk(ior) + filaki(ndr)
         algdrb(ior) = algdrb(ior) + filabl(ndr)
         ssdr(ior)   = ssdr(ior)   + filss(ndr)
         drfaeg(ior) = drfaeg(ior) + drfecg(ndr)
         drfaek(ior) = drfaek(ior) + drfeck(ndr)
         drfaeb(ior) = drfaeb(ior) + drfecb(ndr)
         drfaes(ior) = drfaes(ior) + drfecs(ndr)
         volfdr(ior) = volfdr(ior) + vofkop(ndr)
         draup       = draup + adrg(ndr) + adrk(ndr) + adrb(ndr)
         drHNF(ior)  = drHNF(ior)+filHNF(ndr)
      enddo
      
      ! Ausgabe
      if (CHNF(ior) > 0.0) then 
         HNFdra(ior) = (drHNF(ior)/CHNF(ior))*24.
      else
         HNFdra(ior) = 0.0
      endif
      
      ! Schwebstoffaufnahme durch Dreissena wird vorläufig auf Null gesetzt
      ssdr(ior) = 0.0
      if (algdrg(ior)+algdrk(ior)+algdrb(ior) == 0.0) then
         drpfec(ior) = 0.0
      else
         drpfec(ior) = 1.-(draup/(algdrg(ior)+algdrk(ior)+algdrb(ior)))
      endif
      
      if (draup == 0.0) then 
         drpfec(ior) = 0.0
      else
         drpfec(ior) = drpfec(ior)*100.
      endif
      
      if (drpfec(ior) == 0.0) volfdr(ior) = 0.0
      if (drpfec(ior)  < 0.0) drpfec(ior) = 0.0
      
      ! Berechnung der Larvenbildung
      ddlarn = 0.0
      dlamor = 0.0
      dlafes = 0.0
      dlmax(ior) = dlmax(ior)*(2.*lboem(ior)*elen(ior))
      dlmaxs(ior) = dlmaxs(ior)*(bsohlm(ior)*elen(ior))
      if (lait1 == 0 .and. laim1 == 0)goto 211
      if (drft >= laid1)goto 116
      if (ilang == 0 .or. jahr_tst1 < jahrs) then
         drrt = 0.0
         goto 211
      endif
      
      if (monats > 2) then 
         NRS = (ITAGS+31*(MONATS-1)-INT(0.4*MONATS+2.3))
      else
         NRS = ITAGS+31*(MONATS-1)
      endif
      
      if (laim1 > 2) then 
         NRla1a = (lait1+31*(laim1-1)-INT(0.4*laim1+2.3))
      else
         NRla1a = lait1+31*(laim1-1)
      endif
      
      nrla1e = nrla1a+laid1
      if (nrs < nrla1a .or. nrs >= nrla1e) then
         drrt = 0.0
         goto 113
      endif
      
      drrt1 = 0.0
      drrt3 = 30.
      drrt2 = drrt3/2.
      
      drrt11 = 0.0
      drrt33 = laid1-drrt3
      drrt22 = drrt33/2.
      
      ! Annahme Gewichtverlust der Adulten durch Reproduktion
      if (drrt <= drrt3) spwmx = flai*0.6/(0.5*drrt3)
      if (drrt >  drrt3) spwmx = flai*0.4/(0.5*(laid1-drrt3))
      
      if (dlmax(ior) == 0.0 .and. dlmaxs(ior) == 0.0) then
         dlmax(ior)  = zdrei(ior,2)
         dlmaxs(ior) = zdreis(ior,2)
         gwdmax(ior) = gewdr(ior,2)
         sgwmue(ior) = 0.0
      endif
      
      
      if (drrt > drrt3) then
         ! zweite Kurve
         drrtt = drrt-drrt3
         if (drrtt > drrt22) then
            fdrrt = ((drrtt-drrt33)**2)/((drrtt-drrt22)**2+(drrtt-drrt33)**2)
            fdrrt = fdrrt*spwmx
         else
            fdrrt = ((drrtt-drrt11)**2)/((drrtt-drrt22)**2+(drrtt-drrt11)**2)
            fdrrt = fdrrt*spwmx
         endif
      
      else if (drrt > drrt2) then
         fdrrt = ((drrt-drrt3)**2)/((drrt-drrt2)**2+(drrt-drrt3)**2)
         fdrrt = fdrrt*spwmx
      
      else
         fdrrt = ((drrt-drrt1)**2)/((drrt-drrt2)**2+(drrt-drrt1)**2)
         fdrrt = fdrrt*spwmx
      endif
      
      
      do ndr = 2,nndr
         gewdr(ior,ndr) = gewdr(ior,ndr)-(gwdmax(ior)*tflie*fdrrt)
         
         ! Berechnung der gebildeten Larven im Zeitschritt aus dem Gewichtsverlust
         ! der Weibchen
         ! C-Gehalt einer Eizelle: 3.35e-9 g
         dEi = (dlmax(ior)+dlmaxs(ior))*tflie*fdrrt/3.35e-9
         dEi = dEi*0.75
         ddlarn = dEi*fgesund*fweib
         
         ! Larvenbildung aus Zuwachs im Zeitschritt
         dEimue = ((dyc*flai+dycs*flai)/3.35e-9)*0.75*fgesund*fweib
         if (dEimue < 0.0)dEimue = 0.0
         ddlarn = ddlarn+dEimue
         gewdr(ior,ndr) = gewdr(ior,ndr)-dgewdr*0.52
         dgwmue = (dyc / (2.*lboem(ior)*elen(ior)))*0.52  &
                + (dycs / (bsohlm(ior)*elen(ior)))*0.52
         sgwmue(ior) = sgwmue(ior)+dgwmue
         
         ddlarn = ddlarn/(vol*1000.)
         zdrei(ior,ndr)  = zdrei(ior,ndr)  - (dlmax(ior)*tflie*fdrrt)
         zdrei(ior,ndr)  = zdrei(ior,ndr)  - dyc*0.52
         zdreis(ior,ndr) = zdreis(ior,ndr) - (dlmaxs(ior)*tflie*fdrrt)
         zdreis(ior,ndr) = zdreis(ior,ndr) - dycs*0.52
      enddo
      
      113 continue
      if (nrs>=nrla1a .and. itime_hoch == 1)stdpla = stdpla+tflie
      dlamor = dlarvn(ior)*(1.-exp(-klmor*tflie))
      if (stdpla < tdpla) then
         drft = 0.0
         itime_hoch = 0
         goto 114
      endif
      
      if (itime_hoch == 1) then
         drft = drft+tflie
         itime_hoch = 0
      endif
      
      if (drft <= drrt3)spwmx = flai*0.6/(0.5*drrt3)
      if (drft > drrt3)spwmx = flai*0.4/(0.5*(laid1-drrt3))
      
      if (drft > drrt3) then
         ! zweite Kurve
         drftt = drft-drrt3
         if (drftt > drrt22) then 
            fdrrt = ((drftt-drrt33)**2)/((drftt-drrt22)**2+(drftt-drrt33)**2)
            fdrrt = fdrrt*spwmx
         else
            fdrrt = ((drftt-drrt11)**2)/((drftt-drrt22)**2+(drftt-drrt11)**2)
            fdrrt = fdrrt*spwmx
         endif
      
      else if (drft > drrt2) then 
         fdrrt = ((drft-drrt3)**2)/((drft-drrt2)**2+(drft-drrt3)**2)
         fdrrt = fdrrt*spwmx
      
      else
         fdrrt = ((drft-drrt1)**2)/((drft-drrt2)**2+(drft-drrt1)**2)
         fdrrt = fdrrt*spwmx
      endif
      
      ! Larvengewicht beim Festsetzen: 8.6e-8 gC; 8.6e-5 mgC
      dlafes = (dlmax(ior)+dlmaxs(ior))*tflie*fdrrt/3.35e-9
      dlafes = dlafes*0.75*fgesund*fweib

      116 continue
      dfemue = sgwmue(ior)/(tdpla*1./tflie)
      if (zdreis(ior,2) == 0.0 .and. zdrei(ior,2) == 0.0) then
         dfemue = 0.0
         dfmue = 0.0
         dfmues = 0.0
         sgwmue(ior) = sgwmue(ior)-dfemue
      else
         sgwmue(ior) = sgwmue(ior)-dfemue
         dfmue  = dfemue * (2.*lboem(ior) * elen(ior)) * (zdrei(ior,2)  / (zdrei(ior,2) + zdreis(ior,2)))
         dfmues = dfemue * (bsohlm(ior)   * elen(ior)) * (zdreis(ior,2) / (zdrei(ior,2) + zdreis(ior,2)))
      endif
      
      dfemue = ((dfmue+dfmues)/3.35e-9)*0.75*fgesund*fweib
      dlafes = dlafes+dfemue
      dlafes = (dlafes*exp(-klmorg))/(vol*1000.)
      
      114 continue
      dlarvn(ior) = dlarvn(ior)+ddlarn-dlamor-dlafes
      if (dlarvn(ior) < 0.0)dlarvn(ior) = 0.0
      
      
      do ndr = 1,nndr
         ddrein = 0.0
         
         hconds(ndr) = zdreis(ior,ndr)
         hcondb(ndr) = zdrei(ior,ndr)
         
         if (gewdr(ior,ndr) == 0.0) then
            dreing = 0.0
            dreisn = 0.0
         else
            dreisn = ((zdrei(ior,ndr)+zdreis(ior,ndr))*1000.)/gewdr(ior,ndr)
            dreing = dreisn
         endif
         
         if (ndr == 1 .and. dlafes > 0.0) then
            gewdts = (dreisn*gewdr(ior,1)+dlafes*vol*1000.*8.6e-5) /(dreisn+dlafes*vol*1000.)
            if (gewdts > 0.0246) then
               dlafes = dlafes*exp(-0.1*20.*tflie)
               gewdts = (dreisn*gewdr(ior,1)+dlafes*vol*1000.*8.6e-5) /(dreisn+dlafes*vol*1000.)
            endif
            
            gewdr(ior,1) = gewdts
            dreing = dreisn + dlafes * vol * 1000.
            
         else if (ndr == 2 .and. gewdr(ior,1) > 1.6) then
            
            ddrein = ((zdrei(ior,1)+zdreis(ior,1))*1000.)/gewdr(ior,1)
            dreing = dreisn+ddrein
            
            gewdr(ior,2) = (dreisn*gewdr(ior,2)+ddrein*gewdr(ior,1)) / (dreisn+ddrein)
            gewdr(ior,1) = 0.0
            
            zdrei(ior,1)  = 0.0
            zdreis(ior,1) = 0.0
         endif
         
         ! natürliche Mortalitätsrate
         if (gewdr(ior,ndr) < 0.0246) then 
            dmorg = 0.1
         else if (gewdr(ior,ndr) > 0.0246) then 
            dmorg = 0.0157 * gewdr(ior,ndr)**(-0.502)
         else
            dmorg = 0.0
         endif
         
         ! nur ausschreiben
         drmor(ior,ndr) = dmorg
         if (gewdr(ior,ndr) == 0.0) drmor(ior,ndr) = 0.0
         
         dreinm = dreing*(1.-exp(-dmorg*tflie))
         dreing = dreing-dreinm
         if (dreing < 0.0) dreing = 0.0
         
         if (ndr == 1 .and. dlafes > 0.0 .and. zdrei(ior,ndr)+zdreis(ior,ndr) == 0.0) then
            hcond1 = zdrei(ior,2)/(zdrei(ior,2)+zdreis(ior,2))
            hcond2 = zdreis(ior,2)/(zdrei(ior,2)+zdreis(ior,2))
            goto 351
         endif
         
         if (zdrei(ior,ndr)+zdreis(ior,ndr) == 0.0 .and. ndr == 1) then
            zdrei(ior,ndr) = 0.0
            zdreis(ior,ndr) = 0.0
            cycle
         endif
         
         if (zdrei(ior,ndr)+zdreis(ior,ndr) == 0.0 .and. ndr == 2 .and. ddrein == 0.0) then
            zdrei(ior,ndr) = 0.0
            zdreis(ior,ndr) = 0.0
            cycle
         endif
         
         if (zdrei(ior,ndr)+zdreis(ior,ndr) == 0.0 .and. ndr == 2 .and. ddrein > 0.0) then
            hcond1 = hcondb(1)/(hcondb(1)+hconds(1))
            hcond2 = hconds(1)/(hcondb(1)+hconds(1))
            goto 351
         endif
         
         hcond1 = hcondb(ndr)/(hcondb(ndr)+hconds(ndr))
         hcond2 = hconds(ndr)/(hcondb(ndr)+hconds(ndr))
         
         351 continue
         zdrei(ior,ndr) = (dreing*gewdr(ior,ndr)/1000.)*hcond1
         zdreis(ior,ndr) = (dreing*gewdr(ior,ndr)/1000.)*hcond2
         
      enddo
      
      do ndr = 1,nndr
         zdrei(ior,ndr) = zdrei(ior,ndr)/(2.*lboem(ior)*elen(ior))
         if (bsohlm(ior) <= 0.0) then
            zdreis(ior,ndr) = 0.0
            cycle
         endif
         zdreis(ior,ndr) = zdreis(ior,ndr)/(bsohlm(ior)*elen(ior))
      enddo
      
      211 continue
      dlmax(ior)  = dlmax(ior)/(2.*lboem(ior)*elen(ior))
      dlmaxs(ior) = dlmaxs(ior)/(bsohlm(ior)*elen(ior))
      
   enddo
   
   999 continue 
   dlarvn(anze+1) = dlarvn(anze)
   jahr_tst1 = jahrs
   
   return
end subroutine dreissena
