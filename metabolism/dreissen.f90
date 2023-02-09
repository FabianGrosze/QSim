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
subroutine dreissen(zdrei,zdreis,tempw,flae,elen,anze,                 &
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
                    
   implicit none
   
   integer     :: nrla1e, nrs, nrla1a, nndr, ndr, ilang
   integer     :: mstr, monats, lait1, laim1, laid1
   integer     :: jahr_tst1, jahrs, itime_hoch, itags, ior
   real        :: ztopt, ztmax, zqz10, yc, ycs
   real        :: x, w, vol, up, upt, ablc
   real        :: uptm3, ups, upst, tflie, tdpla
   real        :: stdpla, ssorg, ssc, spwmx, rres
   real        :: rres0, respo2, respc, respcs, respbio
   real        :: rescm3, qres, qfec, pss, pki
   real        :: pgr, pbl, hconvs, hconvk, hconvg
   real        :: hconvb, hcont, hconf, hcond2, hcond1
   real        :: hconc2, hconc1, gewdts, f_lim, fweib
   real        :: fss, fr, foptd, foptde, food
   real        :: flai, fki, fh2ovol, fgr, fgesund
   real        :: fdrrt, fco, fcos, fcom, fbl
   real        :: exdr, exdrvz, exdrs, excm3, dyc
   real        :: dycs, drrtt, drrt3, drrt33, drrt2
   real        :: drrt22, drrt1, drrt11, drmue, drftt
   real        :: dreisn, dreinm, dreing, drcgeh, draup
   real        :: do2krit, dmorg, dlamor, dlafes, dgwmue
   real        :: dgewdr, dfmue, dfmues, dfemue, dei
   real        :: deimue, ddrein, ddlarn, caki, cagr
   real        :: cabl, assr, assrs, akic, agrc
   logical     :: kontroll !< debugging
   integer     :: jjj      !< debugging
   real        :: elen(1000),tempw(1000),flae(1000),zdrei(1000,4)
   real        :: zdreis(1000,4),volfdr(1000)
   real        :: akbcm(1000),agbcm(1000),fkmR(1000),fkm(1000)
   real        :: aki(1000),agr(1000),algdrk(1000),algdrg(1000)
   real        :: ro2dr(1000),vo2(1000),ssdr(1000),idras(1000,2)
   real        :: drmas(1000,2),drakr(1000,2),drbar(1000,2),drmor(1000,2)
   real        :: lboem(1000),bsohlm(1000),ss(1000),dlarvd(1000)
   real        :: drfaek(1000),drfaeg(1000),drfaes(1000)
   real        :: gewdr(1000,4),idr,dlarvn(1000),resdr(1000),exdrvg(1000)
   real        :: dlmax(1000),dlmaxs(1000),gwdmax(1000),exdrvk(1000)
   real        :: adrg(4),adrk(4),drss(4),drfeck(4),drfecg(4),drfecs(4)
   real        :: dchlg(4),dchlk(4),filaki(4),filagr(4)
   real        :: adrb(4),drfecb(4),filabl(4),dchlb(4)
   real        :: filss(4),vofkop(4),filHNF(4),HNFdra(1000)
   real        :: ssalg(1000),drpfec(1000),sgwmue(1000),ffood(1000)
   real        :: coroI(1000),coroIs(1000)
   real        :: klmor,klmorg,hcondb(4),hconds(4)
   real        :: CHNF(1000),drHNF(1000),drfaeb(1000)
   real        :: abl(1000),exdrvb(1000),abbcm(1000),algdrb(1000), drrt, drft
   integer     :: anze, azStr
   
   logical     :: calculate_dfmue     !TODO FG: switch to turn on/off some calculations, used to remove last goto
   
   save jahr_tst1, drrt, drft, stdpla, itime_hoch
   
   if (ilang == 0) then
      dlarvn(anze+1) = dlarvn(anze)
      jahr_tst1      = jahrs
      return
   endif
   
   ! exdrvg(k,b) -  Anteil der Algenbiomasse die exkretiert wird 
   ! (wird nur bei den Nährstoffen berücksichtigt)
   zqz10   = 3.1
   ztmax   = 31.
   ztopt   = 28.
   do2krit = 2.0
   ! drmax = 1000.
   dmorG   = 0.0
   
   drCgeh  = 0.38
   Cagr    = 0.48
   Caki    = 0.48
   Cabl    = 0.48
   fweib   = 0.5
   fgesund = 0.25
   FoptD   = FoptDe
   if (FoptD == 0.0) FoptD = 1.2
   
   ! Kohlenstoffgehalt, bei dem Dreissena die Futteraufnahme einstellt
   F_lim   = 0.01
   
   tdpla   = 22.
   ! tdpla = 2.
   klmorg  = 8.26
   klmor   = klmorg/tdpla
   flai    = 0.52
   nndr    = 2
   
   ! Parameter
   ! pgr   - Preferenzfaktor fuer Gruenalgen
   ! pki   - Preferenzfaktor fuer Kieselalgen
   ! pbl   - Preferenzfaktor fuer Blaualgen
   ! qfec  - Anteil des Futters, dass als Faeces wieder ausgeschieden wird
   ! rres  - Grundrespirationsrate 1/d
   ! qres  - aktive Respirationsrate (abhaengig von der Assimilation) 1
   ! dmorg - natuerliche Mortalitaetsrate
   ! dmorma- maximale Mortalitaetsrate bei Sauerstoffschwund
   pgr   = 1.0
   pki   = 1.0
   pbl   = 0.5     ! ist 0.2
   pss   = 0.1
   qfec  = 0.25
   rres0 = 0.0015
   qres  = 0.29
   
   if (azStr == 1) then
      drrt = drrt + tflie
      ! Schalter für Hochzählen der div. Zeiten
      itime_hoch = 1
   endif
   
   do ior = 1,anze
      
      do ndr = 1,nndr
         if (zdrei(ior,ndr) < 0.0)zdrei(ior,ndr) = 0.0
         if (zdreis(ior,ndr) < 0.0)zdreis(ior,ndr) = 0.0
         if (gewdr(ior,ndr) < 0.0)gewdr(ior,ndr) = 0.0
      enddo
      
      !Einfluss von Corophium auf die Ingest.- und Filtrierrate
      if (coroI(ior) == 0.0 .and. coroIs(ior) == 0.0) then
         fco = 1.
         fcos = 1.
         fcom = 1.
      else
         fco = (90000.-(coroI(ior)-10000.))/90000.
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
      !
      exdrvz = 0.0
      exdrvg(ior) = 0.0
      exdrvk(ior) = 0.0
      exdrvb(ior) = 0.0
      resdr(ior) = 0.0
      ro2dr(ior) = 0.0
      
      ! Berechnung der filtrierbaren Futterkonzentration
      ssorg = SS(ior)*0.1
      ssc = ssorg*0.4
      ssorg = 0.0
      
      agrc = agr(ior)*0.48
      akic = aki(ior)*0.48
      ablc = abl(ior)*0.48
      ! max. Dreissena-Dichte
      ! drpr = zdreis(ior)/(bsohlm(ior)*elen(ior))
      
      do ndr = 1,nndr
         zdrei(ior,ndr) = zdrei(ior,ndr)*(2.*lboem(ior)*elen(ior))
         zdreis(ior,ndr) = zdreis(ior,ndr)*(bsohlm(ior)*elen(ior))
         Yc = zdrei(ior,ndr)
         Ycs = zdreis(ior,ndr)
         
         Fgr = pgr*agrc
         Fki = pki*akic
         Fbl = pbl*ablc
         Fss = 0.0
         !
         if (aki(ior) > 0. .or. agr(ior) > 0. .or. abl(ior) > 0.) then
            hconvk = aki(ior) * pki / (aki(ior) * pki + agr(ior) * pgr + abl(ior) * pbl)
            hconvg = agr(ior) * pgr / (aki(ior) * pki + agr(ior) * pgr + abl(ior) * pbl)
            hconvb = abl(ior) * pgr / (aki(ior) * pki + agr(ior) * pgr + abl(ior) * pbl)
         else
            hconvk = 0.0
            hconvg = 0.0
            hconvb = 0.0
         endif
         hconvs = 0.0
         
         ! Berechnung der Aufnahmerate gC/m2
         food  = fgr + fki + fbl +fss
         hconf = food / FoptD
         if (food > FoptD) then
            hconf = 1.
         elseif (food <= F_lim) then
            hconf = 0.0
         endif
         ffood(ior) = hconf
         
         if (gewdr(ior,ndr) > 0.0) then
            !Walz
            !      idr = (0.1105*gewdr(ior,ndr)**(-0.213))
            !     **exp(-0.00605*(20.0-tempw(ior))**2)
            !modifizierte Walz
            !      idr = (0.295*gewdr(ior,ndr)**(-0.636))
            !     **exp(-0.00605*(20.0-tempw(ior))**2)
            !nach Schneider
            !      idr = (0.1271*gewdr(ior,ndr)**(-0.39))
            !     **exp(-0.00605*(20.0-tempw(ior))**2)
            !
            idr = 0.249 * gewdr(ior,ndr)**(-0.615) * exp(-0.00605 * (20.0 - tempw(ior))**2)
         else
            idr = 0.0
         endif
         
         ! Filtrierrate
         if (gewdr(ior,ndr) > 0.0) then
            FR = 9.24*gewdr(ior,ndr)**(-0.392)
            FR = FR*3.267*exp(-0.037*SSalg(ior))
            FR = FR*exp(-0.00605*(20.-tempw(ior))**2)
            !Umrechnung in m3/g*d
            FR = (FR*24./1000.)*fcom
         else
            FR = 0.0
         endif
         
         up  = idr * hconf * fco  * Yc
         ups = idr * hconf * fcos * Ycs
         idr = idr * hconf * fcom
         
         idras(ior,ndr) = idr
         
         ! Berechnung der Assimilationsrate
         qfec  = 0.315*exp(0.88*hconf)
         assr  = (1.-qfec)*up
         assrs = (1.-qfec)*ups
         
         ! Berechnung der Exkretionsrate
         exdr  = 0.064*(1.-qfec)*up
         exdrs = 0.064*(1.-qfec)*ups
         
         ! Temperaturabhaengigkeit
         if (tempw(ior) < ztmax) then
            w = log(zqz10) * (ztmax - ztopt)
            x = (w / 20. * (1. + sqrt(1. + 40./w)))**2
            hcont = ((ztmax - tempw(ior)) / (ztmax - ztopt) * exp(1. - (ztmax - tempw(ior)) / (ztmax - ztopt)))**x
         else
            hcont = 0.0
         endif
         
         ! Berechnung der Respirationsrate in gC/(m2Gewaesserbod.*d)
         if (gewdr(ior,ndr) > 0.0) then
            rres   = rres0 * gewdr(ior,ndr)**(-0.25)
            respc  = rres  * hcont * Yc  + qres * assr
            respcs = rres  * hcont * Ycs + qres * assrs
         else
            rres   = 0.
            respc  = 0.
            respcs = 0.
         endif
         
         ! zeitl. Aenderung der Dreissena-Biomasse in gC
         dYc  = (assr  - respc  - exdr ) * tflie
         Yc   = Yc + dYc
         zdrei(ior,ndr)  = Yc
         dYcs = (assrs - respcs - exdrs) * tflie
         Ycs  = Ycs + dYcs
         zdreis(ior,ndr) = Ycs
         
         FH2oVol = FR * Yc + FR * Ycs
         FH2oVOL = FH2oVOL * tflie
         
         ! Gewichtsaenderung einer Muschel
         drmue = (1. - qfec) * idr * ( 1. - 0.064 - qres) - rres * hcont
         dgewdr = gewdr(ior,ndr) * drmue * tflie
         gewdr(ior,ndr) = gewdr(ior,ndr) + dgewdr
         drmas(ior,ndr) = drmue
         drakr(ior,ndr) = (1.-qfec) * qres * idr
         drbar(ior,ndr) = rres * hcont
         
         ! Umrechnung in mg/l
         !.Annahme: 1 mg respirierte Biomasse verbraucht 5.59 mg O2(Schneider)
         vol = flae(ior) * elen(ior)
         if (vol == 0.0) then
            dlarvn(anze+1) = dlarvn(anze)
            jahr_tst1 = jahrs
            return
         endif
         
         FH2oVOL = FH2oVOL / vol
         
         rescm3 = (respc + respcs) / vol
         excm3  = (exdr  + exdrs ) / vol
         
         respbio    = rescm3 / drCgeh ! Respiration Umrechnung von C in Biomasse
         resdr(ior) = resdr(ior) + respbio *tflie
         
         exdrvz      = exdrvz + excm3 * tflie
         exdrvg(ior) = exdrvz * hconvg / Cagr
         exdrvk(ior) = exdrvz * hconvk / Caki
         exdrvb(ior) = exdrvz * hconvb / Cabl
         
         respo2     = respbio * 5.59
         ro2dr(ior) = ro2dr(ior) + respo2 * tflie
         
         upt   = up  * tflie
         upst  = ups * tflie
         uptm3 = (upt + upst)/vol
         
         adrg(ndr)   = uptm3 * hconvg / Cagr
         adrk(ndr)   = uptm3 * hconvk / Caki
         adrb(ndr)   = uptm3 * hconvb / Cabl
         drfecg(ndr) = qfec  * adrg(ndr)
         drfeck(ndr) = qfec  * adrk(ndr)
         drfecb(ndr) = qfec  * adrb(ndr)
         drfecs(ndr) = qfec  * drss(ndr)
         vofkop(ndr) = fh2ovol * 100.
         
         Filaki(ndr) = aki(ior) * pki * fh2ovol
         Filagr(ndr) = agr(ior) * pgr * fh2ovol
         Filabl(ndr) = abl(ior) * pbl * fh2ovol
         filss(ndr)  = ss(ior)        * fh2ovol
         
         if (adrk(ndr) > 0.0 .and. adrk(ndr) > Filaki(ndr)) then
            Filaki(ndr) = adrk(ndr)
            filagr(ndr) = adrg(ndr)
            filabl(ndr) = adrb(ndr)
            vofkop(ndr) = Filaki(ndr) / (aki(ior) * pki) * 100.
         elseif (adrg(ndr) > 0.0 .and. adrg(ndr) > Filagr(ndr)) then
            Filaki(ndr) = adrk(ndr)
            filagr(ndr) = adrg(ndr)
            filabl(ndr) = adrb(ndr)
            vofkop(ndr) = Filagr(ndr) / (agr(ior) * pgr) * 100.
         elseif (adrb(ndr) > 0.0 .and. adrb(ndr) > Filabl(ndr)) then
            Filaki(ndr) = adrk(ndr)
            filagr(ndr) = adrg(ndr)
            filabl(ndr) = adrb(ndr)
            vofkop(ndr) = Filabl(ndr) / (abl(ior) * pbl) * 100.
         endif
         
         dchlg(ndr) = 0.0
         dchlk(ndr) = 0.0
         dchlb(ndr) = 0.0
         if (agbcm(ior) > 0.0) dchlg(ndr) = filagr(ndr) * 1000. * Cagr / agbcm(ior)
         if (akbcm(ior) > 0.0) dchlk(ndr) = filaki(ndr) * 1000. * Caki / akbcm(ior)
         if (abbcm(ior) > 0.0) dchlb(ndr) = filabl(ndr) * 1000. * Cabl / abbcm(ior)
         
         filHNF(ndr) = CHNF(ior) * fh2ovol
         
      enddo
      
      algdrg(ior) = 0.0
      algdrk(ior) = 0.0
      algdrb(ior) = 0.0
      ssdr(ior)   = 0.0
      drfaek(ior) = 0.0
      drfaeg(ior) = 0.0
      drfaeb(ior) = 0.0
      drfaes(ior) = 0.0
      draup       = 0.0
      volfdr(ior) = 0.0
      drHNF(ior)  = 0.0
      
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
         drHNF(ior)  = drHNF(ior) + filHNF(ndr)
      enddo
      
      ! Ausgabe
      HNFdra(ior) = 0.0
      if (CHNF(ior) > 0.0) HNFdra(ior) = drHNF(ior) / CHNF(ior) * 24.
      
      ! Schwebstoffaufnahme durch Dreissena wird vorläufig auf Null gesetz
      ssdr(ior) = 0.0
      ! Pseudo faces fraction of Dreissena
      if (algdrg(ior)+algdrk(ior)+algdrb(ior) /= 0.) then
         if (draup /= 0.0) then
            drpfec(ior) = max(0., 100. * (1. - draup / (algdrg(ior) + algdrk(ior) + algdrb(ior))))
         else
            drpfec(ior) = 0.0
         endif
      else
         drpfec(ior) = 0.0
      endif
      if (drpfec(ior) == 0.0) volfdr(ior) = 0.0
      
      ! Berechnung der Larvenbildung
      ddlarn = 0.0
      dlamor = 0.0
      dlafes = 0.0
      dlmax(ior)  = dlmax(ior)  * (2. * lboem(ior) * elen(ior))
      dlmaxs(ior) = dlmaxs(ior) * (    bsohlm(ior) * elen(ior))
      if (lait1 == 0 .and. laim1 == 0) then
         dlmax(ior)  = dlmax(ior)  /  (2. * lboem(ior) * elen(ior))
         dlmaxs(ior) = dlmaxs(ior) /  (    bsohlm(ior) * elen(ior))
         cycle   !  next iteration of ior-loop
      endif
      
      !TODO FG: introduced this switch to get rid of last goto after several code rearrangements
      calculate_dfmue = .true.
      
      if (drft < laid1) then
         if (ilang == 0 .or. jahr_tst1 < jahrs) then
            drrt = 0.0
            dlmax(ior)  = dlmax(ior)  /  (2. * lboem(ior) * elen(ior))
            dlmaxs(ior) = dlmaxs(ior) /  (    bsohlm(ior) * elen(ior))
            cycle   !  next iteration of ior-loop
         endif
         
         NRS = ITAGS + 31 * (MONATS - 1)
         if (monats > 2) NRS = NRS - INT(0.4 *MONATS + 2.3)
         
         NRla1a = lait1 + 31 * (laim1 - 1)
         if (laim1 > 2) NRla1a = NRla1a - INT(0.4 * laim1 + 2.3)
         
         nrla1e = nrla1a + laid1
         
         drrt1 = 0.0
         drrt3 = 30.
         drrt2 = 0.5 * drrt3
         
         if (nrs < nrla1a .or. nrs >= nrla1e) then
            drrt  = 0.
         else
            drrt11 = 0.0
            drrt33 = laid1 - drrt3
            drrt22 = 0.5 * drrt33
            
            ! Annahme Gewichtverlust der Adulten durch Reproduktion
            if (drrt > drrt3) then
               spwmx = 2. * flai * 0.4 / (laid1 - drrt3)
            else
               spwmx = 2. * flai * 0.6 / drrt3
            endif
            
            if (dlmax(ior) == 0.0 .and. dlmaxs(ior) == 0.0) then
               dlmax(ior) = zdrei(ior,2)
               dlmaxs(ior) = zdreis(ior,2)
               gwdmax(ior) = gewdr(ior,2)
               sgwmue(ior) = 0.0
            endif
            
            if (drrt > drrt3) then
               drrtt = drrt -drrt3
               if (drrtt > drrt22) then
                  fdrrt = ((drrtt-drrt33)**2)/((drrtt-drrt22)**2+(drrtt-drrt33)**2)
               else
                  fdrrt = ((drrtt-drrt11)**2)/((drrtt-drrt22)**2+(drrtt-drrt11)**2)
               endif
            else
               if (drrt > drrt2) then
                  fdrrt = ((drrt-drrt3)**2)/((drrt-drrt2)**2+(drrt-drrt3)**2)
               else
                  fdrrt = ((drrt-drrt1)**2)/((drrt-drrt2)**2+(drrt-drrt1)**2)
               endif
            endif
            fdrrt = fdrrt*spwmx
            
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
               zdrei(ior,ndr) = zdrei(ior,ndr)-(dlmax(ior)*tflie*fdrrt)
               zdrei(ior,ndr) = zdrei(ior,ndr)-dyc*0.52
               zdreis(ior,ndr) = zdreis(ior,ndr)-(dlmaxs(ior)*tflie*fdrrt)
               zdreis(ior,ndr) = zdreis(ior,ndr)-dycs*0.52
            enddo
         endif
         
         dlamor = dlarvn(ior)*(1.-exp(-klmor*tflie))
         
         if (nrs >= nrla1a .and. itime_hoch == 1) stdpla = stdpla + tflie
         if (stdpla >= tdpla) then
            if (itime_hoch == 1) then
               drft = drft+tflie
               itime_hoch = 0
            endif
            
            if (drft > drrt3) then
               spwmx = 2. * flai * 0.4 / (laid1 - drrt3)
            else
               spwmx = 2. * flai * 0.6 / drrt3
            endif
            
            if (drft > drrt3) then
               drftt = drft - drrt3
               if (drftt > drrt22) then
                  fdrrt = ((drftt - drrt33)**2) / ((drftt - drrt22)**2 + (drftt - drrt33)**2)
               else
                  fdrrt = ((drftt - drrt11)**2) / ((drftt - drrt22)**2 + (drftt - drrt11)**2)
               endif
            else
               if (drft > drrt2) then
                  fdrrt = ((drft - drrt3)**2) / ((drft - drrt2)**2 + (drft - drrt3)**2)
               else
                  fdrrt = ((drft - drrt1)**2) / ((drft - drrt2)**2 + (drft - drrt1)**2)
               endif
            endif
            fdrrt = fdrrt * spwmx
            
            ! Larvengewicht beim Festsetzen: 8.6e-8 gC; 8.6e-5 mgC
            dlafes = (dlmax(ior) + dlmaxs(ior)) * tflie * fdrrt / 3.35e-9
            dlafes = dlafes * 0.75 * fgesund * fweib
         else
            drft = 0.0
            itime_hoch = 0
            calculate_dfmue = .false.
         endif
      endif
      
      if (calculate_dfmue) then
         if (zdreis(ior,2) > 0.0 .or. zdrei(ior,2) > 0.0) then
            dfemue = sgwmue(ior)/(tdpla*1./tflie)
            sgwmue(ior) = sgwmue(ior)-dfemue
            dfmue  = dfemue * (2. * lboem(ior) * elen(ior)) *                 &
                     (zdrei(ior,2)  / (zdrei(ior,2) + zdreis(ior,2)))
            dfmues = dfemue *     (bsohlm(ior) * elen(ior)) *                 &
                     (zdreis(ior,2) / (zdrei(ior,2) + zdreis(ior,2)))
            dfemue = (dfmue + dfmues) / 3.35e-9 * 0.75 * fgesund * fweib
         else
            dfmue  = 0.0
            dfmues = 0.0
            dfemue = 0.0
         endif
      
         dlafes = dlafes + dfemue
         dlafes = dlafes * exp(-klmorg) / (vol * 1000.)
      endif
      
      dlarvn(ior) = max(0., dlarvn(ior) + ddlarn - dlamor - dlafes)
      
      do ndr = 1,nndr
         ddrein = 0.0
         
         hconds(ndr) = zdreis(ior,ndr)
         hcondb(ndr) = zdrei(ior,ndr)
         
         if (gewdr(ior,ndr) > 0.0) then
            dreisn = ((zdrei(ior,ndr)+zdreis(ior,ndr))*1000.) / gewdr(ior,ndr)
            dreing = dreisn
         else
            dreing = 0.0
            dreisn = 0.0
         endif

         if (ndr == 1 .and. dlafes > 0.0) then
            gewdts = (dreisn*gewdr(ior,1)+dlafes*vol*1000.*8.6e-5)            &
                     /(dreisn+dlafes*vol*1000.)
            if (gewdts > 0.0246) then
               dlafes = dlafes*exp(-2. * tflie)
               gewdts = (dreisn*gewdr(ior,1)+dlafes*vol*1000.*8.6e-5)            &
                        /(dreisn+dlafes*vol*1000.)
            endif
            
            gewdr(ior,1) = gewdts
            
            dreing = dreisn+dlafes*vol*1000.
         elseif (ndr == 2 .and. gewdr(ior,1) > 1.6) then
            ddrein = ((zdrei(ior,1)+zdreis(ior,1))*1000.)/gewdr(ior,1)
            dreing = dreisn+ddrein
            !
            gewdr(ior,2) = (dreisn*gewdr(ior,2)+ddrein*gewdr(ior,1))          &
                           /(dreisn+ddrein)
            !
            gewdr(ior,1) = 0.0
            zdrei(ior,1) = 0.0
            zdreis(ior,1) = 0.0
         endif
         
         if (gewdr(ior,ndr) < 0.0246) then
            dmorg = 0.1
         elseif (gewdr(ior,ndr) > 0.0246) then
            dmorg = 0.0157*gewdr(ior,ndr)**(-0.502)
         endif
         ! nur ausschreiben
         drmor(ior,ndr) = dmorg
         if (gewdr(ior,ndr) == 0.0) drmor(ior,ndr) = 0.0
         
         dreinm = dreing*(1.-exp(-dmorg*tflie))
         dreing = max(0., dreing - dreinm)
         
         if (zdrei(ior,ndr) + zdreis(ior,ndr) == 0.0 .and. (ndr == 1 .or. (ndr == 2 .and. ddrein == 0.0))) then
            zdrei(ior,ndr)  = 0.0
            zdreis(ior,ndr) = 0.0
            cycle
         endif
         
         if (ndr == 1 .and. dlafes > 0.0 .and. zdrei(ior,ndr)+zdreis(ior,ndr) == 0.0) then
            hcond1 = zdrei(ior,2) / (zdrei(ior,2)+zdreis(ior,2))
         else
            if ((zdrei(ior,ndr)+zdreis(ior,ndr)) == 0.0 .and. ndr == 2 .and. ddrein > 0.0) then
               hcond1 = hcondb(1) / (hcondb(1) + hconds(1))
            else
               hcond1 = hcondb(ndr) / (hcondb(ndr) + hconds(ndr))
            endif
         endif
         hcond2 = 1. - hcond1
         zdrei(ior,ndr)  = dreing * gewdr(ior,ndr) / 1000. * hcond1
         zdreis(ior,ndr) = dreing * gewdr(ior,ndr) / 1000. * hcond2
      enddo
      
      do ndr = 1,nndr
         zdrei(ior,ndr) = zdrei(ior,ndr) / (2. * lboem(ior) * elen(ior))
         if (bsohlm(ior) <= 0.0) then
            zdreis(ior,ndr) = 0.0
            cycle
         endif
         zdreis(ior,ndr) = zdreis(ior,ndr) / (bsohlm(ior) * elen(ior))
      enddo
      
      dlmax(ior)  = dlmax(ior)  /  (2. * lboem(ior) * elen(ior))
      dlmaxs(ior) = dlmaxs(ior) /  (    bsohlm(ior) * elen(ior))
      
   enddo
   
   dlarvn(anze+1) = dlarvn(anze)
   jahr_tst1      = jahrs
   
   return
end
