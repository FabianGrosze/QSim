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
!> ini_algae() wird beschrieben in: \ref lnk_phytoplankton
!! Setzung von Parametern, die alle in QSimDatenfelder vereinbart wurden. 
!! Berechnung des Chlorophyll-a/Kohlenstoff-Verhältnisses
!! Angabe in mgChla/mgC
!!
!! Quelle zuflussrand.f90
subroutine ini_algae(akchl, abchl, agchl, a1Ki, a1Bl, a1Gr)
   implicit none
   
   real, intent(in)   :: akchl, abchl, agchl
   real, intent(out)  :: a1Ki, a1Bl, a1Gr
   
   ! TODO (Schönung, June 2023)
   ! Don't use '==' when comparing real-values
       
   a1Ki = -1.
   a1Bl = -1.
   a1Gr = -1.
   
   if (akchl == 13.3) a1Ki = -0.059
   if (akchl == 20.4) a1Ki = -0.064
   if (akchl == 65.7) a1Ki = -0.039
   if (akchl == 20.6) a1Ki = -0.055
   if (akchl == 21.9) a1Ki = -0.039
   if (a1Ki == -1.)   a1Ki = -0.059
   
   
   if (abchl == 68.)  a1Bl = -0.062
   if (abchl == 86.)  a1Bl = -0.055
   if (abchl == 20.6) a1Bl = -0.055
   if (abchl == 35)   a1Bl = -0.039
   if (a1Bl == -1.)   a1Bl = -0.062
   
   if (agchl == 12.4) a1Gr = -0.032
   if (agchl == 32.2) a1Gr = 0.0144
   if (agchl == 20.6) a1Gr = -0.055
   if (agchl == 15.6) a1Gr = -0.039
   if (a1Gr == -1.)   a1Gr = -0.032
   
   return
end subroutine ini_algae


!> algae_start() wird beschrieben in: \ref lnk_phytoplankton
!! 
!! Berechnung des Chlorophyll-a/Kohlenstoff-Verhaeltnisses
!! Angabe in mgChla/mgC
!! Temperaturabhängigkeit des C:Chla-Verhältnisses   
!! ag(k,b)chl gilt für 20°C  in mgC/mgChla
!! mg Algenbiomasse, Chla in µg/l
subroutine algae_start(chlas, vkigrs, antbls, tempws, &
                       akbcms, abbcms, agbcms,        &
                       akis, abls, agrs,              &
                       a1Ki, a1Bl, a1Gr,              &
                       chlaks, chlabs, chlags)
                       
   use module_aparam
   implicit none
   
   ! --- dummy arguments ---
   real, intent(in)  :: chlas 
   real, intent(in)  :: vkigrs
   real, intent(in)  :: antbls
   real, intent(in)  :: tempws
   real, intent(out) :: akbcms, abbcms, agbcms
   real, intent(out) :: akis, abls, agrs
   real, intent(in)  :: a1Ki, a1Bl, a1Gr
   real, intent(out) :: chlaks, chlabs, chlags
   
   ! --- local variables ---
   real :: CChl0k,CChl0b,CChl0g

   real, parameter :: te0 = 20.
   
   ! ag(k,b)chl gilt für 20°C  in mgC/mgChla
   CChl0k = akchl  * exp(-a1Ki * Te0)              ! C:Chla bei 0°C für Kieselalgen mgC/mgChla
   CChl0b = abchl  * exp(-a1Bl * Te0)              ! Blaualgen
   CChl0g = agchl  * exp(-a1Gr * Te0)              ! Grünalgen
   akbcms = CChl0k * exp(a1Ki * tempws )
   abbcms = CChl0b * exp(a1Bl * tempws )
   agbcms = CChl0g * exp(a1Gr * tempws )
   
   ! mg Algenbiomasse, Chla in µg/l
   akis = (chlas * vkigrs /1000.) * (akbcms /Caki)
   abls = (chlas * antbls /1000.) * (abbcms /Cabl)
   agrs = (chlas * (1.-vkigrs -antbls )/1000.) * (agbcms /Cagr)
   
   chlaks = chlas *vkigrs
   chlabs = chlas *antbls
   chlags = chlas *(1.-vkigrs -antbls )
   
   return
end subroutine algae_start


!> orgc_start() wird beschrieben in: \ref lnk_orgC
!!
!! Quelle zuflussrand.f90
!! Berechnung der "BSB-Komponenten" am oberen Rand
!! (auch bei Stundenwert-Generierung)
subroutine orgc_start(TOC_CSB, bsbZoo,              &
                      akis, abls, agrs,             &
                      zooins, vbsbs, vcsbs,         &
                      obsbs, ocsbs,                 &
                      CMs, CDs1, CDs2, CPs1, CPs2,  &
                      ssalgs, frfgrs, BACs, CHNFs,  &
                      CPges, CDges, Cref, TOC)
   use module_aparam
   implicit none
   
   ! --- dummy arguments ---
   real, intent(out)   :: TOC_CSB
   real, intent(out)   :: bsbZoo
   real, intent(in)    :: akis
   real, intent(in)    :: abls
   real, intent(in)    :: agrs
   real, intent(inout) :: zooins 
   real, intent(inout) :: vbsbs 
   real, intent(inout) :: vcsbs
   real, intent(out)   :: obsbs ,ocsbs
   real, intent(out)   :: CMs 
   real, intent(out)   :: CDs1, CDs2
   real, intent(out)   :: CPs1, CPs2
   real, intent(in)    :: ssalgs
   real, intent(out)   :: frfgrs
   real, intent(out)   :: BACs
   real, intent(in)    :: CHNFs
   real, intent(out)   :: CPges
   real, intent(out)   :: CDges
   real, intent(out)   :: Cref
   real, intent(out)   :: TOC
   
   ! --- local variables ---
   real :: algb5, algcs, alphlP, zoobsb, zoocsb
   real :: vcb, antBAC, BTOC5s, BTOCs, alphaD, alphlD 
   real :: hc_CPg, fak_aCref
   
   ! TODO (Schönung, june 2023): define toc_csb gloablly
   TOC_CSB = 3.1
   bsbZoo = 1.6
   
   algb5 = akis * caki * bsbki  &
         + abls * cabl * bsbbl  &
         + agrs * cagr * bsbgr
         
   algcs = akis * caki * csbki  &
         + abls * cabl * csbbl  &
         + agrs * cagr * csbgr
   
   if (zooins < 0.0) zooins = 0.0
   zoobsb = (zooins * grot / 1000.) * bsbzoo
   zoocsb = zooins * (grot * czoo / 1000.) * toc_csb
   
   if (vbsbs == 0.0 .and. vcsbs > 0.0) vbsbs = vcsbs*0.1
   obsbs = vbsbs - algb5 - zoobsb
   if (obsbs < 0.25) obsbs = 0.25
   if (vbsbs > 0.0 .and. vcsbs == 0.0) vcsbs = vbsbs/0.1
   
   ocsbs = vcsbs - algcs - zoocsb
   ocsbs = max(ocsbs, 2.5)
   
   vbsbs = obsbs + algb5 + zoobsb
   vcsbs = ocsbs + algcs + zoocsb
   vcb = obsbs * 1.5 / ocsbs
   antbac = 0.0462 * vcb
   cms = 0.03
   
   ! Berechnung des BTOC
   btoc5s = obsbs*1.5/toc_csb
   btocs = btoc5s*0.8822*vcb**(-0.988)
   
   ! Berechnung des Anteils an gelösten org. C-Verbindungen
   alphaD = 0.802 * vcb + 0.1
   if (alphaD > 1.0) alphaD = 0.95
   if (alphaD < 0.0) alphaD = 0.0
   
   CDges = BTOCs * alphaD - CMs
   if (CDges < 0.00001) CDges = 0.00001
   
   ! Aufteilung der gelösten Fraktion in leicht und schwer abbaubar
   alphlD = 0.864 * vcb + 0.161
   if (alphlD < 0.0) alphlD = 0.0
   CDs1 = CDges * alphlD
   CDs2 = CDges * (1.-alphlD)
   
   ! Aufteilung der partikulären Fraktion in leicht und schwer abbaubar
   CPges = BTOCs * (1. - alphaD)
   
   ! hc_CPg : org C-Gehalt des Sestons
   ! hc_CP  : abbaubarer C-Gehalt des Sestons, soll nicht kleiner sein als der abbaubare Anteil des part. Kohlensoffs
   ! 0.45   :
   ! 0.4    :
   ! refr. partik. Anteil (ant_CrefP) ergibt sich aus dem Verhältnis von CPges und CDges
   if (ssalgs > 0.0) then
      hc_CPg = ssalgs                  &
             - akis - agrs - abls      &
             - (zooins * grot / 1000.)
      !FG: TODO should only one factor be used?
      hc_CPg = hc_CPg * 0.45 * 0.4
      TOC = max((BTOCs+0.01), ocsbs/TOC_CSB)
      Cref = TOC - BTOCs

      fak_aCref = CPges / (CDges + CPges)  ! Anteil ref. am hc_CPg?
      
      
      ! Test ob errechneter CPges größer als hc_CP ist --- löschen?
      ! hc_CP = hc_CPg - hc_CPg * fak_aCref
      ! if (CPges > hc_CP) then
      !    CPges = hc_CP
      !    CDges = BTOCs - CPges
      !    CDs(mstr,1,mRB) = CDges*alphlD
      !    CDs(mstr,2,mRB) = CDges*(1.-alphlD)
      ! endif
   endif
   
   BACs = CPges * antBAC
   CPges = CPges - BACs
   
   alphlP = 1.807 * vcb + 0.006
   if (alphlP < 0.0) alphlP = 0.0
   
   CPs1 = CPges * alphlP
   CPs2 = CPges * (1. - alphlP)
   
   ! Verringerung der einzelnen Fraktionen aufrund von HNF
   cds1 = cds1 - 0.10 * chnfs
   cds2 = cds2 - 0.10 * chnfs
   cps1 = cps1 - 0.35 * chnfs
   cps2 = cps2 - 0.35 * chnfs
   if (cds1 <= 0.0) cds1 = 0.000001
   if (cds2 <= 0.0) cds2 = 0.000001
   if (cps1 <= 0.0) cps1 = 0.000001
   if (cps2 <= 0.0) cps2 = 0.000001
   
   ! TODO (Große):
   ! ant_CrefP was not assigned a value.
   ! Hence, replaced line with line below, assuming that:
   ! ant_CrefP = Cref / (CPges + CDges)
   ! frfgrs = min(1.,(hc_CPg * ant_CrefP/Cref))
   frfgrs = min(1.,(hc_CPg / (CPges + CDges)))
   return
end subroutine orgc_start


!> naehr_start() wird beschrieben in: \ref lnk_stickstoff und \ref lnk_phosphor
!!
!! Quelle zuflussrand.f90
subroutine naehr_start(akis, abls, agrs, vnh4s, vno3s , vno2s, gesns, zooins,  &
                       gelps, gesps, q_nks, q_pks, q_sks, q_ngs, q_pgs, q_nbs, &
                       q_pbs, cpges, cdges, cref, bacs, cms, nl0s, pl0s, sss,  &
                       ssalgs, mstr, mrb, einmalig,                            &
                       control, jjj)
   
   use module_aparam
   implicit none
   
   ! --- dummy arguments ---
   real, intent(in)       :: akis, abls, agrs
   real, intent(in)       :: vnh4s 
   real, intent(in)       :: vno3s
   real, intent(in)       :: vno2s
   real, intent(inout)    :: gesns
   real, intent(in)       :: zooins
   real, intent(in)       :: gelps
   real, intent(inout)    :: gesps
   real, intent(out)      :: q_nks, q_pks, q_sks
   real, intent(out)      :: q_ngs, q_pgs
   real, intent(out)      :: q_nbs, q_pbs
   real, intent(in)       :: cpges
   real, intent(in)       :: cdges
   real, intent(in)       :: cref
   real, intent(in)       :: bacs
   real, intent(in)       :: cms
   real, intent(out)      :: nl0s
   real, intent(out)      :: pl0s
   real, intent(out)      :: sss 
   real, intent(in)       :: ssalgs
   integer, intent(in)    :: mstr
   integer, intent(in)    :: mrb
   logical, intent(inout) :: einmalig
   logical, intent(in)    :: control !< degbugging
   integer, intent(in)    :: jjj      !< debugging
   
   
   ! --- local variables ---
   real    :: hcsum, hcsump, sum_n, f_nk
   real    :: orgn, orgc, akinmx,agrnmx, ablnmx, algnmx, gsn
   real    :: hcon1, hcon2, dgsn, dalgn, fanneu
   real    :: orgp, algpmx, gsp, dgsp, fapneu, dalgp
   real    :: hcq_nk, hcq_ng, hcq_nb
   real    :: hcq_pk, hcq_pg, hcq_pb
   integer :: m
   
   
   ! cellular nutrient levels
   sum_n = vnh4s + vno3s
   
   ! TODO (Große): 
   ! these calculations imply that all cell quotas are set to their maxima
   f_nk = 0.582 * (sum_n / (sum_n + 0.011))
   f_nk = 1.
   q_nks = qmx_nk - (1. - f_nk) * (qmx_nk - qmn_nk)
   q_pks = qmn_pk + (qmx_pk - qmn_pk) / 1.
   q_sks = qmx_sk
   q_ngs = qmn_ng + (qmx_ng - qmn_ng) / 1.
   q_pgs = qmn_pg + (qmx_pg - qmn_pg) / 1.
   q_nbs = qmn_nb + (qmx_nb - qmn_nb) / 1.
   q_pbs = qmn_pb + (qmx_pb - qmn_pb) / 1.
   
   ! organic carbon
   orgc = cref + cdges + cpges + bacs + cms
   
   if (gesns < 0.0) then
      ! kein gesn am oberen Profil vorhanden -  wird errechnet
      hcsum = agrs + akis + abls  &
            + zooins              &
            + vnh4s               &
            + vno2s               &
            + vno3s
      
      if (hcsum == 0.0) then
         nl0s = -1.
         gesns = -1.
      else
         nl0s  = 0.04
         gesns = nl0s * orgc                  &
               + agrs * q_ngs                 &
               + abls * q_nbs                 &
               + akis * q_nks                 &
               + (zooins * grot/1000.) * nzoo &
               + vnh4s                        & 
               + vno2s                        &
               + vno3s
            
      endif
   
   else
      orgn = gesns                       &
           - agrs * q_ngs                &
           - abls * q_nbs                &
           - akis * q_nks                &
           - (zooins*grot/1000.) * nzoo  &
           - vnh4s                       & 
           - vno2s                       &
           - vno3s
      
      ! N:C ratio of detritus and bacteria
      if (orgc > 0.) then
         nl0s = orgn / orgc
      else
         nl0s = 0.
      endif
      
      if (nl0s <= 0.04) then
      
         nl0s = 0.04
         do m = 1,2
            orgn   = nl0s * orgc
            akinmx = akis * q_nks
            agrnmx = agrs * q_ngs
            ablnmx = abls * q_nbs
            algnmx = akinmx + agrnmx + ablnmx
            gsn = algnmx                     &
                + orgn                       &
                + (zooins*grot/1000.) * nzoo &
                + vnh4s                      &
                + vno2s                      &
                + vno3s
            if (gsn <= gesns) exit
            
            hcon1 = gsn  - gesns
            hcon2 = orgn - hcon1
            if (orgc > 0.) then
               nl0s = hcon2 / orgc
               if (nl0s < 0.01 .and. einmalig) then
                  ! Fehlermeldung
                  write(*, 1405)mrb,mstr,jjj   ! jjj-3d knotennummer
                  write(*,*) "  Sum of organic N, NH4, NO2 and NO3 is greater &
                              &than total N. set organic N:C to 0.01."
                  einmalig = .false.
                  nl0s = 0.01
                  exit
               endif
            else
               write(*,1405) mrb,mstr,jjj   ! jjj-3d knotennummer
               write(*, '(a)') "  Sum of detrital and bacterial organic carbon &
                               &less or equal 0. Set organic N:C to 0.01."
               nl0s = 0.01
               exit
            endif
         enddo
         
         if (algnmx > 0.0) then
            dgsn  = gsn - gesns
            dalgn  = algnmx - dgsn
            fanneu = dalgn / algnmx
            q_nks  = q_nks * fanneu
            q_ngs  = q_ngs * fanneu
            q_nbs  = q_nbs * fanneu
         else
            q_nks  = 0.
            q_ngs  = 0.
            q_nbs  = 0.
         endif
         
         hcq_nk = 0.7 * (qmx_nk - qmn_nk) + qmn_nk
         hcq_ng = 0.7 * (qmx_ng - qmn_ng) + qmn_ng
         hcq_nb = 0.7 * (qmx_nb - qmn_nb) + qmn_nb
         
         if (q_nks < hcq_nk) q_nks = hcq_nk
         if (q_ngs < hcq_ng) q_ngs = hcq_ng
         if (q_nbs < hcq_nb) q_nbs = hcq_nb
      endif
   endif
   
      
   if (gesps < 0.0) then
      ! kein gesp am oberen Profil vorhanden - wird errechnet
      hcsump = agrs + akis + abls &
             + zooins             &
             + gelps
      
      if (hcsump == 0.0) then
         pl0s  = -1.
         gesps = -1.
      else
         pl0s = 0.01
         gesps = pl0s * orgc                  &
               + agrs * q_pgs                 &
               + abls * q_pbs                 &
               + akis * q_pks                 &
               + (zooins * grot/1000.) * pzoo &
               + gelps
      endif
      
   else
      orgp = gesps                        &
           - agrs * q_pgs                 &
           - abls * q_pbs                 &
           - akis * q_pks                 &
           - (zooins * grot/1000.) * pzoo &
           - gelps
      
      if (orgc > 0.) then
         pl0s = orgp / orgc
      else
         pl0s = 0.
      endif
      
      
      if (pl0s <= 0.005) then
         pl0s = 0.005
         do m = 1, 2
            orgp   = pl0s * orgc
            algpmx = akis * q_pks &
                   + agrs * q_pgs &
                   + abls * q_pbs
                   
            gsp    = algpmx                         &
                   + orgp                           &
                   + (zooins * grot / 1000.) * pzoo &
                   + gelps
            
            if (gsp <= gesps) exit
            hcon1 = gsp - gesps
            hcon2 = orgp - hcon1
            
            if (orgc > 0.) then
               pl0s = hcon2 / orgc
               if (pl0s < 0.001) then
                  ! Fehlermeldung
                  write(*, 1405) mrb, mstr, jjj 
                  write(*,'(a)') "  Sum of organic and dissolved P is greater&
                                 & than total P. Set organic P:C to 0.001."
                  pl0s = 0.001
                  exit
               endif
            else
               write(*, 1405)mrb,mstr,jjj 
               write(*, '(a)') "  Sum of detrital and bacterial organic carbon&
                               &less or equal 0. Set organic P:C to 0.001."
               pl0s = 0.001
               exit
            endif
         enddo
         
         1405 format('  zufluss:',i3,'  strang-nr.:',i3,'   jjj = ',i8)
         
         if (algpmx > 0.0) then
            dgsp   = gsp - gesps
            dalgp  = algpmx - dgsp
            fapneu = dalgp / algpmx
            q_pks  = q_pks * fapneu
            q_pgs  = q_pgs * fapneu
            q_pbs  = q_pbs * fapneu
         else
            q_pks = 0.
            q_pgs = 0.
            q_pbs = 0.
         endif
         
         hcq_pk = 0.7 * (qmx_pk - qmn_pk) + qmn_pk
         hcq_pg = 0.7 * (qmx_pg - qmn_pg) + qmn_pg
         hcq_pb = 0.7 * (qmx_pb - qmn_pb) + qmn_pb
         
         if (q_pks < hcq_pk) q_pks = hcq_pk
         if (q_pgs < hcq_pg) q_pgs = hcq_pg
         if (q_pbs < hcq_pb) q_pbs = hcq_pb
      endif
   endif
   
   sss = ssalgs               &
       - akis - agrs - abls   &
       - (zooins*grot/1000.)
   sss = max(1., sss)
   
   return
   
end subroutine naehr_start
