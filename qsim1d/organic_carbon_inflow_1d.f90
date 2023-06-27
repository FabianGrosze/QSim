subroutine organic_carbon_inflow_1d(ocsb, obsb, CD, CP, CM, BAC, fbsgr, frfgr, &
                                    vkigr, antbl, ecsb, ebsb, echla, evkigr,   &
                                    eantbl, ezind, eCD, eCP, eCM, eBAC, frfgrs,&
                                    fbsgrs, akbcm, agbcm, abbcm, bsbzoo,       &
                                    toc_csb, mstr, ieinLs, qeinlL, qeinl,      &
                                    vabfl, iorLe, iorLa, jiein, flae, anze,    &
                                    flag, tflie)
   use module_alloc_dimensions
   use module_aparam
                      
                     
   implicit none
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2)          :: ocsb    !< CSB
   real,    intent(inout), dimension(ialloc2)          :: obsb    !< BSB
   real,    intent(inout), dimension(2,ialloc2)        :: cd      !< leicht und schwer abbaubare gelöste organische C-Verbindungen.
   real,    intent(inout), dimension(2,ialloc2)        :: cp      !< leicht und schwer abbaubare partikuläre organische C-Verbindungen.
   real,    intent(inout), dimension(ialloc2)          :: cm      !< monomolekularen organischen C-Verbindungen
   real,    intent(inout), dimension(ialloc2)          :: bac     !< Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
   real,    intent(inout), dimension(ialloc2)          :: fbsgr   !<
   real,    intent(inout), dimension(ialloc2)          :: frfgr   !<
   real,    intent(in),    dimension(ialloc2)          :: vkigr   !< Anteil der Kieselagen
   real,    intent(in),    dimension(ialloc2)          :: antbl   !< Anteil der Blaualgen
   real,    intent(in),    dimension(ialloc1)          :: ecsb    !< CSB im Einleiter
   real,    intent(in),    dimension(ialloc1)          :: ebsb    !< BSB im Einleiter
   real,    intent(in),    dimension(ialloc1)          :: echla   !< Chlorophyll im Einleiter
   real,    intent(inout), dimension(ialloc1)          :: evkigr  !< Anteil der Kieselalgen im Einleiter
   real,    intent(inout), dimension(ialloc1)          :: eantbl  !< Anteil der Blaualgen im Einleiter
   real,    intent(in),    dimension(ialloc1)          :: ezind   !< Zooplankton im Einleiter
   real,    intent(in),    dimension(azStrs,2,ialloc1) :: ecd     !< gelöste organische C-Verbindungen im Einleiter
   real,    intent(in),    dimension(azStrs,2,ialloc1) :: ecp     !< partikuläre organische C-Verbindungen im Einleiter
   real,    intent(in),    dimension(azStrs,ialloc1)   :: ecm     !< monomolekulare organische C-Verbindungen im Einleiter
   real,    intent(in),    dimension(azStrs,ialloc1)   :: ebac    !< Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen im Einleiter
   real,    intent(in),    dimension(azStrs,ialloc1)   :: frfgrs  !< 
   real,    intent(in)                                 :: fbsgrs  !< 
   real,    intent(in),    dimension(ialloc2)          :: akbcm   !< Chla:C-Verhältnis in Kieselalgen
   real,    intent(in),    dimension(ialloc2)          :: agbcm   !< Chla:C-Verhältnis in Grünalgen
   real,    intent(in),    dimension(ialloc2)          :: abbcm   !< Chla:C-Verhältnis in Blaualgen
   real,    intent(in)                                 :: bsbzoo  !<
   real,    intent(in)                                 :: toc_csb !<
   integer, intent(in)                                 :: mstr    !< aktueller Strang
   integer, intent(in),    dimension(azStrs)           :: ieinLs  !< Anzahl der Linienquellen je Strang
   real,    intent(in),    dimension(ialloc1)          :: qeinlL  !< 
   real,    intent(in),    dimension(ialloc1)          :: qeinl   !< 
   real,    intent(in),    dimension(ialloc2)          :: vabfl   !< 
   integer, intent(in),    dimension(ialloc1)          :: iorLe   !<
   integer, intent(in),    dimension(ialloc1)          :: iorLa   !<
   integer, intent(in),    dimension(ialloc2)          :: jiein   !< Anzahl der Einleiter je Knoten
   real,    intent(in),    dimension(ialloc2)          :: flae    !< Oberfläche
   integer, intent(in)                                 :: anze    !< Anzahl Abschnitte im aktuellen Strang
   integer, intent(in),    dimension(ialloc2)          :: flag    !<
   real,    intent(in)                                 :: tflie   !< Zeitschritt [d]
 
   
   ! --- local variables ---
   integer  :: ieinl, ior, j, ior_flag, m, ihcq, ji, iein
   real     :: hcocsb, hcobsb , hcCD1, hcCD2, hcCP1, hcCP2, hcCM, hcBAC, hcfbsg, hcfrfg, hcQ
   real     :: secsb, sebsb, seCD1, seCD2, seCP1, seCP2, seCM, seBAC, seOBSB, sefbsg, sefrfg
   real     :: algb5, zoobsb, algcs, zoocsb, hcqe
   
   ! --------------------------------------------------------------------------
   ! diffuse sources (here only evaporation)
   ! --------------------------------------------------------------------------
   do ieinL = 1, ieinLs(mstr)
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior) cycle
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL) >= ior .and. qeinlL(ieinL) <= 0.0) then
            ocsb(ior) = ocsb(ior) + (-ocsb(ior) * qeinlL(ieinL) / flae(ior)) * tflie * 86400.
            obsb(ior) = obsb(ior) + (-obsb(ior) * qeinlL(ieinL) / flae(ior)) * tflie * 86400.
            CD(1,ior) = CD(1,ior) + (-CD(1,ior) * qeinlL(ieinL) / flae(ior)) * tflie * 86400.
            CD(2,ior) = CD(2,ior) + (-CD(2,ior) * qeinlL(ieinL) / flae(ior)) * tflie * 86400.
            CP(1,ior) = CP(1,ior) + (-CP(1,ior) * qeinlL(ieinL) / flae(ior)) * tflie * 86400.
            CP(2,ior) = CP(2,ior) + (-CP(2,ior) * qeinlL(ieinL) / flae(ior)) * tflie * 86400.
            CM(ior)   = CM(ior)   + (-CM(ior)   * qeinlL(ieinL) / flae(ior)) * tflie * 86400.
            BAC(ior)  = BAC(ior)  + (-BAC(ior)  * qeinlL(ieinL) / flae(ior)) * tflie * 86400.
         endif
      enddo ! Ende Knotenschleife
   enddo   ! Ende Schleife Linienquellen
   
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   do j = 1,anze+1 
      ior = j
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      
      if (flag(ior) == 4) then
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         ! Umbenennen der benötigten Variablen; 1D
         hcocsb = ocsb(ior-m)
         hcobsb = obsb(ior-m)
         hcCD1  = CD(1,ior-m)
         hcCD2  = CD(2,ior-m)
         hcCP1  = CP(1,ior-m)
         hcCP2  = CP(2,ior-m)
         hcCM   = CM(ior-m)
         hcBAC  = BAC(ior-m)
         hcfbsg = fbsgr(ior-m)
         hcfrfg = frfgr(ior-m)
         hcQ    = vabfl(ior-m)
         if (hcQ < 0.0) hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1) hcQ = 1.e-10
         
         do ji = 1,jiein(ior)
            if (ecsb(iein) <= 0.0 .and. ebsb(iein) <= 0.0) then
               secsb = hcocsb
               sebsb = hcobsb
               seCD1 = hcCD1
               seCD2 = hcCD2
               seCP1 = hcCP1
               seCP2 = hcCP2
               seCM  = hcCM
               seBAC = hcBAC
               seOBSB = hcOBSB
               sefbsg = hcfrfg
               sefrfg = hcfrfg
            else
               algb5 = 0.0
               zoobsb = 0.0
               algcs = 0.0
               zoocsb = 0.0
               if (echla(iein)>=0.0) then
                  if (evkigr(iein) < 0.0) evkigr(iein) = vkigr(ior-m)
                  if (eantbl(iein) < 0.0) eantbl(iein) = antbl(ior-m)
                  if (eantbl(iein) < 0.0) eantbl(iein) = 0.0
                  algb5 = echla(iein)*evkigr(iein)*akbcm(ior-m)*bsbki*0.001                        &
                        + (1.-evkigr(iein)-eantbl(iein))*echla(iein)*agbcm(ior-m)*bsbgr*0.001      &
                        + eantbl(iein)*echla(iein)*abbcm(ior-m)*bsbbl*0.001
                  
                  algcs = echla(iein)*evkigr(iein)*akbcm(ior-m)*csbki*0.001                        &
                        + (1.-evkigr(iein)-eantbl(iein))*echla(iein)*agbcm(ior-m)*csbgr*0.001      &
                        + eantbl(iein)*echla(iein)*abbcm(ior-m)*csbbl*0.001
               endif
               
               if (ezind(iein)>=0.0) then
                  zoobsb = (ezind(iein) * GRot/1000.) * bsbzoo
                  zoocsb = ezind(iein) * (GRot * CZoo/1000.) * TOC_CSB
               endif
               
               sebsb = ebsb(iein) - algb5 - zoobsb
               secsb = ecsb(iein) - algcs - zoocsb
               
               if (sebsb < 0.25 .and. ebsb(iein) > 0.0) sebsb = 0.25
               if (ebsb(iein) < 0.0) sebsb = hcobsb
               if (secsb < 2.5 .and. ecsb(iein) > 0.0) secsb = 2.5
               if (ecsb(iein) < 0.0) secsb = hcocsb
            endif
            
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0) seCD1 = eCD(mstr,1,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0) seCD2 = eCD(mstr,2,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0) seCP1 = eCP(mstr,1,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0) seCP2 = eCP(mstr,2,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0) seCM  = eCM(mstr,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0) seBAC = eBAC(mstr,iein)
            sefbsg = fbsgrs
            sefrfg = frfgrs(mstr,iein)
            if (seCD1 < 0.000002) seCD1 = hcCD1
            if (seCD2 < 0.000002) seCD2 = hcCD2
            if (seCP1 < 0.000002) seCP1 = hcCP1
            if (seCP2 < 0.000002) seCP2 = hcCP2
            hcQE = max(0.0,qeinl(iein))
            
            obsb(ior)  = (hcQ * hcobsb + sebsb  * hcQE) / (hcQ+hcQE)
            ocsb(ior)  = (hcQ * hcocsb + secsb  * hcQE) / (hcQ+hcQE)
            CD(1,ior)  = (hcQ * hcCD1  + seCD1  * hcQE) / (hcQ+hcQE)
            CD(2,ior)  = (hcQ * hcCD2  + seCD2  * hcQE) / (hcQ+hcQE)
            CP(1,ior)  = (hcQ * hcCP1  + seCP1  * hcQE) / (hcQ+hcQE)
            CP(2,ior)  = (hcQ * hcCP2  + seCP2  * hcQE) / (hcQ+hcQE)
            CM(ior)    = (hcQ * hcCM   + seCM   * hcQE) / (hcQ+hcQE)
            BAC(ior)   = (hcQ * hcBAC  + seBAC  * hcQE) / (hcQ+hcQE)
            fbsgr(ior) = (hcQ * hcfbsg + sefbsg * hcQE) / (hcQ+hcQE)
            frfgr(ior) = (hcQ * hcfrfg + sefrfg * hcQE) / (hcQ+hcQE)
            
            hcQ = hcQ + qeinl(iein)
            iein = iein+1
            hcocsb = ocsb(ior)
            hcobsb = obsb(ior)
            hcCD1 = CD(1,ior)
            hcCD2 = CD(2,ior)
            hcCP1 = CP(1,ior)
            hcCP2 = CP(2,ior)
            hcCM = CM(ior)
            hcBAC = BAC(ior)
            hcfbsg = fbsgr(ior)
            hcfrfg = frfgr(ior)
         enddo
        
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            obsb(ior)  = obsb(ior+1)
            ocsb(ior)  = ocsb(ior+1)
            CD(1,ior)  = CD(1,ior+1)
            CD(2,ior)  = CD(2,ior+1)
            CP(1,ior)  = CP(1,ior+1)
            CP(2,ior)  = CP(2,ior+1)
            CM(ior)    = CM(ior+1)
            BAC(ior)   = BAC(ior+1)
            fbsgr(ior) = fbsgr(ior+1)
            frfgr(ior) = frfgr(ior+1)
         endif
      endif 
   enddo 

end subroutine organic_carbon_inflow_1d