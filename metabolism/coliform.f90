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

!> Berechnung der Konzentration E. Coli, faecal coliformer und coliformer 
!! Bakterien in Fliessgewässer
!! @author Volker Kirchesch
!! @date 15.08.2017
subroutine coliform(tiefe,rau,vmitt,vabfl,elen,flae,flag,tflie,schwi,ss,zooind,grote,chla,tempw,jiein,ecoli,    &
                    qeinl,colil,qeinll,anze,iorla,iorle,ieinls,ilbuhn,coli,doscf,extks,mstr,                    &
                    ratecde,etacde,ratecie,xnuece,ratecge,ratecse,                                              &
                    kontroll,jjj)
   
   use allodim
   
   logical                       :: kontroll    !< debugging
   integer                       :: jjj         !< debugging
   integer                       :: anze
   integer, dimension(100)       :: iorLa, iorLe
   integer, dimension(1000)      :: flag, jiein
   integer, dimension(azStrs)    :: ieinLs
   real                          :: IUV0, IUVH, mRepair, nueI
   real, dimension(13)           :: eta, aw, achl, as, ah
   real, dimension(100)          :: qeinl, ecoli, coliL, qeinlL
   real, dimension(1000)         :: tiefe, elen, flae, vabfl, ss, zooind, chla, tempw, coli, extk, schwi, DOSCF
   real, dimension(1000)         :: rau, vmitt, ausUV
   real, dimension(azStrs,1000)  :: extkS
   
   if (kontroll) then
      print*,'coliform start: jjj,meinrang,anze = ',          &
              jjj,meinrang,anze,'  coli(1),coli(2),colit = ', &
              coli(1),coli(2),colit
   endif
   iein = 1
   ! APARA    Anteil des PARS Strahlung an der Globalstrahlung
   ! WirkDC   Wirkdosis ab der die Coliformen absterben in J/m2/h
   ! VRATED   Lichtunabhängige, Temperaturabhängige Mortalitätsrate [1/d]
   ! VRATEI   Lichtabhängige Mortalitätsrate [1/d]
   
   APARA   = 0.45
   WirkDC  = 265.     ! Wh/m2
   VRATEI  = 3.       ! 1/h
   VRATEDE = 0.03     ! 1/h
   
   RateKI  = RateCIe  ! m2*MJ-1
   nueI    = xnueCe   ! -
   RateKD  = RateCde  ! 1/d
   etaEC   = etaCde   ! -
   RateCGz = RateCGe  ! 1/d
   RateCSd = RateCSe  ! 1/d
   
   ! Berücksichtigung der Linienquelle
   if (ilbuhn /= 1) then
      do ieinL = 1, ieinLs(mstr)
         if (qeinlL(ieinL)>=0.0 .and. coliL(ieinL) == -1.)cycle
         do ior = 1,anze+1
            if (iorLe(ieinL) < ior)cycle
            if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
               if (qeinlL(ieinL) <= 0.0)qeinlL(ieinL) = 0.0
               coli(ior) = coli(ior)+((coliL(ieinL)-coli(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
            endif
         enddo ! Ende Knotenschleife
      enddo   ! Ende Schleife Linienquellen
   endif
   
   
   do ior = 1,anze+1  ! Schleife über die Ortspunkte
      if (ilbuhn == 1) then
      else if (flag(ior) /= 4) then
      else ! Berücksichtigung der Einleitungen
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         hcColi = Coli(ior-m)     ! Umbenennen der benötigten Variablen
         hcDOSCF = DOSCF(ior-m)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            
            hcColiE = eColi(iein)
            if (hcColiE < 0.0)hcColiE = hcColi
            Coli(ior) = (hcQ*hcColi+hcQE*hcColiE)/(hcQ+hcQE)
            DOSCF(ior) = (hcQ*hcDOSCF)/(hcQ+hcQE)            ! Annahme: Einleitung mit DOSCF = 0
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hcColi = Coli(ior)
            hcDOSCF = DOSCF(ior)
         enddo ! Ende Einleitungsschleife
      endif ! Ende Einleitungs-flag
      
      if (ior > 1) then
         coli(ior-1) = colit
         DOSCF(ior-1) = DOSCFt
      endif
      if (extk(ior) <= 0.0 .and. extkS(mstr,ior) > 0.0)extk(ior) = extkS(mstr,ior)
      if (extk(ior) <= 0.0 .and. extkS(mstr,ior) <= 0.0)extk(ior) = 1.5  ! 0.17 reines Wasser; 0.13 Schwebstoffe; 0.094 Ki; 0.0145 Gr
      
      ! Berechnung der Schubspannungsgeschwindigkeit
      FN = 1./RAU(ior)
      G = 9.81
      UST = ((FN*G**0.5)/TIEFE(ior)**0.166667)*abs(VMITT(ior))
      
      ! Berechnung des mittleren vertikalen Dispersionskoeffizient
      ! nach Fischer im ein-dimensionalen Fall (gute Näherung)
      a = 0.4*ust
      xmuet = a*tiefe(ior)/6.
      PARS = max(0.0001,schwi(ior)*4.2*APARA)   ! J/cm2/h
      tlip = (-log(PARS))/(-extk(ior))
      if (tlip < 0.001)tlip = 0.001
      if (tlip > tiefe(ior))tlip = tiefe(ior)
      vLicht = 0.0
      vGes = 0.0
      if (xmuet > 0.0) then
         vLicht = tlip**2/xmuet
         vGes = tiefe(ior)**2/xmuet
      endif
      PARSW_J = PARS*(1./(tlip*extk(ior)))*(1.-exp(-extk(ior)*tlip))
      PARSW_MJ = PARSW_J * 0.01 * tflie * 24.       ! MJ*m-2
      DOSCFt = DOSCF(ior)
      if (vGes > 0.0) then
         DOSCFt = DOSCFt+PARSW_MJ*vLicht/vGes
      endif
      
      if (PARS <= 0.0001)DOSCFt = 0.0
      
      VRC = RateKD*etaEC**(tempw(ior)-20.)
      colit = coli(ior)*(1.-(1.-exp(-RateKI*DOSCFt))**nueI)*exp(-(VRC+RateCGz+RateCSd)*tflie)
      
      if (kontroll)print*,'COLIFORM RateKI,DOSCFt,nueI,VRC,RateCGz,RateCSd,tflie = ',  &
          RateKI,DOSCFt,nueI,VRC,RateCGz,RateCSd,tflie
      
      decoli = colit-coli(ior)
      if ((colit < 0.0) .and. ((coli(ior)+abs(decoli)) > 0.0)) colit = (coli(ior)/(coli(ior)+abs(decoli)))*coli(ior)
      extk(ior) = -1.
      
   enddo    ! Ende Scleife über die Ortspunkte
   
   coli(anze+1) = colit
   DOSCF(anze+1) = DOSCFt
   extk(anze+1) = -1.
 
   
   if (kontroll)print*,'COLIFORM end: coli(1),coli(2),colit = ',coli(1),coli(2),colit
   
end subroutine COLIFORM
