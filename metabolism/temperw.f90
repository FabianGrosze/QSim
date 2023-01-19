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

!> Berechnung der Wassertemperatur
!!
!! @author Volker Kirchesch
subroutine temperw(RO,TEMPL,TEMPW,SCHWI,WGE,TIEFE,TFLIE,flag,elen,ior,anze,etemp,ewaerm,typ,qeinl,vabfl,    &
                   jiein,cloud,typw,iwied,uhrz,ilbuhn,nwaerm,fkm,nkzs,tempwz,dH2D,iorLa,iorLe,ieinLs,flae,  &
                   qeinlL,etempL,mstr,IDWe,ilang,dtemp,extk,itags,monats,Tsed,Wlage,hWS,                    &
                   htempw,htempz,WUEBKS,SPEWKSS,PSREFSS,extkS,iwsim,iform_VerdR,                            &
                   kontroll,jjj)
                   
   use allodim
                   
   implicit none

   ! TODO (frassl) 
   ! die folgenden Variablen von oben, werden nicht im Code verwendet:
   ! elen, iwied, nwaerm, ilang, monats
   !     
   !     Liste der von QSim übergebenen Parameter
   !     ----------------------------------------
  
   !     RO     : relative Luftfeuchte im Zeitschritt [%]
   !     TEMPL  : Lufttemperatur im Zeitschritt [°C]
   !     WGE    : die in der Höhe zWmess gemessene Windgeschwindigkeit [m/s]
   !     TEMPW  : mittlere Wassertemperatur im Querschnitt [°C]
   !     TIEFE  : querschnittgemittelte Wassertiefe [m]
   !     TFLIE  : Zeitschrittweite [d]
   !     FLAG   : Knoten-Kennung [-]
   !     ELEN   : Abstand zwischen den Knoten i und i+1 [-]
   !     IOR    : Laufvariable für die Knoten in einem Strang [-]
   !     IDWE   : Identifikationsnummer für die Wetterstationen [-]
   !     ANZE   : Anzahl der Knoten in einem Strang [-]
   !     ETEMP  : Einleiter-Temperatur [°C]
   !     EWAERM : Waermeeinleitung MJ/s (wird hier in Mcal/s umgerechnet "/4.2")
   !     TYP    : wird nicht mehr benutzt
   !     VABFL  : Abfluss im Vorfluter [m3/s]
   !     JIEIN  : Anzahl der Einleitungen am Knoten ior [-]
   !     CLOUD  : Bedeckungsgrad in achtel [-]
   !     TYPW   : Wolkentyp (0-6) [-]
   !     IWIED  : erster Zeitschritt iwied=0, dann iwied=1
   !     UHRZ   : Uhrzeit, wird nicht benutzt, Kontrollparameter
   !     ILBUHN : ilbuhn=1 Berechnung erfolgt für Buhnenfelder
   !     NWAERM : wird nicht mehr benutzt
   !     FKM    : Flusskilometer, wird nicht benutzt, Kontrollparameter
   !     NKZS   : Anzahl der Tiefenschichten an einem Querprofil [-]
   !     TEMPWZ : Temperatur in der Tiefenschicht nkz
   !   #mf TEMPWZ in [°C] ?
   !     NKZ    : Laufvariable über die Tiefenschichten (1..nkzs)
   !     DH2D   : Dicke der Tiefenschichten
   !   #mf DH2D Einheit = [m]?
   !     IORLA  : AnfangsKnoten der Linienquelle im Strangs [-]
   !     IORLE  : EndKnoten der Linienquelle im Strang [-]
   !     IEINLS : Anzahl der Linienquellen im Strang [-]
   !     FLAE   : Querschnittsfläche [m2]
   !     QEINLL : Einleitmenge der linienförmigen Zuflüsse [m3/m]
   !   #mf: Einheit überprüfen -> [m3/s]?
   !     ETEMPL : Temperatur der linienförmigen Zuflüsse [°C]
   !     MSTR   : Strangnummer [-]
   !     ILANG  : Schalter, wird hier nicht benutzt
   !     DTEMP  : Temperaturänderung in den einzelnen Tiefenschichten [°C/h]
   !     EXTK   : Lichtextinktionskoeffizient [1/m]
   !     ITAGS  : aktueller Berechnungstag, dient nur zu Kontrollzwecken [-]
   !     MONATS : aktueller Berechnungsmonat, dient nur zu Kontrollzwecken [-]
   !     TSED   : Sedimenttemperatur [°C]
   !     WLAGE  : Lage der Wetterstation, Höhe ü. NN [m]
   !     HWS    : Wasserspiegellage am Querprofil ior, Höhe ü. NN [m]
   !     HTEMPW : mittlere Wassertemperatur im Querschnitt, wird nicht benutzt
   !     HTEMPZ : vertikale Wassertemperatur am Tiefenpunkt z [°C]
   !     WUEBKS : Eingabewert für den Wärmeübergangskoeffizient zwischen Wasser und Sediment [KJ/(K*m2*h)]
   !     SPEWKSS: Eingabewert für die spezifische Wärmekapazität des Sediments [KJ/(Kg*K)]
   !     PSREFSS: Eingabewert für den Reflextionsanteils der Strahlung am Sediment [%/100]
   !   #mf d.h. PSREFSS hat Werte zwischen 0-1? Einheit hat mich verwirrt
   !     EXTKS  : Eingabewert für die Lichtextinktion [1/m]
   !     IWSIM  : Schalter für die Auswahl der zu simulierenden Parameter [-]
   !     IFORM_VERDR: Schalter für die Auswahl der Verdunstungsformeln [-]
   ! wichtige Parameter, die für die Berechnung benutzt werden
   !   #mf gibt es einen Grund für das "wichtige"? -> wenn nicht: weglassen
   !-----------------------------------------------------------
   !     EL     : Dampfdruck der Luft in mm Hg od. *1.3333 [mbar]
   !     EW     : Dampfdruck des Wassers in mm Hg od. *1.3333 [mbar]
   !   #mf: EL und EW werden im Code nicht verwendet -> weglassen?
   !     stbk   : Stefan-Boltzmann-Konstante [KJ/(m2*h*k**4)]
   !              (2.0411e-7)
   !     SCHWI  : Globalstrahlung [cal/(cm2*h)]
   !     A      : Ausstrahlung
   !   #mf Einheit A & G [kJ/(m2*h)] ? -> mf prüft
   !     G      : Gegenstrahlung
   !     HR     : Verdunstungshoehe [mm/d]
   !     VDW    : Verdunstungswaerme [Kcal/Kg]
   !     WV     : Waermestromdichte durch Verdunstung [cal/cm2/h]
   !     ROH2O  : Dichte des Wassers (1000.[Kg/m3])
   !     SDDW   : Saettigungsdampfdruck bei Wassertemperatur an der
   !              Wasseroberflaeche [hPa]
   !     SDTT   : Saettigungsdampfdruck bei Lufttemperatur [hPa]
   !     PDLTT  : Partialdampfdruck der Luft bei der Temperatur TT [hPa]
   !     speWKW : spezifische Wärmekapazität des Wassers [KJ/(Kg*K)]
   !     speWKS : spezifische Wärmekapazität des Sediments [KJ/(Kg*K)]
   !     rohS   : Dichte des Sediments [Kg/m3]
   !     WUEBK  : Wärmeübergangskoeffizient [KJ/(K*m2*h)]
   !     APARS  : Anteil PARS an der Globalstrahlung [-]
   
   
   ! Bei der Simulation eines Tracer-Durchgangs wird automatisch die Einleiterkonz.
   ! auf 0 gesetzt.

   integer                         :: ior, anze, mstr, nkz, iein, ieinL, j, ior_flag, ilbuhn, m, ihcQ
   integer                         :: ji, iwsim, itags, iwied, nwaerm, ilang, monats
   integer                         :: iform_VerdR
   real                            :: tflie, WUEBK, speWKS,PSREFS, hctemp
   real                            :: hctemp1, hcQ, hcWE, hcQE, deltTW, hcTE, rohE, tempmt, tempwt
   real                            :: btiefe, Uhrz, dH2D
   integer, dimension(100)         :: iorLa, iorLe, typ
   integer, dimension(azStrs)      :: ieinls
   integer, dimension(1000)        :: flag, jiein, nkzs
   integer, dimension(azStrs,1000) :: IDWe
   real, dimension(20)             :: RO, WGE, typw, cloud
   real, dimension(50)             :: D, Cpart, hctemp1z, xtempwz, xdtemp
   real, dimension(1000)           :: tempw, vabfl, fkm, flae, tiefe, elen, schwi, Templ, extk, Tsed
   real, dimension(100)            :: qeinlL, etempL, etemp, qeinl, ewaerm
   real, dimension(50,1000)        :: tempwz, hctemz, dtemp
   real, dimension(azStrs,1000)    :: Wlage, hWS, htempw, WUEBKS, SPEWKSS, PSREFSS, extkS
   real, dimension(azStrs,50,1000) :: htempz
   logical, intent(in)             :: kontroll  !< debugging
   integer, intent(in)             :: jjj       !< debugging
   character(1000)                 :: message
   
   real                            :: density_1d
   save hctemp1,hctemp1z
   
   ! Konstanten
   WUEBK   = 0.0
   speWKS  = 0.0
   PSREFS  = 0.0
   hctemp1 = 0.0
   hctemp  = 0.0
   hcQ     = 0.0
   hcQE    = 0.0
   deltTW  = 0.0
   hcWE    = 0.0
   hcTE    = 0.0
   rohE    = 0.0
   
   ! -------------------------------------------------------------------------
   ! diffuse sources
   ! -------------------------------------------------------------------------
   do ieinL = 1, ieinLs(mstr)
      if (qeinlL(ieinL)>=0.0 .and. etempL(ieinL) == -9.99) cycle
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior)cycle
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            if (qeinlL(ieinL) <= 0.0)qeinlL(ieinL) = 0.0
            
            do nkz = 1,nkzs(ior)  ! 2D
               if (flae(ior) > 0.0 .and. etempL(ieinL) > 0.0)tempwz(nkz,ior) = tempwz(nkz,ior)+((etempL(ieinL)-tempwz(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400. ! Ki
            enddo
            if (flae(ior) > 0.0 .and. etempL(ieinL) > 0.0)tempw(ior) = tempw(ior)+((etempL(ieinL)-tempw(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D            ! Ki
         endif
      enddo
   enddo
   
   
   ! -------------------------------------------------------------------------
   ! point sources
   ! -------------------------------------------------------------------------
   iein = 1
   do j = 1, anze+1
      ior_flag = 0
      ior = j
      btiefe = tiefe(ior)
      if (btiefe < 0.01)btiefe = 0.01
      
      if (WUEBKS(mstr,ior) > 0.0) then
         WUEBK = WUEBKS(mstr,ior)
      else 
         WUEBK = 350.
      endif
      
      if (SPEWKSS(mstr,ior) > 0.0) then
         SPEWKS = SPEWKSS(mstr,ior)
      else
         speWKS = 0.8
      endif
         
      if (PSREFSS(mstr,ior) > 0.0) then
         PSREFS = PSREFSS(mstr,ior)
      else
         PSREFS = 0.8
      endif
      
      if (extk(ior) <= 0.0 .and. extkS(mstr,ior) > 0.0)extk(ior) = extkS(mstr,ior)
      if (extk(ior) <= 0.0 .and. extkS(mstr,ior) <= 0.0)extk(ior) = 1.5  ! 0.17 reines Wasser; 0.13 Schwebstoffe; 0.094 Ki; 0.0145 Gr
      
      ! hctemp1 wird bei der Berechnung der Temperatur nach Wärmeeinleitung benutzt, falls sich
      ! die Temperatur an der Einleitstelle auch nach Oberstrom ausdehnt.
      if (vabfl(ior)>=0.0 .and. vabfl(ior+1) < 0.0) then
         hctemp1 = tempw(ior)
         do nkz = 1,nkzs(ior)
            hctemp1z(nkz) = tempwz(nkz,ior)
         enddo
      endif
      
      
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0.and.vabfl(ior+1) > 0.0) then  ! An der Einleitstelle verteilt sich die eingeleitete
         ! Temperatur ober- und unterstromig
         ior = ior+1
         ior_flag = 1
      endif
      
      if (ilbuhn == 1) then    ! Buhnen sind vorhanden, keine Einleitung in ein Buhnenfeld
         nkzs(ior) = 1
      else if (flag(ior) == 4 .and. ilbuhn == 0) then  ! Berücksichtigung der Einleitungen
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcQ = 1
         do nkz = 1,nkzs(ior)
            hctemz(nkz,ior) = tempwz(nkz,ior-m) ! hc.. Hilfsgrößen, Umbenennung der Variablen
         enddo
         hctemp = tempw(ior-m)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10 ! Q_ein gelöscht hat keine Bedeutung
         if (ihcQ == 1) then
            hctemp = hctemp1
            do nkz = 1, nkzs(ior)
               hctemz(nkz,ior) = hctemp1z(nkz)
            enddo
         endif
         
         do ji = 1,jiein(ior) ! Beginn Einleiterschleife
            hcWE = 0.0
            hcQE = max(0.0,qeinl(iein))
            if (iwsim == 5) ewaerm(iein) = -9999.9
            
            ! Waermeeinleitung
            if (ewaerm(iein) > -9999.9) then 
               
               hcWE = ewaerm(iein)/4.2
               if (hcQE == 0.0) then
                  ! 1D
                  if (hcQ > 1.e-10)tempw(ior) = hctemp+hcWE/hcQ
                  
                  ! 2D
                  do nkz = 1,nkzs(ior)
                     if (hcQ > 1.e-10)tempwz(nkz,ior) = hctemz(nkz,ior)+hcWE/hcQ
                  enddo
                  
               else  
                  ! Wärmeienleitung hat ein Q
                  deltTW = hcWE/(hcQ+hcQE)
                  tempw(ior) = hctemp+deltTW  ! 1D
                  hcTE = (tempw(ior)*(hcQ+hcQE)-hcQ*hctemp)/hcQE
                  rohE = density_1D(hcTE) ! Dichte im Wärmeeinleiter
                  call Dichte(hctemz,nkzs,ior,D) ! Dichte im Vorfluter
                  
                  call Einleiter_Misch(nkzs,ior,hctemz,Cpart,hcQ,hcQE,hcTE,rohE,D,dH2D)  ! Berechnung der vertikalen Einmischung
                  tempwz(1:nkzs(ior),ior) = Cpart(1:nkzs(ior))
               endif
            
            
            else    
               ! Temperatureinleitung
               hcTE = etemp(iein)
               if (iwsim == 5 .and. etemp(iein) < 0.0)hcTE = hctemp
               if (iwsim /= 5) then
                  if (hcTE < -9.8) then
                     hcTE = hctemp
                     do nkz = 1,nkzs(ior)
                        tempwz(nkz,ior) = hctemz(nkz,ior)
                     enddo
                  else
                     rohE = density_1D(hcTE) ! Dichte im Einleiter
                     call Dichte(hctemz,nkzs,ior,D) ! Dichte im Vorfluter
                     call Einleiter_Misch(nkzs,ior,hctemz,Cpart,hcQ,hcQE,hcTE,rohE,D,dH2D)  ! Berechnung der vertikalen Einmischung
                     tempwz(1:nkzs(ior),ior) = Cpart(1:nkzs(ior))
                  endif
               endif
               tempw(ior) = (hcQ*hctemp+hcTE*hcQE)/(hcQ+hcQE)   ! 1D
            endif
            hcQ = hcQ+hcQE
            iein = iein+1
            do nkz = 1,nkzs(ior)
               hctemz(nkz,ior) = tempwz(nkz,ior)
            enddo
            hctemp = tempw(ior)
            
         enddo  ! Ende Einleiterschleife
        
        if (ior_Flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            tempw(ior) = tempw(ior+1)
            do nkz = 1,nkzs(ior)
               tempwz(nkz,ior) = tempwz(nkz,ior+1)
            enddo
         endif
      endif   ! Ende Einleiter-flag
      
      if (ior > 1) then
         tempw(ior-1) = tempmt
         tempwz(1,ior-1) = tempwt
      endif
      
      
      ! konservative Substanzen (iwsim=5)
      if (iwsim == 5) then
         tempmt = tempw(ior)
         cycle
      endif
      
      xtempwz(1:nkzs(ior)) = tempwz(1:nkzs(ior),ior)
      xdtemp(1:nkzs(ior)) = 0.0
      
      call temperw_quer(nkzs(ior),typw(IDWe(mstr,ior)),schwi(ior),extk(ior),hWS(mstr,ior),templ(ior)        &
                        ,ro(IDWe(mstr,ior)),wge(IDWe(mstr,ior)),cloud(IDWe(mstr,ior)),Wlage(mstr,ior),dH2D  &
                        ,tflie,WUEBK,SPEWKS,PSREFS,xtempwz,tempwt,tempmt,tempw(ior),btiefe,Tsed(ior)        &
                        ,xdtemp,iform_VerdR,itags,uhrz,ior,kontroll ,jjj )
      dtemp(1:nkzs(ior),ior) = xdtemp(1:nkzs(ior))
      tempwz(1:nkzs(ior),ior) = xtempwz(1:nkzs(ior))
     
      ! Fehlermeldung
      if (isnan(tempmt)) then
         write(message,"(a,i0)") "subroutine temperw: Division by zero in stretch: ", mstr
         call qerror(message)
      endif
      
      
      extk(ior) = -1.
   enddo ! Ende Knotenschleife
   tempwz(1,anze+1) = tempwt
   tempw(anze+1) = tempmt
   extk(anze+1) = -1.
   return
end subroutine temperw


!> Berechnung der Dichte im 1-dimensionalen Fall
pure real function density_1d(tempw)
   implicit none
   real, intent(in)  :: tempw !< water temperature [°C]
   
   density_1d = 999.842594             &
             + 6.793952e-2 * tempw     &
             - 9.095290e-3 * tempw**2  &
             + 1.001685e-4 * tempw**3  &
             - 1.120083e-6 * tempw**4  &
             + 6.536332e-9 * tempw**5
   return
end function density_1d


subroutine temperw_quer(xnkzs, xtypw, xschwi, xextk, xhWS, xtempl, xro, xwge,  &
                        xcloud, xWlage, dH2D, tflie, WUEBK, SPEWKS, PSREFS,    &
                        xtempwz, tempwt, tempmt, xtempw, btiefe, xTsed, xdtemp,&
                        iform_VerdR, itags, uhrz, ior,                         &
                        kontroll, jjj)
   implicit none
   integer               :: nkz, iform_VerdR, itags, xnkzs, ior
   real                  :: dH2D, btiefe, tempmt, tflie, WUEBK, SPEWKS, PSREFS, tempwt, sdtemp, dtempS_mit, uhrz
   real                  :: xRO, xWGE, xtypw, xcloud, xWlage, xhWS
   real                  :: xtempw, xschwi, xTempl, xextk, xTsed, xdtemp_mit
   real, dimension(50)   :: xtempwz, xdtemp
   logical, intent(in)   :: kontroll  !< debugging
   integer, intent(in)   :: jjj       !< debugging
   tempwt = 0.0
   
   ! das Abarbeiten der einzelnen Schichten erfolgt von
   ! der Oberfläche zur Gewässersohle.(nkz=1: Oberflächenschicht; nkz=xnkzs: Sohlschicht)
   ! übergeben wird die Temperaturänderung dtemp in den einzelnen Schichten
   do nkz = 1,xnkzs
      call temperw_kern(nkz,xnkzs,xtypw,xschwi,xextk,xhWS,xtempl,xro,xwge,xcloud,xWlage,dH2D, xdtemp_mit                    &
                        ,tflie,WUEBK,SPEWKS,PSREFS,xtempwz(1),tempmt,xtempw,btiefe,xTsed,xdtemp(nkz),dtempS_mit,iform_VerdR &
                        ,kontroll,jjj)
   enddo
   if (xnkzs == 1) then
      tempmt = xtempw + xdtemp_mit   ! neu xdtemp(1)
      if (tempmt < 0.0)tempmt = 0.001
      tempwt = tempmt
   else                                 !!! das kann so nicht stimmen !!wy
      tempwt = xtempwz(1)
      sdtemp = 0.0
      do nkz = 1,xnkzs-1
         sdtemp = sdtemp + xdtemp(nkz)
      enddo
      tempmt = xtempw + sdtemp/(xnkzs-1) + dtempS_mit ! neu
      tempmt = xtempw + xdtemp_mit                    ! alt
      if (tempmt < 0.0)tempmt = 0.001
   endif
end subroutine temperw_quer
