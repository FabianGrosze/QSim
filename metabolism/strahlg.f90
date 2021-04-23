!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualit�t
!
!   Copyright (C) 2020 Bundesanstalt f�r Gew�sserkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie k�nnen es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation ver�ffentlicht, weitergeben und/oder modifizieren. 
!   Die Ver�ffentlichung dieses Programms erfolgt in der Hoffnung, da� es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT F�R EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

      SUBROUTINE strahlg(glob,uhrz,sa,su,schwi,tflie,geol,tdj,geob,dk,cloud,schwia,imet,mstr,IDWe,itags,monats,VTYP   &                                           
                ,VALTBL,EDUFBL,VALTBR,EDUFBR,breite,anze,ifehl,ifhStr,it_h,ij,jahrs,itage,monate,jahre,uhren          &
                ,isim_end,azStr,azStrs)            
!                                                                       
!                                                                       
!                                                                       
!                                                                       
!                                                                       
!     EIN PROGRAMM ZUR BERECHNUNG DER MITTL. GLOBALSTRAHLUNG            
!     FUER DEN BERECHNUNGSSCHRITT IN CAL/(CM2*H) AUS DER TAGES-         
!     SUMME                                                             
!                                                                       
!                                                                       
!     AUTOR : VOLKER KIRCHESCH                                          
!                                                                       
!                                                                       
!     STAND: 06.11.1987                                                 
!                                                                       
!                                                                       
!                                                                       
!                                                                       
!     Parameter:                                                        
!                                                                       
!     SCHWI  - Globalstrahlung an der Wasseroberflaeche unter Berueck-  
!              sichtigung der Reflektion an der Wasseroberflaeche       
!              [cal/(cm2*h)]                                            
!                                                                       
!     SH     - Sonnenhoehe im Bogenmass                                 
!     SHGR   - Sonnenhoehe im Gradmass                                  
!                                                                       
!     CLOUD  - Bewoelkungsgrad                                          
!                                                                       
!                                                                       
      integer                                       :: tdj, anze, azStr, azStrs
      integer, Dimension(8)                         :: NRV
      integer, Dimension(azStrs,1000)               :: IDWe, it_h

      real                                          :: maxi,lt
      real, Dimension(4)                            :: ar, br
      real, Dimension(14)                           :: EVALT, EKRBRT, EDUFER, EVDICH, ESLEN, ESLENS
      real, Dimension(20)                           :: glob, cloud
      real, Dimension(1000)                         :: Schwi, Breite, VALTBL, EDUFBL, VALTBR, EDUFBR    
      real, Dimension(1000,14)                      :: VTYP
      real, Dimension(azStrs)                       :: SHtest

      save itags_tst, monats_tst, jahrs_tst, uhrz_tst
                                                                       
                                                                       
      ifehl = 0 
      ifhStr = 0 
      j = 0 
      PI = 22./7. 
      Uhrv = Uhrz 

      isim_end = 0
!                                                                       
!                                                                       
!....NrV1, NrV2 :	Winter                                                
!....NRV3, NrV4 : �bergang Fr�hjahr                                     
!....NRV5, NRV6 : Sommer                                                
!....NRV7, NRV8 : �bergang Herbst                                       
!                                                                       
      itagv = 16 
      monatv = 11 
      j = j+1 
      goto 777 
!                                                                       
  700 itagv = 15 
      monatv = 4 
      j = j+1 
      goto 777 
!                                                                       
  705 itagv = 16 
      monatv = 4 
      j = j+1 
      goto 777 
!                                                                       
  710 itagv = 15 
      monatv = 5 
      j = j+1 
      goto 777 
!                                                                       
  715 itagv = 16 
      monatv = 5 
      j = j+1 
      goto 777 
!                                                                       
  720 itagv = 15 
      monatv = 10 
      j = j+1 
      goto 777 
!                                                                       
  725 itagv = 16 
      monatv = 10 
      j = j+1 
      goto 777 
!                                                                       
  730 itagv = 15 
      monatv = 10 
      j = j+1 
      goto 777 
!                                                                       
  777 if(monatv.gt.2)goto 26 
      NRV(j) = ITAGV+31*(MONATV-1) 
      goto 27 
   26 NRV(j) = (ITAGV+31*(MONATV-1)                                     &
     &-INT(0.4*MONATV+2.3))                                             
   27 continue 
!                                                                       
      goto (700,705,710,715,720,725,730)j 
!                                                                       
      if(monats.gt.2)goto 28 
      NRS = ITAGS+31*(MONATS-1) 
      goto 29 
   28 NRS = (ITAGS+31*(MONATS-1)                                        &
     &-INT(0.4*MONATS+2.3))                                             
   29 continue 
!                                                                       
! Vegetationstypen und Zugeordnete Parameter                            
!                                                                       
!                                                                       
!--------------------------------------------                           
!..EVALT	-	Hoehe des Vegetationstyps in m                               
      EVALT(1) = 0.8 
      EVALT(2) = 3. 
      EVALT(3) = 8. 
      EVALT(4) = 15. 
      EVALT(5) = 18. 
      EVALT(7) = 0.8 
      EVALT(8) = 3. 
      EVALT(9) = 8. 
      EVALT(10) = 15. 
      EVALT(11) = 18. 
      EVALT(13) = 15. 
      EVALT(14) = 18. 
                                                                        
!...EKRBRT	-	Kronenbreite in m                                          
      EKRBRT(1) = 0.0 
      EKRBRT(2) = 1.0 
      EKRBRT(3) = 5.0 
      EKRBRT(4) = 12.0 
      EKRBRT(5) = 8.0 
      EKRBRT(6) = 0.0 
      EKRBRT(7) = 0.0 
      EKRBRT(8) = 1.0 
      EKRBRT(9) = 5.0 
      EKRBRT(10) = 12.0 
      EKRBRT(11) = 8.0 
      EKRBRT(12) = 0.0 
      EKRBRT(13) = 12.0 
      EKRBRT(14) = 8.0 
!--EDUFER	-	Uferabstand in m                                            
      EDUFER(1) = 0.0 
      EDUFER(2) = 1.0 
      EDUFER(3) = 2.0 
      EDUFER(4) = 4.0 
      EDUFER(5) = 3.0 
      EDUFER(7) = 0.0 
      EDUFER(8) = 1.0 
      EDUFER(9) = 2.0 
      EDUFER(10) = 4.0 
      EDUFER(11) = 3.0 
      EDUFER(13) = 4.0 
      EDUFER(14) = 3.0 
!                                                                       
      if(NRS.ge.NRV(3).and.NRS.le.NRV(4))goto 410 
      if(NRS.ge.NRV(7).and.NRS.le.NRV(8))goto 410 
      if(NRS.ge.NRV(5).and.NRS.le.NRV(6))goto 420 
!                                                                       
!...EVDICH	-	Dichte der Vegetationsart in %                             
!...Winter                                                              
      EVDICH(1) = 10. 
      EVDICH(2) = 20. 
      EVDICH(3) = 20. 
      EVDICH(4) = 25. 
      EVDICH(5) = 85. 
      EVDICH(6) = 100. 
      EVDICH(7) = 10. 
      EVDICH(8) = 20. 
      EVDICH(9) = 20. 
      EVDICH(10) = 25. 
      EVDICH(11) = 85. 
      EVDICH(12) = 100. 
      EVDICH(13) = 25. 
      EVDICH(14) = 85. 
      goto 450 
!...�bergang                                                            
  410 EVDICH(1) = 30. 
      EVDICH(2) = 35. 
      EVDICH(3) = 30. 
      EVDICH(4) = 45. 
      EVDICH(5) = 85. 
      EVDICH(6) = 100. 
      EVDICH(7) = 30. 
      EVDICH(8) = 35. 
      EVDICH(9) = 30. 
      EVDICH(10) = 45. 
      EVDICH(11) = 85. 
      EVDICH(12) = 100. 
      EVDICH(13) = 45. 
      EVDICH(14) = 85. 
      goto 450 
!....Sommer                                                             
  420 EVDICH(1) = 65. 
      EVDICH(2) = 57. 
      EVDICH(3) = 43. 
      EVDICH(4) = 93. 
      EVDICH(5) = 85. 
      EVDICH(6) = 100 
      EVDICH(7) = 65. 
      EVDICH(8) = 57. 
      EVDICH(9) = 43. 
      EVDICH(10) = 93. 
      EVDICH(11) = 85. 
      EVDICH(12) = 100 
      EVDICH(13) = 93. 
      EVDICH(14) = 85. 
!                                                                       
!--------------------------------------------                           
!                                                                       
  450 continue 
!                                                                       
!....falls Stundenwerte aus der Datei WETTER.TXT eingelesen wurden      
      do 25 ior = 1,anze+1 
                                                                       
!...Fehlermeldung                                                       
      if(IDWe(mstr,ior).le.0)then 
        ifehl = 12 
        ifhStr = mstr 
        goto 999 
      endif 
!                                                                       
!..Fehlerabfrage bei Beschattung hier !!                                        
!      if(VTYP(ior,4).gt.0.0.and.VTYP(ior,10).gt.0.0)then 
!         ifehl = 13 
!         ifhStr = mstr 
!         goto 999 
!      endif 
!                                                                       
!      if(VTYP(ior,5).gt.0.0.and.VTYP(ior,11).gt.0.0)then 
!          ifehl = 14 
!          ifhStr = mstr 
!          goto 999 
!      endif 
!                                                                       
      Vtest1 = VTYP(ior,1)+VTYP(ior,2)+VTYP(ior,4)+VTYP(ior,5)          &
     &+VTYP(ior,6)+VTYP(ior,13)+VTYP(ior,14)                            
      Vtest2 = VTYP(ior,7)+VTYP(ior,8)+VTYP(ior,9)+VTYP(ior,10)         &
     &+VTYP(ior,11)+VTYP(ior,12)+VTYP(ior,13)+VTYP(ior,14)              

      if(Vtest1.gt.100)then 
         ifehl = 15 
          ifhStr = mstr 
          goto 999 
      endif 
!                                                                       
      if(Vtest2.gt.100)then 
          ifehl = 16 
          ifhStr = mstr 
          goto 999 
      endif 
!                                                                       
!....Parameter zur Ber�cksichtigung der Strahlungsabschirmung durch Bebauung
      EVALT(6) = VALTBL(ior) 
      EVALT(12) = VALTBR(ior) 
      EDUFER(6) = EDUFBL(ior) 
      EDUFER(12) = EDUFBR(ior) 
                                                                       
      if(imet.eq.0)goto 333 
      schwi(ior) = glob(IDWe(mstr,ior))/4.2 
      goto 444 

  333 THELL = su-sa 
      time0 = -thell/2. 

      MAXI = 2.*glob(IDWe(mstr,ior))/(THELL*4.2) 
      IF(UHRZ.LE.SA)MAXI=0.0 
      IF(UHRZ.GE.SU)MAXI=0.0 
      TSSA = UHRZ-SA 
      time = time0+tssa 
      SCHWI(ior) = MAXI*0.5*(1.+cos(2.*pi*time/thell)) 
                                                                       
!    ************************************************************       
!    *Berechnung der Globalstrahlung unter Beruecksichtigung der*       
!    *Reflexion an der Wasseroberflaeche                        *       
!    ************************************************************       
!                                                                       
!   I.) Einfluss der Sonnenhoehe                                        
!                                                                       
!    - BERECHNUNG DES SONNENHOEHENWINKELS  <sh>                         
!                                                                       
!     sw - Stundenwinkel                                                
!                                                                       
  444 continue 
      dtsl = (1./15.)*(15.-geol) 
      lt = uhrz-dtsl 
      hconx = 0.9856*tdj-2.72 
      hconx = hconx*pi/180. 
      eqtime = (-7.66*sin(hconx)-9.87*sin(2.*hconx+0.4362+0.06685*sin   &
     &(hconx)))/60.                                                     
!                                                                       
      if(lt.lt.12.)epsi = 12. 
      if(lt.ge.12.)epsi = -12. 
      sw = (pi*(lt+epsi+eqtime))/12. 
!                                                                       
      SH = SIN(GEOB*pi/180.)*SIN(DK)+COS(GEOB*pi/180.)*COS(DK)*COS(SW) 
      SH = ASIN(SH) 
      IF(SH.LT.0)SH=0 
      shgr = sh*180./pi 
!                                                                       
!   II.) Einfluss der Bewoelkung                                        
!                                                                       
      ar(1) = 1.18 
      br(1) = -0.77 
!                                                                       
      ar(2) = 2.20 
      br(2) = -0.97 
!                                                                       
      ar(3) = 0.95 
      br(3) = -0.75 
!                                                                       
      ar(4) = 0.35 
      br(4) = -0.45 
!                                                                       
      if(cloud(IDWe(mstr,ior))>8.or.cloud(IDWe(mstr,ior))<0.)then 
           ifehl = 11 
            goto 999 
      endif 
                                                                        
      if(cloud(IDWe(mstr,ior)).lt.1.)icl = 1 
      if(cloud(IDWe(mstr,ior)).ge.1.and.                                 &
     &cloud(IDWe(mstr,ior)).lt.5.)icl = 2                              
      if(cloud(IDWe(mstr,ior)).ge.5.and.                                 &
     &cloud(IDWe(mstr,ior)).lt.7.)icl = 3                              
      if(cloud(IDWe(mstr,ior)).ge.7.)icl = 4 
!                                                                       
      if(shgr.le.1.)hconwr = ar(icl) 
      if(shgr.gt.1.)hconwr = ar(icl)*shgr**br(icl) 
      if(hconwr.gt.1.)hconwr = 1. 
!                                                                       
!.... Ber�cksichtigung der Beschattung durch Ufervegetation             
!....Abschattung der direkten Strahlung                                 
                                                                        
      EMDIR = 0.0 
      ETAS = Uhrz*180./12. 
      ETAS = ETAS*PI/180. 
!                                                                       
      Do 600 I = 1,14 

      if(VTYP(ior,I).le.0.0.or.SH.eq.0.0)goto 600 
!                                                                       
      if(shgr.gt.88.)ESLEN(i) = 0.0 
      if(shgr.le.88.)ESLEN(i) = EVALT(i)/tan(SH) 
!....Schattenlaenge                                                     
      ESLENS(i) = ESLEN(i)*abs(sin(ETAS)) 
!....Abzug der Uferbreite                                               
      ESLENS(i) = ESLENS(i)-EDUFER(i) 
      if(ESLENS(i).lt.0.0)ESLENS(i) = 0.0 
      if(ESLENS(i).gt.Breite(ior))ESLENS(i) = Breite(ior) 
!....Ber�cksichtigung der Kronenbreite                                  
      ESLENK = (EKRBRT(i)/2.)-EDUFER(i) 
      if(ESLENK.lt.0.0)ESLENK = 0.0 
      if(ESLENK.gt.breite(ior))ESLENK = breite(ior) 
      if(ESLENK.gt.ESLENS(i))ESLENS(i) = ESLENK 
!                                                                       
!...Abfrage ob Kronenschluss                                            
      if(i.eq.13.or.i.eq.14)then 
      hconK = EKRBRT(i)-2.*EDUFER(i) 
      if(hconK.gt.Breite(ior))then 
      ESLENS(i) = Breite(ior) 
      goto 630 
      endif 
      if(shgr.gt.88.)then 
      ESLENS(i) = 2.*((EKRBRT(i)/2.)-EDUFER(i)) 
      goto 630 
      endif 
      endif 
!                                                                       
!...Vereinfacht: Fliessrichtung Norden nach Sueden                      
!....linkes Ufer in Fliessrichtung                                      
!...SH<SHtest Sonne geht unter linke Ufervegetation wirft keinen Schatte
!...nur rechtes Ufer                                                    
      if(shgr.gt.88.)goto 630 
      if(shgr.le.88.and.i.le.6.and.SH.lt.SHtest(mstr))                  &
     &ESLENS(i) = 0.0                                                   
      if(shgr.le.88.and.i.gt.6.and.i.le.12.and.SH.gt.SHtest(mstr))      &
     &ESLENS(i) = 0.0                                                   
!                                                                       
!                                                                       
  630 HCON1 = ESLENS(i)/Breite(ior) 
      if(HCON1.gt.1.)HCON1 = 1. 
      EMDIR = EMDIR+(EVDICH(i)/100.)*((VTYP(ior,i)/100.)*HCON1) 
  600 continue 
!                                                                       
!                                                                       
!...Abschattung der Diffusen Strahlung                                  
      EMDIF = 0.0 
      if(EMDIR.eq.0.0)goto 850 
!                                                                       
!... Mittelwertbildung der Vegetationswerte                             
!                                                                       
      ECONL = 0.0 
      EVALTL = 0.0 
      EUFERL = 0.0 
      EKRBRL = 0.0 
      ECONR = 0.0 
      EVALTR = 0.0 
      EUFERR = 0.0 
      EKRBRR = 0.0 
                                                                        
      do 760 i = 1,6 
      if(VTYP(ior,i).lt.0.0)VTYP(ior,i) = 0.0 
      if(EVALT(i).lt.0.0)EVALT(i) = 0.0 
      if(EDUFER(i).lt.0.0)EDUFER(i) = 0.0 
      ECONL = ECONL+((VTYP(ior,i)/100.)*(EVDICH(i)/100.)) 
      EVALTL = EVALTL+((VTYP(ior,i)/100.)*EVALT(i)) 
      EUFERL = EUFERL+((VTYP(ior,i)/100.)*EDUFER(i)) 
      EKRBRL = EKRBRL+((VTYP(ior,i)/100.)*EKRBRT(i)) 
  760 continue 
      if(VTYP(ior,13).lt.0.0)VTYP(ior,13) = 0.0 
      if(VTYP(ior,14).lt.0.0)VTYP(ior,14) = 0.0 
      ECONL = ECONL+((VTYP(ior,13)/100.)*(EVDICH(13)/100.)) 
      ECONL = ECONL+((VTYP(ior,14)/100.)*(EVDICH(14)/100.)) 
      EVALTL = EVALTL+((VTYP(ior,13)/100.)*EVALT(13)) 
      EVALTL = EVALTL+((VTYP(ior,14)/100.)*EVALT(14)) 
      EUFERL = EUFERL+((VTYP(ior,13)/100.)*EDUFER(13)) 
      EUFERL = EUFERL+((VTYP(ior,14)/100.)*EDUFER(14)) 
      EKRBRL = EKRBRL+((VTYP(ior,13)/100.)*EKRBRT(13)) 
      EVALTL = EKRBRL+((VTYP(ior,14)/100.)*EKRBRT(14)) 
!                                                                       
!                                                                       
!                                                                       
      do 765 i = 7,12 
      if(VTYP(ior,i).lt.0.0)VTYP(ior,i) = 0.0 
      if(EVALT(i).lt.0.0)EVALT(i) = 0.0 
      if(EDUFER(i).lt.0.0)EDUFER(i) = 0.0 
      ECONR = ECONR+((VTYP(ior,i)/100.)*(EVDICH(i)/100.)) 
      EVALTR = EVALTR+((VTYP(ior,i)/100.)*EVALT(i)) 
      EUFERR = EUFERR+((VTYP(ior,i)/100.)*EDUFER(i)) 
      EKRBRR = EKRBRR+((VTYP(ior,i)/100.)*EKRBRT(i)) 
  765 continue 
      ECONR = ECONR+((VTYP(ior,13)/100.)*(EVDICH(13)/100.)) 
      ECONR = ECONR+((VTYP(ior,14)/100.)*(EVDICH(14)/100.)) 
      EVALTR = EVALTR+((VTYP(ior,13)/100.)*EVALT(13)) 
      EVALTR = EVALTR+((VTYP(ior,14)/100.)*EVALT(14)) 
      EUFERR = EUFERR+((VTYP(ior,13)/100.)*EDUFER(13)) 
      EUFERR = EUFERR+((VTYP(ior,14)/100.)*EDUFER(14)) 
      EKRBRR = EKRBRR+((VTYP(ior,13)/100.)*EKRBRT(13)) 
      EVALTR = EKRBRR+((VTYP(ior,14)/100.)*EKRBRT(14)) 
!                                                                       
!                                                                       
      hconKR = (EKRBRL/2.)-EUFERL 
      if(hconKR.lt.0.0)hconKR = 0.0 
      xl = EUFERL+hconKR 
!                                                                       
      hconKR = (EKRBRR/2.)-EUFERR 
      if(hconKR.lt.0.0)hconKR = 0.0 
      xr = EUFERL+Breite(ior)-hconKR 
!.... Kronenschluss                                                     
      if(xl.lt.xr)goto 800 
      EMDIF = EconL*0.5+EconR*0.5 
      goto 850 
!                                                                       
  800 Ephi = EUFERL+(Breite(ior)/2.) 
      EVL = EUFERL+Breite(ior)+EUFERR 
      Ealpha = atan(Ephi/EVALTL) 
      Ebeta = atan((EVL-Ephi)/EVALTR) 
      D = (EconL*Ealpha+EconR*Ebeta)/PI 
      s1 = xl-EUFERL 
      s2 = xr-xl 
      s3 = EUFERL+Breite(ior)-xr 
      EMDIF = (s1*ECONL+s2*D+s3*ECONR)/Breite(ior) 
!                                                                       
!                                                                       
  850 continue 
!                                                                       
      fdiff = 1./(11.*sin(SH)+1.) 
      if(cloud(IDWe(mstr,ior)).gt.7)fdiff = 1. 
      diffI = schwi(ior)*fdiff 
      dirI = schwi(ior)*(1.-fdiff) 
!                                                                       
      dirI = dirI*(1.-EMDIR) 
      diffI = diffI*(1.-EMDIF) 
!                                                                       
      schwia = schwi(ior) 
!                                                                       
      schwi(ior) = (dirI+diffI)*(1.-hconwr) 

      if(ior.lt.anze+1)Uhrz = uhrv 

! Anzahl der Zeitschritte w�hrend der Hellphase

      if(ij==1)it_h(mstr,ior) = 0
      if(Schwi(ior)>0.001)it_h(mstr,ior) = it_h(mstr,ior) + 1

!....Berechnung der neuen Uhrzeit und des neuen Datums                  


    if(azStr==1.and.ior==1)then
      itags_tst = itags
      Uhrz_tst = Uhrz+tflie*24. 
      if((24.-Uhrz_tst)<0.0001)Uhrz_tst = 24. 
      if(Uhrz_tst>=24.)then 
      Uhrz_tst = Uhrz_tst-24. 
      itags_tst = itags+1 
      endif

      monats_tst = monats 

      call anztag(monats_tst,jahrs,jtage) 
      if(itags_tst>jtage)then 
      itags_tst = 1 
      monats_tst = monats_tst+1 
      endif 

      jahrs_tst = jahrs
      if(monats_tst>12)then 
      monats_tst = 1. 
      jahrs_tst = jahrs+1 
      endif
    endif 
!....Ende Datumsberechnung

      if(itags_tst==itage.and.monats_tst==monate.and.jahrs_tst==jahre.and.uhrz_tst==uhren.and.it_h(mstr,ior)==0)then
         it_h(mstr,ior) = 1
         isim_end = 1
      endif                                             

   25 continue 
      SHtest(mstr) = SH 
!                                                                       
  999 RETURN 
      END                                           
