!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

  SUBROUTINE oxygen(VO2,TEMPW,RAU,VMITT,TIEFE,rhyd,FLAE,TFLIE,go2n,dalgki,dalggr,dalgak,dalgag,akinh4              &
                    ,agrnh4,akino3,agrno3,bsbt,hJO2,flag,elen,ior,anze,dzres1,dzres2,hschlr                        &
                    ,eo2,qeinl,vabfl,po2p,po2r,so2ein,dO2o2D,salgo,dalgo,dalgao,o2ein1,jiein                       &
                    ,opgrmi,opgrma,opkimi,opkima,albewg,alberg,abeowg,abeorg,opblmi,opblma,ablnh4                  &
                    ,ablno3,dalgbl,dalgab,albewk,alberk,abeowk,abeork,ro2dr,wge,IDWe,fkm,uhrz                      &
                    ,zooro2,rO2HNF,ilbuhn,iwied,vo2z,susO2N,nkzs,dH2D,o2L,qeinlL                                   &
                    ,iorLa,iorLe,ieinLs,agnh4z,aknh4z,abnh4z,dalgkz,dalgbz,dalggz,agno3z,akno3z                    &
                    ,abno3z,algakz,algagz,algabz,vz1,tempwz,saett,mstr,cpfad,ij,itags,monats                       &
                    ,dC_DenW,TOC_CSB,WLage,hWS,etemp,dH2De,ifehl,ifhStr,azStrs,zooind,GROTe,iphy                   &
                    ,kontroll ,jjj ) !!wy  					! chlagr  unbenutzt                                       
                                                                       
!##############################################################
!PROGRAMM ZUR BERECHNUNG DES SAUERSTOFFGEHALTS IN FLIEßGEWÄSSER
!##############################################################                                                                       
                                                                       

!###################################                                                                       
!     AUTOR : VOLKER KIRCHESCH                                          

!     STAND : 17.04.2018                                                
!###################################                                                                       
                                                                       
      logical kontroll !!wy
      integer jjj !!wy
      character (len=255)              :: cpfad
 
      integer                          :: anze, azStrs
      integer, Dimension(azStrs)       :: ieinLs
      integer, Dimension(100)          :: iorLa, iorLe 
      integer, Dimension(1000)         :: flag, jiein, nkzs
      integer, Dimension(azStrs,1000)  :: IDWe 

      real, Dimension(20)              :: wge
      real, Dimension(50)              :: vo2zt, seo2, hco2Ez, Cpart, D
      real, Dimension(100)             :: eo2, etemp, qeinl, o2L, qeinlL
      real, Dimension(1000)            :: vo2, tempw, go2n, dalgki, dalggr, dalgak, dalgag, albewg, bsbt, dzres1,dzres2 
      real, Dimension(1000)            :: alberg, rau, tiefe, rhyd, vmitt, elen, po2p, po2r, so2ein, bbei, salgo, flae, saett
      real, Dimension(1000)            :: dalgo, dalgao, o2ein1, akinh4, agrnh4, akino3, agrno3, ablnh4, ablno3, dalgbl
      real, Dimension(1000)            :: dalgab, vabfl, dO2o2D, dH2De, abeowg, abeorg, abeowk, abeork, dC_DenW, zooind
      real, Dimension(1000)            :: albewk, alberk, ro2dr, fkm, zooro2, rO2HNF, susO2N  

      real, Dimension(50,1000)         :: vo2z, vo2z1, agnh4z, aknh4z, abnh4z, dalgkz, dalgbz, dalggz, tempwz, hcvo2z
      real, Dimension(50,1000)         :: dalgoz, vz1, agno3z, akno3z, abno3z, algakz, algagz, algabz, algaoz
      real, Dimension(azStrs,1000)     :: hJO2, WLage, hWS, hschlr 

                                                                       
!     open(unit=65,file='o2.tst')                                      
                                                                       
!     iphy = 1	! neue Formel von mir mit Wind
!     iphy = 2	! neue Formel von mir ohne Wind
!     iphy = 3	! Formel von Wolf
!     iphy = 4	! Formel von Melching
                                                                       
      iein = 1

!###### Umrechnung von RQ [mol C/mol O2] und PQ [mol O2/mol C] in mg O2/mg Bio ######

      Caki = 0.48
      Cagr = 0.48
      Cabl = 0.48

      opkimix = ((1./opkimi) * 32./12.) * Caki
      opkimax  = (opkima * 32./12.) * Caki
      opgrmix = ((1./opgrmi) * 32./12.) * Cagr
      opgrmax  = (opgrma * 32./12.) * Cagr
      opblmix = ((1./opblmi) * 32./12.) * Cabl
      opblmax  = (opblma * 32./12.) * Cabl

!....Berücksichtigung der Linienquelle
    
      do ieinL = 1, ieinLs(mstr)
        if(qeinlL(ieinL)>=0.0.and.o2L(ieinL)==-1.)cycle
        do ior = 1,anze+1
          if(iorLe(ieinL)<ior)cycle
          if(iorLa(ieinL)<=ior.and.iorLe(ieinL)>=ior)then
            if(qeinlL(ieinL)<=0.0)qeinlL(ieinL) = 0.0
               do nkz = 1,nkzs(ior)  ! 2D
                 vo2z(nkz,ior) = vo2z(nkz,ior)+((o2L(ieinL)-vo2z(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
               enddo
               vo2(ior) = vo2(ior)+((o2L(ieinL)-vo2(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
                 else
          endif
       enddo ! Ende Knotenschleife 
     enddo   ! Ende Schleife Linienquellen


    do j=1,anze+1  !Beginn longitudinale Knotenschleife
  
      ior = j 
                                                                       
      SAETT(ior) = 14.603-TEMPWz(1,ior)*0.40215+(TEMPWz(1,ior)**2)      &
     &*0.007687-(tempwz(1,ior)**3)*0.0000693                            
                                                                   
      ior_flag = 0
      if(flag(ior)==6.and.vabfl(ior)<0.0.and.vabfl(ior+1)>0.0)then
        ior = ior+1
        ior_flag = 1
      endif

      if(ilbuhn==1)then
        nkzs(ior) = 1
          else if(flag(ior)/=4)then
            else                        ! Berücksichtigung der Einleitungen
              m = 1
              ihcQ = 0
              if(vabfl(ior-1)<0.0.and.vabfl(ior)<0.0)m = -1
              if(vabfl(ior-1)<0.0.and.vabfl(ior)>0.0)ihcQ = 1 ! Konzentration an der Einleitstelle 
                                                              ! ist gleich der Konzentration der Einleitung 

              
              hcvO2 = vo2(ior-m)     ! Umbenennen der benötigten Variablen; 1D
              hcQ = vabfl(ior-m)
              if(hcQ<0.0)hcQ = abs(hcQ)
              if(hcQ==0.0.or.ihcQ==1)hcQ = 1.e-10
              
              do nkz=1,nkzs(ior) 
                hcvo2z(nkz,ior) = vo2z(nkz,ior-m)
              enddo 

              do ji=1,jiein(ior)   ! Beginn Einleitungsschleife  
              hcQE = max(0.0,qeinl(iein))

              hco2E = eo2(iein)
              if(hco2E<0.0)then            ! 1D
                hco2E = hcvo2

                do nkz = 1, nkzs(ior)      ! 2D
                  hco2Ez(nkz) = hcvo2z(nkz,ior)
                enddo
                  else
                    do nkz = 1,nkzs(ior)
                      hco2Ez(nkz) = hco2E   
                    enddo
              endif
              
              if(etemp(iein)>-9.9)then
                 hcTE = etemp(iein)
                 rohE = Dichte_1D(hcTE)                                                 ! Dichte im Einleiter
                 call Dichte(tempwz,nkzs,D,ior,itags,uhrz,fkm)                          ! Dichte im Vorfluter
                 call Einleiter_Misch(nkzs,ior,hcvo2z,Cpart,hcQ,hcQE,hco2E,rohE,D,dH2D)  ! Berechnung der vertikalen Einmischung
                 vo2z(1:nkzs(ior),ior) = Cpart(1:nkzs(ior))
                   else
                      do nkz = 1,nkzs(ior)      ! 2D 
                        vo2z(nkz,ior) = (hcQ*hcvo2z(nkz,ior)+hcQE*hco2Ez(nkz))/(hcQ+hcQE)                                                     
                      enddo 
              endif
             
             vo2vor = vo2(ior)
             vo2(ior) = (hcQ*hcvo2+hcQE*hco2E)/(hcQ+hcQE) 
             hcQ = hcQ+qeinl(iein) 
             iein = iein+1 

             hcvo2 = vo2(ior) 
                                                                         
             do nkz = 1,nkzs(ior) 
               hcvo2z(nkz,ior) = vo2z(nkz,ior)
             enddo 
           enddo                        ! Ende Einleitungsschleife

           if(ior_flag==1)then
              iein = iein - jiein(ior)
             ior = ior-1
             vo2(ior) = vo2(ior+1)
             
             do nkz = 1,nkzs(ior)
               vo2z(nkz,ior) = vo2z(nkz,ior+1)
             enddo
           endif 
    endif                               ! Ende Einleitungs-flag                                                                  
                                                                       
      if(ior>1)then 
        vo2z(1,ior-1) = vo2zt(1) 
        vo2(ior-1) = vo2t 
      endif 
                                                                       
!     Gruenalgen!                                                       
                                                                       
      if(agrnh4(ior)==0.0)agrnh4(ior) = 0.00001 
      falgog = agrno3(ior)/agrnh4(ior) 
      falgog = (opgrmix+falgog*opgrmax)/(1.+falgog) 

!     Benthische Gruenalgen                                             
                                                                       
      abeowg(ior) = albewg(ior)*falgog 
                                                                       
!     kieselalgen!                                                      
                                                                       
      if(akinh4(ior)==0.0)akinh4(ior) = 0.00001 
      falgok = akino3(ior)/akinh4(ior) 
      falgok = (opkimix+falgok*opkimax)/(1.+falgok) 
                                                                       
!     Benthische Kieselalgen                                            
                                                                       
      abeowk(ior) = albewk(ior)*falgok 
                                                                       
!     Blaualgen!                                                        
                                                                       
      if(ablnh4(ior)==0.0)ablnh4(ior) = 0.00001 
      falgob = ablno3(ior)/ablnh4(ior) 
      falgob = (opblmix+falgob*opblmax)/(1.+falgob) 

      dalgo(ior) = dalggr(ior)*falgog+dalgki(ior)*falgok+dalgbl(ior)*falgob 

!     Respiration der Gruen-, Kiesel-, und Blaualgen                    
                                                                       
      dalgao(ior) = dalgag(ior)*opgrmix+dalgak(ior)*opkimix+dalgab(ior)*opblmix                                               

      if(nkzs(ior)>1)then     ! 2D-Modellierung, Respiration 
        do nkz = 1,nkzs(ior) 
          algaoz(nkz,ior) = algagz(nkz,ior)*opgrmix+algakz(nkz,ior)*opkimix+algabz(nkz,ior)*opblmix
        enddo
      endif                                    
                                                                       
!     Respiration der benthischen Gruenalgen und Kieselalgen            
                                                                       
      abeorg(ior) = alberg(ior)*opgrmix 
      abeork(ior) = alberk(ior)*opkimix 
      
      abeor = abeorg(ior) + abeork(ior)
      !!wy BUG ## abeow = abeowg(ior) + abeowg(ior) 
      abeow = abeowg(ior) + abeowk(ior) 
                                                                       
      if(nkzs(ior)>1)then 
        do nkz = 1,nkzs(ior) ! O2-Produktion der Algen bei 2D-Modellierung 
                                                                       
          if(agnh4z(nkz,ior)==0.0)agnh4z(nkz,ior) = 0.00001   ! Gruenalgen 
          falgog = agno3z(nkz,ior)/agnh4z(nkz,ior) 
          falgog = (opgrmix+falgog*opgrmax)/(1.+falgog) 
                                                                       
          if(aknh4z(nkz,ior)==0.0)aknh4z(nkz,ior) = 0.00001    ! Kieselalgen 
          falgok = akno3z(nkz,ior)/aknh4z(nkz,ior) 
          falgok = (opkimix+falgok*opkimax)/(1.+falgok) 

          if(abnh4z(nkz,ior)==0.0)abnh4z(nkz,ior) = 0.00001   !Blaualgen 
          falgob = abno3z(nkz,ior)/abnh4z(nkz,ior) 
          falgob = (opblmix+falgob*opblmax)/(1.+falgob) 

          dalgoz(nkz,ior) = dalggz(nkz,ior)*falgog+dalgkz(nkz,ior)*falgok+dalgbz(nkz,ior)*falgob 
        enddo                                                                       
      endif                                                                  
                                                                       
                                                                       
!     Sauerstoffverbrauch durch Zooplanktonrespiration                  
                                                                       
      ft = 1.047**(tempw(ior)-20.) 

      vo2Rot = 13.08*GRote**0.716   ! ngO2/Ind/h (Galkovskaya 1995)
      vo2Rot = Vo2Rot*24.*1.e-6     ! mgO2/Ind/d 
      vo2Rot = vO2Rot*zooind(ior)
      vo2Rot = vo2Rot*ft*tflie
      zooro2(ior) = vo2Rot     ! Ausgabewert
                                                                       
      hSchlr(mstr,ior) = hJO2(mstr,ior)*tflie/tiefe(ior) 
                                                                       
      v = dalgo(ior)-dalgao(ior)-go2n(ior)-(bsbt(ior)-dC_DenW(ior)*TOC_CSB)-vo2Rot           &
         -hschlr(mstr,ior)+po2p(ior)-po2r(ior)+abeow-abeor-ro2dr(ior)-rO2HNF(ior)                   
                                                                      
!....Ausgabe in mgO2/(l*h)                                              
      hSchlr(mstr,ior) = hschlr(mstr,ior)/(tflie*24.) 
!                                                                       
      if(nkzs(ior)>1)then        ! 2D 
        do nkz = 1,nkzs(ior) 
          vz1(nkz,ior) = -go2n(ior)-(bsbt(ior)-dC_DenW(ior)*TOC_CSB)+dalgoz(nkz,ior)-algaoz(nkz,ior)-vo2Rot+po2p(ior)  &
                         -po2r(ior)+abeow-abeor-ro2dr(ior)-rO2HNF(ior)                                                      
          vz1(nkz,ior) = vz1(nkz,ior)/(tflie*24.) 
        enddo 
     endif

     if((rau(ior).gt. 0.0).and.(tiefe(ior).gt. 0.0).and.(rhyd(ior).gt. 0.0))then
        !call Belueftung_K2(rau,tiefe,vmitt,rhyd,flae,tempw,WLage,hws,wge,IDWe,iphy,bbei,mstr,ior,azStrs)
		call Belueftung_K2(rau(ior),tiefe(ior),vmitt(ior),rhyd(ior),flae(ior),tempw(ior)   &
     &                    ,WLage(mstr,ior),hWS(mstr,ior),wge(IDWe(mstr,ior)),iphy,bbei(ior))
     else
        bbei(ior)=0.0
     endif

                                                                       
      DEFIZ = SAETT(ior)-VO2z(1,ior) 
      DEFIZ_1D = SAETT(ior)-VO2(ior)
      if(nkzs(ior)>1)DEFIZ_1D = DEFIZ 

      o2ein = saett(ior)*(1-exp(-bbei(ior)*tflie)) 
      o2ein1(ior) = DEFIZ_1D*(1-exp(-bbei(ior)*tflie)) 
      if(kontroll)then
        print*,'oxygen oberflächenbelüftung, o2ein1,DEFIZ_1D,bbei,tflie,tiefe,vmitt,Rau,rhyd=' &
     &        ,o2ein1(ior),DEFIZ_1D,bbei(ior),tflie,tiefe(ior),vmitt(ior),Rau(ior),rhyd(ior)
      endif
    
      if(ior>1)then
        so2ein(ior) = so2ein(ior-1)+o2ein 
        salgo(ior) = salgo(ior-1)+dalgo(ior)-dalgao(ior) 
          else
            so2ein(ior) = o2ein 
            salgo(ior) = dalgo(ior)-dalgao(ior) 
      endif

                                                                
      BBEI(ior) = BBEI(ior)*TFLIE
                                                                             
      DELO2 = (V+BBEI(ior)*DEFIZ_1D)/(1.+BBEI(ior)*0.5) 
      vo2t = vo2(ior)+delo2 
      if(vo2t.lt.0.01)vo2t = 0.01 
                                                                       
      if(nkzs(ior)==1)then 
        vo2zt(1) = vo2z(1,ior)+delo2 
        if(vo2zt(1)<0.01)vo2zt(1) = 0.01
          else
            vo2zt(1) = vo2z(1,ior)
            dO2o2D(ior) = (vz1(1,ior)+BBEI(ior)*DEFIZ)/(1.+BBEI(ior)*0.5)
            dO2o2D(ior) = (dO2o2D(ior)-vz1(1,ior))*tiefe(ior)/(dH2D*0.5)
            dO2o2D(ior) = dO2o2D(ior)/(tflie*24.) 
      endif 
                                                                       
!...Fehlermeldung                                                       
      ifehl = 0
      if(ISNAN(vo2t))then 
        ifehl = 23 
        ifhStr = mstr 
        exit 
      endif 

    enddo   ! Ende longitudinale Knotenschleife 
                                                                       
      vo2(anze+1) = vo2t 

      if(nkzs(anze+1)==1)then
        vo2z(1,anze+1) = vo2zt(1)
      endif

  END subroutine oxygen                                          

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     Unterprogramm zur Berechnung der Dichte im 1-dimensionalen Fall
!
!      real function Dichte_1D(hcTE)
!      
!
!
!      a0 = 999.842594
!      a1 = 6.793952e-2
!      a2 = -9.095290e-3
!      a3 = 1.001685e-4
!      a4 = -1.120083e-6
!      a5 = 6.536332e-9
!
!      Dichte_1D = a0+a1*hcTE+a2*hcTE**2+a3*hcTE**3+a4*hcTE**4+a5*hcTE**5
! 
!      return
!      end

