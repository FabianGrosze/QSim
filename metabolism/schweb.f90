!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualit‰t
!
!   Copyright (C) 2020 Bundesanstalt f¸r Gew‰sserkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie kˆnnen es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation verˆffentlicht, weitergeben und/oder modifizieren. 
!   Die Verˆffentlichung dieses Programms erfolgt in der Hoffnung, daﬂ es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT F‹R EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

      SUBROUTINE SCHWEB(zooind,dorgSS,ss,ssalg,tiefe,rau,tflie,VMITT,flae,flag,elen,ior,anze,ess      &
                ,ssL,qeinl,qeinlL,vabfl,dkimor,dgrmor,abszo,zexki,zexgr,iorLa,iorLe,ieinls            &
                ,abl,zexbl,dblmor,drfaeb,jiein,aki,agr,ssdr,drfaek,drfaeg,drfaes,fssgr,sedss,sedSS_MQ & 
                ,fssgrs,tauscs,ischif,ilbuhn,fkm,ieros,iwied,echla,vkigr,akbcm,agbcm,antbl,abbcm      &
                ,ezind,GROTe,mstr,itags,monats,uhrz,azStrs                                            &                                                   
                ,kontroll ,jjj ) !!wy  
                                                                       
                                                                       
!     UNTERPROGRAMM ZUR BERECHNUNG DER SCHWEBSTOFF-                     
!     KONZENTRATION IN FLIESSGEWAESSERN                                 
                                                                       
                                                                       
!     AUTOR: VOLKER KIRCHESCH                                           
                                                                       
!     STAND: 22.09.2011                                                
                                                                       
                                                                       
                                                                       
!     SS   ORG. UND ANORG. SCHWEBSTOFFE(OHNE ALGEN UND ZOOPLANKTER      
!     SSALG   GESAMTSCHWEBSTOFFE                                        
                                                                       

      logical kontroll !!wy
      integer jjj !!wy
      integer                        :: anze, azStrs
      integer, Dimension(azStrs)     :: ieinLs
      integer, Dimension(100)        :: iorLa, iorLe 
      integer, Dimension(1000)       :: flag, jiein, ischif

      real, Dimension(100)           :: ess, echla, ezind, qeinl, ssL, qeinlL 
      real, Dimension(1000)          :: zooind, ss, vabfl, ssalg, tiefe, rau, vmitt, flae, elen, zexki, zexgr, zexbl 
      real, Dimension(1000)          :: fkm, dblmor ,drfaeb, abl, aki, agr, dkimor, dgrmor, abszo, dorgSS 
      real, Dimension(1000)          :: ssdr, drfaek, drfaeg, drfaes, fssgr, sedss, vkigr, antbl, akbcm, agbcm, abbcm
      real, Dimension(azStrs,1000)   :: sedSS_MQ

!      open(unit=113,file='ss.tst')                                     
                                                                       
      iein = 1 
                                                                       
      Cagr = 0.48 
      Caki = 0.48 
      Cabl = 0.48 
      GRot = GRote 
                                                                       
!....Ber¸cksichtigung der Linienquelle 
    
      do ieinL = 1, ieinLs(mstr)
        if(qeinlL(ieinL)>=0.0.and.ssL(ieinL)==-1.)cycle 
        do ior = 1,anze+1
          if(iorLe(ieinL)<ior)cycle
          if(iorLa(ieinL)<=ior.and.iorLe(ieinL)>=ior)then
            if(qeinlL(ieinL)<=0.0)ssL(ieinL) = 0.0
              ss(ior) = ss(ior)+((ssL(ieinL)-ss(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
                else
          endif
       enddo ! Ende Knotenschleife 
     enddo   ! Ende Schleife Linienquellen


      do j=1,anze+1                    ! Beginn Knotenschleife 

      ior = j

      ior_flag = 0
      if(flag(ior)==6.and.vabfl(ior)<0.0.and.vabfl(ior+1)>0.0)then
        ior = ior+1
        ior_flag = 1
      endif

      if(ilbuhn==1)then
          else if(flag(ior)/=4)then
            else                        ! Ber¸cksichtigung der Einleitungen
              m = 1
              ihcQ = 0
              if(vabfl(ior-1)<0.0.and.vabfl(ior)<0.0)m = -1
              if(vabfl(ior-1)<0.0.and.vabfl(ior)>0.0)ihcQ = 1 ! Konzentration an der Einleitstelle 
                                                              ! ist gleich der Konzentration der Einleitung 

              
              hcss = ss(ior-m)       ! Umbenennen der benˆtigten Variablen
              hcfssg = fssgr(ior-m) 

              hcQ = vabfl(ior-m)
              if(hcQ<0.0)hcQ = abs(hcQ)
              if(hcQ==0.0.or.ihcQ==1)hcQ = 1.e-10
              
              do ji=1,jiein(ior)   ! Beginn Einleitungsschleife  
              hcQE = max(0.0,qeinl(iein))

              hcssE = ess(iein)

              if(hcssE<0.0)then
                hcssE = hcss
                  else
                    if(echla(iein)>=0.0)then
                      akie = (echla(iein)*vkigr(ior-1)/1000.)*(akbcm(ior-1)/Caki) 
                      agre = (echla(iein)*(1.-vkigr(ior-1)-antbl(ior-1))/1000.)*(agbcm(ior-1)/Cagr)                                        
                      able = (echla(iein)*antbl(ior-1)/1000.)*(abbcm(ior-1)/Cabl) 
                      hcssE = hcssE-akie-agre-able
                    endif
                    if(ezind(iein)>=0.0)hcssE = hcssE-(ezind(iein)*GROT/1000.) 
              endif

              hcfssgE = fssgrs 

              ss(ior) = (hcQ*hcss+hcQE*hcssE)/(hcQ+hcQE) 
              fssgr(ior) = (hcQ*hcfssg+hcQE*hcfssgE)/(hcQ+hcQE) 
                                                                      
              hcQ = hcQ+qeinl(iein) 
              iein = iein+1 
              hcss = ss(ior) 
              hcfssg = fssgr(ior) 
           enddo                        ! Ende Einleitungsschleife
          
           if(ior_flag==1)then
             iein = iein - jiein(ior)
             ior = ior-1
             ss(ior) = ss(ior+1)
             fssgr(ior) = fssgr(ior+1)
           endif
    endif                               ! Ende Einleitungs-flag                                                                  
                                                                       
      if(ior.gt.1)then 
      ss(ior-1) = sst 
      fssgr(ior-1) = fssgrt 
      endif 
      fssgrv = fssgr(ior) 
                                                                       
      g = sqrt(9.81) 
      ust = (((1./rau(ior))*g)/(tiefe(ior)**0.16667))*abs(vmitt(ior)) 
                                                                       
      ustkri = sqrt(tauscs/1000.) 
      vkrit = (ustkri*tiefe(ior)**0.166667)/((1./rau(ior))*g) 
                                                                       
      tiefe1 = tiefe(ior) 

      if(ischif(ior)==0)then 
      v6 = 0.0 
        else 
          nschif = ischif(ior) 
          vmitt1 = vmitt(ior) 
          call schiff(vmitt1,tiefe1,v6,nschif) 
      endif
                                                                       
      vges = vmitt(ior)+v6 
                                                                       
                                                                       
      SSSED = fssgr(ior)*SS(ior) 

      ised = 3
      jsed = 1
      ZellV = 0.0

      call Sedimentation(ior,tiefe,ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,kontroll,jjj)

      ceq = sssed*qsgr 

      sedss(ior) = max(0.0,(sssed-ceq)) * oc

      sedSS_MQ(mstr,ior) = sedss(ior)
                                                                       
!      Einfluss der Schifffahrt                                         
                                                                       
      if(ieros.eq.1.and.vges.gt.vkrit)sedss(ior) = 0.0 
                                                                       
      exzo = zexki(ior)+zexgr(ior)+zexbl(ior) 
                                                                       
      zomor = abszo(ior) 
                                                                       
!...Schwebstoffverluste durch Dreissena werden nicht berÅcksichtigt     
      ssdr(ior) = 0.0 
      SSt = SS(ior)-sedss(ior)+exzo+dkimor(ior)+dgrmor(ior)+dblmor(ior) &
     &+zomor-ssdr(ior)+dorgSS(ior)+drfaek(ior)+drfaeg(ior)+drfaeb(ior)  &
     &+drfaes(ior)                                                      

      SSt = SS(ior)+(SSt-SS(ior))
                                                                      
!     Neuberechnung des Faktors zur Berechnung der ablagerungsfreien    
!     Grenzkonzentration                                                
                                                                       
      hc1 = SS(ior)-sedss(ior)+exzo+dkimor(ior) 
      hc1 = hc1+dgrmor(ior)+dblmor(ior)+zomor-ssdr(ior) 
      hc1 = hc1+dorgSS(ior)+drfaek(ior)+drfaeg(ior) 
      hc1 = hc1+drfaeb(ior)+drfaes(ior) 
                                                                       
      hc2 = sssed-sedss(ior)+exzo+dkimor(ior) 
      hc2 = hc2+dgrmor(ior)+dblmor(ior)+zomor-ssdr(ior) 
      hc2 = hc2+dorgSS(ior)+drfaek(ior)+drfaeg(ior) 
      hc2 = hc2+drfaeb(ior)+drfaes(ior)
      if(hc2<0.0)hc2 = 0.0 
      if(hc1<0.0)hc1 = 0.0
                                                                       
      if(hc1>0.0)then
        fssgr(ior) = hc2/hc1
          else
            fssgr(ior) = 0.0
      endif

      delfss = fssgrv-fssgr(ior) 
      if(fssgr(ior).lt.0.0)fssgr(ior) = (fssgrv/(fssgrv+abs(delfss)))*fssgrv                 
      fssgrt = fssgr(ior) 
      fssgr(ior) = fssgrv 
                                                                       
      delss = sst-ss(ior) 
      if(sst.lt.0.0)sst = (ss(ior)/(ss(ior)+abs(delss)))*ss(ior)                      
                                                                       
      SSALG(ior) = SSt+agr(ior)+aki(ior)+abl(ior)+(ZOOind(ior)*GROT/1000.)                                         
                                                                       
  ENDDO                        ! Ende Knotenschleife
                                                                       
      ss(anze+1) = sst 
      fssgr(anze+1) = fssgrt 
                                                                       
      RETURN 
      END                                           
