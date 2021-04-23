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

      SUBROUTINE silikat(si,flag,elen,ior,esi,qeinl,vabfl,anze,tflie,jiein,aki         &
                         ,albewk,alberk,tiefe,tempw,ilbuhn,akkssi,Qmx_SK,Q_SK          &
                         ,up_Siz,Siz,algakz,akitbr,akibrz,hJSi,nkzs,dH2D,dH2De,mstr    &
                         ,iorLa,iorLe,ieinLs,flae,qeinlL,SiL,itags,Uhrz,azStrs         &                                                   
                         ,kontroll ,jjj ) !!wy  
                                                                       
                                                                       
!     EIN PROGRAMM zu Berechnung des geloesten Silikats                 
                                                                       
                                                                       
                                                                       
!     AUTOR : VOLKER KIRCHESCH                                          
                                                                       
                                                                       
                                                                       
                                                                       
!     STAND : 24.08.2011                                                
                                                                       
                                                                       
      logical kontroll !!wy
      integer jjj !!wy
    integer                        :: anze, azStrs
    integer, Dimension(azStrs)     :: ieinLs
    integer, Dimension(100)        :: iorLa,iorLe  
    integer, Dimension(1000)       :: flag,jiein,nkzs
    real, Dimension(50)            :: Sizt,hcSiEz,akisi,hcSiz, siz_neu
    real, Dimension(100)           :: esi,qeinl,qeinlL,SiL
    real, Dimension(1000)          :: si,elen,tiefe,albewk,alberk,tempw,vabfl,flae,dH2De,akitbr,Q_SK,aki
    real, Dimension(50,1000)       :: siz,akibrz,up_Siz,algakz
    real, Dimension(azStrs,1000)   :: hJSi  
                                              
      iein = 1 
      ieinL = 1 

      Sit = 0.0
                                                                       
!....Berücksichtigung der Linienquelle                                  
    
      do ieinL = 1, ieinLs(mstr)
        if(qeinlL(ieinL)>=0.0.and.SiL(ieinL)==-1.)cycle 
        do ior = 1,anze+1
          if(iorLe(ieinL)<ior)cycle
          if(iorLa(ieinL)<=ior.and.iorLe(ieinL)>=ior)then
            if(qeinlL(ieinL)<=0.0)SiL(ieinL) = 0.0
            do nkz = 1,nkzs(ior)  ! 2D
              Siz(nkz,ior) = Siz(nkz,ior)+((SiL(ieinL)-Siz(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
            enddo
              Si(ior) = Si(ior)+((SiL(ieinL)-Si(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
                else
          endif
       enddo ! Ende Knotenschleife 
     enddo   ! Ende Schleife Linienquellen
                                                                     
      do j = 1,anze+1                   ! Beginn Knotenschleife 

      ior = j
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

              
              hcSi = Si(ior-m)     ! Umbenennen der benötigten Variablen; 1D
              hcQ = vabfl(ior-m)
              if(hcQ<0.0)hcQ = abs(hcQ)
              if(hcQ==0.0.or.ihcQ==1)hcQ = 1.e-10
              
              nkzs_alt = nkzs(ior-m)
              nkzs_neu = nkzs(ior)
              
              do nkz = 1,nkzs_alt
                siz_neu(nkz) = Siz(nkz,ior-m)
              enddo
  
             

!              call z_gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,siz_neu,hcsiz)
               do nkz=1,nkzs_neu
                 hcsiz(nkz) = siz_neu(nkz)
               enddo 

              do ji=1,jiein(ior)   ! Beginn Einleitungsschleife  
              hcQE = max(0.0,qeinl(iein))

              hcSiE = eSi(iein)
              if(hcSiE<0.0)then            ! 1D
                hcSiE = hcSi

                do nkz = 1, nkzs(ior)      ! 2D
                  hcSiEz(nkz) = hcsiz(nkz)
                enddo
                  else
                    do nkz = 1,nkzs(ior)
                      hcSiEz(nkz) = hcSiE   
                    enddo
              endif

              do nkz = 1,nkzs(ior)      ! 1D 
                Siz(nkz,ior) = (hcQ*hcSiz(nkz)+hcQE*hcSiEz(nkz))/(hcQ+hcQE)                                                     
              enddo 
             
             Si(ior) = (hcQ*hcSi+hcQE*hcSiE)/(hcQ+hcQE) 
             Q_SK(ior) = (hcQ*Q_SK(ior-1)+hcQE*Qmx_SK)/(hcQ+hcQE) 

             hcQ = hcQ+qeinl(iein) 
             iein = iein+1 

             hcSi = Si(ior) 
                                                                         
             do nkz = 1,nkzs(ior) 
               hcSiz(nkz) = Siz(nkz,ior)
             enddo 
           enddo                        ! Ende Einleitungsschleife

           if(ior_flag==1)then
             iein = iein - jiein(ior)
             ior = ior-1
             si(ior) = si(ior+1)

             do nkz = 1,nkzs(ior)
               Siz(nkz,ior) = Siz(nkz,ior+1)
             enddo
           endif
    endif                               ! Ende Einleitungs-flag                                                                  
                                                                       
      if(ior>1)then 
      do  nkz = 1,nkzs(ior-1) 
        Siz(nkz,ior-1) = Sizt(nkz) 
       enddo 
                                                                     
      si(ior-1) = sit 
      endif 
                                                                       
!     Konzentrationsaenderung durch Kieselalgen                         
                                                                       
                                                                       
!....Neuberechnung der Silikatmenge an der Gewässersohle nach Rücklösung

    dSised = hJSi(mstr,ior)*tflie/tiefe(ior) ! Änderungsrate durch Silikatfreisetzung aus dem Sediment
                                                                       
!....2D-Modellierung                                                    
!                                                                       

    sakSi = 0.0
    sumH = 0.0

    do nkz = 1,nkzs(ior) 
      akisi(nkz) = -up_Siz(nkz,ior)*(akibrz(nkz,ior)-algakz(nkz,ior))-albewk(ior)*Qmx_SK               
      Sizt(nkz) = siz(nkz,ior)+akisi(nkz) 
                                                                    
      delSi = Sizt(nkz)-Siz(nkz,ior) 
      if(Sizt(nkz)<0.0)Sizt(nkz) = (Siz(nkz,ior)/(Siz(nkz,ior)+abs(delSi)))*Siz(nkz,ior)                                                     
      
      if(nkz>1)then
        sakSi = sakSi + ((akisi(nkz)+akisi(nkz-1))/2.)*dH2D
        sumH = sumH + dH2D
      endif
    enddo 
                                                                       
    if(nkzs(ior)>1)then
      Sit = Si(ior) + sakSi/sumH + dSised 
        else
          Sit = Si(ior) + akisi(1) + dSised
    endif 
                                                                       
      delSi = Sit-Si(ior) 
      if(Sit<0.0)Sit = (Si(ior)/(Si(ior)+abs(delSi)))*Si(ior)                                                          

    enddo                                    ! Ende Knotenschleife
                                                                       
    do nkz = 1,nkzs(anze) 
      Siz(nkz,anze+1) = Sizt(nkz) 
    enddo 
!                                                                       
      si(anze+1) = sit 
!                                                                       
      RETURN 
      END                                           

