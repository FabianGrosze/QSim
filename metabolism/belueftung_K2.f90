  subroutine Belueftung_K2(rau,tiefe,vmitt,rhyd,flae,tempw,WLage,hws,wge,IDWe,iphy,bbei,mstr,ior,azStrs)


!##########################################
!  Berechnung des BELUEFTUNGSBEIWERT in 1/d
!##########################################       
                                                                        
      integer                                :: azStrs
      integer, Dimension(azStrs,1000)        :: IDWe 

      real, Dimension(20)                    :: wge
      real, Dimension(1000)                  :: rau,tiefe,vmitt,rhyd,flae,tempw,bbei 
      real, Dimension(azStrs,1000)           :: WLage, hWS 


      FN = 1./RAU(ior) 
      G = 9.81 
      UST = ((FN*G**0.5)/tiefe(ior)**0.166667)*abs(VMITT(ior)) 
      Slope = (vmitt(ior)/(Rau(ior)*rhyd(ior)**0.6667))**2
      Breite = flae(ior)/tiefe(ior) 
      !print*,"Belueftung_K2: rau,VMITT,rhyd,UST,Slope,Breite,Tiefe,ior=",rau(ior),VMITT(ior),rhyd(ior),UST,Slope,Breite,Tiefe(ior),ior


      zw10 = 10.
      fkWind = 1.
      zWmess = WLage(mstr,ior)-hWS(mstr,ior)
      if(zWmess .gt. 0.0) fkwind = (zw10/zWmess)**0.11 
      
      wge10 = 0.0
      if(wge(IDWe(mstr,ior)) .gt. 0.0) wge10 = wge(IDWe(mstr,ior)) * fkwind
      
      SC = -0.0308*Tempw(ior)**3 + 3.0286*Tempw(ior)**2 - 112.37*Tempw(ior) + 1845
      if(SC .lt. 1.0) SC= 1.0

      Wind_Kl = (40.94*SC**(-0.5)) * wge10**1.81 * ((1.2/998.)**0.5)
      ! Wind_Kl = 40.94*SC**(-0.5) * wge10**1.81 * (1.2/998.)**0.5  ! original
      if(isnan(Wind_Kl)) print*,"Belueftung_K2: Wind_Kl,SC,wge10,zWmess,fkwind,WLage,hWS,wge=",   &
                Wind_Kl,SC,wge10,zWmess,fkwind,WLage(mstr,ior),hWS(mstr,ior),wge(IDWe(mstr,ior))

      if(iphy==1.or.iphy==2)then
        if(iphy==2)Wind_Kl = 0.0
        bbeiw = 79.6*(abs(vmitt(ior))*Slope)**0.32*Tiefe(ior)**(-0.38)*Breite**(-0.16)  ! Tracer
        bbei(ior) = bbeiw+Wind_Kl/Tiefe(ior)
        if(isnan(bbei(ior))) print*,'Belueftung_K2 isnan(bbei iphy=',iphy
      else if(iphy==3)then
        bbei(ior) = ((3.+40./rau(ior))*abs(vmitt(ior))/tiefe(ior)**2)   ! +0.5/tiefe(ior)                                                       
        bbeiw = 10.47*abs(vmitt(ior))**0.43*Tiefe(ior)**(-1.37)*Slope**0.22  ! gleiche Datengrundlage wie Wolf (1974)
        bbei(ior) = bbei(ior)+Wind_Kl/Tiefe(ior)
        if(isnan(bbei(ior))) print*,'Belueftung_K2 isnan(bbei iphy=',iphy
      else if(iphy==4)then
        bbei(ior) = 142.*(abs(vmitt(ior))*Slope)**0.333*tiefe(ior)**(-0.66)*Breite**(-0.243)
        if(isnan(bbei(ior))) print*,'Belueftung_K2 isnan(bbei iphy=',iphy
      endif
        
      if(bbei(ior)>20.)bbei(ior) = 20.
                                                                       
! +++ Temperaturabhängigkeit ++++ 
      BBEI(ior) = BBEI(ior)*(1.024**(TEMPW(ior)-20.))     

  end subroutine Belueftung_K2
