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
  subroutine Belueftung_K2(raus,tiefes,vmitts,rhyds,flaes,tempws,WLages,hwss,wges,iphys,bbeis)

!##########################################
!  Berechnung des BELUEFTUNGSBEIWERT in 1/d
!##########################################       
	  
      implicit none
                                                                        
      real                  :: wges
      real                  :: raus,tiefes,vmitts,rhyds,flaes,tempws,bbeis 
      real                  :: WLages, hwss
      integer               :: iphys
	  
	  real FN,G,UST,Slope,Breite,zw10,fkWind,zWmess,wge10,SC,Wind_Kl,bbeiw

      FN = 1./raus 
      G = 9.81 
      UST = ((FN*G**0.5)/tiefes**0.166667)*abs(vmitts) 
      Slope = (vmitts/(raus*rhyds**0.6667))**2
      Breite = flaes/tiefes 
      !print*,"Belueftung_K2: rau,VMITT,rhyd,UST,Slope,Breite,Tiefe,ior=",raus,vmitts,rhyds,UST,Slope,Breite,tiefes,ior


      zw10 = 10.
      fkWind = 1.
      zWmess = WLages-hWSs
      if(zWmess .gt. 0.0) fkwind = (zw10/zWmess)**0.11 
      
      wge10 = 0.0
      if(wges .gt. 0.0) wge10 = wges * fkwind
      
      SC = -0.0308*tempws**3 + 3.0286*tempws**2 - 112.37*tempws + 1845
      if(SC .lt. 1.0) SC= 1.0

      Wind_Kl = (40.94*SC**(-0.5)) * wge10**1.81 * ((1.2/998.)**0.5)
      ! Wind_Kl = 40.94*SC**(-0.5) * wge10**1.81 * (1.2/998.)**0.5  ! original
      if(isnan(Wind_Kl)) print*,"Belueftung_K2: Wind_Kl,SC,wge10,zWmess,fkwind,WLage,hWS,wge=",   &
                Wind_Kl,SC,wge10,zWmess,fkwind,WLages,hWSs,wges

      select case (iphys) ! verschiedene Belüftungsformeln </AerFormulas>
      case(1) ! k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16+K2wind (mit Wind) - Berechnung nach Kirchesch
         bbeiw = 79.6*(abs(vmitts)*Slope)**0.32*tiefes**(-0.38)*Breite**(-0.16)
         bbeis = bbeiw+Wind_Kl/tiefes
      case(2) ! k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16 (ohne Wind) - Berechnung nach Kirchesch
         bbeis = 79.6*(abs(vmitts)*Slope)**0.32*tiefes**(-0.38)*Breite**(-0.16)
      case(3) ! k2=10.47*v^0.43*H^-1.37*S^0.22+K2wind (Dantengrundlage Wolf 1974) - Berechnung nach Wolf (überarbeitete Form)
	     !### rechnet nicht nach angegebener Formel
         bbeis = ((3.+40./raus)*abs(vmitts)/tiefes**2)   ! +0.5/tiefes                                                       
         bbeiw = 10.47*abs(vmitts)**0.43*tiefes**(-1.37)*Slope**0.22  ! gleiche Datengrundlage wie Wolf (1974)
         bbeis = bbeis+Wind_Kl/tiefes
      case(4) ! K2=142*(v*S)^0.333*H^-0.66*B^-0.243 (Melching 1999) - Berechnung nach Melching
         bbeis = 142.*(abs(vmitts)*Slope)**0.333*tiefes**(-0.66)*Breite**(-0.243)
      case default
	     print*,'Belueftung_K2: Belüftungsformel iphy=',iphys,' nicht vorhanden.'
         stop 234
      end select

      if(isnan(bbeis))then
		 print*,'Belueftung_K2 isnan(bbei iphys=',iphys
         stop 234
      endif
      if(bbeis>20.)bbeis = 20.
                                                                       
! +++ Temperaturabhängigkeit ++++ 
      bbeis = bbeis*(1.024**(tempws-20.))     

  end subroutine Belueftung_K2
