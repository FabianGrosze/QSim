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

  subroutine van_Veen(rau,tiefe,hvmitt,nkzs,dH2D,zf,xU,dvdz,WGe,IDWe,mstr,ior,hconus,hconub,Uvert,Wlage,hWS    &
                      ,i_windP,azStrs)                                                         
                                                                       
                                                                       
!  ...subroutine zur Berechnung eines vertikalen Geschwindigkeitsprofils
                                                                       

!     Autor: Volker Kirchesch                                           
                                                                       

!     Stand:04.01.2005                                                  
                                                                       
                                                                       
      integer                                   :: azStrs
      integer, Dimension(1000)                  :: nkzs
      integer, Dimension(azStrs,1000)           :: IDWe
 
      real                                      :: kappa,lamda0
      real, Dimension(20)                       :: WGe 
      real, Dimension(50)                       :: xU, dvdz, Uzz, uwind 
      real, Dimension(1000)                     :: tiefe, rau, vmitt   
      real, Dimension(50,1000)                  :: Uvert                   

      real, Dimension(azStrs,1000)              :: hvmitt, WLage, hWS 

                                                                       
      g = 9.81      ! in m/m2 
      PI = 22./7. 
                                                                     
      expm = 3.

      alpham = ((10.*expm)/(expm+1))*0.02**(1./expm) 
                                                                       
      z = tiefe(ior)

      vmitt(ior) = hvmitt(mstr,ior)

      FN = 1./RAU(ior) 
      uf = ((FN*G**0.5)/TIEFE(ior)**0.166667)*abs(VMITT(ior))
      u0 = 10.*uf

      zf = tiefe(ior)/(((abs(vmitt(ior))+alpham*uf)/(alpham*uf))**expm)

      z0 = min((tiefe(ior)-dH2D),50.*zf)
                                                          

      z10 = 10.    ! 10 m Höhe über der Wasseroberfläche  

      if(Wlage(mstr,ior)-hWS(mstr,ior)<=0.0)then 
        hconW = 1. 
          else 
            zWmess = Wlage(mstr,ior)-hWS(mstr,ior)     ! Höhe der Windmessung 
            hconW = (z10/zWmess)**0.1 
      endif 

      W = Wge(IDwe(mstr,ior))*hconW 
                                                                       
      C10 = 0.8+0.065*W 
      C10 = C10*1.e-3 
      hconus = (((C10*1.2/1000.)**0.5)*W) 
      hconus = +1.*hconus ! -1. Wind Gegen die Fließrichtung; +1. Wind in Fließrichtung
 
      call wind_stroemung(tiefe,nkzs,ior,dH2D,hconus,uwind)                                                    

      do nkz = 1,nkzs(ior) 

        Uzz(nkz) = u0*(max(0.0,(z/z0)))**(1./expm)

        if(ISNAN(Uzz(nkz)))Uzz(nkz) = 0.0 

        z = z-dH2D 

        if(vmitt(ior)<0.0)Uzz(nkz) = -1.* Uzz(nkz)

!        if(uwind(nkz)<0.0.or.i_windP==0)uwind(nkz) = 0.0

        Uvert(nkz,ior) = Uzz(nkz)+uwind(nkz)
!        if(Uzz(nkz)<0.0)Uvert(nkz,ior) = Uzz(nkz)+(-1.*uwind(nkz))
        if(vmitt(ior)>0.0.and.Uzz(nkz)<0.0)Uvert(nkz,ior) = 0.001

        xU(nkz) = Uvert(nkz,ior)

     enddo                                                                       

      dvdz(1) = abs((xU(2)-xU(1)))/dH2D 
      dvdz(nkzs(ior)) = abs((xU(nkzs(ior))-xU(nkzs(ior)-1)))/dH2D                                                             

                                                                       
      FN = 1./RAU(ior) 
      hconub = ((FN*G**0.5)/TIEFE(ior)**0.166667)*abs(VMITT(ior))   ! wird sich beim Testen noch zeigen, ob abs hier richtig 

                                                                       
  end Subroutine van_veen 
