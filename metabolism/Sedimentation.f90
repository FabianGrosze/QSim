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

  subroutine Sedimentation(ior,xtiefe,ised,ust,qsgr,oc,Oc0,ytflie,wst,jsed,ZellV    &
     &                ,kontroll ,jjj ) !!wy  
 
                                                                       
      logical kontroll !!wy
      integer jjj !!wy
    double precision :: sedoc, ws 

     real, Dimension(1000)    :: xtiefe
                                                                       
  if(jsed==0)then
!   Algen                                                                
    if(ised==1)then 
      ased = 6.67e-7 
      bsed = 2.78 
      prop = 0.53 
!   BSB                                                                 
        else if (ised==2)then 
          ased = 1.44E-6 
          bsed = 3.13 
          prop = 0.6 
!   Gesamt-SS                                                           
            else if(ised==3)then
              ased = 1.74E-4 
              bsed = 1.63 
              prop = 1.36 
!   Nitrifikanten
              else if(ised==4)then   
                ASED = 1.91E-7 
                BSED = 3.00
                prop = 0.56  
    endif 
      ELSE
!   Algen                                                                
    if(ised==1)then
      bsed = 2.7 
      prop = 0.5 

      WsAlg = 2.0155*log10(ZellV)-11.512
      WsAlg = 10**WsAlg

      Ased = 0.5/(0.5*exp(-bsed*log10(WsAlg)))
!   BSB                                                                 
        else if (ised==2)then 
          ased = 2.43E-7 
          bsed = 2.5 
          prop = 0.7 
!   Gesamt-SS                                                           
            else if(ised==3)then
              ased = 1.55e-7 
              bsed = 2.8
              prop = 0.75 
!   Nitrifikanten
              else if(ised==4)then   
                ASED = 2.43E-7 
                BSED = 2.5
                prop = 0.7  
    endif 

  ENDIF
                                                                       
!      wsgr = 0.625*ust**2.1
      wsgr =  0.14*ust**2+0.0054*ust+1.25e-6

      qsgr = 1./(1+ased*exp(-bsed*alog10(wsgr))) 

      qssed = (1+qsgr)/2. 
      if(kontroll)print*,"Sedimentation: qssed-1.)-log(ased))/(-bsed),ust",qssed,ased,bsed,ust
      ws = 0.0
      ws0 = 0.0
      if((qssed.ne. 1.0).and.(bsed.ne. 0.0)) ws = (log(1./qssed-1.)-log(ased))/(-bsed)
      if(bsed.ne. 0.0) ws0 = (log(1./0.5-1.)-log(ased))/(-bsed)   !ws0 - Sinkgeschwindigkeit in ruhendem Medium
      ws = 10**ws 
      ws0 = 10**ws0 
                                                                       
!      fwst = 1.14*exp(-188.2*ust) 
      fwst = exp(-604.2*ust)

      if(ised==3)fwst = max(1.91e-19,fwst)
      if(fwst.gt.1.)fwst = 1. 
                                                                       
      wst = ws*fwst 
      if(kontroll)print*,"Sedimentation: prop*wst*yTFLIE*86400./xTIEFE(ior)",prop,wst,yTFLIE,xTIEFE(ior)
      Oc = 1./(EXP(prop*wst*yTFLIE*86400./xTIEFE(ior))) 
      oc = 1.-Oc 

      Oc0 = 1./(EXP(prop*ws0*yTFLIE*86400./xTIEFE(ior))) ! sedimentierter Anteil in ruhendem Medium
      OC0 = 1. - Oc0

                                                                       
  END subroutine Sedimentation    
