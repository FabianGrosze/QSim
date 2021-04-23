  subroutine wind_stroemung(tiefe,nkzs,ior,dH2D,hconus,uwind)                                                     
                                                                       
                                                                       
                                                                       
!   Autor: V. Kirchesch                                               
                                                                       
                                                                      
!   Stand: 14.02.07                                                   
                                                                       
!   Berechnung des Windeinflusses auf die Strömungsgeschwindigkeit    
                                                                       
                                                                       
    integer,Dimension(1000) :: nkzs 
    real,Dimension(50)      :: uwind
    real, Dimension(1000)   :: tiefe 
                                                                       
                                                                       
    zwS = tiefe(ior) 
                                                                       
    gamW = 0.35 

    zsh = 2.2e-4 
    zbh = 6.e-5 
    zb = zbh*tiefe(ior) 
    zs = zsh*tiefe(ior) 
                                                                       
!.......................                                                
                                                                       
    p1 = gamW*zsh 
    p2 = gamW*zsh/zbh 
    q1 = ((1.+zsh)*log(1.+(1./zsh)))-1. 
    q2 = (zsh*log(1.+(1./zbh)))-1. 
                                                                       
    A = q2/((p1*q2)-(q1*p2)) 
    B = -q1/((p1*q2)-(q1*p2)) 
    C = 0.0 
                                                                       
    do nkz = 1,nkzs(ior) 
      uwind(nkz) = A*hconus*(-1.)*log(1+(zwS/zs))+B*hconus*(-1.)*log(1.-(zwS/(zb+tiefe(ior))))                     
      zWs = ZWs-dH2D 
      if(zWs<0.0)zWs = 0.0 
    enddo 
                                                                       
      return 
  END subroutine wind_stroemung                                           
