  subroutine C_Chla(roh_Chlz, xup_N, xakres, CChlaz, nkz, tflie, C_Bio, CChl_Stern, xChla, xaC, xagrow, isyn, iaus)
 
                                                                       

!   Berechnung der Chla-Bildung im Zeitschritt tflie


!   Autor: Volker Kirchesch
!   Stand: 12.11.2015
   



     real, Dimension(50)          :: roh_Chlz, CChlaz

    up_N = xup_N/C_Bio
    PChl = roh_Chlz(nkz)*up_N/(tflie*86400.)  ! up_N Umrechnung von Std. auf sec.
    hconz = exp(PChl * tflie*86400.)
    hconN = exp(xakres*tflie)

    if(isyn==1)then     ! Geider (1997)
      hconV = 1./CChl_Stern

      dChl = hconV*xaC*1000.*(exp(xagrow*roh_Chlz(nkz)*tflie)-1.)

      chla_neu = xchla + dChl

      hconz = chla_neu/xChla
      hconz = exp((log(chla_neu) -log(xChla))) 
    endif

     CChlaz(nkz) = (hconz/hconN)*(1./CChlaz(nkz))
     CChlaz(nkz) = max(CChl_Stern,1./CChlaz(nkz))
   
!   if(iaus==1)write(96,'(f6.3,2x,f6.3,2x,f10.5,2x,f7.3,2x,f10.7,2x,f10.7,2x,f7.3,2x,f10.7,2x,f10.7)')xagrow,dchl,chla_neu,xchla,hconz,hconN
   

  END subroutine C_chla                                       

