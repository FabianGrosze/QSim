  subroutine LichtHemmung(dt,Ic,yK,CChl_Stern,ChlaCz,j)                                             
                                                                  

!   ....Lösung der Diff-Gleichung zur Beschreibung der Lichthemmung        
!   .....mit dem Runge-Kutta-Verfahren 4. Ordnung                          
                                                                       
!   Gleichung:                                                          
                                                                        
!   dH/dt = Kd*I*H+Tau*(1-H)

!    Kd in m2/µE                                        
                                                                        
    real                  :: KI, k1, k2, k3, k4 
    real                  :: Ic,Kd0,Kd
    real, Dimension(50)   :: ChlaCz
                                                                       
!   open(unit=198,file='lichthemmung.tst')                           
                                                                    
!   ...KI0 Zuwachsrate der Hemmung                                         

    PSII0 = 1.5           !  Absortionsfläche dunkeladaptierter Algen in m2/E

    ChlC_Stern = 1./CChl_Stern
    if(ChlaCz(j)>0.0)then
      Chl_C = 1./ChlaCz(j)
      PSII = min(PSII0,PSII0 * (Chl_C/ChlC_Stern)**0.22)
        else
          PSII = PSII0
     endif
     fclose =  PSII0/PSII

    Kd0 = 1.04e-8   ! Fiting-Wert, Messungen nach Anning (2000) 
    Kd = PSII*kd0*fclose*dt*86400.
    
!   ....TauI0 Parameter zur Beschreibung der Erholung                      
    TauI0 = 4.5e-5    ! Ross
    Tau = TauI0*dt*86400.
                                                                       
    h = 0.5 
    do it = 1,2 
      k1 = -Kd*Ic*yk+Tau*(1.-yk) 
      k2 = (yk+0.5*h*k1)*(-Kd*Ic)+Tau*(1.-yk+0.5*h*k1) 
      k3 = (yk+0.5*h*k2)*(-Kd*Ic)+Tau*(1.-yk+0.5*h*k2) 
      k4 = (yk+h*k3)*(-Kd*Ic)+Tau*(1.-yk+h*k3) 

      yk = yk+((h/6.)*(k1+2.*k2+2.*k3+k4)) 
    enddo 

  END Subroutine Lichthemmung                                          
                                                                       
