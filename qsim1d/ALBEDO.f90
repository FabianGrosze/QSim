  SUBROUTINE ALBEDO(SH,REFL) 


                                                                       
!   UNTERPROGRAMM ZUR BESTIMMUNG DES REFLEKTIONSANTEILS               
!   DER DIREK.SONNENSTRAHLUNG AN DER GEWAESSEROBERFLAECHE             
                                                                       
                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
                                                                       
!     STAND: 22.09.2010                                                  
                                                                       
!   Literaturangabe!!!!
                                                                       
    REAL, Dimension(15) :: ALB 
 
    PI = 22./7. 

    ALB(1) =100.0 
    ALB(2) = 70.5 
    ALB(3) = 46.0 
    ALB(4) = 32.5 
    ALB(5) = 25.0 
    ALB(6) = 20.0 
    ALB(7) = 15.4 
    ALB(8) = 12.0 
    ALB(9) =  9.5 
    ALB(10) = 8.5 
    ALB(11) = 7.5 
    ALB(12) = 7.0 
    ALB(13) = 6.5 
                                                                       
    SHGR = SH*180/PI 
    IF(SHGR>60.0)THEN 
      REFL = 6.5 
        else 
          N = INT(SHGR/5)+1 
          DALB = ALB(N)-ALB(N+1) 
          DSHGR = SHGR-(N-1)*5 
          ALBINT = DALB/5*DSHGR 
          REFL = ALB(N)-ALBINT 
    endif

    REFL = REFL/100 
    REFL = 1-REFL

!    write(63,*)uhrz,sh,refl 

    RETURN 
  END subroutine albedo                                           
