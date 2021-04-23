  SUBROUTINE TAGE(ITAGs,MONATs,ANZT) 
                                                                       
!   UNTERPROGRAMM ZUR BERECHNUNG DER VERGANGENEN TAGE                 
!   SEIT DEM 21.MAERZ                                                 
                                                                       
                                                                       
!   AUTOR:VOLKER KIRCHESCH                                            
                                                                       
!   STAND:21.8.84                                                     
                                                                       
                                                                       
    INTEGER :: ANZT 


    IF(MONATs<=2)then
      ANZT = ITAGs+31*(MONATs-1)+285 
        else if(monats==3.and.itags<=21)then
          ANZT = (ITAGs+31*(MONATs-1)-INT(0.4*MONATs+2.3))+285 
            else 
              ANZT = (ITAGs+31*(MONATs-1)-INT(0.4*MONATs+2.3))-80 
    endif 

    RETURN 
  END subroutine tage                                           
