  SUBROUTINE SASU(itags,monats,geob,geol,sa,su,zg,zlk,dk,tdj,ifehl)  
                                                                       
                                                                       
                                                                       
                                                                       
!   AUTOR : VOLKER KIRCHESCH                                          
                                                                       
                                                                       
!   STAND : 05.11.1987                                                
                                                                       
                                                                       
!   UNTERPROGRAMME : UP.TAGE                                          
                                                                       
                                                                       
                                                                       
    INTEGER :: ANZT, tdj, stunde 
                                                                       
!   BERECHNUNG DER VERGANGENEN TAGE SEIT DEM 21.M{RZ                  
                                                                       
    CALL TAGE(ITAGs,MONATs,ANZT) 
                                                                       
                                                                       
    if(geoB<=0.0.or.geoL<=0.0)then
      ifehl = 21
        else
         PI = 22./7. 
         INEG = 0 
         tdj = anzt+80 
         PH = 0.9876*ANZT*PI/180. 
         ZG =-7.683*SIN(PH+1.3788)+9.867*SIN(2.*PH) 
         FK = 1.921*SIN(PH+1.3788)-1.8855 
         PH = (PH*180./PI)+FK 
         DK = 0.39875*SIN(PH*PI/180.) 
         DK = ATAN(DK/SQRT(1-DK**2)) 
         GEOBn = GEOB*PI/180. 
         T0 = (-1.)*TAN(GEOBn)*TAN(DK) 
           IF(T0<0.0)THEN 
             T0=T0*(-1.) 
             INEG = 1 
           ENDIF 

         T0 = ATAN(SQRT(1.-T0**2)/T0) 
         if(ineg>0)then 
           t1 = pi-t0 
           t0 = t1 
         endif 

         D0 = 0.833/(COS(GEOBn)*COS(DK)*SIN(T0)) 
         T0 = ((T0*180./PI)+D0)*4. 
         ZLK = (15.-GEOL)*4. 
         SA = (720.-T0-ZG+ZLK)/60. 
         SU = (720.+T0-ZG+ZLK)/60. 
       endif                                                              

       RETURN 
       END subroutine sasu                                           
