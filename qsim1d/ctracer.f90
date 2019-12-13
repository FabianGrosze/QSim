      SUBROUTINE CTracer(TEMPW,flag,anze,qeinl,etemp,vabfl,jiein,ilbuhn,nkzs,itags,uhrz,mstr)
                                                                       
                                                                       
!                                                                       
!     EIN PROGRAMM ZUR BERECHNUNG DER EINLEITUNGEN bei TRACERBERECHNUNG                  
                                                                       
                                                                       
                                                                       
!     AUTOR : VOLKER KIRCHESCH                                          
                                                                       
                                                                       
                                                                       
!     STAND : 02.07.2012                                                
                                                                       
                                                                       
     
      integer                          :: anze
      real, Dimension(100)             :: qeinl, etemp                                
      integer, Dimension(1000)         :: flag, jiein, nkzs  
      real, Dimension(1000)            :: tempw, vabfl 
                                                                      
                                                                       
!      open(unit=23,file='temp.tst') 
                                                                       
      iein = 1

      do j = 1,anze+1
        nkzs(j) = 1
        ior = j

        ior_flag = 0
        if(flag(ior)==6.and.vabfl(ior)<0.0.and.vabfl(ior+1)>0.0.and.flag(ior+1)==4)then
          ior = ior+1
          ior_flag = 1
        endif

      if(ilbuhn==1)then
          else if(flag(ior)/=4)then 
            else  ! Einleitung
              m = 1
              ihcQ = 0
              if(vabfl(ior)<0.0)m = -1            ! if(vabfl(ior-1)<0.0.and.vabfl(ior)<0.0)m = -1
              if(vabfl(ior-1)<0.0.and.vabfl(ior)>0.0)ihcq = 1

              hctemp = tempw(ior-m) 

              hcQ = vabfl(ior-m)
              if(hcQ<0.0)hcQ = abs(hcQ)
              if(hcQ==0.0.or.ihcQ==1)hcQ = 1.e-10
                                                                       
              do ji=1,jiein(ior) ! Beginn Einleiterschleife 
                hcQE = max(0.0,qeinl(iein))
 
                hcTE = etemp(iein)
                if(hcTE<0.0)hcTE = 0.0

                tempw(ior) = (abs(hcQ)*hctemp+hcQE*hcTE)/(hcQ+hcQE)

                hcQ = hcQ+qeinl(iein) 
                hctemp = tempw(ior) 
                iein = iein+1 
              enddo ! Ende Einleiterschleife 
              
              if(ior_flag==1)then
                 ior = ior-1
                 tempw(ior) = tempw(ior+1)
              endif

      endif

      enddo ! Ende Knotenschleife 

      RETURN 
      END                                           
