  subroutine lin_spline (dz_alt,dz_neu,deltaz,n_alt_s,n_neu_s,Y, YY,i_zeiger,iaus,ior)                             
                                                            
                                                            
                                                            
!   AUTOR:VOLKER KIRCHESCH                                 
                                                            
!   STAND:27.07.2012                                       
                                                            
                                                            

!   Ein Programm zur Berechnung eines linearen Splines 



    integer, Dimension (50)              :: m
    real, Dimension(50)                  :: y, yy, aa, bb, ddx

!    open(unit=114,file='lin_Spline.tst')


     x_neu = 0.0
     x_alt = 0.0

     if(i_zeiger==0)then  !  i_zeiger=0: altes Gitter dz = 0.25
       n_alt = 2
         else             !  i_zeiger=1: neues Gitter dz = 0.25
           x_neu = -dz_neu
           n_alt = 1          
     endif

    do n = 1,n_neu_s                         ! Schleife neues Gitter
      if(i_zeiger==0.and.n==n_neu_s)dz_neu = deltaz
      x_neu = x_neu + dz_neu

        do nn = n_alt,n_alt_s                ! Schleife altes Gitter
          if(i_zeiger==1.and.nn==n_alt_s)dz_alt = deltaz
          if(nn>1)x_alt_1 = x_alt
          x_alt = x_alt + dz_alt 

          if(x_neu<=x_alt.and.nn>1)then
            YY(n) = Y(nn-1)+(((y(nn)-y(nn-1))/dz_alt)*(x_neu-x_alt_1))
!          if(ior==113.and.iaus==1)write(96,*)n_alt_s, n_neu_s,n,nn,x_neu,x_alt

              n_alt = nn
            x_alt = x_alt - dz_alt
             exit
          endif           

          if(x_neu<=x_alt.and.nn==1)then
            YY(n) = Y(nn)
            n_alt = nn
            x_alt = x_alt - dz_alt
            exit
          endif
            cycle
         enddo
        if(ior==113.and.iaus==1)write(96,*)n,nn 

      if(x_neu>=x_alt.and.nn>=n_alt_s.and.n<=n_neu_s)YY(n) = Y(n_alt_s)
      enddo
!      if(x_neu>x_alt)YY(n_neu_s) = Y(n_alt_s)
       if(ior==113.and.iaus==1)write(96,*)n,nn,n_alt_s,n_neu_s,Y(n_alt_s)

  end subroutine lin_spline
