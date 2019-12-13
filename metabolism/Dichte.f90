      subroutine Dichte(tempwz,nkzs,D,ior,itags,uhrz,fkm) 



     
      integer, Dimension(1000)             :: nkzs
      real, Dimension(50)                  :: D
      real, Dimension(1000)                :: fkm 
      real, Dimension(50,1000)             :: tempwz


      a0 = 999.842594
      a1 = 6.793952e-2
      a2 = -9.095290e-3
      a3 = 1.001685e-4
      a4 = -1.120083e-6
      a5 = 6.536332e-9

      j = nkzs(ior)
 
      do nkz = 1,nkzs(ior)  

        D(nkz) = a0+a1*tempwz(j,ior)+a2*tempwz(j,ior)**2                               &
                 +a3*tempwz(j,ior)**3+a4*tempwz(j,ior)**4+a5*tempwz(j,ior)**5
        j = j-1

     enddo

      end subroutine Dichte
