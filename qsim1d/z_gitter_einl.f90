    subroutine z_Gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,Y_alt,Y_var)


! Ein Programm zur Belegung eines neuen vertikalen Gitters mit äquidistanten Gitterpunkten mit Werten 
! von einem alten Gitter mit gleichen Gitterweiten aber unterschiedlicher Anzahl von Gitterpunkten an Einleitungsstellen

 


    integer             :: nkzs_alt, nkzs_neu
    real, Dimension(50) :: zStern_alt, z_neu, Y_alt, Y_var 


    Tiefe_alt = (nkzs_alt-1)*dH2D
    Tiefe_neu = (nkzs_neu-1)*dH2D

    delta_H = (Tiefe_neu - Tiefe_alt)/nkzs_alt  ! 

    zStern_alt(nkzs_alt) = delta_H
    do nkz = nkzs_alt-1,1,-1
      zStern_alt(nkz) = zStern_alt(nkz+1)+dH2D+delta_H ! Gitterweiten für neue Tiefe; Gitteranzahl des alten Gitters.
    enddo

    do nkz = nkzs_neu-1,1,-1
      z_neu(nkz) = z_neu(nkz+1)+dH2D
    enddo
       
!   lineare Interpolation nach Isaac Newton

    Y_var(nkzs_neu) = Y_alt(nkzs_alt)

    do nkz_neu = nkzs_neu-1,2,-1
      do nkz_alt = nkzs_alt,2,-1
        if(z_neu(nkz_neu)<zStern_alt(nkz_alt))then ! Falls der zweite Wert von z_neu (0.25) kleiner ist als der erste Wert von zStern_alt
          Y_var(nkz_neu) = Y_alt(nkz_alt)
          exit
        endif  
        if(z_neu(nkz_neu)>=zStern_alt(nkz_alt).and.z_neu(nkz_neu)<=zStern_alt(nkz_alt-1))then
          y_var(nkz_neu) = Y_alt(nkz_alt)+((Y_alt(nkz_alt-1)-Y_alt(nkz_alt))/(zStern_alt(nkz_alt-1)-zStern_alt(nkz_alt)))*(z_neu(nkz_neu)-zStern_alt(nkz_alt))
          exit
            else
              cycle
        endif
      enddo
    enddo

    Y_var(1) = y_alt(1)

  end subroutine z_gitter_einl

