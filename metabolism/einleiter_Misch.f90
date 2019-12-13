  subroutine Einleiter_misch(ns,i,C,Cpart,hcQ,hcQE,CE,rohE,D,dH2D)


    integer, Dimension(1000)        :: ns                       
    real                            :: Masseg, Massep, Masse_neu
    real, Dimension(50)             :: Cg, Cpart, dMassep, D
    real , Dimension(50,1000)       :: C


    Masseg = 0.0
    Massep = 0.0

    j = 0
    do n = 1,ns(i)
      j = j+1
      if(rohE<D(n))exit    ! D wird von der Sohle aus berechnet D1 -> Sohle, Dns -> Oberfläche
    enddo

    jneu = (ns(i)-j)+1        !jneu: Anzahl der Schichten von der Oberfläche aus
    j = jneu 
    if(j==(ns(i)-1))j = ns(i)

    Qp = hcQ/ns(i)
    QEp = hcQE/j

    do n=1,ns(i)                         ! gleichverteilte Einleitung ueber die Vertikale
      Cg(n) = (hcQ*C(n,i)+hcQE*CE)/(hcQ+hcQE)
    enddo
  
    do n=1,j
      Cpart(n) = (Qp*C(n,i)+QEp*CE)/(Qp+QEp)  ! Einleitung wird nur in den Teil der Vertikalen eingemischt
                                              ! fuer den Gilt DEin<=DVorfluter 
    enddo

    do n = j+1,ns(i)
      Cpart(n) = C(n,i)                       ! für die uebrigen vertikalen Schichten erfolgt keine Temperaturaenderung
    enddo
      

  end subroutine einleiter_misch

