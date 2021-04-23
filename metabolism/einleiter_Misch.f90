!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

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

