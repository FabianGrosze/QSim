! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !

!> Volumenstrom und Tracerfluss (nach Backbord) entlang aller Querschnitte ermitteln
!!
!! wird von ganglinien_zeitschritt() aufgerufen
subroutine querschnitt_flux_casu(zeitzaehler)
   use modell
   implicit none
   
   integer, intent(in) :: zeitzaehler
   
   integer :: i, j, k, bt, tp, no
   real    :: deltax, d1, d2, u1, u2, la, flae, vox, pox, kix
   real    :: lang, flaeche, vol_strom, pot_ener_flux, kin_ener_flux
   real    :: c1, c2, masx, massen_flux
   
   do i = 1, anzahl_quer !! alle i Querschnitte
      lang = 0.0
      flaeche = 0.0
      vol_strom = 0.0
      pot_ener_flux = 0.0
      kin_ener_flux = 0.0
      massen_flux = 0.0
      do j = 1,querschnitt(i)%schnittlinie%anzkanten
         bt = querschnitt(i)%schnittlinie%kante(j)%bottom
         tp = querschnitt(i)%schnittlinie%kante(j)%top
         deltax = ( (querschnitt(i)%schnittlinie%kante(j)%normal_x**2)   &
                  + (querschnitt(i)%schnittlinie%kante(j)%normal_y**2) )**0.5
         
         d1 = p(bt) - knoten_z(bt)
         d2 = p(tp) - knoten_z(bt)
         u1 = querschnitt(i)%schnittlinie%kante(j)%normal_x*u(bt)*cos(dir(bt))   &
            + querschnitt(i)%schnittlinie%kante(j)%normal_y*u(bt)*sin(dir(bt))
         u2 = querschnitt(i)%schnittlinie%kante(j)%normal_x*u(tp)*cos(dir(tp))   &
            + querschnitt(i)%schnittlinie%kante(j)%normal_y*u(tp)*sin(dir(tp))
         ! u1 und u2 sind bereits das Produkt aus Normalgeschwindigkeitskomponente mal Kantenlänge
         
         c1 = planktonic_variable(71+(bt-1)*number_plankt_vari) ! passiver Alterstracer
         c2 = planktonic_variable(71+(tp-1)*number_plankt_vari)
         
         call flux_casu(deltax, d1, d2, u1, u2, c1, c2, p(bt), p(tp), &
                        u(bt), u(tp), la, flae, vox, pox, kix, masx)
         
         lang = lang + la
         flaeche = flaeche + flae
         vol_strom = vol_strom + vox
         pot_ener_flux = pot_ener_flux + pox
         kin_ener_flux = kin_ener_flux + kix
         massen_flux = massen_flux + masx 
      enddo ! alle j Kanten in Querschnitt i
      
      
      schnittflux_gang(i,zeitzaehler,1) = lang
      schnittflux_gang(i,zeitzaehler,2) = flaeche 
      schnittflux_gang(i,zeitzaehler,3) = vol_strom ! in m³
      schnittflux_gang(i,zeitzaehler,4) = pot_ener_flux / 1000000 ! in Mega-Watt
      schnittflux_gang(i,zeitzaehler,5) = kin_ener_flux / 1000000 ! in Mega-Watt
      schnittflux_gang(i,zeitzaehler,6) = massen_flux !! 71
      
   enddo ! alle i Querschnitte
   
end subroutine querschnitt_flux_casu


!> Fuss über eine Kante
!!
!! Volumenstrom ist das Integral(d*u)dx ; alle beide linear über x (d-Tiefe, u-Geschwindigkeitskomponente senkrecht zum Rand)
!! Massenfluss ist das Integral(c*d*u)dx ; alle 3 (c-Konzentration) sind linear über x
!! Fluss potentielle Energie Integral(rho*g*wsp*d*u)dx ; alle 3 linear über x,
!!                           rho und g konstant (wsp-Wasserspiegelhöhe, rho-Dichte=1000, g-Gravitation=9.81)
!! Fluss kinetische Energie Integral(rho*0.5*v*v*d*u)dx ; alle 4 lin. über x (v*v-Quadrat des Geschwindigkeitsbetrages)
!! u1 und u2 sind bereits das Produkt aus Normalgeschwindigkeitskomponente mal Kantenlänge !!
!!
!! aus schnitt.f95
subroutine flux_casu(deltax, d1, d2, u1, u2, c1, c2, wsp1, wsp2, v1, v2,   &
                     la, flae, vox, pox, kix, masx)
   use modell, only: grav
   implicit none
   
   ! --- dummy arguments ---
   real, intent(in)  :: deltax
   real, intent(in)  :: d1, d2
   real, intent(in)  :: u1, u2
   real, intent(in)  :: c1, c2
   real, intent(in)  :: wsp1, wsp2
   real, intent(in)  :: v1, v2
   real, intent(out) :: la    !< Wasserspiegellänge
   real, intent(out) :: flae  !< Randfläche
   real, intent(out) :: vox   !< Geschwindigkeit am Uferpunkt 0
   real, intent(out) :: pox
   real, intent(out) :: kix
   real, intent(out) :: masx !< Konzentration am Uferpunkt
   
   ! --- local variables ---
   real            :: x_kreuz 
   integer         :: fall
   character(500)  :: message
   
   real, parameter :: rho = 1000.
   
   ! Fallunterscheidung wieviel der Kante unter Wasser ist
   if ((d1 > 0.0 .and. d2 >= 0.0) .or. (d1 >= 0.0 .and. d2 > 0.0)) then
      fall = 1 ! Kante vollständig nass
   else if (d1 <= 0.0 .and. d2 <= 0.0) then
      fall = 2 ! Kante ganz trocken
   else if (d1 <  0.0 .and. d2 >  0.0) then
      fall = 3 ! bottom1-trocken, top2-nass
   else if (d1 >  0.0 .and. d2 <  0.0) then
      fall = 4 ! bottom1-nass, top2-trocken
   else
      fall = 0
   endif
   
   
   select case (fall)
      case (1) ! VOLL
         la = deltax
         flae = deltax * (d1 + d2) / 2.0 
         vox = (1./6.) * (2.*u1*d1 + u1*d2 + u2*d1 + 2.*u2*d2)
         
         ! Integral(wsp*d*u)dx ; alle 3 linear über x
         pox = (1./4.) * (wsp1*d1*u1 + wsp2*d2*u2)   &  
             + (1./12.)* (wsp1*d1*u2 + wsp1*d2*u1 + wsp2*d1*u1 + wsp1*d2*u2 + wsp2*d1*u2 + wsp2*d2*u1)
         pox = pox * rho * grav
         kix = v1 * v1 * ((1./5.)*d1*u1 + (1./20.)*d1*u2 + (1./20.)*d2*u1 + (1.0/30.0)*d2*u2 )  &
             + v1*v2* ((1./10.) *d1*u1 + (1.0/15.0)*d1*u2 + (1.0/15.0)*d2*u1 + (1.0/10.0)*d2*u2 )  &
             + v2*v2* ((1./30.) *d1*u1 + (1.0/20.0)*d1*u2 + (1.0/20.0)*d2*u1 + (1.0/ 5.0)*d2*u2 )
         kix = kix * rho / 2.
         ! Integral(c*d*u)dx ; alle 3 linear über x
         masx = (1./4.) * (c1*d1*u1 + c2*d2*u2)   &  
              + (1./12.)* (c1*d1*u2 + c1*d2*u1 + c2*d1*u1 + c1*d2*u2 + c2*d1*u2 + c2*d2*u1)
      
      case(2) ! NIX
         la   = 0.0 
         flae = 0.0 
         vox  = 0.0
         pox  = 0.0
         kix  = 0.0
         masx = 0.0
      
      case(3) ! bottom1-trocken-negativ, top2-nass-positiv
         ! Integral von Ufer-Punkt bis 2
         if (d1 - d2 == 0.0) then
            call qerror("x_kreuz == 0.0,flux,case (3)")
         endif
         
         x_kreuz = d1 / (d1 - d2) ! relative Lage Uferpunkt (Kreuzung Sohle-Wasserspiegel) ab 1
         la = deltax * (1. - x_kreuz) 
         ! Wassertiefe am Uferpunkt 0
         flae = (deltax * (1. - x_kreuz) * d2) / 2.
         
         vox = ((1.-x_kreuz) / 3.) * u2 * d2
         pox = (1./4.) * (wsp2*d2*u2)   &  ! Integral(wsp*d*u)dx ; alle 3 linear über x
             + (1./12.) * ((wsp2*x_kreuz) + wsp1*(1.-x_kreuz))*d2*u2
         pox = (1. - x_kreuz) * pox * rho * grav
         kix = 0.0
         ! Konzentration am Uferpunkt (c2*x_kreuz)+c1*(1.0-x_kreuz)
         masx = (1.-x_kreuz) * ((1./4.)*c2*d2*u2 + (1./12.)*((c2*x_kreuz) + c1 * (1.-x_kreuz)) * d2 * u2)
      
      case(4) ! bottom1-nass-positiv, top2-trocken-negativ
         if (d1 - d2 == 0.0) then
            call qerror("x_kreuz == 0.0,flux,case (4)")
         endif
         
         ! Integral von 1 bis Ufer-Punkt
         x_kreuz = d1 / (d1 - d2) ! relative Lage Uferpunkt (Kreuzung Sohle-Wasserspiegel) ab 1
         la = deltax * x_kreuz ! Wasserspiegellänge
         ! Wassertiefe am Uferpunkt 0
         flae = deltax * x_kreuz * d1 / 2. ! Randfläche
         ! Geschwindigkeit am Uferpunkt 0
         vox = (x_kreuz/3.) * u1 * d1
         pox = (1./ 4.) * (wsp1*d1*u1)   &  ! Integral(wsp*d*u)dx ; alle 3 linear über x
             + (1./12.) * ((wsp2*x_kreuz) + wsp1 * (1.-x_kreuz)) * d1 * u1
         pox = x_kreuz * pox * rho * grav
         kix = 0.0
         ! Konzentration am Uferpunkt (c2*x_kreuz)+c1*(1.0-x_kreuz)
         masx = x_kreuz * ((1./4.) * c1*d1*u1 + (1./12.) * ((c2*x_kreuz) + c1*(1.-x_kreuz)) * d1 * u1)
      
      case default
         write(*, "(a,i0)") 'Fallunterscheidung in flux_casu(): fall = ', fall
         call qerror("Fallunterscheidung fehlgeschlagen in flux_casu().")
   end select
   
   return
end subroutine flux_casu



!> Schnitte für die Flussberechnung lesen und prüfen
!!
!! wird von ganglinien_lesen() aufgerufen.
!!
!! aus schnitt.f95 , zurück: \ref lnk_ergebnisausgabe
logical function querschnitt_lesen()
   use modell
   implicit none
   
   integer               :: nio, io_error, alloc_status, i, j, n, k, l, l1, l2 
   integer               :: nel, bt, tp, nelkntp, nelknbt
   integer, dimension(2) :: el
   character(len=300)    :: filename
   real                  :: lang
   
   ! --------------------------------------------------------------------------
   ! read
   ! --------------------------------------------------------------------------
   print*," "
   print*,"Reading schnitt.txt"
   
   filename = trim(modellverzeichnis) // 'schnitt.txt'
   open(newunit = nio , file = filename, status = 'old', action = 'read ', iostat = io_error)
   if (io_error /= 0) then
      print*,"schnitt.txt nicht vorhanden, daher keine Querschnitte."
      querschnitt_lesen = .false.
      return
   else
      print*,"schnitt.txt vorhanden. Flussberechnungen an Querschnitten wird eingerichtet"
      print*,"Ausgaben auf ganglinien/q*.txt)"
      querschnitt_lesen = .true.
   endif 


   if (.not. naechste_zeile(nio)) call qerror("Anzahl der Querschnitte in schnitt.txt nicht gefunden")
   read (ctext, * , iostat = io_error) anzahl_quer
   if (io_error /= 0) then
      call qerror("schnitt.txt anzahl_quer nicht lesbar.")
   endif !anzahl_quer lesbar
   
   print*,"schnitt.txt - Anzahl der Querschnitte: ", anzahl_quer
   ! type(qusch) , allocatable , dimension (:) :: querschnitt
   
   allocate (querschnitt(anzahl_quer), stat = alloc_status)
   if (alloc_status /= 0) then
      write(fehler,*)'allocate (querschnitt(anzahl_quer) fehlgeschlagen'
      call qerror(fehler)
   endif 
   
   do i = 1, anzahl_quer
      if (.not. naechste_zeile(nio)) then
         write(fehler,*)"Querschnitt #",i," Knotenanzahl nicht auffindbar"
         call qerror(fehler)
      endif
      
      read (ctext, *, iostat = io_error) querschnitt(i)%schnittlinie%anzkanten
      if (io_error /= 0) then
         print*,"schnitt #",i
         call qerror("Lesefehler schnitt.txt Knotenanzahl")
      endif
      
      if (querschnitt(i)%schnittlinie%anzkanten <= 1) then
         call qerror("Querschnitte mit weniger als 2 Knoten machen keinen Sinn.")
      endif
      
      querschnitt(i)%schnittlinie%anzkanten = querschnitt(i)%schnittlinie%anzkanten - 1
      allocate (querschnitt(i)%schnittlinie%kante(querschnitt(i)%schnittlinie%anzkanten),    stat = alloc_status)
      allocate (querschnitt(i)%schnittlinie%knoten(querschnitt(i)%schnittlinie%anzkanten+1), stat = alloc_status)
      
      do j = 1, querschnitt(i)%schnittlinie%anzkanten+1 !! alle j Knoten
         if (.not. naechste_zeile(nio)) then
            call qerror("Lesefehler schnitt.txt Knotennummer")
         endif
         
         read (ctext, * , iostat = io_error) querschnitt(i)%schnittlinie%knoten(j)
         if (io_error /= 0) then
            print*,"schnitt #",i," knoten ",j
            call qerror("Lesefehler schnitt.txt Knotennummer")
         endif
      enddo 
   
   enddo ! alle i Querschnitte
   
   
   ! Ganglinen allocieren
   allocate (schnittflux_gang(anzahl_quer,zeitschrittanzahl+1, n_pl+2 ), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)'allocate (schnittflux_gang(anzahl_quer, fehlgeschlagen'
      call qerror(fehler)
   endif 

   allocate (q_gangl(zeitschrittanzahl+1), stat = alloc_status )
   
   schnittflux_gang(:,:,:) = 0.0
   q_gangl(:) = 0

   rewind (nio)
   close (nio)
   
   ! --------------------------------------------------------------------------
   ! check
   ! --------------------------------------------------------------------------
   do i = 1,anzahl_quer !! alle i Querschnitte
      if (knoten_rand(querschnitt(i)%schnittlinie%knoten(1)) <= 0) then
         print*,"Erster Knoten in Querschnitt ",i," kein Randknoten"
      endif
      
      if (knoten_rand(querschnitt(i)%schnittlinie%knoten(querschnitt(i)%schnittlinie%anzkanten+1)) <= 0) then
         print*,"Letzter Knoten in Querschnitt ",i," kein Randknoten"
      endif
      
      if (querschnitt(i)%schnittlinie%knoten(1) == querschnitt(i)%schnittlinie%knoten(querschnitt(i)%schnittlinie%anzkanten+1)) then
         print*,"Querschnitt ",i," offensichtlich geschlossene Linie"
      endif
      
      
      lang = 0.0
      do j = 1,querschnitt(i)%schnittlinie%anzkanten !! alle j Kanten
         
         bt = querschnitt(i)%schnittlinie%knoten(j)
         tp = querschnitt(i)%schnittlinie%knoten(j+1)
         querschnitt(i)%schnittlinie%kante(j)%normal_x = -1 * (knoten_y(tp)-knoten_y(bt))
         querschnitt(i)%schnittlinie%kante(j)%normal_y = knoten_x(tp) - knoten_x(bt)
         querschnitt(i)%schnittlinie%kante(j)%bottom   = bt
         querschnitt(i)%schnittlinie%kante(j)%top      = tp
         querschnitt(i)%schnittlinie%kante(j)%laengs   = ((querschnitt(i)%schnittlinie%kante(j)%normal_x**2)  &
                                                       +  (querschnitt(i)%schnittlinie%kante(j)%normal_y**2))**0.5
         do n = 1,kantenanzahl ! alle Kanten
            if (bottom_node(n) == bt .and. top_node(n) == tp) querschnitt(i)%schnittlinie%kante(j)%num = n
            if (bottom_node(n) == tp .and. top_node(n) == bt) querschnitt(i)%schnittlinie%kante(j)%num = -1 * n
         enddo
         
         
         nel = 0
         nelkntp = 0
         nelknbt = 0
         do n = 1,n_elemente ! alle n Elemente
            do l = 1,cornernumber(n)
               if (elementnodes(n,l) == tp) then
                  nelkntp = nelkntp + 1
               endif
               
               if (elementnodes(n,l) == bt) then
                  nelknbt = nelknbt + 1
               endif
               
               ! erste Ecke
               if (l == 1) then
                  l1 = cornernumber(n)
               else
                  l1 = l-1
               endif 
               
               ! Kante bt-tp erkannt
               if (bt == elementnodes(n,l1) .and. tp == elementnodes(n,l)) then
                  nel = nel + 1
                  if (nel >= 3) call qerror("Eine Kante kann nicht in 3 Elementen vorkommen")
                  el(nel) = n
               endif
               
               ! Kante bt-tp erkannt
               if (tp == elementnodes(n,l1) .and. bt == elementnodes(n,l)) then
                  nel = nel + 1
                  if (nel >= 3) call qerror("Eine Kante kann nicht in 3 Elementen vorkommen")
                  el(nel) = n
               endif 
               
            enddo ! alle l (3 oder 4) Ecken von Element k
         enddo ! alle n Elemente im Netz
         
         if (nel /= 2) then
            call qerror("Querschnittskante nicht innerhalb des Berechnungsgebiets")
         endif
         
         lang = lang + querschnitt(i)%schnittlinie%kante(j)%laengs
      
      enddo ! alle j Kanten in Querschnitt i
      
      ! -----------------------------------------------------------------------
      ! print to console
      ! -----------------------------------------------------------------------
      print*,"Querschnitt #", i, "hat ", querschnitt(i)%schnittlinie%anzkanten, &
             " Kanten und ist ", lang, " lang"
      
      do j = 1, querschnitt(i)%schnittlinie%anzkanten
         print*, j
         print*, "n  = ", querschnitt(i)%schnittlinie%kante(j)%num
         print*, "l  = ", querschnitt(i)%schnittlinie%kante(j)%laengs 
         print*, "bt = ", querschnitt(i)%schnittlinie%kante(j)%bottom
         print*, "tp = ", querschnitt(i)%schnittlinie%kante(j)%top
         print*, ""
      enddo
     
   enddo ! alle i Querschnitte
      
   return
end function querschnitt_lesen
