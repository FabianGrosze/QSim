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

!----+-----+----
!
!> <h1>Volumenstrom und Tracerfluss (nach Backbord) entlang aller Querschnitte ermitteln</h1>
!! wird von ganglinien_zeitschritt() aufgerufen
!! \n\n aus schnitt.f95 , zurück: \ref Ergebnisse
!!
      SUBROUTINE querschnitt_flux(zeitzaehler)
      use modell
      implicit none
      integer :: zeitzaehler, i,j,k, bt,tp, no
      real :: deltax,d1,d2,u1,u2,la,flae,vox,pox,kix
      real :: lang, flaeche, vol_strom, pot_ener_flux, kin_ener_flux
      real :: c1, c2, masx, massen_flux
      ! real , allocatable , dimension (:) :: c1, c2, masx, massen_flux

      ! allocate(c1(n_pl))
      ! allocate(c2(n_pl))
      ! allocate(masx(n_pl))
      ! allocate(massen_flux(n_pl))

      do i=1,anzahl_quer !! alle i Querschnitte
         lang=0.0
         flaeche=0.0
         vol_strom=0.0
         pot_ener_flux=0.0
         kin_ener_flux=0.0
         massen_flux=0.0
         do j=1,querschnitt(i)%schnittlinie%anzkanten
            bt=querschnitt(i)%schnittlinie%kante(j)%bottom
            tp=querschnitt(i)%schnittlinie%kante(j)%top
            deltax=( (querschnitt(i)%schnittlinie%kante(j)%normal_x**2)   &
     &             + (querschnitt(i)%schnittlinie%kante(j)%normal_y**2) )**0.5
            !# tief(j)=p(j)-knoten_z(j) ! Wassertiefe
            !# if(tief(j).le. min_tief )then ! min_tief parameter aus module_modell
            !#    tief(j)= min_tief
            d1=p(bt)-knoten_z(bt)
            d2=p(tp)-knoten_z(bt)
            u1=querschnitt(i)%schnittlinie%kante(j)%normal_x*u(bt)*cos(dir(bt))   &
     &        +querschnitt(i)%schnittlinie%kante(j)%normal_y*u(bt)*sin(dir(bt))
            u2=querschnitt(i)%schnittlinie%kante(j)%normal_x*u(tp)*cos(dir(tp))   &
     &        +querschnitt(i)%schnittlinie%kante(j)%normal_y*u(tp)*sin(dir(tp))
            !! u1 und u2 sind bereits das Produkt aus Normalgeschwindigkeitskomponente mal Kantenlänge !!
            c1= planktonic_variable(71+(bt-1)*number_plankt_vari) !! passiver alters-tracer
            c2= planktonic_variable(71+(tp-1)*number_plankt_vari)
            call flux(deltax,d1,d2,u1,u2,c1,c2,p(bt),p(tp),u(bt),u(tp),la,flae,vox,pox,kix,masx)
            lang=lang + la
            flaeche=flaeche + flae
            vol_strom=vol_strom + vox
            pot_ener_flux=pot_ener_flux + pox
            kin_ener_flux=kin_ener_flux + kix
            massen_flux=massen_flux + masx !! 
         end do ! alle j Kanten in Querschnitt i
         schnittflux_gang(i,zeitzaehler,1)= lang!!
         schnittflux_gang(i,zeitzaehler,2)= flaeche !!
         schnittflux_gang(i,zeitzaehler,3)= vol_strom !! 
         schnittflux_gang(i,zeitzaehler,4)= pot_ener_flux/1000000 !! in mega-Watt
         schnittflux_gang(i,zeitzaehler,5)= kin_ener_flux/1000000 !! in mega-Watt
         schnittflux_gang(i,zeitzaehler,6)= massen_flux !! 71
         ! print*,"Querschnitt #",i, " ist ",lang," m lang und hat eine Fläche von=",flaeche,"; mittlere Tiefe=",flaeche/lang  &
         ! ," vol_strom=",vol_strom," mittlere Fließgeschwindigkeit=",vol_strom/flaeche
      end do ! alle i Querschnitte

      ! deallocate(c1)
      ! deallocate(c2)
      ! deallocate(masx)
      ! deallocate(massen_flux)

      RETURN
      END subroutine querschnitt_flux

!----+-----+----
!
!> <h1>Fuss über eine Kante</h1>
!! Volumenstrom ist das Integral(d*u)dx ; alle beide linear über x (d-Tiefe, u-Geschwindigkeitskomponente senkrecht zum Rand)\n
!! Massenfluss ist das Integral(c*d*u)dx ; alle 3 (c-Konzentration) sind linear über x \n
!! Fluss potentielle Energie Integral(rho*g*wsp*d*u)dx ; alle 3 linear über x, 
!!                           rho und g konstant (wsp-Wasserspiegelhöhe, rho-Dichte=1000, g-Gravitation=9.81)\n
!! Fluss kinetische Energie Integral(rho*0.5*v*v*d*u)dx ; alle 4 lin. über x (v*v-Quadrat des Geschwindigkeitsbetrages)\n
!! u1 und u2 sind bereits das Produkt aus Normalgeschwindigkeitskomponente mal Kantenlänge !!\n
!! \n\n aus schnitt.f95
      SUBROUTINE flux(deltax,d1,d2,u1,u2,c1,c2,wsp1,wsp2,v1,v2,la,flae,vox,pox,kix,masx)
      !use modell
      implicit none
      real :: deltax,d1,d2,u1,u2,wsp1,wsp2,v1,v2,la,flae,vox,pox,kix
      real :: x_kreuz, rho=1000, grav=9.81
      integer fall, no, k
      real :: c1, c2, masx

      !print*,'flux:  c1(1), c2(1) = ',c1(1), c2(1)

      fall=0 !! Fallunterscheidung wieviel von der kante unte Wasser ist
      if( ((d1 .gt. 0.0).and.(d2 .ge. 0.0)) .or. ((d1 .ge. 0.0).and.(d2 .gt. 0.0)) )fall=1 ! Kante VOLL nass
      if((d1 .le. 0.0).and.(d2 .le. 0.0))fall=2 ! NIX Kante ganz trocken
      if((d1 .lt. 0.0).and.(d2 .gt. 0.0))fall=3 ! bottom1-trocken, top2-nass
      if((d1 .gt. 0.0).and.(d2 .lt. 0.0))fall=4 ! bottom1-nass, top2-trocken
      if(fall.eq.0) call qerror('Fall-Unterscheidung in flux() fehlgeschlagen')

            select case (fall)
            case (1) ! VOLL
               la=deltax !! Wasserspiegellänge
               flae=(deltax*(d1+d2))/2.0 !! Randfläche
               vox=(1.0/6.0)*(2.0*u1*d1 + u1*d2 + u2*d1 + 2.0*u2*d2)
               pox= (1.0/ 4.0)*(wsp1*d1*u1 + wsp2*d2*u2)   &  !! Integral(wsp*d*u)dx ; alle 3 linear über x
                  + (1.0/12.0)*(wsp1*d1*u2 + wsp1*d2*u1 + wsp2*d1*u1 + wsp1*d2*u2 + wsp2*d1*u2 + wsp2*d2*u1)
               pox=pox*rho*grav
               kix= v1*v1* ( (1.0/ 5.0)*d1*u1 + (1.0/20.0)*d1*u2 + (1.0/20.0)*d2*u1 + (1.0/30.0)*d2*u2 )  &
     &            + v1*v2* ( (1.0/10.0)*d1*u1 + (1.0/15.0)*d1*u2 + (1.0/15.0)*d2*u1 + (1.0/10.0)*d2*u2 )  &
     &            + v2*v2* ( (1.0/30.0)*d1*u1 + (1.0/20.0)*d1*u2 + (1.0/20.0)*d2*u1 + (1.0/ 5.0)*d2*u2 )
               kix=kix*(rho/2.0)
               masx= (1.0/ 4.0)*(c1*d1*u1 + c2*d2*u2)   &  !! Integral(c*d*u)dx ; alle 3 linear über x
                   + (1.0/12.0)*(c1*d1*u2 + c1*d2*u1 + c2*d1*u1 + c1*d2*u2 + c2*d1*u2 + c2*d2*u1)
            case(2) ! NIX
               la=0.0 !! Wasserspiegellänge
               flae= 0.0 !! Randfläche
               vox=0.0 
               pox=0.0
               kix=0.0
               masx=0.0
            case(3) ! bottom1-trocken-negativ, top2-nass-positiv
               !! Integral von Ufer-Punkt bis 2
               x_kreuz=d1-d2
               if(x_kreuz.eq. 0.0)call qerror("x_kreuz.eq. 0.0,flux,case (3)")
               x_kreuz=d1/x_kreuz !! relative Lage Uferpunkt (Kreuzung Sohle-Wasserspiegel) ab 1
               la=(deltax*(1.0-x_kreuz)) !! Wasserspiegellänge
               !! Wassertiefe am Uferpunkt 0
               flae=(deltax*(1.0-x_kreuz)*d2)/2.0 !! Randfläche
               !! Geschwindigkeit am Uferpunkt 0
               vox=((1.0-x_kreuz)/3.0)*(u2*d2)
               pox= (1.0/ 4.0)*(wsp2*d2*u2)   &  !! Integral(wsp*d*u)dx ; alle 3 linear über x
                  + (1.0/12.0)*( (wsp2*x_kreuz)+wsp1*(1.0-x_kreuz) )*d2*u2
               pox=(1.0-x_kreuz)*pox*rho*grav
               kix=0.0
               !! Konzentration am Uferpunkt (c2*x_kreuz)+c1*(1.0-x_kreuz)
               masx=(1.0-x_kreuz)*( (1.0/ 4.0)*c2*d2*u2 + (1.0/12.0)*((c2*x_kreuz)+c1*(1.0-x_kreuz))*d2*u2 )
            case(4) ! bottom1-nass-positiv, top2-trocken-negativ
               !! Integral von 1 bis Ufer-Punkt
               x_kreuz=d1-d2
               if(x_kreuz.eq. 0.0)call qerror("x_kreuz.eq. 0.0,flux,case (4)")
               x_kreuz=d1/x_kreuz !! relative Lage Uferpunkt (Kreuzung Sohle-Wasserspiegel) ab 1
               la=(deltax*x_kreuz) !! Wasserspiegellänge
               !! Wassertiefe am Uferpunkt 0
               flae=(deltax*(x_kreuz)*d1)/2.0 !! Randfläche
               !! Geschwindigkeit am Uferpunkt 0
               vox=(x_kreuz/3.0)*(u1*d1)
               pox= (1.0/ 4.0)*(wsp1*d1*u1)   &  !! Integral(wsp*d*u)dx ; alle 3 linear über x
                  + (1.0/12.0)*( (wsp2*x_kreuz)+wsp1*(1.0-x_kreuz) )*d1*u1
               pox=(x_kreuz)*pox*rho*grav
               kix=0.0
               !! Konzentration am Uferpunkt (c2*x_kreuz)+c1*(1.0-x_kreuz)
               masx=(x_kreuz)*( (1.0/ 4.0)*c1*d1*u1 + (1.0/12.0)*((c2*x_kreuz)+c1*(1.0-x_kreuz))*d1*u1  )
            case default
               print*,'Fall-Unterscheidung flux() fall=', fall
               call qerror("Fall-Unterscheidung fehlgeschlagen in flux()")
            end select
      RETURN
      END subroutine flux

!----+-----+----
!
!> <h1>Schnitte für die Flussberechnung lesen und prüfen</h1>
!! wird von ganglinien_lesen() aufgerufen.
!! \n\n aus schnitt.f95 , zurück: \ref Ergebnisse
!!
      logical function querschnitt_lesen()
      use modell
      implicit none
      integer :: nio, io_error, alloc_status, i, j, n, k, l, l1,l2, el(2), nel, bt, tp, nelkntp, nelknbt
      character (len=300) :: dateiname
      real :: lang

      !!!!!!!!!!!!!!!!!!! lesen !!!!!!!!!!!!!!!!
      nio=123
      print*," " 
      print*,"schnitt.txt lesen ..." 
      write(dateiname,'(2A)')trim(modellverzeichnis),'schnitt.txt'
      open ( unit =nio , file = dateiname, status ='old', action ='read ', iostat = io_error )
      if(io_error.ne.0)then
         print*,"schnitt.txt nicht vorhanden (jedenfalls lässt es sich nicht öffnen ), daher keine Querschnitte , sorry"
         querschnitt_lesen=.false.
         return
      else
         print*,"schnitt.txt vorhanden ... Flussberechnungen an Querschnitten wird eingerichtet"
         print*,"(Ausgaben auf ganglinein/q???.txt)"
      end if ! schnitt.txt lässt sich nicht öffnen

      if(.not.naechste_zeile(nio)) call qerror("Anzahl der Querschnitte in schnitt.txt nicht gefunden")
      read (ctext, * , iostat = io_error) anzahl_quer
      if(io_error.eq.0) then
         print*,"schnitt.txt - Anzahl der Querschnitte: ", anzahl_quer
      else
         call qerror("schnitt.txt anzahl_quer nicht lesbar")
      end if !anzahl_quer lesbar

      allocate (querschnitt(anzahl_quer), stat = alloc_status )
      ! type(qusch) , allocatable , dimension (:) :: querschnitt
      if(alloc_status.ne.0) then
         write(fehler,*)'allocate (querschnitt(anzahl_quer) fehlgeschlagen'
         call qerror(fehler)
      end if ! alloc_status .ne.0

      do i=1,anzahl_quer
         if(.not.naechste_zeile(nio)) then
            write(fehler,*)"Querschnitt #",i," Knotenanzahl nicht auffindbar"
            call qerror(fehler)
         end if ! keine nächste Zeile
         read (ctext, * , iostat = io_error) querschnitt(i)%schnittlinie%anzkanten
         if(io_error.eq.0) then
            print*,"schnitt #",i," soll ", querschnitt(i)%schnittlinie%anzkanten," Knoten enthalten"
         else
            print*,"schnitt #",i
            call qerror("Lesefehler schnitt.txt Knotenanzahl")
         end if ! lesen der knotenanzahl des Schnitts hat geklappt
         if(querschnitt(i)%schnittlinie%anzkanten.le.1)call qerror("Quer-Schnitte mit weniger als 2 Knoten machen keinen Sinn!")
         querschnitt(i)%schnittlinie%anzkanten=querschnitt(i)%schnittlinie%anzkanten-1
         allocate (querschnitt(i)%schnittlinie%kante(querschnitt(i)%schnittlinie%anzkanten), stat = alloc_status )
         allocate (querschnitt(i)%schnittlinie%knoten(querschnitt(i)%schnittlinie%anzkanten+1), stat = alloc_status )
         do j=1,querschnitt(i)%schnittlinie%anzkanten+1 !! alle j Knoten
            if(.not.naechste_zeile(nio)) call qerror("Lesefehler schnitt.txt Knotennummer")
            read (ctext, * , iostat = io_error) querschnitt(i)%schnittlinie%knoten(j)
            if(io_error.eq.0) then
               print*,"schnitt #",i," knoten ",j, " #",querschnitt(i)%schnittlinie%knoten(j)
            else
               print*,"schnitt #",i," knoten ",j
               call qerror("Lesefehler schnitt.txt Knotennummer")
            end if ! Lesen Knotennummer hat geklappt
         end do ! alle j Knoten in Querschnitt i
      end do ! alle i Querschnitte
      ! Ganglinen allocieren
      allocate (schnittflux_gang(anzahl_quer,zeitschrittanzahl+1, 6 ), stat = alloc_status )
      if(alloc_status.ne.0) then
         write(fehler,*)'allocate (schnittflux_gang(anzahl_quer, fehlgeschlagen'
         call qerror(fehler)
      end if ! alloc_status .ne.0

      rewind (nio)
      close (nio)

      !!!!!!!!!!!!!!!!!!! prüfen !!!!!!!!!!!!!!!!
      do i=1,anzahl_quer !! alle i Querschnitte
         if( knoten_rand(querschnitt(i)%schnittlinie%knoten(1)).le.0)  &
     &      print*,"Erster Knoten in Querschnitt ",i," kein Randknoten"
         if( knoten_rand(querschnitt(i)%schnittlinie%knoten(querschnitt(i)%schnittlinie%anzkanten+1)).le.0)  &
     &      print*,"Letzter Knoten in Querschnitt ",i," kein Randknoten"
         if(querschnitt(i)%schnittlinie%knoten(1).eq.querschnitt(i)%schnittlinie%knoten(querschnitt(i)%schnittlinie%anzkanten+1))  &
     &      print*,"Querschnitt ",i," offensichtlich geschlossene Linie"
         lang=0.0
         do j=1,querschnitt(i)%schnittlinie%anzkanten !! alle j Kanten 
            bt=querschnitt(i)%schnittlinie%knoten(j)
            tp=querschnitt(i)%schnittlinie%knoten(j+1)
            print*," querschnitt_lesen Kante ",j," top,bottom=",tp,bt
            nel=0 ; nelkntp =0 ; nelknbt =0
            do n=1,n_elemente ! alle n Elemente
               do l=1,cornernumber(n)
			      if(elementnodes(n,l).eq.tp)then
   				     nelkntp=nelkntp+1 ;print*,tp,'=tp an Element=',n,l,"ter Knoten"
				  endif
			      if(elementnodes(n,l).eq.bt)then
				     nelknbt=nelknbt+1 ;print*,bt,'=bt an Element=',n,l,"ter Knoten"
				  endif
                  if(l.eq.1)then
                     l1=cornernumber(n)
                  else
                     l1=l-1
                  end if !! erste ecke
                  if((bt.eq.elementnodes(n,l1)).and.(tp.eq.elementnodes(n,l)))then
                    nel=nel+1
                    if(nel.ge.3)call qerror("eine Kante kann nicht in 3 elementen vorkommen")
                    el(nel)=n
                  end if ! kante bt-tp erkannt
                  if((tp.eq.elementnodes(n,l1)).and.(bt.eq.elementnodes(n,l)))then
                    nel=nel+1
                    if(nel.ge.3)call qerror("eine Kante kann nicht in 3 elementen vorkommen")
                    el(nel)=n
                  end if ! kante bt-tp erkannt
               end do ! alle l (3 oder 4) Ecken von Element k
            end do ! alle n Elemente im Netz
            print*,"An Kante ",j," bottom= ",bt," top= ",tp, " von Querschnitt #",i," wurden ", nel ," Elemente gezählt"
            print*,nelkntp," elemente an top ",nelknbt," elemente an bottom"
            if(nel.eq.2)then
               print*," Element 1 =",el(1)," Element 2 =",el(2)
            else
               call qerror("Querschnittskante nicht innerhalb des Berechnungsgebiets")
            endif
            querschnitt(i)%schnittlinie%kante(j)%normal_x=-1*(knoten_y(tp)-knoten_y(bt))
            querschnitt(i)%schnittlinie%kante(j)%normal_y=(knoten_x(tp)-knoten_x(bt))
            querschnitt(i)%schnittlinie%kante(j)%bottom=bt
            querschnitt(i)%schnittlinie%kante(j)%top=tp
            lang=lang + ( (querschnitt(i)%schnittlinie%kante(j)%normal_x**2)  &
    &                   + (querschnitt(i)%schnittlinie%kante(j)%normal_y**2) )**0.5
         end do ! alle j Kanten in Querschnitt i
         print*,"Querschnitt #",i, "hat ",querschnitt(i)%schnittlinie%anzkanten," Kanten und ist ",lang," lang"
      end do ! alle i Querschnitte

      querschnitt_lesen=.true.
      RETURN
      END function querschnitt_lesen
