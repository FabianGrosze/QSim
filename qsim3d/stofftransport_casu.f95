
!> Subroutine stofftransport_casu()
!! Beschreibung in \ref Transport_casu
      SUBROUTINE stofftransport_casu()
      use modell                                                   
      implicit none
      integer :: i, wrong, n, j, as, l, ieck(4), nullzae, trockzae
      integer :: nt, stranglang
      real , allocatable , dimension (:,:) :: zwischen
      real :: summ, tiwicht(4), anteil, tracervolumen, tief
      logical :: negativ(number_plankt_vari)
!
      !! ###print*,'stofftransport zu Testzwecken überbrückt, keine Advektion' !! ###
      !! ###return !! ###

      if(meinrang.ne.0)call qerror('stofftransport_casu darf nur von prozessor 0 aufgerufen werden')
      !stationaer=.true. !! wird in TQsim.f95 gesetzt; erprobung einde
      if(stationaer)then
         write(fehler,*)'stationaer so noch nicht vorgesehen (stofftransport.f95)'
         call qerror(fehler)
      end if ! stationaer

      call transinfo_schritte(startzeitpunkt, endzeitpunkt)

      print*,'stofftransport: von Zeitschrittt ... bis', na_transinfo, ne_transinfo
      do nt=na_transinfo, ne_transinfo ! alle Transport (zwischen) Zeitschritte

         call holen_trans(nt)
         !print*,'stofftransport: Zeit=',transinfo_zeit(transinfo_zuord(nt)), &
         !          ' Datei: ',trim(transinfo_datei(transinfo_zuord(nt)))

   !     vorläufige Advektion (noch nicht parallelisiert)
         allocate (zwischen(number_plankt_vari, number_plankt_point), stat = as )
         do n=1,number_plankt_vari !! 
            negativ(n)=.false.
         end do ! alle planktischen variablen
         nullzae=0
         trockzae=0
         do j=1,number_plankt_point ! alle j Knoten
            tief=rb_hydraul(2+(j-1)*number_rb_hydraul)
            do n=1,number_plankt_vari !! Kontrolle auf negative Werte vorab:
               if(planktonic_variable(n+(j-1)*number_plankt_vari).lt. 0.0)then
                  negativ(n)=.true.
                  !write(7,*)'An Knoten #',j,' hat planktonic_variable #',n,' i.e. '  &
                  !      ,planktonic_variable_name( n),' den Wert=' &
                  !      ,planktonic_variable(n+(j-1)*number_plankt_vari)
               endif
               if(isNaN( planktonic_variable(n+(j-1)*number_plankt_vari) ))  &
                  print*,'isNaN_planktonic_variable plankt_vari=',n,' plankt_point=',j
            end do ! alle variablen

            do l=1,4
               ieck(l)=intereck((j-1)*4+l)
               if(isNaN( wicht((j-1)*4+l) ))print*,'isNaN_wicht,j,l,intereck',wicht((j-1)*4+l),j,l,intereck((j-1)*4+l)
           end do ! alle 4 wichtungsfaktoren

            do l=1,3
               !1! tiwicht(l)=wicht(nm*4+l)
               !2! tiwicht(l)=wicht(nm*4+l)*tief(ieck(l))
               tiwicht(l)=wicht((j-1)*4+l) * rb_hydraul(2+(ieck(l)-1)*number_rb_hydraul) !  neu31jan14 * tief(ieck(l))
               if( rb_hydraul(2+(ieck(l)-1)*number_rb_hydraul).le. min_tief)tiwicht(l)=0.0  
            end do ! alle 3 wichtungsfaktoren
            if(ieck(4).gt.0)then !! nur bei vierecken 4.Knoten
               !1! tiwicht(4)=wicht(nm*4+4)
               !2! tiwicht(4)=wicht(nm*4+4)*tief(ieck(4))
               tiwicht(4)=wicht((j-1)*4+4)  * rb_hydraul(2+(ieck(4)-1)*number_rb_hydraul) ! ! neu31jan14 * tief(ieck(l))
               if(rb_hydraul(2+(ieck(4)-1)*number_rb_hydraul).le. min_tief) tiwicht(4)=0.0  !4! 
            else  !3!
               tiwicht(4)=0.0  !3!
            end if

            summ=0.0
            do l=1,4
               summ=summ+tiwicht(l)
            end do ! alle 4 wichtungsfaktoren
            if(summ.gt. 0.0)then
               do l=1,4
                  tiwicht(l)=tiwicht(l)/summ
               end do ! alle 4 wichtungsfaktoren
            else ! summ=0 konzentration bleibt liegen !
               !do l=1,4 !! Versuch 30jul15 wy
               !   tiwicht(l)=0.25
               !   ieck(l)=j
               !end do ! alle 4 wichtungsfaktoren
               nullzae=nullzae+1
           end if ! summ.gt.0
           if(tief .le. min_tief) then
               trockzae=trockzae+1
           end if ! trocken
           if(j .eq. kontrollknoten)print*,'ieck,l=1,4',(ieck(l),l=1,4),'tiwicht,l=1,4',(tiwicht(l),l=1,4), summ

           if((summ .le. 0.0).or.(tief.le. min_tief))then ! summ=0 oder alles trocken?, Wert bleibt | 19jul17 wy
              do n=1,number_plankt_vari
                  zwischen(n,j)=planktonic_variable(n+(j-1)*number_plankt_vari)
              end do ! alle n Konzentrationen
           else ! summ!=0 und nass?
              do n=1,number_plankt_vari
                  zwischen(n,j)=planktonic_variable(n+(ieck(1)-1)*number_plankt_vari)*tiwicht(1) +  &
                              planktonic_variable(n+(ieck(2)-1)*number_plankt_vari)*tiwicht(2) +  &
                              planktonic_variable(n+(ieck(3)-1)*number_plankt_vari)*tiwicht(3)
                  if(ieck(4).gt.0)then !! nur bei vierecken 4.Knoten
                     zwischen(n,j)=zwischen(n,j) + planktonic_variable(n+(ieck(4)-1)*number_plankt_vari)*tiwicht(4)
                  end if
                  if(isNaN(zwischen(n,j)))then
                     print*,'stofftransport isNaN(zwischen(',n,',',j,')), summ',zwischen(n,j),summ
                     print*,'ieck,l=1,4',(ieck(l),l=1,4)
                     print*,'tiwicht,l=1,4',(tiwicht(l),l=1,4)
                     print*,'wicht,l=1,4',(wicht((j-1)*4+l),l=1,4)
                     print*,'tief,l=1,3',(rb_hydraul(2+(ieck(l)-1)*number_rb_hydraul),l=1,3)
                     if(ieck(4).gt.0)print*,'tief(ieck(4)=',rb_hydraul(2+(ieck(4)-1)*number_rb_hydraul) ! nur bei vierecken 4.Knoten
                     print*,'(planktonic_variable(n+(ieck(l)-1)*number_plankt_vari),l=1,3)=',  &
     &                      (planktonic_variable(n+(ieck(l)-1)*number_plankt_vari),l=1,3)
                     if(ieck(4).gt.0)print*,'planktonic_variable(n+(ieck(4)=',planktonic_variable(n+(ieck(4)-1)*number_plankt_vari) ! nur bei vierecken 4.Knoten
                     print*,'planktonic_variable_name',n, planktonic_variable_name(n)
                     write(fehler,*)'stofftransport: isNaN(zwischen planktonic_variable_name',planktonic_variable_name(n)
                     call qerror(fehler)
                  endif ! isNaN
               end do ! alle n Konzentrationen
            endif ! summ=0 alles trocken?, Wert bleibt
         end do ! alle j Knoten

         do j=1,number_plankt_point ! alle j Knoten
            !if(j .eq. kontrollknoten)then ! Ausgabe kontrollknoten
            !   print*,'kontrollknoten vor Stofftransport lf=',planktonic_variable(65+(j-1)*number_plankt_vari)
            !end if ! kontrollknoten
            do n=1,number_plankt_vari
               planktonic_variable(n+(j-1)*number_plankt_vari)=zwischen(n,j)
            end do ! alle n Konzentrationen
            !if(j .eq. kontrollknoten)then ! Ausgabe kontrollknoten
            !   print*,'kontrollknoten nach Stofftransport lf=',planktonic_variable(65+(j-1)*number_plankt_vari)
            !end if ! kontrollknoten
         end do ! alle j Knoten
         deallocate (zwischen, stat = as )

         do n=1,number_plankt_vari !! 
            if(negativ(n))print*,'stofftransport negativ, planktonic_variable #',n,' ',planktonic_variable_name(n)
         end do ! alle planktischen variablen
         if(nullzae.gt. 0)print*,'stofftransport_casu: an ',nullzae,' Berechnungspunkten war die Wichtungssumme Null ?!'
         if(trockzae.gt. 0)print*,'stofftransport_casu: ',trockzae,' Berechnungspunkte waren trocken'

         tracervolumen=0.0
         do j=1,number_plankt_point ! alle j Knoten
            tief=rb_hydraul(2+(j-1)*number_rb_hydraul)
            if(tief.ge. min_tief) then !nass
               tracervolumen=tracervolumen+( tief*knoten_flaeche(j)*planktonic_variable(71+(j-1)*number_plankt_vari) )
            end if !nass
         end do ! alle j Knoten
         print*,'tracervolumen=', transinfo_zeit(transinfo_zuord(nt)), tracervolumen

         !!if((kontrollknoten.gt.0).and.(kontrollknoten.le.number_plankt_point))then ! Ausgabe
         !!   print*,' nt= ', nt  &
         !!   ,' T_wass=', planktonic_variable(1*kontrollknoten) &
         !!   ,' Tracer=', planktonic_variable(71*kontrollknoten)
         !!end if
         !!print*,'transport nt=',nt,' start ende= ',startzeitpunkt, endzeitpunkt

    end do ! nt, alle Transport (zwischen) Zeitschritte

      RETURN
      END subroutine stofftransport_casu

!----+-----+----
!
!> \page Transport_casu Transportinformationen von casu (Strombahnursprünge)
!! Die Transportinformationen werden von holen_trans() in die \subpage hydraul_rb eingelesen.\n
!! Transportinformationen für den Stofftransport bestehen aus den
!! Ecknummern der vier(drei) Knoten des Elements aus dem die Strombahn kommt und je
!! 4(3) Wichtungsfaktoren, die angegeben, welcher Anteil vom dazugehörenden Knoten kommt. 
!! Die Summe der Wichtungsfaktoren muss immer genau 1.0 sein.\n
!! Dies entspricht der Angabe des Strombahnursprungs (in einem Elementlokalen Koordinatensystem) 
!! und konstruiert auf diese Weise eine Euler-Lagrange Methode (ELM) für die Advektion.\n
!! \f[ 
!! C^{n+1}_k = {\sum_{i=1}^{3(4)}} C_{Nr(i)}^n \cdot  w(i)
!! \f]
!! Bisher ist nur Advektion realisiert worden, die Diffusion steht bisher noch aus.
!! Es ist geplant, dies im Zusammenhang mit der Messung und Kalibrierung der realen Diffusivitäten
!! und der Ermittlung der numerischen Diffusivität durchzuführen.\n
!! mit <table >
!!<tr><th> Formelzeichen </th><th> Variablen-Name </th><th> Beschreibung </th><th> Dimension </th><th> Wertebereich </th></tr>
!!<tr><td> \f$ C \f$  </td><td> planktonic_variable \ref tiefengemittelte_planktische_variable </td><td> als Konzentration modellierte planktische Variable </td><td>  </td><td>  </td></tr>
!!<tr><td> \f$ i \f$  </td><td> - </td><td> Zähler über alle Ecken des Elements </td><td> - </td><td> 1-3(4) </td></tr>
!!<tr><td> \f$ n \f$  </td><td> - </td><td> Zeitschritt-Zähler </td><td> - </td><td> - </td></tr>
!!<tr><td> \f$ k \f$  </td><td> - </td><td> Konzentrations-Nummer </td><td> - </td><td> - </td></tr>
!!<tr><td> \f$ Nr(i) \f$  </td><td> ieck() </td><td> globle Knotennummer der Elementecke </td><td> - </td><td> </td></tr>
!!<tr><td> \f$ w(i) \f$  </td><td> wicht() </td><td> Wichtungsfaktor (d.h. elementlokale Koordinaten des Strombahnursprungs im Ursprungselement) </td><td> - </td><td> 0-1 </td></tr>
!!</table>\n
!! \n\n
!! Es wurde die Möglichkeit geschaffen, durch \subpage kombitransi die hydraulischen Voraussetzungen für die Gütesimulation abschnittsweise zu simulieren. 
!! D. h.: Der Hydraulische Treiber rechnet das Jahr z. B. monatsweise in 12 parallelen Rechenläufen (mit wenigen Tagen Überlappung), 
!! deren Ergebnisse dann im nachhinein zu einem kontinuerlichen Datensatz der Transportinformationen kombiniert werde.
!! \n\n
!! Zum Lesen des Netzes aus dem Modellverzeichnis bedient sich netz_lesen() der Subroutinen points(), elements() und edges()
!! \n\n
!! aus Datei stofftransport_casu.f95; zurück zu \ref Datentechnik oder \ref Transportinformationen


!> subroutine holen_trans(nt) , mit nt - Zeitschrittzähler, 
!! holt die Transportinformationen für einen Zeitschritt mittels der c++-Funktion trans_read() aus Datei trans_read.c.\n
!! In den Dateien aus dem Verzeichnis transinfo, deren Name mit t beginnt und danach den Zeitpunkt als Zahl enthält,
!! ist der Strombahnursprung, die Wasserspiegellage und der Geschwindigkeitsbetrag abgelegt.\n
!! holen_trans() macht daraus die \ref hydraul_rb Geschwindigkeitsbetrag, Wassertiefe und Wasserspiegellage.\n
!! holen_trans() wird nur aus stofftransport() heraus von Prozess 0 aufgerufen
!! \n\n aus Datei stofftransport.f95; zurück:\ref Transportinformationen
      subroutine holen_trans(nt)
      use modell
      implicit none
      integer :: nt, ntist, trockzae
      integer :: wrong, lang, j,i,jj,ll
      real :: ubetr, utau, infl, flaeche, volumen, anteil, ks, zet, tief
      character(500) vollerdateiname

      trockzae=0
      if(stationaer)then
         ntist=1
      else !! instationaer
         ntist=nt
      end if
      write(vollerdateiname,'(3A)')trim(modellverzeichnis),'transinfo/',trim(transinfo_datei(transinfo_zuord(ntist)))
      ! lang=len(trim(transinfo_datei(transinfo_zuord(nt))))
      lang=len(trim(vollerdateiname))

      !if((.not. stationaer).or.(nt.eq.1))then !! instationär oder erster Schritt stationär
         call trans_read(trim(vollerdateiname), lang, &
                         nonu, intereck, wicht, wrong, p, u, dir, kontrollknoten)
         if(wrong.ne.0)then
            write(fehler,*)' trans_read Lesen der Transportinformationen fehlgeschlagen ', wrong
            call qerror(fehler)
         end if
         if(nonu.ne.number_plankt_point)then
             write(fehler,*)'holen_trans: nonu.ne.number_plankt_point',nonu,number_plankt_point
             call qerror(fehler)
         endif ! nonu
         do jj=1,number_plankt_point ! alle j Knoten
            do ll=1,4
               if(isNaN( wicht((jj-1)*4+ll) ))print*,'holen_trans: isNaN( wicht((jj-1)*4+ll)  jj,ll,((jj-1)*4+ll) =' &
     &                                              ,jj,ll,((jj-1)*4+ll)
            end do ! alle 4 wichtungsfaktoren
         end do ! all jj nodes

         flaeche=0.0
         volumen=0.0
         anteil=1.0/real(zeitschrittanzahl*anz_transinfo) !! Zeit-Anteil am Gesamt-Simulations-Zeitraum
         do j=1,nonu !! 2D-Knoten
            tief=p(j)-knoten_z(j)  ! Wassertiefe ermitteln:
            if(tief.le. min_tief )then ! trocken ! min_tief parameter aus module_modell
               trockzae=trockzae+1
            else ! nass
               flaeche=flaeche+knoten_flaeche(j)
               volumen=volumen+(tief*knoten_flaeche(j))
               if(uedau_flag) call qerror(" holen_trans(nt) Überstaudauer nicht mehr implementiert")
            endif ! trocken
         end do ! alle j Knoten
         mittelflaech=mittelflaech+(flaeche*anteil)
         mittelvolumen=mittelvolumen+(volumen*anteil)

         ! Bahnlinienursprünge ermitteln + 
         ! Zuflussränder detektieren: d.h. Rand an dem Geschwindigkeit ohne Bahnlinielänge
         do j=1,nonu ! all nodes
            inflow(j)=.false.
            ur_x(j)=0.0
            ur_y(j)=0.0
            ur_z(j)=0.0
            do i=1,4
               if(intereck((j-1)*4+i).gt.0)then !! nur bei vierecken 4.Knoten
                  ur_x(j)=ur_x(j)+knoten_x(intereck((j-1)*4+i))*wicht((j-1)*4+i)
                  ur_y(j)=ur_y(j)+knoten_y(intereck((j-1)*4+i))*wicht((j-1)*4+i)
                  ur_z(j)=ur_z(j)+knoten_z(intereck((j-1)*4+i))*wicht((j-1)*4+i)
               end if
            end do ! alle 4 ecken
            ubetr=(((knoten_x(j)-ur_x(j))**2 + (knoten_y(j)-ur_y(j))**2 + (knoten_z(j)-ur_z(j))**2)**0.5)/dttrans
            infl=10.0
            if(u(j).gt. 0.0)infl=ubetr/u(j)
            if((infl.lt. 0.1).and. (knoten_rand(j).gt. 0))then
               inflow(j)=.true.
               !print*,'Zuflussknoten ',j
            endif

            !! Sohlschubspannungsgeschwindigkeit berechnen (Nikuradse-Sandrauh Darcy-Weißbach)
            !!lami=0.1 !! ### vorläufig =zone(point_zone(j))%reib_ks reib_ks(point_zone(j))
            if(tief.le. min_tief )then ! trocken ! min_tief parameter aus module_modell
               utau= 0.0
            else ! nass
               ks=zone(point_zone(j))%reib
               zet=tief*0.4665; !! 2D
               utau=((lambda(ks,zet)/8.0)**0.5)*u(j)
            endif

!!if(meinrang.eq.0)then !! nur prozessor 0
      !!do j=1,knotenanzahl2D
            rb_hydraul(1+(j-1)*number_rb_hydraul)    = u(j)
            rb_hydraul(2+(j-1)*number_rb_hydraul)    = tief
            rb_hydraul(3+(j-1)*number_rb_hydraul)    = p(j)
            !! benthic_distribution(44+(j-1)*number_benth_distr)=ks ! da sollte eigentlich der strickler-Wert stehen
            benthic_distribution(45+(j-1)*number_benth_distr)=utau
      !!end do !! alle j knoten
!!end if !! nur prozessor 0

            if(j.eq.kontrollknoten)print*,'holen_trans: ', j,' p=',p(j),' u=', u(j), ' tief=',tief,   &
             " utau=",utau," Ks=",zone(point_zone(j))%reib,' knoten_lage=',knoten_x(j),knoten_y(j),knoten_z(j),  &
             ' ursprung',ur_x(j),ur_y(j),ur_z(j), ' inflow',inflow(j), ' ubetr', ubetr,' nt=',nt
         end do ! all j nodes

      print*,'Transport mit Datei ',trim(vollerdateiname),' nt=',nt,' Wasserpiegelflaeche=',flaeche  &
            ,' Wasservolumen=',volumen,' Anzahl trockene Knoten=',trockzae

      return
      END subroutine holen_trans
!----+-----+----

!> für einen gegebenes Zeitintervall ermitteln welche Transportinformationsschritte es abdecken.
!! \n\n
      subroutine transinfo_schritte(start_zeitschritt, ende_zeitschritt)
      use modell
      implicit none
      integer :: start_zeitschritt, ende_zeitschritt
      integer :: n, deti
      logical :: gefunden

      if(ende_zeitschritt.lt.start_zeitschritt)then
         write(fehler,*)'transinfo_schritte: Zeitintervall ' &
     &         ,start_zeitschritt, ' bis ', ende_zeitschritt,' geht nicht'
         call qerror(fehler)
      end if

      gefunden=.false.
      na_transinfo=-1
      ne_transinfo=-1
      do n=1,transinfo_anzahl
         if((transinfo_zeit(transinfo_zuord(n)).gt.start_zeitschritt).and.(.not.gefunden))then
            na_transinfo=n
            gefunden=.true.
            !print*,'na_transinfo,n ',na_transinfo,n
            !print*,'Strat Hydraulik Zeitschritt in diesem Qualitäts-Zeitschritt ', transinfo_zeit(transinfo_zuord(n))
         end if
      end do ! alle transportinfo Zeitpunkte
      if(stationaer)then
         na_transinfo=1
         gefunden=.true.
      end if
      if(.not.gefunden)then
         write(fehler,*)'Zum Anfangszeitpunkt des abgefragten Zeitintervalls existiert keine passende transportinfo Datei ' &
     &         ,start_zeitschritt
         call qerror(fehler)
      end if
      do n=1,transinfo_anzahl
         if((transinfo_zeit(transinfo_zuord(n)).gt.start_zeitschritt).and. &
            (transinfo_zeit(transinfo_zuord(n)).le.ende_zeitschritt ))then
            !print*,'weiterer Hydraulik Zeitschritt in diesem Qualitäts-Zeitschritt ', transinfo_zeit(transinfo_zuord(n))
            ne_transinfo=n
         end if
      end do ! alle transportinfo Zeitpunkte
      if(stationaer)then
         !ne_transinfo=3 !! einde: 3*300s casu-zeitschirtt auf 1*900s tiqu-zeitschritt ######
         !ne_transinfo=6 !! einde: 6*150s casu-zeitschirtt auf 1*900s tiqu-zeitschritt ######
         !ne_transinfo=1 !! einde: 1*900s casu-zeitschirtt auf 1*900s tiqu-zeitschritt ######
         ne_transinfo=deltat/dttrans
         gefunden=.true.
      end if
      if(ne_transinfo.lt.na_transinfo) ne_transinfo=na_transinfo
      !print*,'start_zeitschritt,ende_zeitschritt ',start_zeitschritt,ende_zeitschritt
      !print*,'ne_transinfo ',ne_transinfo 
      !print*,'Ende Zeitschritt ', transinfo_zeit(transinfo_zuord(ne_transinfo))
      anz_transinfo=1+ne_transinfo-na_transinfo
      if(stationaer)then
         deti=dttrans*anz_transinfo
      else !instationär
         deti=transinfo_zeit(transinfo_zuord(ne_transinfo))-transinfo_zeit(transinfo_zuord(na_transinfo))
         deti=deti+dttrans
      end if
      if(deti.ne.deltat)then
         print*,'Zeitschrittweiten Transport=',dttrans,' - Güte=',deltat,' passen nicht zueinander.' 
         print*,'ganzzahlige Vielfache erforderlich.'
         print*,'deti=',deti
         print*,'anfang ',na_transinfo, transinfo_zuord(na_transinfo), transinfo_zeit(transinfo_zuord(na_transinfo))
         print*,'  ende ',ne_transinfo, transinfo_zuord(ne_transinfo), transinfo_zeit(transinfo_zuord(ne_transinfo))
         if(hydro_trieb.eq.1)call qerror('deti.ne.deltat')!! nur bei casu-Strombahnen abbrechen
      end if

      print*," Für den Transport im Gütezeitschritt von ",start_zeitschritt," bis ", ende_zeitschritt  &
     &      ," werden ", anz_transinfo," Transportzeitschritte verwendet."
      !do n=1,anz_transinfo
      !   print*,"#",na_transinfo+n-1," Zeitpunkt=",transinfo_zeit(transinfo_zuord(na_transinfo+n-1))
      !end do
      return
      END subroutine transinfo_schritte
!----+-----+----

!> Transportinformationen sichten und sortieren
!! \n\n
      subroutine transinfo_sichten() 
      use modell   
      implicit none
      character(200) irgendeinstring
      character(len=longname) :: dateiname, systemaufruf
      integer :: system_error, open_error, alloc_status, io_error, errcode
      integer :: n, nt, nz, np, i, ion, zwischenwert, delt, didi
      logical :: offsetvorhanden

      print*,'Transportinformationen casu-transinfo sichten ...'
      write(dateiname,'(2A)',iostat = errcode)trim(modellverzeichnis),'trafo'
      if(errcode .ne. 0)call qerror('transinfo_sichten writing filename elemente_ failed')
      write(systemaufruf,'(4A)',iostat = errcode)'ls ',trim(modellverzeichnis),'transinfo >', trim(dateiname)
      if(errcode .ne. 0)call qerror('transinfo_sichten writing filename elemente_ failed')
      call system(trim(systemaufruf),system_error)
      if(system_error.ne.0) then
         print*,trim(systemaufruf)
         call qerror('Auflisten der Transportinformationen fehlgeschlagen.')
      end if ! io_error.ne.0
      ion=333
      open ( unit =ion , file = dateiname, status ='old', action ='read', iostat = open_error )
      if(open_error.ne.0)then
         call qerror('open_error trafo')
      !else
      !   print*,"trafo offen"
      end if
      nz=0
      nt=0
      do while (zeile(ion))
         !print*,trim(ctext)
         nz=nz+1
         if(ctext(1:1).eq.'t')nt=nt+1
      end do ! while Zeile
      !print*,'trafo hat ',nt," Zeilen"
      transinfo_anzahl=nt
      if(transinfo_anzahl.lt. 1)call qerror('No transport info')
      rewind(ion)
      allocate (transinfo_zeit(transinfo_anzahl), stat = alloc_status )
      allocate (transinfo_datei(transinfo_anzahl), stat = alloc_status )
      allocate (transinfo_zuord(transinfo_anzahl), stat = alloc_status )
      nt=0
      np=0
      do n=1,nz,1
         if(.not.zeile(ion))call qerror('2 .not.zeile(ion)')
         !write(*,*)trim(ctext)
         if(ctext(1:1).eq.'t')then
            nt=nt+1
            write(transinfo_datei(nt),'(A)')trim(ctext)
            i=len(trim(ctext))
            do while (ctext(i:i).ne.'t')
               i=i-1
            end do ! while Zeile
            write(irgendeinstring,'(A)')ctext(i+1:len(trim(ctext)))
            !print*,'irgendeinstring:',trim(irgendeinstring)
            read(irgendeinstring,*)transinfo_zeit(nt)
            transinfo_zuord(nt)=nt
            !print*,"transinfo   zuord=", transinfo_zuord(n), '  transinfo_zeit=',transinfo_zeit(n), &
            !       '  Datei:', trim(transinfo_datei(n))
         end if !! alle t* Dateien
      end do ! alle zeilen aus trafo
      close(ion)
      write(dateiname,'(2A)')trim(modellverzeichnis),'transinfo/meta'
      open ( unit =ion , file = dateiname, status ='old', action ='read', iostat = open_error )
      if(open_error.ne.0)call qerror('open_error transinfo/meta')
      offsetvorhanden=.false.
      do while (zeile(ion))
         if (ctext(1:1).eq.'#') then !! Infos zum Modell
            print*,trim(ctext)
         else
            read(ctext,*,iostat=io_error) dttrans
            if(io_error.ne.0) then
               write(fehler,*)'Transportinfo-Zeitschritt in /transinfo/meta nicht lesbar',io_error
               call qerror(fehler)
            endif
            if(.not.zeile(ion))then
               write(fehler,*)'2. Datenzeile in ', trim(dateiname),' nicht lesbar'
               call qerror(fehler)
            endif
            if(.not.offsetvorhanden)then
               !read(ctext,*,iostat=io_error)time_offset
               read(ctext,*,iostat=io_error) tag, monat, jahr, stunde, minute, sekunde
               if(io_error.ne.0) then
                  write(fehler,*)'time_offset-Lesefehler in der Datei ', trim(dateiname)
                  call qerror(fehler)
               else
                  print*,"meta-zeit=",tag, monat, jahr, stunde, minute, sekunde
                  offsetvorhanden=.true.
                  time_offset=0
                  call sekundenzeit(1)
                  print*,"meta-sekundenzeit=",zeitpunkt,tag, monat, jahr, stunde, minute, sekunde
                  !call zeitsekunde() !! damit auch die Uhrzeit stimmt
                  write(*,227)'time-offset(transportinfo/meta) '  &
                              ,tag,monat,jahr,stunde,minute,sekunde,zeitpunkt,referenzjahr
                  time_offset=zeitpunkt !! Offset vom Referenzjahr zum transinfo/meta Zeitursprung
                  write(time_offset_string,'(I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," Uhr")')  &
                        tag,monat,jahr,stunde,minute,sekunde
               end if ! io_error.ne.0
            end if ! not vorhanden
         end if ! not #
      end do ! Zeile in /transinfo/meta'
      if(.not.offsetvorhanden) then
         write(fehler,*)'Fehler in der Datei ', trim(dateiname), ' beim Lesen der Verschiebung', &
                ' der Zeitskalen zwischen Hydraulik und Qualitätsmodell. '
         call qerror(fehler)
      end if ! offset nicht vorhanden
      ! write(*,'(A,2x,F15.2,2x,I9)')'time-offset=',real(time_offset),time_offset
      do n=1,transinfo_anzahl,1
         do i=n+1,transinfo_anzahl,1
            if(transinfo_zeit(transinfo_zuord(n)).gt.transinfo_zeit(transinfo_zuord(i)))then ! tauschen
               zwischenwert=transinfo_zuord(n)
               transinfo_zuord(n)=transinfo_zuord(i)
               transinfo_zuord(i)=zwischenwert
            end if ! Zeitreihenfolge falsch
         end do ! alle weiteren i durch
      end do ! alle n durch
      !do n=1,transinfo_anzahl,1
      !    transinfo_zeit(transinfo_zuord(n))=transinfo_zeit(transinfo_zuord(n))+real(time_offset)
      !end do ! alle n durch
      !do n=1,transinfo_anzahl,1
      !   transinfo_zeit(transinfo_zuord(n))=time_offset+transinfo_zeit(transinfo_zuord(n))
      !end do ! alle n durch

      !dttrans=1 !! jetzt aus meta lesen!
      !if(transinfo_anzahl.gt.1)then
      !   dttrans=transinfo_zeit(transinfo_zuord(2))-transinfo_zeit(transinfo_zuord(1))
      !   print*,'Transport Zeitschrittweite=',dttrans
      !end if ! mehr als ein Transportzeitschritt
      do n=2,transinfo_anzahl,1
         delt=transinfo_zeit(transinfo_zuord(n))-transinfo_zeit(transinfo_zuord(n-1))
         if(delt.ne.dttrans)then
            write(fehler,*)' ERROR unregelmäßiger Transportzeitschritt ',delt, 'sollte sein: ', dttrans &
                  ,' n=', n,trim(transinfo_datei(transinfo_zuord(n)))
            call qerror(fehler)
         end if ! mehr als ein Transportzeitschritt
      end do ! alle Transportzeitschritte ab 2

      print*, transinfo_anzahl,' Transport-Zeitschritte'
      zeitpunkt=transinfo_zeit(transinfo_zuord(1))
      call zeitsekunde()
      write(*,228)'von: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt, trim(time_offset_string),  &
                  trim(transinfo_datei(transinfo_zuord(1)))
      zeitpunkt=transinfo_zeit(transinfo_zuord(transinfo_anzahl))
      call zeitsekunde()
      write(*,228)'bis: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt, trim(time_offset_string),  &
                  trim(transinfo_datei(transinfo_zuord(transinfo_anzahl)))
      !print*,' transinfo_sichten rechenzeit=', rechenzeit, ' startzeitpunkt=',startzeitpunkt
      print*,'in regelmäßigen Schritten von  ',dttrans, ' Sekunden'

      return
  227 FORMAT (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," Uhr  = ",I9," sek. seit 1.Jan.",I4)
  228 FORMAT (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," Uhr  = ",I9," sek. seit ",A,2x,A)
      END subroutine transinfo_sichten
!----+-----+----
!
!
!! in der Subroutine transport.f90 (QSim v12.40) werden 70 Größen transportiert. Sprunglabel 911 goto ktrans \n
!! Die folgende Liste enthält nun die Angaben, von welcher Subroutine/Modul die jeweiligen planktischen Konzentrationen geändert werden.
!! <table >
!! <tr><th> Sprunglabel </th><th> Variablenname </th><th> Beschreibung 	</th><th> Subroutine 	</th></tr>
!!
!! <tr><td> 600 </td><td> tempw 	</td><td> Temperatur 			</td><td> temperw_huelle() 	</td></tr>
!! <tr><td></td></tr>			
!! <tr><td> 602 </td><td> vo2 		</td><td> Sauerstoffgehalt 		</td><td> oxygen 	</td></tr>
!! <tr><td></td></tr>			
!! <tr><td> 604 </td><td> vNH4 		</td><td> Ammonium 			</td><td> ncyc_huelle() 		</td></tr>
!! <tr><td> 606 </td><td> vNO2 		</td><td> Nitrit 			</td><td> ncyc_huelle() 		</td></tr>
!! <tr><td> 608 </td><td> vNO3 		</td><td> Nitrat 			</td><td> ncyc_huelle() 		</td></tr>
!! <tr><td> 610 </td><td> vx0 		</td><td> Nitrosomonas  		</td><td> ncyc_huelle() 		</td></tr>
!! <tr><td> 612 </td><td> vx02 		</td><td> Nitrobacter			</td><td> ncyc_huelle() 		</td></tr>
!! <tr><td> 532 </td><td> gesN 		</td><td> GesamtStickstoff 		</td><td> ncyc_huelle() 		</td></tr>
!! <tr><td></td></tr>			
!! <tr><td> 534 </td><td> gesP 		</td><td> GesamtPhosphat?? 		</td><td> po4s 		</td></tr>
!! <tr><td> 616 </td><td> gelP 		</td><td> ortho-Phosphat 		</td><td> po4s 		</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 614 </td><td> Si 		</td><td> Silikat 			</td><td> silikat 	</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 618 </td><td> obsb 		</td><td> biochemischer Sauerstoffbed. 	</td><td> orgC 		</td></tr>
!! <tr><td> 672 </td><td> CD(1 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 674 </td><td> CD(2 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 676 </td><td> CP(1 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 678 </td><td> CP(2 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 680 </td><td> CM 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 682 </td><td> BAC 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 684 </td><td> O2BSB 	</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 686 </td><td> BL01 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 688 </td><td> BL02 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 690 </td><td> vbsb 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 692 </td><td> vcsb 		</td><td> ?? 				</td><td> orgC 		</td></tr>
!! <tr><td> 508 </td><td> fbsgr 	</td><td> ?? 				</td><td> orgC 			</td></tr>
!! <tr><td> 510 </td><td> frfgr 	</td><td> ?? 				</td><td> orgC 			</td></tr>
!! <tr><td> 512 </td><td> nl0 		</td><td> Verhältnis von Stickstoff zu Kohlenstoff in organischem Material	</td><td> orgC 			</td></tr>
!! <tr><td> 514 </td><td> pl0 		</td><td> Phosphor? 				</td><td> orgC 			</td></tr>
!! <tr><td></td></tr>			
!! <tr><td> 622 </td><td> chla 		</td><td> ?? 					</td><td> algaesgr 	</td></tr>
!! <tr><td> 624 </td><td> chlaki 	</td><td> ?? 					</td><td> algaesgr 	</td></tr>
!! <tr><td> 626 </td><td> chlagr 	</td><td> ?? 					</td><td> algaesgr 	</td></tr>
!! <tr><td> 648 </td><td> agbcm 	</td><td> Biomasse/Kohlenstoff gruen-Algen	</td><td> algaesgr 	</td></tr>
!! <tr><td> 628 </td><td> chlabl 	</td><td> ?? 					</td><td> algaesgr 	</td></tr>
!! <tr><td> 632 </td><td> agr 		</td><td> ?? 					</td><td> algaesgr 	</td></tr>
!! <tr><td> 636 </td><td> vkigr 	</td><td> ?? 					</td><td> algaesgr 	</td></tr>
!! <tr><td> 638 </td><td> antbl 	</td><td> ?? 					</td><td> algaesgr 	</td></tr>
!! <tr><td> 642 </td><td> svhemg 	</td><td> ?? 					</td><td> algaesgr 	</td></tr>
!! <tr><td> 664 </td><td> Q_NG 		</td><td> Stickstoffanteil der Gruen-Algen 	</td><td> algaesgr 	</td></tr>
!! <tr><td> 666 </td><td> Q_PG 		</td><td> ?? 					</td><td> algaesgr 	</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 658 </td><td> Q_NK 		</td><td> Stickstoffanteil der Kiesel-Algen 	</td><td> algaeski 	</td></tr>
!! <tr><td> 630 </td><td> aki 		</td><td> ?? 					</td><td> algaeski 	</td></tr>
!! <tr><td> 640 </td><td> svhemk 	</td><td> ?? 					</td><td> algaeski 	</td></tr>
!! <tr><td> 646 </td><td> akbcm 	</td><td> Biomasse/Kohlenstoff kiesel-Algen	</td><td> algaeski 	</td></tr>
!! <tr><td> 536 </td><td> SKmor 	</td><td> ?? 					</td><td> algaeski 	</td></tr>
!! <tr><td> 660 </td><td> Q_PK 		</td><td> Phosporanteil Kieselalgen 		</td><td> algaeski 	</td></tr>
!! <tr><td> 662 </td><td> Q_SK 		</td><td> Siliziumanteil Kieselalgen		</td><td> algaeski 	</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 634 </td><td> abl 		</td><td> ?? 					</td><td> algaesbl 	</td></tr>
!! <tr><td> 644 </td><td> svhemb 	</td><td> ?? 					</td><td> algaesbl 	</td></tr>
!! <tr><td> 650 </td><td> abbcm 	</td><td> Biomasse/Kohlenstoff blau-Algen	</td><td> algaesbl 	</td></tr>
!! <tr><td> 668 </td><td> Q_NB 		</td><td> Stickstoffanteil der Blau-Algen 	</td><td> algaesbl 	</td></tr>
!! <tr><td> 670 </td><td> Q_PB 		</td><td> ?? 					</td><td> algaesbl 	</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 652 </td><td> akiiv 	</td><td> wohl unbenutzt 		</td><td> ???? 		</td></tr>
!! <tr><td> 654 </td><td> agriv 	</td><td> wohl unbenutzt 		</td><td> ???? 		</td></tr>
!! <tr><td> 656 </td><td> abliv 	</td><td> wohl unbenutzt 		</td><td> ???? 		</td></tr>
!! <tr><td> 500 </td><td> abrzo1 	</td><td> wohl unbenutzt 		</td><td> ???? 		</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 694 </td><td> CHNF 		</td><td> ?? 				</td><td> HNF 		</td></tr>
!! <tr><td> 696 </td><td> BVHNF 	</td><td> ?? 				</td><td> HNF 		</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 504 </td><td> ss 		</td><td> ORG. UND ANORG. SCHWEBSTOFFE 	</td><td> algaeski + SCHWEB 	</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 502 </td><td> ssalg 	</td><td> GESAMTSCHWEBSTOFFE 		</td><td> SCHWEB 		</td></tr>
!! <tr><td> 506 </td><td> fssgr 	</td><td> ?? 				</td><td> SCHWEB 		</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 698 </td><td> zooind 	</td><td> Anzahl der Rotatorien		</td><td> konsum 	</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 518 </td><td> dlarvn 	</td><td> ?? 				</td><td> dreissen 		</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 516 </td><td> stind 	</td><td> ?? 				</td><td> ph 			</td></tr>
!! <tr><td> 522 </td><td> mw 		</td><td> ?? 				</td><td> ph 		</td></tr>
!! <tr><td> 524 </td><td> pw 		</td><td> ?? 				</td><td> ph 		</td></tr>
!! <tr><td> 526 </td><td> ca 		</td><td> ?? 				</td><td> ph 		</td></tr>
!! <tr><td> 528 </td><td> lf 		</td><td> Leitfähigkeit			</td><td> ph 		</td></tr>
!! <tr><td> 530 </td><td> vh 		</td><td> ?? 				</td><td> ph(vph) 	</td></tr>
!! <tr><td></td></tr>
!! <tr><td> 537 </td><td> DOSCF 	</td><td> ?? 				</td><td> COLIFORM 	</td></tr>
!! <tr><td> 520 </td><td> coli 		</td><td> ?? 				</td><td> COLIFORM 	</td></tr>
!!</table>

!> Die suboutine netz_lesen() ließt:\n
!! Das casu Netz wird mittels der Subroutinen points(), elements() und edges() von den Dateien point, file.elements und edges gelesen. \n
!! Das Netz von SCHISM wird mit netz_gr3() aus einer ELCIRC .gr3 Datei gelesen.
!! \n\n
!! aus Datei module_modell.f95 ; zurück zu \ref Netz
      subroutine netz_lesen()
      use modell
      implicit none
      logical points, elements, netz_gr3, edges

      !!call points(vorhanden)
      if(points())then
         print*,"netz_lesen() aus transinfo/points + transinfo/file.elements"
         if(.not. elements())then
            write(fehler,*)'wenn transinfo/points da ist, müsste es auch ein transinfo/file.elements geben !!'
            call qerror(fehler)
         end if !! elements()
         kanten_vorhanden = edges()
      else
         if( netz_gr3()) then
            print*,"netz_lesen() aus ecirc .gr3 Datei"
         else
            call qerror("netz_lesen() findet kein Netz, weder in points noch in .gr3")
         endif !netz_gr3
      endif ! points

      END subroutine netz_lesen
!----+-----+----
!> Die suboutine netz_gr3() ließt das Netz aus der GR3-Datei: ## noch nicht implementiert ##
      logical function netz_gr3()
      implicit none
      call qerror('netz_gr3 noch nicht implementiert')
      netz_gr3=.true.
      return      
      end function netz_gr3

!----+-----+----
!> Die suboutine points() ließt die Datei points:
!! diese enthält die horizontalen Knotenorte, die Knotenhöhen und die Zonen-Nummern der Knoten.\n
!! <a href="./exp/points" target="_blank">Beispiel</a> \n
!! Die Datei points wird von <a href="http://www.wasserimunterricht.de/wyrwa/casu12.html"  target="_blank">casu</a>
!! in das transinfo Verzeichnis ausgegeben. \n
!! generiert wird points auch von casu:out dir t. 
!! \n\n
!! Dateiaufbau:\n
!! In der ersten Zeile steht die Anzahl der Knoten; in den Folgezeilen je ein Knoten.
!! die ersten drei Zahlen in einer Knotenzeile sind die beiden horizontalen Koordinaten und die Sohlhöhe (x,y,z) \n
!! danach Folgen Zonennummer und Randnummer; 
!! als letztes ist die horizontale Fläche der Finite-Volumen Zelle aufgeführt, die von dem Knoten repräsentiert wird.
!! \n\n
!! aus Datei module_modell.f95 ; zurück zu \ref Netz
      logical function points()
      use modell
      implicit none
      character (len=300) :: dateiname
      integer :: open_error, string_read_error, ion, nknot, n, alloc_status, anzrand
      real :: xmax, xmin, ymax, ymin, zmax, zmin
!
      modell_flaeche=0.0

      print*,'Netzknoten aus Datei'
      write(dateiname,'(2A)')trim(modellverzeichnis),'transinfo/points'
      print*,trim(dateiname)
      ion=103
      open ( unit =ion , file = dateiname, status ='old', action ='read ', iostat = open_error )
      if(open_error.ne.0) then
         !write(fehler,*)'open_error points'
         !call qerror(fehler)
         points=.false.
         print*,'points open error; Annahme: nicht vorhanden.'
         return
      else
         points=.true.
      end if ! open_error.ne.0
  777 continue
      if(zeile(ion))then
         if(ctext(1:1).eq.'#')then
            print*,trim(ctext)
            goto 777
         endif 
         read(ctext, *, iostat = string_read_error ) knotenanzahl2D
         if(string_read_error.ne.0) then
           write(fehler,*)'string_read_error SUBROUTINE points nknot'
            call qerror(fehler)
         end if ! open_error.ne.0
         print*,knotenanzahl2D, 'Knoten sollen in points sein'
         nonu=knotenanzahl2D ! kontrollwert
      else
         write(fehler,*)'Lesen der knotenanzahl2D im Kopf von point fehlgeschlagen'
         call qerror(fehler)
      end if !erste zeilen aus points gelesen

      !knotenanzahl2D=knotenanzahl3D
      print*,'Momentan noch 2D-Tiefengemittelt'

      allocate (knoten_x(knotenanzahl2D), stat = alloc_status )
      if(alloc_status.ne.0)then
         write(fehler,*)' Rueckgabewert   von   allocate knoten_x(knotenanzahl2D) :', alloc_status
         call qerror(fehler)
      end if 
      allocate (knoten_y(knotenanzahl2D), stat = alloc_status )
      if(alloc_status.ne.0)then
         write(fehler,*)' Rueckgabewert   von   allocate knoten_y(knotenanzahl2D) :', alloc_status
         call qerror(fehler)
      end if 
      allocate (knoten_z(knotenanzahl2D), stat = alloc_status )
      if(alloc_status.ne.0)then
         write(fehler,*)' Rueckgabewert   von   allocate knoten_z(knotenanzahl2D) :', alloc_status
         call qerror(fehler)
      end if 
      allocate (knoten_rand(knotenanzahl2D), stat = alloc_status )
      if(alloc_status.ne.0)then
         write(fehler,*)' Rueckgabewert   von   allocate knoten_rand(knotenanzahl2D) :', alloc_status
         call qerror(fehler)
      end if 
      allocate (knoten_zone(knotenanzahl2D), stat = alloc_status )
      if(alloc_status.ne.0)then
         write(fehler,*)' Rueckgabewert   von   allocate knoten_zone(knotenanzahl2D) :', alloc_status
         call qerror(fehler)
      end if 
      allocate (knoten_flaeche(knotenanzahl2D), stat = alloc_status )
      if(alloc_status.ne.0)then
         write(fehler,*)' Rueckgabewert   von   allocate knoten_flaeche(knotenanzahl2D) :', alloc_status
         call qerror(fehler)
      end if 

      ! Knotenzeilen nacheinander einlesen
      n=0
      do while( zeile(ion))
         n=n+1
         read(ctext(1:2000), *, iostat = string_read_error ) &
             knoten_x(n), knoten_y(n), knoten_z(n),          & 
             knoten_zone(n), knoten_rand(n), knoten_flaeche(n)

         if(string_read_error.ne.0)then
            write(fehler,*)'Lesen fehlgeschlagen aus points an knoten #', n,' read_error:', string_read_error, &
     &                     ' points wird von casu auf /transinfo ausgegeben. Nicht die converti-Version verwenden!'
            call qerror(fehler)
         end if 
         if(knoten_zone(n).lt.0)then
            write(fehler,*)' knoten #',n,': Zonennummer darf nicht negativ sein'
            call qerror(fehler)
         end if
         if(knoten_rand(n).lt.0)then
            write(fehler,*)' knoten #',n,': Randnummer darf nicht negativ sein'
            call qerror(fehler)
         end if
         if(knoten_flaeche(n).le.0.0)then
            write(fehler,*)' knoten #',n,': Knotenfläche muss größer Null sein'
            call qerror(fehler)
         end if
         modell_flaeche=modell_flaeche+knoten_flaeche(n)
      end do ! zeile
      if(n.ne.knotenanzahl2D)then
         write(fehler,*)'Zeilenzahl falsch in Datei points'
         call qerror(fehler)
      end if
      close (ion)
!!
      xmax=-999999999999.9 
      xmin=999999999999.9  
      ymax=-999999999999.9  
      ymin=999999999999.9  
      zmax=-999999999999.9  
      zmin=999999999999.9 
      min_rand=9999
      min_zone=9999
      max_rand=-9999
      max_zone=-9999
      anzrand=0
      do n=1,knotenanzahl2D
         if(xmax.le.knoten_x(n))xmax=knoten_x(n)
         if(xmin.ge.knoten_x(n))xmin=knoten_x(n)  
         if(ymax.le.knoten_y(n))ymax=knoten_y(n)  
         if(ymin.ge.knoten_y(n))ymin=knoten_y(n) 
         if(zmax.le.knoten_z(n))zmax=knoten_z(n)
         if(zmin.ge.knoten_z(n))zmin=knoten_z(n)
         if(knoten_zone(n).lt.min_zone)min_zone=knoten_zone(n)
         if(knoten_zone(n).gt.max_zone)max_zone=knoten_zone(n)
         if(knoten_rand(n).lt.min_rand)min_rand=knoten_rand(n)
         if(knoten_rand(n).gt.max_rand)max_rand=knoten_rand(n)
         if(knoten_rand(n).gt. 0)anzrand=anzrand+1
      end do ! alle Knoten
      print*,'x-koordinate max+min', xmax, xmin
      print*,'y-koordinate max+min', ymax, ymin
      print*,'Sohlhöhe max+min', zmax, zmin 
      print*,'Zonen# von ', min_zone, ' bis ', max_zone
      print*,'Rand# von ', min_rand, ' bis ', max_rand
      print*,'modell_flaeche= ',modell_flaeche
      print*,'Von ',knotenanzahl2D,' Knoten sind ',anzrand ,' Randknoten'

      mittelflaech=0.0
      mittelvolumen=0.0
      return      
      end function points
!----+-----+----

!> Die suboutine elements() ließt Vermaschung von der Datei
!! <a href="./exp/file.elements" target="_blank">file.elements</a>.\n
!! In der ersten Zeile steht die Anzahl der Elemente,
!! in den folgezeilen steht je ein Element . der erste Integer in der Zeile ist 3 oder 4 und gibt an, ob es 
!! sich um ein Drei- oder Vieleck handelt. Danach folgen 3 oder 4 Knotennummern\n
!! ## ACHTUNG ## Knotennummerierung beginnt bei Null
!! \n\n
!! aus Datei module_modell.f95 ; zurück zu \ref Modellerstellung
      logical function elements()
      use modell
      implicit none
      character (len=longname) :: dateiname, systemaufruf
      integer :: ndumm, n, j, alloc_status, ion, open_error, string_read_error, system_error, errcode
      logical zeile_vorhanden

!     Datei file.elements lesen falls vorhanden
      element_vorhanden=.false.
      write(dateiname,'(2A)',iostat = errcode)trim(modellverzeichnis),'transinfo/file.elements'
      if(errcode .ne. 0)call qerror('elements writing filename elemente_ failed')
      write(systemaufruf,'(3A)',iostat = errcode)'stat ',trim(dateiname),' >/dev/null 2>/dev/null'
      if(errcode .ne. 0)call qerror('elements writing filename elemente_ failed')
      call system(systemaufruf,system_error)
      !print*,'systemaufruf :',trim(systemaufruf),' system_error=',system_error
      if(system_error.eq.0) then
         element_vorhanden=.true.
         ion=101
         open ( unit =ion , file = dateiname, status ='old', action ='read ', iostat = open_error )
         zeile_vorhanden=zeile(ion)
         read(ctext, *, iostat = string_read_error ) n_elemente
         if(string_read_error.eq.0) then
            print*,'file.elements mit ',n_elemente,' Elementen'
         else
            write(fehler,*)'string_read_error ausgabe.f95, file.elements'
            call qerror(fehler)
         end if ! string_read_error.ne.0
         allocate (cornernumber(n_elemente), stat = alloc_status )
         allocate (elementnodes(n_elemente,4), stat = alloc_status )
         summ_ne=0
         do n=1,n_elemente
            if(zeile(ion))then
               read(ctext, *, iostat = string_read_error ) cornernumber(n)
               if (cornernumber(n).eq.3)then ! Dreieck
                  read(ctext, *, iostat = string_read_error ) ndumm, &
                  elementnodes(n,1),elementnodes(n,2),elementnodes(n,3)
               else ! nicht dreieck
                  if(cornernumber(n).eq.4)then ! Vieleck
                     read(ctext, *, iostat = string_read_error ) ndumm, &
                     elementnodes(n,1),elementnodes(n,2),elementnodes(n,3),elementnodes(n,4)
                  else ! weder Drei- noch Viereck
                     write(fehler,*)'weder Drei- noch Viereck ',n
                     call qerror(fehler)
                  end if !Viereck
               end if !Dreieck
            else ! Zeile nicht lesbar
               write(fehler,*)'Lesen aus file.elements fehlgeschlagen'
               call qerror(fehler)
            end if !Zeile gelesen
            do j=1,cornernumber(n)
               elementnodes(n,j)=elementnodes(n,j)+1
            end do ! alle Knoten im Element
            summ_ne=summ_ne+cornernumber(n)+1
         end do ! alle elemente
         close (ion) 
      else
         write(fehler,*)'Datenausgabe ohne file.elements sieht nicht gut aus'
         call qerror(fehler)
      end if ! end if file.elements vorhanden

      allocate (element_zone(n_elemente), stat = alloc_status )
      if(alloc_status.ne.0)then
         call qerror('allocate (element_zone failed')
      else
         print*,' allocate (element_zone(n_elemente) worked elements',meinrang,n_elemente
      endif
      ! bei casu-Netzen hat der Knoten die Zone, vorsichtshalber wird sie hier auf -7 initialisiert
      do n=1,n_elemente ! alle Elemente
         element_zone(n)=-7
      end do ! alle Elemente

      elements=.true.
      print*,'logical function elements(), module_modell.f95, hat aus file.elements ',n_elemente,' Elemente gelesen'
      return
      end function elements
!----+-----+----

!> function edges() 
!! liesst die Datei edges, falls vorhanden:
!! Informationen zu den Elementkanten\n
!! die Zonen-Nummern der Knoten.\n
!! <a href="./exp/edges" target="_blank">Beispiel</a> \n
!! Ausschnitt:\n
!!  casu Modell: /mreferate/wyrwa/casulli/test08/ue4\n
!!  casu Version 5. 5. 2015  edgenumber=\n
!! 141544\n
!!  top_node,bottom_node ; left_element,right_element ; edge_length,ground ; cell_bound_length,dist_left,dist_right,false_dist ; e.x,e.y 
!! ; boundary_type,boundary_face,boundary_number ; zone\n
!! 12  16  0  3769  4.78167  -0.42545  11.0632  5.78884  5.27436  3.31291e-12  0.836527 -0.547925  -1  -1  -1  5\n
!! 4517  12  0  3763  11.8354  -0.48355  3.87221  2.05119  1.82102  -5.05151e-13  0.498503 0.866888  -1  -1  -1  5\n
!! ...\n
!! \n\n
!! aus Datei module_modell.f95 ; zurück zu \ref Modellerstellung
      logical function edges()
      use modell
      character (len=300) :: dateiname
      integer :: n, ion, alloc_status, io_error
      real :: ground, dist_left, dist_right, false_dist, dummy1, dummy2
      integer :: boundary_type, boundary_face

      write(dateiname,'(2A)')trim(modellverzeichnis),'transinfo/edges'
      print*,trim(dateiname)
      ion=109
      open ( unit =ion , file = dateiname, status ='old', action ='read ', iostat = io_error )
      if(io_error.ne.0) then
         edges=.false.
         print*,'transinfo/edges open error; wohl keine kanteninformationen vorhanden'
         return
      else
         edges=.true.
         print*,'Datei transinfo/edges vorhanden'
      end if ! open_error.ne.0

      if(zeile(ion))read(ctext, *) kantenanzahl
      n=0
      do while( zeile(ion))
         n=n+1
      end do ! nächste zeile vorhanden
      print*,"transinfo/edges: kantenanzahl=",kantenanzahl," n=",n

      allocate (top_node(kantenanzahl), stat = io_error )
      allocate (bottom_node(kantenanzahl), stat = io_error )
      allocate (left_element(kantenanzahl), stat = io_error )
      allocate (right_element(kantenanzahl), stat = io_error )
      allocate (cell_bound_length(kantenanzahl), stat = io_error )
      allocate (edge_normal_x(kantenanzahl),edge_normal_y(kantenanzahl), stat = io_error )
      allocate (boundary_number(kantenanzahl), stat = io_error )
      allocate (zon_num(kantenanzahl), stat = io_error )
      if(io_error.ne.0) then
         edges=.false.
         write(fehler,*)'alloc_error in edges =', io_error
         call qerror(fehler)
      end if !! alloc_error

      rewind(ion) !! nochmal von vorne:
      if(zeile(ion))read(ctext, *) kantenanzahl
      n=0
      do while( zeile(ion))
         n=n+1
         read(ctext, *, iostat = io_error ) top_node(n), bottom_node(n), left_element(n), right_element(n),  &
     &                  dummy1, ground, cell_bound_length(n), dist_left, dist_right, false_dist,  &
     &                  edge_normal_x(n),edge_normal_y(n),boundary_type,boundary_face,boundary_number(n),zon_num(n)
     !    read(ctext, *, iostat = io_error ) top_node(n), bottom_node(n), left_element(n), right_element(n),  &
     !&                  edge_length(n), ground, cell_bound_length(n), dist_left, dist_right, false_dist,  &
     !&                  edge_x(n),edge_y(n),boundary_type,boundary_face,boundary_number(n),zon_num(n)
         if(io_error.ne.0) then
            edges=.false.
            write(fehler,*)'io_error =', io_error,' at edge #',n
            call qerror(fehler)
         end if !! io_error
         top_node(n)=top_node(n) + 1 !! Zählweise Fortran ab 1, C++ ab 0
         bottom_node(n)=bottom_node(n) + 1
         left_element(n)=left_element(n) + 1
         right_element(n)=right_element(n) + 1
         if((top_node(n).eq.kontrollknoten).or.(bottom_node(n).eq.kontrollknoten))then
            print*,'Kante #', n, "hat den kontrollknoten # ",kontrollknoten," als Top oder Bottom",top_node(n),bottom_node(n)
            print*,"cell_bound_length =",cell_bound_length(n)
            print*,"left_element right_element =",left_element(n), right_element(n)
            print*,"boundary_number zone =",boundary_number(n),zon_num(n)
         endif !! kontrrollknoten
      end do ! nächste zeile vorhanden
      
      return
      end function edges
!----+-----+----
