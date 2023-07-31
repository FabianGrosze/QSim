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

!> Initialisieren von MPI
subroutine parallel_ini()
   use modell
   use QSimDatenfelder
   !#### use schism_msgp, only: myrank,parallel_abort !,nproc
   implicit none
   
   call mpi_init(ierr)
   if (ierr /= 0) call qerror('mpi_init(ierr) /= 0')
   mpi_komm_welt = MPI_COMM_WORLD
   
   call mpi_comm_size(mpi_komm_welt,proz_anz, ierr)
   if (ierr /= 0)call qerror('mpi_comm_size(mpi_komm_welt,proz_anz, ierr) /= 0')
   
   call mpi_comm_rank(mpi_komm_welt,meinrang,ierr)
   ! #### myrank=meinrang
   if (ierr /= 0)call qerror('mpi_comm_rank(mpi_komm_welt,meinrang,ierr) /= 0')
      
   write(*,"(a,i3,a,i0)") 'Prozess # ', meinrang,' started with PID = ', getpid()
   
   call mpi_barrier(mpi_komm_welt, ierr)
   return
end subroutine parallel_ini

!----+-----+----
!> parallel_vorbereiten()\n
!! ruft die subroutinen auf, die auf allen Processoren >0
!! die jeweiligen Felder allokieren und füllen.
!! \n\n
!! aus Datei parallel.f95; zurück: \ref lnk_datenstruktur
subroutine parallel_vorbereiten()
   use modell
   use QSimDatenfelder
   use mod_suspendedMatter, only: init_suspendedMatter
   use mod_salinity, only: init_salinity
   !!!###    use schism_msgp, only: myrank,parallel_abort !,nproc
   implicit none
   integer kontroll_lokal
   ! prepare for parallel
   call modell_parallel()
   !print*,meinrang," modell_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call planktkon_parallel() !calls scatter_planktkon()
   !print*,meinrang," planktkon_parallel() ... fertig"
   call mpi_barrier (mpi_komm_welt, ierr)
   call benthic_parallel()
   !print*,meinrang," benthic_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call ueber_parallel()
   !print*,meinrang," ueber_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call zonen_parallel()
   !print*,meinrang," zonen_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call wetter_parallel()
   !print*,meinrang," wetter_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   call randbedingungen_parallel()
   !print*,meinrang," randbedingungen_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   
   ! initialize SPM (and salinity)
   call init_salinity
   if (iEros>=0) then
      call schwebstoff_salz_parallel()
      !print*,meinrang," schwebstoff_salz_parallel() ... danach"
      call mpi_barrier (mpi_komm_welt, ierr)
   else
      call init_suspendedMatter
   endif
   
   call alter_parallel()
   !print*,meinrang," alter_parallel() ... danach"
   call mpi_barrier (mpi_komm_welt, ierr)
   kontroll_lokal = kontrollknoten-(meinrang*part)
   if ((kontroll_lokal > 0) .and. (kontroll_lokal <= part)) then
      print*,'meinrang,part,number_plankt_vari,kontrollknoten,kontroll_lokal = '  &
            , meinrang,part,number_plankt_vari,kontrollknoten,kontroll_lokal
      print*,'nach randbedingungen_parallel(): tempw,chla = ',  &
            planktonic_variable_p( 1+(kontroll_lokal-1)*number_plankt_vari),  &
            planktonic_variable_p(11+(kontroll_lokal-1)*number_plankt_vari)
   else ! keine kontrollausgabe
      print*,'meinrang,part,number_plankt_vari = ',meinrang,part,number_plankt_vari
   endif
   call ganglinien_parallel()
   call ausgeben_parallel()
   if (meinrang == 0)print*,'parallel_vorbereiten done'
   call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine parallel_vorbereiten
!----+-----+----
!> zentrale Modellwerte an alle Prozesse verteilen.\n
!! und part berechnen. d.i. die Anzahl der Knoten, die jeder Prozess erhält.
!! \n\n
!! aus Datei parallel.f95; zurück: \ref lnk_datenstruktur
subroutine modell_parallel()
   use modell
   use QSimDatenfelder
   implicit none
   integer n, alloc_status
   
   call MPI_Bcast(deltat,           1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(rechenzeit,       1, MPI_INTEGER8, 0, mpi_komm_welt, ierr)
   call MPI_Bcast(zeitpunkt,        1, MPI_INTEGER8, 0, mpi_komm_welt, ierr)
   call MPI_Bcast(zeitschrittanzahl,1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(time_offset,      1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(knotenanzahl3D,   1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(knotenanzahl2D,   1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(n_elemente,       1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(kontrollknoten,   1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(modell_geob,      1, MPI_FLOAT,    0, mpi_komm_welt, ierr)
   call MPI_Bcast(modell_geol,      1, MPI_FLOAT,    0, mpi_komm_welt, ierr)
   
   zeitpunkt = rechenzeit
   
   ! start + end time
   call MPI_Bcast(itags,          1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(monats,         1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(jahrs,          1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(uhrs,           1, MPI_FLOAT,    0, mpi_komm_welt, ierr)
   call MPI_Bcast(startzeitpunkt, 1, MPI_INTEGER8, 0, mpi_komm_welt, ierr)
   call MPI_Bcast(itage,          1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(monate,         1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(jahre,          1, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(uhren,          1, MPI_FLOAT,    0, mpi_komm_welt, ierr)
   call MPI_Bcast(endzeitpunkt,   1, MPI_INTEGER8, 0, mpi_komm_welt, ierr)
   
   ! Berechnung- und Ausgabeflags
   !read(92,9220)imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren,ilongDis,FlongDis,iColi,ikonsS,iSchwer,iphy  aus EREIGG.txt
   call MPI_Bcast(imitt,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ipH,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(idl,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(itemp,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(itracer,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ieros,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ischwa,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iverfahren,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ilongDis,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(FlongDis,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iColi,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ikonsS,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iSchwer,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iphy,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iformVert,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iform_verdr,1,MPI_INT,0,mpi_komm_welt,ierr)
   !call MPI_Bcast(min_tief,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   !print*,meinrang," modell_parallel() min_tief,n_elemente=", min_tief,n_elemente
   call MPI_Bcast(number_plankt_point,1,MPI_INT,0,mpi_komm_welt,ierr)
   !print*,meinrang," modell_parallel() number_plankt_point=", number_plankt_point
   !     !partitioning of variable arrays needed earlier now in eingabe()
   !     part=number_plankt_point/proz_anz
   !     n=part*proz_anz
   !     !print*,'ini_par knotenanzahl=', nk,' proz_anz=', proz_anz, ' part=', part, ' part*proz_anz=', n
   !     if(n.lt.number_plankt_point)part=part+1
   !     print*,'part=', part, ' part*proz_anz=',part*proz_anz," meinrang=",meinrang  &
   ! &         ," modell_parallel() number_plankt_point=", number_plankt_point
   !!geländehöhe überall verfügbar machen
   if (meinrang /= 0) then
      allocate (knoten_z(knotenanzahl2D), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)'modell_parallel() Rueckgabewert von allocate knoten_z(knotenanzahl2D) :', alloc_status
         call qerror(fehler)
      endif ! allocate fehlgeschlagen
   endif !! nicht 0-Prozess
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Bcast(knoten_z,knotenanzahl2D,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call mpi_barrier (mpi_komm_welt, ierr)
   !if(meinrang .eq.0)print*,"modell_parallel fertig"
   !print*,"modell_parallel fertig ",meinrang
   return
end subroutine modell_parallel

!----+-----+----
!> aparam_parallel makes all parameters available to all processes
!!
!! aus Datei parallel.f95 
subroutine aparam_parallel()
   use modell
   use module_aparam
   implicit none
   
   !namelist /ALGAE/  &
   call MPI_Bcast(AGCHL,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AGGMAX,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(IKge,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AGKSN,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AGKSP,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AGREMI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(frmuge,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(BSBGR,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(CSBGR,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMX_NG,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMX_PG,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMN_NG,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMN_PG,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(UPMXNG,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(UPMXPG,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(OPGRMI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(OPGRMA,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ASGRE,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(TOPTG,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(KTEMP_GR,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AKCHL,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AKGMAX,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(IKke,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AKKSN,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AKKSP,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AKKSSI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(AKREMI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(frmuke,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(BSBKI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(CSBKI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMX_NK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMX_PK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMX_SK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMN_NK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMN_PK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMN_SK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(UPMXNK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(UPMXPK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(UPMXSK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(OPKIMI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(OPKIMA,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ASKIE,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(TOPTK,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(KTEMP_Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ABCHL,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ABGMAX,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(IKbe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ABKSN,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ABKSP,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ABREMI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(frmube,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(BSBBL,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(CSBBL,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMX_NB,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMX_PB,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMN_NB,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(QMN_PB,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(UPMXNB,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(UPMXPB,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(OPBLMI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(OPBLMA,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ASBLE,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(TOPTB,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(KTEMP_Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ifix,1,MPI_INT,0,mpi_komm_welt,ierr)

   !namelist /Rotatorien/ 
   call MPI_Bcast(IRMAX,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(FOPTR,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(GROT,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ZRESG,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ZAKI,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ZAGR,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ZABL,1,MPI_FLOAT,0,mpi_komm_welt,ierr)

   !namelist /Nitrosomonas/ 
   call MPI_Bcast(YNMAX1,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(STKS1,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ANITR1,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(BNMX1,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(BNKS1,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   !namelist /Nitrobacter/ 
   call MPI_Bcast(YNMAX2,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(STKS2,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ANITR2,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(BNMX2,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(BNKS2,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   !namelist /Kohlenstoff/ 
   call MPI_Bcast(HyP1,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(hymxD,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(KsD1,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(KsD2,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(KsM,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(upBAC,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(YBAC,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(rsGBAC,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   !namelist /Muscheln/ 
   call MPI_Bcast(FoptD,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   !namelist /HNF/ 
   call MPI_Bcast(upHNF,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(BACks,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   !namelist /Wasser/ 
   call MPI_Bcast(ALAMDA,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   !namelist /Sediment/ 
   call MPI_Bcast(KNH4,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(KapN3,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(fPOC1,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(fPOC2,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(SorpCap,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(Klang,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(KdNh3,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   !namelist /Hygiene/ 
   call MPI_Bcast(ratecd,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(etacd,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(rateci,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(xnuec,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ratecg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ratecs,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   !namelist /Schwermetalle/
   call MPI_Bcast(c1Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_Pb,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_Cad,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_Cr,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_Fe,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_Cu,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_Mn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_Ni,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_Hg,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_U,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_Zn,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   call MPI_Bcast(c1As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e1As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c2As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e2As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c3As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e3As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c4As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e4As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(c5As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(e5As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(VTKoeffDe_As,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
   
   return
end subroutine aparam_parallel