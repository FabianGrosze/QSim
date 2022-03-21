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

!> <h1>SUBROUTINE aparam_lesen()</h1>
!! Beschreibung siehe: \ref globaleParameter \n
!! Quelle: aparam_lesen.f95
      SUBROUTINE aparam_lesen()
      use modell
      use QSimDatenfelder
      use aparam                                                   
      implicit none
      character(500) dateiname
      integer :: io_error, j,i
      logical :: logi
      real dummy

      !print*,'Parameter sollen gelesen werden'
      write(dateiname,'(2A)')trim(modellverzeichnis),'APARAM.txt'
      open ( unit =55 , file = dateiname, status ='old', action ='read ', iostat = io_error )
      !open(unit=55, DEFAULTFILE=cpfad, file='APARAM.txt') 
      if(io_error.ne.0)call qerror('open_error APARAM.txt ... Datei vorhanden?')
      rewind (55) 

      if(zeile(55))read(ctext,*,iostat=io_error)agchl,aggmax,IKge,agksn,agksp 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)agremi,frmuge,bsbgr,csbgr,Qmx_NG 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)opgrmi,opgrma,asgre,ToptG,kTemp_Gr
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)akchl,akgmax,IKke,akksn,akksp 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)akkssi,akremi,frmuke,bsbki,csbki 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi 
      if(io_error.ne.0) goto 198
!      if(zeile(55))read(ctext,*,iostat=io_error)opkima,askie,ToptK,TmaxK,abchl 
      if(zeile(55))read(ctext,*,iostat=io_error)opkima,askie,ToptK,kTemp_Ki,abchl 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)abgmax,IKbe,abksn,abksp,abremi 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)opblma,asble,ToptB,kTemp_Bl,ifix
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)irmaxe,FopIRe,GRote,zresge,zakie 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)zagre,zable,ynmx1e,stks1e,anitrie 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)bnmx2e,bnks2e,KNH4e,KapN3e,hyPe 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)hymxDe,KsD1e,KsD2e,KsMe,upBACe 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)YBACe,rsGBACe,FoptDe,upHNFe,BACkse 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)alamda,fPOC1e,fPOC2e,SorpCape,Klange
      if(io_error.ne.0) goto 198
      ratecde = 0.0 ! coliform evt. nicht angegeben
      etacde = 0.0
      ratecie = 0.0
      xnuece = 0.0
      ratecge = 0.0
      ratecse = 0.0
      if(zeile(55))then
         read(ctext,*,iostat=io_error)KdNh3e
         if(io_error.ne.0) goto 198
         read(ctext,*,iostat=io_error)dummy,ratecde,etacde,ratecie,xnuece
         if(io_error.eq.0)then
            print*,'APARAM.txt coliform parameters present'
            if(zeile(55))read(ctext,*,iostat=io_error)ratecge,ratecse
            !if(io_error.ne.0) goto 198
		 else
            print*,'APARAM.txt coliform parameters missing'
         endif ! coliformpresent
      end if !zeile 21

      if(kontrollknoten.gt.0)then
         print*,'agchl,aggmax,IKge,agksn,agksp =',agchl,aggmax,IKge,agksn,agksp
         print*,'agremi,frmuge,bsbgr,csbgr,Qmx_NG =',agremi,frmuge,bsbgr,csbgr,Qmx_NG
         print*,'Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG =',Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG
         print*,'opgrmi,opgrma,asgre,ToptG,kTemp_Gr =',opgrmi,opgrma,asgre,ToptG,kTemp_Gr
         print*,'akchl,akgmax,IKke,akksn,akksp =',akchl,akgmax,IKke,akksn,akksp
         print*,'akkssi,akremi,frmuke,bsbki,csbki =',akkssi,akremi,frmuke,bsbki,csbki
         print*,'Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK =',Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK
         print*,'Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi =',Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi
         print*,'opkima,askie,ToptK,kTemp_Ki,abchl =',opkima,askie,ToptK,kTemp_Ki,abchl 
         print*,'abgmax,IKbe,abksn,abksp,abremi =',abgmax,IKbe,abksn,abksp,abremi
         print*,'frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB =',frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB
         print*,'Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi =',Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi
         print*,'opblma,asble,ToptB,kTemp_Bl,ifix =',opblma,asble,ToptB,kTemp_Bl,ifix
         print*,'irmaxe,FopIRe,GRote,zresge,zakie =',irmaxe,FopIRe,GRote,zresge,zakie
         print*,'zagre,zable,ynmx1e,stks1e,anitrie =',zagre,zable,ynmx1e,stks1e,anitrie
         print*,'bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e =',bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e
         print*,'bnmx2e,bnks2e,KNH4e,KapN3e,hyPe =',bnmx2e,bnks2e,KNH4e,KapN3e,hyPe
         print*,'hymxDe,KsD1e,KsD2e,KsMe,upBACe =',hymxDe,KsD1e,KsD2e,KsMe,upBACe
         print*,'YBACe,rsGBACe,FoptDe,upHNFe,BACkse =',YBACe,rsGBACe,FoptDe,upHNFe,BACkse
         print*,'alamda,fPOC1e,fPOC2e,SorpCape,Klange =',alamda,fPOC1e,fPOC2e,SorpCape,Klange
         print*,'KdNh3e,ratecde,etacde,ratecie,xnuece =',KdNh3e,ratecde,etacde,ratecie,xnuece
         print*,'ratecge,ratecse =',ratecge,ratecse
      endif
      print*,'read successfully 107 parameters from APARAM.txt.'

      if( agchl .lt. 0.0)call qerror('APARAM.txt agchl negativ (nicht zulässig in QSim3D)')
      if( aggmax .lt. 0.0)call qerror('APARAM.txt aggmax negativ (nicht zulässig in QSim3D)')
      if( IKge .lt. 0.0)call qerror('APARAM.txt IKge negativ (nicht zulässig in QSim3D)')
      if( agksn .lt. 0.0)call qerror('APARAM.txt agksn negativ (nicht zulässig in QSim3D)')
      if( agksp .lt. 0.0)call qerror('APARAM.txt agksp negativ (nicht zulässig in QSim3D)')

      if( agremi .lt. 0.0)call qerror('APARAM.txt agremi negativ (nicht zulässig in QSim3D)')
      if( frmuge .lt. 0.0)call qerror('APARAM.txt frmuge negativ (nicht zulässig in QSim3D)')
      if( bsbgr .lt. 0.0)call qerror('APARAM.txt bsbgr negativ (nicht zulässig in QSim3D)')
      if( csbgr .lt. 0.0)call qerror('APARAM.txt csbgr negativ (nicht zulässig in QSim3D)')
      if( Qmx_NG .lt. 0.0)call qerror('APARAM.txt Qmx_NG negativ (nicht zulässig in QSim3D)')

      if( Qmx_PG .lt. 0.0)call qerror('APARAM.txt Qmx_PG negativ (nicht zulässig in QSim3D)')
      if( Qmn_NG .lt. 0.0)call qerror('APARAM.txt Qmn_NG negativ (nicht zulässig in QSim3D)')
      if( Qmn_PG .lt. 0.0)call qerror('APARAM.txt Qmn_PG negativ (nicht zulässig in QSim3D)')
      if( upmxNG .lt. 0.0)call qerror('APARAM.txt upmxNG negativ (nicht zulässig in QSim3D)')
      if( upmxPG .lt. 0.0)call qerror('APARAM.txt upmxPG negativ (nicht zulässig in QSim3D)')

      if( opgrmi .lt. 0.0)call qerror('APARAM.txt opgrmi negativ (nicht zulässig in QSim3D)')
      if( opgrma .lt. 0.0)call qerror('APARAM.txt opgrma negativ (nicht zulässig in QSim3D)')
      if( asgre .lt. 0.0)call qerror('APARAM.txt asgre negativ (nicht zulässig in QSim3D)')
      if( ToptG .lt. 0.0)call qerror('APARAM.txt ToptG negativ (nicht zulässig in QSim3D)')
      if( kTemp_Gr .lt. 0.0)call qerror('APARAM.txt kTemp_Gr negativ (nicht zulässig in QSim3D)')

      if( akchl .lt. 0.0)call qerror('APARAM.txt akchl negativ (nicht zulässig in QSim3D)')
      if( akgmax .lt. 0.0)call qerror('APARAM.txt akgmax negativ (nicht zulässig in QSim3D)')
      if( IKke .lt. 0.0)call qerror('APARAM.txt IKke negativ (nicht zulässig in QSim3D)')
      if( akksn .lt. 0.0)call qerror('APARAM.txt akksn negativ (nicht zulässig in QSim3D)')
      if( akksp .lt. 0.0)call qerror('APARAM.txt akksp negativ (nicht zulässig in QSim3D)')

      if( akkssi .lt. 0.0)call qerror('APARAM.txt akkssi negativ (nicht zulässig in QSim3D)')
      if( akremi .lt. 0.0)call qerror('APARAM.txt akremi negativ (nicht zulässig in QSim3D)')
      if( frmuke .lt. 0.0)call qerror('APARAM.txt frmuke negativ (nicht zulässig in QSim3D)')
      if( bsbki .lt. 0.0)call qerror('APARAM.txt bsbki negativ (nicht zulässig in QSim3D)')
      if( csbki .lt. 0.0)call qerror('APARAM.txt csbki negativ (nicht zulässig in QSim3D)')

      if( Qmx_NK .lt. 0.0)call qerror('APARAM.txt Qmx_NK negativ (nicht zulässig in QSim3D)')
      if( Qmx_PK .lt. 0.0)call qerror('APARAM.txt Qmx_PK negativ (nicht zulässig in QSim3D)')
      if( Qmx_SK .lt. 0.0)call qerror('APARAM.txt Qmx_SK negativ (nicht zulässig in QSim3D)')
      if( Qmn_NK .lt. 0.0)call qerror('APARAM.txt Qmn_NK negativ (nicht zulässig in QSim3D)')
      if( Qmn_PK .lt. 0.0)call qerror('APARAM.txt Qmn_PK negativ (nicht zulässig in QSim3D)')

      if( Qmn_SK .lt. 0.0)call qerror('APARAM.txt Qmn_SK negativ (nicht zulässig in QSim3D)')
      if( upmxNK .lt. 0.0)call qerror('APARAM.txt upmxNK negativ (nicht zulässig in QSim3D)')
      if( upmxPK .lt. 0.0)call qerror('APARAM.txt upmxPK negativ (nicht zulässig in QSim3D)')
      if( upmxSK .lt. 0.0)call qerror('APARAM.txt upmxSK negativ (nicht zulässig in QSim3D)')
      if( opkimi .lt. 0.0)call qerror('APARAM.txt opkimi negativ (nicht zulässig in QSim3D)')

      if( opkima .lt. 0.0)call qerror('APARAM.txt opkima negativ (nicht zulässig in QSim3D)')
      if( askie .lt. 0.0)call qerror('APARAM.txt askie negativ (nicht zulässig in QSim3D)')
      if( ToptK .lt. 0.0)call qerror('APARAM.txt ToptK negativ (nicht zulässig in QSim3D)')
      if( kTemp_Ki .lt. 0.0)call qerror('APARAM.txt kTemp_Ki negativ (nicht zulässig in QSim3D)')
      if( abchl .lt. 0.0)call qerror('APARAM.txt abchl negativ (nicht zulässig in QSim3D)')
 
      if( abgmax .lt. 0.0)call qerror('APARAM.txt abgmax negativ (nicht zulässig in QSim3D)')
      if( IKbe .lt. 0.0)call qerror('APARAM.txt IKbe negativ (nicht zulässig in QSim3D)')
      if( abksn .lt. 0.0)call qerror('APARAM.txt abksn negativ (nicht zulässig in QSim3D)')
      if( abksp .lt. 0.0)call qerror('APARAM.txt abksp negativ (nicht zulässig in QSim3D)')
      if( abremi .lt. 0.0)call qerror('APARAM.txt abremi negativ (nicht zulässig in QSim3D)')

      if( frmube .lt. 0.0)call qerror('APARAM.txt frmube negativ (nicht zulässig in QSim3D)')
      if( bsbbl .lt. 0.0)call qerror('APARAM.txt bsbbl negativ (nicht zulässig in QSim3D)')
      if( csbbl .lt. 0.0)call qerror('APARAM.txt csbbl negativ (nicht zulässig in QSim3D)')
      if( Qmx_NB .lt. 0.0)call qerror('APARAM.txt Qmx_NB negativ (nicht zulässig in QSim3D)')
      if( Qmx_PB .lt. 0.0)call qerror('APARAM.txt Qmx_PB negativ (nicht zulässig in QSim3D)')

      if( Qmn_NB .lt. 0.0)call qerror('APARAM.txt Qmn_NB negativ (nicht zulässig in QSim3D)')
      if( Qmn_PB .lt. 0.0)call qerror('APARAM.txt Qmn_PB negativ (nicht zulässig in QSim3D)')
      if( upmxNB .lt. 0.0)call qerror('APARAM.txt upmxNB negativ (nicht zulässig in QSim3D)')
      if( upmxPB .lt. 0.0)call qerror('APARAM.txt upmxPB negativ (nicht zulässig in QSim3D)')
      if( opblmi .lt. 0.0)call qerror('APARAM.txt opblmi negativ (nicht zulässig in QSim3D)')

      if( opblma .lt. 0.0)call qerror('APARAM.txt opblma negativ (nicht zulässig in QSim3D)')
      if( asble .lt. 0.0)call qerror('APARAM.txt asble negativ (nicht zulässig in QSim3D)')
      if( ToptB .lt. 0.0)call qerror('APARAM.txt ToptB negativ (nicht zulässig in QSim3D)')
      if( kTemp_Bl .lt. 0.0)call qerror('APARAM.txt kTemp_Bl negativ (nicht zulässig in QSim3D)')
      if( ifix .lt. 0.0)call qerror('APARAM.txt ifix negativ (nicht zulässig in QSim3D)')

      if( irmaxe .lt. 0.0)print*,'irmaxe wird berechnet in konsum()'
      if( FopIRe .lt. 0.0)print*,'FopIRe wird berechnet in konsum()'
      if( GRote .lt. 0.0)call qerror('APARAM.txt GRote negativ (nicht zulässig in QSim3D)')
      if( zresge .lt. 0.0)call qerror('APARAM.txt zresge negativ (nicht zulässig in QSim3D)')
      if( zakie .lt. 0.0)call qerror('APARAM.txt zakie negativ (nicht zulässig in QSim3D)')

      if( zagre .lt. 0.0)call qerror('APARAM.txt zagre negativ (nicht zulässig in QSim3D)')
      if( zable .lt. 0.0)call qerror('APARAM.txt zable negativ (nicht zulässig in QSim3D)')
      if( ynmx1e .lt. 0.0)call qerror('APARAM.txt ynmx1e negativ (nicht zulässig in QSim3D)')
      if( stks1e .lt. 0.0)call qerror('APARAM.txt stks1e negativ (nicht zulässig in QSim3D)')
      if( anitrie .lt. 0.0)call qerror('APARAM.txt anitrie negativ (nicht zulässig in QSim3D)')

      if( bnmx1e .lt. 0.0)call qerror('APARAM.txt bnmx1e negativ (nicht zulässig in QSim3D)')
      if( bnks1e .lt. 0.0)call qerror('APARAM.txt bnks1e negativ (nicht zulässig in QSim3D)')
      if( ynmx2e .lt. 0.0)call qerror('APARAM.txt ynmx2e negativ (nicht zulässig in QSim3D)')
      if( stks2e .lt. 0.0)call qerror('APARAM.txt stks2e negativ (nicht zulässig in QSim3D)')
      if( anitri2e .lt. 0.0)call qerror('APARAM.txt anitri2e negativ (nicht zulässig in QSim3D)')

      if( bnmx2e .lt. 0.0)call qerror('APARAM.txt bnmx2e negativ (nicht zulässig in QSim3D)')
      if( bnks2e .lt. 0.0)call qerror('APARAM.txt bnks2e negativ (nicht zulässig in QSim3D)')
      if( KNH4e .lt. 0.0)call qerror('APARAM.txt KNH4e negativ (nicht zulässig in QSim3D)')
      if( KapN3e .lt. 0.0)call qerror('APARAM.txt KapN3e negativ (nicht zulässig in QSim3D)')
      if( hyPe .lt. 0.0)call qerror('APARAM.txt hyPe negativ (nicht zulässig in QSim3D)')

      if( hymxDe .lt. 0.0)call qerror('APARAM.txthymxDe  negativ (nicht zulässig in QSim3D)')
      if( KsD1e .lt. 0.0)call qerror('APARAM.txt KsD1e negativ (nicht zulässig in QSim3D)')
      if( KsD2e .lt. 0.0)call qerror('APARAM.txt KsD2e negativ (nicht zulässig in QSim3D)')
      if( KsMe .lt. 0.0)call qerror('APARAM.txt KsMe negativ (nicht zulässig in QSim3D)')
      if( upBACe .lt. 0.0)call qerror('APARAM.txt upBACe negativ (nicht zulässig in QSim3D)')

      if( YBACe .lt. 0.0)call qerror('APARAM.txt YBACe negativ (nicht zulässig in QSim3D)')
      if( rsGBACe .lt. 0.0)call qerror('APARAM.txt rsGBACe negativ (nicht zulässig in QSim3D)')
      if( FoptDe .lt. 0.0)call qerror('APARAM.txt FoptDe negativ (nicht zulässig in QSim3D)')
      if( upHNFe .lt. 0.0)call qerror('APARAM.txt upHNFe negativ (nicht zulässig in QSim3D)')
      if( BACkse .lt. 0.0)call qerror('APARAM.txt BACkse negativ (nicht zulässig in QSim3D)')

      if( alamda .lt. 0.0)call qerror('APARAM.txt alamda negativ (nicht zulässig in QSim3D)')
      if( fPOC1e .lt. 0.0)call qerror('APARAM.txt fPOC1e negativ (nicht zulässig in QSim3D)')
      if( fPOC2e .lt. 0.0)call qerror('APARAM.txt fPOC2e negativ (nicht zulässig in QSim3D)')
      if( SorpCape .lt. 0.0)call qerror('APARAM.txt SorpCape negativ (nicht zulässig in QSim3D)')
      if( Klange .lt. 0.0)call qerror('APARAM.txt Klange negativ (nicht zulässig in QSim3D)')

      if( KdNh3e .lt. 0.0)print*,'Partitionskoeffizient für Ammonium KdNh3e wird berechnet in sedflux()'
      if( ratecde .lt. 0.0)call qerror('APARAM.txt ratecde negativ (nicht zulässig in QSim3D)')
      if( etacde .lt. 0.0)call qerror('APARAM.txt etacde negativ (nicht zulässig in QSim3D)')
      if( ratecie .lt. 0.0)call qerror('APARAM.txt ratecie negativ (nicht zulässig in QSim3D)')
      if( xnuece .lt. 0.0)call qerror('APARAM.txt xnuece negativ (nicht zulässig in QSim3D)')

      if( ratecge .lt. -1.0)call qerror('APARAM.txt ratecge kleiner -1 (nicht zulässig in QSim3D)')
      if( ratecse .lt. -1.0)call qerror('APARAM.txt ratecse kleiner -1 (nicht zulässig in QSim3D)')

      close (55)
      RETURN

  198 continue
      print*,io_error,trim(ctext)
      call qerror('Lesefehler APARAM.txt')

      END subroutine aparam_lesen

!----+-----+----