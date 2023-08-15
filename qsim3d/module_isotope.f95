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
module isotope
   implicit none
   
   public  :: isotop_no3konsum

contains

   subroutine isotop_no3konsum(f, control)
      real        :: c_source, delta_source, R_source
      real        :: c_sink, delta_sink, R_sink
      real        :: f, alpha, eps, dc, R_standard
      real        :: d_logie, d_logik, d0, c0
      logical     :: control
      
      control = .true.
      return
      
      
      open(unit = 86, file = 'd15N.1000.10.01a' )
      write(86,*)'#f,c_source,R_source,delta_source,d_logie,c_sink,R_sink,delta_sink,d_logik'
      c0 = 1.0
      dc = 0.001
      eps = 10.0
      alpha = 1+(eps/1000.0)
      c_source = c0
      c_sink = 0.10
      R_standard = 0.0036765
      R_source = R_standard
      R_sink = R_standard
      d0 = 1000.0*((R_source-R_standard)/R_standard)
      d_logik = 0.0
      do while (c_source > 0.0)
         f = dc/c_source
         R_source = R_source*(((1.0+alpha*R_source)-alpha*f*(1.0+R_source)) &
                    /((1.0+alpha*R_source)-      f*(1.0+R_source)))
         
         R_sink = ( ((R_sink*c_sink)/(1.0+R_sink)) + ((alpha*dc*R_source)/(1.0+alpha*R_source)) )    &
                  /( (       (c_sink)/(1.0+R_sink)) + (      (dc)         /(1.0+alpha*R_source)) )
         delta_source = 1000.0 * ((R_source-R_standard)/R_standard)
         delta_sink = 1000.0 * ((R_sink  -R_standard)/R_standard)
         d_logie = d0 + eps * log(1.0-((c0-c_source)/c0))
         
         if (c0 > c_source) then
            d_logik = ( eps*(1.0-((c0-c_source)/c0))*log(1.0-((c0-c_source)/c0)) ) / ((c0-c_source)/c0)
         endif
         
         write(86,*)f,c_source,R_source,delta_source,d_logie,c_sink,R_sink,delta_sink,d_logik
         c_source = c_source-dc
         c_sink = c_sink+dc
      enddo
      
      return
   end subroutine isotop_no3konsum
end module isotope
