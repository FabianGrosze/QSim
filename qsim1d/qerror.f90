!> Errormessage and End of Program
!!
!! This subroutine is only used for QSim1D. In QSim3D `subroutine qerror` also
!! exists, which also stops parallel execution.
!!
!! The errormessage is both displayed in the console and written to `file1.err`
!! (Gerris uses this file)
!!
!! @author Michael Sch�nung
subroutine qerror(message)
   use iso_fortran_env,       only: error_unit, output_unit
   use module_model_settings, only: cpfad
   ! TODO (Sch�nung, February 2023):
   ! This is not working on HPC
   !use ifport,                only: sleep
   implicit none
   
   character, intent(in)  :: message*(*) !< text of errormessage 
   integer                :: file1
   
   ! --- console-output ---
   ! This ensures that the error message does not interfere with regular
   ! QSim output prior to the error.
   flush(output_unit)
   call sleep(1)
   
   write(error_unit,*) repeat('=', 78)
   write(error_unit,*) repeat(' ', 32), '!!! ERROR !!!'
   write(error_unit,*) repeat('=', 78)
   write(error_unit,*) trim(message)
   
   ! --- write to file1.err ---
   ! Gerris needs this file. It reads the error message from there
   ! and displays it to the user.
   open(newunit  = file1,                               &
        file     = trim(adjustl(cpfad)) // 'file1.err', &
        encoding = 'UTF-8')
   rewind(file1)
   write(file1,*) trim(message)
   close(file1)
   
   stop 'QSim terminated with an error!'
end subroutine qerror