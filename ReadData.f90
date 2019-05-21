! module real_precision
!   implicit none
!
!   integer, parameter :: sp = selected_real_kind(6)
!   integer, parameter :: dp = selected_real_kind(15)
!
! end module real_precision
!
! program ReadFile
!
!   use real_precision
!
!   implicit none
!
!   !Read data from a file called mydata.txt
!   !real :: x, y, z
!
!   !open(integer unit_num, string file_name, string file_type, string file_status [[new/old/replace]])
!   !open(unit=1, file="mat.txt", form="formatted", status="old", action="read")
!
!   real(sp) :: a = 1.0_sp
!   real(dp) :: b = 1.0_dp
!
!   print *, a
!   print *, b
!
!   !close(unit=unit_number)
!
! end program ReadFile

program in_rule

  ! integer (name starts with a letter between I and N)

  ! use, intrinsic :: iso_fortran_env
  !
  ! implicit none
  !
  ! integer, parameter :: sp = REAL32
  ! integer, parameter :: dp = REAL64
  ! integer, parameter :: qp = REAL128
  !
  ! real :: i, a


  i = 1
  ! print *, '',i,''
  print *, i

  ! real (starting with all other letters)
  a = 3
  ! print *, '',a,''
  print *, a

end program
