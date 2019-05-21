! total = total + &
!           amount * payments
! ! total = total + amount*payments
!
! program &
!   ContinuationLine
! ! program ContinuationLine

program data_types
  implicit none

  ! character(len = 8) :: DateINFO
  ! character(len = 4) :: Year, Month*2, Day*2
  ! character(len = 10) :: TimeINFO, PrettyTime*12
  ! character(len =2) :: Hour, Minute, Second*6
  !
  ! call DATE_AND_TIME(DateINFO, TimeINFO)

  ! integer :: a, b
  ! integer, parameter :: c = 1
  ! a = 1
  ! b = 2

  ! real :: a
  ! double precision :: b

  ! complex :: a, b
  ! a = (1.0, 0.0)
  ! b = (0.0, 1.0)

  ! logical :: FALSE = .false.
  ! logical :: a
  ! a = .true.

  logical :: a
  integer :: b
  real :: c
  character :: d
  complex :: e

  print *, 'Before assignment'
  print *, 'logical :', a
  print *, 'integer :', b
  print *, 'real :', c
  print *, 'character :', d
  print *, 'complex :', e

  a = .false.
  b = 1
  c = 3.14
  d = 'a'
  e = (1.0, 0.0)

  print *, 'After assignment'
  print *, 'logical :', a
  print *, 'integer :', b
  print *, 'real :', c
  print *, 'character :', d
  print *, 'complex :', e

end program data_types
