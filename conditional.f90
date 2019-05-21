! program conditional
!
!   implicit none
!   integer :: val
!
!   val = .true.
!
!   if( val ) then
!     print *, 'True value'
!   else
!     print *, 'False value'
!   endif
!
! end program conditional
!-------------------------------------------------------------------------------
! program loop
!
!   !do counter = initial, final, step
!   ! [ loop body ]
!   !end do
!
!   integer :: max = 10
!   integer :: i
!
!   do i=1, max
!     print *, 'Interation i=', i
!   enddo
! end program loop
!-------------------------------------------------------------------------------
! program function
!   implicit none
!   integer :: val = -4
!   logical :: res = .true.
!
!   res = sign_test(val)
!   print *, 'Is this value positive? : ', res
!
! contains
!
!   logical function sign_test(input) result(output)
!     integer :: input
!     print *, 'Enter a number: '
!     read (*,*) input
!     if( input >= 0) then
!       output = .true.
!     else
!       output = .false.
!     endif
!   end function sign_test
!
! end program function
!-------------------------------------------------------------------------------
! program subroutine
!
!   implicit none
!   integer :: i, j
!
!   i = 1
!   j = 3
!   call print_pair(i, j)
!
! end program subroutine
!
! subroutine print_pair(a,b)
!   integer, intent(in) :: a,b
!   print *,'Pair of numbers : (',a,',',b,')' !the commas are REQUIRED
! end subroutine print_pair
!
! program do_loop
!
!   implicit none
!
!   integer :: i, start_i, end_i, step
!
!   start_i = 10
!   end_i = 1
!   step = -1
!
!   do i=start_i, end_i, step
!     print *, real(i)**2
!   end do
!
! end program do_loop
!-------------------------------------------------------------------------------
! program while_loop
!
!   implicit none
!
!   integer :: input, idx, factorial
!
!   input = 5
!   idx = input
!
!   factorial = 1
!   do while (idx > 0)
!
!     factorial = factorial * idx
!     print *, idx, factorial
!     idx = idx - 1
!
!   end do
!
!   print *, 'Value of factorial ', input, "! = ", factorial
!
! end program while_loop

!-------------------------------------------------------------------------------
! program loop_break
!
!   implicit none
!
!   integer :: i, total
!   integer, parameter :: max_value = 50
!
!   total = 0
!
!   do i=1, 10
!
!     total = total + 10
!     if( total > max_value ) exit
!     print *, i , total
!   end do
!
!   print *, 'final total value : ', total
!
! end program loop_break

!-------------------------------------------------------------------------------
! program until_loop

!   ! modification of infinite do loop

!   implicit none
!
!   integer :: i
!
!   i = 0
!   do
!     print *, i
!     i = i + 1
!
!     !condition check at the bottom of the loop
!     if ( .not.(i < 5) ) exit
!   end do
!
! end program until_loop

!-------------------------------------------------------------------------------
! ! forall ( var=initial_value:final_value[:stride])
! !   [ body ]
! ! end forall
!
! program forall_example
!
!   implicit none
!
!   real, allocatable, dimension(:,:) :: A
!   integer :: n, i
!
!   n = 100
!
!   allocate( A(n,n) )
!
!   forall( i=1:n ) A(i,i) = 1.0 ! when only one line don't need to 'end forall'
!
!   deallocate(A)
!
! end program forall_example

! program random_num
!
!   implicit none
!
!   real :: val
!
!   call random_number(val)
!
!   if (val > 0.5) then
!     print *, 'Value larger than 0.5 : ', val
!   else
!     print *, 'Value smaller than 0.5 : ', val
!   end if
!
! end program random_num

!-------------------------------------------------------------------------------
! Case Conditional
! if (var == val1) then
!   [ block 1 ]
! else if (var = val2) then
!   [ block 2 ]
! else if (var == val3) then
!   [ block 3 ]
! else
!   [ block 4 ]
! end if
!
! Can be replaced with 'case' conditional
!
! select case (var)
!   case (val1)
!     [ block 1 ]
!   case (val2)
!     [ block 2 ]
!   case (val3)
!     [ block 3 ]
!   case default
!     [ block 4 ]
! end select

program case_condition

  implicit none

  integer :: val

  val = 10

  select case (val)
  case (1)
    print *, 'One', val
  case (2)
    print *, 'Two', val
  case (3)
    print *, 'Three', val
  case default
    print *, 'Value different than 1,2,3 : ', val
  end select
  
end program case_condition
