! module module_name
!   [ body 1 ]
!   contains
!
!   [ body 2 ]
! end module module_name
!
! module can be used in the body of the program by giving
!
!   use module_name
!
! command at the top of the program, function, or subroutine.

module my_module
  contains
    subroutine my_subroutine()
      print *, "Hello World!!!"
    end subroutine my_subroutine
end module my_module

program main
  use my_module
  call my_subroutine()
end program main
