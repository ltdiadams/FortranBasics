! A namelist combines several related variables (referred to as 'members' of the namelist)
! together, which are then read with a single statement. The members can appear in
! any order.
! Takes the following format:
!
! &GROUP_NAME
!   char_variable = "a char variable",
!   logical_variable = T,
!   nitems = 5,
!   list_variable = 0.1 0.2 0.3 0.4 0.5
! /

program namelist_input_file

  use iso_fortran_env

  implicit none

  integer :: momentum, nelectrons, charge
  real :: mass

  namelist /molecule/ momentum, mass, nelectrons, charge

  read(INPUT_UNIT, molecule)

  print molecule

end program namelist_input_file
