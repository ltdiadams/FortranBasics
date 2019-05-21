program derive_data_type

  ! type declaration
  type Books
    character(len = 50) :: title
    character(len = 50) :: author
    character(len = 150) :: subject
    integer :: book_id
  end type Books

  ! ! declaring type variables
  ! type(Books) :: book1
  ! type(Books) :: book2
  !
  ! !accessing the components of the structure
  !
  ! book1%title = "C Programming"
  ! book1%author = "Me"
  ! book1%subject = "C Programming Tutorial"
  ! book1%book_id = 12345
  !
  ! book2%title = "Harry Potter"
  ! book2%author = "Me also"
  ! book2%subject = "Voldemort"
  ! book2%book_id = 54321
  !
  ! !display book info
  !
  ! print *, book1%title
  ! print *, book1%author
  ! print *, book1%subject
  ! print *, book1%book_id
  !
  ! print *, book2%title
  ! print *, book2%author
  ! print *, book2%subject
  ! print *, book2%book_id

  ! OR................................................................

  type(Books), dimension(2) :: list

  !accessing the components of the structure

  list(1)%title = "C Programming"
  list(1)%author = "Me"
  list(1)%subject = "C Programming Tutorial"
  list(1)%book_id = 12345

  list(2)%title = "Harry Potter"
  list(2)%author = "Me also"
  list(2)%subject = "Voldemort"
  list(2)%book_id = 54321

  !display book info

  print *, list(1)%title
  print *, list(1)%author
  print *, list(1)%subject
  print *, list(1)%book_id

  print *, list(2)%title
  print *, list(2)%author
  print *, list(2)%subject
  print *, list(2)%book_id


end program derive_data_type
!-------------------------------------------------------------------------------

! program derived_type ! also called a structure
!                      ! used to represent a record
!
!   implicit none
!
!   ! type type_name
!   !   declarations
!   ! end type
!
!
!   type Matrix
!
!     integer :: num_cols = 0
!     integer :: num_rows = 0
!
!     real, dimension(:,:), allocatable :: elements
!
!   end type Matrix
!
!   integer :: n, matrix_size
!   integer :: i,j,k
!   type(Matrix), dimension(:), allocatable :: A
!
!   n = 4
!
!   allocate( A(n) )
!
!   do i=1, n
!
!     allocate( A(i)%elements(n,n) )
!
!     A(i)%num_cols = i
!     A(i)%num_rows = i
!
!     A(i)%elements = real(i)
!
!   end do
!
!   do i=1, n
!     print *, 'Element ', i
!
!     do j=1, A(i)%num_rows
!       print *, ( A(i)%elements(j,k), k=1, A(i)%num_cols)
!     end do
!
!   end do
!
!   do i=1, n
!     deallocate( A(i)%elements )
!   end do
!
!   deallocate( A )
!
! end program derived_type

!-------------------------------------------------------------------------------
