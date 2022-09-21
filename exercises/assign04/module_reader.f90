module module_reader

  use iso_fortran_env

  implicit none

  private

  public :: file_nrecords , file_load_records

  contains

  integer function file_nrecords(fname) result(nrecs)
    implicit none
    character(len=*), intent(in) :: fname
    integer :: ierr, iunit

    open(newunit=iunit, file=fname, status='old', &
        form='formatted', iostat=ierr, action='read')
    if ( ierr /= 0 ) then
      write(output_unit,*) 'Error opening file ',trim(fname)
      stop 'I/O erorr'
    end if
    nrecs = 0
    ierr = 0
    read(iunit, *, iostat=ierr)
    do while ( ierr == 0 )
      nrecs = nrecs + 1
      read(iunit, *, iostat=ierr)
    end do
    close(iunit)
  end function file_nrecords

  subroutine file_load_records(fname,x,nr)
    implicit none
    character(len=*), intent(in) :: fname
    integer, intent(in) :: nr
    real, dimension(:) , intent(inout) :: x
    integer :: i, rr
    integer :: ierr, iunit
    rr = min(nr,size(x))
    open(newunit=iunit, file=fname, status='old', &
        form='formatted', iostat=ierr, action='read')
    if ( ierr /= 0 ) then
      write(output_unit,*) 'Error opening file ',trim(fname)
      stop 'I/O erorr'
    end if
    do i = 1, rr
      read(iunit, *, iostat=ierr) x(i)
    end do
    close(iunit)
  end subroutine file_load_records

end module module_reader

!program test
!  use module_reader
!  implicit none
!  integer :: nrec
!  real, allocatable, dimension(:) :: temperature
!  nrec = file_nrecords('myfile')
!  allocate(temperature(nrec))
!  call file_load_records('myfile',temperature,nrec)
!end program test
