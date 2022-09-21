module module_sorter

  use , intrinsic :: iso_fortran_env

  implicit none
  private

  public :: sort

  contains

  subroutine sort(x)
    implicit none
    real, dimension(:) , intent(inout) :: x
    integer :: istart, istop
    istart = 1
    istop = size(x)
    if ( istop < 2 ) return
    if ( all(x == x(1)) ) return
    call quicksort_real(x,istart,istop)
  end subroutine sort

  recursive subroutine quicksort_real(x,first,last)
    implicit none
    real, dimension(:) , intent(inout) :: x
    integer , intent(in) :: first, last
    real :: pivot, temp
    integer :: left, right

    if ( first >= last ) return
    pivot = x((first+last)/2)
    left = first
    right = last

    do while ( left <= right )
      do while ( x(left) < pivot )
        left = left + 1
      end do
      do while ( x(right) > pivot )
        right = right - 1
      end do
      if ( left <= right ) then
        temp = x(left)
        x(left) = x(right)
        x(right) = temp
        left = left + 1
        right = right - 1
      end if
      call quicksort_real(x,first,right)
      call quicksort_real(x,left,last)
    end do
  end subroutine quicksort_real

end module module_sorter

!program testqsort
!  use module_sorter , only : sort
!  implicit none
!
!  real, dimension(11) :: x = [ -1, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 ]
!
!  print *, x
!  call sort(x)
!  print *, x
!
!end program testqsort
