program assign04
use module_reader
use module_statistic
implicit none
INTEGER :: nrec
character(len=*), parameter :: file_name = './assign04/pr_m.dat'
REAL, ALLOCATABLE, DIMENSION(:) :: arr
! Read the number of lines from the file: use the module_reader
! Allocate space for storage of the data
! Read the data from the file: use the module_reader
! Compute the data statistics: all the one in the module_statistic
! print out the results


nrec = file_nrecords(file_name)

PRINT*,'Number of lines=',nrec

ALLOCATE(arr(nrec))

CALL file_load_records(file_name,arr,nrec)

PRINT*,'Mean=',mean(arr)
PRINT*,'Std dev=',stddev(arr)
PRINT*,'90th percentile=',pct90(arr)
PRINT*,'95th percentile=',pct95(arr)
PRINT*,'99th percentile=',pct99(arr)


end program assign04
