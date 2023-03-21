MODULE MOD_MPI
    ! Module for program mpi control data shared in the program
    ! Non-control data is still on the other modules
    ! TODO(César): Move all the mpi data declarations here
  
    IMPLICIT NONE
    INTEGER(4), PARAMETER :: MPI_TAG_SEARCH_STATUS = 7
    INTEGER(4), PARAMETER :: MPI_TAG_SEARCH_DONE_OK = 8
    INTEGER(4), PARAMETER :: MPI_TAG_SEARCH_ERROR_MAXED_OUT = 9
    INTEGER(4), PARAMETER :: MPI_TAG_SEARCH_DONE_MANY_TRIES = 10
    INTEGER(4) :: mpi_child_writter_comm
  
END MODULE MOD_MPI