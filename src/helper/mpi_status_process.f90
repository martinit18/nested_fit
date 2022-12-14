PROGRAM MPI_STATUS_PROCESS
    USE MPI
    USE MOD_MPI
    IMPLICIT NONE

    INTEGER(4) :: mpi_rank, mpi_cluster_size, mpi_ierror, mpi_istatus(MPI_STATUS_SIZE)
    INTEGER(4) :: parent_nodes, parent_comm
    CHARACTER  :: info_string*256, lines*32
    INTEGER(4) :: last_info_node = 0
    INTEGER(4) :: line_diff = 0
    LOGICAL    :: move_up = .FALSE.

    INTEGER(4) :: i, c
    

    ! Control variable for parents that completed their work
    INTEGER(4) :: work_done = 0

    CALL MPI_INIT(mpi_ierror)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_cluster_size, mpi_ierror)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, mpi_rank, mpi_ierror)
    CALL MPI_Comm_get_parent(parent_comm, mpi_ierror)

    IF((mpi_cluster_size.NE.1).AND.(mpi_rank.EQ.0)) THEN
        WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
        WRITE(*,*) '       ATTENTION:           MPI_STATUS_PROCESS spawned with size != 1.'
        WRITE(*,*) '       ATTENTION:           If you are seeing this warning, there is a bug somewhere. Oops!'
        WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
    ENDIF

    IF(parent_comm.EQ.MPI_COMM_NULL) THEN
        IF(mpi_rank.EQ.0) THEN
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
            WRITE(*,*) '       ATTENTION:           MPI_STATUS_PROCESS not spawned indirectly.'
            WRITE(*,*) '       ATTENTION:           This process cannot be manually executed.'
            WRITE(*,*) '       ATTENTION:           Aborting execution...'
            WRITE(*,*) '------------------------------------------------------------------------------------------------------------------'
        ENDIF
        CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_ierror)
        STOP
    ENDIF

    CALL MPI_RECV(parent_nodes, 1, MPI_INT, 0, MPI_ANY_TAG, parent_comm, MPI_STATUS_IGNORE, mpi_ierror)

    ! Move parent_nodes lines up

    DO i=1,parent_nodes
        PRINT *, 'N#', i-1
    END DO

    WRITE (lines, *) parent_nodes+1
    lines = ADJUSTL(lines)
    PRINT *, ACHAR(27)//"["//TRIM(lines)//"A"

    i = 0
    DO WHILE (work_done.NE.parent_nodes)
        CALL MPI_RECV(info_string, 256, MPI_CHARACTER, MPI_ANY_SOURCE, MPI_ANY_TAG, parent_comm, mpi_istatus, mpi_ierror)

        ! We can use terminal escape codes, since newer windows (>=Win10 I think) support this
        line_diff = last_info_node - mpi_istatus(MPI_SOURCE)

        ! This only works assuming the message has a line span of 1
        IF(line_diff.GT.0) THEN
            ! line_diff lines up
            WRITE (lines, *) line_diff
            lines = ADJUSTL(lines)
            WRITE(*, fmt="(a)", advance='no') ACHAR(27)//"["//TRIM(lines)//"A"
        ELSE IF(line_diff.LT.0) THEN
            ! line_diff lines down
            WRITE (lines, *) ABS(line_diff)
            lines = ADJUSTL(lines)
            WRITE(*, fmt="(a)", advance='no') ACHAR(27)//"["//TRIM(lines)//"B"
        ENDIF

        IF(mpi_istatus(MPI_TAG).EQ.MPI_TAG_SEARCH_DONE) THEN
            ! Green color = finished try
            WRITE(*, fmt="(a)", advance='no') ACHAR(27)//"[32m"

            ! TODO(César): Red color on reaching maxntries before acc. target

            work_done = work_done + 1
        ENDIF
        
        WRITE(*,1) info_string
1       FORMAT(A220)
        WRITE(*, fmt="(a)", advance='no') ACHAR(27)//"[39m"

        ! 1 line up
        WRITE (lines, *) 1
        lines = ADJUSTL(lines)
        WRITE(*, fmt="(a)", advance='no') ACHAR(27)//"["//TRIM(lines)//"A"

        last_info_node = mpi_istatus(MPI_SOURCE)
    END DO

    WRITE (lines, *) parent_nodes - last_info_node
    lines = ADJUSTL(lines)
    ! parent_nodes - last_info_node lines DOWN
    WRITE(*, fmt="(a)", advance='no') ACHAR(27)//"["//TRIM(lines)//"B"

    CALL MPI_FINALIZE(mpi_ierror)

END PROGRAM MPI_STATUS_PROCESS