SUBROUTINE HALT_EXECUTION()
USE MOD_PERFPROF

#ifdef OPENMPI_ON
    CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_ierror) ! Gracefully shutdonw MPI
#endif

#ifdef PPROF
    CALL SHUTDOWN_PERF_PROF() ! Gracefully shutdown performance profiling
#endif

    STOP
END SUBROUTINE
