    SUBROUTINE MAESTRO(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!          Retreive the time increment as set in the maestro file of openfast
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    use cfuni
    use mod_utils
    implicit none
    INTEGER ISTAT
    CHARACTER*30  CDUM,CID
    REAL TMAX,DT,DT_Out,Tstart
!
    IF (trim(FAST%CMAESTRO(1)).EQ.' ') THEN
        PRINT*,' FASTER ERRROR: Maestro file for Openfast is not given.'
        PRINT*,'                Please provide a valid meastro file in your input file'
        PRINT*,'                with a command like:'
        PRINT*,'                MAESTRO OPENFAST openfast_main_file.fst'
        STOP
    ENDIF
!
!   Open maestro file
    CALL OPEN_ASCII(IBI2,'OLD',FAST%CMAESTRO(1))  
!
    DO 
!               The following parameters cannot be altered in the input files of Faster.
      READ(IBI2,*,END=10) CDUM,CID ! Read from maestro. 
      IF (to_upper(TRIM(CID)).EQ.'TMAX'  ) READ(CDUM,*) TMAX    ! Total run time (s)
      IF (to_upper(TRIM(CID)).EQ.'DT'    ) READ(CDUM,*) DT      ! Recommended module time step (s)
      IF (to_upper(TRIM(CID)).EQ.'DT_OUT') THEN
                                     IF (to_upper(TRIM(CDUM)).EQ.'DEFAULT') THEN
                                          DT_Out=DT
                                     ELSE
                                          READ(CDUM,*) DT_Out   ! Time step for tabular output (s) (or "default")
                                     ENDIF
      ENDIF
      IF (to_upper(TRIM(CID)).EQ.'TSTART') READ(CDUM,*) TStart  ! Time to begin tabular output (s)
    ENDDO
10  CONTINUE    
!
    IF (ABS(DT_Out).LT.1.0e-5) THEN
        PRINT*,' FASTER FATAL ERROR: YOUR TIME STEP (DT_Out) IS TOO LOW.'
        PRINT*,'                     TIME STEP ..',DT_OUT
        STOP
    ENDIF
    IF (TMAX.LT.TStart) THEN
        PRINT*,' FASTER FATAL ERROR: YOUR TOTAL RUN TIME IS LESS THAN THE START TIME.'
        PRINT*,'                     Total run time ................',TMAX
        PRINT*,'                     Time to begin tabular output ..',TStart
        STOP
    ENDIF
    
!   It will now be possible to evaluate the number of lines of the output files..:
    NLINES_OF_OUTUT_FILE=(TMAX-TStart)/DT_Out+NUMCOMM+3  ! NUMCOMM..: Number of initial comment lines in the output files.
!
    CLOSE(IBI2)
    RETURN
    END