!  Faster.f90 
!
!  FUNCTIONS:
!  Faster - Entry point of console application.
!
!****************************************************************************
!
!  PROGRAM: Faster
!
!  PURPOSE:  Generate input files to Openfast and Turbsim and run the analyses 
!            in parallel. Listing of statistics and time series results in a 
!            plotting friendly format as required.
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!  
!****************************************************************************
    PROGRAM FASTER
    use cfast
    use clist
    IMPLICIT NONE
!
    integer istat,nitem
    ISTAT=0
    LIST%MOOR=.FALSE.
    FAST%CMAESTRO(1)=' '                          ! Dummy file for openfast maestro file
    FAST%CMAESTRO(2)='Maestro_turbsim_dummy.inp'  ! Dummy file for turbsim maestro file
!
50 CALL MEMALLOC(ISTAT)         ! Allocate memory  
! 
!   Routine for user input
    CALL DCMAIN(ISTAT)
    IF ( ISTAT.EQ.-1 ) STOP
    IF (ISTAT.EQ.-2) GOTO 50    ! Allocate more memory and start all over.
!
    IF (CLEAN_UP_MESS) CALL CLEAN_UP(ISTAT)   ! Clean up mess
!
    IF (.NOT.LANAL) GOTO 100   ! Perform the analyses? (openfast,turbsim etc..)
!
!   Get time increment as set in the maestro file of openfast
!   This is needed to evaluate the size of the output files
!
    CALL MAESTRO(ISTAT)
    IF ( ISTAT.EQ.-1 ) STOP   
!
    CALL MASTERS(ISTAT)       ! Update the master files
    IF ( ISTAT.EQ.-1 ) STOP   
!
    CALL SLAVES(ISTAT)        ! Update the slave files
    IF ( ISTAT.EQ.-1 ) STOP    
!
    CALL SWAP_STRING(ISTAT)   ! Swap strings in the slave files
    IF ( ISTAT.EQ.-1 ) STOP    
!
    IF (.NOT.LSIMUL) GOTO 200 ! Perform simulation ?
    CALL CALC(ISTAT) 
    IF ( ISTAT.EQ.-1 ) STOP 
!   
    CALL STORE_RESULTS(ISTAT) ! Store results on binary files for later access        
    IF ( ISTAT.EQ.-1 ) STOP 
!   
!   Check if mooring line results must also be stored..
!   Results from Moordyn when the output flags pt is set in the Moordyn input file.
    IF (MOOR_LINES.GT.0) CALL STORE_MOOR(ISTAT)
    IF ( ISTAT.EQ.-1 ) STOP 
!------------------------------------------
!   We now hopefully got some results, lets go on an open the binary files.
!   This might become a bit tricky.. the files are binary and the record lenghts are needed.
100 CONTINUE
!
    CALL PFOPEN()       ! Open the result files
!
    CALL MEMALY(ISTAT)  ! Allocate memory needed for any result, memaly?? well, why not?
!
!   Routine for user input (what is to be reported)
    CALL DCOUT(ISTAT)
    IF ( ISTAT.EQ.-1 ) STOP
!
!   Generate appropriate output
    CALL OUTPUT(ISTAT) 
    IF ( ISTAT.EQ.-1 ) STOP    
!
200 END PROGRAM FASTER