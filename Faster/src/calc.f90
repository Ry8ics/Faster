    SUBROUTINE CALC(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!           Perform the analysis (turbsim,openfast,...)
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    use clist
    use mod_time
    use mod_utils
    use mod_check_and_wait  
    use ifport
    implicit none
!
    INTEGER ISTAT,IPAR,ESTAT
    CHARACTER*144 CMSG
    CHARACTER*136 FINP,FPRE
    CHARACTER*136 FOUT(128)  ! ASSUME MAX NUMBER OF CORES=128  ... OR MAKE IT ALLOCATEABLE
    CHARACTER*4 CEXT1,CEXT2,CEXTT,CNUM
    integer ista,iend
    integer OMP_GET_MAX_THREADS,ncores
    real*8 RT0,CT0
    integer nrange,ir
    CHARACTER*1360 cexp
    CHARACTER*20  CSTART,CPROG,CMDid
    CHARACTER*200 CPROC
    integer statio
    istat=0 
    CEXT1=FAST%CMAESTRO(1)(LEN(TRIM(FAST%CMAESTRO(1)))-3:)
    CEXT2=FAST%CMAESTRO(2)(LEN(TRIM(FAST%CMAESTRO(2)))-3:)
!
! Start the watches, store later
!--------------------------------
     RT0 = RealTime()
     CT0 = CPUTIME()    
!
    NCORES=OMP_GET_MAX_THREADS()       ! maximum number of threads available
    IF (NCORES.GT.1) NCORES=NCORES-1   ! Leave one spare core for other work, e.g. for this program
!                                      ! This will also keep the computer from freezing and it should
!                                      ! still be able to use it for some other work, e.g. writing these lines
!                                      ! which I did while openfast was running. This program is also
!                                      ! running along with openfast, altough it's mostly in sleep mode.
!=======================================================================
    IF (trim(FAST%CMAESTRO(2)).EQ.'Maestro_turbsim_dummy.inp') THEN
        PRINT*,' MESSAGE : Maestro file for Turbsim is not given.'
        PRINT*,'           Turbsim analyses will not be performed.'
        GOTO 100
    ENDIF
!
!   Turbsim analysis...
    CSTART=' start /b '
    CPROG=' turbsim ' 
    CEXTT='.bts'       ! extension of output file from turbsim
    NRANGE=CEILING(real(NPARA)/real(NCORES))    ! Number of ranges, i.e. batch of parallel analyses
!
    DO IR=1,NRANGE
        CEXP=''
        CALL SUBPROK(NPARA,NCORES,IR,NRANGE,ISTA,IEND) ! get batch, ista to iend.
        DO IPAR=ISTA,IEND
            WRITE(CNUM,'(I4)') IPAR          
            FPRE=fast%cmaestro(2)(1:len(trim(fast%cmaestro(2)))-4)// &  
                  '_'//CANA//'_'//trim(adjustl(CNUM))
            FINP=trim(FPRE)//CEXT2         ! Input file should already exist
            FOUT(IPAR)=trim(FPRE)//CEXTT   ! Output file that will be created 
            open(unit=2821, iostat=statio, file=FOUT(IPAR), status='old')  
            if (statio == 0) close(2821, status='delete')   ! Delete any existing output file                    
            CMDid='TURB'//trim(adjustl(CNUM))
            CPROC=trim(CSTART)//' "'//TRIM(CMDid)//'"'//' '//trim(CPROG)//' '//trim(FINP)//' &'
            WRITE(CEXP,'(a)') cexp(1:LEN(TRIM(cexp)))//TRIM(CPROC)
        ENDDO
!
!       Analyse the batch ista to iend in parallel that will create the output files.
        CALL EXECUTE_COMMAND_LINE (trim(CEXP),wait=.true.,EXITSTAT=ESTAT, &
                                   CMDSTAT=ISTAT, CMDMSG=CMSG)
        IF (ISTAT > 0) THEN 
             PRINT *, 'Command execution failed with error ', TRIM(CMSG)
             PRINT *, 'A fatal error occured: Contact support'
             STOP
        ELSEIF (ISTAT < 0) THEN
            PRINT *, 'Command execution not supported'
            PRINT *, 'A fatal error occured: Contact support'
            STOP
         ELSEIF (ESTAT > 0) THEN
            PRINT *, 'Faster error. Not able to run Turbsim.'
            PRINT *, 'Maestro file..: ',trim(fast%cmaestro(2))
            STOP   
         ENDIF
!         call ProcessStatus()
!         stop
!        print*,'Running ... ',IsProgramRunning(processID, ProgramName, Len)
         DO IPAR=ISTA,IEND
            CALL CHECK_AND_WAIT_TURB(FOUT(IPAR))  ! wait for the batch of processes to create the output files.
         ENDDO
    ENDDO
!
    print*,' real time turbsim...', RealTime()-RT0
!             call ProcessStatus()
!=======================================================================
!   Openfast analysis...

100 RT0 = RealTime()   ! reset the clock
    CSTART=' start /b '
    CPROG=' Openfast ' 
    CEXTT='.out'       ! extension of output file from openfast
    NRANGE=CEILING(real(NPARA)/real(NCORES))    ! Number of ranges, i.e. batch of parallel analyses
!
    DO IR=1,NRANGE
        CEXP=''
        CALL SUBPROK(NPARA,NCORES,IR,NRANGE,ISTA,IEND) ! get batch, ista to iend.
        DO IPAR=ISTA,IEND
            WRITE(CNUM,'(I4)') IPAR          
            FPRE=fast%cmaestro(1)(1:len(trim(fast%cmaestro(1)))-4)// &  
                  '_'//CANA//'_'//trim(adjustl(CNUM))
            FINP=trim(FPRE)//CEXT1         ! Input file should already exist
            FOUT(IPAR)=trim(FPRE)//CEXTT   ! Output file that will be created 
            open(unit=2821, iostat=statio, file=FOUT(IPAR), status='old')  
            if (statio == 0) close(2821, status='delete')   ! Delete any existing output file                    
            CMDid='FAST'//trim(adjustl(CNUM))
            CPROC=trim(CSTART)//' "'//TRIM(CMDid)//'"'//' '//trim(CPROG)//' '//trim(FINP)//' &'
            WRITE(CEXP,'(a)') cexp(1:LEN(TRIM(cexp)))//TRIM(CPROC)
        ENDDO
!
!       Analyse the batch ista to iend in parallel that will create the output files.
        CALL EXECUTE_COMMAND_LINE (trim(CEXP),wait=.true.,EXITSTAT=ESTAT, &
                                   CMDSTAT=ISTAT, CMDMSG=CMSG)
        IF (ISTAT > 0) THEN
             PRINT *, 'Command execution failed with error ', TRIM(CMSG)
             PRINT *, 'A fatal error occured: Contact support'
             STOP
        ELSEIF (ISTAT < 0) THEN
            PRINT *, 'Command execution not supported'
            PRINT *, 'A fatal error occured: Contact support'
            STOP
         ELSEIF (ESTAT > 0) THEN
            PRINT *, 'Faster error. Not able to run Openfast.'
            PRINT *, 'Maestro file..: ',trim(fast%cmaestro(1))
            STOP   
         ENDIF
         DO IPAR=ISTA,IEND
            CALL CHECK_AND_WAIT_FAST(FOUT(IPAR))  ! wait for the batch of processes to create the output files.
         ENDDO
    ENDDO
!
    print*,' real time openfast...', RealTime()-RT0  
    RETURN
    END