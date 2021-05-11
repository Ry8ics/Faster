    MODULE MOD_CHECK_AND_WAIT
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!          Module that should be obsolete... 
!          Just to keep Faster in sleep mode while Openfast/Turbsim is running.        
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    IMPLICIT NONE 
    PRIVATE 
    PUBLIC :: CHECK_AND_WAIT_FAST  ! put the program is sleep mode while openfast is running
    PUBLIC :: CHECK_AND_WAIT_TURB  ! put the program is sleep mode while turbsim is running
    CONTAINS 
!
!--------------------------------------------------------------------------------------  
    SUBROUTINE CHECK_AND_WAIT_FAST(CFILE)
!
!   Routine to ensure that openfast will finish its work, i.e. the output file is created.
!
!   This routine will just keep the program occupied until openfast is finished.
!   This routine should actually be obsolete. The reason why it's needed is that when
!   a batch of openfast analyses are executed with EXECUTE_COMMAND_LINE there's is no 
!   way to keep the program waiting till the analyses have finished. It would just start
!   another batch of analyses. This is in general OK as several openfast analyses than
!   number of cores may be run in parallel. However, it's not a good practice, and it will
!   not speed up the analyses, it will actually slow it down, and eventually the computer
!   will hang, and you will be frustrated, and eventually you will find that you should 
!   keep the number of analyses down, probably because somebody told you to, or that I
!   would need to warn about it, which I now don't have to, because I've made this routine
!   that will keep the program occupied until openfast has finished its work, but if 
!   there is a way or a workaround that would make this stupid routine obsolete I would be 
!   be happy, and I would not need to write all this crap. There might however be an obscure 
!   routine in psapi that could be used, or some c-routine (check clib), where the real problem is that 
!   windows just keep swallowing everything that it's being thrown at it and you can't halt
!   a process, there should be a command like: "put this program in sleepmode while openfast is running" 
!   That would be a nice.
!   
    use ifport  ! give access to the sleep function
    use cfast   ! give access to NLINES_OF_OUTUT_FILE
    use mod_utils
!
    IMPLICIT NONE
    CHARACTER*(*) CFILE   ! Keep the program occupied while some other process is creating CFILE.
    INTEGER IDELAY,ICOUNT,NLINES
    LOGICAL LEXIST,LCREAT
    ICOUNT=0
    IDELAY=5     ! IDELAY IN SECONDS. Note it's an integer. Adjust as needed...
    LEXIST=.FALSE.
    DO WHILE (.NOT. LEXIST)
!      The output file is not opened yet
       INQUIRE (FILE=CFILE,EXIST=LEXIST)    ! Check if file exists
       IF (.NOT. LEXIST) CALL SLEEP(IDELAY) ! Sleep for IDELAY seconds and wait for it to open       
       ICOUNT=ICOUNT+1
       write(*,'(a,i6,2x,a)') ' Waiting to open the output file....',icount,TRIM(CFILE)
       IF (ICOUNT.GT.INT(14400/IDELAY)) THEN ! Effectively relying on the file to open.. sooner or later.
           WRITE(*,*) ' Faster warning: File seems not to open ',trim(cfile)
       ENDIF
    ENDDO
!
    ICOUNT=0
!    IDELAY=10     ! IDELAY IN SECONDS. Note it's an integer. Adjust as needed...
    IDELAY=5      ! IDELAY IN SECONDS. Note it's an integer. Adjust as needed...
    LCREAT=.FALSE. 
    DO WHILE (.NOT.LCREAT)      
       NLINES=GETNLINES(CFILE)   ! Number of lines in current output file.. will probably grow
       IF (NLINES.LT.NLINES_OF_OUTUT_FILE) THEN  ! check if goal is reached
         CALL SLEEP(IDELAY)      ! Sleep for IDELAY seconds  
       ELSE
         LCREAT=.TRUE.           ! The output file is finished.. so should openfast  
!!         CALL SLEEP(IDELAY)      ! However, allow for a short nap for openfast to properly finish
       ENDIF
       ICOUNT=ICOUNT+1
       IF (ICOUNT.GT.INT(28800/IDELAY)) THEN
           WRITE(*,*) ' Faster warning: File seems not to grow ',trim(cfile)
       ENDIF
       write(*,'(a,i6,2x,a,2x,i6,a,2x,i6)')' Writing output file ',icount,trim(cfile),nlines,' out of ',NLINES_OF_OUTUT_FILE
    ENDDO
    RETURN
    END
!
!--------------------------------------------------------------------------------------  
    SUBROUTINE CHECK_AND_WAIT_TURB(CFILE)
!
!   Routine to ensure that turbsim will finish its work, i.e. the output file is created.
!   This routine is a slight variation of CHECK_AND_WAIT_FAST (and it's simpler).
!   It relies on the fact that the size of the file will grow between each call to
!   INQUIRE (FILE=CFILE,SIZE=NSIZE). But it's not foolproof as the size may not grow between each call,
!   i.e. stay constant (if say the process is hanging for some reason) in which case this 
!   routine thinks that the file is created and the process has finished its work (which is not true).
!   This routine may fail, i.e. Openfast will be start to execute before Turbsim is finished.
!
    use ifport  ! give access to the sleep function
!
    IMPLICIT NONE
    CHARACTER*(*) CFILE   ! Keep the program occupied while some other process is creating CFILE.
    INTEGER IDELAY,ICOUNT,NSIZE,NPREV
    LOGICAL LEXIST,LCREAT
    ICOUNT=0
    IDELAY=5     ! IDELAY IN SECONDS. Note it's an integer. Adjust as needed...
    LEXIST=.FALSE.
    DO WHILE (.NOT. LEXIST)
!      The output file is not opened yet
       INQUIRE (FILE=CFILE,EXIST=LEXIST)    ! Check if file exists
       IF (.NOT. LEXIST) CALL SLEEP(IDELAY) ! Sleep for IDELAY seconds and wait for it to open       
       ICOUNT=ICOUNT+1
       print*,' Waiting to open the output file....',icount,TRIM(CFILE)
       IF (ICOUNT.GT.INT(14400/IDELAY)) THEN ! Effectively relying on the file to open.. sooner or later.
           WRITE(*,*) ' Faster warning: File seems not to open ',trim(cfile)
       ENDIF
    ENDDO
!
    ICOUNT=0
    NPREV=0
    IDELAY=10     ! IDELAY IN SECONDS. Note it's an integer. Adjust as needed...
    LCREAT=.FALSE. 
    DO WHILE (.NOT.LCREAT)      
       INQUIRE (FILE=CFILE,SIZE=NSIZE)
       IF (NSIZE.GT.0 .AND. NSIZE .EQ. NPREV) LCREAT=.TRUE.  ! Is the file still growing?  
       IF (.NOT.LCREAT)  CALL SLEEP(IDELAY)                  ! Sleep for IDELAY seconds  
       ICOUNT=ICOUNT+1
       IF (ICOUNT.GT.INT(28800/IDELAY)) THEN
           WRITE(*,*) ' Faster warning: File seems not to grow ',trim(cfile)
       ENDIF
       print*,' Writing output file ',icount,trim(cfile)
       NPREV=NSIZE  
    ENDDO
    RETURN
    END
!...................................................................
    END MODULE MOD_CHECK_AND_WAIT  
    


