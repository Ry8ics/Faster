    SUBROUTINE CLEAN_UP(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!        Clean up mess, i.e. delete all slave files etc...
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use clist
    implicit none
    CHARACTER*100 CCDEL
    CHARACTER*144 CMSG
    INTEGER ISTAT,ESTAT
    
!   Clean up... i.e. delete all the messy files...
    CCDEL='set mydir=%cd% & cd .. & del *_'//CANA//'_*.* /S /Q & cd %mydir%'
!   
    CALL EXECUTE_COMMAND_LINE (trim(CCDEL),wait=.true.,EXITSTAT=ESTAT, &
                              CMDSTAT=ISTAT, CMDMSG=CMSG)
!   Make a bet that this will work...
!   IF (ISTAT > 0) THEN 
!        PRINT *, 'Command execution failed with error ', TRIM(CMSG)
!        PRINT *, 'A fatal error occured: Contact support'
!        STOP
!   ELSEIF (ISTAT < 0) THEN
!       PRINT *, 'Command execution not supported'
!       PRINT *, 'A fatal error occured: Contact support'
!       STOP
!    ELSEIF (ESTAT > 0) THEN
!       PRINT *, 'Faster error. Not able to delete files.'
!       STOP   
!    ENDIF
    RETURN
    END