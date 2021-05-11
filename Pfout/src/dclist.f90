      SUBROUTINE DCLIST(Cerr,Citem,Istat)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Decode list of user defined data
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      use clist
      IMPLICIT NONE
      INCLUDE 'redwri'
      CHARACTER*(*) Cerr , Citem
      LOGICAL MATCH
      INTEGER iend , IFERR , Istat
!
      IF ( MATCH('CHANNEL',2) ) THEN    
         CALL DCCHANNEL(cerr,citem,Istat)
         IF ( ISTAT.EQ.-1 ) GOTO 200     
      ELSEIF (MATCH('LOAD',3)) THEN
         CALL DCLOAD(cerr,citem,Istat)     ! Activate a list of loads, i.e runs...
         IF ( ISTAT.EQ.-1 ) GOTO 200  
      ELSEIF (MATCH('STAT',3)) THEN
         CALL DCSTAT(cerr,citem,Istat)
      ELSEIF (MATCH('ALL',3) ) THEN     ! already implemented elsewhere,,,...
!        basically regenerating the openfast result files..
!        In case they are deleted... 
!        Everything should be stored on the binary files.
         CALL PRIERR()
         WRITE (IOUT,CERR) 'Not yet implemented' , &
                            'PROGRAM STOPPED'
         ISTAT = -1
         GOTO 200
      ELSEIF (MATCH('MOORING',3)) THEN
!        Moordyn results.. MORE DETAILS ARE NEEDED HERE...++
         LIST%MOOR=.TRUE.         
      ELSE
         CALL PRIERR()
         WRITE (IOUT,CERR) 'CHANNEL STAT OR MOORING EXPECTED' , &
                            'PROGRAM STOPPED'
         ISTAT = -1
         GOTO 200
      ENDIF     
200   RETURN
      END
