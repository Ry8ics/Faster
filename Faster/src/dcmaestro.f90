      SUBROUTINE DCMAESTRO(Cerr,Citem,Istat)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!        Read maestro files (openfast and possibly turbsim)
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      use cfast
      IMPLICIT NONE
      INCLUDE 'redwri'
      CHARACTER*(*) Cerr , Citem
      LOGICAL MATCH ,LEXIST
      integer iferr,istat
      integer ilen,iend
!
!.................................................
!     Read variable id to be updated in the slave files
      IF ( MATCH('OPENFAST',3) ) THEN
         CALL DCSTRG(FAST%CMAESTRO(1),ilen,3)
         IF ( IFERR().LT.0 ) THEN
          CALL PRIERR()
          WRITE (IOUT,CERR) 'Error reading Openfast meastro.' , &
                            'PROGRAM STOPPED'
          ISTAT = -1
          GOTO 200
         ENDIF   
!
         INQUIRE(FILE=FAST%CMAESTRO(1),EXIST=LEXIST)         
         IF (.NOT. LEXIST) THEN
               CALL PRIERR()
               WRITE(*,*)' Maestro file...: ',TRIM(FAST%CMAESTRO(1))
               WRITE (IOUT,CERR) 'Requested Openfast maestro file does not exist.'   
               ISTAT=-1
               GOTO 200    
         ENDIF   
         
      ELSEIF ( MATCH('TURBSIM',3) ) THEN
         CALL DCSTRG(FAST%CMAESTRO(2),ilen,3)
         IF ( IFERR().LT.0 ) THEN
          CALL PRIERR()
          WRITE (IOUT,CERR) 'Error reading Turbsim meastro.' , &
                            'PROGRAM STOPPED'
          ISTAT = -1
          GOTO 200
         ENDIF 
!
         INQUIRE(FILE=FAST%CMAESTRO(2),EXIST=LEXIST)         
         IF (.NOT. LEXIST) THEN
               CALL PRIERR()
               WRITE(*,*)' Maestro file...: ',TRIM(FAST%CMAESTRO(2))
               WRITE (IOUT,CERR) 'Requested Turbsim maestro file does not exist.'   
               ISTAT=-1
               GOTO 200    
         ENDIF   
      ELSE
         CALL PRIERR()
         WRITE (IOUT,CERR) 'OPENFAST OR TURBSIM EXPECTED' , &
                           'PROGRAM STOPPED'
         ISTAT = -1
         GOTO 200                                
      ENDIF
200   RETURN
      END