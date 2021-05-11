      SUBROUTINE DCPARA_FAST(Cerr,Citem,CSLAVE,Istat)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!          Read parameters to be updated in the slave files
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      use cfast
      use mod_utils
      IMPLICIT NONE
      INCLUDE 'redwri'
      CHARACTER*(*) Cerr , Citem,CSLAVE
      CHARACTER*14 cpar,cvar
      LOGICAL MATCH  
      integer iferr,istat
      integer ilen,iend,ipar,IP
      LOGICAL ILLEGAL
!
!.................................................
!     Read variable id to be updated in the slave files
      SLAVE%NVAR(SLAVE%NSLAVE)=1
      ILLEGAL=.FALSE.
100   CALL DCSTRG(cvar,ilen,3) 
       IF ( IFERR().LT.0 ) THEN
         WRITE (*,Citem) cvar
         WRITE (*,Cerr)'ERROR IN READING VARIABEL ','PROGRAM STOPPED'
         Istat = -1
         GOTO 200
       ENDIF
!
!     Check if this is a legal variable .. DT,DT_out,Tstart and Tmax are not allowed
      IF (to_upper(TRIM(cvar)).EQ.'TMAX'  ) ILLEGAL=.TRUE.
      IF (to_upper(TRIM(cvar)).EQ.'DT'  ) ILLEGAL=.TRUE.
      IF (to_upper(TRIM(cvar)).EQ.'DT_OUT'  ) ILLEGAL=.TRUE.
      IF (to_upper(TRIM(cvar)).EQ.'TSTART'  ) ILLEGAL=.TRUE.
      IF (ILLEGAL) THEN
         CALL PRIERR()
         WRITE (*,Citem) cvar
         WRITE (*,Cerr)'THIS PARAMETER CANNOT BE ALTERED. ','PROGRAM STOPPED'
         Istat = -1
         GOTO 200          
      ENDIF
! 
         IF (SLAVE%NVAR(SLAVE%NSLAVE).GT.MAX_VAR) THEN
!               CALL PRIERR()
!               WRITE (IOUT,CERR) 'Max number of variables exceeded.' , &
!                               'Use the command MALLOC VARIABLES <max_num>'
!               WRITE(IOUT,*) ' The default number of maximum allowed slaves are ',MAX_VAR
               ISTAT = -2
               MAX_VAR=MAX_VAR*2     ! reallocate
               GOTO 200                
         ENDIF
!        Search through the slave file for existence of CVAR
         CALL DCVARCHK(cvar,CSLAVE,ISTAT)      ! THIS IS A VAR CHECK...
         IF (ISTAT.EQ.-1) THEN
              CALL PRIERR()
               WRITE (*,Citem) cvar
               WRITE(*,*)' Slave file...: ',TRIM(CSLAVE)
               WRITE (IOUT,CERR) 'Requested identifier not found in slave file.'        
               GOTO 200                
         ENDIF
!
      SLAVE%CVAR (SLAVE%NVAR(SLAVE%NSLAVE),SLAVE%NSLAVE)=CVAR
      SLAVE%CONST(SLAVE%NVAR(SLAVE%NSLAVE),SLAVE%NSLAVE)=.FALSE.
!
!     Read NPAR consecutive strings to be inserted in the SLAVE files
!
      DO IPAR=1,NPARA
          CALL DCSTRG(cpar,ilen,3) 
          IF ( IFERR().LT.0 ) THEN
            WRITE (*,Citem) cpar
            WRITE (*,Cerr)'ERROR IN READING PARAMETER ','PROGRAM STOPPED'
            Istat = -1
            GOTO 200
          ENDIF  
           IF (ilen.eq.0) THEN
            CALL PRIERR()
            WRITE (*,Cerr)'Please provide the needed number of parameters. ','PROGRAM STOPPED'
            Istat = -1
            GOTO 200
          ENDIF  
!...........................................................
!         New code to cope for constant parameteres......... 
!         There should be no need to repeat the same parameter NPARA times
!         Check if the same parameter is to be used, i.e. it is constant
          IF (IPAR.EQ.1) THEN
             IF ( MATCH('+',1) ) THEN
                DO IP=1,NPARA
                    IF (IP.GT.MAX_PAR) THEN
                          CALL PRIERR()
!                          WRITE (IOUT,CERR) 'Max number of parameters exceeded.' , &
!                                          'Use the command MALLOC PARAMETERS <max_num>'
!                          WRITE(IOUT,*) ' The default number of maximum allowed parameters are ',MAX_PAR
                          ISTAT = -2
                          MAX_PAR=MAX_PAR*2   ! Allocate more memory...
                          GOTO 200                
                    ENDIF                    
                    SLAVE%CPAR(IP,SLAVE%NVAR(SLAVE%NSLAVE),SLAVE%NSLAVE)=cpar
                ENDDO 
                SLAVE%CONST(SLAVE%NVAR(SLAVE%NSLAVE),SLAVE%NSLAVE)=.TRUE. 
                GOTO 150
             ENDIF
          ENDIF
!..........................................................
          SLAVE%CPAR(IPAR,SLAVE%NVAR(SLAVE%NSLAVE),SLAVE%NSLAVE)=cpar
      ENDDO
!
150   CALL MOVEIP(iend)
!     Check if more variables shall be read      
      IF ( iend.EQ.0 ) THEN
!          Update number of variables for current slave (e.g. WaveHs, WaveTp, etc..)
            SLAVE%NVAR(SLAVE%NSLAVE)=SLAVE%NVAR(SLAVE%NSLAVE)+1    
            GOTO 100
      ENDIF
!      
200   RETURN
      END