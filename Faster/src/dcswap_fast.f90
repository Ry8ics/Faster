      SUBROUTINE DCSWAP_FAST(Cerr,Citem,Istat)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!        Read strings to be updated in the slave files
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      use cfast
      IMPLICIT NONE
      INCLUDE 'redwri'
      CHARACTER*(*) Cerr , Citem
      CHARACTER*128 cswap,cstring
      LOGICAL MATCH  
      integer iferr,istat
      integer ilen,ipar
!
!.................................................
!     Read strings to be updated in the slave files
      CALL DCSTRG(cstring,ilen,2) 
       IF ( IFERR().LT.0 ) THEN
         WRITE (*,Citem) cstring
         WRITE (*,Cerr)'ERROR IN READING STRING ','PROGRAM STOPPED'
         Istat = -1
         GOTO 200
       ENDIF
      SLAVE%CSWAP_STR_FROM(SLAVE%NSWAP)=cstring      
!
      IF ( MATCH('TO',2) ) THEN
!           Read NPAR consecutive strings to be swapped in the SLAVE file
                
                CALL DCSTRG(cswap,ilen,2) 
                IF ( IFERR().LT.0 ) THEN
                  WRITE (*,Citem) cswap
                  WRITE (*,Cerr)'ERROR IN READING STRING ','PROGRAM STOPPED'
                  Istat = -1
                  GOTO 200
                ENDIF
!................................................................
            DO IPAR=1,NPARA                        
                 IF (IPAR.GT.MAX_PAR) THEN
                       ISTAT = -2
                       MAX_PAR=MAX_PAR*2   ! Increase by a factor of 2 and reallocate
                       GOTO 200                
                 ENDIF                               
                SLAVE%CSWAP_STR_TO(IPAR,SLAVE%NSWAP)=cswap 
            ENDDO
      ELSE
            CALL PRIERR()
            WRITE (IOUT,CERR) 'TO EXPECTED' , &
                             'PROGRAM STOPPED'
            ISTAT = -1
            GOTO 200
      ENDIF
!      
200   RETURN
      END