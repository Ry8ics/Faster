      SUBROUTINE DCVARCHK(CVAR,CSLAVE,ISTAT) 
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!       Check if the identifier cvar exists in the file CSLAVE
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni
    IMPLICIT NONE
    CHARACTER*(*) CVAR,CSLAVE
    CHARACTER*30 CDUM,CID 
    INTEGER ISTAT
    ISTAT=-1   
!   Open slave file
    CALL OPEN_ASCII(IBI2,'OLD',CSLAVE)  
    DO 
!     Do the var check.
      READ(IBI2,*,END=10) CDUM,CID             ! Read from slave. 
      IF (TRIM(CID).EQ.TRIM(CVAR)) ISTAT = 1   ! a hit, i.e. no penalty.
    ENDDO
10  CONTINUE    
    CLOSE(IBI2)
    RETURN
    END