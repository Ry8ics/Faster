**==readl.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE READL
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE :
C       - Read a line of data into buffer CHBUFF. Ignore blank lines.
C       - Initialize internal error-flag to 0.
C       - Set line-pointer IP to position 1 in CHBUFF.
C
C  METHOD:
C
C  ERROR HANDLING:
C       - Set internal error-flag IDCERR to a negative value,
C         if any error.
C
C  INPUT:
C       None.
C
C  OUTPUT:
C       None. (New line read into COMMON variable CHBUFF).
C
C  INTERNAL VARIABLES:
C
C  CALL TO:
C       None.
C
C  CALL FROM:
C       General purpose routine.
C
C  LIBRARY:
C       FFDLIB
C
C  MACHINE DEPENDENT PART:
C       INCLUDE statement
C
C  PROGRAMMED BY:
C       Per Ivar Loekstad / ETOE / Aker Engineering a.s.
C
C  DATE/VERSION:
C       19.02.92/1.0
C
C ======================================================================
C
      INTEGER j
C
      INCLUDE 'ffd.inc'
C
C ======================================================================
C
C Initialize
C
      IP = 1
      IDCerr = 0
C
C Loop: Read line until non-blank line
C
 100  READ (INP,'(A200)',ERR=300,END=400) CHBuff
C
C Increment no of lines Read
C
      NLIn = NLIn + 1
C
C Do not accept blank lines
C
      DO 200 j = 1 , NCOl
         IF ( CHBuff(j:j).NE.' ' ) GOTO 500
 200  CONTINUE
C
      GOTO 100
C
 300  WRITE (LPU,99001) INP
 400  IDCerr = -1
C
C ----------------------------------------------------------------------
 500  RETURN
99001 FORMAT (/,' *** FFDLIB ERROR - Reading unit =',I2,
     &        ' - (MODULE: READ)')
C ----------------------------------------------------------------------
C
      END
