**==dcintm.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE DCINTM(Maxitm,Num,Nitem)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Decode several INTEGER number(s).
C
C  METHOD:
C       Loop and call DCINT to decode max MAXITM, INTEGER number(s)
C
C  ERROR HANDLING:
C       See DCINT
C
C  INPUT:
C       MAXITM - Max no of numbers allowed
C
C  OUTPUT:
C       NUM    - Array of decoded INTEGER number(s)
C       NITEM  - No of numbers decoded
C
C  INTERNAL VARIABLES:
C       IEND   - See MOVEIP
C
C  CALL TO:
C       MOVEIP, DCINT
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
C       11.02.94/1.0
C
C ======================================================================
C
      INTEGER iend , Nitem , Num(*) , Maxitm
      INTEGER IFERR
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
      Nitem = 0
C
 100  CALL MOVEIP(iend)
C
      IF ( iend.EQ.0 ) THEN
         Nitem = Nitem + 1
C
         IF ( Nitem.GT.Maxitm ) THEN
            WRITE (LPU,99001) Maxitm
            CALL PRIERR()
            IDCerr = -1
            GOTO 200
         ENDIF
C
         CALL DCINT(Num(Nitem))
C
         IF ( IFERR().LT.0 ) THEN
            WRITE (LPU,99002)
            GOTO 200
         ENDIF
C
         GOTO 100
C
      ENDIF
C
C ----------------------------------------------------------------------
 200  RETURN
C ----------------------------------------------------------------------
C
99001 FORMAT (/,' *** FFDLIB ERROR - Max no of INTEGER numbers',
     &        ' exceeded (=',I6,') - (MODULE: DCINTM)',/)
99002 FORMAT (/,' *** FFDLIB ERROR - Decoding INTEGER number',
     &        ' - (MODULE: DCINTM)',/)
C
      END
