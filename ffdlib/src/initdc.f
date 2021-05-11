**==initdc.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE INITDC(Lunr,Lunp)
      IMPLICIT NONE
C
C ======================================================================
C  PURPOSE:
C       Initialize the free-format subroutine package. Set units for
C       read and write.
C
C  METHOD:
C
C  ERROR HANDLING:
C
C  INPUT:
C       LUNR - UNIT for READ.
C       LUNP - UNIT for WRITE.
C
C  OUTPUT:
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
      INTEGER Lunp , Lunr
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
C Initialize COMMON variables.
C
      NLIn = 0
      IDCerr = 0
      INP = 5
      LPU = 6
      IP = 0
      CHBuff = ' '
C
C NOTE: If NCOL is changed, remember to change decl. (size) of CHBUFF
C       in ffd.inc
C
      NCOl = 200
      IF ( Lunr.GT.0 ) INP = Lunr
      IF ( Lunp.GT.0 ) LPU = Lunp
C
C ----------------------------------------------------------------------
      RETURN
C ----------------------------------------------------------------------
C
      END
