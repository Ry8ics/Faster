**==iferr.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      INTEGER FUNCTION IFERR()
      IMPLICIT NONE
C
C ======================================================================
C  PURPOSE:
C       Obtain the errorflagg
C
C  METHOD:
C
C  ERROR HANDLING:
C
C  INPUT:
C       None.
C
C  OUTPUT:
C       IFERR  - Error flag.
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
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
      IFERR = IDCerr
C
C ----------------------------------------------------------------------
      RETURN
C ----------------------------------------------------------------------
      END
