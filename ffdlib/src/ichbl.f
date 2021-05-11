**==ichbl.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      INTEGER FUNCTION ICHBL(Chstr)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Check if string CHSTR contains any non-blank characters.
C
C  METHOD:
C
C  ERROR HANDLING:
C
C  INPUT:
C       CHSTR - Char. string to check for non-blank char.
C
C  OUTPUT:
C       ICHBL  - 0: Only blank char. in string
C                1: Non blank char. found
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
C  PROGRAMMED BY:
C       Per Ivar Loekstad / ETOE / Aker Engineering a.s.
C
C  DATE/VERSION:
C       19.02.92/1.0
C
C ======================================================================
C
      INTEGER i , ilench , LEN
      CHARACTER*(*) Chstr
C
      ilench = LEN(Chstr)
      ICHBL = 0
      DO 100 i = 1 , ilench
         IF ( Chstr(i:i).NE.' ' ) ICHBL = 1
 100  CONTINUE
C ----------------------------------------------------------------------
      RETURN
      END
