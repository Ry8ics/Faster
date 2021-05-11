**==lotoup.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE LOTOUP(Str,Lstr)
      IMPLICIT NONE
C
C ======================================================================
C
C PURPOSE:
C      Convert lower case to upper case
C
C  METHOD:
C
C  ERROR HANDLING:
C
C  INPUT:
C       STR  - Char. string.
C       LSTR - Length of STR.
C
C  OUTPUT:
C       STR  - Char. string (Upper case).
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
      INTEGER i , INDEX , j , k
      INTEGER Lstr
      CHARACTER*(*) Str
      CHARACTER*60 chstr
C
C ----------------------------------------------------------------------
C
      chstr = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
      DO 100 i = 1 , Lstr
         j = 0
 50      j = j + 1
         IF ( INDEX(Str(i:i),chstr(j:j)).LE.0 .AND. j.LE.26 ) GOTO 50
         IF ( j.LE.26 ) THEN
            k = j + 26
            Str(i:i) = chstr(k:k)
         ENDIF
 100  CONTINUE
C
C ----------------------------------------------------------------------
C
      RETURN
      END
