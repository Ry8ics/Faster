**==lenstr.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      INTEGER FUNCTION LENSTR(Str,N)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Calc. real length of char. string STR
C
C  METHOD:
C       Ignore trailing blanks
C
C  INPUT:
C       STR - Char. string
C       N   - Max length of STR. Use N=LEN(STR) if no special effect
C             wanted.
C             N must be <= LEN(STR). Value less than LEN(STR) may be
C             given.
C             Example: CALL LENSTR( STR, LEN(STR) )
C
C  OUTPUT:
C       LENSTR - Length of STR without trailing blanks
C
C  INTERNAL VARIABLES:
C
C  CALLS FROM:
C       General purpose routine.
C
C  CALLS TO:
C       None.
C
C  LIBRARY:
C       FFDLIB
C
C  PROGRAMMED BY:
C       Per Ivar Loekstad / ETOE / Aker Engineering a.s.
C
C  DATE/VERSION:
C       30.10.93 / 1.0
C
C ======================================================================
C
      CHARACTER*(*) Str
      INTEGER i , N , LEN
      N = MIN(N,LEN(Str))
      IF ( N.LE.0 ) N = 1
      i = N + 1
 100  i = i - 1
      IF ( Str(i:i).EQ.' ' .AND. i.GT.1 ) GOTO 100
      IF ( Str(i:i).EQ.' ' .AND. i.EQ.1 ) i = i - 1
      LENSTR = i
      IF ( LENSTR.LT.0 ) LENSTR = 0
      RETURN
      END
