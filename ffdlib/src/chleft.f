**==chleft.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      CHARACTER*8 FUNCTION CHLEFT(N)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Convert integer N to character CHLEFT, then
C       left justify. ( Remark: N <= 99999999 or +/-9999999 ).
C
C  METHOD:
C       - Write N into character BUFF, right justified.
C       - Then left justify.
C
C  INPUT:
C       N (Valid integer)
C
C  OUTPUT:
C       CHLEFT
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
C  MACHINE DEPENDENT PART:
C       INCLUDE statement
C
C  PROGRAMMED BY:
C       Per Ivar Loekstad / ETOE / Aker Engineering a.s.
C
C  DATE/VERSION:
C       19.07.01 / 1.0
C
C ======================================================================
C
      INTEGER i , k , N
      CHARACTER*8 buff , buff1
C
C ----------------------------------------------------------------------
C
      WRITE (buff,99001) N
C
C FIND BEGINNING OF NUMERIC STRING
C
      DO 100 i = 1 , 8
         k = i
         IF ( buff(i:i).NE.' ' ) GOTO 200
 100  CONTINUE
C
C LEFT JUSTIFY NUMERIC STRING
C
 200  buff1(1:) = buff(k:)
C
      CHLEFT(1:8) = buff1(1:8)
C
C ----------------------------------------------------------------------
      RETURN
99001 FORMAT (I8)
C ----------------------------------------------------------------------
C
      END
