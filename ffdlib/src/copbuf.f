**==copbuf.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE COPBUF(Text,Ityp)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Obtain copy of input line CHBUFF, or copy TEXT to CHBUFF
C
C  METHOD:
C
C  INPUT:
C       ITYP: 0: Copy CHBUFF to TEXT
C             1: Copy TEXT to CHBUFF
C  OUTPUT:
C       TEXT - Copy of buffer CHBUFF. (ITYP=0)
C
C  INTERNAL VARIABLES:
C       N - Lenght of string TEXT or NCOL.
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
C       19.02.92 / 1.0
C
C ======================================================================
C
      INTEGER LEN , MIN
      INTEGER n , Ityp
      CHARACTER Text*(*)
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
      IF ( Ityp.EQ.0 ) THEN
         Text = ' '
         n = MIN(LEN(Text),NCOl)
         IF ( n.GT.0 ) Text = CHBuff(1:n)
      ELSEIF ( Ityp.EQ.1 ) THEN
         CHBuff = ' '
         n = MIN(LEN(CHBuff),NCOl)
         IF ( n.GT.0 ) CHBuff = Text(1:n)
      ELSE
         WRITE (LPU,99001)
      ENDIF
C
C ----------------------------------------------------------------------
      RETURN
C ----------------------------------------------------------------------
C
99001 FORMAT (/,' *** FFDLIB ERROR - Illegal ITYP used - ',
     &        '(MODULE: COPBUF)')
      END
