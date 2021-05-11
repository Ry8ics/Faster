**==moveip.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE MOVEIP(Iend)
      IMPLICIT NONE
C
C ======================================================================
C  PURPOSE:
C       Move pointer IP to the next non-blank in CHBUFF.
C
C  METHOD:
C       Move line-pointer IP to next non-blank character
C       in line-buffer CHBUFF.
C       If next non-blank character is &, then read new line into
C       CHBUFF and move IP to next non-blank. (Etc.)
C
C  INPUT:
C
C  OUTPUT:
C       IEND = 0: More information in line-buffer CHBUFF, between
C                 position for IP and NCOL.
C              1: End of line-buffer CHBUFF; IP = > NCOL.
C
C  CALLS FROM:
C       General purpose routine.
C
C  CALLS TO:
C       READL
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
      INTEGER INDEX , MAX
      INTEGER Iend , istop
      CHARACTER ch
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
      Iend = 0
C
 100  IF ( IP.LT.1 ) IP = 1
      IF ( IP.LE.NCOl ) THEN
         ch = CHBuff(IP:IP)
         IF ( ch.NE.' ' ) THEN
C
C Line continuation char. (&)
C
            IF ( ch.EQ.'&' ) THEN
               istop = IDCerr
               CALL READL
C
C Starts reading in column INDEX(CHBUFF,'$') + 1
C or INDEX(CHBUFF,'!') + 1. (This may be changed).
C
               IP = MAX(INDEX(CHBuff,'$')+1,INDEX(CHBuff,'!')+1)
               IDCerr = IDCerr + istop
               IF ( IDCerr.EQ.istop ) GOTO 100
               WRITE (LPU,99001)
               IDCerr = -1
               Iend = 1
            ENDIF
            GOTO 200
         ELSE
            IP = IP + 1
            IF ( IP.LE.NCOl ) GOTO 100
         ENDIF
      ENDIF
C
C END-of-line, Column NCOL + 1
C
      Iend = 1
      IP = NCOl + 1
 200  RETURN
C ----------------------------------------------------------------------
99001 FORMAT (/,' *** FFDLIB ERROR - Call to READL (MODULE: MOVEIP)')
C ----------------------------------------------------------------------
      END
