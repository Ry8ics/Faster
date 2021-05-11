**==dcint.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE DCINT(Num)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Decode one integer number.
C
C  METHOD:
C       - CALL MOVEIP to move line pointer IP to first non-blank character
C         in line buffer CHBUFF.
C       - Check sign.
C       - Move IP to next char. after sign.
C       - Decode integer NUM from CHBUFF.
C       - After decoding of NUM, IP will point at the first blank char,
C         after NUM.
C       - If any error, IP will point at char. that caused the error,
C         and error-flag will be set to a negative value.
C
C  ERROR HANDLING:
C       - If call to MOVEIP return error flag on, then error message
C         is printed.
C       - If invalid char. then error message is printed. Also
C         CHBUFF is printed with a ? at position where invalid
C         char. was found.
C       - If invalid char. IP is moved to the next blank character in
C         CHBUFF.
C
C  INPUT:
C       None.
C
C  OUTPUT:
C       NUM  - Decoded integer number, if no error.
C
C  INTERNAL VARIABLES:
C
C  CALL TO:
C       MOVEIP
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
      INTEGER INDEX
      INTEGER Num , isign , iend , m
      CHARACTER ch*1
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
C Initialize.
C
      Num = 0
      isign = 0
C
C Move IP to next non-blank in line buffer CHBUFF.
C
      CALL MOVEIP(iend)
      IF ( IDCerr.LT.0 ) THEN
         WRITE (LPU,99002)
         IDCerr = -1
         GOTO 300
      ELSE
         IF ( iend.NE.0 ) GOTO 300
C
C Check sign.
C
         ch = CHBuff(IP:IP)
         IF ( ch.NE.'+' ) THEN
            IF ( ch.NE.'-' ) GOTO 200
            isign = 1
         ENDIF
      ENDIF
C
C Move IP to next char. after + or -
C
 100  IP = IP + 1
      IF ( IP.GT.NCOl ) THEN
C
C Shift sign.
C
         IF ( isign.NE.0 ) Num = -Num
         GOTO 300
      ELSE
         ch = CHBuff(IP:IP)
         IF ( ch.EQ.' ' ) THEN
            IF ( isign.NE.0 ) Num = -Num
            GOTO 300
         ENDIF
      ENDIF
C
C Decode integer number.
C
 200  m = INDEX('0123456789',ch) - 1
      IF ( m.LT.0 ) THEN
C
C ----------------------------------------------------------------------
C
C Error section.
C
         m = IP - 1
         CALL PRIERR()
C
C If invalid character, move IP to next blank character in CHBUFF.
C
 250     IF ( IP.GT.NCOl ) THEN
            IDCerr = -1
         ELSEIF ( CHBuff(IP:IP).EQ.' ' ) THEN
            IDCerr = -1
         ELSE
            IP = IP + 1
            GOTO 250
         ENDIF
      ELSE
         Num = Num*10 + m
         GOTO 100
      ENDIF
C
C ----------------------------------------------------------------------
 300  RETURN
C ----------------------------------------------------------------------
C
99001 FORMAT (/,' *** FFDLIB ERROR - Invalid character ',
     &        '- (MODULE: DCINT)',/)
99002 FORMAT (/,' *** FFDLIB ERROR - Call to MOVEIP - (MODULE: DCINT)')
C
      END
