**==dcdbl.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE DCDBL(Dfpn)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Decode one double precision number.
C
C  METHOD:
C       - CALL MOVEIP to move line pointer IP to first non-blank character
C         in line buffer CHBUFF.
C       - Check sign.
C       - Move IP to next char. after sign.
C       - Decode double precision DFPN from CHBUFF.
C       - After decoding of DFPN, IP will point at the first blank char.
C         after DFPN.
C       - If any error, IP will point at the char. that caused the error,
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
C       FPN  - Decoded double precision number, if no error.
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
      INTEGER isign , id , iend , istop , m
      DOUBLE PRECISION Dfpn , fm
      CHARACTER ch
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
C Initialize.
C
      Dfpn = 0.0D+00
      istop = 0
      isign = 0
      id = 0
      m = 0
C
C Move IP to next non-blank char. in line buffer CHBUFF.
C
      CALL MOVEIP(iend)
      IF ( IDCerr.NE.istop ) THEN
         WRITE (LPU,99001)
         IDCerr = -1
         GOTO 500
      ELSE
         IF ( iend.NE.0 ) GOTO 500
C
C Check sign
C
         ch = CHBuff(IP:IP)
         IF ( ch.NE.'+' ) THEN
            IF ( ch.NE.'-' ) GOTO 200
            isign = 1
         ENDIF
      ENDIF
C
 100  IP = IP + 1
C
      IF ( IP.GT.NCOl ) GOTO 300
      ch = CHBuff(IP:IP)
      IF ( ch.EQ.' ' ) GOTO 300
C
C Exp. format ?
C
      IF ( ch.EQ.'E' .OR. ch.EQ.'e' .OR. ch.EQ.'D' .OR. ch.EQ.'d' ) THEN
         IP = IP + 1
         CALL DCINT(m)
         IF ( IDCerr.EQ.istop ) GOTO 300
         WRITE (LPU,99002)
         IDCerr = -1
         GOTO 500
      ENDIF
C
 200  IF ( ch.NE.'.' ) THEN
         fm = INDEX('0123456789',ch) - 1
         IF ( fm.LT.0.0D+00 ) THEN
C
C ----------------------------------------------------------------------
C
C Error section
C
            CALL PRIERR()
            GOTO 400
         ELSE
            Dfpn = Dfpn*10.0D+00 + fm
            IF ( id.NE.0 ) id = id + 1
            GOTO 100
         ENDIF
      ELSEIF ( id.NE.0 ) THEN
         CALL PRIERR()
         GOTO 400
      ELSE
         id = 1
         GOTO 100
      ENDIF
C
 300  IF ( isign.NE.0 ) Dfpn = -Dfpn
      IF ( id.GT.1 ) m = m - (id-1)
      IF ( m.NE.0 ) Dfpn = Dfpn*(10.0D+00**m)
      GOTO 500
C
 400  IF ( IP.GT.NCOl ) THEN
         IDCerr = -1
      ELSEIF ( CHBuff(IP:IP).EQ.' ' ) THEN
         IDCerr = -1
      ELSE
         IP = IP + 1
         GOTO 400
      ENDIF
 500  RETURN
C
C ----------------------------------------------------------------------
99001 FORMAT (/,' *** FFDLIB ERROR - Call to MOVEIP - (MODULE: DCDBL)')
99002 FORMAT (/,' *** FFDLIB ERROR - Call to DCINT - (MODULE: DCDBL)')
99003 FORMAT (/,' *** FFDLIB ERROR - Invalid character ',
     &        '- (MODULE: DCDBL)',/)
C ----------------------------------------------------------------------
C
      END
