**==alpha.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      LOGICAL FUNCTION ALPHA(Ityp)
      IMPLICIT NONE
C
C ======================================================================
C 
C  PURPOSE:
C       Check if the next non-blank character in CHBUFF
C       is according to criterion given by ITYP (See below).
C
C  METHOD:
C       - Call MOVEIP to move line-pointer IP to next non-blank
C         character in line-buffer CHBUFF. Do not move IP any
C         further.
C       - Check if first non-blank character is according to
C         criterion given by ITYP, by use of INDEX function.
C
C  INPUT:
C       ITYP: 1: ALPHA = .TRUE. if next non-blank character in CHBUFF
C                is 0123456789-+.
C       ITYP: 2: ALPHA = .TRUE. if the next non-blank character in
C                CHBUFF is '
C       ITYP: 3: ALPHA = .TRUE. if the next non-blank character in
C                CHBUFF is A-Z, a-z, 0123456789 or .-+
C
C  OUTPUT:
C       ALPHA (.TRUE. or .FALSE.)
C
C  INTERNAL VARIABLES:
C       IEND  - 0: If more relevant information in CHBUFF.
C               1: If end of line (CHBUFF), i.e. IP > NCOL.
C       CH    - Char. in CHBUFF.
C
C  CALLS FROM:
C       General purpose routine.
C
C  CALLS TO:
C       MOVEIP
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
      INTEGER j , iend , Ityp
      CHARACTER ch*1
      INTEGER INDEX
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
C Initialize
C
      ALPHA = .FALSE.
C
C Move IP to next non-blank character in line-buffer CHBUFF.
C
      CALL MOVEIP(iend)
C
C Any error ?
C
      IF ( IDCerr.LT.0 ) THEN
C
         WRITE (LPU,99001)
         IDCerr = -1
C
      ELSEIF ( iend.EQ.0 ) THEN
C
C Check if char is 0123456789-+.
C
         IF ( Ityp.EQ.1 ) THEN
            ch = CHBuff(IP:IP)
            j = INDEX('0123456789-+.',ch)
            IF ( j.GT.0 .AND. j.LE.13 ) ALPHA = .TRUE.
C
C Check if char. is '
C
         ELSEIF ( Ityp.EQ.2 ) THEN
            ch = CHBuff(IP:IP)
            j = INDEX('''',ch)
            IF ( j.EQ.1 ) ALPHA = .TRUE.
C
C Check if char. is alphabetic or numeric.
C
         ELSEIF ( Ityp.EQ.3 ) THEN
            ch = CHBuff(IP:IP)
            j = INDEX('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',ch)
            IF ( j.GT.0 .AND. j.LE.36 ) ALPHA = .TRUE.
            j = INDEX('-+.abcdefghijklmnopqrstuvwxyz',ch)
            IF ( j.GT.0 .AND. j.LE.29 ) ALPHA = .TRUE.
         ENDIF
      ENDIF
C ----------------------------------------------------------------------
      RETURN
99001 FORMAT (/,' *** FFDLIB ERROR - Call to MOVEIP - (MODULE: ALPHA)')
C ----------------------------------------------------------------------
      END
