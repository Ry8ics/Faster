**==dcstrg.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE DCSTRG(Str,Lstr,Ityp)
      IMPLICIT NONE
C     Note: Minor update by B. Melhus on 21.04.2021
C ======================================================================
C  PURPOSE:
C       Decode char. string according to criterion given by ITYP.
C       (See below).
C
C  METHOD:
C       - CALL MOVEIP to move line pointer IP to first non-blank
C         character in line buffer CHBUFF.
C       - Decode string from CHBUFF.
C       - After decoding of string, IP will be the first blank
C         after string.
C
C  ERROR HANDLING:
C       - If call to MOVEIP return error flag on, then error message
C         is printed.
C       - If invalid char. then error message is printed.
C       - If invalid char. IP is moved to the next blank character in
C         CHBUFF.
C
C  INPUT:
C       ITYP: Type of char. string to be decoded.
C             1: Decode any string until first blank char.
C             2: Decode any string between ' '.
C                Return STR and LSTR. If string enclosed by ' '
C                can not be decoded as next data-item, error-flag
C                is set to a negative value.
C             3: Decode string until first blank character.
C                Return STR and LSTR. Nearly equal to ITYP=1,
C                but call ALPHA(3) before any decoding.
C             4: Decode any string with length LSTR from first
C                non-blank character.
C                Return STR only.
C       LSTR: If ITYP = 1 - 4: Max length of string to be decoded is
C             verified.
C             (LEN() is used to calc. length of STR). If string to
C             be decoded is outside max limit: decoding is stopped.
C             IDCERR is set to a value < 0.
C
C
C  OUTPUT:
C       STR   - Decoded string.
C       LSTR  - Length of STR.
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
      INTEGER iend , Ityp , maxwid
      INTEGER Lstr , istop , j , itell
      CHARACTER Str*(*) , ch
C
C Function
C
      LOGICAL ALPHA
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
C Initialize
C
      IF ( Ityp.NE.4 ) THEN
         maxwid = LEN(Str)
         Lstr = 0
      ENDIF
      Str = ' '
      istop = 0
C
C     New code due to run time error experienced by Intel Visual Fortran
C     Error: The variable 'J' is being used without being defined at line 193.
C     The parameter is now initialized to zero. B. Melhus on 21.04.2021
      j = 0
C
C Move IP to first non-blank character.
C
      CALL MOVEIP(iend)
      IF ( IDCerr.NE.istop ) THEN
C
         WRITE (LPU,99001)
         IDCerr = -1
      ELSEIF ( iend.EQ.0 ) THEN
C ----------------------------------------------------------------------
C Decode string
C
         IF ( Ityp.EQ.1 ) THEN
C
            Lstr = 1
            j = 1
 20         IF ( IP.LE.NCOl ) THEN
               ch = CHBuff(IP:IP)
               IF ( ch.EQ.' ' ) THEN
C
 25               IF ( IP.LE.NCOl ) THEN
                     IF ( CHBuff(IP:IP).NE.' ' ) THEN
                        IP = IP + 1
                        GOTO 25
                     ENDIF
                  ENDIF
               ELSE
                  IP = IP + 1
                  IF ( j.GT.maxwid ) GOTO 100
                  Str(j:j) = ch
                  j = j + 1
                  GOTO 20
               ENDIF
            ENDIF
            Lstr = j - 1
C
C ----------------------------------------------------------------------
C
            ELSEIF ( Ityp.EQ.2 ) THEN
C
            IF ( ALPHA(2) ) THEN
               j = 1
               itell = 0
 30            IF ( IP.LE.NCOl ) THEN
                  IF ( .NOT.CHBuff(IP:IP).EQ.'''' ) ch = CHBuff(IP:IP)
                  IF ( CHBuff(IP:IP).EQ.'''' ) THEN
                     itell = itell + 1
                     IF ( itell.EQ.1 ) Lstr = IP
                     IP = IP + 1
                     IF ( itell.EQ.1 ) GOTO 30
                  ENDIF
                  IP = IP + 1
                  IF ( itell.EQ.2 ) THEN
                     Lstr = IP - Lstr - 3
                     GOTO 200
                  ENDIF
                  IF ( j.GT.maxwid ) GOTO 100
                  Str(j:j) = ch
                  j = j + 1
                  GOTO 30
               ELSE
                  IDCerr = -1
                  WRITE (LPU,99003) NCOl
               ENDIF
            ELSE
               IDCerr = -1
               WRITE (LPU,99002)
            ENDIF
C
C ----------------------------------------------------------------------
C
         ELSEIF ( Ityp.EQ.3 ) THEN
C
C           Minor update: Error encountered with Visual Studio
C                         Use of uninitalized variable j
C                         B. Melhus by 21.04.2021
            j=0
C
            IF ( ALPHA(3) ) THEN
               Lstr = 1
               j = 1
 40            IF ( IP.LE.NCOl ) THEN
                  ch = CHBuff(IP:IP)
                  IF ( ch.EQ.' ' ) THEN
C
 42                  IF ( IP.LE.NCOl ) THEN
                        IF ( CHBuff(IP:IP).NE.' ' ) THEN
                           IP = IP + 1
                           GOTO 42
                        ENDIF
                     ENDIF
                  ELSE
                     IP = IP + 1
                     IF ( j.GT.maxwid ) GOTO 100
                     Str(j:j) = ch
                     j = j + 1
                     GOTO 40
                  ENDIF
               ENDIF
            ELSE
               IDCerr = -1
               WRITE (LPU,99002)
            ENDIF
            Lstr = j - 1
C
C ----------------------------------------------------------------------
C
         ELSEIF ( Ityp.EQ.4 ) THEN
C
            IF ( LEN(Str).LT.Lstr ) THEN
               IDCerr = -1
               WRITE (LPU,99004) LEN(Str)
               GOTO 200
            ENDIF
            IF ( IP.LE.NCOl ) THEN
               DO 50 j = 1 , Lstr
                  IF ( IP.LE.NCOl ) Str(j:j) = CHBuff(IP:IP)
                  IP = IP + 1
 50            CONTINUE
            ENDIF
C           LSTR = 1
C
C ----------------------------------------------------------------------
C
         ENDIF
      ENDIF
      GOTO 200
C
 100  IDCerr = -1
      WRITE (LPU,99004) maxwid
      IP = IP - 1
      Lstr = maxwid
C
 200  RETURN
C ----------------------------------------------------------------------
99001 FORMAT (/,' *** FFDLIB ERROR - Call to MOVEIP - (MODULE: DCSTRG)')
99002 FORMAT (/,' *** FFDLIB ERROR - Illegal character -',
     &        ' (MODULE: DCSTRG)')
99003 FORMAT (/,' *** FFDLIB ERROR - Try to read outside max.',
     &        ' line-width, NCOL: ',I3,' - (MODULE: DCSTRG)')
99004 FORMAT (/,' *** FFDLIB ERROR - Length of string exceed max.',
     &        ' length given as: ',I3,' - (MODULE: DCSTRG)')
C ----------------------------------------------------------------------
      END
