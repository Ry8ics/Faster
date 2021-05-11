**==match.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      LOGICAL FUNCTION MATCH(Str,N)
      IMPLICIT NONE
C
C ======================================================================
C  PURPOSE:
C       - If next N characters in CHBUFF = STR then MATCH = .TRUE. and
C         IP is moved to next blank char. after N-first char.
C         that match.
C       - If MATCH return .FALSE. the IP is only moved behind leading
C         blanks.
C       - Upper/lower-case is ignored.
C
C  METHOD:
C
C  ERROR HANDLING:
C
C  INPUT:
C       STR - Char. string
C       N   - First N significant number of char. in STR.
C             (Example: If STR=MEMBER and N=3; then MATCH will return
C             .TRUE. if next N char. string in CHBUFF is MEM). Note that
C             upper/lower-case is ignored.
C
C  OUTPUT:
C       MATCH  - .TRUE. if next N char. in CHBUFF are equal to STR(1:N),
C                otherwise .FALSE.
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
      INTEGER N , iend , istop , j
      CHARACTER Str*(*) , str1*100 , str2*100
C
      INCLUDE 'ffd.inc'
C
C ======================================================================
C
C Initialize.
C
      MATCH = .FALSE.
      istop = 0
C
      IF ( N.LT.1 .OR. N.GT.NCOl ) THEN
C
         WRITE (LPU,99001) N
         IDCerr = -1
      ELSE
         CALL MOVEIP(iend)
         IF ( IDCerr.NE.istop ) THEN
            WRITE (LPU,99002)
            IDCerr = -1
         ELSEIF ( iend.EQ.0 ) THEN
C
            j = IP + N - 1
            IF ( j.LE.NCOl ) THEN
C
C Check if N>100 (=max. length of STR1,2
C
               IF ( N.GT.100 ) THEN
                  WRITE (LPU,99003)
                  IDCerr = -1
                  GOTO 100
               ENDIF
C
               str1(1:N) = Str(1:N)
               str2(1:N) = CHBuff(IP:j)
C
C Convert to upper-case
C
               CALL LOTOUP(str1,N)
               CALL LOTOUP(str2,N)
               IF ( str1(1:N).EQ.str2(1:N) ) MATCH = .TRUE.
               IF ( MATCH ) THEN
C
C Move IP to fist blank character after string
C
                  IP = j
 5                IF ( IP.LE.NCOl ) THEN
                     IF ( CHBuff(IP:IP).NE.' ' ) THEN
                        IP = IP + 1
                        GOTO 5
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C ----------------------------------------------------------------------
 100  RETURN
99001 FORMAT (/,' *** FFDLIB ERROR - Illegal N =',I3,
     &        ' - (MODULE: MATCH)')
99002 FORMAT (/,' *** FFDLIB ERROR - Call to MOVEIP - (MODULE: MATCH)')
99003 FORMAT (/,' *** FFDLIB ERROR - Max 100 char. allowed in string',
     &        ' - (MODULE: MATCH)')
C ----------------------------------------------------------------------
      END
