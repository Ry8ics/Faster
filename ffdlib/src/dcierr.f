      SUBROUTINE DCIERR(ISTAT,IMIN,IMAX,I)
C=======================================================================
C     LATEST CORRECTION BY
C
C     PURPOSE
C         CHECKS IF THE INPUT VALUE IS WITHIN THE VALUES IMIN<I<IMAX,
C         AND IF THE INPUT VALUE IS CORRECT FORMAT. IF THE CHECK FAIL 
C         PROGRAM IS STOPPED AND AN ERROR MESSAGE IS WRITTEN
C
C
C     METHOD
C
C     CALLS FROM
C
C     CALLS TO
C
C
C     INPUT/OUTPUT
C
C          ISTAT => STATUS FLAG........................: OUT
C          
C
C     LOCAL
C
C
C     PROGRAMMED BY:  M. Sarwar  - KVAERNER
C
C     VERSION:
C
C
C
C     CREATED: 23.03.2016
C
C     OTHER
C
C=======================================================================
C
C
      INTEGER IFERR, ISTAT,IMIN,IMAX,I
C
C----------------
C
      IF ( IFERR().LT.0 ) THEN
          WRITE (*,99001)
     &    'ERROR READING VALUE OR COMMAND PROGRAM STOPPED'
          Istat = -1
          GOTO 200
      ENDIF
      IF ( I.LT.IMIN ) THEN
          CALL PRIERR()
           WRITE(*,99002)
     &    'VALUE LESS THAN ALLOWED: ',IMIN,
     &    '  PROGRAM STOPPED'
              Istat = -1
              GOTO 200
      ENDIF
      IF ( I.GT.IMAX ) THEN
          CALL PRIERR()
          WRITE (*,99003)
     &    'VALUE GREATER THAN ALLOWED: ',IMAX,
     &    '  PROGRAM STOPPED'
          Istat = -1
          GOTO 200
      ENDIF
C
C--------------------------------------------------------------------
C
99001 FORMAT (A46)
99002 FORMAT (A25,I0,/,A17)
99003 FORMAT (A28,I0,/,A17)
      
  200 RETURN
      END