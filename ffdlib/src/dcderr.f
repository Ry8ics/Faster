      SUBROUTINE DCDERR(ISTAT,RMIN,RMAX,R)
C=======================================================================
C     LATEST CORRECTION BY
C
C     PURPOSE
C         CHECKS IF THE INPUT VALUE IS WITHIN THE VALUES RMIN<R<RMAX,
C         AND IF THE INPUT VALUE IS CORRECT FORMAT. IF THE CHECK FAIL 
C         PROGRAM IS STOPPED AND AN ERROR MESSAGE IS WRITTEN
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
      INTEGER IFERR, ISTAT
      DOUBLE PRECISION RMIN,RMAX,R
C
C----------------
C
      IF ( IFERR().LT.0 ) THEN
          WRITE (*,99001)
     &    'ERROR READING VALUE OR COMMAND PROGRAM STOPPED'
          Istat = -1
          GOTO 200
      ENDIF
      IF ( R.LT.RMIN ) THEN
          CALL PRIERR()
          WRITE (*,99002)
     &    'VALUE GREATER THAN ',RMIN,
     &    ' EXPECTED, PROGRAM STOPPED'
              Istat = -1
              GOTO 200
      ENDIF
      IF ( R.GT.RMAX ) THEN
          CALL PRIERR()
          WRITE (*,99003)
     &    'VALUE EQUAL OR LESS THAN ',RMAX,
     &    ' EXPECTED, PROGRAM STOPPED'
          Istat = -1
          GOTO 200
      ENDIF
C
C--------------------------------------------------------------------
C
99001 FORMAT (A46)
99002 FORMAT (A19,F14.4,A25)
99003 FORMAT (A25,F14.4,A26)
C
  200 RETURN
      END