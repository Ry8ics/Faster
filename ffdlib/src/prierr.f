**==prierr.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE PRIERR()
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Print char. string CHBUFF, and mark char. where error occured,
C       with a ? in position IP.
C
C  METHOD:
C
C  ERROR HANDLING:
C
C  INPUT:
C
C  OUTPUT:
C
C  INTERNAL VARIABLES:
C
C  CALL TO:
C       None.
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
      INTEGER j , m
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
C - Print char. string CHBUFF.
C - Print a ? in position (IP) where error occur.
C
      WRITE (LPU,*)
      WRITE (LPU,'(1X,A)') CHBuff
      m = IP - 1
      IF ( IP.EQ.1 ) WRITE (LPU,'(1X,A1)') '?'
C
C Format 200A1 indicates NCOL = 200
C
      IF ( IP.GT.1 ) WRITE (LPU,'(1X,200A1)') (' ',j=1,m) , '?'
C
      RETURN
      END
