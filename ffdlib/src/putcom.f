**==putcom.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE PUTCOM(Ityp,Ipar)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Copy of INTEGER variable to internal COMMON /FFD1/ in
C       ffdlib.a.
C
C  METHOD:
C
C  INPUT:
C       ITYP - Indicate which INTEGER variable in COMMON to copy.
C       IPAR - INTEGER var. in internal COMMON, specified by
C              input var. ITYP.
C       ITYP=1: IDCERR - Error flag
C       ITYP=2: INP    - Unit no. for READ
C       ITYP=3: LPU    - Unit no. for WRITE
C       ITYP=4: IP     - Line pointer. (Give current position
C                        in CHBUFF).
C       ITYP=5: NCOL   - Max. width for CHBUFF.
C       ITYP=6: NLIN   - Current line number after call to
C               READL (or READUI).
C
C  INTERNAL VARIABLES:
C       None.
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
C       19.03.94 / 1.0
C
C ======================================================================
C
      INTEGER Ityp , Ipar
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
      IF ( Ityp.EQ.1 ) IDCerr = Ipar
      IF ( Ityp.EQ.2 ) INP = Ipar
      IF ( Ityp.EQ.3 ) LPU = Ipar
      IF ( Ityp.EQ.4 ) IP = Ipar
      IF ( Ityp.EQ.5 ) NCOl = Ipar
      IF ( Ityp.EQ.6 ) NLIn = Ipar
C
C ----------------------------------------------------------------------
      RETURN
C ----------------------------------------------------------------------
C
      END
