**==dcstrm.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE DCSTRM(Maxitm,Maxchl,Str,Lstr,Ityp,Nitem)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Decode several CHARACTER strings
C
C  METHOD:
C       Loop and call DCSTRG to decode max MAXITM, CHARACTER string(s)
C
C  ERROR HANDLING:
C       See DCSTRG
C
C  INPUT:
C       MAXITM - Max no of numbers allowed
C       MAXCHL - Max length of character string
C       ITYP   - See DCSTRG, but only ITYP=1, 2 or 3 allowed
C
C  OUTPUT:
C       STR    - Array of decoded CAHARCTER strings
C       LSTR   - Array of length for CAHARCTER strings
C       NITEM  - No of numbers decoded
C
C  INTERNAL VARIABLES:
C       IEND   - See MOVEIP
C
C  CALL TO:
C       MOVEIP, DCSTRG
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
C       11.02.94/1.0
C
C ======================================================================
C
      INTEGER iend , Nitem , Maxitm , Maxchl , Ityp , Lstr(*)
      INTEGER IFERR
      CHARACTER*(*) Str(*)
C
      INCLUDE 'ffd.inc'
C
C ----------------------------------------------------------------------
C
      IF ( Ityp.GT.3 ) WRITE (LPU,99003)
      Nitem = 0
C
 100  CALL MOVEIP(iend)
C
      IF ( iend.EQ.0 ) THEN
         Nitem = Nitem + 1
C
         IF ( Nitem.GT.Maxitm ) THEN
            WRITE (LPU,99001) Maxitm
            CALL PRIERR()
            IDCerr = -1
            GOTO 200
         ENDIF
C
         CALL DCSTRG(Str(Nitem),Lstr(Nitem),Ityp)
C
         IF ( IFERR().LT.0 ) THEN
            WRITE (LPU,99002)
            GOTO 200
         ENDIF
         IF ( Lstr(Nitem).GT.Maxchl ) THEN
            WRITE (LPU,99004) Maxchl
            CALL PRIERR()
            IDCerr = -1
            GOTO 200
         ENDIF
C
         GOTO 100
C
      ENDIF
C
C ----------------------------------------------------------------------
 200  RETURN
C ----------------------------------------------------------------------
C
99001 FORMAT (/,' *** FFDLIB ERROR - Max no of CHARACTERs strings',
     &        ' exceeded (=',I6,') - (MODULE: DCSTRM)',/)
99002 FORMAT (/,' *** FFDLIB ERROR - Decoding CHARACTER string',
     &        ' - (MODULE: DCREAM)',/)
99003 FORMAT (/,' *** FFDLIB ERROR - Illegal ITYP selected',
     &        ' - (MODULE: DCREAM)',/)
99004 FORMAT (/,' *** FFDLIB ERROR - Max length of CHARACTER string',
     &        ' exceeded (=',I6,') - (MODULE: DCREAM)',/)
C
      END
