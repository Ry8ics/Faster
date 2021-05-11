      SUBROUTINE DCSTAT(Cerr,Citem,Istat)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!           Decode channels for statistical analysis
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      use clist
      IMPLICIT NONE
      INCLUDE 'redwri'
      CHARACTER*(*) Cerr , Citem
      CHARACTER*14 channel
      LOGICAL MATCH
      INTEGER iend , IFERR , imemb , ilen , intnum ,Istat
!
      intnum=0
 20   CALL MOVEIP(iend)
      IF ( iend.EQ.0 ) THEN
         CALL DCSTRG(channel,ilen,3)
!
         IF ( IFERR().LT.0 ) THEN
            WRITE (IOUT,Citem) channel
            WRITE (IOUT,Cerr) 'ERROR IN READING CHANNEL ' , &
                              'PROGRAM STOPPED'
            Istat = -1
            GOTO 100
         ENDIF
!
         intnum=intnum+1
         LIST%CHAN_REP(intnum) = channel
         LIST%NCHAN_REP=LIST%NCHAN_REP+1
!
         GOTO 20
      ENDIF
 100  RETURN
      END
