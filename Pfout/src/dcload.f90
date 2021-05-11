      SUBROUTINE DCLOAD(Cerr,Citem,Istat)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Activate a list of runs to be reported.
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      use clist
      IMPLICIT NONE
      INCLUDE 'redwri'
      CHARACTER*(*) Cerr , Citem
      LOGICAL MATCH
      EXTERNAL ipfnum
      INTEGER iend , IFERR , intnum ,IPAR, Istat,IRUN, ipfnum,NPARA
!
      intnum=0
      LIST%NRUNS=0
      npara =  ipfnum('P')
      IF ( MATCH('ALL',3) ) THEN
!         Activiate all runs...
          DO IPAR=1,NPARA
              intnum=intnum+1
              LIST%MRUN(intnum)=intnum
              LIST%NRUNS=LIST%NRUNS+1  
          ENDDO
      ELSE
!         Activate a list of runs
200       CALL DCINT(Irun)
          IF ( IFERR().LT.0 ) THEN
             WRITE (IOUT,Cerr) 'ERROR IN READING RUN ' , &
                               'PROGRAM STOPPED'
             Istat = -1
             GOTO 100
          ENDIF
!
          IF (Irun.gt.NPARA) THEN
              CALL PRIERR()
              WRITE (IOUT,Cerr) 'Openfast run do not exist.' , &
                                'PROGRAM STOPPED'
             Istat = -1
             GOTO 100                   
          ENDIF           
!
          intnum=intnum+1
          LIST%MRUN(intnum)=Irun
          LIST%NRUNS=LIST%NRUNS+1  
!
          CALL MOVEIP(iend)
          IF ( iend.EQ.0 ) GOTO 200                           
      ENDIF
!
 100  RETURN
      END
