      SUBROUTINE DCCHANNEL(Cerr,Citem,Istat)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Decode the listing commmand
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
      INTEGER iend , IFERR , ilen , intnum ,Istat,IRUN
      EXTERNAL IPFNUM,IPFCHA
      INTEGER IPFNUM,IPFCHA,NCHANNELS,ICHAN
!
      LIST%CHANNEL=.FALSE.
      CALL MOVEIP(iend)
      IF ( iend.NE.0 ) THEN
!           Nothing is given... just list the channel id's
            LIST%CHANNEL=.TRUE.
            GOTO 100
      ENDIF
!............................................................
      IF ( MATCH('FILE',3) ) THEN
!         The output will be written to external files
          LIST%FILE=.TRUE.
      ENDIF
!
!     Read channel to be evaluated
      IF ( MATCH('ALL',3) ) THEN
!        Write all available channels...
!        Ok. Just fill in...
         LIST%ALL=.TRUE.    
         nchannels=ipfnum('C')    
         DO intnum=1,nchannels
             LIST%MCHAN(intnum)=intnum
         ENDDO     
         LIST%NCHAN=nchannels
         GOTO 100      ! Nothing more to do here..
      ENDIF
!
      intnum=0
      LIST%NCHAN=0
!
200   CALL DCSTRG(channel,ilen,3)
      IF ( IFERR().LT.0 ) THEN
         WRITE (IOUT,Citem) channel
         WRITE (IOUT,Cerr) 'ERROR IN READING CHANNEL ' , &
                           'PROGRAM STOPPED'
         Istat = -1
         GOTO 100
      ENDIF
      ichan=ipfcha(channel) ! Retreive channel no for given channel id
!   
      IF (ichan.eq.0) THEN
         CALL PRIERR()
         WRITE (IOUT,Citem) channel
         write(*,*) ' FASTER ERROR: REQUESTED CHANNEL DO NOT EXIST'
         write(*,*) ' PROGRAM STOPPED'
         Istat = -1
         GOTO 100
      ENDIF
!     
      intnum=intnum+1
      LIST%MCHAN(intnum)=ichan
      LIST%NCHAN=LIST%NCHAN+1
!
      CALL MOVEIP(iend)          
      IF ( iend.EQ.0 ) GOTO 200     ! List more channels?       
!
 100  RETURN
      END
