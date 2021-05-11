    SUBROUTINE READCHAN(IBIF,CHAN,CUNIT,NCHAN,NUMCOMM,ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Retrieve number of channels, channel ids and units.
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    IMPLICIT NONE
    CHARACTER*(*) CHAN(*),CUNIT(*)
    INTEGER IBIF,NCHAN,IREC,NUMCOMM,ISTAT,ICHAN,IO
!
!   Bet that the number of characters of one line is less than MAX_CHARACTERS
    INTEGER, PARAMETER :: MAX_CHARACTERS=2500    
    CHARACTER(MAX_CHARACTERS) CLINE
!
    ISTAT=0
!
!   Assume first NUMCOMM lines are comments (presumably=6)
!   Then follows the channel ID's
    REWIND(IBIF)
    DO IREC=1,NUMCOMM
       READ (IBIF,*,IOSTAT=ISTAT)
       IF (ISTAT==-1) RETURN
    ENDDO 
!..............................................................
!   Read the line containing the channel ID's
!
    READ(IBIF,'(A)',IOSTAT=ISTAT) CLINE
    IF (ISTAT==-1) RETURN
!
   DO NCHAN=1,MAX_CHARACTERS
!    Get rid of nasty slash.... a slash will be interpreted as "new line" when reading.
     IF(CLINE(NCHAN:NCHAN).EQ."/") CLINE(NCHAN:NCHAN)=':' 
     READ(CLINE,*,IOSTAT=IO) CHAN(:NCHAN)   ! These will be the channel ID's
     IF(IO==-1) EXIT
   ENDDO
!
   NCHAN=NCHAN-1     ! We now got number of channels as well as the channel IDs
!..............................................................
!   Take the next line also, which contains the unit identifiers 
    READ(IBIF,'(A)',IOSTAT=ISTAT) CLINE
    IF (ISTAT==-1) RETURN
!
    DO ICHAN=1,MAX_CHARACTERS
!      Get rid of nasty slash.... a slash will be interpreted as "new line" when reading.
       IF(CLINE(ICHAN:ICHAN).EQ."/") CLINE(ICHAN:ICHAN)=':'
       READ(CLINE,*,END=600) CUNIT(:ICHAN)  ! These will be the channel units
   ENDDO
!..........................................................
600 RETURN
    END
