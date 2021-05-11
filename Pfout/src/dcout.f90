      SUBROUTINE DCOUT(ISTAT) 
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Decode user defined output
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================    
      use clist
      IMPLICIT NONE
      INCLUDE 'redwri'
!
      INTEGER IFERR,ISTAT,INCLUD,IEND,ILEN1
      LOGICAL MATCH
      CHARACTER*80 CBLANK
      CHARACTER*40 CITEM,IITEM,CERR
      CHARACTER*7 CSTAT
!
      DATA INCLUD/0/
      DATA CITEM/'(/''  INPUT ITEM...: '',A ,'' ?'')'/
      DATA IITEM/'(/''  INPUT ITEM...: '',I2,'' ?'')'/
      DATA CERR/'(/''   FASTER ERROR.: '',A/:9(18X,A/:))'/
!
      CBLANK(1:80) = ' '
!
!     SET THE INPUT AND OUTPUT UNITS
!
      CALL INITDC(IINN,IOUT)          
      rewind(iinn)                      ! make sure to start reading from the top
!
!     START DECODING THE INPUT FILE
!
 100  CALL READL
!
!     CHECK IF ANYTHING IS LEFT
!
      IF ( (IFERR().GE.0) .OR. (INCLUD.NE.0) ) THEN
         IF ( (IFERR().LT.0) .AND. (INCLUD.EQ.1) ) THEN
            CALL PUTCOM(1,0)
            INCLUD = 0
            CALL INITDC(IINN,IOUT)
            GOTO 100
         ENDIF
!--------------------------------------------
!        List user defined data. The results from openfast will be read from binary files.
         IF ( MATCH('LIST',3) ) THEN
            CALL DCLIST(cerr,citem,Istat)
!
!           If several commands are given here, you probably need to include it in
!           dcmain as well, in order to avoid error,i.e:
!
!        ELSEIF ( MATCH('something',3) ) THEN   ! (insert in dcmain as well...)
!
!           If you wonder what that 3 means, it's just that the command is recognised
!           when the 3 first characters are read, i.e. you could write "som" and the
!           if statement becomes true. This will work just fine provided you don't
!           have some other command that starst with "som" in which case you could just
!           increase it to 4.
!          
!-------------------------------------------------------
!
!     CHECK FOR LEGAL COMMENT SIGNS
!
         ELSEIF ( MATCH('%',1) ) THEN
         ELSEIF ( MATCH('$',1) ) THEN
         ELSEIF ( MATCH('#',1) ) THEN
         ELSEIF ( MATCH('!',1) ) THEN
         ELSEIF ( MATCH('*',1) ) THEN
!
!     BLANK LINE?
!
         ELSEIF ( .NOT.(MATCH(CBLANK,80)) ) THEN
!!            CALL PRIERR()      ! Will need to ignore anything that is unknown here...
!!            WRITE (IOUT,CERR) 'UNKNOWN INPUT'
!!            ISTAT = -1   
!!            GOTO 200
         ENDIF
         GOTO 100
      ENDIF
!
 200  RETURN
      END
