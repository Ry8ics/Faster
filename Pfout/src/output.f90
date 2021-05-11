    Subroutine output(istat)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!          Generate output
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use clist
    implicit none
    INTEGER ISTAT
!------------------------------------------
!
!    Read the requested output 
     
     IF (LIST%CHANNEL)        CALL OUTCID()    !  print channel id's
     IF (LIST%NRUNS.GT.0)     CALL OUTCHA()    !  print channel results
     IF (LIST%NCHAN_REP.GT.0) CALL OUTSTA()    !  print statistics for all runs
     IF (LIST%MOOR)           CALL OUTMOR()    !  print moordyn results
!
     return
     end