    SUBROUTINE MEMALY(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!          Allocate memory needed during reporting of resuls.
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    use clist
    implicit none
    integer istat,nchannels
    external ipfnum
    integer ipfnum
    npara =  ipfnum('P')
    allocate (LIST%MRUN(NPARA))         ! Array of runs to be reported. NPARA is number of openfast runs.
    nchannels=ipfnum('C')               ! Corresponds to number of columns.
    allocate (LIST%CHAN_REP(nchannels)) ! Channel ID's to be reported. Could be lower than nchannels. Never mind.
    allocate (LIST%MCHAN(nchannels))    ! Channel results to be reported.
    ISTAT=1                             ! Assume all good.
    RETURN                              ! Note, that's we've now started all over and assumed that all the results
                                        ! as produced by Openfast is stored on binary files, ready to be displayed.
    END