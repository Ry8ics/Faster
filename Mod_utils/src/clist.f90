      MODULE CLIST
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!             Module for result listing.
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================

      IMPLICIT NONE
      CHARACTER*64                 :: FASTER_VERSION='FASTER version 2.0.0'
!
      CHARACTER*4                  :: CANA='ABBA'           ! Unique identifacion of all the Openfast analyses
!
      TYPE TLIST
        LOGICAL                    :: ALL= .FALSE.          ! Logical for if all runs shall be reported.
        LOGICAL                    :: CHANNEL = .FALSE.     ! Logical for if listing of channel id's'
        LOGICAL                    :: FILE = .FALSE.        ! Logical for if writing output to external file
        LOGICAL                    :: MOOR =.FALSE.         ! Logical for listing moordyn results ..NOT USED?
        CHARACTER*14               :: CHAN_RUN              ! Channel to be reported for NRUNS runs.
        INTEGER, allocatable       :: MCHAN(:)              ! Channel numbers to be reported
        INTEGER                    :: NCHAN=0               ! Number of channels to be reported
        INTEGER                    :: NCHAN_REP=0           ! Number of channel id's to be reported (given in list command)
        CHARACTER*14, allocatable  :: CHAN_REP(:)           ! Channel id's as requested to be reported by faster.
        INTEGER                    :: NRUNS=0               ! Number of runs to be reported
        INTEGER , allocatable      :: MRUN(:)               ! Array of runs to be reported 
      END TYPE TLIST
!
      TYPE (TLIST) ::  LIST
 END MODULE 