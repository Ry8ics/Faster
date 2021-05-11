    SUBROUTINE PFUNI(CUNI,NCOL)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!        Retrieve the channel units 
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni 
    IMPLICIT NONE
    CHARACTER*(*) CUNI(*)
    INTEGER ICOL,NCOL
!
    READ(IBIK,REC=4) (cuni(icol),icol=1,ncol)
!    
    RETURN
    END 

