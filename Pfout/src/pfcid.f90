    SUBROUTINE PFCID(CID,NCOL)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!          Retrieve the channel id's  
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni 
    IMPLICIT NONE
    CHARACTER*(*) CID(*)
    INTEGER ICOL,NCOL
!
    READ(IBIK,REC=3) (cid(icol),icol=1,ncol)
!    
    RETURN
    END 

