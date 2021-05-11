    SUBROUTINE PFPAR(CPAR,IPAR,NVAR)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!       Read the user defined parameters for given openfast run.
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni 
    IMPLICIT NONE
    CHARACTER*(*) CPAR(*)
    INTEGER IPAR,NVAR,IVAR
!
    READ(IBIU,REC=8+IPAR) (cpar(ivar),ivar=1,nvar)
!    
    RETURN
    END 

