    SUBROUTINE PFVAR(CTABVAR,NVAR)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!        Read the user defined variables as given in the input file (WaveHs, WaveTp,... etc) 
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni 
    IMPLICIT NONE
    CHARACTER*(*) CTABVAR(*)
    INTEGER IVAR,NVAR
!
    READ(IBIU,REC=7) (ctabvar(ivar),ivar=1,nvar)
!    
    RETURN
    END 

