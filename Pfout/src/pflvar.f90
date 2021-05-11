    SUBROUTINE PFLVAR(LTABVAR,NVAR)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!         Read logicals for if the variable is equal/constant for all runs. 
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni 
    IMPLICIT NONE
    LOGICAL LTABVAR(*)
    INTEGER IVAR,NVAR
!
    READ(IBIU,REC=8) (ltabvar(ivar),ivar=1,nvar)
!    
    RETURN
    END 

