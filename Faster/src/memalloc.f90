    SUBROUTINE MEMALLOC(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!         Allocate memory
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    implicit none
    integer istat
    FAST%NMASTER=0
    FAST%NSLAVE=0
    SLAVE%NSLAVE=0
    SLAVE%NSWAP=0
!
    if (allocated (FAST%CMASTER))  deallocate (FAST%CMASTER)
    allocate (FAST%CMASTER(MAX_MASTER))
!     
    if (allocated (FAST%MSLAVES))  deallocate (FAST%MSLAVES)
    allocate (FAST%MSLAVES(MAX_MASTER))
!
    if (allocated (FAST%CSLAVE))  deallocate (FAST%CSLAVE)    
    allocate (FAST%CSLAVE(MAX_SLAVE,MAX_MASTER))
!
    if (allocated (SLAVE%CSLAVE))  deallocate (SLAVE%CSLAVE)        
    allocate (SLAVE%CSLAVE(MAX_SLAVE))
!
    if (allocated (SLAVE%CSWAP))  deallocate (SLAVE%CSWAP)       
    allocate (SLAVE%CSWAP(MAX_SLAVE))
!
    if (allocated (SLAVE%CSWAP_STR_FROM))  deallocate (SLAVE%CSWAP_STR_FROM)           
    allocate (SLAVE%CSWAP_STR_FROM(MAX_SLAVE))
!
    if (allocated (SLAVE%CSWAP_STR_TO))  deallocate (SLAVE%CSWAP_STR_TO)    
    allocate (SLAVE%CSWAP_STR_TO(MAX_PAR,MAX_SLAVE))
!
    if (allocated (SLAVE%NVAR))  deallocate (SLAVE%NVAR)  
    allocate (SLAVE%NVAR(MAX_SLAVE))
!
    if (allocated (SLAVE%CVAR))  deallocate (SLAVE%CVAR)  
    allocate(SLAVE%CVAR(MAX_VAR,MAX_SLAVE))
!
    if (allocated (SLAVE%CONST))  deallocate (SLAVE%CONST)  
    allocate(SLAVE%CONST(MAX_VAR,MAX_SLAVE))
!
    if (allocated (SLAVE%CPAR))  deallocate (SLAVE%CPAR)  
    allocate(SLAVE%CPAR(MAX_PAR,MAX_VAR,MAX_SLAVE))
!
    ISTAT=1    ! Assume all good
!
    RETURN
    END