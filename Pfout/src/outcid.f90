    Subroutine outcid()
 !=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!          Output channels id's and units
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use clist
    use cfuni       
    implicit none
    INTEGER IPFNUM,NCOL,ICOL
    CHARACTER*14, allocatable :: cuni(:),cid(:)
!
    NCOL= IPFNUM('C')
!
    allocate (cuni(ncol)) 
    allocate (cid(ncol)) 
!
    call pfuni(cuni,ncol)     ! channel units
    call pfcid(cid,ncol)      ! channel id's
!   
    DO ICOL=1,NCOL
        WRITE(*,*) ICOL,trim(cid(ICOL)),' ',trim(cuni(ICOL))
    ENDDO
    
    return
    end