    SUBROUTINE PFCRES(irun,ncol,nrow,rdata)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!          Read all channel results for the given run
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni 
    IMPLICIT NONE
!
    INTEGER irun,ncol,nrow
    INTEGER irow,icol,istart,irec
    REAL rdata(ncol,nrow)
!   
    irow=0
!
!   Start record
    istart=nrow*(irun-1)+1
!    
!   Read all the channels into matrice..
    do irec=istart,istart+nrow-1
        irow=irow+1
        read (ibib,rec=irec) (rdata(icol,irow),icol=1,ncol)
    enddo    
!
    return
    end

