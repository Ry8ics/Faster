    SUBROUTINE PFCMOR(irun,imor,ncol,nrow,nmor,rdata)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!           Read moordyn results
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni 
    IMPLICIT NONE
!
    INTEGER irun,ncol,nrow,nmor
    INTEGER irow,icol,istart,irec,imor
    REAL rdata(ncol,nrow)
!   
    irow=0
!
!   Start record
    istart=(irun-1)*nrow*nmor+(imor-1)*nrow+1
!    
!   Read all the channels into matrice..
    do irec=istart,istart+nrow-1
        irow=irow+1
        read (ibir,rec=irec) (rdata(icol,irow),icol=1,ncol)
    enddo    
!
    return
    end

