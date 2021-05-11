    Subroutine rstat(rdata,rmin,rmax,rmean,rstd,nrow)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!       Evaluate some statistical data (min,max,mean and standard deviation)
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================    
    implicit none
    real rdata(nrow),rmin,rmax,rsum,rmean,rvar,rstd 
    integer nrow,irow
!        
    rmin=1.0E+22
    rmax=-1.0E+22
    rsum=0.0
!   Evaluate min,max and mean
    do irow=1,nrow
       if (rdata(irow)<rmin)  rmin=rdata(irow)
       if (rdata(irow)>rmax)  rmax=rdata(irow)
       rsum=rsum+rdata(irow)
    enddo
    rmean=rsum/real(nrow)
!
    rvar=0.0
!   Evaluate standard deviation
    do irow=1,nrow
       rvar=rvar+(rdata(irow)-rmean)**2     
    enddo
!   
    rstd=sqrt(rvar/real(nrow))
!
    RETURN
    END 

