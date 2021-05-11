    Subroutine outmor()
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!           Output mordyn results
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use clist
    use cfuni       
    implicit none
    INTEGER NROW,NCOL,IROW,NMOR,ICOL,IMOR,NSEG,ISEG,icor,iten
    INTEGER IRUN,iout
    CHARACTER*14, allocatable :: cuni(:),cid(:)
    real, allocatable :: rdata(:,:)
!
!   Number of rows (time instances) , columns and number of mooring lines...
    READ(IBIM,REC=2) NROW,NCOL,NMOR,NSEG     ! Number of rows, columns, mooring lines and mooring segments
!
    allocate (rdata(ncol,nrow)) 
    allocate (cuni(ncol)) 
    allocate (cid(ncol)) 
!
    READ(IBIM,REC=3) (cid(icol),icol=1,ncol)    ! moordyn channel id's
    READ(IBIM,REC=4) (cuni(icol),icol=1,ncol)   ! moordyn channel units
!
    iout=6     ! just write to standard output here.. to be updated, ref. outcha.f90
    CALL PFHEAD(iout,CANA,FASTER_VERSION)   ! Make a header
!    
!   Collect data for the runs to be reported..
    
    irun=1  ! same as ipar !!!!
    irow=1  ! time instance  ... irow=1 is of course at t=0 sec.
!
    do irow=1,nrow    ! this will probably generate too much output... 
!                     ! never mind... only relevant for floaters and thus not much needed
       do imor=1,nmor   ! mooring line
              call PFCMOR(irun,imor,ncol,nrow,nmor,rdata)   ! retreive channel data
!.........................................................................................
!            - Extract the coordinates for a given time instance (i.e. irow)
!            - Extract the tension for a given time instance
!            - and Pulling it all together. 
!              The tension forces will be assigned to start node of the segment.
!     
              WRITE(*,'(A,i2)') '# Mooring line no.: ',imor
              WRITE(*,'(A,1P,E10.3)') '# Time instance...: ',rdata(1,irow)
              WRITE(*,'(a1,4(2x,A10))') '#', 'X','Y','Z','Tension'
              do iseg=1,nseg
                  icor=3*(iseg-1)+2
                  iten=3*(nseg+1)+1+iseg
                  write(*,'(1x,1P,4(2x,E10.3))') (rdata(icol,irow),icol=icor,icor+2),rdata(iten,irow)
              enddo     
!             and here's the last node...with the same tension as the second last... 
             icor=3*(nseg)+2
             iten=3*(nseg+1)+1+nseg
             write(*,'(1x,1P,4(2x,E10.3))') (rdata(icol,irow),icol=icor,icor+2),rdata(iten,irow)  
       enddo
    enddo
!................................................................................
    return
    end