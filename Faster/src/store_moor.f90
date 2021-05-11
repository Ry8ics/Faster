    SUBROUTINE STORE_MOOR(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Store moordyn results on binary file
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    use clist
    use cfuni
    implicit none
    INTEGER IPAR,ISTAT,NUMROW,irec,ichan,irow,iskip,IMOOR
    CHARACTER*132 CRES_FILE
    CHARACTER*13  cfile
    CHARACTER*2 CMOOR_LINE
    CHARACTER*4 CEXTT,CNUM
    CHARACTER*136 FPRE
    INTEGER IRECB,IRECK,ICOL
    real, allocatable :: rchan(:)   
!.................................................    
!    Open binary fileS for storing of results.... instead of just keeping it internally in tables...
!    The binary files will be kept stored on the machine and may thus be re-read without 
!    the neeed to perform any reanlyses in openfast... e.g. if some other result is needed...
!.................................................    
!
!    Count number of channels, fetch the channel id's and units, count number of rows (time instances)
!    and do some quality checks on the output files from moordyn.
    CALL CHANQA_MOOR(FAST%CMAESTRO(1),CANA,NPARA,IBIF,NUMCOMM,RES%CHAN,RES%CUNI,RES%NCOL,RES%NROW,MOOR_LINES)
!    CALL CHANQA(FAST%CMAESTRO(1),    CANA,NPARA,IBIF,NUMCOMM,RES%CHAN,RES%CUNI,           RES%NCOL,RES%NROW)  
!.................................................    
!   Sore channels and units on binary file
    cfile=cana//'_KEYM.BIN' 
!   A character takes 1 byte (by definition) .. how many 4 byte words is there in 14 bytes?
!   Round it off to 4... add 16 to allow for at least NUMROW,NCHAN_FAST,MOOR_LINES and NSEGMENTS
    IRECK=RES%NCOL*4
    IF (IRECK.LT.12) IRECK=16
    CALL OPEN_BIN(IBIM,'UNKNOWN',Cfile,IRECK)
!
    WRITE(IBIM,REC=2) RES%NROW,RES%NCOL,MOOR_LINES,NSEGMENTS
    WRITE(IBIM,REC=3) (RES%CHAN(ICOL),ICOL=1,RES%NCOL)     ! write channel ID's
    WRITE(IBIM,REC=4) (RES%CUNI(ICOL),ICOL=1,RES%NCOL)     ! write channel units
!.................................................        
!   
!    Store result from moordyn on binary file
     cfile=cana//'_MOOR.BIN'
!    If file exists it will be overwritten and the needed record length 
!    (a real takes 4 bytes) will be:
!    Correction: For some strange odd reason it seems that number to be used is 1 !!
!                Could be caused by some compiler setting... or word-length.. 
     IRECB=RES%NCOL*1
!      
!     Open binary files for storing of results from openfast
     CALL OPEN_BIN(IBIR,'UNKNOWN',Cfile,IRECB)   
!
!    To properly reopening the files the record lenghts are needed
!    Store these in the keys-file at the very first record which can always be read, 
!    the remaining will need to know the record length.
     WRITE(IBIM,REC=1) IRECB,IRECK
!    
!    Temporary table to retreive channel results
     ALLOCATE (rchan(RES%NCOL))
!
     irec=0
     CEXTT='.out'       ! extension of output file from openfast
     DO IPAR=1,NPARA
!     
         WRITE(CNUM,'(I4)') IPAR  
         DO IMOOR=1,MOOR_LINES            
             write (CMOOR_LINE,'(I2)') IMOOR
!            Assume the name of the output file from openfast is equal to the input file
!            with the extension .out.
!             CRES_FILE=CINPUT_FILE_FAST(IPAR)(1:len(trim(CINPUT_FILE_FAST(IPAR)))-4)// &
!                                            '.MD.Line'//trim(adjustl(CMOOR_LINE))//'.out'
             FPRE=FAST%CMAESTRO(1)(1:len(trim(FAST%CMAESTRO(1)))-4)// &  
                     '_'//CANA//'_'//trim(adjustl(CNUM))
             CRES_FILE=trim(FPRE)// '.MD.Line'//trim(adjustl(CMOOR_LINE))//CEXTT   ! Output file that by now should already exist
!
!            Open the result file ...     
             CALL OPEN_ASCII(IBIF,'OLD',CRES_FILE) 
!
!             Assume first NUMCOMM=0 lines are comments 
!             Then follows the channel and units, i.e. 2 more..
             do iskip=1,0+2
                 read (ibif,*)
             enddo
 
!             DO IROW=1,NUMROW
             DO IROW=1,RES%NROW
!                read it... 
!                at least 2 lines are already read and we should now be down to the juice.
!                To increase speed a format statement should prefereably be used for reading.
                 READ(IBIF,*) (RCHAN(ICHAN),ICHAN=1,RES%NCOL)   
!                And store the results on binary file
                 irec=irec+1
                 WRITE (IBIR,REC=IREC) (RCHAN(ICHAN),ICHAN=1,RES%NCOL)                   
             ENDDO     
         ENDDO
     ENDDO 
!    
     CLOSE (IBIF)
     CLOSE (IBIR)
     CLOSE (IBIM)   
!.................................................        
    RETURN
    END