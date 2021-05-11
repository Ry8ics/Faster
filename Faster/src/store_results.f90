    SUBROUTINE STORE_RESULTS(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!           Store openfast analysis results on binary file
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    use clist
    use cfuni
    implicit none
    INTEGER IPAR,ISTAT,irec,ichan,irow,iskip
    CHARACTER*136 CRES_FILE
    CHARACTER*132 FPRE
    CHARACTER*4 CEXT,CNUM
    CHARACTER*13  cfile
    INTEGER IRECU,IRECB,IRECK
    real, allocatable :: rchan(:)   
    integer icol
!.................................................    
!    Open binary fileS for storing of results.... instead of just keeping it internally in tables...
!    The binary files will be kept stored on the machine and may thus be re-read without 
!    the neeed to perform any reanlyses in openfast... e.g. if some other result is needed...
!.................................................    
!
!    Count number of channels, fetch the channel id's and units, count number of rows (time instances)
!    and do some quality checks on the output files from openfast.
    allocate(RES%CHAN(MAX_CHARACTERS))
    allocate(RES%CUNI(MAX_CHARACTERS))
    CALL CHANQA(FAST%CMAESTRO(1),CANA,NPARA,IBIF,NUMCOMM,RES%CHAN,RES%CUNI,RES%NCOL,RES%NROW)  
!.................................................    
!   Sore channels and units on binary file
    cfile=cana//'_KEYS.BIN' 
!   A character takes 1 byte (by definition) .. how many 4 byte words is there in 14 bytes?
!   Round it off to 4... add 8 to allow for at least NUMROW and RES%NCOL (number of columns
    IRECK=RES%NCOL*4+8
    CALL OPEN_BIN(IBIK,'UNKNOWN',Cfile,IRECK)
!
    WRITE(IBIK,REC=2) RES%NROW,RES%NCOL                    ! write number of rows and columns
    WRITE(IBIK,REC=3) (RES%CHAN(ICOL),ICOL=1,RES%NCOL)     ! write channel ID's
    WRITE(IBIK,REC=4) (RES%CUNI(ICOL),ICOL=1,RES%NCOL)     ! write channel units
!.................................................        
!
!   Store user input on binary file
    CALL STORE_USER(IRECU)
!.................................................        
!   
!    Store result from openfast on binary file
     cfile=cana//'_FAST.BIN'
!    If file exists it will be overwritten and the needed record length 
!    (a real takes 4 bytes) will be:
!    Correction: For some strange odd reason it seems that number to be used is 1 !!
!                Could be caused by some compiler setting... or word-length.. 
!     IRECB=RES%NCOL*4
     IRECB=RES%NCOL*1
!      
!     Open binary files for storing of results from openfast
     CALL OPEN_BIN(IBIB,'UNKNOWN',Cfile,IRECB)   
!
!    To properly reopening the files the record lenghts are needed
!    Store these in the keys-file at the very first record which can always be read, 
!    the remaining will need to know the record length.
     WRITE(IBIK,REC=1) IRECB,IRECK,IRECU
!    
!    Temporary table to retreive channel results
!     ALLOCATE (rchan(NCHAN_FAST))
     ALLOCATE (rchan(RES%NCOL))
!
!    Assume the name of the output file from openfast is equal to the input file
!    with the extension .out.
     CEXT='.out'             ! extension of output file from openfast
     irec=0
     DO IPAR=1,NPARA
!   
         WRITE(CNUM,'(I4)') IPAR          
!        CRES_FILE=CINPUT_FILE_FAST(IPAR)(1:len(trim(CINPUT_FILE_FAST(IPAR)))-4)// '.out'  
     
         FPRE=fast%cmaestro(1)(1:len(trim(fast%cmaestro(1)))-4)// &  
               '_'//CANA//'_'//trim(adjustl(CNUM))
         CRES_FILE=trim(FPRE)//CEXT        ! Output file that will be created        
!
!        Open the result file ...     
         CALL OPEN_ASCII(IBIF,'OLD',CRES_FILE) 
!
!         Assume first NUMCOMM lines are comments (presumably 6)
!         Then follows the channel and units, i.e. 2 more..
         do iskip=1,NUMCOMM+2
             read (ibif,*)
         enddo
 
!         DO IROW=1,NUMROW
         DO IROW=1,RES%NROW
!            read it... 
!            at least 8 lines are already read and we should now be down to the juice.
!            To increase speed a format statement should prefereably be used for reading.
             READ(IBIF,*) (RCHAN(ICHAN),ICHAN=1,RES%NCOL)    
!            And store the results on binary file
             irec=irec+1
!             WRITE (IBIB,REC=IREC) (RCHAN(ICHAN),ICHAN=1,NCHAN_FAST)                    
             WRITE (IBIB,REC=IREC) (RCHAN(ICHAN),ICHAN=1,RES%NCOL)                    
         ENDDO          
     ENDDO 
!    
     CLOSE (IBIF)
     CLOSE (IBIB)
     CLOSE (IBIU)
     CLOSE (IBIK)   
!.................................................        
    RETURN
    END