    Subroutine outcha()
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!           Output channels as requested by user
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use clist
    use cfuni       
    implicit none
    INTEGER NPAR,NVAR,NROW,NCOL,IROW
    INTEGER IPAR,IVAR,ICHAN,IRUN
    INTEGER IPFNUM,ipfcha,nrun,nchan
    CHARACTER*14, allocatable :: cuni(:),cid(:)
    real, allocatable :: rdata(:,:)
    CHARACTER*1 CC
!....................................
    CHARACTER*14, allocatable :: cvar(:),cvar_VARY(:),cvar_CONST(:),cpar(:),cpar_CONST(:),cpar_VARY(:,:)
    LOGICAL, allocatable :: LVAR(:)
    integer nconst,iconst,nvary
    integer iout
    CHARACTER*4 CNUM,CEXT
    CHARACTER*13 CFILE
!....................................
!
    NPAR= IPFNUM('P')
    NVAR= IPFNUM('V')
    NROW= IPFNUM('R')
    NCOL= IPFNUM('C')
!
    allocate (LVAR(NVAR))
    allocate (cvar(NVAR))    ! defined variables (WaveHs, WaveTp ,..etc)
    allocate (cvar_vary(NVAR))  
    allocate (cvar_const(NVAR))      
    CALL PFLVAR(Lvar,NVAR)
    CALL PFVAR(cvar,NVAR)
!
    allocate (cpar_CONST(NVAR)) 
    allocate (cpar_VARY(NVAR,NPAR)) 
!...............
    allocate (cpar(NVAR))       ! Parameters unique for each run
    allocate (rdata(ncol,nrow)) ! and here's the results produced by Openfast
    allocate (cuni(ncol))       ! with these units
    allocate (cid(ncol))        ! and identifications
!...............
!
    call pfuni(cuni,ncol)       ! channel units
    call pfcid(cid,ncol)        ! channel id's
!...........................................................................................
!   Make a table of parameters. 
!   Make a split between parameters that are equal (constant) 
!   and parameters that varies for each run. This is made just for output purposes.
!   It would be much better to have 2 different retrieval routines, i.e. something like
!   PFPAR_VARY and PFPAR_CONST.... but it works... so leave it...
     nconst=0
     nvary=0
     ipar=1
     CALL PFPAR(CPAR,IPAR,NVAR)      ! Retreive the parameters for the first openfast run      
     do ivar=1,nvar
        if(lvar(ivar)) then          ! Constant for each run?
            nconst=nconst+1
            cvar_CONST(nconst)=cvar(ivar) 
            cpar_CONST(nconst)=cpar(ivar)   
        else
            nvary=nvary+1       
            cvar_VARY(nvary)=cvar(ivar)   
            do ipar=1,npar
               CALL PFPAR(CPAR,IPAR,NVAR)  ! Retreive the parameters for a given openfast run  
               cpar_VARY(nvary,ipar)=cpar(ivar) 
            enddo
        endif
     enddo      
!...........................................................................................
!
    CC='#'      ! comment  
    CEXT='.out'
    do irun=1,LIST%NRUNS   
        nchan=LIST%NCHAN
        ipar=LIST%MRUN(irun)
        call PFCRES(ipar,ncol,nrow,rdata)  ! retreive channel data
!..........................................................
!
        IF (LIST%FILE) THEN    
           WRITE(CNUM,'(I4)') ipar 
           CFILE=CANA//'_'//trim(adjustl(CNUM))//CEXT
           CALL open_ascii(IBIO,'UNKNOWN',Cfile) !  Set file for output
           IOUT=IBIO   ! Write to file
        ELSE
           IOUT=6      ! Write to standard output
        ENDIF
!
        CALL PFHEAD(IOUT,CANA,FASTER_VERSION)   !  Write header 
!        
        do iconst=1,nconst
            write(iout,'(A1,1X,A14,A,A14)') CC,cvar_CONST(iconst),' ',cpar_CONST(iconst)
        enddo
!
        write(iout,'(a1)') CC
        write(iout,'(a1,a,i5)') CC,' Run number ....: ',ipar
        write(iout,'(a1)') CC
        if (nvary.ge.1) then
           WRITE(iout,'(A1,2x,8x,<nvary>(a14,2x))') CC,(cvar_vary(ivar),ivar=1,nvary) 
           write(iout,'(A1,3x,8x,<nvary>(a14,2x))') CC,(cpar_vary(ivar,ipar),ivar=1,nvary) 
        endif
        write(iout,'(a1)') CC
!..........................................................
!
        IF (.NOT.LIST%ALL.AND.nchan.gt.0) THEN          
           write(iout,'(A1,1x,A10,<nchan>(2x,A10))') CC, trim(cid (1)), (trim(cid (LIST%MCHAN(ichan))),ichan=1,nchan)
           write(iout,'(A1,1x,A10,<nchan>(2x,A10))') CC, trim(cuni(1)), (trim(cuni(LIST%MCHAN(ichan))),ichan=1,nchan) 
           do irow=1,nrow
             write(iout,'(2x,F10.4,1P,<nchan>(2x,E10.3))') rdata(1,irow), (rdata(LIST%MCHAN(ichan),irow),ichan=1,nchan) 
           enddo
        ELSEIF (nchan.gt.0) THEN
           write(iout,'(A1,1x,A10,<nchan>(2x,A10))') CC, trim(cid (1)), (trim(cid (LIST%MCHAN(ichan))),ichan=2,nchan)
           write(iout,'(A1,1x,A10,<nchan>(2x,A10))') CC, trim(cuni(1)), (trim(cuni(LIST%MCHAN(ichan))),ichan=2,nchan) 
           do irow=1,nrow
             write(iout,'(2x,F10.4,1P,<nchan>(2x,E10.3))') rdata(1,irow), (rdata(LIST%MCHAN(ichan),irow),ichan=2,nchan)      
           enddO
        ENDIF
        CLOSE(IBIO)
    enddo    
    return
    end