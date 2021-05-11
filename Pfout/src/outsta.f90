    Subroutine outsta()
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!         Output statistics for channels as given by user
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use clist
    implicit none
    INTEGER NPAR,NVAR,NROW,NCOL
    INTEGER IPAR,IVAR,ICHAN,ichan_rep
    INTEGER IPFNUM,ipfcha,IOUT
    REAL rminX,rmaxX,rmeanX,rstdX
    CHARACTER*14, allocatable :: cvar(:),cvar_VARY(:),cvar_CONST(:),cpar(:),cpar_CONST(:),cpar_VARY(:,:),cuni(:)
    LOGICAL, allocatable :: LVAR(:)
    real, allocatable :: rdata(:,:)    
    integer nconst,iconst,nvary
    CHARACTER*1 CC
!
    NPAR= IPFNUM('P')
    NVAR= IPFNUM('V')
    NROW= IPFNUM('R')
    NCOL= IPFNUM('C')
!........................................................................
!   Retreive user defined variables (WaveHs, WaveTp ,..etc)
    allocate (cvar(NVAR))  
    allocate (cvar_vary(NVAR))  
    allocate (cvar_const(NVAR))  
    allocate (LVAR(NVAR))  
    CALL PFVAR(cvar,NVAR)
    CALL PFLVAR(Lvar,NVAR)
!
    allocate (cpar(NVAR)) 
    allocate (cpar_CONST(NVAR)) 
    allocate (cpar_VARY(NVAR,NPAR)) 
!........................................................................
    allocate (rdata(ncol,nrow)) 
    allocate (cuni(ncol))    
    call pfuni(cuni,ncol)     ! channel units
!................................................................................
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
!................................................................................
     IOUT=6     ! This will always be written to standard output
     CALL PFHEAD(IOUT,CANA,FASTER_VERSION)     ! Make a header
     CC='#'      ! comment
!................................................................................
!   Loop over all number of channels defined by user (in the LIST command )
    do ichan_rep=1,LIST%NCHAN_REP
!
        ichan=ipfcha(LIST%CHAN_REP(ichan_rep))    ! Retreive channel no for given channel id
        if (ichan.eq.0) then
            write(*,*) ' FASTER ERROR: REQUESTED CHANNEL DO NOT EXIST'
            write(*,*) '               CHANNEL ID..: ',LIST%CHAN_REP(ichan_rep)
            STOP
        endif
        WRITE(*,'(a1)') CC
        WRITE(*,'(a1,A,A14)') CC,' Channel id...: ',LIST%CHAN_REP(ichan_rep) 
        WRITE(*,'(a1,A,I4)')  CC,' Channel no...: ',ichan    
        write(*,'(a1)') CC
        do iconst=1,nconst
            write(*,'(A1,1X,A14,A,A14)') CC,cvar_CONST(iconst),' ',cpar_CONST(iconst)
        enddo
        write(*,'(a1)') CC
!
        if (nvary.ge.1) then
           WRITE(*,'(a1,2x,A4,8x,<nvary>(a14,2x),4(2x,A10))') CC, &
           'Run',(cvar_vary(ivar),ivar=1,nvary) ,'MIN','MAX','MEAN','STDEV'        
           write(*,'(a1,14x,<nvary>(16x),4(2x,A10))') CC ,  (trim(cuni(ichan)),ipar=1,4)
        else
            WRITE(*,'(a1,2x,A4,8x,4(2x,A10))') CC, 'Run' ,'MIN','MAX','MEAN','STDEV'        
           write(*,'(a1,14x,4(2x,A10))') CC,  (trim(cuni(ichan)),ipar=1,4)
        endif
!
        do ipar=1,npar                       ! Loop over openfast runs   
           call PFCRES(ipar,ncol,nrow,rdata) ! retreive channel data
           call rstat(rdata(ichan,:),rminX,rmaxX,rmeanX,rstdX,nrow) ! statistics for channel ichan
           if (nvary.ge.1) then
              write(*,'(3x,I4,8x,<nvary>(a14,2x),1P,4(2x,E10.3))') &
                IPAR,(cpar_vary(ivar,ipar),ivar=1,nvary),rminX,rmaxX,rmeanX,rstdX 
           else
              write(*,'(3x,I4,8x,1P,4(2x,E10.3))') &
                IPAR,rminX,rmaxX,rmeanX,rstdX                
           endif
        enddo
    enddo   
    return
    end