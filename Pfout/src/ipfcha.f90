    INTEGER FUNCTION ipfcha(CHA_REP)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Retreive the channel number for given channel id.
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni
    implicit none
    CHARACTER*(*) CHA_REP
    CHARACTER*14, allocatable :: cid(:)
    INTEGER NCOL,ICOL,ICHAN
    INTEGER IPFNUM
    EXTERNAL IPFNUM
!
!   Retreive the channel id's
    NCOL= IPFNUM('C')
    allocate (cid(ncol)) 
    CALL PFCID(CID,NCOL)
!
!   Search through the channels for a hit...    
    ichan=0
    do icol=1,NCOL  
        IF (CHA_REP(:).eq.CID(icol)(:)) THEN
!           hit... 
            ichan=icol 
            exit
        ENDIF    
    enddo  
!   
!   Return the requested data...
    ipfcha=ichan
    RETURN
    END 

