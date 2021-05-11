    SUBROUTINE STORE_USER(IRECL)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!           Store user defined data on binary file
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    use clist
    use cfuni
    IMPLICIT NONE
    INTEGER islave,ivar,nvar,ipar,irecl
    CHARACTER*13 CFILE
    CHARACTER*14, allocatable :: ctabvar(:)   ! Table of strings to be updated in the slave files ('WaveTp', 'WaveTP', etc...)
    Logical, allocatable :: ltabvar(:)   ! Table of strings constant?
    CHARACTER*14, allocatable :: cmatpar(:,:) ! Contains set of values to be updated in the slave file, one slave file for each set 
    CHARACTER*4 CEXT_SLAVE,CNUM
    CHARACTER*136 CNEW_SLAVE
!
    IF (.not.LANAL) return                   ! Things will be stored if an analyses is performed...
!    
!.......................................................................................
!   Should've made a count on the number of variables, i.e. 'WaveTp', 'WaveTP', etc...
!   Never mind, just count them the stupid way..:
    nvar=0
    DO ISLAVE=1,SLAVE%NSLAVE          ! lOOP over slaves...
       DO IVAR=1,SLAVE%NVAR(ISLAVE)   ! lOOP over number of variables for given slave
            nvar=nvar+1 
       ENDDO
    ENDDO
!.......................................................................................
!
    allocate (ctabvar(nvar))   
    allocate (ltabvar(nvar))   
    allocate (CMATPAR(npara,nvar))  
!    
!   Write the user input to some temporary tables 
!   and make a dump to binary fles for later access
    nvar=0
    DO ISLAVE=1,SLAVE%NSLAVE    ! lOOP over slaves...
       DO IVAR=1,SLAVE%NVAR(ISLAVE)    
           nvar=nvar+1 
           ctabvar(nvar)=SLAVE%CVAR(IVAR,ISLAVE)
           ltabvar(nvar)=SLAVE%CONST(IVAR,ISLAVE)         
           DO IPAR=1,NPARA
               cmatpar(ipar,nvar)=SLAVE%CPAR(IPAR,IVAR,ISLAVE)
           ENDDO
       ENDDO
    ENDDO
!--------------------------------------------------------------------       
!
!    Store user data on binary file
     cfile=cana//'_USER.BIN'
!    If file exists it will be overwritten and the needed record length 
!    (each parameter is 14 characters long) will be:
!    Note: the record length is for intel fortran given in 4 byte words... 
!          also..: the first record will contain the record length of the 3 binary files..
!                  so at least 4*3=12 bytes.
!          Correction: The ID of the run is stored, which contains 64 characters...
!          New correction: The maestro file is 128 characters.
     irecl=nvar*4
     if (irecl.lt.128) irecl=128
!            
     CALL OPEN_BIN(IBIU,'UNKNOWN',Cfile,IRECL)
!
!     Store number of parameters (NPARA) i.e. number of openfast analyses and
!     number of user defined variables (NVAR), e.g. WaveHs, WaveTp etc.
!     on the first record.
!
     WRITE(IBIU,REC=1) NPARA,NVAR
!
!    Store the user ID for the run if analyses in turned on..
      WRITE(IBIU,REC=2) CIDENT
!
      WRITE(IBIU,REC=3) FAST%CMAESTRO(1)     ! Openfast maestro
!
!    Note: Leave a couple of spare records here, just in case. Max length of irecl as defined above.
!     
!      WRITE(IBIU,REC=4)  < whatever >
!      WRITE(IBIU,REC=5)  < whatever >
!      WRITE(IBIU,REC=6)  < whatever >  ! insert whatever is needed and make a retreival routine pf<something>.f90
     
     WRITE(IBIU,REC=7) (ctabvar(ivar),ivar=1,nvar)      ! The variables defined by user e.g. WaveHs, WaveTp etc.:
     WRITE(IBIU,REC=8) (ltabvar(ivar),ivar=1,nvar)      ! if .true. then constant for all openfast analyses
!     
!   And then the user defined parameters for each openfast analysis:
    DO IPAR=1,NPARA  
        WRITE(IBIU,REC=8+IPAR) (cmatpar(ipar,ivar),ivar=1,nvar)
    ENDDO    
!
    DEALLOCATE (CTABVAR)
    DEALLOCATE (ltabvar)
    DEALLOCATE (CMATPAR)
!   
    RETURN
    END