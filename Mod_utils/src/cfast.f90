      MODULE CFAST
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Module defining the the Faster structures.
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      CHARACTER*64      :: CIDENT='<description>' ! User defined ID of the analyses
      INTEGER           :: NPARA=0            ! Number of parameters, i.e. number of openfast runs
      LOGICAL           :: LANAL=.FALSE.      ! Logical for performing the Openfast analyses
      LOGICAL           :: LSIMUL=.TRUE.      ! Logical for running the analyses, default=.true.
                                              !  (LSIMUL=.FALSE. and LANAL=.TRUE. will generate the files, but omit the simulation)     
      INTEGER           :: MOOR_LINES=0       ! Number of mooring lines
      INTEGER           :: NSEGMENTS=0        ! Number of segments on each mooring line
      INTEGER           :: MAX_MASTER=3       ! Number of masters (will be increased if needed)
      INTEGER           :: MAX_SLAVE=4        ! Number of slaves (will be increased if needed)
      INTEGER           :: MAX_PAR=30         ! Number of assigned parameters = number of openfast analyses (will be increased if needed)
      INTEGER           :: MAX_VAR=4          ! Number of variables variables; WaveTp, WaveTP, etc. 
      INTEGER           :: NLINES_OF_OUTUT_FILE
      INTEGER,PARAMETER :: MAX_CHARACTERS=2500     ! Max characters in one line of the output file  
      INTEGER,PARAMETER :: NUMCOMM=6               ! Initial number of comment lines of the output file
      LOGICAL           :: CLEAN_UP_MESS=.FALSE.   ! Logical for clean up, i.e. delete all slave files etc.
!
      TYPE TMASTER
        INTEGER                    :: NMASTER= 0            ! Number of master files
        INTEGER                    :: NSLAVE = 0            ! Number of slaves for given master
        INTEGER , allocatable      :: MSLAVES(:)            ! Array of slave numbers
        CHARACTER*128, allocatable :: CMASTER(:)            ! Array of masters MAX_MASTER
        CHARACTER*128              :: CMAESTRO(2)           ! Meastros for Openfast and Turbsim
        CHARACTER*128, allocatable :: CSLAVE(:,:)           ! Array of slaves for given master
      END TYPE TMASTER
!
      TYPE TSLAVE
        INTEGER                    :: NSLAVE = 0            ! Number of slaves
        INTEGER                    :: ITYPE=1               ! ITYPE=1 -> Openfast,  ITYPE=2 -> Turbsim
        INTEGER                    :: NSWAP=0               ! Number of files where strings are to be swapped
        INTEGER,allocatable        :: NVAR(:)               ! Number of variables for given slave
        CHARACTER*128,allocatable  :: CSLAVE(:)             ! Slave id; MAXSLAVE
        CHARACTER*128,allocatable  :: CSWAP_STR_FROM(:)     ! String to be swappeed  ; MAX_SLAVE
        CHARACTER*128,allocatable  :: CSWAP_STR_TO(:,:)     ! String to be swappeed to in npar files ; NPAR*MAXSLAVE    
        CHARACTER*128,allocatable  :: CSWAP(:)              ! Files to be swapped from  ; MAX_SLAVE
        CHARACTER*14, allocatable  :: CPAR(:,:,:)           ! parameters ; MAX_PAR*MAX_VAR*MAX_SLAVE
        CHARACTER*14, allocatable  :: CVAR(:,:)             ! variables ; MAX_VAR*MAX_SLAVE
        LOGICAL , allocatable      :: CONST(:,:)            ! Constant for all runs?  MAX_VAR*MAX_SLAVE
      END TYPE TSLAVE
!
      TYPE TRES
         CHARACTER*14, allocatable  :: CHAN(:)              ! Channel ID's
         CHARACTER*14, allocatable  :: CUNI(:)              ! Channel units
         INTEGER                    :: NROW                 ! Number of rows = Number of time instances
         INTEGER                    :: NCOL                 ! Number of columns = Number of channels
       END TYPE TRES
!
      TYPE (TMASTER) ::  FAST
      TYPE (TSLAVE)  ::  SLAVE
      TYPE (TRES)    ::  RES
!
     END MODULE