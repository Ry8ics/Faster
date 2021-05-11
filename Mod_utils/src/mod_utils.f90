    MODULE MOD_UTILS   
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Some utility routines
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    IMPLICIT NONE 
    PRIVATE 
    PUBLIC :: to_upper     ! convert to upper case
    PUBLIC :: SUBPROK      ! Determine batch of processes (for parallelization)
    PUBLIC :: GETNLINES    ! Count number of lines of file with known name
    PUBLIC :: FLINES       ! Count number of lines of file with known unit number
    CONTAINS 
!...................................................................
    function to_upper(strIn) result(strOut)
!
!   Original author: Clive Page
!   Utility code for conversion to upper case 
!
     implicit none

     character(len=*), intent(in) :: strIn
     character(len=len(strIn)) :: strOut
     integer :: i,j

     do i = 1, len(strIn)
          j = iachar(strIn(i:i))
          if (j>= iachar("a") .and. j<=iachar("z") ) then
               strOut(i:i) = achar(iachar(strIn(i:i))-32)
          else
               strOut(i:i) = strIn(i:i)
          end if
     end do

    end function to_upper
!...................................................................
    SUBROUTINE SUBPROK(NPROC,NCORES,IR,NR,ISTA,IEND)
!   Determine batch of processes, ista to iend.
    INTEGER NPROC,NCORES,IR,NR,ISTA,IEND  
        IF (IR.LT.NR) THEN
            ISTA=NCORES*(IR-1)+1
            IEND=NCORES*IR
        ELSE
            ISTA=NCORES*(IR-1)+1
            IEND=NPROC
        ENDIF    
    RETURN
    END   
!...................................................................
    INTEGER FUNCTION GETNLINES(CFILE)
    IMPLICIT NONE
!   Count number of lines of CFILE
!   CFILE => File name
    CHARACTER*(*) CFILE
    INTEGER NLINES,IBITMP
    NLINES=0
    OPEN (NEWUNIT=IBITMP, FILE = CFILE, STATUS='UNKNOWN') 
100 READ (IBITMP,*, END=200) 
       NLINES = NLINES + 1 
    GOTO 100
200 CLOSE (IBITMP)
    GETNLINES=NLINES
    RETURN
    END
!...................................................................
    INTEGER FUNCTION FLINES(IBIF)
    IMPLICIT NONE
!   Count number of lines of the ascii file
!   IBIF => unit number of file
    INTEGER IBIF,NLINES
!
    REWIND(IBIF)
    NLINES=0
100 READ(IBIF,*,END=200)
       NLINES=NLINES+1
    GOTO 100
200 FLINES=NLINES
    REWIND(IBIF)
    RETURN
    END 
    
    END MODULE MOD_UTILS  
    


