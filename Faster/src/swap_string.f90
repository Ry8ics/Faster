    SUBROUTINE SWAP_STRING(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Swap strings in the slave file
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    use clist
    implicit none
    integer istat
    INTEGER ISWAP_FAST,ipar
    CHARACTER*4 CEXT_SWAP,CNUM
    CHARACTER*136 FNAME,THIS,THAT,FNEW
!
!   Master files
    DO ISWAP_FAST=1,SLAVE%NSWAP
!       Extract the slave file e.g ".fst" 
        CEXT_SWAP=SLAVE%CSWAP(ISWAP_FAST)(len(trim(SLAVE%CSWAP(ISWAP_FAST)))-3:)
!
        FNAME=trim(SLAVE%CSWAP(ISWAP_FAST))   
!
!       Update the slave files with new references to the slave files
!
        THIS=TRIM(SLAVE%CSWAP_STR_FROM(ISWAP_FAST))
         DO IPAR=1,NPARA         
             WRITE(CNUM,'(I4)') IPAR            
             FNEW=SLAVE%CSWAP(ISWAP_FAST)(1:len(trim(SLAVE%CSWAP(ISWAP_FAST)))-4)// &
                  '_'//CANA//'_'//trim(adjustl(CNUM))//CEXT_SWAP       
             THAT=TRIM(SLAVE%CSWAP_STR_TO(IPAR,ISWAP_FAST))    
!             Hack the file...i.e. take the file FNAME and replace THIS with THAT
!             and call the new file FNEW.
!            Make a quick and dirty solution.... bet that FNEW already exists...
!            i.e. just replace strings in the file FNEW.
             CALL FILEHACK(trim(FNEW),trim(THIS),trim(THAT),trim(FNEW))    
         ENDDO    
    ENDDO
!
    RETURN
    END