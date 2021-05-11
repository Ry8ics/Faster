    SUBROUTINE SLAVES(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!         Update the slave files with appopriate user defined data
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    use clist
    use cfuni
    use mod_utils
    implicit none
    integer istat
    INTEGER ISLAVE_FAST,IVAR,IPAR, NLINE,ILINE,IO
    CHARACTER*500 buffer
    CHARACTER*30 CS1,CS2,CS3,CTARGET
    CHARACTER*4 CEXT_SLAVE,CNUM
    CHARACTER*136 CNEW_SLAVE
!
    integer nline_ibi2
    CHARACTER*500, allocatable  :: CASSIGN(:)   
    CHARACTER*500, allocatable  :: CLINE(:)   
    LOGICAL, allocatable  :: LASSIGN(:)
!===========================================================================
!   Openfast slave files
!
    DO ISLAVE_FAST=1,SLAVE%NSLAVE     
       CALL OPEN_ASCII(IBI2,'OLD',SLAVE%CSLAVE(ISLAVE_FAST))      ! Open original slave file...
       nline_ibi2=flines(ibi2)                                    ! Number of lines
       allocate (CASSIGN(nline_ibi2))
       allocate(LASSIGN(nline_ibi2))
       allocate(cline(nline_ibi2))
       close(ibi2)
!
       DO IPAR=1,NPARA
!           Open slave file for current run..  
            WRITE(CNUM,'(I4)') IPAR      
            CEXT_SLAVE=SLAVE%CSLAVE(ISLAVE_FAST)  (len(trim(SLAVE%CSLAVE(ISLAVE_FAST)))-3:)   
            CNEW_SLAVE=SLAVE%CSLAVE(ISLAVE_FAST)(1:len(trim(SLAVE%CSLAVE(ISLAVE_FAST)))-4)//  &
                          '_'//CANA//'_'//trim(adjustl(CNUM))//CEXT_SLAVE    
!
            CALL OPEN_ASCII(IBI2,'OLD',CNEW_SLAVE) 
            NLINE=0
100         READ (IBI2,'(A500)',END=200) BUFFER              ! Read line from slave file into buffer
            NLINE=NLINE+1         
            LASSIGN(NLINE)=.FALSE. 
            READ(BUFFER,*,IOSTAT=IO) CS1,CS2,CS3             ! Read the first 3 items of buffer
            IF ( LEN(TRIM(CS1)).GT.0.AND.  &                 ! The initial parameter should not be blank                   
                 IO.NE.-1.AND.             &                 ! and it should be able to read the line
                 TRIM(CS3).EQ.'-'.OR.TRIM(CS3).EQ.'!') THEN  ! the 3'rd parameter should be a minus sign or !                           
                 CTARGET=CS2                                 ! then keep the parameter
            ELSE
                 CLINE(NLINE)=BUFFER                         ! keep the line as is and
                 GOTO 100                                    ! just read next line
            ENDIF
!
!----------------------------              
!       
            DO IVAR=1,SLAVE%NVAR(ISLAVE_FAST)
              IF (TRIM(CTARGET).EQ.TRIM(SLAVE%CVAR(IVAR,ISLAVE_FAST))) THEN
                     WRITE(CNUM,'(I4)') IPAR   
                     WRITE(CASSIGN(NLINE),'(A14,3X,A14,1X,A1,1X,A,1X,A4)') &
                         ADJUSTL(TRIM(SLAVE%CPAR(IPAR,IVAR,ISLAVE_FAST))), &
                         adjustr(TRIM(SLAVE%CVAR(IVAR,ISLAVE_FAST))),'-',CANA,trim(adjustl(CNUM))
                  LASSIGN(NLINE)=.TRUE.
               ELSE          
                  CLINE(NLINE)=BUFFER        ! JUST KEEP AS IS... 
               ENDIF              
            ENDDO
            GOTO 100
! new
200         REWIND(IBI2)
!
!           Just write the slave file all over
            DO ILINE=1,NLINE
                IF(.NOT.LASSIGN(ILINE)) THEN
                   write(ibi2,'(a)') trim(CLINE(ILINE))
                ELSE
                   write(ibi2,'(a)') trim(CASSIGN(ILINE))  ! Could have kept this into CLINE as well
                                                           ! meaning; I don't need LASSIGN, it's obsolete.
                                                           ! However, it works...
                ENDIF
            ENDDO
            CLOSE(ibi2)
       ENDDO
       deallocate(CASSIGN)
       deallocate(LASSIGN)
       deallocate(CLINE)
    ENDDO
     RETURN
     END
 
 