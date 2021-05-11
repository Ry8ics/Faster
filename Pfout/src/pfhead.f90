    SUBROUTINE PFHEAD(IOUT,CANA,FASTER_VERSION)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Make a heading    
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni 
    IMPLICIT NONE
    CHARACTER*64 CIDENT
    CHARACTER*128 CMAESTRO
    CHARACTER*(*) CANA,FASTER_VERSION
    integer i,IOUT
    CHARACTER*4 CYRS
    CHARACTER*2 CMON,CDAY,CTIM,CMIN
    CHARACTER*1 CC
!
!   Date and time retreival..................................
    INTEGER DATE_TIME (8)
    CHARACTER (LEN = 12) REAL_CLOCK (3)
    CALL DATE_AND_TIME (REAL_CLOCK (1), REAL_CLOCK (2), &
                        REAL_CLOCK (3), DATE_TIME)
!
    CYRS=REAL_CLOCK (1)(1:4)
    CMON=REAL_CLOCK (1)(5:6)
    CDAY=REAL_CLOCK (1)(7:8)
    CTIM=REAL_CLOCK (2)(1:2)
    CMIN=REAL_CLOCK (2)(3:4)
!
    READ(IBIU,REC=2) CIDENT      ! Read identification
    READ(IBIU,REC=3) CMAESTRO     ! Read maestro
!
    CC='#'      ! comment
    write(IOUT,'(60a)') (CC,i=1,60)
    write(IOUT,'(a1,1x,a)') CC,TRIM(FASTER_VERSION)
    write(IOUT,'(a1,a)') CC,' Developed by Bjorn Melhus, Oslo, Norway'
    write(IOUT,'(a1,a,a2,a1,a2,a1,a4,a,a2,a1,a2)') CC, &
      ' Executed on ',CDAY,'.',CMON,'.',CYRS,' at ',CTIM,':',CMIN
!
    Write(IOUT,'(a1,a,a)') CC,' Maestro.....: ',trim(cmaestro)
    write(IOUT,'(a1,a,a)') CC,' Run Id......: ',CANA
    write(IOUT,'(a1,a,a)') CC,' Description.: ',TRIM(cident)
    write(IOUT,'(60a)') (CC,i=1,60)
!
    RETURN
    END 

