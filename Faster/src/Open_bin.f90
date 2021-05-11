    Subroutine open_bin(IBI,CSTAT,Cfile,IRECL)
    implicit none
!   Open binary file
    INTEGER IBI,IRECL
    CHARACTER*(*) Cfile,CSTAT
!
!   Open requested file
!    
    OPEN (IBI,FORM='UNFORMATTED',FILE=Cfile,RECL=IRECL, &
         ACCESS='DIRECT',STATUS=CSTAT,ERR=902)
!   
     RETURN
!*********************************************************   
902 WRITE (*,*) Cfile(1:INDEX(Cfile,' '))
    WRITE (*,*) 'Error opening requested file',' Program stopped'    
    STOP
    END 

