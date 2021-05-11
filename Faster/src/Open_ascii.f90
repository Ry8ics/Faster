    Subroutine open_ascii(IBI,CSTAT,Cfile)
    implicit none
!   Open ascii-file (text file)
    INTEGER IBI
    CHARACTER*(*) Cfile,CSTAT
!
!   Open requested file
!    
     OPEN (UNIT=IBI,FILE=Cfile,STATUS=CSTAT,ERR=1900)
!   
     RETURN
!*********************************************************   
 1900 WRITE (*,*) 'FILE..:',trim(cfile)
      WRITE (*,*) 'Error opening requested file.',' Program stopped.'    
    STOP
    END 

