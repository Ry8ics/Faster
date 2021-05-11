    SUBROUTINE PFOPEN()
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!        Open binary files containing openfast results.
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use clist    
    use cfuni
    IMPLICIT NONE 
    INTEGER IRECB,IRECK,IRECU
    LOGICAL LEXIST
!
!   Open the requested binary files
!   The record length is needed to properly reopen the files 
!   Use some hacking tricks... the first record can be read.. which contains the record lenghts...
!   Only the first block, i.e. about 500 bytes or so can be read,
!   but's that sufficient to just read the record lengths... 
    OPEN (IBIK,FORM='UNFORMATTED',FILE=CANA//'_KEYS.BIN',RECL=12,ACCESS='DIRECT',STATUS='OLD',ERR=902)
    READ (IBIK,REC=1) IRECB,IRECK,IRECU
    CLOSE(IBIK)
!
!   We now have the record lengths. Reopen the files with correct record lengths.
    OPEN (IBIB,FORM='UNFORMATTED',FILE=CANA//'_FAST.BIN',RECL=IRECB,ACCESS='DIRECT',STATUS='OLD',ERR=901)
    OPEN (IBIK,FORM='UNFORMATTED',FILE=CANA//'_KEYS.BIN',RECL=IRECK,ACCESS='DIRECT',STATUS='OLD',ERR=902)
    OPEN (IBIU,FORM='UNFORMATTED',FILE=CANA//'_USER.BIN',RECL=IRECU,ACCESS='DIRECT',STATUS='OLD',ERR=903)
!
!   Do the same with the moordyn files ... if exists.... or requested... or whatever...
    INQUIRE(FILE=CANA//'_KEYM.BIN',EXIST=LEXIST)         
    IF (LEXIST) THEN
      OPEN (IBIM,FORM='UNFORMATTED',FILE=CANA//'_KEYM.BIN',RECL=12,ACCESS='DIRECT',STATUS='OLD',ERR=904)
      READ(IBIM,REC=1) IRECB,IRECK
      CLOSE(IBIM)
!
!     We now have the record lengths. Reopen the files with correct record lengths.
      OPEN (IBIR,FORM='UNFORMATTED',FILE=CANA//'_MOOR.BIN',RECL=IRECB,ACCESS='DIRECT',STATUS='OLD',ERR=905)
      OPEN (IBIM,FORM='UNFORMATTED',FILE=CANA//'_KEYM.BIN',RECL=IRECK,ACCESS='DIRECT',STATUS='OLD',ERR=904)
    ENDIF
!
    RETURN
!*********************************************************   
901 WRITE (*,*) 'FASTER error:'  
    WRITE (*,*)  CANA//'_FAST.BIN'
    WRITE (*,*) 'Error opening requested file.',' Program stopped' 
    STOP
902 WRITE (*,*) 'FASTER error:'   
    WRITE (*,*)  CANA//'_KEYS.BIN'
    WRITE (*,*) 'Error opening requested file.',' Program stopped' 
    STOP
903 WRITE (*,*) 'FASTER error:'   
    WRITE (*,*)  CANA//'_USER.BIN'
    WRITE (*,*) 'Error opening requested file.',' Program stopped' 
    STOP
904 WRITE (*,*) 'FASTER error:'   
    WRITE (*,*)  CANA//'_KEYM.BIN'
    WRITE (*,*) 'Error opening requested file.',' Program stopped' 
    STOP
905 WRITE (*,*) 'FASTER error:'   
    WRITE (*,*)  CANA//'_MOOR.BIN'
    WRITE (*,*) 'Error opening requested file.',' Program stopped' 
    STOP
    END 

