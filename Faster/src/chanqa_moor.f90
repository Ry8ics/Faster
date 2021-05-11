    SUBROUTINE CHANQA_MOOR(C_FILE,        CANA, NPARA,IBIF,NUMCOMM,CHAN,   CUNIT,             NCHAN,NUMROW,MOOR_LINES)
!        CALL CHANQA_MOOR(FAST%CMAESTRO(1),CANA,NPARA,IBIF,NUMCOMM,RES%CHAN,RES%CUNI,          ,RES%NCOL,RES%NROW,MOOR_LINES)
!=======================================================================
!     LATEST CORRECTION BY
!
!    PURPOSE
!    Count number of channels, fetch the channel id's and units, count number of rows (time instances)
!    and do some quality checks on the output files from moordyn.
!
!  OUTPUT:
!      CHAN    - Channel id's 
!      CUNIT   - Units on each channel
!      NCHAN   - Number of channels (i.e. columns of output file)
!      NUMROW  - Number of time instances
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================

    use mod_utils  ! get access to FLINES
    IMPLICIT NONE
    CHARACTER*(*) C_FILE
    CHARACTER*(*) CANA
    CHARACTER*140 CRES_FILE,C_FILE1
    INTEGER NPARA,IBIF,NROWCHECK,NCHECK,ISTAT,IMOOR,NUMB_FILES
    CHARACTER*2 CMOOR_LINE
    CHARACTER*(*) CHAN(*),CUNIT(*)
    INTEGER NCHAN,NUMROW,IPAR,NUMCOMM,MOOR_LINES
    CHARACTER*4 CEXTT,CNUM
    CHARACTER*136 FPRE
!-----------------------------------------------------------------------------------   
    NUMB_FILES=0
!
    CEXTT='.out'       ! extension of output file from openfast
!   Loop over number of openfast analyses
    DO IPAR=1,NPARA
       WRITE(CNUM,'(I4)') IPAR     
!      Assume the name of the output file from moordyn is equal to the input file
!      with the extension .MD.Linei where i is the mooring line
       DO IMOOR=1,MOOR_LINES
          write (CMOOR_LINE,'(I2)') IMOOR      
          FPRE=C_FILE(1:len(trim(C_FILE))-4)// &  
                  '_'//CANA//'_'//trim(adjustl(CNUM))
          CRES_FILE=trim(FPRE)// '.MD.Line'//trim(adjustl(CMOOR_LINE))//CEXTT   ! Output file that by now should already exist
!
!         Open the moordyn output file ...     
          CALL OPEN_ASCII(IBIF,'OLD',CRES_FILE) 
!         Read the channels and number of channels (i.e. Time,px, etc.   
          CALL READCHAN(IBIF,CHAN,CUNIT,NCHAN,0,ISTAT)
          IF (ISTAT.NE.0) THEN
                 WRITE(*,*) ' FASTER ERROR.: File cannot be read.'
                 WRITE(*,*) ' FILE: ',TRIM(CRES_FILE)
                 STOP
          ENDIF          
!
!          Read number of rows (time instances) in result file
!          Assume NUMCOMM=0 initial comment lines followed by channel identifiers and untis
!          Number of time instances:
           NUMROW=FLINES(IBIF)-0-2 
!
!          Make some quality checks... the channels and number of rows should be identical for each
!          openfast run. Also the id's should be identical, but it's considered to be an overkill to check this.
!
           NUMB_FILES=NUMB_FILES+1
           IF (NUMB_FILES.EQ.1) THEN
               C_FILE1=CRES_FILE
               NROWCHECK=NUMROW
               NCHECK=NCHAN
           ELSE
              IF (NROWCHECK.NE.NUMROW) THEN
                  WRITE(*,*) ' FASTER ERROR.: The number of rows (time instances) deviates from initial run.'
                  WRITE(*,*) ' INITIAL FILE.: ',TRIM(C_FILE1)  ,' , Rows =',NROWCHECK          
                  WRITE(*,*) ' CURRENT FILE.: ',TRIM(CRES_FILE),' , Rows =',NUMROW  
                  STOP
              ELSEIF (NCHECK.NE.NCHAN) THEN
                  WRITE(*,*) ' FASTER ERROR.: The number of columns (channels) deviates from initial run.'
                  WRITE(*,*) ' INITIAL FILE.: ',TRIM(C_FILE1)  ,' , Columns =',NCHECK          
                  WRITE(*,*) ' CURRENT FILE.: ',TRIM(CRES_FILE),' , Columns =',NCHAN  
                  STOP
              ENDIF
           ENDIF
!
!          Just close the file... this will also rewind it.
           CLOSE(IBIF)
       ENDDO
!
    ENDDO
    RETURN
    END