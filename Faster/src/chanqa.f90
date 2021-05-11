    SUBROUTINE CHANQA(C_FILE,CANA,NPARA,IBIF,NUMCOMM,CHAN,CUNIT,NCHAN,NUMROW)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!       Count number of channels, fetch the channel id's and units, 
!       count number of rows (time instances) and do some quality checks 
!       on the output files from openfast.
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
    CHARACTER*132 CRES_FILE
    INTEGER NPARA,IBIF,NROWCHECK,NCHECK,ISTAT
    CHARACTER*(*) CHAN(*),CUNIT(*)
    INTEGER  NCHAN,NUMROW,IPAR,NUMCOMM
    CHARACTER*4 CEXTT,CNUM
    CHARACTER*136 FPRE,FOUT,FOUTCHECK
!-----------------------------------------------------------------------------------   
!
    CEXTT='.out'       ! extension of output file from openfast
!   Loop over number of openfast analyses
    DO IPAR=1,NPARA
!      Assume the name of the output file from openfast is equal to the input file
!      with the extension .out.
       WRITE(CNUM,'(I4)') IPAR     
       FPRE=C_FILE(1:len(trim(C_FILE))-4)// &  
            '_'//CANA//'_'//trim(adjustl(CNUM))
       FOUT=trim(FPRE)//CEXTT          ! Output file that by now should already exist
!
       CALL OPEN_ASCII(IBIF,'OLD',FOUT) 
!
!      Read the channels and number of channels (i.e. Time,Wind1VelX,Wind1VelY etc.)
       CALL READCHAN(IBIF,CHAN,CUNIT,NCHAN,NUMCOMM,ISTAT)
       IF (ISTAT.LT.0) THEN
           WRITE(*,*) ' FASTER FATAL ERROR'
           WRITE(*,*) '  Not possible to read the channels from the file ',trim(FOUT)
           STOP
       ENDIF
!
!      Read number of rows (time instances) in result file
!      Assume NUMCOMM initial comment lines followed by channel identifiers and untis
!      Number of time instances:
       NUMROW=FLINES(IBIF)-NUMCOMM-2 
!
!      Make some quality checks... the channels and number of rows should be identical for each
!      openfast run. Also the id's should be identical, but it's considered to be an overkill to check this.
!
       IF (IPAR.EQ.1) THEN
           NROWCHECK=NUMROW
           NCHECK=NCHAN
           FOUTCHECK=FOUT
       ELSE
          IF (NROWCHECK.NE.NUMROW) THEN
              WRITE(*,*) ' FASTER ERROR.: The number of rows (time instances) deviates from initial run.'
              WRITE(*,*) ' INITIAL FILE.: ',TRIM(FOUTCHECK),' , Rows =',NROWCHECK          
              WRITE(*,*) ' CURRENT FILE.: ',TRIM(FOUT),     ' , Rows =',NUMROW  
              STOP
          ELSEIF (NCHECK.NE.NCHAN) THEN
              WRITE(*,*) ' FASTER ERROR.: The number of columns (channels) deviates from initial run.'
              WRITE(*,*) ' INITIAL FILE.: ',TRIM(FOUTCHECK),' , Columns =',NCHECK          
              WRITE(*,*) ' CURRENT FILE.: ',TRIM(FOUT)     ,' , Columns =',NCHAN  
              STOP
          ENDIF
       ENDIF
!
!      Just close the file... 
       CLOSE(IBIF)
!
    ENDDO
    RETURN
    END