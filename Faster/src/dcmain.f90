      SUBROUTINE DCMAIN(ISTAT) 
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!              MAIN DECODING ROUTINE
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      use cfast
      use clist
      IMPLICIT NONE
      INCLUDE 'redwri'
!
      INTEGER IFERR,ISTAT,INCLUD,IEND
      LOGICAL MATCH
      CHARACTER*80 CBLANK,Text
      CHARACTER*40 CITEM,IITEM,CERR
      CHARACTER*7 CSTAT
      INTEGER ilen,ILEN1,ilen2,NumSlave,ISLAVE
      LOGICAL LEXIST
!
      DATA INCLUD/0/
      DATA CITEM/'(/''  INPUT ITEM...: '',A ,'' ?'')'/
      DATA IITEM/'(/''  INPUT ITEM...: '',I2,'' ?'')'/
      DATA CERR/'(/''   FASTER ERROR.: '',A/:9(18X,A/:))'/
!
      CBLANK(1:80) = ' '
!
!     SET THE INPUT AND OUTPUT UNITS
!
      CALL INITDC(IINN,IOUT)          
      rewind(iinn)                      ! make sure to start reading from the top
!
!     START DECODING THE INPUT FILE
!
 100  CALL READL
!
!     CHECK IF ANYTHING IS LEFT
!
      IF ( (IFERR().GE.0) .OR. (INCLUD.NE.0) ) THEN
         IF ( (IFERR().LT.0) .AND. (INCLUD.EQ.1) ) THEN
            CALL PUTCOM(1,0)
            INCLUD = 0
            CALL INITDC(IINN,IOUT)
            GOTO 100
         ENDIF
!
!*********************************************************************
!
        IF ( MATCH('IDRUN',2) ) THEN
!         UNIQUE NAME FOR THE ANALYSES..
          CALL DCSTRG(CANA,ILEN1,3) 
          IF ( IFERR().LT.0 ) THEN
              CALL PRIERR()
              WRITE (IOUT,CERR) 'ERROR READING ID' , &
                                'PROGRAM STOPPED'
              ISTAT = -1
              GOTO 200
          ENDIF
!-----------------
           IF ( MATCH('EXPAND',3) ) THEN
!            Speify expancion, i.e. number of openfast runs.
             CALL DCINT(NPARA)
              IF ( IFERR().LT.0 ) THEN
                 CALL PRIERR()
                 WRITE (IOUT,CERR) 'ERROR READING NUMBER OF EXPANSION' , &
                                'PROGRAM STOPPED'
                 ISTAT = -1
                 GOTO 200
              ENDIF
              IF (NPARA.GT.MAX_PAR) THEN
!                 CALL PRIERR()
!                 WRITE (IOUT,CERR) 'Max number of expansions exceeded.' , &
!                                 'Use the command MALLOC MASTERS <max_num>'
!                 WRITE(IOUT,*) ' The default number of maximum allowed masters are ',MAX_PAR
                 ISTAT = -2
                 MAX_PAR=MAX_PAR*2     ! Expand by a factor of 2, reallocate and start all over
                 GOTO 200                
              ENDIF
           ENDIF
!-------------------------------------------------------
!     READ THE IDENTIFICATION OF THE RUN
!
           CALL MOVEIP(IEND)
           IF (IEND.EQ.0) THEN
                  CALL DCSTRG(Cident,ilen,2)
                  IF ( ilen.GT.64 ) THEN
                     CALL PRIERR()
                     WRITE (Iout,Citem) Cident
                     WRITE (Iout,Cerr) 'TOO MANY CHARACTERS IN INPUT ITEM' , &
                                       'MAX NO = 64' , 'PROGRAM STOPPED'
                     Istat = -1
                     GOTO 100
                  ENDIF
!
                  IF ( IFERR().LT.0 ) THEN
                     CALL PRIERR()
                     WRITE (Iout,Citem) Cident
                     WRITE (Iout,Cerr) 'ERROR IN READING IDENTIFICATION' , &
                                       'OF RUN ' , 'PROGRAM STOPPED'
                     Istat = -1
                  ENDIF
          ENDIF
!-------------------------------------------------------
         ELSEIF ( MATCH('Analysis',3) ) THEN
             IF ( MATCH('ON',2)) THEN
                 LANAL=.TRUE.
!-----------------------------------------------
                 IF ( MATCH('SIMULATION',3)) THEN
                     IF ( MATCH('OFF',2)) THEN
                         LSIMUL=.FALSE. 
                     ELSEIF ( MATCH('ON',2)) THEN
                         LSIMUL=.TRUE. 

                     ELSE
                         CALL PRIERR()
                         WRITE (IOUT,CERR) 'ON OR OFF EXPECTED' , &
                                          'PROGRAM STOPPED'
                         ISTAT = -1
                         GOTO 200
                     ENDIF
                 ENDIF
!-----------------------------------------------                       
             ELSEIF ( MATCH('OFF',2)) THEN
                 LANAL=.FALSE.
             ELSE 
                 CALL PRIERR()
                 WRITE (IOUT,CERR) 'ON OR OFF EXPECTED' , &
                                  'PROGRAM STOPPED'
                 ISTAT = -1
                 GOTO 200
             ENDIF    
!-------------------------------------------------------
         ELSEIF ( MATCH('Maestro',3) ) THEN
             CALL DCMAESTRO(Cerr,Citem,Istat) 
             if (istat.eq.-1) goto 200
!-------------------------------------------------------             
         ELSEIF ( MATCH('Master',3) ) THEN
            FAST%NMASTER=FAST%NMASTER+1   
            IF (FAST%NMASTER.GT.MAX_MASTER) THEN
!                CALL PRIERR()
!                WRITE (IOUT,CERR) 'Max number of masters exceeded.' , &
!                                'Use the command MALLOC MASTERS <max_num>'
!                WRITE(IOUT,*) ' The default number of maximum allowed masters are ',MAX_MASTER
                ISTAT = -2
                MAX_MASTER=MAX_MASTER*2     ! Expand by a factor of 2, reallocate and start all over
                GOTO 200                
            ENDIF
            CALL DCSTRG(FAST%CMASTER(FAST%NMASTER),ilen1,3)
            
             INQUIRE(FILE=FAST%CMASTER(FAST%NMASTER),EXIST=LEXIST)         
             IF (.NOT. LEXIST) THEN
                   CALL PRIERR()
                   WRITE(*,*)' Master file...: ',TRIM(FAST%CMASTER(FAST%NMASTER))
                   WRITE (IOUT,CERR) 'Requested master file does not exist.'   
                   ISTAT=-1
                   GOTO 200    
             ENDIF   
               IF (FAST%NMASTER.GT.MAX_MASTER) THEN
!                   CALL PRIERR()
!                   WRITE (IOUT,CERR) 'Max number of masters exceeded.' , &
!                                   'Use the command MALLOC MASTER <max_num>'
!                   WRITE(IOUT,*) ' The default number of maximum allowed masters are ',MAX_MASTER
                   MAX_MASTER=MAX_MASTER*2    ! Expand by a factor of 2, reallocate and start all over
                   ISTAT = -2
                   GOTO 200                
               ENDIF
!
            IF ( MATCH('Slave',3) ) THEN
               NumSlave=0 
300            NumSlave=NumSlave+1
               FAST%NSLAVE=NumSlave
               FAST%MSLAVES(FAST%NMASTER)=NumSlave          
               IF (NumSlave.GT.MAX_SLAVE) THEN
!                   CALL PRIERR()
!                   WRITE (IOUT,CERR) 'Max number of slaves exceeded.' , &
!                                   'Use the command MALLOC SLAVES <max_num>'
!                   WRITE(IOUT,*) ' The default number of maximum allowed slaves are ',MAX_SLAVE
                   MAX_SLAVE=MAX_SLAVE*2    ! Expand by a factor of 2, reallocate and start all over
                   ISTAT = -2
                   GOTO 200                
               ENDIF
!
                IF ( IFERR().LT.0 ) THEN
                  CALL PRIERR()
                  WRITE (IOUT,CERR) 'Error reading number of slaves' , &
                                    'PROGRAM STOPPED'
                  ISTAT = -1
                  GOTO 200
                endif          
!              Open slave file..
                  CALL DCSTRG(FAST%CSLAVE(NumSlave,FAST%NMASTER),ilen2,3)
                  IF ( IFERR().LT.0 ) THEN
                      CALL PRIERR()
                      WRITE (IOUT,CERR) 'Error reading slave file' , &
                                    'PROGRAM STOPPED'
                      ISTAT = -1
                      GOTO 200
                   endif                            
                 INQUIRE(FILE=FAST%CSLAVE(NumSlave,FAST%NMASTER),EXIST=LEXIST)         
                 IF (.NOT. LEXIST) THEN
                       CALL PRIERR()
                       WRITE(*,*)' Slave file...: ',TRIM(FAST%CSLAVE(NumSlave,FAST%NMASTER))
                       WRITE (IOUT,CERR) 'Requested slave file does not exist.'   
                       ISTAT=-1
                       GOTO 200    
                 ENDIF            
!check                ENDDO
                 
                 CALL MOVEIP(iend)
!                Check if more slave files shall be read      
                 IF ( iend.EQ.0 ) GOTO 300  
            ENDIF    
!-------------------------------------------------------
         ELSEIF ( MATCH('Slave',3) ) THEN
!             IF ( MATCH('Openfast',3) ) THEN        
              SLAVE%NSLAVE=SLAVE%NSLAVE+1   
!
!   needed ??????? openfast/turbsim etc ....
              IF ( MATCH('Openfast',3) ) THEN
!                Nothing to be done. Just to keep backward compatibility.
              ELSEIF ( MATCH('Turbsim',3) ) THEN
!                Nothing to be done. Just to keep backward compatibility.
              ENDIF
!
!                Read openfast slave file to be updated                    
                 IF (SLAVE%NSLAVE.GT.MAX_SLAVE) THEN
!                       CALL PRIERR()
!                       WRITE (IOUT,CERR) 'Max number of slaves exceeded.' , &
!                                       'Use the command MALLOC SLAVES <max_num>'
!                       WRITE(IOUT,*) ' The default number of maximum allowed slaves are ',MAX_SLAVE
!             
                       MAX_SLAVE=MAX_SLAVE*2   ! Just make a silent retreat and reallocate
                       ISTAT = -2
                       GOTO 200                
                 ENDIF
                 CALL DCSTRG(SLAVE%CSLAVE(SLAVE%NSLAVE),ilen2,3)   
!                Check if slave file exists...
                 INQUIRE(FILE=SLAVE%CSLAVE(SLAVE%NSLAVE),EXIST=LEXIST)
                 
                 IF (.NOT. LEXIST) THEN
                       CALL PRIERR()
                       WRITE(*,*)' Slave file...: ',TRIM(SLAVE%CSLAVE(SLAVE%NSLAVE))
                       WRITE (IOUT,CERR) 'Requested slave file does not exist.'   
                       ISTAT=-1
                       GOTO 200    
                 ENDIF  
                 
                 IF ( MATCH('SWAP',3) ) THEN
                    SLAVE%NSWAP=SLAVE%NSWAP+1
                    IF (SLAVE%NSWAP.GT.MAX_SLAVE) THEN 
                       MAX_SLAVE=MAX_SLAVE*2    ! Expand by a factor of 2, reallocate and start all over
                       ISTAT = -2
                       GOTO 200                
                    ENDIF
!               
                    SLAVE%CSWAP(SLAVE%NSWAP)=SLAVE%CSLAVE(SLAVE%NSLAVE)     ! file to be swapped from
                    SLAVE%NSLAVE=SLAVE%NSLAVE-1                             ! regret incrementation             
                    
                    CALL DCSWAP_FAST(Cerr,Citem,Istat)          
                    IF ( ISTAT.EQ.-1 ) GOTO 200
                 ELSE
!                   Read parameters to be updated...
                    CALL DCPARA_FAST(Cerr,Citem,SLAVE%CSLAVE(SLAVE%NSLAVE),Istat)                    
                    IF ( ISTAT.LT.0) GOTO 200
                 ENDIF           
!--------------------------------------------
!        List user defined data. The results from openfast will be read from binary files.
         ELSEIF ( MATCH('LIST',3) ) THEN
!            If list command is given here, just ignore it ... 
!            It will be read in the routine dclist in the pfout library
!             CALL DCLIST(cerr,citem,Istat)
!-------------------------------------------------------
!        Are mooring lines defined? if so mooring line results should exist.
         ELSEIF (MATCH('MOORING',3)) THEN
              CALL DCINT(MOOR_LINES)
              IF ( IFERR().LT.0 ) THEN
                CALL PRIERR()
                WRITE (IOUT,CERR) 'Error reading number of mooring lines' , &
                                  'PROGRAM STOPPED'
                ISTAT = -1
                GOTO 200
              ENDIF 
              IF  (MATCH('SEGMENTS',3)) THEN
                 CALL DCINT(NSEGMENTS)
                 IF ( IFERR().LT.0 ) THEN
                   CALL PRIERR()
                   WRITE (IOUT,CERR) 'Error reading number of segments' , &
                                     'PROGRAM STOPPED'
                   ISTAT = -1
                   GOTO 200
                 ENDIF 
                  
              ELSE
                CALL PRIERR()
                WRITE (IOUT,CERR) 'SEGMENTS EXPECTED' , &
                                  'PROGRAM STOPPED'
                ISTAT = -1
                GOTO 200                                         
              ENDIF     
!............................................
         ELSEIF ( MATCH('CLEAN',3) ) THEN
             CLEAN_UP_MESS=.TRUE.       ! Clean up mess, delete all slave files etc..
!............................................            
!
!     CHECK FOR LEGAL COMMENT SIGNS
!
         ELSEIF ( MATCH('%',1) ) THEN
         ELSEIF ( MATCH('$',1) ) THEN
         ELSEIF ( MATCH('#',1) ) THEN
         ELSEIF ( MATCH('!',1) ) THEN
         ELSEIF ( MATCH('*',1) ) THEN
!
!     BLANK LINE?
!
         ELSEIF ( .NOT.(MATCH(CBLANK,80)) ) THEN
            CALL PRIERR()
            WRITE (IOUT,CERR) 'UNKNOWN INPUT'
            ISTAT = -1
            GOTO 200
         ENDIF
         GOTO 100
      ENDIF
!
 200  RETURN
      END
