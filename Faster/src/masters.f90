    SUBROUTINE MASTERS(ISTAT)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!         Update the master files
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfast
    use clist
    implicit none
    integer istat,ESTAT
    INTEGER IMASTER,ISLAVE,ipara,N,J1
    CHARACTER*4 CEXT_MASTER,CEXT_SLAVE,CNUM
    CHARACTER*136 FNAME,THIS,THAT,FNEW
    CHARACTER*144 CMSG,CEXEC
!
!   Just in case no masters are given, then just use the openfast meastro
    IF (FAST%NMASTER.EQ.0) THEN
        FAST%NMASTER=1
        FAST%CMASTER(1)=FAST%CMAESTRO(1)
    ENDIF
!
!   Master files
    DO IMASTER=1,FAST%NMASTER
!       Extract the extension of the master file e.g ".fst"
        CEXT_MASTER=FAST%CMASTER(IMASTER)(len(trim(FAST%CMASTER(IMASTER)))-3:)    
!
!       Update the master files with new references to the slave files 
         DO IPARA=1,NPARA
             FNAME=trim(FAST%CMASTER(IMASTER))            
             WRITE(CNUM,'(I4)') IPARA            
             FNEW=FAST%CMASTER(IMASTER)(1:len(trim(FAST%CMASTER(IMASTER)))-4)// &
                  '_'//CANA//'_'//trim(adjustl(CNUM))//CEXT_MASTER      
!
!            Just in case no masters are given, let the slave be it's own master
             IF (FAST%MSLAVES(IMASTER).EQ.0) THEN
                 FAST%MSLAVES(IMASTER)=1
                 FAST%CSLAVE(1,IMASTER)=FAST%CMASTER(IMASTER)
             ENDIF
!
             DO ISLAVE=1,FAST%MSLAVES(IMASTER)
!
                 CEXT_SLAVE=FAST%CSLAVE(ISLAVE,IMASTER)(len(trim(FAST%CSLAVE(ISLAVE,IMASTER)))-3:)
                 THIS=TRIM(FAST%CSLAVE(ISLAVE,IMASTER))
                 THAT=FAST%CSLAVE(ISLAVE,IMASTER)(1:len(trim(FAST%CSLAVE(ISLAVE,IMASTER)))-4)// &
                      '_'//CANA//'_'//trim(adjustl(CNUM))//CEXT_SLAVE    
                 J1=1
                 do N=1,LEN(THIS)
                    if(THIS(N:N).eq."/".OR.THAT(N:N).eq."\") J1=N+1
                 enddo  
!
!                Hack the file...i.e. take the file FNAME and replace THIS with THAT
!                and call the new file FNEW.
                 CALL FILEHACK(trim(FNAME),TRIM(THIS(J1:LEN(THIS))),TRIM(THAT(J1:LEN(THAT))),trim(FNEW))
!......................................................................................
!                Make a prehack.... 
!                i.e. swap an occurence of "/" with "\" of the file name
!                to make the file windows compatible if it contains a full path.
                 do N=1,LEN(THIS)
                    if(THIS(N:N).eq."/") THIS(N:N)='\'
                 enddo
!
                 do N=1,LEN(THAT)
                    if(THAT(N:N).eq."/") THAT(N:N)='\'
                 enddo
!                Create the slave file ready to be updated in slaves.f90
!                I.e. just copy the existing slave file.
                 CEXEC='copy /Y '//trim(THIS)//' '//trim(THAT)
                 CALL EXECUTE_COMMAND_LINE (CEXEC, EXITSTAT=ESTAT, &
                                            CMDSTAT=ISTAT, CMDMSG=CMSG)
                 IF (ISTAT > 0) THEN
                      PRINT *, 'Command execution failed with error ', TRIM(CMSG)
                      PRINT *, 'A fatal error occured: Contact support'
                      PRINT *, 'Failed to produce the slave file ',trim(THAT)
                     STOP
                 ELSEIF (ISTAT < 0) THEN
                     PRINT *, 'Command execution not supported'
                     PRINT *, 'A fatal error occured: Contact support'
                     PRINT *, 'Failed to produce the slave file ',trim(THAT)
                     STOP
                 ELSEIF (ESTAT > 0) THEN
                     PRINT *, 'Faster error.'
                     PRINT *, 'The slave file cannot be created.'
                     PRINT *, 'Master file..: ',trim(THIS)
                     PRINT *, 'Slave file...: ',trim(THAT)
                     PRINT *, 'The probale cause is that the full file name '
                     PRINT *, 'of master file including correct directory is not given.'
                     STOP                     
                 ENDIF
!......................................................................................
                 FNAME=FNEW
             ENDDO                    
         ENDDO    
    ENDDO
!
    RETURN
    END