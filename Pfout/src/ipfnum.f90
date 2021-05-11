    INTEGER FUNCTION ipfnum(S)
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Retreive user defined data
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
    use cfuni
    implicit none
!   Open binary files
    CHARACTER*1 S
    INTEGER NS,NDUM
!
   SELECT CASE (S)
      CASE ("P")
!       Number of user defined parameters (number of Openfast runs)
        READ(IBIU,REC=1) NS,NDUM
     CASE ("V")
!       Number of user defined variables  (WaveHs, ... whatever...)
        READ(IBIU,REC=1) NDUM,NS
     CASE ("R")
!       Number of rows (time instances)
        READ(IBIK,REC=2) NS,NDUM
     CASE ("C")
!       Number of columns (number of channels)
        READ(IBIK,REC=2) NDUM,NS
     CASE DEFAULT
       WRITE(*,*) '/IPFNUM/ PROGRAMMING ERRROR'  ! good you mentioned..
       STOP
    END SELECT
!   
!   Return the requested data...
    IPFNUM=NS
    RETURN
    END 

