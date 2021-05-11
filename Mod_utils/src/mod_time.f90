      MODULE MOD_TIME
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!           Evaluate cpu and elapsed time between two consecutive calls.
!
!     System routines:
!     CPU_TIME.....: Will return sum of thread time
!     SYSTEM_CLOCK.: Elapsed time
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      IMPLICIT NONE
      CONTAINS
!------------------------------------------   
!	
      REAL*8 FUNCTION CPUTIME()
      REAL*8 TCP
!     Apply system routine from VS to get cputime        
      CALL CPU_TIME(TCP)
      CPUTIME=TCP
      RETURN
      END      
!
!------------------------------------------            
!      
      REAL*8 FUNCTION REALTIME()
      INTEGER CSYS,count_rate, count_max
!     Apply system routine from VS        
      CALL SYSTEM_CLOCK(CSYS, count_rate, count_max)
!     Evaluate system time   
      REALTIME=DBLE(CSYS)/DBLE(count_rate) 
      RETURN
      END
!------------------------------------------         
	END MODULE MOD_TIME