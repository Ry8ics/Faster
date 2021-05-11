**==swap.spg  processed by SPAG 5.00N  at 11:12 on 23 Mar 1999
      SUBROUTINE SWAP(X,Y)
      IMPLICIT NONE
C
C ======================================================================
C
C  PURPOSE:
C       Swap value of variables X and Y
C
C  METHOD:
C
C  INPUT:
C       X, Y - Variables to be swapped
C
C  OUTPUT:
C       X (=Y as input)
C       Y (=X as input)
C
C  INTERNAL VARIABLES:
C       TMP - Temporary
C
C  CALLS FROM:
C       General purpose routine.
C
C  CALLS TO:
C       None.
C
C  LIBRARY
C       FFDLIB
C
C  PROGRAMMED BY:
C       Per Ivar Loekstad / ETOE / Aker Engineering a.s.
C
C  DATE/VERSION:
C       19.02.92 / 1.0
C
C ======================================================================
C
      REAL X , Y , tmp
C
      tmp = X
      X = Y
      Y = tmp
      RETURN
      END

