      MODULE CFUNI
!=======================================================================
!     LATEST CORRECTION BY
!
!     PURPOSE
!            Module defining unit numbers used on the internal files
!
!     PROGRAMMED BY:  bjorn.melhus@akersolutions.com
!     CREATED......:  10.05.2021
!=======================================================================
      INTEGER IBIF,IBIB,IBIK,IBIU,ibi2,IBIM,IBIR,IBIO
      PARAMETER (IBI2=101)                 ! Ascii file holding original slave file ---> .dat
      PARAMETER (IBIM=102)                 ! Binary file holding moordyn channels and units ---> cana//'_KEYS.BIN' 
      PARAMETER (IBIF=103)                 ! Ascii file holding openfast/moordyn result file ---> .out
      PARAMETER (IBIB=104)                 ! Binary file holding openfast results --> cana//'_FAST.BIN'
      PARAMETER (IBIK=105)                 ! Binary file holding openfast channels and units ---> cana//'_KEYS.BIN' 
      PARAMETER (IBIU=106)                 ! Binary file holding user defined input ---> cana//'_USER.BIN'
      PARAMETER (IBIR=107)                 ! Binary file holding moordyn results ---> cana//'_MOOR.BIN'
      PARAMETER (IBIO=108)                 ! Ascii file for output results
      END MODULE
!