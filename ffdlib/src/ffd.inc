C
C Named COMMON blocks, only for use in the free-format
C decoding system.
C
C IDCERR - Error flag
C INP    - Unit no. for READ
C LPU    - Unit no. for WRITE
C IP     - Line pointer. (Give current position in CHBUFF).
C NCOL   - Max. width for CHBUFF.
C NLIN   - Current line number after call to READL (or READUI).
C
C NOTE: If NCOL is changed, remember to change decl. (size)
C       of CHBUFF
C
      INTEGER IDCERR, INP, LPU, IP, NCOL, NLIN
      CHARACTER CHBUFF*200
C
      COMMON /FFD1/ IDCERR, INP, LPU, IP, NCOL, NLIN
      COMMON /FFD2/ CHBUFF
