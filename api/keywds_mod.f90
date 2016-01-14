module keywds_mod
    contains
    subroutine keywds()
      use keycom_mod
      implicit none
!     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
!
!     DATA STATEMENT FOR TABLE OF KEYWORDS
!
      INTEGER I

      TABLE (1:20) = (/ &
           'PROCESS ','TIMEINT ','FIXCW   ','TREEDATA','TREEFMT ','MPB     ', &
           'DFTM    ','WSBW    ','CWEQN   ','DESIGN  ','NUMCYCLE','TFIXAREA', &
           'GROWTH  ','STDINFO ','STDIDENT','INVYEAR ','TREELIST', &
           'REWIND  ','NOSUM   ','DEBUG   ' /)
      TABLE (21:40) = (/ &
           'ECHOSUM ','ADDFILE ','THINAUTO','THINBTA ','THINATA ', &
           'THINBBA ','THINABA ','THINPRSC','THINDBH ','SALVAGE ', &
           'SPLABEL ','AGPLABEL','COMPUTE ','FERTILIZ','THINHT  ', &
           'STATS   ','TOPKILL ','HTGSTOP ','MCFDLN  ','BFFDLN  ' /)
      TABLE (41:60) = (/ &
           'MCDEFECT','BFDEFECT','VOLUME  ','BFVOLUME','REGDMULT', &
           'COVER   ','ESTAB   ','MINHARV ','SPECPREF','SPCODES ','NODEBUG ', &
           'CUTEFF  ','NOTRIPLE','READCORD','REUSCORD','NOCALIB ', &
           'DGSTDEV ','BAIMULT ','MORTMULT','NOHTDREG' /)
      TABLE (61:80) = (/ &
           'RANNSEED','HTGMULT ','CHEAPO  ','NUMTRIP ','ENDFILE ','BAMAX   ', &
           'READCORH','REUSCORH','MGMTID  ','REGHMULT','TCONDMLT', &
           'NOAUTOES','READCORR','REUSCORR','BRUST   ','IF      ','SCREEN  ', &
           'COMPRESS','THEN    ','ALSOTRY '/)
      TABLE (81:100) = (/ &
           'ENDIF   ','NOTREES ','CALBSTAT','OPEN    ','CLOSE   ','NOSCREEN', &
           'RRIN    ','FIXMORT ','SDIMAX  ','DELOTAB ','SERLCORR','CUTLIST ', &
           'RESETAGE','SITECODE','MISTOE  ','CRNMULT ','CFVOLEQU', &
           'BFVOLEQU','ANIN    ','DFB     '/)
      TABLE (101:120) = (/ &
           'RDIN    ','MANAGED ','YARDLOSS','FMIN    ','STRCLASS','MODTYPE ', &
           'FVSSTAND','PRUNE   ','SVS     ','FIXDG   ','FIXHTG  ','THINSDI ', &
           'LOCATE  ','BGCIN   ','THINCC  ','ECON    ','DATABASE', &
           'SYSTEM  ','DEFECT  ','CRUZFILE'/)
      TABLE (121:135) = (/ &
           'STANDCN ','THINMIST','TREESZCP','THINRDEN','SPGROUP ', &
           'BMIN    ','DATASCRN','SETPTHIN','THINPT  ','VOLEQNUM', &
           'POINTREF','ECHO    ','NOECHO  ','CYCLEAT ','ATRTLIST'/)
      TABLE (136:150) = (/ &
           'THINRDSL','MORTMSB ','SETSITE ','CLIMATE ','SDICALC ', &
           'THINQFA ','PTGROUP ','ORGANON ','        ','        ', &
           '        ','        ','        ','        ','        '/)

      end subroutine keywds
end module keywds_mod
