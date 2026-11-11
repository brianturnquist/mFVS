!==================================!
!   Initialize the keyword table   !
!==================================!
! BASE $Id$
block data KEYWDS
  implicit none

  !     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.

  !     DATA STATEMENT FOR TABLE OF KEYWORDS

  ! Common blocks
  include 'KEYCOM.F77'

  ! Variables
  integer :: I

  data (TABLE(I),I = 1,20) / &
       & 'PROCESS','TIMEINT','FIXCW','TREEDATA','TREEFMT','MPB', &
       & 'DFTM','WSBW','CWEQN','DESIGN','NUMCYCLE','TFIXAREA', &
       & 'GROWTH','STDINFO','STDIDENT','INVYEAR','TREELIST', &
       & 'REWIND','NOSUM','DEBUG'/
  data (TABLE(I),I = 21,40) / &
       & 'ECHOSUM','ADDFILE','THINAUTO','THINBTA','THINATA', &
       & 'THINBBA','THINABA','THINPRSC','THINDBH','xSALVAGE', &
       & 'SPLABEL','AGPLABEL','COMPUTE','FERTILIZ','THINHT', &
       & 'STATS','TOPKILL','HTGSTOP','MCFDLN','BFFDLN'/
  data (TABLE(I),I = 41,60) / &
       & 'MCDEFECT','BFDEFECT','VOLUME','BFVOLUME','REGDMULT', &
       & 'COVER','ESTAB','MINHARV','SPECPREF','SPCODES','NODEBUG', &
       & 'CUTEFF','NOTRIPLE','READCORD','REUSCORD','NOCALIB', &
       & 'DGSTDEV','BAIMULT','MORTMULT','NOHTDREG'/
  data (TABLE(I),I = 61,80) / &
       & 'RANNSEED','HTGMULT','CHEAPO','NUMTRIP','ENDFILE','BAMAX', &
       & 'READCORH','REUSCORH','MGMTID','REGHMULT','TCONDMLT', &
       & 'NOAUTOES','READCORR','REUSCORR','BRUST','IF','SCREEN', &
       & 'COMPRESS','THEN','ALSOTRY'/
  data (TABLE(I),I = 81,100) / &
       & 'ENDIF', 'NOTREES', 'CALBSTAT','OPEN','CLOSE','NOSCREEN', &
       & 'RRIN','FIXMORT','SDIMAX','DELOTAB','SERLCORR','CUTLIST', &
       & 'RESETAGE','SITECODE','MISTOE','CRNMULT','CFVOLEQU', &
       & 'BFVOLEQU','ANIN','DFB'/
  data (TABLE(I),I = 101,120) / &
       & 'RDIN','MANAGED','YARDLOSS','FMIN','STRCLASS','MODTYPE', &
       & 'FVSSTAND','PRUNE','SVS','FIXDG','FIXHTG','THINSDI', &
       & 'LOCATE','        ','THINCC','ECON','DATABASE', &
       & '        ','DEFECT','        '/
  data (TABLE(I),I = 121,135) / &
       & 'STANDCN','THINMIST','TREESZCP','THINRDEN','SPGROUP ', &
       & 'BMIN','DATASCRN','SETPTHIN','THINPT','VOLEQNUM', &
       & 'POINTREF','ECHO','NOECHO','CYCLEAT','ATRTLIST'/
  data (TABLE(I),I = 136,150) / &
       & 'THINRDSL','MORTMSB','SETSITE','CLIMATE','SDICALC', &
       & 'THINQFA','PTGROUP ','ORGANON','SPLEAVE','CCADJ', &
       & 'PRMFROST','FIAVBC','        ','        ','        '/
end block data KEYWDS
