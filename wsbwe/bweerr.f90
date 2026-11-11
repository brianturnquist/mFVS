!====================!
!   wsbwe : bweerr   !
!====================!
! WSBWE $Id$
subroutine BWEERR (IREAD,IRECNT,JOSTND,ICCODE,LRETRN,IERRN)
  implicit none

  !     ERROR PROCESSOR FOR THE BUDWORM MODEL.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
  !     INT-FORESTRY SCIENCES LABORATORY-MOSCOW, IDAHO

  !  Revision History:
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Previous noted revision was 1984
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Variables
  character(len=11) :: PREF
  character(len=28) :: DSRN
  integer :: ICCODE,IERRN,IREAD,IRECNT,JOSTND
  logical :: LRETRN

  data DSRN /'DATA SET REFERENCE NUMBER = '/
  data PREF /'********   '/
  go to( 1000,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100&
       &),  IERRN
  1000 continue
  !                        ERROR NUMBER 1
  write (JOSTND,1110) PREF,DSRN,IREAD,IRECNT
  1110 format (/,A11,'BWM01 ERROR:  INVALID KEYWORD WAS SPECIFIED. ',&
       &A28,I3,' RECORDS READ=',I4)
  if (ICCODE .lt.2) ICCODE = 2
  go to 9000
  1200 continue
  !                        ERROR NUMBER 2
  write (JOSTND,1210) PREF,DSRN,IREAD,IRECNT
  1210 format (/,A11,'BWM02 ERROR:  END-OF-FILE REACHED WHEN IT WAS ',&
       &'NOT EXPECTED, ', A28,I3,' RECORDS READ =',I5)
  if (ICCODE .lt.2) ICCODE = 2
  go to 9000
  1300 continue
  !                        ERROR NUMBER 3
  write (JOSTND,1310) PREF
  1310 format (/,A11,'BWM03 ERROR:  NO STANDS TO PROJECT.')
  1400 continue
  !                        ERROR NUMBER 4
  write (JOSTND,1410) PREF,DSRN,IREAD,IRECNT
  1410 format (/,A11,'BWM04 ERROR:  A REQUIRED PARAMETER IS MISSING',&
       &' OR INCORRECT; KEYWORD IGNORED. ',&
       &/T13,A28,I3,' RECORDS READ=',I4)
  if (ICCODE .lt.2) ICCODE = 2
  go to 9000
  1500 continue
  !                        ERROR NUMBER 5
  go to 9000
  1600 continue
  !                        ERROR NUMBER 6
  write (JOSTND,1610) PREF,DSRN,IREAD,IRECNT
  1610 format (/,A11,'BWM06 ERROR:  COLUMN 1 OF KEYWORD RECORD ',&
       &'BLANK.  ',A28,I3,' RECORDS READ =',I5)
  if (ICCODE .lt.2) ICCODE = 2
  goto 9000
  1700 continue
  !                        ERROR NUMBER 7
  write (JOSTND,1710) PREF
  1710 format (/,A11,'BWM07 ERROR:  TOO MANY STANDS.')
  goto 9000
  1800 continue
  !                        ERROR NUMBER 8
  write (JOSTND,1810) PREF
  1810 format (/,A11,'BWM08 ERROR:  DUPLICATE READFOL STAND-IDS ',&
       &'WERE ENCOUNTERED WHEN READING READFOL FILE.  ',&
       &'NET DISPERSAL RATES WERE USED.')
  goto 9000
  1900 continue
  !                        ERROR NUMBER 9
  write (JOSTND,1910) PREF
  1910 format (/,A11,'BWM09 ERROR:  STNDLOC RECORD NOT FOUND FOR ',&
       &'STAND THAT WAS ENTERED USING READFOL.  ',&
       &'NET DISPERSAL RATES ARE USED.')
  goto 9000
  2000 continue
  !                     ERROR NUMBER 10
  write (JOSTND,2010) PREF
  2010 format(/,A11,'BWM10 ERROR:  END OF MEMORY IN STAND ',&
       &'HOLDING AREA.')
  goto 9000
  2100 continue
  !                     ERROR NUMBER 11
  write (JOSTND,2110) PREF
  ICCODE = 0
  2110 format (/,A11,'BWM11 ERROR:  STARTING YEAR GREATER THAN ',&
       &'ENDING YEAR.')
  9000 continue

  !     CALL THE SUBROUTINE TRACING ROUTINE (IBM ONLY)

  !     IF (ICCODE .GT. 2) CALL ERRTRA

  !     RETURN OR NOT??????

  if (LRETRN) return

  entry BWSTOP(ICCODE)

  if (ICCODE .eq.0) stop
  go to (9010,9020,9030,9040,9050), ICCODE
  9010 continue
  stop 10
  9020 continue
  stop 20
  9030 continue
  stop 30
  9040 continue
  stop 40
  9050 continue
  stop 50
end
