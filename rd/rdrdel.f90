!=================!
!   rd : rdrdel   !
!=================!
! RD $Id$
subroutine RDRDEL
  implicit none

  !  THIS SUBROUTINE LOOPS THRU THE TREELIST TO DETERMINE IF
  !  ANY TREES HAVE BEEN ZEROED OUT FOR ALL THE ROOT ROT PROB VARIABLES.
  !  THIS CAN HAPPEN IN THE FIRST CYCLE IF AN INFECTED TREE IS KILLED
  !  TOTALLY, IE THE ENTIRE VALUE FOR PROBI IS ASSIGNED TO STUMPS AND
  !  SET TO ZERO. THIS TREE SHOULD BE DELETED FROM THE TREELIST TO
  !  PREVENT ERRORS IN RRGROW AS A RESULT OF PROBI,PROBIU,PROBIT
  !  AND FPROB ALL BEING ZERO.

  !  CALLED BY :
  !     RDTREG  [ROOT DISEASE]

  !  CALLS     :
  !     TREDEL  (SUBROUTINE)   [PROGNOSIS]
  !     SPESRT  (SUBROUTINE)   [PROGNOSIS]
  !     RDPSRT  (SUBROUTINE)   [PROGNOSIS]
  !     PCTILE  (SUBROUTINE)   [PROGNOSIS]
  !     DIST    (SUBROUTINE)   [PROGNOSIS]
  !     COMP    (SUBROUTINE)   [PROGNOSIS]

  !  Revision History
  !    21-MAR-00 Lance David (FHTET)
  !      Added Debug code.
  !   09/02/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'OUTCOM.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'RDADD.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,I1,I2,IM,IS,IT,IVAC,J,KSP
  real    :: SPCNT(MAXSP,3),TOAKL

  !.... SEE IF WE NEED TO DO SOME DEBUG.

  call DBCHK (DEBUG,'RDRDEL',6,ICYC)
  if (DEBUG)&
       &write (JOSTND,*) 'ENTER RDRDEL:  ICYC=',ICYC,' ITRN=',ITRN

  do I = 1,MAXSP
     SPCNT(I,1) = 0.0
     SPCNT(I,2) = 0.0
     SPCNT(I,3) = 0.0
     444 continue
  end do

  !     DEBUG CODE

  if (DEBUG) then
     do I = 1,ITRN
        write(JOSTND,889) I,PROBI(I,1,1),PROBIU(I),FPROB(I)
        889 format(' IN RDRDEL :   I,PROBI,PROBIU,FPROB',I4,3F8.2)
        884 continue
     end do
  end if

  IVAC = 0
  do KSP = 1,MAXSP
     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)
     if (ISCT(KSP,1) .eq.0) goto 555

     do J = I1,I2
        IT = IND1(J)
        if (PROBIU(IT) .eq.0.0.and. PROBIT(IT) .eq.0.0&
             &.and. FPROB(IT) .eq.0.0) then

           TOAKL = OAKL(1,IT) + OAKL(2,IT) + OAKL(3,IT)

           if (RDKILL(IT) .le.0.0.and. TOAKL .le.0.0) then

              !                WE ONLY WANT TO DELETE TREE RECORD IF NOTHING IS USING IT
              !                (FOR PRINTING, WE NEED TO ACCESS CFV(IT) AND FOR BEETLES,
              !                WE NEED THE DBH AND PROPI OF THESE RECORDS). CORRECTIONS
              !                WILL BE ADDED TO RDGROW TO COMPENSATE FOR NOT DELETING THESE
              !                RECORDS.

              PROB(IT) = 0.0
              IVAC = IVAC + 1
              IND1(J) = -IT

           end if
        end if
        556 continue
     end do
     555 continue
  end do

  !     IF ANY TREES TO BE DELETED CALL TREDEL AND RRTDEL, SPESRT TO
  !     REALIGN SPECIES SORT AND RDPSRT TO RESORT BY DBH.

  if (DEBUG) write (JOSTND,999) IVAC,(I,IND1(I),I = 1,ITRN)
  999 format (' IN RDRDEL BEFORE DELETE: IVAC = ',I4/&
       &((' ITREE = ',I4,'; IND1(ITREE) = ',I4)))

  if (IVAC .le.0) goto 566
  call TREDEL (IVAC,IND1)
  call SPESRT
  if (ITRN .le.0) goto 565
  call RDPSRT (ITRN,DBH,IND,.true.)

  !     RE-COMPUTE THE SPECIES COMPOSITION

  565 continue
  do I = 1,ITRN
     IS = ISP(I)
     IM = IMC(I)
     SPCNT(IS,IM) = SPCNT(IS,IM) + PROB(I)
     60 continue
  end do

  !     RE-COMPUTE THE DISTRIBUTION OF TREES PER ACRE AND SPECIES-
  !     TREE CLASS COMPOSITON BY TREES PER ACRE.

  !     (MAKE SURE IFST=1, TO GET A NEW SET OF POINTERS TO THE
  !      DISTRIBUTIONS).

  IFST = 1
  call PCTILE (ITRN,IND,PROB,WK3,ONTCUR(7))
  call DIST (ITRN,ONTCUR,WK3)
  call COMP (OSPCT,IOSPCT,SPCNT)

  if (DEBUG)&
       &write(JOSTND,619) (I, PROBI(I,1,1),PROBIU(I),FPROB(I),I = 1,ITRN)
  619 format(' IN RDRDEL AFT: I PROBI,PROBIU,FPROB',I4,3F8.2)

  566 continue

  !     DEBUG CODE

  if (DEBUG) then
     do I = 1,ITRN
        write (JOSTND,489) I,PROB(I),WK2(I)
        489 format (' IN RDRDEL, I,PROB ,WK2 ',I4,2F8.2)
        484 continue
     end do
  end if

  if (DEBUG) write (JOSTND,*) 'EXIT RDRDEL'
  return
end
