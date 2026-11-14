!===================!
!   wwpb : bmcnum   !
!===================!
! WWPB $Id$
subroutine BMCNUM(ISTD,IYR)

  !      CALLED BY: BMDRV

  !  **BMCNUM  Date of last revision: Aug, 1999 (AJM)
  !                                   June 7, 1994

  !  Purpose:
  !      This routine calculates the numerator of the "scoring" equation
  !      used to move beetles between stands. Calculations are done here
  !      because many of the necessary variables are already in memory
  !      (and would not be in memory outside of this stand loop).

  !      There will be potentially two passes through this routine.
  !      If Ips are not the primary pest, then we need to calculate
  !      the numerator for Ips and for the main pest (MPB or WPB)
  !      separately. 'NUMER' will thus have two dimensions.
  !      If Ips is the primary pest, then there only needs to be one pass.

  !      Change made 8/99: add back in previous year's BA killed to
  !      attractiveness.  Without this change, a stand of 150 BA having
  !      50 BA killed "looks" just like a stand of 100 to the beetles.
  !      With this change, the stand will continue to be attractive to
  !      the beetles.

  !  Call list definitions:
  !     NUMER:  (2,plot) Numerator of the scoring equation for special trees
  !             in the stand.

  !  Local variable definitions:
  !     BA:     BA in size class
  !     BAH:    BAH host tree BA modified by stand GRF
  !     BAHG:   BA of host trees above a minimum size class for attack
  !     BAIS:   BA in stand
  !     DEBUG:  Logical flag to turn debug on and off.
  !     IDSIZ:  Loop counter over Dead Woody Pool size classes
  !     IDTYP:  Loop over Dead Woody Pool categories
  !     IPASS:  Loop counter over the # passes through routine
  !     ITYP:   Loop counter over tree types
  !     ISCMIN: Array holding the minimum dbh for attack (by pass #)
  !     ISIZ:   Loop counter over dbh size classes
  !     NPASS:  Number of passes to do through routine
  !     SPEC:   Temporary total # special trees in stand

  !  Common block variables and parameters:
  !     USERA:  Variable used to scale attractiveness of "special" trees
  !     DWPHOS: From BMCOM; Array to hold volumes of downed host tree
  !             volume for Ips (stratified by standing/dead and size class)
  !     GRFSTD: From BMCOM; GRF for the stand
  !     MSBA:   FROM BMCOM; BA in each size class
  !     MXDWSZ: From BMPRM; Maximum # Downed Woody Pool size classes
  !     NSCL:   From BMPRM; Number of dbh size classes
  !     PBSPEC: From BMPRM; Pine Beetle specie(s) being simulated
  !     REPPHE: From BMCOM; Repellant pheromone...
  !     SCMIN:  From BMCOM; minimum size class for attack
  !     TFOOD:  From BMCOM; total beetle attractive 'food'

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.

  include 'BMCOM.f90'

  !.... Variable declarations.

  ! Variables
  integer :: ISIZ
  integer :: LISCMIN(2)
  integer :: NPASS

  real    :: BAIS,BAHG
  real    :: SPTREE,SPAREA
  real    :: SPEC
  real    :: ASPEC(2)

  if (LBMDEB) write(JBMBPR,10) IYR, ISTD
  10 format(' Begin BMCNUM: Year= ',I5, 'Stand= ', I6)

  LISCMIN(1) = ISCMIN(PBSPEC)
  ASPEC(1) = USERA(PBSPEC)
  NUMER(1,ISTD) = 0.0
  NUMER(2,ISTD) = 0.0
  TFOOD(ISTD,1) = 0.0
  TFOOD(ISTD,2) = 0.0

  NPASS = 1

  if (IPSON) then
     !       If Ips is a driving variable, then need to go second time through loop
     NPASS = 2
     LISCMIN(2) = ISCMIN(3)
     ASPEC(2) = USERA(3)
  end if

  !.... DO for the number of passes through this routine

  do IPASS = 1,NPASS

     BAIS = 0.0
     SPEC = 0.0
     BAHG = 0.0
     SPAREA = 0.0

     BAIS = BAH(ISTD,NSCL+1) + BANH(ISTD,NSCL+1)
     if (BAIS .le.1E-6) goto 100

     do ISIZ = 1,NSCL

        !....     find total special trees in stand

        call BMCSPT(ISTD,ISIZ,IPASS,IYR)

        SPTREE = TREE(ISTD,ISIZ,1) * SPCLT(ISTD,ISIZ,IPASS)
        SPEC = SPEC + SPTREE
        if (ISIZ .ge. LISCMIN(IPASS)) SPAREA = SPAREA+ SPTREE*MSBA(ISIZ)

        !....     find BA in stand of all trees and of big host trees only,
        !         using only clumped info

        if (ISIZ .ge. LISCMIN(IPASS)) BAHG = BAHG + BAH(ISTD,ISIZ)

        110 continue
     end do

     !....   for repelling pheromone

     if (REPPHE(ISTD) .eq.0.0) REPPHE(ISTD) = 1.0

     if ((IPASS .eq.2) .or. (PBSPEC .eq.3)) then

        !....     Ips likes slash so is "special" for attractiveness.
        !         DWPHOS() includes blowdown trees calculated in SUB CalcWindthr.

        do IDSIZ = 1,MXDWSZ

           !....        1=standing, 2=down

           do IDTYP = 1,2

              TFOOD(ISTD,IPASS) = TFOOD(ISTD,IPASS) +&
                   &DWPHOS(ISTD,IDTYP,IDSIZ) * WPBA(IDSIZ)

              SPEC = SPEC + DWPHOS(ISTD,IDTYP,IDSIZ)
              210 continue
           end do
           200 continue
        end do

     end if

     !         Total beetle 'food' for use in attractiveness eq.
     !         Note that slash is not modified by the proportion
     !         of host in the stand.

     ! BAIS total basal area in stand
     ! BAHG basal area host > minimum size

     TFOOD(ISTD,IPASS) = TFOOD(ISTD,IPASS) + BAHG * (BAHG / BAIS)

     NUMER(IPASS,ISTD) = (ASPEC(IPASS) * SPEC + 1.0) * BAIS *&
          &((REPPHE(ISTD) * BAHG) + SPAREA + SSBATK(ISTD)) / GRFSTD(ISTD)
     !     >  REPPHE(ISTD) * (BAHG + SPAREA) / GRFSTD(ISTD)
     !...  Added back in previous year's beetle-killed BA.  ajm 8/99.

     100 continue
  end do

  if (LBMDEB) write(JBMBPR,99) IYR, ISTD
  99 format(' Begin BMCNUM: Year= ',I5, 'Stand= ', I6)

  return
end
