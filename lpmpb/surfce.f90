!====================!
!   lpmpb : surfce   !
!====================!
! LPMPB $Id$
subroutine SURFCE
  implicit none

  !     SURFACE AREA CALCUTLATION ROUTINE
  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
  !     NICK CROOKSTON; PROGRAMMER

  !      THIS ROUTINE IS CALLED FROM MPBCUP WHEN MPBMOD WILL BE CALLED

  !      EQUATIONS ARE FROM A. STAGE

  !          SNOHST= TOTAL NON-LPP SURFACE AREA TO 5 INCH TOPS
  !          SURF  = SURFACE OF LPP BY CLASS TO 5 INCH TOPS
  !          SUR   = TEMPORARY.  SURFACE TO THE TOP
  !          SUR5  = TEMPORARY.  SURFACE TO 5 INCH TOP

  ! Revision History:
  !   06/05/00  Glen Brink (FHTET)
  !     Added variables IDXWP,IDXWL,IDXDF,IDXLP and IDXPP, array indices of
  !       White Pine, Western Larch, Douglas Fir, Lodgepole Pine and
  !       Ponderosa Pine respectively.  Added to common block in file
  !       MPBCOM.F77.
  !     Added array MPBSPM to govern the computations by species using an
  !       IF block as opposed to the old COMPUTED GO TO, since the array
  !       allows the definition to be made in mpblkd.f, instead of always
  !       having to change the COMPUTED GO TO. Added to common block in
  !       file MPBCOM.F77.
  !   11/10/00  Lance David (FHTET)
  !     Basic cleanup of code and comments.
  !     Removed old & inactive code.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MPBCOM.f90'

  ! Variables
  integer :: I,I1,I2,II,IS
  real    :: SUR,SUR5,SURFLP

  SNOHST = 0.0

  !     SPECIES LOOP.

  do IS = 1,MAXSP
     if (IS .eq. IDXLP) go to 200
     I1 = ISCT(IS,1)
     if (I1.eq.0) go to 200
     I2 = ISCT(IS,2)

     !       TREE RECORD LOOP WITHIN SPECIES

     do II = I1, I2
        I = IND1(II)

        if (MPBSPM(IS) .eq. IDXWP) then

           !           WHITE PINE

           SUR = -1.8673+0.86*ALOG(DBH(I))+1.07854*ALOG(HT(I))
           SUR5 = 19.6794 + 0.2345*HT(I) + 0.1363*CFV(I) + 0.8841*SUR

        else if (MPBSPM(IS) .eq. IDXWL) then

           !           WESTERN LARCH

           SUR = -3.0779+0.062565*ALOG(DBH(I))+1.46018*ALOG(HT(I))
           SUR5 = 31.90062 - 0.12195784*HT(I) + 0.071106*CFV(I) +&
                &0.972312*SUR

        else if (MPBSPM(IS) .eq. IDXDF) then

           !           DOUGLAS-FIR

           SUR = -1.73648+0.912291*ALOG(DBH(I))+1.010666*ALOG(HT(I))
           SUR5 = 19.4183 + 0.2631*HT(I) + 0.2593*CFV(I) + 0.8274*SUR

        else if (MPBSPM(IS) .eq. IDXPP) then

           !           PONDEROSA PINE

           SUR = -1.51556+0.99399*ALOG(DBH(I))+0.93723*ALOG(HT(I))
           SUR5 = 9.1767 + 0.4531*HT(I) + 0.3719*CFV(I) + 0.7589*SUR

        end if

        if (SUR5.ge.0.0)  SNOHST = SNOHST + SUR5 * PROB(I)

        100 continue
     end do
     200 continue
  end do

  !     LODGEPOLE PINE   (BY CLASS)

  do I = 1,NACLAS
     SURF(I) = SURFLP(CLASS(I,2))
     250 continue
  end do

  if (DEBUIN) then
     write (JOMPB,400) SNOHST,(SURF(I),I = 1,NACLAS)
     400 format (/,'SNOHST (TOTAL NON-LPP SURFACE AREA TO',&
          &' 5 INCH TOPS) ',E14.7,//,&
          &'LPP SURFACE AREA BY CLASS . . .',//,&
          &3(10E13.3,/))
  end if

  return
end
