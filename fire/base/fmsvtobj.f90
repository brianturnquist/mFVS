!==========================!
!   fire/base : fmsvtobj   !
!==========================!
! FIRE-BASE $Id$
subroutine FMSVTOBJ(IFTYP)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     A.H.DALLMANN -- RMRS MOSCOW -- JANUARY 2000

  !     COPIES THE CURRENT CONTENTS OF IOBJTP VECTOR INTO TIOBJTP VECTOR
  !     SO THAT SVMORT (ETC) CAN MANIPULATE THE OBJECT LIST AND CAN BE COMPARED
  !     AN ORIGINAL.

  !     CALLED FROM FMEFF.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'SVDATA.f90'
  include 'SVDEAD.f90'
  include 'FMSVCM.f90'

  ! Variables
  integer :: I,IFTYP

  do I = 1,NSVOBJ
     IOBJTPTMP(I) = IOBJTP(I)
     IS2FTMP(I) = IS2F(I)
  end do

  !     SINCE THIS IS ONLY CALLED FROM FMEFF.F, WE NEED TO
  !     CONVERT STANDING "GREEN" AND "RED" SNAGS INTO STANDING BURNED
  !     SNAGS (CROWNING AND TORCHING FIRES ONLY)

  do I = 1,NSVOBJ
     if (IOBJTP(I) .eq.2) then
        if (FALLDIR(IS2F(I)) .eq. -1) then
           if (IFTYP.eq.1.or. IFTYP.eq.2 ) then
              if (ISTATUS(IS2F(I)) .eq.2) then
                 ISTATUS(IS2F(I)) = 5
              else if (ISTATUS(IS2F(I)) .eq.3) then
                 ISTATUS(IS2F(I)) = 6
              else if (ISTATUS(IS2F(I)) .eq.4) then
                 ISTATUS(IS2F(I)) = 6
              end if
           end if
        else

           !              ALWAYS CONVERT LAYING SNAGS INTO BURNED SNAGS.

           if (ISTATUS(IS2F(I)) .eq.2) then
              ISTATUS(IS2F(I)) = 5
           else if (ISTATUS(IS2F(I)) .eq.3) then
              ISTATUS(IS2F(I)) = 6
           else if (ISTATUS(IS2F(I)) .eq.4) then
              ISTATUS(IS2F(I)) = 6
           end if
        end if
     end if
  end do

  return
end
