!==========================!
!   fire/base : fmsvsync   !
!==========================!
! FIRE-BASE $Id$
subroutine FMSVSYNC
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     A.H.DALLMANN -- RMRS MOSCOW -- MAY 2000

  !     SYNCHRONIZES THE SV OUTPUT SNAGS WITH THE SNAGS THAT THE FIRE
  !     MODEL IS KEEPING TRACK OF.

  !     *****  UNDER DEVELOPMENT ******

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'SVDATA.f90'
  include 'SVDEAD.f90'

  ! Variables
  real    :: FMTOTAL
  integer :: D,S
  integer :: SVTOTAL
  integer :: DBHCL
  logical :: DEBUG

  real    :: CURFMSN(19,MAXSP)
  integer :: CURSVSN(19,MAXSP)
  integer :: I

  call DBCHK (DEBUG,'FMSVSYNC',8,ICYC)

  if (DEBUG) write (JOSTND,'('' IN FMSVSYNC, JSVOUT='',I4)') JSVOUT

  !     NO NEED TO SYNC THE DATA IF VISUALIZATION IS NOT BEING DONE!

  if (JSVOUT.eq.0) return

  !     CLEAR OUT OUT TEMP LIST

  do D = 1,19
     do S = 1,MAXSP
        CURFMSN(D,S) = 0.
        CURSVSN(D,S) = 0
     end do
  end do

  FMTOTAL = 0.
  SVTOTAL = 0

  !     ADD UP ALL OF THE DIFFERENT DENSITIES FROM THE FIRE SNAGS.

  do I = 1,NSNAG
     DBHCL = INT(DBHS(I)/2.0+1)
     if (DBHCL .gt.19) DBHCL = 19

     CURFMSN(DBHCL,SPS(I)) = CURFMSN(DBHCL,SPS(I))+DENIS(I)+DENIH(I)
     if (DBHCL .ge.2) FMTOTAL = FMTOTAL + DENIS(I) + DENIH(I)
  end do

  !     ADD UP ALL OF THE STANDING SV SNAGS IN EACH CATEGORY OF THE FIRE SNAGS.

  do I = 1,NSVOBJ

     if (IOBJTP(I).ne.2) cycle
     if (FALLDIR(IS2F(I)).eq.-1) then

        !           THIS IS A STANDING SNAG, GET THE DIAMETER CLASS AND SPECIES

        DBHCL = INT(ODIA(IS2F(I)) / 2.0 + 1.0)
        if (DBHCL .gt.19) DBHCL = 19

        !           NOW THAT WE HAVE THE CORRECT DIAMETER CLASS, WE NEED TO
        !           ADD ONE TO THE MATCHING SPECIES-DIAMETER COUNT

        CURSVSN(DBHCL,ISNSP(IS2F(I))) = CURSVSN(DBHCL,ISNSP(IS2F(I)))&
             &+ 1

        !           ADD ONE TO THE SVTOTAL AS WELL.

        if (DBHCL .ge.2) SVTOTAL = SVTOTAL + 1
     end if

  end do

  !     DISPLAY THE DIFFERENCE BETWEEN THE PROB AND THE OBJECTS.

  if (DEBUG) then

     do S = 1,MAXSP
        do D = 2,19
           if (CURFMSN(D,S)+CURSVSN(D,S).gt.0.) then
              write (JOSTND,10) S,D,CURFMSN(D,S),CURSVSN(D,S)
              10 format (' Species=',A,' DBHClass=',I3,' CURFM=',&
                   &F8.3,' CURSVSN=',I4)
           end if
        end do
     end do
     write (JOSTND,'('' TOTAL DIFF='',F10.3)') SVTOTAL-FMTOTAL
  end if

  return
end
