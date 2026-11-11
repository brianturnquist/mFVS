!=================================!
!   Stand visualization: SVTRIP   !
!=================================!
! BASE $Id$
subroutine SVTRIP (IBASE,IT1)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     UPDATES THE VISUALIZATION FOR RECORD TRIPLING.

  !     CALLED FROM TRIPLE.
  !     INPUT IBASE, IS THE BASE TREE RECORD, IT1 IS THE FIRST
  !     TRIPLE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'SVDATA.f90'

  ! Variables
  integer :: NRT(3),ITR(3)
  integer :: IT1,IBASE,NR,ISVOBJ,I,NTOC
  ITR(1) = IBASE
  ITR(2) = IT1
  ITR(3) = IT1+1

  !     GET THE NUMBER OF OBJECTS USED TO REPRESENT THE BASE TREE.

  NR = 0

  do ISVOBJ = 1,NSVOBJ
     if (IOBJTP(ISVOBJ).eq.1 .and. IS2F(ISVOBJ).eq.IBASE) NR = NR +1
  end do

  if (NR.le.1) return

  !     COMPUTE THE NUMBER OF OBJECTS THAT EACH TRIPLE WILL GET.

  NRT(1) = IFIX(FLOAT(NR)*.6+.5)
  NRT(2) = MAX(0,IFIX(FLOAT(NR-NRT(1))*.625+.5))
  NRT(3) = MAX(0,NR-NRT(1)-NRT(2))

  if (NRT(2).le.0) return

  !     PROCESS THE TRIPLES.

  I = 1
  NTOC = NRT(1)
  do ISVOBJ = 1,NSVOBJ
     if (IS2F(ISVOBJ).eq.IBASE .and. IOBJTP(ISVOBJ).eq.1) then
        IS2F(ISVOBJ) = ITR(I)
        NTOC = NTOC -1
        if (NTOC.eq.0) then
           I = I + 1
           if (I.gt.3) return
           NTOC = NRT(I)
           if (NTOC.eq.0) return
        end if
     end if
  end do

  return
end subroutine SVTRIP
