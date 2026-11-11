!===================!
!   estb : escprs   !
!===================!
! ESTB $Id$
subroutine ESCPRS (ITRGT,DEBUG)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ESHAP.f90'
  include 'ARRAYS.f90'
  include 'OUTCOM.f90'

  ! Variables
  logical :: DEBUG
  real    :: PRMS(2)
  real    :: SPCNT(MAXSP,3)
  real    :: SITAGE,SITHT,AGMAX,H,HTMAX,HTMAX2,D1,D2
  integer :: IS,IM,ISPC
  integer :: MYACT(1)
  integer :: ITRGT,I,N,KDT
  data MYACT/250/

  !     CALL THE COMPRESSION ROUTINE.

  I = ITRN
  N = ITRGT
  PRMS(2) = .5
  call COMPRS (N,PRMS(2))

  !     REESTABLISH THE SPECIES-ORDER SORT.

  call SPESRT

  !     INITIALIZE SPCNT.

  do I = 1,MAXSP
     SPCNT(I,1) = 0.
     SPCNT(I,2) = 0.
     SPCNT(I,3) = 0.
  end do
  if (ITRN.gt.0) then

     !        RE-COMPUTE THE SPECIES COMPOSITION

     do I = 1,ITRN
        IS = ISP(I)
        IM = IMC(I)
        SPCNT(IS,IM) = SPCNT(IS,IM)+PROB(I)
        70 continue
     end do

     !        REESTABLISH THE DIAMETER SORT.

     call RDPSRT (ITRN,DBH,IND,.true.)

     !        RE-COMPUTE THE DISTRIBUTION OF TREES PER ACRE AND
     !        SPECIES-TREE CLASS COMPOSITON BY TREES PER ACRE.

     call PCTILE(ITRN,IND,PROB,WK3,ONTCUR(7))

     !  ESTTE MISSING TOTAL TREE AGES

     do I = 1,ITRN
        if (ABIRTH(I) .le.0.) then
           SITAGE = 0.0
           SITHT = 0.0
           AGMAX = 0.0
           HTMAX = 0.0
           HTMAX2 = 0.0
           ISPC = ISP(I)
           D1 = DBH(I)
           H = HT(I)
           D2 = 0.0
           call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,&
                &HTMAX2,DEBUG)
           if (SITAGE .gt.0.)ABIRTH(I) = SITAGE
        end if
     end do

  end if

  !     (MAKE SURE IFST=1, TO GET A NEW SET OF POINTERS TO THE
  !      DISTRIBUTIONS).

  IFST = 1
  call DIST(ITRN,ONTCUR,WK3)
  call COMP(OSPCT,IOSPCT,SPCNT)

  !     WRITE A MSG SAYING THAT COMPRESS WAS CALLED.

  if (DEBUG) write (JOREGT,10) I,N,IY(ICYC+1)-1
  10 format('IN ESCPRS: COMPRS: ',I5,' RECS',&
       &' TO',I5,'.  YEAR:',I5)

  !     ADD COMPRESS TO THE ACTIVITIY SCHEDULE AND SIGNAL THAT IT IS
  !     BY SETTING THE STATUS CODE TO KDT (THIRD ARGUMENT).

  PRMS(1) = N
  KDT = IY(ICYC+1)-1
  call OPADD (KDT,MYACT(1),KDT,2,PRMS,I)
  return
end
