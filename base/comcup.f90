!======================================!
!   Coordinate tree list compression   !
!======================================!
! BASE $Id$
subroutine COMCUP
  implicit none

  !     INTERFACE ROUTINE TO COUPLE THE COMPRESSION ROUTINE TO
  !     THE PROGNOSIS MODEL FOR STAND DEVELOPMENT.

  !     ADDED THE CAPABILITY TO DELETE TREES THAT HAVE ZERO (AND/OR
  !     NEAR-ZERO) PROBS...TOTAL REWRITE.  NL CROOKSTON, JUNE 1991.

  !     N.L. CROOKSTON       INT-MOSCOW     DEC. 1981 & JUNE 1982

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'

  ! Variables
  logical :: LDEBU,LCMPRS
  integer :: NDEL,I,NTODO,NPRMS,IACT,IDT,ITARG,IS,IM,MYACT(1),ISPC
  real    :: SPCNT(MAXSP,3),PRMS(3),PN1
  real    :: SITAGE,SITHT,AGMAX,H,HTMAX,HTMAX2,D1,D2

  data MYACT/250/

  !     CHECK FOR DEBUG REQUEST.

  call DBCHK (LDEBU,'COMCUP',6,ICYC)

  !     CHECK THE TREELIST FOR ZERO PROBS.  SET UP IND2 IN THE SAME
  !     PASS.

  LCMPRS = .false.
  NDEL = 0
  if (ITRN.gt.0) then
     do I = 1,ITRN
        if (PROB(I).le. 1E-5) then
           IND2(I) = -I
           NDEL = NDEL+1
        else
           IND2(I) = I
        end if
        10 continue
     end do
  end if

  !     IF ALL THE TREES HAVE ZERO PROBS, SET ITRN TO ZERO.

  if (NDEL.eq.ITRN) then
     ITRN = 0
     IREC1 = 0
  end if
  if (LDEBU) write (JOSTND,20) ITRN,NDEL
  20 format (/' IN COMCUP (TOP): ITRN,NDEL=',2I6)

  !     IF THERE ARE SOME ZERO PROBS, AND ITRN IS GREATER THAN ZERO,
  !     THEN DELETE THEM USING TREDEL.

  if (NDEL.gt.0 .and. ITRN.gt.0) call TREDEL (NDEL,IND2)

  !     GET THE COMPRESSION REQUEST.

  call OPFIND (1,MYACT,NTODO)

  !     IF NTODO IS GT ZERO, THEN:

  if (NTODO .gt. 0) then

     !        GET THE LAST COMPRESSION REQUEST AND DELETE ALL THE OTHERS.

     call OPGET (NTODO,3,IDT,IACT,NPRMS,PRMS)
     if (NTODO.gt.1) then
        do I = 1,(NTODO-1)
           call OPDEL1(I)
           40 continue
        end do
     end if

     !        SET THE PARAMETERS OF COMPRESSION.

     ITARG = IFIX(PRMS(1))
     PN1 = PRMS(2)/100.

     !        DETERMINE IF COMPRESSION WILL TAKE PLACE AFTER ALL.

     LCMPRS = ITARG.lt.ITRN

     !        WRITE DEBUG MSG, IF REQUESTED.

     if (LDEBU) write (JOSTND,50) ITARG,PN1,LCMPRS
     50 format (/' IN COMCUP: ITARG,PN1,LCMPRS: ',I5,F8.3,L3)

     !        IF COMPRESSION IS TO TAKE PLACE; THEN: COMPRESS THE
     !        TREE LIST.

     if (LCMPRS) then

        !           FOR THIS CALL TO COMPRS, WK2 MUST BE INITIALIZED
        !           TO ZERO.

        do I = 1,ITRN
           WK2(I) = 0.0
           55 continue
        end do

        !           COMPRESS THE TREE LIST.

        call COMPRS (ITARG,PN1)

        !           SIGNAL THAT COMPRESSION HAS TAKEN PLACE.

        call OPDONE (NTODO,IY(ICYC))

        !           SUPPRESS RECORD TRIPLING.

        NOTRIP = .true.

     else

        !           SIGNAL OPDEL1.

        call OPDEL1 (NTODO)
     end if
  end if

  !     IF COMPRESSION TOOK PLACE, OR IF TREES WITH PROBS OF ZERO,
  !     WERE DELETED FROM THE TREELIST, THEN: DO THE FOLLOWING:
  !     (NOTE: ALL THIS CAN BE DONE WITH ITRN=0).

  if (LCMPRS.or.NDEL.gt.0) then

     !        REESTABLISH THE SPECIES-ORDER SORT.

     call SPESRT

     !        INITIALIZE SPCNT.

     do I = 1,MAXSP
        SPCNT(I,1) = 0.
        SPCNT(I,2) = 0.
        SPCNT(I,3) = 0.
     end do
     if (ITRN.gt.0) then

        !           RE-COMPUTE THE SPECIES COMPOSITION

        do I = 1,ITRN
           IS = ISP(I)
           IM = IMC(I)
           SPCNT(IS,IM) = SPCNT(IS,IM)+PROB(I)
           70 continue
        end do

        !           REESTABLISH THE DIAMETER SORT.

        call RDPSRT (ITRN,DBH,IND,.true.)

        !           RE-COMPUTE THE DISTRIBUTION OF TREES PER ACRE AND
        !           SPECIES-TREE CLASS COMPOSITON BY TREES PER ACRE.

        call PCTILE(ITRN,IND,PROB,WK3,ONTCUR(7))

        !  ESTIMATE MISSING TOTAL TREE AGES

        do I = 1,ITRN
           if (ABIRTH(I) .le. 0.) then
              SITAGE = 0.0
              SITHT = 0.0
              AGMAX = 0.0
              HTMAX = 0.0
              HTMAX2 = 0.0
              ISPC = ISP(I)
              D1 = DBH(I)
              H = HT(I)
              D2 = 0.0
              call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX, &
                   & HTMAX2,LDEBU)
              if (SITAGE .gt. 0.)ABIRTH(I) = SITAGE
           end if
        end do

     end if

     !        (MAKE SURE IFST=1, TO GET A NEW SET OF POINTERS TO THE
     !         DISTRIBUTIONS).

     IFST = 1
     call DIST(ITRN,ONTCUR,WK3)
     call COMP(OSPCT,IOSPCT,SPCNT)

     !        RE-COMPUTE THE STAND DENSITY STATISTICS AND SIGNAL THAT
     !        DENSE HAS BEEN COMPUTED.

     call DENSE
  end if
  if (LDEBU) write (JOSTND,80) ITRN,NDEL
  80 format (/' IN COMCUP (BOT): ITRN,NDEL=',2I6)
  return
end subroutine COMCUP
