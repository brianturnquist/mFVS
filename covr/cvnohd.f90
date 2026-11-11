!===================!
!   covr : cvnohd   !
!===================!
! COVR $Id$
subroutine CVNOHD
  implicit none

  !     WRITES THE COVER STATISTICS TO A FLAT FILE (NOHEADINGS).

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'CVCOM.f90'
  include 'PLOT.f90'
  include 'OUTCOM.f90'

  ! Variables
  integer :: I,ITHN,ISUM1,ISUM2,ISUM31,ISUM32,ISUM33,ISUM3,ISUM4,ISUM7
  integer :: ISUM8,ISUM9,ISUM10,ISUM11,ISUM12,ISUM13,ISUM14,ISUM15
  character(len=11) :: CISN

  !     RETURN IF NO COVER STATISTICS WERE COMPUTED.

  if (.not.LCOV) return

  !     RETURN IF THE NOHEADING VERSION OF THE COVER STATS IS NOT WANTED.

  if (.not.LCVNOH) return

  !  SET OLD PPE VARIABLE TO BLANK, BUT KEEP TO PRESERVE HEADER FORMAT

  CISN = '           '

  !     LOOP OVER ALL CYCLES PLUS ONE.

  do I = 1,NCYC+1

     !     IF THE COUNTER IS LESS THAT THE COVER BEGINNING CYCLE, SKIP
     !     THE OUTPUT.

     if (I.lt.ICVBGN) go to 100

     !     LOOP OVER THIN AND NON-THIN.

     do ITHN = 1,2

        !     IF ON THE THIN LOOP AND NO THINNING OCCURRED, SKIP THE OUTPUT.

        if (ITHN.eq.2.and..not.LTHIND(I)) goto 90

        !     PREPARE STATISTICS FOR OUTPUT (CODE IS TAKEN FROM CVOUT).

        ISUM1 = IFIX (.5+TIMESD(I,ITHN))
        ISUM2 = IFIX (.5+PGT0(I,ITHN)*100.)
        ISUM31 = IFIX (.5+CLOW(I,ITHN))
        ISUM32 = IFIX (.5+CMED(I,ITHN))
        ISUM33 = IFIX (.5+CTALL(I,ITHN))
        ISUM3 = IFIX (.5+TOTLCV(I,ITHN))
        ISUM4 = IFIX (.5+SBMASS(I,ITHN))
        ISUM7 = ICVAGE(I)
        ISUM8 = IFIX (.5+STDHT(I,ITHN))
        ISUM9 = IFIX (.5+TPCTCV(I,ITHN))
        ISUM10 = IFIX (.5+TOTBMS(I,ITHN))
        ISUM11 = IFIX (.5+SDIAM(I,ITHN))
        ISUM12 = IFIX (.5+TRETOT(I,ITHN))
        ISUM13 = IFIX (.5+PROXHT(I,ITHN,1))
        ISUM14 = IFIX (.5+PROXHT(I,ITHN,2))
        ISUM15 = IFIX (.5+TPROAR(I,ITHN))

        !     WHEN THE DATE OF DISTURBANCE IS GREATER THAN 40 YEARS, DO NOT
        !     WRITE THE SHRUB DATA, ONLY THE TREE STATISTICS

        if (TIMESD(I,ITHN) .le.40.) then
           write (JCVNOH,10) NPLT,MGMID,IY(I),ITHN,ISUM1,&
                &ISUM2,ISUM31,ISUM32,ISUM33,ISUM3,ASHT(I,ITHN),&
                &ISUM4,TWIGS(I,ITHN),ISTAGE(I,ITHN),ISUM7,ISUM8,&
                &ISUM9,ISUM10,ISUM11,ISUM12,ISUM13,ISUM14,ISUM15,CISN
           10 format (A26,1X,A4,I5,I2,I3,&
                &5I3,F5.1,I6,F5.1,I3,&
                &3I5,I6,I5,I6,3I6,1X,A11)
        else
           write (JCVNOH,20) NPLT,MGMID,IY(I),ITHN,ISUM1,ISUM7,ISUM8,&
                &ISUM9,ISUM10,ISUM11,ISUM12,ISUM13,ISUM14,ISUM15,CISN
           20 format (A26,1X,A4,I5,I2,I3,&
                &34X,3I5,I6,I5,I6,3I6,1X,A11)
        end if
        90 continue
     end do
     100 continue
  end do
  return
end
