!================!
!   pg : chput   !
!================!
! PG $Id$
subroutine CHPUT
  implicit none

  !     WRITE THE ALL-DATA CHARACTER DATA TO THE DA FILE.

  !     PART OF THE PARALLEL PROCESSING EXTENSION OF PROGNOSIS SYSTEM.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'CALDEN.f90'
  include 'OPCOM.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'DBSTK.f90'
  include 'GGCOM.f90'
  include 'VOLSTD.f90'

  ! Variables
  integer :: LNCBUF,IRECLN
  parameter (IRECLN = 1024)
  parameter (LNCBUF = IRECLN*4)
  character(len=1) :: CBUFF(LNCBUF)
  character(len=1) :: CDMB
  integer :: K,J,I,IPNT

  !     PUT THE CHARACTERS.

  K = 1
  do J = 1,4
     call CHWRIT(CBUFF,IPNT,LNCBUF,MGMID(J:J),K)
     K = 2
     5 continue
  end do
  do J = 1,26
     call CHWRIT(CBUFF,IPNT,LNCBUF,NPLT(J:J),2)
     10 continue
  end do
  do J = 1,LEN(DBCN)
     call CHWRIT(CBUFF,IPNT,LNCBUF,DBCN(J:J),2)
  end do

  if (LBSETS) then
     if (LENSLS.gt.0) then
        do K = 1,LENSLS
           call CHWRIT (CBUFF,IPNT,LNCBUF,SLSET(K:K),2)
           20 continue
        end do
     end if
     if (IEVA.gt.1) then
        do I = 1,IEVA-1
           J = LENAGL(I)
           if (J.ge.1) then
              do K = 1,J
                 call CHWRIT (CBUFF,IPNT,LNCBUF,AGLSET(I)(K:K),2)
                 30 continue
              end do
           end if
           40 continue
        end do
     end if
  end if

  do J = 1,4
     do I = 1,3
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPTT(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPBR(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPTV(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPMR(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPSR(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPAC(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPCT(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPCV(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPMC(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPSC(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPBV(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPRT(J)(I:I),2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,IOSPMO(J)(I:I),2)
        50 continue
     end do
     60 continue
  end do

  do J = 1,6
     do I = 1,3
        call CHWRIT(CBUFF,IPNT,LNCBUF,IONSP(J)(I:I),2)
        70 continue
     end do
     80 continue
  end do

  do I = 1,6
     call CHWRIT (CBUFF,IPNT,LNCBUF,ALLSUB(I:I),2)
     85 continue
  end do

  if (ITOP.gt.0) then
     do I = 1,ITOP
        call CHWRIT (CBUFF,IPNT,LNCBUF,SUBNAM(I:I),2)
        90 continue
     end do
  end if

  if (ITST5.gt.0) then
     do J = 1,ITST5
        do I = 1,8
           call CHWRIT (CBUFF,IPNT,LNCBUF,CTSTV5(J)(I:I),2)
        end do
     end do
  end if
  if (ICACT.gt.0) then
     do I = 1,ICACT
        call CHWRIT (CBUFF,IPNT,LNCBUF,CACT(I),2)
     end do
  end if

  do J = 1,30
     do I = 1,10
        call CHWRIT(CBUFF,IPNT,LNCBUF,NAMGRP(J)(I:I),2)
        101 continue
     end do
     102 continue
  end do

  do J = 1,MAXSP
     do I = 1,11
        call CHWRIT(CBUFF,IPNT,LNCBUF,VEQNNB(J)(I:I),2)
        103 continue
     end do
     104 continue
  end do

  do J = 1,MAXSP
     do I = 1,11
        call CHWRIT(CBUFF,IPNT,LNCBUF,VEQNNC(J)(I:I),2)
        105 continue
     end do
     106 continue
  end do

  do I = 1,10
     call CHWRIT(CBUFF,IPNT,LNCBUF,CPVREF(I:I),2)
     108 continue
  end do

  do I = 1,72
     call CHWRIT(CBUFF,IPNT,LNCBUF,ITITLE(I:I),2)
     110 continue
  end do

  do J = 1,30
     do I = 1,10
        call CHWRIT(CBUFF,IPNT,LNCBUF,PTGNAME(J)(I:I),2)
        111 continue
     end do
     112 continue
  end do

  do I = 1,2
     call CHWRIT(CBUFF,IPNT,LNCBUF,VARACD(I:I),2)
     114 continue
  end do

  do I = 1,7
     call CHWRIT(CBUFF,IPNT,LNCBUF,CALCSDI(I:I),2)
     115 continue
  end do
  call CHWRIT(CBUFF,IPNT,LNCBUF,BFCTYPE,2)
  call CHWRIT(CBUFF,IPNT,LNCBUF,CFCTYPE,2)

  call DBSCHPUT(CBUFF,IPNT,LNCBUF)
  call VARCHPUT(CBUFF,IPNT,LNCBUF)
  call MSCHPUT(CBUFF,IPNT,LNCBUF)

  !     Store dummy character as last write and finalize character
  !     variable storage, last parameter is 3 to specify.

  CDMB = 'X'
  call CHWRIT(CBUFF,IPNT,LNCBUF,CDMB,3)

  return
end
