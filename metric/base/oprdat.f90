!======================================!
!   Read additional activity records   !
!======================================!
! METRIC-BASE $Id$
subroutine OPRDAT (JEXOPT,KODE)
  implicit none

  !     READS A FILE OF "MORE ACTIVITIES" AND SCHEDULES THEM FOR
  !     THE CURRENT STAND.

  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--FEB 2004

  !     JEXOPT = THE DATA SET REFERENCE NUMBER FOR READING THE FILE
  !              (NOTE THAT THE FILE MUST ALREADY BE OPENED).
  !     KODE   = IF POSITIVE, THE NUMBER OF ACTIVITIES ADDED
  !              IF NEGATIVE, THE NUMBER OF FAILURES TO ADD ACTIVITIES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'OPCOM.f90'
  include 'PLOT.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: MXADDP,IPASS,NADD,I,NPRMS,IACTK,IDT,KODE,JEXOPT,NFAIL
  parameter (MXADDP = N_METRIC_PRMS)
  real    :: PRMS(MXADDP)
  real    :: CPRMS(MXADDP)

  KODE = -999
  if (JEXOPT.le.0) return

  !     PICK UP READING WHERE EVER THE FILE POINTER IS AT THE TIME...

  IPASS = 0
  NADD = 0
  NFAIL = 0
  10 continue
  read (JEXOPT,'(A)',end = 30) WKSTR1
  if (TRIM(WKSTR1).eq.TRIM(NPLT)) then
     15 continue
     read (JEXOPT,'(A)',end = 30) WKSTR1
     if (WKSTR1(1:3).ne.'End') then
        read (WKSTR1,*) IACTK,IDT,NPRMS,(PRMS(I),I = 1,&
             &MIN(NPRMS,MXADDP))
        if (IDT.ge.IY(ICYC)) then
           call MCNVRT(IACTK,1,PRMS,CPRMS)
           do I = 1,NPRMS
              PRMS(I) = PRMS(I) * CPRMS(I)
           end do
           call OPADD (IDT,IACTK,0,NPRMS,PRMS,KODE)
           if (KODE .eq.0) then
              NADD = NADD+1
           else
              NFAIL = NFAIL+1
           end if
        end if
        goto 15
     end if
     if (NADD.gt.0) call OPINCR (IY,ICYC,NCYC)
     if (NFAIL .eq.0) then
        KODE = NADD
     else
        KODE = -NFAIL
     end if
     return
  else
     20 continue
     read (JEXOPT,'(A)',end = 30) WKSTR1
     if (WKSTR1(1:3).ne.'End') goto 20
     goto 10
  end if
  30 continue

  rewind JEXOPT

  !     IF ONLY ON THE FIRST PASS THROUGH THE DATA, TRY AGAIN.

  if (IPASS.eq.1) return
  IPASS = 1
  goto 10
end
