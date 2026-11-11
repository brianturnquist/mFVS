!===============================!
!   metric/fire/base : fmssum   !
!===============================!
! METRIC-FIRE-BASE $Id$
subroutine FMSSUM (IYR)
  implicit none

  !  Purpose:
  !     Reports a summary of snag statistics for all years that
  !     coinside with a FVS cycle boundary.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'PLOT.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: I,II,JOUT,K
  real    :: THD(7),TSF(7),THDSF
  integer :: IYR,DBSKODE

  if (ISNGSM .eq. -1.or. IYR.ne.IFMYR1) return

  do I = 1,7
     THD(I) = 0.
     TSF(I) = 0.
  end do
  THDSF = 0.

  do II = 1,NSNAG
     TSF(7) = TSF(7)+DENIS(II)
     if (HARD(II)) then
        THD(7) = THD(7)+DENIH(II)
     else
        TSF(7) = TSF(7)+DENIH(II)
     end if

     do I = 1,6
        if (DBHS(II).ge.SNPRCL(I)) then
           TSF(I) = TSF(I)+DENIS(II)
           if (HARD(II)) then
              THD(I) = THD(I)+DENIH(II)
           else
              TSF(I) = TSF(I)+DENIH(II) ! adding DENIH is CORRECT
           end if
        end if
     end do
  end do

  do I = 1,7
     TSF(I) = TSF(I) / ACRtoHA
     THD(I) = THD(I) / ACRtoHA
  end do
  THDSF = THD(7) + TSF(7)

  !     CALL THE DBS MODULE TO OUTPUT SUMMARY SNAG REPORT TO A DATABASE

  DBSKODE = 1
  call DBSFMSSNAG(IYR,NPLT,THD(1),THD(2),THD(3),THD(4),THD(5),&
       &THD(6),THD(7),TSF(1),TSF(2),TSF(3),TSF(4),TSF(5),TSF(6),TSF(7),&
       &THDSF,DBSKODE)
  if (DBSKODE.eq.0) return

  call GETLUN (JOUT)

  if (ISNGSM .eq.0) then
     call GETID (ISNGSM)
     write (JOUT,10) ISNGSM,NPLT,MGMID,&
          &((INT(SNPRCL(I)*INtoCM),I = 1,6),K = 1,2)
     10 format (/I6,' $#*%'//,114('-')/&
          &42X,'******  FIRE MODEL VERSION 1.0 ******',/&
          &46('-'),' SNAG SUMMARY REPORT ',&
          &'(BASED ON STOCKABLE AREA) ',21('-')/,&
          &' STAND ID: ',A26,4X,'MGMT ID: ',A4/&
          &7X,15('-'),' HARD SNAGS/HA   ',16('-'),&
          &2X,15('-'),' SOFT SNAGS/HA   ',16('-'),'   GRAND'/&
          &'YEAR ',2(1X,6(' >=',I2.2,'CM'),'  TOTAL'),'   TOTAL'/&
          &'---- ',2(1X,7(' ------')),'  ------'/&
          &'$#*%')
  end if

  write (JOUT,20) ISNGSM,IYR,(THD(I),I = 1,7),(TSF(I),I = 1,7),THDSF
  20 format (1X,I5,1X,I4,1X,2(1X,7(1X,F6.1)),2X,F6.1)

  return
end
