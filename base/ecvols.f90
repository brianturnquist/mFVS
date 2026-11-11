!====================================!
!   Compute economic volume values   !
!====================================!
! BASE $Id$
subroutine ECVOLS
  implicit none

  !  THIS SUBROUTINE IS CALLED TWICE PER CYCLE.
  !  IT CALCULATES VALUES FOR THE CURRENT STAND AND ALSO
  !  FOR REMOVALS WHEN THEY OCCUR.
  !  ENTRY POINTS FOR OTHER ECONOMIC ANALYSIS RELATED SUBROUTINES
  !  ARE ALSO CONTAINED WITHIN THIS SUBROUTINE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ECON.f90'

  ! Variables
  real    :: P,D
  integer :: I,ICLASS,IOAGE,IYEAR,II,ISPC,J,K,IDBH
  character(len=10) :: REV
  logical :: LREMOV,LACTV,LLAST

  !  DIMENSIONS FOR INTERNAL VARIABLES.

  integer*4 :: ICVOLT,ICVOLC,ICVOLB,ICTREE,ICBA
  real    :: CVOLT(MAXSP),CVOLC(MAXSP),CVOLB(MAXSP),CTREE(MAXSP),CBA(MAXSP)

  LLAST = .false.
  LREMOV = .false.

  10 continue
  do I = 1,MAXSP
     CVOLT(I) = 0.0
     CVOLC(I) = 0.0
     CVOLB(I) = 0.0
     CTREE(I) = 0.0
     CBA(I) = 0.0
     100 continue
  end do
  ICLASS = 0
  IOAGE = IAGE+IY(ICYC)-IY(1)
  IYEAR = IY(ICYC)
  if (ITRN .le. 0 ) go to 550
  do II = 1,ITRN
     I = IND(II)
     P = PROB(I)
     if (LREMOV) P = WK3(I)
     if ( P .lt. 0.0 ) P = 0.0
     D = DBH(I)
     IDBH = INT(DBH(I))
     ISPC = ISP(I)
     if (II .eq. 1) goto 400
     if (IDBH .eq. ICLASS) go to 400
     if (LECBUG)write(JOSTND,8000) JOSUME,LECON,ICLASS,LREMOV, &
          & (CVOLT(J),CVOLC(J),CVOLB(J),CTREE(J),CBA(J),J = 1,MAXSP)
     8000 format(' IN **ECVOLS** ', &
          & '  JOSUME=',I3,' LECON=',L1,' ICLASS=',I3,' LREMOV=',L1/ &
          & ((1X,F6.1,2X,F6.1,2X,F6.1,2X,F8.2,2X,F6.2/)))

     !   ROUND VALUES AND OUTPUT FOR CURRENT DBH CLASS

     do K = 1,MAXSP
        if ( CTREE(K) .le. 0.0) go to 200
        ICVOLT = INT((CVOLT(K)/GROSPC)*10.0 + 0.5)
        ICVOLC = INT((CVOLC(K)/GROSPC)*10.0 + 0.5)
        ICVOLB = INT((CVOLB(K)/GROSPC)*10.0 + 0.5)
        ICTREE = INT((CTREE(K)/GROSPC)*100.0 + 0.5)
        ICBA = INT((CBA(K)/GROSPC)*100.0 + 0.5)
        if (LREMOV) then
           write(JOSUME,8002)IYEAR,K,ICLASS,ICVOLT,ICVOLC, &
                & ICVOLB,ICTREE,ICBA,IOAGE,NPLT,MGMID
        else
           write(JOSUME,8001)IYEAR,K,ICLASS,ICVOLT,ICVOLC,ICVOLB, &
                & ICTREE,ICBA,IOAGE,NPLT,MGMID
        end if
        8001 format(I4,1X,I3,2X,I3,2X,I5,2X,I5,2X,I6,2X,I7,2X,I5,2X,'1',2X,I6, &
             & ' 1 ',A26,1X,A4,:,T81,A)
        8002 format(I4,1X,I3,2X,I3,2X,I5,2X,I5,2X,I6,2X,I7,2X,I5,2X,'2',2X,I6, &
             & ' 1 ',A26,1X,A4,:,T81,A)
        200 continue
        CVOLT(K) = 0.0
        CVOLC(K) = 0.0
        CVOLB(K) = 0.0
        CTREE(K) = 0.0
        CBA(K) = 0.0
        300 continue
     end do

     !   SUM WITHIN DBH CLASS

     400 continue
     ICLASS = IDBH
     if (P .le. 0.0) goto 500
     CVOLT(ISPC) = CVOLT(ISPC)+CFV(I)*P
     CVOLC(ISPC) = CVOLC(ISPC)+MCFV(I)*P
     CVOLB(ISPC) = CVOLB(ISPC)+BFV(I)*P
     CTREE(ISPC) = CTREE(ISPC)+P
     CBA(ISPC) = CBA(ISPC)+ P*D*D*.005454
     500 continue
  end do
  if (LECBUG) write(JOSTND,8000) JOSUME,LECON,ICLASS,LREMOV, &
       & (CVOLT(J),CVOLC(J),CVOLB(J),CTREE(J),CBA(J),J = 1,MAXSP)

  !   ROUND AND OUTPUT LAST DBH CLASS

  550 continue
  do K = 1,MAXSP
     if ( CTREE(K) .le. 0.0  .and.  K .lt. MAXSP ) go to 600
     if ( CTREE(K) .le. 0.0  .and.  ITRN .gt. 0 ) go to 600
     ICVOLT = IFIX((CVOLT(K)/GROSPC)*10.0 + 0.5 )
     ICVOLC = IFIX((CVOLC(K)/GROSPC)*10.0 + 0.5 )
     ICVOLB = IFIX((CVOLB(K)/GROSPC)*10.0 + 0.5 )
     ICTREE = IFIX((CTREE(K)/GROSPC)*100.0 + 0.5 )
     ICBA = IFIX((CBA(K)/GROSPC)*100.0 + 0.5 )
     if (LREMOV) then
        write(JOSUME,8002)IYEAR,K,ICLASS,ICVOLT,ICVOLC, &
             & ICVOLB,ICTREE,ICBA,IOAGE,NPLT,MGMID
     else
        write(JOSUME,8001)IYEAR,K,ICLASS,ICVOLT,ICVOLC,ICVOLB, &
             & ICTREE,ICBA,IOAGE,NPLT,MGMID
     end if
     600 continue
  end do

  !   OUTPUT FLAG FOR END OF STAND SUMMARY DATA

  if ( LLAST ) write(JOSUME,8003)
  8003 format('-999')
  return
  entry ECREMS
  LLAST = .false.
  LREMOV = .true.
  goto 10
  entry ECOUT
  return
  entry ECEND
  LLAST = .true.
  LREMOV = .false.
  goto 10
  entry ECACTV (LACTV)
  LACTV = LECON
  return
  entry ECAVAL
  return
  entry ECLBL

  !   OUTPUT FLAG FOR END OF ACTIVITY SUMMARY DATA AND STAND LABELS

  call REVISE (VARACD,REV)
  write(JOSUME,8004) NPLT,MGMID,VARACD,REV
  8004 format('-999'/A26,T10,A4,2(1X,A))
  return
end subroutine ECVOLS
