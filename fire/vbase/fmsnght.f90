!==========================!
!   fire/vbase : fmsnght   !
!==========================!
! FIRE-VBASE $Id$
subroutine FMSNGHT(VAR,KSP,HTD,HTCURR,IHRD,HTSNEW)
  implicit none

  !     SNAG HEIGHT PREDICTION

  !   Purpose:
  !     This routine predicts snag height loss due to top breakage.

  !     The base logic in this routine was extracted from its original
  !     location in FMSNAG, in order to structure/generalize the logic
  !     for use with both the FFE snag pools, and the base FVS model
  !     snag records.

  !   Called from: FMSNAG to compute height loss for a given FFE snag pool.
  !                SVSNAGE to compute height loss for a given FVS snag record.

  !   Local variable definitions:
  !     IHRD:    Indicates if current height loss prediction is for a hard
  !              or soft snag. Used to reference appropriate HTX array elements.
  !     HTCURR:  Current height of snag pool/record.
  !     HTSNEW:  Updated height for snag pool/record.
  !     HTD:     Height of current snag pool/record, at time of death.
  !     KSP:     Species number for current snag pool/record.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'PLOT.f90'

  ! Variables
  character(len=2) :: VAR
  integer :: HTINDX1,HTINDX2,IHRD,KSP
  real    :: HTCURR,HTD,HTSNEW,SFTMULT,X2

  !  Set array indices for retrieving hard/soft height loss coefficients.

  if ( IHRD .eq.1 ) then
     HTINDX1 = 1
     HTINDX2 = 2
     SFTMULT = 1.0
  else
     HTINDX1 = 3
     HTINDX2 = 4
     SFTMULT = HTXSFT
  end if

  !  Decrease the height of snags according to current height and
  !  whether they were initially hard or soft. If height gets to
  !  be less than 1.5 foot, call it 0 (the 'snag' is considered to be
  !  'fuel' now instead).

  select case (VAR)
  case('CI')
     !       CI variant has special rules for WP & RC

     select case (KSP)
     case (1,6) ! WP, RC STOP LOSING HEIGHT AT 75%
        if (HTCURR .gt. (0.75 * HTD)) then
           HTSNEW = HTCURR * (1.0 - HTR1 * HTX(KSP,HTINDX1)&
                &* SFTMULT)**NYRS
        else
           HTSNEW = HTCURR
        end if
     case default
        if (HTCURR .gt. (0.5 * HTD)) then
           HTSNEW = HTCURR * (1.0 - HTR1 * HTX(KSP,HTINDX1)&
                &* SFTMULT)**NYRS
        else
           HTSNEW = HTCURR * (1.0 - HTR2 * HTX(KSP,HTINDX2)&
                &* SFTMULT)**NYRS
        end if
     end select

  case('PN', 'WC', 'BM', 'EC', 'OP')
     !       First, get the height loss rate from fmr6htls. But if the
     !       height loss is adjusted by user (snagbrk keyword), make sure
     !       you use their values.

     call FMR6HTLS(KSP,X2)
     if (HTCURR .gt. (0.5 * HTD)) then
        if ((HTX(KSP,HTINDX1) .gt.1.01) .or.&
             &(HTX(KSP,HTINDX1) .lt.0.99)) then
           HTSNEW = HTCURR *&
                &(1.0 - HTR1 * HTX(KSP,HTINDX1)*SFTMULT)**NYRS
        else
           HTSNEW = HTCURR * (1.0 - X2)**NYRS
        end if
     else
        if ((HTX(KSP,HTINDX2) .gt.1.01) .or.&
             &(HTX(KSP,HTINDX2) .lt.0.99)) then
           HTSNEW = HTCURR *&
                &(1.0 - HTR2 * HTX(KSP,HTINDX2)*SFTMULT)**NYRS
        else
           HTSNEW = HTCURR * (1.0 - X2)**NYRS
        end if
     end if

  case('SO')
     select case (KODFOR)
     case(505,506,509,511,701,514) ! CALIFORNIA

        if (HTCURR .gt. (0.5 * HTD)) then
           HTSNEW = HTCURR *&
                &(1.0 - HTR1 * HTX(KSP,HTINDX1) *SFTMULT)**NYRS
        else
           HTSNEW = HTCURR *&
                &(1.0 - HTR2 * HTX(KSP,HTINDX2) *SFTMULT)**NYRS
        end if

     case default ! OREGON

        !         First, get the height loss rate from fmr6htls. But if the
        !         height loss is adjusted by user (snagbrk keyword), make sure
        !         you use their values.

        call FMR6HTLS(KSP,X2)
        if (HTCURR .gt. (0.5 * HTD)) then
           if ((HTX(KSP,HTINDX1) .gt.1.01) .or.&
                &(HTX(KSP,HTINDX1) .lt.0.99)) then
              HTSNEW = HTCURR *&
                   &(1.0 - HTR1 *HTX(KSP,HTINDX1)*SFTMULT)**NYRS
           else
              HTSNEW = HTCURR * (1.0 - X2)**NYRS
           end if
        else
           if ((HTX(KSP,HTINDX2) .gt.1.01) .or.&
                &(HTX(KSP,HTINDX2) .lt.0.99)) then
              HTSNEW = HTCURR *&
                   &(1.0 - HTR2 *HTX(KSP,HTINDX2)*SFTMULT)**NYRS
           else
              HTSNEW = HTCURR * (1.0 - X2)**NYRS
           end if
        end if
     end select

  case default
     if (HTCURR .gt. (0.5 * HTD)) then
        HTSNEW = HTCURR *&
             &(1.0 - HTR1 * HTX(KSP,HTINDX1) * SFTMULT)**NYRS
     else
        HTSNEW = HTCURR *&
             &(1.0 - HTR2 * HTX(KSP,HTINDX2) * SFTMULT)**NYRS
     end if

  end select

  if (HTSNEW .lt.1.5) HTSNEW = 0.0

  return
end
