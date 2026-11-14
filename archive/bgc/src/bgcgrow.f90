!===============================!
!   archive/bgc/src : bgcgrow   !
!===============================!
subroutine BGCGRO(IY1,IY2)

  !  **BGCGROW  BGC--DATE OF LAST REVISION: 3/13/00
  !    Revised 11/12/02.  Removing index ISTND, and removing PPE common
  !                       "includes" (PPEPRM, PPCNTL, & PRGPRM).  AJM
  !           These changes--also made in BGCFVS, BGCGROW, BGCINT, BGCGO,
  !           BGCIN, and BGCCOM.f77--remove all PPE funtionality.
  !           The FVS-BGC code is now, once again, a single stand model.

  !     RUNS THE BGC EXTENSION

  !     CALLED FROM: GRINCR

  ! Common blocks
  include 'BGCCOM.f90'
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'PDEN.f90'
  include 'OUTCOM.f90'
  include 'ARRAYS.f90'
  include 'CVCOM.f90'
  !      INCLUDE 'PPCNTL.F77'                         ! removed 11/02 ajm

  !      print *, 'in BGCGROW, LBGCON=',LBGCON(ISTND)
  !      IF(.NOT.LBGCON(ISTND)) RETURN                ! removed 11/02 ajm
  if (.not.LBGCON) return

  !  LOAD BGC ENTITY ARRAYS WITH FVS OUTPUT

  !  SKIP CALLS TO BENTYLOAD AND BSITELOAD IF BGC INCREMENTS ARE TO
  !  BE USED AND FVS CYCLE IS GREATER THAN ONE. INSTEAD CALL BENTYUPDT
  !  WHICH WILL ADD NEW TREES FROM REGEN ESTAB MODEL AND UPDATE PROB
  !  IN CASE THINNING HAS OCCURRED.

  !      IF(IBGC(ISTND).EQ.1 .AND. ICYC.GT.1) THEN   ! removed 11/02 ajm
  !        IBCYC(ISTND)=1                            ! ditto
  if (IBGC.eq.1.and. ICYC.gt.1) then
     IBCYC = 1
     call BENTYUPDT(ICYC,LTHIND,ITRN,DBH,HT,ICR,PROB,ISP,JSP,IDTREE,&
          &CLOW,CMED,CTALL,MAXCY1)
  else
     !        IBCYC(ISTND)=0 !removed 11/02 ajm
     IBCYC = 0
     call BENTYLOAD(ICYC,LTHIND,ITRN,DBH,HT,ICR,PROB,ISP,JSP,IDTREE,&
          &CLOW,CMED,CTALL,MAXCY1)
     call BSITELOAD(SLOPE,ASPECT,ELEV,TLAT,KODTYP,KODFOR)
  end if

  !  RUN STAND-BGC

  NUMYRS = IY2-IY1
  call BSTNDBGCN(NUMYRS,ICYC)
  return
end
