!=================================!
!   archive/bgc/src : bentyupdt   !
!=================================!
subroutine BENTYUPDT(FVS_CYC,CV_THIN,FVS_TRN,FVS_DBH,FVS_HT,&
     &FVS_CR,FVS_PROB,FVS_ISP,FVS_JSP,FVS_IDTREE,&
     &CV_CLOW,CV_CMED,CV_CTALL,MAXCY1)

  !  **BENTYUPDT  DATE OF LAST REVISION JULY 2003, AJM.  INSERTING NEW METHOD
  ! FOR CACLULATING BD.  ADDING NEW VARIALBE "A_"; THIS IS THE PARAMETER "a" IN
  ! THE EQUATION FOR A PARABOLA : X^2 = 4ay.  EACH TREE HAS ITS OWN "a".

  !CC              BGC--DATE OF LAST REVISION: 3/13/00

  !     ADDS NEW TREE RECORDS FROM FVS TO THOSE PASSED FROM STAND-BGC.
  !     EXISTING TREE RECORDS ONLY HAVE PROB UPDATED. ONLY
  !     CALLED IF USER HAS SPECIFIED BGC INCREMENTS AND IF AT LEAST ONE
  !     FVS CYCLE HAS BEEN COMPLETED.

  !     CALLED FROM: BGCGROW

  ! Common blocks
  include 'ENTITY.f90'
  include 'SITE.f90'
  ! Variables
  logical :: CV_THIN
  integer :: FVS_ISP(*),FVS_CR(*),FVS_IDTREE(*),FVS_CYC
  integer :: FVS_TRN,MAXCY1
  character(len=4) :: FVS_JSP(*)
  character(len=4) :: SPEC
  dimension FVS_PROB(*),FVS_DBH(*),FVS_HT(*),CV_THIN(*),&
       &CV_CLOW(MAXCY1,2),CV_CMED(MAXCY1,2),CV_CTALL(MAXCY1,2)

  ! LOOK FOR NEW TREE RECORDS AND DEFINE ATTRIBUTES FOR BGC WHEN FOUND

  NX = NLF ! NLF = NO. LIVE PLANTS FROM BGC
  10 do I = 1,FVS_TRN
     do J = 1,NLF
        if (TREENO(J).eq.FVS_IDTREE(I)) then
           TPH(J) = FVS_PROB(I)*2.47
           EXPAND(J) = TPH(J)
           !************troubleshoot ajm 8/29/00
           !        WRITE(76,15)FVS_CYC,FVS_TRN,NLF,TREENO(J),FVS_IDTREE(I),
           !     +                  FVS_PROB(I)
           !   15       FORMAT(5(2X,I4),F6.2)

           goto 50
        end if
        60 continue
     end do
     ! NO MATCH WAS FOUND. DEFINE AND ADD A NEW BGC TREE
     NX = NX+1
     ID(NX) = 'T'
     TREENO(NX) = FVS_IDTREE(I)
     D(NX) = 2.54*FVS_DBH(I)
     H(NX) = FVS_HT(I)/3.28
     TPH(NX) = 2.47*FVS_PROB(I)
     EXPAND(NX) = TPH(NX)
     IK = FVS_ISP(I)
     SPP(NX) = FVS_JSP(IK)
     CR(NX) = FVS_CR(I)/100. ! BGC wants decimal value
     SPEC = SPP(NX)
     DOB = FVS_DBH(I)
     ! FOR SMALL TREES WITH HT<1.3M, SET DBH=0
     if (H(NX).le.1.3) D(NX) = 0.0
     ! CALCULATE BD FOR TREES > 2" FVS_DBH BY ADDING 2*BARK THICKNESS
     if (FVS_DBH(I).ge.2.0) then
        call BARK(SPEC,DOB,BRK)
        BD(NX) = (FVS_DBH(I)+2*BRK)*2.54
     else

        ! COMMENTING OUT THIS METHOD.
        ! NEW METHOD FOR CALCULATION OF BASAL DIAMETER (BD)
        ! 1) ASSUME PARABOLIC SHAPE
        ! 2) KNOWING HT AND DBH, GET PARAMETER "a" IN EQN FOR PARABOLA X^2 = 4aY
        ! 3) ONCE "a" IS KNOWN, WE CAN SOLVE FOR ANY X AT ANY Y (A DIAMETER AT ANY HT)

        !    *****  OLD WAY   *****

        ! CALCULATE FROM FVS_HT FOR TREES WITH FVS_DBH < 2.0". FIRST GET HEIGHT
        ! AT REFERENCE DBH FROM H:D CURVE. ASSUME BD FOR REFERENCE DBH IS
        ! DBH + 2*BRK. STRAIGHT LINE INTERPOLATION.
        !               REFD=2.0
        !               CALL BARK(SPEC,REFD,BRK)
        !               CALL HTDIAM(SPEC,REFD,REFHT)
        !               BD(NX)=(FVS_HT(I)*(REFD+2*BRK)/REFHT)*2.54

        !     *****  NEW WAY  *****  (ASSUMES TREE HAS A DBH!)

        A_(NX) = (FVS_DBH(I)/2.)**2/(4.*(FVS_HT(I)-4.5))
        BD(NX) = (4.*SQRT(A_(I)*FVS_HT(I)))*2.54 !METRIC

     end if
     ! CALCULATE TREE BASAL AREA. USE BD FOR TREES WITH DBH < 5.08cm.
     if (D(NX).lt.5.08) then
        !               DIB=BD(NX)-2*BRK*2.54 ! THIS NOT NEEDED EITHER W/ OR W/O MY NEW CHANGE AJM 7/03
        BA(NX) = ((BD(NX)/1.074)**2.)*0.00007854
     else
        BA(NX) = D(NX)**2. * 0.00007854
     end if

     ! accumulate basal area per hectare
     TOTBA = TOTBA+BA(NX)*TPH(NX)
     ! INCREASE ENTITY COUNT
     NB = NB+1
     !************troubleshoot ajm 8/29/00
     !        WRITE(76,45)FVS_CYC,FVS_TRN,NLF,TREENO(J),FVS_IDTREE(I),
     !     +                  FVS_PROB(I)
     !   45       FORMAT(5(2X,I4),F6.2)

     50 continue
  end do
  return
end
