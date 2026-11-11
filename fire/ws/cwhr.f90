!====================!
!   fire/ws : cwhr   !
!====================!
! FIRE-WS $Id$
subroutine CWHR(ISP, DBH, FVS_HT, FMICR, FMPROB, CWID, FMITRN,&
     &CWXPTS,CCBP,DBHBP,SZDN,CWHR_MOD,CWHR_WT)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'

  !      Public Function CWHRSizeDensity(rst As DAO.Recordset) As String
  !'  rst is a DAO recordset, field names must be trees, Dbh, Ht, CrA

  !      Dim cc As Single, cc1 As Single, cc2 As Single, cc3 As Single,
  !          cc4 As Single, cc5 As Single
  !      Dim ht3 As Single, ht4 As Single, ht5 As Single
  !      Dim tpa3 As Single, tpa4 As Single, tpa5 As Single
  !      Dim basal As Single, dbh As Single, bapct As Single, ltrees
  !          As Single, sumdbhsq As Single, qmd As Single
  !      Dim sng1 As Single
  !      Dim ftpa As Field, fdbh As Field, fht As Field, fcra As Field
  !      Dim fba As Field
  !      Dim size As String, dens As String
  !      Dim ccadj As Single, cc3adj As Single
  !      Dim bDone As Boolean, QmdPctile As Single, targetbasal
  !          As Single, treebasal As Single, sumbasal As Single
  !      Dim ccnetavg As Single

  ! Variables
  character(len=3) :: SZDN
  integer :: FMITRN,ISP,FMICR,IDUM
  real    :: DBH,FVS_HT,FMPROB,CCBP,DBHBP,CWXPTS,XSUM,CWID

  dimension   ISP(FMITRN), FMICR(FMITRN), DBH(FMITRN), CWID(FMITRN)
  dimension   FVS_HT(FMITRN), FMPROB(FMITRN)
  dimension   CWXPTS(4,2), CCBP(3), DBHBP(4)

  integer :: CWHR_MOD(4)
  real    :: CWHR_WT(4),XV(4),YV(4)

  character(len=1) :: SZ
  character(len=1) :: DN
  integer :: I,J,K
  integer :: IXS(MAXTRE)
  real    :: CWIDTH,CAREA,XX
  real    :: QMDPCTILE,BASAL,QMD,TARGETBASAL,TREEBASAL
  real    :: SUMBASAL,SUMDBHSQ,LTREES,SNG1
  real    :: CCNETAVG,CCADJ,PCNETAVG
  real    :: HT(0:5),CC(0:5),TPA(0:5)

  !     VARIABLES FOR CALL TO FMDYN

  integer :: IPTR(4),ITYP(4)
  real    :: EQWT(4)
  integer :: IDANUW

  !     THE INTEGER TAGS ASSOCIATED WITH EACH MODEL
  !     CLASS. THEY ARE RETURNED WITH THE WEIGHT
  !     10=S; 20=P; 30=M; 40=D

  data IPTR / 10,20,30,40 /
  data ITYP /  0, 0, 0, 0 /

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = FMICR(1)
  IDANUW = ISP(1)

  !     INITIALIZE WEIGHTS OF DYNAMIC MODELS

  do I = 1, 4
     EQWT(I) = 0.0
     CWHR_MOD(I) = 0
     CWHR_WT(I) = 0.0
  end do

  !      With rst
  !        Set ftpa = !trees
  !        Set fdbh = !dbh
  !        Set fht = !ht
  !        Set fcra = !cra ! must be crown radius OR AREA??
  !        .MoveFirst
  !        Do Until .EOF
  !          basal = basal + ftpa * fdbh ^ 2 * 0.005454
  !          cc = cc + ftpa * fcra
  !          trees = trees + ftpa
  !          Select Case fdbh
  !            Case Is < 1
  !              cc1 = cc1 + ftpa * fcra
  !            Case Is < 6
  !              cc2 = cc2 + ftpa * fcra
  !            Case Is < 11
  !              cc3 = cc3 + ftpa * fcra
  !              ht3 = ht3 + fht * ftpa
  !              tpa3 = tpa3 + ftpa
  !            Case Is < 24
  !              cc4 = cc4 + ftpa * fcra
  !              ht4 = ht4 + fht * ftpa
  !              tpa4 = tpa4 + ftpa
  !            Case Is >= 24
  !              cc5 = cc5 + ftpa * fcra
  !              ht5 = ht5 + fht * ftpa
  !              tpa5 = tpa5 + ftpa
  !          End Select
  !          MoveNext
  !        Loop
  !      End With

  XX = 0.0
  do I = 0,5
     HT(I) = 0.0
     CC(I) = 0.0
     TPA(I) = 0.0
  end do
  BASAL = 0.0
  do I = 1,FMITRN
     if (FMPROB(I) .gt.0.0) then

        BASAL = BASAL + FMPROB(I) * DBH(I) * DBH(I) * 0.0054542

        CWIDTH = CWID(I)
        CAREA = 3.1415927*CWIDTH*CWIDTH/4.0

        if (DBH(I) .lt. DBHBP(1)) then
           K = 1
        else if (DBH(I) .lt. DBHBP(2)) then
           K = 2
        else if (DBH(I) .lt. DBHBP(3)) then
           K = 3
        else if (DBH(I) .lt. DBHBP(4)) then
           K = 4
        else
           K = 5
        end if

        HT(K) = HT(K) + (FVS_HT(I) * FMPROB(I))
        CC(K) = CC(K) + (CAREA * FMPROB(I))
        TPA(K) = TPA(K) + FMPROB(I)

     end if
  end do

  do I = 1,5
     HT(0) = HT(0)  + HT(I)
     CC(0) = CC(0)  + CC(I)
     TPA(0) = TPA(0) + TPA(I)
  end do
  !'  convert crown cover to percent

  !      cc = cc / 435.6
  !      cc1 = cc1 / 435.6
  !      cc2 = cc2 / 435.6
  !      cc3 = cc3 / 435.6
  !      cc4 = cc4 / 435.6
  !      cc5 = cc5 / 435.6
  !      ccnetavg = pcNetAvg(cc)

  do I = 0,5
     CC(I) = CC(I)/435.60
  end do
  CCNETAVG = PCNETAVG(CC(0))

  !'  divide accumulated ht * tpa by tpa to get avg hts for size 3, 4, and 5

  !      If tpa3 > 0 Then ht3 = ht3 / tpa3
  !      If tpa4 > 0 Then ht4 = ht4 / tpa4
  !      If tpa5 > 0 Then ht5 = ht5 / tpa5

  do I = 3,5
     if (TPA(I) .gt.0.0) HT(I) = HT(I) / TPA(I)
  end do

  !'  Check for tree dominated habitat
  !'  NOTE: pcNetAvg produces a weighted average bewteen uncorrected
  !'  percent cover and percent cover assuming random spacing

  !      ccadj = pcNetAvg(cc)
  !      If ccnetavg < 10 Then
  !        If trees >= 150 Then
  !          CWHRSizeDensity = "1"
  !        Else
  !          CWHRSizeDensity = "XX"     '  designates as non-forest
  !        End If
  !        Exit Function
  !      End If

  CCADJ = PCNETAVG(CC(0))
  if (CCNETAVG .lt.10) then
     if (TPA(0) .ge.150.) then
        SZDN = "1,-"
     else
        SZDN = "X,-"
     end if
     CWHR_MOD(1) = 10
     CWHR_WT(1) = 1.0
     return
  end if

  !      trees = 0      '  reset to use for qmd calculation

  !'  Check for multi-story

  !      If ccnetavg >= 60 And ht5 > 0 And ht4 > 0 And ht3 > 0 Then
  !        If cc5 >= 20 And cc5 <= 80 Then
  !          If cc3 >= 20 And (ht3 / ht5) <= 0.6667 Then
  !            CWHRSizeDensity = "6"
  !            Exit Function
  !          ElseIf cc4 >= 20 And (ht4 / ht5) <= 0.6667 Then
  !            CWHRSizeDensity = "6"
  !            Exit Function
  !          ElseIf cc3 + cc4 >= 30 And (ht3 * tpa3 + ht4 * tpa4)
  !                  / (tpa3 + tpa4) / ht5 <= 0.6667 Then
  !            CWHRSizeDensity = "6"
  !            Exit Function
  !          End If
  !        End If
  !      End If

  if (CCNETAVG .ge. CCBP(3) .and.&
       &HT(5) .gt.0.0.and. HT(4) .gt.0.0.and. HT(3) .gt.0.0) then
     if (CC(5) .ge.20.0.and. CC(5) .le.80.0) then
        if (CC(3) .ge.20.0.and. (HT(3)/HT(5)) .le. (2./3.)) then
           SZDN = "6,-"
           CWHR_MOD(1) = 40
           CWHR_WT(1) = 1.0
           return
        else if (CC(4) .ge.20.0.and.&
             &(HT(4)/HT(5)) .le. (2./3.)) then
           SZDN = "6,-"
           CWHR_MOD(1) = 40
           CWHR_WT(1) = 1.0
           return
        else if ( (CC(3) + CC(4)) .ge.30.0.and.&
             &( ( (HT(3) * TPA(3) + HT(4) * TPA(4)) /&
             &(TPA(3) + TPA(4)) ) / HT(5)) .le. (2./3.)) then
           SZDN = "6,-"
           CWHR_MOD(1) = 40
           CWHR_WT(1) = 1.0
           return
        end if
     end if
  end if

  !'  Not multi-story

  !      If ccadj < 25 Then   '  do the sparse stands
  !                                 by predominance of pct cover

  if (CCADJ .lt. CCBP(1)) then

     !        If cc3 + cc4 + cc5 < cc1 + cc2 Then
     !          If cc2 >= cc1 Then
     !            CWHRSizeDensity = "2S"
     !          Else
     !            CWHRSizeDensity = "1S"
     !          End If
     !        Else
     !          If cc3 > cc4 + cc5 Then
     !            CWHRSizeDensity = "3S"
     !          ElseIf cc5 >= cc4 + cc3 Then
     !            CWHRSizeDensity = "3S"
     !          Else
     !            CWHRSizeDensity = "4S"
     !          End If
     !        End If
     !        Exit Function

     if ((CC(3) + CC(4) + CC(5)) .lt.&
          &(CC(1) + CC(2))) then
        if (CC(2) .ge. CC(1)) then
           SZDN = "2,S"
        else
           SZDN = "1,S"
        end if
     else
        if (CC(3) .gt. (CC(4) + CC(5))) then
           SZDN = "3,S"
        else if (CC(5) .ge. (CC(4) + CC(3))) then
           SZDN = "5,S"
        else
           SZDN = "4,S"
        end if
     end if
     CWHR_MOD(1) = 10
     CWHR_WT(1) = 1.0
     return

     !      Else

  else

     !        If cc4 + cc5 < 10 Then  '  use whole stand qmd for small-tree stands
     !          QmdPctile = 100
     !        End If
     !        targetbasal = basal * QmdPctile
     !        sumbasal = 0: ltrees = 0
     !        bDone = False

     QMDPCTILE = 0.75
     if (CC(4) + CC(5) .lt.10.0) then
        QMDPCTILE = 1.00
     end if
     TARGETBASAL = BASAL * QMDPCTILE
     SUMBASAL = 0.0
     LTREES = 0.0
     SUMDBHSQ = 0.0
     !        bDone = False
     !        With rst
     !          .Sort = "dbh DESC"
     !         .Requery
     !         .MoveFirst
     !          Do Until bDone
     !            treebasal = ftpa * fdbh ^ 2 * 0.005454
     !            If sumbasal + treebasal < targetbasal Then
     !              sumbasal = sumbasal + treebasal
     !              sumdbhsq = sumdbhsq + ftpa * fdbh ^ 2
     !              ltrees = ltrees + ftpa
     !            Else
     !'  how much of this tree record do I need?
     !              sng1 = (targetbasal - sumbasal) / treebasal
     !              ltrees = ltrees + ftpa * sng1
     !              sumdbhsq = sumdbhsq + fdbh ^ 2 * ftpa * sng1
     !              bDone = True
     !            End If
     !            MoveNext
     !            If .EOF Then bDone = True
     !         Loop
     !        End With

     if (FMITRN .gt.0) call RDPSRT(FMITRN,DBH,IXS,.true.)
     do J = 1,FMITRN
        I = IXS(J)
        TREEBASAL = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        if ((SUMBASAL + TREEBASAL) .lt. TARGETBASAL) then
           SUMBASAL = SUMBASAL + TREEBASAL
           SUMDBHSQ = SUMDBHSQ + (FMPROB(I) * DBH(I) * DBH(I))
           LTREES = LTREES + FMPROB(I)
        else
           SNG1 = (TARGETBASAL - SUMBASAL) / TREEBASAL
           LTREES = LTREES + (FMPROB(I) * SNG1)
           SUMDBHSQ = SUMDBHSQ + (DBH(I) * DBH(I) * FMPROB(I) * SNG1)
           goto 100
        end if
     end do
     100 continue

     !        If ltrees > 0 Then
     !          qmd = Sqr(sumdbhsq / ltrees)
     !        Else
     !          qmd = 0
     !        End If

     QMD = 0.0
     if (LTREES .gt.0.0) QMD = SQRT(SUMDBHSQ / LTREES)

     !        Select Case qmd
     !          Case Is < 1
     !            CWHRSizeDensity = "1"
     !            Exit Function
     !          Case Is < 6
     !            size = "2"
     !            ccadj = pcNetAvg(cc)
     !          Case Is < 11
     !            size = "3"
     !            cc = cc2 + cc3 + cc4 + cc5
     !            ccadj = pcNetAvg(cc)
     !'  don't do percent cover correction for medium and large-tree stands
     !          Case Is < 24
     !            size = "4"
     !            cc = cc3 + cc4 + cc5
     !            ccadj = cc
     !          Case Else
     !            size = "5"
     !            cc = cc3 + cc4 + cc5
     !            ccadj = cc
     !        End Select

     if (QMD .lt. DBHBP(1)) then
        SZDN = "1,-"
        CWHR_MOD(1) = 10
        CWHR_WT(1) = 1.0
        return
     else if (QMD .lt. DBHBP(2)) then
        SZ = "2"
        CCADJ = PCNETAVG(CC(0))
     else if (QMD .lt. DBHBP(3)) then
        SZ = "3"
        CC(0) = CC(2) + CC(3) + CC(4) + CC(5)
        CCADJ = PCNETAVG(CC(0))
     else if (QMD .lt. DBHBP(4)) then
        SZ = "4"
        CC(0) = CC(3) + CC(4) + CC(5)
        CCADJ = CC(0)
     else
        SZ = "5"
        CC(0) = CC(3) + CC(4) + CC(5)
        CCADJ = CC(0)
     end if

     !      End If

  end if

  !      Select Case ccadj
  !        Case Is < 25
  !          dens = "S"
  !        Case Is < 40
  !          dens = "P"
  !          If ccnetavg < 25 Then dens = "S"
  !        Case Is < 60
  !          dens = "M"
  !          If ccnetavg < 40 Then dens = "P"
  !        Case Else
  !          dens = "D"
  !          If ccnetavg < 60 Then dens = "M"
  !      End Select

  if (CCADJ .lt. CCBP(1)) then
     DN = "S"
  else if (CCADJ .lt. CCBP(2)) then
     DN = "P"
     if (CCNETAVG .lt. CCBP(1)) DN = "S"
  else if (CCADJ .lt. CCBP(3)) then
     DN = "M"
     if (CCNETAVG .lt. CCBP(2)) DN = "P"
  else
     DN = "D"
     if (CCNETAVG .lt. CCBP(3)) DN = "M"
  end if

  !     CALL FMDYN TO FIND WEIGHTS FOR THE S,M,P,D MODELS
  !     FMDYN PLACES RESULTS IN FWT,FMOD (IN **FMFCOM**) AND IN
  !     THE RETURNED VARIABLE FMD. THESE VALUES ARE OVERWRITTEN
  !     IN A LATER CALL THE FMDYN, SO TEMPORARILY SETTING THEM
  !     HERE IS BENIGN

  do I = 1,4
     EQWT(I) = 1.0
  end do
  call FMDYN(CCADJ,CCNETAVG,ITYP,CWXPTS,EQWT,IPTR,4,.true.,IDUM)
  do I = 1,4
     CWHR_MOD(I) = FMOD(I)
     CWHR_WT(I) = FWT(I)
  end do

  !     IN SOME CIRCUMSTANCES 3 MODELS MAY BE CANDIDATES. MODEL
  !     40 HAS A LOW SLOPE AND IN THE UP/DN/LF/RT SEARCH CAN BE
  !     FOUND 'ABOVE' THE POINT BUT OUTSIDE THE DOMAIN. THE
  !     THIRD MODEL TO BE REMOVED IS ALWAYS OF TYPE 40.
  !     THE REMAINING 2 MODELS ARE PUT IN POSITIONS 1 AND 2

  J = 0
  do I = 1,4
     if (CWHR_MOD(I) .gt.0) J = J + 1
  end do

  if (J .gt.2) then
     do I = 1,4
        if (CWHR_MOD(I) .eq.40) then
           CWHR_MOD(I) = 0
           CWHR_WT(I) = 0.0
        end if
        XV(I) = real(CWHR_MOD(I))
        YV(I) = CWHR_WT(I)
        CWHR_MOD(I) = 0
        CWHR_WT(I) = 0.0
     end do
     call RDPSRT(4,XV,IXS,.true.)
     do I = 1,2
        J = IXS(I)
        CWHR_MOD(I) = INT(XV(J))
        CWHR_WT(I) = YV(J)
     end do
     XSUM = 0.0
     do I = 1,2
        XSUM = XSUM + CWHR_WT(I)
     end do
     do I = 1,2
        CWHR_WT(I) = CWHR_WT(I) / XSUM
     end do
  end if

  !      CWHRSizeDensity = size & dens

  SZDN(1:1) = SZ(1:1)
  SZDN(2:2) = ','
  SZDN(3:3) = DN(1:1)
  return

end

  !      End Function

  !     LOOKS LIKE THIS DOES A CORRECTED % CANOPY COVER

function PCNETAVG(grosspercentcover)

  ! Variables
  real    :: pcRndPack,avg1

  pcRndPack = (1. - 1. / Exp(grosspercentcover / 100.)) * 100.
  avg1 = pcRndPack / 100. * pcRndPack + (100. - pcRndPack) / 100 *&
       &grosspercentcover
  !      If (avg1 > 100.) Then avg1 = 100.
  if (AVG1.gt.100.)AVG1 = 100.
  pcNetAvg = (pcRndPack + avg1) / 2.
  return
end
