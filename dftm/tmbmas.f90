!===================!
!   dftm : tmbmas   !
!===================!
! DFTM $Id$
subroutine TMBMAS
  implicit none

  !     CALCULATES BIOMASS  OF THE NOMINAL BRANCHES

  !     NOTE: TMBMAS IS CALLED BEFORE DGDRIV SIMPLY BECAUSE OPTION 2
  !     REQUIRES THE PAST 10 YR GROWTH INCREMENT. IF OPTION 2 IS
  !     ELIMINATED (AS IT SHOULD BE), THIS ROUTINE MAY BE CALLED FROM
  !     TMCOUP.  ALSO NOTE THAT FBIOMS AND PCNEWF ARE BOTH RECORD TRIPLED
  !     HERE...IF THE ROUTINE CALL IS MOVED TO TMCOUP, THE RECORD
  !     TRIPLING CODE EMBEDED HERE WILL HAVE TO BE REMOVED.

  !     N.L. CROOKSTON NOV 1980.

  !  REVISION HISTORY:
  !    01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'TMCOM1.f90'
  include 'PLOT.f90'
  include 'BIOMAS.f90'

  ! Variables
  integer :: IPT(MAXTRE)
  integer :: I,IS,IK,J,I1,I2
  real    :: CR,DGI,FERROR,PERROR,TMBCHL,SCALDG

  equivalence (IPT(1), WK6(1))

  !     IF IBMTYP IS 1 3 OR 4 BRANCH TO STM 900; ELSE: USE CODE BELOW.

  if (IBMTYP .ne.2) go to 900

  !     COMPUTE THE SCALING FACTOR OF DG...

  SCALDG = ICL6
  if (ICYC .gt.1) SCALDG = IY(ICYC) - IY(ICYC-1)
  SCALDG = 10.0 / SCALDG

  !     EQUATIONS FROM C. HATCH AND P. MIKA, UN. OF IDAHO, COL. OF
  !     FORESTRY.  SEE: 'FOLIAGE BIOMASS ESTIMATES FOR DOUGLAS-FIR,
  !     GRAND FIR AND WHITE FIR FROM SELECTED DFTM OUTBREAK AREAS IN
  !     THE WESTERN U. S.'  FINAL REPORT SUBMITTED TO THE DFTM
  !     PROGRAM FEB. 8, 1978.
  !     ( USED IF IBMTYP = 2 )

  if (TMDTRE) write (JODFTM,610) ITRN
  610 format(//' DUMP FROM TMBMAS:  ITRN=',I4//&
       &'    I   IS  SLOPE    ASPECT        BA       TPROB  RELDEN',&
       &'       DBH        HT        DG        CR       PCT    FBIOMS',&
       &'    PCNEWF ISP'/)

  if (.not. LDF) goto 15

  !     CALCULATE BRANCH BIOMASS FOR DOUGLAS FIR.

  I1 = ISCT(IDFCOD,1)
  I2 = ISCT(IDFCOD,2)

  do I = I1,I2
     IS = IND1(I)
     IK = IS
     J = -2
     CR = ICR(IK) / 100.0
     DGI = DG(IK) * SCALDG

     8 continue
     FBIOMS(IS) = 195.20482 -&
          &249.78151 * SLOPE -&
          &99.36800  * SLOPE * COS(ASPECT) -&
          &129.92132 * SLOPE * SIN(ASPECT) +&
          &22.05280  * DBH(IK) -&
          &2.14018   * HT(IK) +&
          &79.43490  * CR -&
          &0.62932   * BA

     if (FBIOMS(IS) .gt.400.0) FBIOMS(IS) = 400.0
     if (FBIOMS(IS) .lt.91.0) FBIOMS(IS) = 91.0

     PCNEWF(IS) = 0.49738 +&
          &0.080570  * SLOPE +&
          &0.27017   * SLOPE * COS(ASPECT) +&
          &0.32162   * SLOPE * SIN(ASPECT) +&
          &0.039360  * ALOG(PCT(IK) * (0.31830989 *&
          &ATAN(((RELDEN / 100) - 1.5) / 1.7) + 0.5)) -&
          &0.0043258 * HT(IK) +&
          &0.077044  * DGI -&
          &0.078413  * TPROB / 100 +&
          &0.0048268 * (TPROB**2 / 10000)

     PCNEWF(IS) = PCNEWF(IS) * 100.0

     if (PCNEWF(IS) .gt.42.0) PCNEWF(IS) = 42.0
     if (PCNEWF(IS) .lt.11.0) PCNEWF(IS) = 11.0

     if (TMDTRE) write(JODFTM,620) I, IS, SLOPE, ASPECT, BA, TPROB,&
          &RELDEN, DBH(IK), HT(IK), DG(IK), CR, PCT(IK), FBIOMS(IS),&
          &PCNEWF(IS), ISP(IK)
     620 format(2I5,F7.3,11F10.3,I3 )

     !     RECORD TRIPLES:

     if (.not. LTRIP) goto 10
     J = J + 1
     if (J .gt.0) goto 10
     IS = ITRN + 2 * IK + J
     goto 8

     10 continue
  end do
  15 continue
  if (.not. LGF) goto 25

  !     CALCULATE BRANCH BIOMASS FOR GRAND FIR.

  I1 = ISCT(IGFCOD,1)
  I2 = ISCT(IGFCOD,2)

  do I = I1,I2
     IS = IND1(I)
     IK = IS
     J = -2
     CR = ICR(IK) / 100.0
     DGI = DG(IK) * SCALDG

     18 continue
     FBIOMS(IS) = EXP(4.70244 +&
          &0.15833  * DBH(IK) -&
          &0.014290 * HT(IK) +&
          &0.17778  * DGI)

     if (FBIOMS(IS) .gt.400.0) FBIOMS(IS) = 400.0
     if (FBIOMS(IS) .lt.125.0) FBIOMS(IS) = 125.0

     PCNEWF(IS) = 0.12690 -&
          &0.017636  * ALOG(PCT(IK) * (0.31830989 *&
          &ATAN(((RELDEN / 100) - 1.5) / 1.7) + 0.5)) +&
          &0.037998  * DBH(IK) -&
          &0.0062929 * HT(IK) +&
          &0.49115   * CR -&
          &0.59622   * SLOPE

     PCNEWF(IS) = PCNEWF(IS) * 100.0
     if (PCNEWF(IS) .gt.47.0) PCNEWF(IS) = 47.0
     if (PCNEWF(IS) .lt.15.0) PCNEWF(IS) = 15.0

     if (TMDTRE) write(JODFTM,620) I, IS, SLOPE, ASPECT, BA, TPROB,&
          &RELDEN, DBH(IK), HT(IK), DG(IK), CR, PCT(IK), FBIOMS(IS),&
          &PCNEWF(IS), ISP(IK)

     !       RECORD TRIPLES:

     if (.not. LTRIP) goto 20
     J = J + 1
     if (J .gt.0) goto 20
     IS = ITRN + 2 * IK + J
     goto 18
     20 continue
  end do

  25 continue

  return

  900 continue

  !     IBMTYP   METHOD

  !       1      RAMDOMLY ASSIGN THE FOLIAGE BIOMASS AND %NEW
  !       3      USE HATCH'S EQUATIONS TO PREDICT BIOMASS AS A
  !              FUNCTION OF BASAL AREA PERCENTILE.
  !       4      LIKE 3, EXCEPT THAT A RANDOM ERROR IS ADDED

  if (TMDTRE) write(JODFTM,630) IBMTYP, ITRN
  630 format(//' DUMP FROM TMBMAS(IBMTYP=',I4,';ITRN=',I4,'):'//&
       &'    I   IS  ISP       DBH       PCT    FBIOMS    FERROR',&
       &'    PCNEWF    PERROR'/ )

  !     SINCE RANDOM PROCESSES ARE USED TO ASSIGN FOLIAGE BIOMASS,
  !     THE RESULTS OF THE CALCULATIONS ARE DEPENDENT UPON THE ORDER
  !     OF THE TREE RECORDS.  TO KEEP RESULTS INDEPENDENT OF ORDER THE
  !     TREE RECORDS NEED TO BE SORTED ON A TREE ATTRIBUTE SUCH AS DBH.
  !     THIS IS A 2 STEP PROCESS:
  !     1 - LOAD IND1 INTO IPT
  !     2 - SORT IPT IN ORDER OF DBH BY SPECIES. THIS REQUIRES
  !         TWO CALLS TO RDPSRT.

  do I = 1,ITRN
     IPT(I) = IND1(I)
     26 continue
  end do

  if (IBMTYP .ne.4) goto 27
  I1 = ISCT(IDFCOD,1)
  if (LDF) call RDPSRT (ISCT(IDFCOD,2)-I1+1, DBH, IPT(I1), .false.)
  I1 = ISCT(IGFCOD,1)
  if (LGF) call RDPSRT (ISCT(IGFCOD,2)-I1+1, DBH, IPT(I1), .false.)

  27 continue
  FERROR = 0.0
  PERROR = 0.0

  if (.not. LDF) goto 35

  !     CALCULATE BRANCH BIOMASS FOR DOUGLAS FIR.

  I1 = ISCT(IDFCOD,1)
  I2 = ISCT(IDFCOD,2)
  if (IBMTYP .eq.1) goto 31

  do I = I1,I2
     IS = IPT(I)
     IK = IS
     J = -2

     28 continue

     if (IBMTYP .eq.4) FERROR = TMBCHL(0.0, 56.885)
     if (IBMTYP .eq.4) PERROR = TMBCHL(0.0, 10.970)

     FBIOMS(IS) = 154.3661 + 1.3706 * PCT(IK) + FERROR
     PCNEWF(IS) = 100.0 * (0.168298 + 0.002634 * PCT(IK)) + PERROR

     if (FBIOMS(IS) .gt.400.0) FBIOMS(IS) = 400.0
     if (FBIOMS(IS) .lt.91.0) FBIOMS(IS) = 91.0
     if (PCNEWF(IS) .gt.42.0) PCNEWF(IS) = 42.0
     if (PCNEWF(IS) .lt.11.0) PCNEWF(IS) = 11.0

     if (TMDTRE) write (JODFTM,640) I, IS, ISP(IK), DBH(IK),&
          &PCT(IK), FBIOMS(IS), FERROR, PCNEWF(IS), PERROR
     640 format (3I5,6F10.3)

     !       RECORD TRIPLES

     if (.not. LTRIP) goto 30
     J = J + 1
     if (J .gt.0) goto 30
     IS = ITRN + 2 * IK + J
     goto 28

     30 continue
  end do
  goto 35

  31 continue
  do I = I1,I2
     IS = IPT(I)
     IK = IS
     J = -2

     32 continue
     PCNEWF(IS) = TMBCHL(DFPNEW(1), DFPNEW(2))
     FBIOMS(IS) = TMBCHL(DFFBIO(1), DFFBIO(2))
     if (TMDTRE) write (JODFTM,640) I, IS, ISP(IK), DBH(IK),&
          &PCT(IK), FBIOMS(IS), FERROR, PCNEWF(IS), PERROR

     !       RECORD TRIPLES

     if (.not. LTRIP) goto 33
     J = J + 1
     if (J .gt.0) goto 33
     IS = ITRN + 2 * IK + J
     goto 32
     33 continue
  end do

  35 continue
  if (.not. LGF) goto 45

  !     CALCULATE BRANCH BIOMASS FOR GRAND FIR.

  I1 = ISCT(IGFCOD,1)
  I2 = ISCT(IGFCOD,2)
  if (IBMTYP .eq.1) goto 41

  do I = I1,I2
     IS = IPT(I)
     IK = IS
     J = -2
     38 continue

     if (IBMTYP .eq.4) FERROR = TMBCHL(0.0, 57.590)
     if (IBMTYP .eq.4) PERROR = TMBCHL(0.0, 7.229)

     FBIOMS(IS) = 174.7162  + 1.3794 * PCT(IK) + FERROR
     PCNEWF(IS) = 100.0 * (0.324830 + 0.000723 * PCT(IK)) + PERROR

     if (FBIOMS(IS) .gt.400.0) FBIOMS(IS) = 400.0
     if (FBIOMS(IS) .lt.125.0) FBIOMS(IS) = 125.0
     if (PCNEWF(IS) .gt.47.0) PCNEWF(IS) = 47.0
     if (PCNEWF(IS) .lt.15.0) PCNEWF(IS) = 15.0

     if (TMDTRE) write (JODFTM,640) I, IS, ISP(IK), DBH(IK),&
          &PCT(IK), FBIOMS(IS), FERROR, PCNEWF(IS), PERROR

     !       RECORD TRIPLES

     if (.not. LTRIP) goto 40
     J = J + 1
     if (J .gt.0) goto 40
     IS = ITRN + 2 * IK + J
     goto 38
     40 continue
  end do
  goto 45

  41 continue
  do I = I1,I2
     IS = IPT(I)
     IK = IS
     J = -2

     42 continue
     PCNEWF(IS) = TMBCHL(GFPNEW(1), GFPNEW(2))
     FBIOMS(IS) = TMBCHL(GFFBIO(1), GFFBIO(2))
     if (TMDTRE) write (JODFTM,640) I, IS, ISP(IK), DBH(IK),&
          &PCT(IK), FBIOMS(IS), FERROR, PCNEWF(IS), PERROR

     !       RECORD TRIPLES

     if (.not. LTRIP) goto 43
     J = J + 1
     if (J .gt.0) goto 43
     IS = ITRN + 2 * IK + J
     goto 42
     43 continue
  end do

  45 continue

  return
end
