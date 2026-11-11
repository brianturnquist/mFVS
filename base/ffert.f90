!=========================================!
!   Compute fertilizer growth modifiers   !
!=========================================!
! BASE $Id$
subroutine FFERT
  implicit none

  !  THIS SUBROUTINE COMPUTES A RATIO OF THE FERTILIZATION TREATMENT
  !  TO A CONTROL ON DIAMETER GROWTH, AND USES IT AS A MULTIPLICATIVE
  !  ADJUSTMENT TO THE PREDICTED VALUES OF DDS (CHANGE IN SQUARED
  !  DIAMETER). ALSO, A MULTIPLICATIVE ADJUSTMENT TO THE PREDICTED
  !  HEIGHT GROWTH IS APPLIED USING THE RATIO OF AVERAGE HEIGHT
  !  INCREMENTS OF FERTILIZER TREATMENT TO A CONTROL. THIS SUBROUTINE
  !  IS CALLED BY ** TREGRO **.

  common /FFCOM/ IFFDAT, FFPRMS(4)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: DEBUG
  integer :: MYACTS(1),IFFDAT
  integer :: I,NTODO,NP,IACTK,IDATE,IFSTRT,IFFIN,IFLEN,ISPC,I1,I2,I3
  real    :: HTGIT,RHT,DDSIT,FEFF,DDS,DIB,RDDS,BAL,BRATIO,BARKS,D
  real    :: CDFGF,FFPRMS,DDSYR

  ! RDDS  - DDSGF RATIO OF FERTILIZER TREATMENT TO CONTROL. USED AS A
  !         MULTIPLICATIVE ADJUSTMENT TO THE PREDICTED VALUES OF DDS.
  ! RHT   - HTGF RATIO OF FERTILIZER TREATMENT TO CONTROL. USED AS A
  !         MULTIPLICATIVE ADJUSTMENT TO THE PREDICTED HEIGHT GROWTH
  !         VALUES OF EACH TREE.

  data MYACTS/260/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'FFERT',5,ICYC)

  !     IF THIS IS THE FIRST CYCLE, INITIALIZE IFFDAT AND FFPRMS

  if (ICYC .le. 1) then
     IFFDAT = -1
     do I = 1,4
        FFPRMS(I) = 0.0
        10 continue
     end do
  end if

  !     FIND OUT IF THERE IS A FERTILIZATION THIS TIME PERIOD.

  call OPFIND (1,MYACTS,NTODO)

  !     IF THERE ARE NONE; THEN: BRANCH TO PROCESS CARRY OVER
  !     APPLICATIONS FROM PREVIOUS CYCLE.

  if (NTODO.gt.0) then

     !        GET THE PARAMETERS FOR THE LAST FERTILIZER APPLICATION
     !        SCHEDULED FOR THIS CYCLE, AND DELETE/CANCEL ANY OTHERS.

     call OPGET(NTODO,4,IDATE,IACTK,NP,FFPRMS)
     if (IACTK.lt.0) goto 100
     IFFDAT = IY(ICYC)

     !        SET THE STATUS OF THE OPTION AS ACCOMPLISHED
     !        IN THE BEGINNING OF THE CURRENT CYCLE

     call OPDONE (NTODO,IY(ICYC))

     !        IF THERE ARE OTHER FERTILIZE KEYWORDS FOR THE
     !        CURRENT CYCLE CANCEL/DELETE ALL BUT THE LAST.

     if (NTODO.gt.1) then
        do I = 1,(NTODO-1)
           call OPDEL1 (I)
           15 continue
        end do
     end if
  else
     if (IFFDAT.lt.0) goto 100
  end if

  !     COMPUTE HOW MANY YEARS OF FERTILIZER
  !     NEED TO BE ACCOUNTED FOR THIS CYCLE

  IFSTRT = IY(ICYC) - IFFDAT
  if ( IFSTRT .gt. 10 ) goto 100
  IFFIN = IFSTRT + IFINT
  if (IFFIN .gt. 10) IFFIN = 10
  IFLEN = IFFIN - IFSTRT
  if (IFLEN .le. 0 ) goto 100
  if (IFLEN .ne. 10) then
     write(JOSTND,21) IFLEN,IFINT
     21 format(/' ***** WARNING: FERTILIZER EFFECT IS BEING APPLIED ', &
          & 'FOR ',I2,' OF ',I2,' YEARS.  BIASED ESTIMATES MAY RESULT.')
     call RCDSET (1,.true.)
  end if

  !     AT THIS POINT, FFPRMS(1) IS N FERT.(LB/AC), FFPRMS(2) IS P FERT.
  !     (LB/AC), FFPRMS(3) IS K FERT. (LB/AC), AND FFPRMS(4) IS PROP. OF
  !     EFFICACITY OF FERTIL. APPLICATION.

  !     SEE IF HABITAT TYPE IS OUTSIDE THE RANGE OF THE FERTILIZER
  !     MODEL (HABITAT CODES: 520 AND 530). PRINT A WARNING IF YES.

  if (ICL5 .ne. 520 .and. ICL5 .ne. 530) then
     write(JOSTND,26) ICL5
     26 format(/' ***** WARNING: HABITAT CODE:',I4, &
          & ' IS OUTSIDE THE RANGE OF THE FERTILIZER MODEL.')
     call RCDSET (1,.true.)
  end if

  !     SEE IF SPECIES COMPOSITION IS OUTSIDE THE RANGE OF THE
  !     FERTILIZER MODEL (DF AND GF SPECIES). PRINT A WARNING IF YES.
  !     (THE CONTRIBUTION TO THE STAND CCF OF DF AND GF < 50%)

  CDFGF = (RELDSP(3) + RELDSP(4))/RELDEN
  if (CDFGF .lt. 0.5) then
     write(JOSTND,27)
     27 format(/' ***** WARNING: SPECIES COMPOSITION IS OUTSIDE', &
          & ' THE RANGE OF THE FERTILIZER MODEL.')
     call RCDSET (1,.true.)
  end if

  !     BEGIN SPECIES LOOP.

  do ISPC = 1,MAXSP

     I1 = ISCT(ISPC,1)
     if (I1 .eq. 0) goto 40
     I2 = ISCT(ISPC,2)

     !     BEGIN TREE LOOP WITHIN SPECIES ISPC

     do I3 = I1,I2

        I = IND1(I3)
        D = DBH(I)
        BARKS = BRATIO(ISPC,D,HT(I))
        if (D .le. 0.0) goto 35

        BAL = (1.-(PCT(I)/100.))*BA
        RDDS = EXP(0.1108*ALOG(D)+0.003004*BAL/ALOG(D+1.))
        if (RDDS.gt.2.6) RDDS = 2.6

        DIB = D*BARKS
        DDS = 2*DIB*DG(I) + DG(I)*DG(I)
        FEFF = FFPRMS(4)
        DDSIT = (DDS/YR)*(RDDS*IFLEN*FEFF + IFINT-IFLEN)
        DDSYR = DDSIT * YR/FINT
        DG(I) = SQRT(DIB*DIB + DDSYR) - DIB

        !     COMPUTE AVERAGE HEIGHT GROWTH RATIO OF FERTILIZER
        !     TREATMENT ... CURRENTLY A CONSTANT.

        RHT = 1.1626
        HTGIT = (HTG(I)/YR)*(RHT*IFLEN*FEFF + IFINT-IFLEN)
        HTG(I) = HTGIT * YR/FINT

        !     END OF TREE LOOP. PRINT DEBUGING INFORMATION IF DESIRED

        if (DEBUG) then
           write (JOSTND,38) I,ISPC,D,DG(I),HTG(I),RDDS
           38 format(' IN FFERT I=', I4,', ISPC=',I3,', DBH=',F7.2, &
                & ', DG(I)=',F7.4,', HTG(I)=',F7.4,', RDDS=',F7.2)
        end if

        35 continue
     end do
     40 continue
  end do

  100 continue
  return
end subroutine FFERT
