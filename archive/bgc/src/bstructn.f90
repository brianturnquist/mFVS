!================================!
!   archive/bgc/src : bstructn   !
!================================!
subroutine BSTRUCTN (FVS_CYC)
  !  Amended 6/30/03.  adding call to new subroutine.  THe new subroutine--
  !  "REDOLAI" recalculates the distribution of Leaf area and leaf biomass.
  !  Heretofore, they were solely a function of bole volume, no crown info was
  !  used.  New routine prorates leaf area and biomass as by crown volume.

  ! Common blocks
  include 'ENTITY.f90'
  include 'SITE.f90'

  ! Variables
  integer :: FVS_CYC

  call CROWNCF
  call CROWNHT
  call GEOMETRY
  if (S(7).eq.0.) then !individual tree crown zones
     call ZONES

     !  CALL THE IDSORT SUBROUTINE
     !  SORTS AN INDEX OF ZONE BOUNDARIES, DEFINED IN SUBROUTINE 'ZONE', IN
     !  ASCENDING ORDER. IF I1=INDEX(L) THEN ZONE(I1) IS THE HEIGHT FROM GROUND TO
     !  ZONE BOUNDARY.--NOTE: THIS WAS MOVED 12/93, DWC.

     call IDSORT(NB*2,ZONE,INDEX,.true.)
  else
     call STRATA
     call ZONES2
     call IDSORT(NUM2,ZONE,INDEX,.true.)
  end if

  !  New subroutine: recalculate leaf area and biomass distribution among
  !  entities ONLY in year 1, cycle 1.  added 6/30/03 ajm.
  !  !!NOTE!! I am temporarily "co-opting" BETA.dat parameter B2(9)--currently
  !  it not used--as a "flag" variable, so that one can "turn on" or "turn off"
  !  this logic via the BETA.dat file.  Set B2(9) = to 1 to invoke this logic.
  ! Another change 7/23 added third way to do this, flag(via B2(9)) with "-3"

  if (FVS_CYC.eq.1.and.YRFLAG.eq.1.and.&
       &(B2(9).eq.1.0.or. B2(9) .eq. -1.0.or. B2(9) .eq.-3))&
       &call REDOLAI

  call LAIJ
  return
end

subroutine CROWNCF

  !   THIS SUBROUTINE CALCULATES THE CROWN COMPETITION FACTOR
  !   FOR THE STAND BASED ON PROGNOSIS MODEL (see p.49-51 in manual).
  !!!!NOTE: CCF IS IN ENGLISH UNITS FOR PROGNOSIS.
  !      -- DWC, 1/24/94.
  !!!! CCF FOR SMALL TREES (<1.3m) USES BASAL DIAMETER IN CALCULATION
  !!!! modified 3/2/96 KSM

  ! Variables
  integer :: L
  real    :: A1(11),A2(11),A3(11)
  real    :: C1(11),C2(11)
  ! Common blocks
  include 'ENTITY.f90'

  ! LOAD COEFFICIENTS FOR CCF EQUATIONS FOR FOLLOWING SPECIES
  !                 PP      DF      WL      ES      LP      AF
  !                 GF      BP      WP      RC      WH
  data A1 /.03, .11, .02, .03, .02, .03,&
       &.04, .03, .03, .03, .03/
  data A2 /.0180, .0333, .0148, .0173, .0168, .0216,&
       &.0270, .0167, .0167, .0238, .0215/
  data A3 /.00281, .00259, .00338, .00259, .00325, .00405,&
       &.00405, .00230, .00230, .00490, .00363/
  data C1 /.00781, .01730, .00724, .00788, .00919, .01140,&
       &.01525, .00988, .00988, .00892, .01111/
  data C2 /1.7680, 1.5571, 1.8182, 1.7360, 1.7600, 1.7560,&
       &1.7333, 1.6667, 1.6667, 1.7800, 1.7250/

  do I = 1,NB
     L = BGCISP(I)
     if (ID(I).eq.'T') then
        if (D(I).ge.25.4) then !if dbh >= 10 inches
           CCF(I) = (EXPAND(I)/2.47) * (A1(L) + A2(L) * (D(I)/2.54) +&
                &A3(L) * (D(I)/2.54)**2.)
        else if (D(I).lt.25.4.and.D(I).gt.0.0) then !if 0<dbh<10 in.
           CCF(I) = (EXPAND(I)/2.47) * C1(L) * (D(I)/2.54)**C2(L)
           ! for small trees use BD(I) which is in cm.
        else if (D(I).eq.0.0) then !if dbh=0.0 in.
           CCF(I) = (EXPAND(I)/2.47) * C1(L) * (BD(I)/2.54)**C2(L)
        end if
        SUMCCF = SUMCCF + CCF(I)
        !          WRITE(*,*) I, BD(I), D(I), CCF(I), SUMCCF
     end if
     10 continue
  end do
  return
end

subroutine CROWNHT

  !  CALCULATES THE CROWN TOP AND CROWN BOTTOM BY ENTITY.

  ! Common blocks
  include 'ENTITY.f90'

  MAXCRHT = 0.
  MINCRHT = 999.
  do I = 1,NB
     if (ID(I).eq.'T') then
        CTOP(I) = H(I)
        CBOT(I) = H(I) - ( H(I)*CR(I) )
     else if (ID(I).eq.'S') then
        CTOP(I) = H(I)
        CBOT(I) = 0.
     else if (ID(I).eq.'G') then
        CTOP(I) = H(I)
        CBOT(I) = 0.0
     end if
     MAXCRHT = MAX(MAXCRHT,CTOP(I))
     MINCRHT = MIN(MINCRHT,CBOT(I))
     !          WRITE(72,*) 'I= ',I,' ID= ',ID(I),' CR= ',CR(I),
     !     + ' CROWN TOP= ',CTOP(I),'  CROWN BOT= ',CBOT(I),' HT =',H(I)
     10 continue
  end do
  return
end

subroutine GEOMETRY

  !     FOR INDIVIDUAL CONIFER ENTITIES ASSUME RIGHT CIRCULAR CONICAL CROWNS
  !     WITH MAXIMUM WIDTH AT BASE OF CROWN. FOR SHRUB AND GRASS ENTITIES
  !     ASSUME A CYLINDER. RETURN CROWN WIDTH AND BETA. BETA IS ANGLE AT
  !     VERTEX OF CONE FOR CONIFERS; RADIUS OF CIRCLE OF 1ha AREA FOR GRASS
  !     AND SHRUBS.  SUBROUTINE CALLED AS EACH TREE IS READ IN, OR AS TREES
  !     ARE BEING UPDATED.

  ! Variables
  real    :: BT1(11),BT3(11),ST1(11),CL
  ! Common blocks
  include 'ENTITY.f90'

  ! LOAD COEFFICIENTS FOR CROWN WIDTH EQUATIONS FOR FOLLOWING SPECIES
  !                 PP      DF      WL      ES      LP      AF
  !                 GF      BP      WP      RC      WH
  data BT1 /1.62635,3.02271,2.31359,3.76535,1.06804,1.74558,&
       &2.20611,-0.91984,4.308,2.79784,1.32772/
  data BT3 /-0.68098,-1.00486,-0.80919,-1.18257,-0.55987,-0.73972&
       &,-0.76936,-0.07299,-1.37265,-0.89666,-0.52554/
  data ST1 /0.3638,0.32874,0.23846,0.33089,0.26342,0.33722,&
       &0.38503,0.07049,0.37031,0.46452,0.25622/

  ! DEFINE CROWN WIDTH FUNCTIONS FOR LARGE AND SMALL TREES.
  ! MELINDA MOEUR. 1981.

  ! XCWBT IS FOR LARGE TREES
  XCWBT(X,Y,Z,K) = EXP( BT1(K) + 1.08137*LOG(X/2.54) +&
       &BT3(K)*LOG(Y*3.28084) +&
       &0.29786*LOG(Z*3.28084) )

  ! XCWST IS FOR SMALL TREES
  XCWST(X,Y,Z,K) = EXP( ST1(K)*LOG(X*3.28084) +&
       &0.28283*LOG(Y*3.28084) +&
       &0.04032*LOG(Z*10.76391) )

  !     calculate canopy thickness (crown length) for all entities
  do I = 1,NB
     CL = CTOP(I) - CBOT(I)
     if (ID(I).eq.'T') then
        !     do geometry for trees
        if (D(I).gt.8.89) then
           !           calculate crown width for Big Trees
           CW(I) = XCWBT(D(I), CTOP(I), CL, BGCISP(I)) / 3.28084
        else
           !           calculate crown width for Small Trees
           CW(I) = XCWST(CTOP(I), CL, TOTBA, BGCISP(I)) / 3.28084
           !              WRITE(*,*) 'SMALL TREE CW ',I,CL,TPH(I),CW(I)
        end if

        ! Adjust CW to reflect perceived bias in Melinda's equations.

        !         CW(I)=2.0*CW(I)

        !        calculate the slope of the line defining the vertex angle
        !        of a cone for trees
        BETA(I) = ( 0.5*CW(I) ) / CL
        !           BETA(I)=.5
     else
        !        do geometry for grass and shrub entities
        !        calculate the radius of circle with area=1ha-bare ground
        BETA(I) = ( (AREA-BARE) / 3.14159 )**0.5
     end if
     10 continue
  end do
  return
end

subroutine ZONES

  !  CALCULATES ZONE BOUNDARY. A ZONE BOUNDARY IS
  !  LOCATED AT EACH ENTITIES CANOPY TOP AND BOTTOM.

  ! Common blocks
  include 'ENTITY.f90'

  do I = 1,NB
     ZONE(I) = CTOP(I)
     ZONE(I+NB) = CBOT(I)
     10 continue
  end do
  NUM = 2*NB-1
  NUM2 = 2*NB
  return
end

subroutine LAIJ

  !     LAIJ RETURNS LEAF AREA INDEX FOR EACH ENTITY BY CANOPY ZONE

  ! Variables
  integer :: MORE,J,I1,I2
  real    :: A,B,RAT,TOTVOL,FRUSTVOL,TIP,BOT
  ! Common blocks
  include 'ENTITY.f90'

  !  XCONE IS GENERAL VOLUME FUNCTION FOR A CONE OR FRUSTUM OF A CONE
  !  THAT USES THE ANGLE, BETA, OF THE CONE VERTEX AND TWO
  !  DISTANCES, A & B, FROM THE CONE TIP.

  !  coefficient on X was corrected from 3 to 2 3/16/96. KSM.
  XCONE(X,Y,Z) = ((3.1415*X**2.)/3.)*(Z**3.-Y**3.)

  !  XCYL IS A FUNCTION FOR VOLUME OF A CYLINDER OR FRUSTUM THEREOF.
  !  USES THE RADIUS, BETA, OF THE CYLINDER AND TWO DISTANCES, A & B,
  !  FROM THE TOP OF THE CONE.

  XCYL(X,Y,Z) = (3.1415*X**2.)*(Z-Y)

  !  XBASE CALCULATES THE AREA OF A CROSS SECTION OF A RIGHT CIRCULAR CONE
  !  FROM THE VERTEX ANGLE, BETA, AND THE DISTANCE FROM THE CONE TIP

  XBASE(X,Y) = (((Y*X)*1.)**2.)*3.1415

  !     CALCULATE, FOR EACH ENTITY, THE DISTANCE FROM TREE TIP TO ZONE BOUNDARY
  do I = 1,NB
     MORE = 1
     !calculate total crown volume
     if (ID(I).eq.'T') then
        TIP = 0.0
        BOT = CTOP(I) - CBOT(I)
        TOTVOL = XCONE(BETA(I),TIP,BOT)
        FOLDENS(I) = LEAF(I)/TOTVOL ! used in CROWN.FOR
        !           WRITE (*,*)I, FOLDENS(I)
     else
        TIP = 0.0
        BOT = CTOP(I) - CBOT(I)
        TOTVOL = XCYL(BETA(I),TIP,BOT)
     end if
     !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     ! ADDING NEW WRITE-FILE HERE.  ANALYZE TREE GEOMETRY.
     if (ID(I).eq.'T') then
        write(77,15)Z1(1)+1,I,TREENO(I),CTOP(I),CBOT(I),&
             &CTOP(I)-CBOT(I),CW(I),BETA(I),TOTVOL,FOLDENS(I),&
             &LEAF(I),LA(I),LA(I)/(((CW(I)/2.)**2)*3.14159)
        15 format(F4.0,1X,I4,1X,I6,1X,4(F5.1,1X),F5.2,1X,5(F9.2,1X))
     end if
     !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     do L = NUM,1,-1
        I1 = INDEX(L)
        I2 = INDEX(L+1)
        J = I1
        if (CTOP(I).le.ZONE(I1)) goto 20
        if (CTOP(I).lt.ZONE(I2)) then
           A = 0.0
        else
           A = CTOP(I)-ZONE(I2)
        end if
        if (ZONE(I1).le.CBOT(I)) then
           B = CTOP(I)-CBOT(I)
           MORE = 0
        else
           B = CTOP(I)-ZONE(I1)
        end if
        !                 WRITE(*,*) I,J,A,B,ZONE(I1)
        if (ID(I).eq.'T') then
           FRUSTVOL = XCONE(BETA(I),A,B)
           BASE(I,J) = XBASE(BETA(I),B)
        else
           FRUSTVOL = XCYL(BETA(I),A,B)
           BASE(I,J) = (AREA - BARE)
        end if
        RAT = FRUSTVOL/TOTVOL
        LLA(I,J) = RAT*LA(I)
        LAI(I,J) = LLA(I,J)/BASE(I,J)

        ! Commenting out writing of file "TEST" for release version.  ajm 11/02
        !       WRITE(72,*) 'I= ',I,' TREELA= ',LA(I),' FRUSTVOL= ',FRUSTVOL,
        !     +    ' BASE= ',BASE(I,J),' TOTVOL= ',TOTVOL

        ! sum cover and LA by zone. EXPAND = TPH for trees, else = 1
        ZCOVER(J) = ZCOVER(J) + BASE(I,J) * EXPAND(I)
        ZONELA(J) = ZONELA(J)+LLA(I,J)*EXPAND(I)
        !               WRITE(*,*) I, J, BETA(I), LLA(I,J), ZONE(I1)
        if (MORE.eq.0) goto 10 !RETURN
        20 continue
     end do
     10 continue
  end do
  ! write out cover by zone to see where crown closure happens
  !      DO 30 L=NUM,1,-1
  !          I1=INDEX(L)
  !          I2=INDEX(L+1)
  !          J=I1
  !          CLOSURE=ZCOVER(J)/10000.
  !          WRITE(*,*) J, ZONE(J), ZCOVER(J), CLOSURE, ZONELA(J)
  !   30    CONTINUE
  !      PAUSE
  return
end

subroutine IDSORT (N,A,INDEX,LSEQ)

  !  **IDSORT DATE OF LAST REVISION:  10:35:00 02/10/87

  !  CHARACTER*4 INDEX SORT.

  !  IDSORT USES THE VECTOR INDEX TO INDIRECTLY ADDRESS THE ARRAY A.
  !  THE FIRST N ELEMENTS OF THE ARRAY INDEX REPRESENT INDICES OF
  !  ELEMENTS OF ARRAY A TO BE SORTED OVER.  THE FIRST N ELEMENTS OF THE
  !  INDEX ARRAY ARE REARRANGED SUCH THAT FOR EACH I FROM 1 TO N-1,
  !  A(INDEX(I)) IS LESS THAN A(INDEX(I+1)).  THE PHYSICAL ARRANGEMENT
  !  OF ARRAY A IS NOT ALTERED.

  !  IF LSEQ IS PASSED IN AS TRUE, THE VECTOR INDEX IS INITIALLY LOADED
  !  WITH VALUES FROM 1 TO N INCLUSIVE.  THIS SORTS OVER THE FIRST
  !  N ELEMENTS OF ARRAY A.

  !  IF LSEQ IS PASSED IN AS FALSE, THE FIRST N ELEMENTS OF INDEX ARE
  !  ASSUMED TO BE THE INDICES OF A TO BE SORTED OVER.

  !  THIS ALGORITHM IS AN ADAPTATION OF THE TECHNIQUE DESCRIBED IN:

  !       SCOWEN, R.A. 1965. ALGORITHM 271; QUICKERSORT. COMM ACM.
  !                    8(11) 669-670.

  !  DECLARATIONS:

  ! Variables
  logical :: LSEQ
  integer :: INDEX,IPUSH,IL,IP,IU,INDIL,INDIP,INDIU,INDKL,INDKU,ITOP,JL,JU,KL,KU

  !  DIMENSIONS:

  dimension INDEX(N),A(N),IPUSH(33)

  !  LOAD IND WITH VALUES FROM 1 TO N.

  if (LSEQ) then
     do I = 1,N
        10 INDEX(I) = I
     end do
  end if

  !  RETURN IF FEWER THAN TWO ELEMENTS IN ARRAY A.

  if (N.lt.2) return

  !  BEGIN THE SORT.

  ITOP = 0
  IL = 1
  IU = N
  30 continue
  if (IU.le.IL) go to 40
  INDIL = INDEX(IL)
  INDIU = INDEX(IU)
  if (IU.gt.IL+1) go to 50
  if (A(INDIL).le.A(INDIU)) go to 40
  INDEX(IL) = INDIU
  INDEX(IU) = INDIL
  40 continue
  if (ITOP.eq.0) return
  IL = IPUSH(ITOP-1)
  IU = IPUSH(ITOP)
  ITOP = ITOP-2
  go to 30
  50 continue
  IP = (IL+IU)/2
  INDIP = INDEX(IP)
  T = A(INDIP)
  INDEX(IP) = INDIL
  KL = IL
  KU = IU
  60 continue
  KL = KL+1
  if (KL.gt.KU) go to 90
  INDKL = INDEX(KL)
  if (A(INDKL).le.T) go to 60
  70 continue
  INDKU = INDEX(KU)
  if (KU.lt.KL) go to 100
  if (A(INDKU).lt.T) go to 80
  KU = KU-1
  go to 70
  80 continue
  INDEX(KL) = INDKU
  INDEX(KU) = INDKL
  KU = KU-1
  go to 60
  90 continue
  INDKU = INDEX(KU)
  100 continue
  INDEX(IL) = INDKU
  INDEX(KU) = INDIP
  if (KU.le.IP) go to 110
  JL = IL
  JU = KU-1
  IL = KU+1
  go to 120
  110 continue
  JL = KU+1
  JU = IU
  IU = KU-1
  120 continue
  ITOP = ITOP+2
  IPUSH(ITOP-1) = JL
  IPUSH(ITOP) = JU
  go to 30
end

subroutine STRATA

  !     STRATA CALCULATES THE NUMBER OF STRATA BASED ON THE CROWN ZONE
  !     DEPTH, S(8), SPECIFIED BY THE USER. -- DWC, 5/30/95.
  !     NOTE: NUMBER OF STRATA < 2 * NUMBER OF ENTITIES.

  ! Common blocks
  include 'ENTITY.f90'
  include 'SITE.f90'

  NSTR = INT((MAXCRHT - MINCRHT) / S(8)) + 1
  write(*,*) 'NUMBER OF STRATA= ',NSTR
  !       WRITE(*,*) 'MAX CROWN HT= ',MAXCRHT
  !       WRITE(*,*) 'MIN CROWN HT= ',MINCRHT
  return
end

subroutine ZONES2

  !     ZONES2 CREATES THE CROWN ZONE BOUNDARIES BASED ON THE NUMBER OF
  !     STRATA AND CROWN DEPTH, S(8). -- DWC, 5/30/95.

  ! Variables
  real    :: TEMP
  ! Common blocks
  include 'ENTITY.f90'
  include 'SITE.f90'

  !     TEMP=MINCRHT
  !     DO 10 K=NSTR,1,-1
  !        IF(K.EQ.1) THEN
  !           ZONE(K)=MAXCRHT
  !        ELSE
  !           ZONE(K)=TEMP
  !        ENDIF
  !           WRITE(*,*) 'ZONE DEPTHS= ',ZONE(K),'  STRATA= ',K
  !        TEMP=TEMP + S(8)
  !        NUM=NSTR-1
  !        NUM2=NSTR
  !  10 CONTINUE
  !     RETURN
  !     END

  ! REVISED ALGORITHM  --- KSM 7/5/95.
  ! ORIGINAL MISSES A ZONE IN SOME CASES
  ! DEFINE ZONES FROM THE TOP, RATHER THAN THE BOTTOM.
  TEMP = MAXCRHT
  NUM = NSTR
  NUM2 = NSTR+1
  do K = 1,NUM2
     ZONE(K) = TEMP
     !           WRITE(*,*) 'ZONE DEPTHS= ',ZONE(K),'  STRATA= ',K
     TEMP = TEMP - S(8)
     !           IF(TEMP.LT.0) TEMP=0.0
     10 continue
  end do
  return
end
