!===================================!
!   Evaluate compiled expressions   !
!===================================!
! BASE $Id$
subroutine ALGEVL (LREG,MXL,XREG,MXX,IOPCD,MXCD,IYR1,IYRCUR, &
     & LDB,JOUT,IRC)
  implicit none

  !     CALLED FROM EVMON, OPEVAL, EVTSTV, EVAGRP, HVTHN1, HVHRV1,
  !                 AND HVALOC.

  !     EVALUATES EXPRESSIONS AS COMPILED BY ALGCMP

  !     N.L.CROOKSTON - APR 87 - FORESTRY SCIENCES LAB - MOSCOW, ID

  !     LREG  = WORK SPACE USED TO HOLD THE LOGICAL STACK...AND...
  !             (LOOK OUT FOR THIS ONE:)... THE BOTTOM OF THIS ARRAY
  !             HOLDS STATUS CODES FOR CORRESPONDING MEMBERS OF XREG.
  !             IF XREG(1) IS DEFINED, LREG(MXL) IS FALSE...IF XREG(2)
  !             IS DEFINED LREG(MXL-2+1) IS FALSE...AND TRUE IF
  !             UNDEFINED.  THIS ALLOWS THE DECADE AND TIME FUNCTIONS
  !             TO IGNORE THE FACT THAT SOME ARGUMENTS ARE UNDEFINED.
  !             SORRY ABOUT THIS, I JUST COULD NOT RESIST AVOIDING THE
  !             CREATION OF ANOTHER ARRAY.
  !     MXL   = MAX LENGTH OF LREG.
  !     XREG  = WORK SPACE USED TO HOLD THE REAL STACK.
  !     MXX   = MAX LENGTH OF XREG.
  !     IOPCD = OPERATION CODE IN POSTFIX LOGIC.
  !     MXCD  = MAX LENGTH OF IOPCD.
  !     IYR1  = FIRST YEAR OF THE SIMULATION (USED BY THE DECADE FUNC.)
  !     IYRCUR= CURRENT YEAR OF THE SIMULATION (USED BY THE DECADE AND
  !             TIME FUNCTIONS).
  !     LDEB  = TRUE IF DEBUG OUTPUT IS REQUESTED.
  !     JOUT  = DEBUG OUTPUT DATA SET REFERENCE NUMBER.
  !     IRC   = RETURN CODES, WHERE:
  !             0=THE ANSWER YOU WANT IS LREG(1) IF THE EXPRESSION
  !               IS A LOGICAL EXPRESSION OR XREG(1) IF THE EXPRESSION
  !               IS AN ALGEBRAIC EXPRESSION.  IF THE PARMS FUNCTION IS
  !               EXECUTED, XREG(1) IS THE NUMBER OF ARGUMENTS TO THE
  !               PARMS FUNCTION AND XREG(2 TO 1+XREG(1)) IS THE
  !               LIST OF ARGUMENTS.
  !             1=THE OP-CODE REFERENCED AN UNDEFINED VARIABLE.
  !             2=THE OP-CODE CONTAINED AN UNDECIPHERABLE CODE (OR
  !               DIVIDE BY ZERO).
  !             3=THE RESULT OF RUNNING THE OP-CODE LEFT VARIABLES ON
  !               ONE OR BOTH OF THE STACKS.
  !             4=ONE OF THE STACKS OVERFLOWED OR UNDERFLOWED.

  external RANN
  ! Variables
  logical :: LREG(MXL),LDB
  real    :: XREG(MXX)
  integer :: IOPCD(MXCD)
  integer :: IRC,JOUT,IYRCUR,IYR1,MXL,MXX,MXCD,ILSTK,IXSTK,IPC
  integer :: INSTR,I,J,NDC,K,N
  real    :: ALGSLP,X,BACHLO

  !     NOTE: THIS ROUTINE USES RPN LOGIC TO EVALUATE LOGICAL
  !     EXPRESSIONS. LREG IS AN ANALOG TO A LOGICAL STACK IN A COMPUTER,
  !     AND XREG IS AN ANALOG TO A REAL STACK.  THEY ARE LOADED BY
  !     INCREMENTING A STACK POINTER, AND PLACING THE NEW VALUE IN THE
  !     STACK AT THE LOCATION POINTED TO BY THE POINTER.

  !     INITIALIZE STACK POINTERS.

  ILSTK = 0
  IXSTK = 0

  !     WRITE DEBUG OUTPUT...

  if (LDB) write (JOUT,1) MXL,MXX,MXCD,IYR1,IYRCUR
  1 format (/' IN ALGEVL: MXL=',I5,'; MXX=',I5,'; MXCD=',I5, &
       & '; IYR1=',I5,'; IYRCUR=',I5)

  !     START DOWN THRU THE INSTRUCTION LIST.  THE LAST INSTRUCTION
  !     WILL BE A STOP INSTRUCTION, SO BRANCH TO EXIT THE LOOP IF
  !     THE OPCODE IS ZERO.

  !     LET IPC BE A PROGRAM COUNTER.

  do IPC = 1,MXCD

     !     LOAD THE INSTRUCTION.

     INSTR = IOPCD(IPC)

     !     PRINT DEBUG INFO.

     if (LDB) then
        if (ILSTK.gt.0) write (JOUT,11) (LREG(I),I = 1,ILSTK)
        11 format (' LREG=',((T8,20L3)))
        if (IXSTK.gt.0) then
           write (JOUT,12) (XREG(I),I = 1,IXSTK)
           write (JOUT,11) (LREG(MXL-I+1),I = 1,IXSTK)
           12 format (' XREG=',((T8,5E14.6)))
        end if
        write (JOUT,13) IPC,INSTR,ILSTK,IXSTK
        13 format (' PROG COUNTER=',I6,'; INSTR= ',I5,'; ILSTK= ',I4, &
             & '; IXSTK=',I4)
     end if

     !     IF INSTRUCTION IS A ZERO (END-OF-COMPUTIONS), THEN: BRANCH
     !     TO EXIT LOOP.

     if (INSTR.eq.0) goto 1020

     !     IF THE INSTRUCTION IS A NEGATIVE NUMBER, ITS VALUE IS A
     !     SPECIES CODE OR A SPECIES GROUP CODE. IF IT IS BETWEEN
     !     -1 AND -1000 IT IS A SPECIES CODE AND IT NEEDS TO BE CHANGED
     !     TO A POSITIVE NUMBER. IF IT IS LESS THAN -1000 THEN ADD
     !     1000 TO THE NUMBER SO THAT IT IS < -1.
     !     THEN LOAD THE RESULT ONTO THE XREG.

     if (INSTR.lt.0) then
        IXSTK = IXSTK+1
        if (IXSTK.gt.MXX .or. ILSTK.ge.MXL-IXSTK+1) goto 2040
        if ((INSTR.lt.-1000).and.(INSTR.gt.-2000)) then
           INSTR = INSTR+1000

           !  IF INSTR IS LT. -1000 AND GT -2000 THEN THE INSTRUCTION IS A
           !  GROUP POINT  CODE

        else if (INSTR.lt.-2000) then
           INSTR = INSTR+2000
        else
           INSTR = -INSTR
        end if
        XREG(IXSTK) = FLOAT(INSTR)
        LREG(MXL-IXSTK+1) = .false.
        goto 1000
     end if

     !     IS THE INSTRUCTION LOGICAL OR BOOLEAN?

     if (INSTR.le.9) goto 20

     !     IS THE INSTRUCTION BINARY ARITHMETIC?

     if (INSTR.gt.10 .and. INSTR.le.15) goto 100

     !     IS THE INSTRUCTION UNARY ARITHMETIC (INCLUDING FUNCTIONS
     !     OF ONLY ONE ARGUMENT)?

     if (INSTR.le.34) goto 70

     !     IS THE INSTRUCTION A FUNCTION OF MORE THAN ONE ARGUMENT?

     if (INSTR.gt.10000) goto 300

     !     THE INSTRUCTION LOAD OPERAND: 100 < INSTR < 10000

     if (INSTR.gt.100 .and. INSTR.lt.10000) goto 200
     goto 2020
     20 continue

     !     INSTRUCTION IS A LOGICAL OR BOOLEAN OPERATION.

     !     RULES FOR LOGICAL/BOOLEAN OPERATIONS:
     !     OPT CODE   NAME   PROGRAM OPERATION

     !        1       NOT    SWITCH LREG(ILSTK)

     !        2       AND    LOGICALLY AND LREG(ILSTK+1) AND LREG(ILSTK),
     !        3        OR    LOGICALLY OR LREG(ILSTK+1) AND LREG(ILSTK),
     !                       STORE RESULT IN LREG(ILSTK) AND POP THE STACK.

     !        4        EQ
     !        5        NE    INCREMENT ILSTK, COMPARE XREG(IXSTK) AND
     !        6        GT    XREG(IXSTK+1), AND PUT THE RESULT IN
     !        7        GE    LREG(ILSTK)
     !        8        LT
     !        9        LE

     if (INSTR.eq.1) then

        !        IF THE LOGICAL STACK IS ZERO, THEN: STACK UNDERFLOW.

        if (ILSTK.eq.0) goto 2040
        LREG(ILSTK) = .not.LREG(ILSTK)
        goto 1000
     end if
     if (INSTR.le.3) then

        !        IF THE LOGICAL STACK IS LE 1, THEN: STACK UNDERFLOW.

        if (ILSTK.le.1) goto 2040
        ILSTK = ILSTK-1
        if (INSTR.eq.2) then
           LREG(ILSTK) = LREG(ILSTK+1).and.LREG(ILSTK)
        else
           LREG(ILSTK) = LREG(ILSTK+1).or.LREG(ILSTK)
        end if
        goto 1000
     end if

     !     INCREMENT THE LOGICAL STACK POINTER AND DECREMENT THE REAL
     !     STACK POINTER.

     IXSTK = IXSTK-2
     if (IXSTK.lt.0) goto 2040
     ILSTK = ILSTK+1
     if (ILSTK.gt.MXL-IXSTK) goto 2040

     !     IF EITHER OPERAND IS NOT DEFINED, BRANCH TO EXIT.

     if (LREG(MXL-IXSTK).or.LREG(MXL-IXSTK-1)) goto 2010

     !     DECODE THE INSTRUCTION AND BRANCH TO OPERATION.

     I = INSTR-3
     goto (21,22,23,24,25,26),I
     21 continue
     LREG(ILSTK) = XREG(IXSTK+1).eq.XREG(IXSTK+2)
     goto 1000
     22 continue
     LREG(ILSTK) = XREG(IXSTK+1).ne.XREG(IXSTK+2)
     goto 1000
     23 continue
     LREG(ILSTK) = XREG(IXSTK+1).gt.XREG(IXSTK+2)
     goto 1000
     24 continue
     LREG(ILSTK) = XREG(IXSTK+1).ge.XREG(IXSTK+2)
     goto 1000
     25 continue
     LREG(ILSTK) = XREG(IXSTK+1).lt.XREG(IXSTK+2)
     goto 1000
     26 continue
     LREG(ILSTK) = XREG(IXSTK+1).le.XREG(IXSTK+2)
     goto 1000
     70 continue

     !     UNARY OPERATIONS.
     !     OPT CODE     OPERATION         OPT CODE    OPERATION

     !        16        UNARY MINUS         22        SQRT
     !        23        EXP                 24        ALOG
     !        25        ALOG10              26        FRAC
     !        27        INT                 28        SIN
     !        29        COS                 30        TAN
     !        31        ARCSIN              32        ARCCOS
     !        33        ARCTAN              34        ABS

     if (INSTR.eq.16) then
        J = 1
     else
        J = INSTR-20
        if (J.lt.2 .or. J.gt.14) goto 2020
     end if

     !     ARITHMETIC UPON AN UNDEFINED VARIABLE CREATES AN UNDEFINED
     !     VARIABLE.

     if (LREG(MXL-IXSTK+1)) goto 1000
     goto (71,72,73,74,75,76,77,78,79,80,81,82,83,84),J
     71 continue
     XREG(IXSTK) = -XREG(IXSTK)
     goto 1000
     72 continue
     XREG(IXSTK) = SQRT(XREG(IXSTK))
     goto 1000
     73 continue
     XREG(IXSTK) = EXP(XREG(IXSTK))
     goto 1000
     74 continue
     XREG(IXSTK) = ALOG(XREG(IXSTK))
     goto 1000
     75 continue
     XREG(IXSTK) = ALOG10(XREG(IXSTK))
     goto 1000
     76 continue
     XREG(IXSTK) = XREG(IXSTK)-AINT(XREG(IXSTK))
     goto 1000
     77 continue
     XREG(IXSTK) = AINT(XREG(IXSTK))
     goto 1000
     78 continue
     XREG(IXSTK) = SIN(XREG(IXSTK))
     goto 1000
     79 continue
     XREG(IXSTK) = COS(XREG(IXSTK))
     goto 1000
     80 continue
     XREG(IXSTK) = TAN(XREG(IXSTK))
     goto 1000
     81 continue
     XREG(IXSTK) = ASIN(XREG(IXSTK))
     goto 1000
     82 continue
     XREG(IXSTK) = ACOS(XREG(IXSTK))
     goto 1000
     83 continue
     XREG(IXSTK) = ATAN(XREG(IXSTK))
     goto 1000
     84 continue
     XREG(IXSTK) = ABS(XREG(IXSTK))
     goto 1000
     100 continue

     !     BINARY OPERATIONS ON THE XREG.
     !     OPT CODE     OPERATION     OPT CODE   OPERATION

     !        11        PLUS             12        MINUS
     !        13        MULT             14        DIVIDE
     !        15        RAISE TO A POWER.

     !     DECREMENT THE STACK POINTER.

     IXSTK = IXSTK-1
     if (IXSTK.le.0) goto 2040

     !     ARITHMETIC UPON UNDEFINED OPERANDS CREATES AN UNDEFINED
     !     RESULT.

     LREG(MXL-IXSTK+1) = LREG(MXL-IXSTK+1).or.LREG(MXL-IXSTK)
     if (LREG(MXL-IXSTK+1)) then
        XREG(IXSTK) = 0.0
        goto 1000
     end if

     !     DECODE INSTRUCTION, AND BRANCH.

     J = INSTR-10
     goto (101,102,103,104,105),J
     101 continue
     XREG(IXSTK) = XREG(IXSTK)+XREG(IXSTK+1)
     goto 1000
     102 continue
     XREG(IXSTK) = XREG(IXSTK)-XREG(IXSTK+1)
     goto 1000
     103 continue
     XREG(IXSTK) = XREG(IXSTK)*XREG(IXSTK+1)
     goto 1000
     104 continue
     if (XREG(IXSTK+1).eq.0.0) then
        LREG(MXL-IXSTK+1) = .true.
     else
        XREG(IXSTK) = XREG(IXSTK)/XREG(IXSTK+1)
     end if
     goto 1000
     105 continue
     XREG(IXSTK) = XREG(IXSTK)**XREG(IXSTK+1)
     goto 1000
     200 continue

     !     THE INSTRUCTION IS A LOAD XREG(IXSTK) INSTRUCTION.  INCREMENT
     !     IXSTK AND LOAD THE VARIABLE INTO XREG(IXSTK).  MAKE SURE THERE
     !     IS ROOM FOR THE NEW X AND ITS STATUS IN LREG.

     IXSTK = IXSTK+1
     if (IXSTK.gt.MXX .or. ILSTK.ge.MXL-IXSTK+1) goto 2040
     call EVLDX (XREG(IXSTK),1,INSTR,IRC)
     LREG(MXL-IXSTK+1) = .not.(IRC.eq.0)
     if (IRC.eq.2) goto 2020
     goto 1000
     300 continue

     !     OPERATIONS ON MORE THAN ONE ARGUMENT.  NN = THE NUMBER
     !     OF ARGUMENTS IN THE FUNCTION.

     !     OPT CODE     OPERATION     OPT CODE   OPERATION

     !      101NN       MOD            102NN       DECADE
     !      103NN       TIME           104NN       SUMSTAT
     !      105NN       DBHDIST        106NN       SPMCDBH
     !      107NN       PARMS          108NN       LININT
     !      109NN       MIN            110NN       MAX
     !      111NN       BOUND          112NN       BCCFSP
     !      113NN       ACCFSP         114NN       MININDEX
     !      115NN       MAXINDEX       116NN       NORMAL
     !      117NN       FUELLOAD       118NN       SNAGS
     !      119NN       POTFLEN        120NN       INDEX
     !      121NN       POTFMORT       122NN       FUELMODS
     !      123NN       SALVVOL        124NN       POINTID
     !      125NN       STRSTAT        126NN       POTFTYPE
     !      127NN       POTSRATE       128NN       POTREINT
     !      129NN       TREEBIO        130NN       CARBSTAT
     !      131NN       HTDIST         132NN       HERBSHRB
     !      133NN       DWDVAL         134NN       ACORNS
     !      135NN       CLSPVIAB

     !     LET I = THE OPT CODE, 1=MOD,2=DECADE,3=TIME,...
     !         J = THE NUMBER OF OPERANDS = NN

     I = (INSTR/100)-100
     J = MOD(INSTR,100)

     !     POP THE STACK J TIMES.  IXSTK NOW POINTS TO THE LOCATION
     !     OF THE RESULT.  IF THE XREG STACK HAS FEWER THAN J ENTRIES,
     !     STACK UNDERFLOW HAS OCCURRED.

     if (IXSTK-J.lt.0) goto 2040
     IXSTK = IXSTK-J+1

     !     BRANCH TO EXECUTE THE OPERATION.

     goto (2020,301,302,303,340,340,340,350,360,370,370,380,340,340, &
          & 390,390,400,340,340,340,410,340,340,340,340,340,340,340,340, &
          & 340,340,340,340,340,340,340,2020),(I+1)
     301 continue

     !     IF EITHER ARGUMENT IS UNDEFINED, SO IS THE RESULT.

     LREG(MXL-IXSTK+1) = LREG(MXL-IXSTK+1).or.LREG(MXL-IXSTK)
     if (LREG(MXL-IXSTK+1)) goto 1000

     !     PROTECT AGAINST DIVISION BY ZERO.

     if (XREG(IXSTK+1).eq.0.0) then
        LREG(MXL-IXSTK+1) = .true.
     else
        XREG(IXSTK) = AMOD(XREG(IXSTK),XREG(IXSTK+1))
     end if
     goto 1000

     !     COMPUTE THE DECADE FUNCTION.  THE RESULT CARRIES THE DEFINED
     !     OR NOT DEFINED STATUS OF THE ARGUMENT THAT IS PICKED.  THUS
     !     THE DECADE FUNCTION MAY BE A DEFINED FUNCTION OF SOME, BUT NOT
     !     ALL, UNDEFINED ARGUMENTS.

     302 continue
     NDC = ((IYRCUR-IYR1)/10)+1
     if (NDC.gt.J) NDC = J
     LREG(MXL-IXSTK+1) = LREG(MXL-IXSTK-NDC+2)
     if (.not.LREG(MXL-IXSTK+1)) XREG(IXSTK) = XREG(IXSTK+NDC-1)
     goto 1000

     !     COMPUTE THE TIME FUNCTION.  THE TIME FUNCTION IS UNDEFINED IF
     !     A CRITICAL YEAR LEVEL IS UNDEFINED OR IF THE CHOSEN ARGUMENT
     !     CARRIES AN UNDEFINED STATUS.

     303 continue
     if (J.le.2) goto 320
     do NDC = (IXSTK+1),(IXSTK+J-1),2
        XREG(IXSTK) = XREG(NDC-1)

        !     LET THE "DEFINED" STATUS OF THE ARGUMENT FOLLOW ON THE STACK.

        LREG(MXL-IXSTK+1) = LREG(MXL-NDC+2)

        !     IF THERE IS AN ARGUEMENT FOLLOWING THE YEAR, THEN THE YEAR
        !     IS CRITICAL TO THE RESULT.  IF THE YEAR IS UNDEFINED, SET
        !     THE DEFINED STATUS OF THE RESULT TO UNDEFINED AND EXIT.

        if (NDC+1.le.(IXSTK+J-1)) then
           if (LREG(MXL-NDC+1)) then
              LREG(MXL-IXSTK+1) = .true.
              goto 320
           end if

           !        CHECK THE YEAR, IF THE CURRENT YEAR IS GE THE YEAR ON
           !        THE STACK, THEN PICK THE NEXT ARGUMENT FOLLOWING THE DATE.

           if (IYRCUR.ge.IFIX(XREG(NDC))) then
              XREG(IXSTK) = XREG(NDC+1)
              LREG(MXL-IXSTK+1) = LREG(MXL-NDC)
           else
              goto 320
           end if
        end if
        310 continue
     end do
     320 continue
     goto 1000

     !     DO THE SUMSTAT, DBHDIST, BCCFSP, ACCFSP, SPMCDBH, FUELLOAD,
     !          SNAGS, POTFLEN, POTFMORT, FUELMODS, SALVVOL,
     !          POINTID, STRSTAT, POTFTYPE, POTSRATE, POTREINT, TREEBIO,
     !          HTDIST, HERBSHRB, DWDVAL AND ACORNS FUNCTIONS.
     !     PROCESS CLSPVIAB

     340 continue

     !     ALL THE ARGUMENTS MUST BE DEFINED.

     do NDC = (IXSTK+1),(IXSTK+J-1)
        LREG(MXL-IXSTK+1) = LREG(MXL-NDC+2)
        if (LREG(MXL-IXSTK+1)) goto 1000
        345 continue
     end do

     call EVLDX (XREG(IXSTK),MXX-IXSTK+1,INSTR,IRC)
     LREG(MXL-IXSTK+1) = .not.(IRC.eq.0)
     if (IRC.eq.2) goto 2020
     goto 1000

     !     DO THE PARMS FUNCTION.
     !     LREG(1) IS TRUE IF ALL PARMS ARE DEFINED, FALSE OTHERWISE
     !     XREG(1) IS THE NUMBER OF PARAMETERS
     !     XREG(2) IS THE FIRST PARAMETER, LREG(2) IS TRUE IF XREG(2)
     !                                     IS DEFINED, FALSE OTHERWISE
     !     XREG(3) IS THE SECOND, LREG(3) IS TRUE IF XREG(3) IS DEFINED
     !     XREG(N+1) IS THE NTH, LREG(N+1) IS TRUE IF XREG(N+1) IS DEFINED

     !     THE NUMBER OF ARGUMENTS J SHOULD BE EQUAL TO THE NUMBER OF VALUES
     !     ON THE XREG AND THE MAX SIZE OF LREG MUST BE 2*J+1 OR LARGER.

     350 continue
     if (J+1.gt.MXX .or. J*2+1.gt.MXL) goto 2040
     LREG(1) = .true.
     K = MXL
     do NDC = J+1,2,-1
        XREG(NDC) = XREG(NDC-1)
        LREG(NDC) = LREG(K)
        K = K-1
        LREG(1) = LREG(1).and. .not.LREG(NDC)
        355 continue
     end do
     XREG(1) = J
     LREG(MXL) = .not.LREG(1)
     goto 1000
     360 continue

     !     EVALUATE THE LINEAR INTERPOLATION FUNCTION.

     if (J.ge.5 .and. MOD(J-1,2).eq.0) then
        do K = 1,J
           if (LREG(MXL-IXSTK+2-K)) goto 366
           362 continue
        end do
        XREG(IXSTK) = ALGSLP(XREG(IXSTK),XREG(IXSTK+1), &
             & XREG(IXSTK+((J-1)/2)+1),(J-1)/2)
        LREG(MXL-IXSTK+1) = .false.
        goto 1000
     end if
     366 continue
     LREG(MXL-IXSTK+1) = .true.
     XREG(IXSTK) = 0.0
     goto 1000
     370 continue

     !     COMPUTE THE MIN AND MAX FUNCTIONS.  IF THERE IS ONLY ONE ARG,
     !     THE MIN OR MAX IS EQUAL TO THE ARG, SO SIMPLY BRANCH TO 1000.
     !     USE THE FORTRAN MIN,MAX FUNCTIONS IN A PAIR-WISE MANNER.

     if (J.eq.1) then
        if (LREG(MXL-IXSTK+1)) goto 376
        goto 1000
     end if
     do K = 2,J
        if (LREG(MXL-IXSTK+2-K)) goto 376
        if (I.eq.9) then
           XREG(IXSTK) = AMIN1(XREG(IXSTK),XREG(IXSTK+K-1))
        else
           XREG(IXSTK) = AMAX1(XREG(IXSTK),XREG(IXSTK+K-1))
        end if
        372 continue
     end do
     goto 1000
     376 continue
     LREG(MXL-IXSTK+1) = .true.
     XREG(IXSTK) = 0.0
     goto 1000
     380 continue

     !     COMPUTE THE BOUND FUNCTION

     if (J.ne.3) goto 2040
     if (LREG(MXL-IXSTK+1).or.LREG(MXL-IXSTK).or.LREG(MXL-IXSTK-1) &
          & .or.XREG(IXSTK).ge.XREG(IXSTK+2)) goto 366
     XREG(IXSTK) = AMIN1(AMAX1(XREG(IXSTK),XREG(IXSTK+1)),XREG(IXSTK+2))
     goto 1000
     390 continue

     !     COMPUTE THE MININDEX AND MAXINDEX FUNCTIONS.

     if (J.eq.1) then
        if (LREG(MXL-IXSTK+1)) goto 396
        XREG(IXSTK) = 1.0
        goto 1000
     end if
     N = 0
     if (I.eq.14) then
        X = 1E30
     else
        X = -1E30
     end if
     do K = 1,J
        if (LREG(MXL-IXSTK+2-K)) goto 396
        if (I.eq.14) then
           if (XREG(IXSTK+K-1).lt.X) then
              N = K
              X = XREG(IXSTK+K-1)
           end if
        else
           if (XREG(IXSTK+K-1).gt.X) then
              N = K
              X = XREG(IXSTK+K-1)
           end if
        end if
        392 continue
     end do
     if (N.eq.0) goto 2040
     XREG(IXSTK) = N
     goto 1000
     396 continue
     LREG(MXL-IXSTK+1) = .true.
     XREG(IXSTK) = 0.0
     goto 1000

     !     COMPUTE THE NORMAL FUNCTION.

     400 continue

     !     IF EITHER ARGUMENT IS UNDEFINED, SO IS THE RESULT...
     !     AND THERE MUST BE EXACTLY 2 ARGUMENTS.

     LREG(MXL-IXSTK+1) = (J.ne.2).or.LREG(MXL-IXSTK+1).or. &
          & LREG(MXL-IXSTK)
     if (LREG(MXL-IXSTK+1)) goto 1000
     XREG(IXSTK) = BACHLO(XREG(IXSTK), XREG(IXSTK+1), RANN)
     goto 1000

     !     COMPUTE THE INDEX FUNCTION.

     410 continue
     if (J.lt.2) then
        LREG(MXL-IXSTK+1) = .true.
        XREG(IXSTK) = 0.0
        goto 1000
     end if

     !     IF FIRST ARGUMENT IS UNDEFINED, THE FUNCTION IS UNDEFINED.

     if (LREG(MXL-IXSTK+1)) then
        XREG(IXSTK) = 0.0
        goto 1000
     end if

     !     K IS THE ARGUMENT WE WANT...BOUNDED TO THE NUMBER OF ARGUMENTS.

     K = IFIX(XREG(IXSTK)+.5)
     if (K.lt.1) K = 1
     if (K.gt.J-1) K = J-1

     !     IF THE ARGUMENT WE WANT IS UNDEFINED, THEN THE RESULT IS UNDEFINED.

     if (LREG(MXL-IXSTK+1-K)) then
        LREG(MXL-IXSTK+1) = .true.
        XREG(IXSTK) = 0.0
        goto 1000
     end if

     XREG(IXSTK) = XREG(IXSTK+K)
     LREG(MXL-IXSTK+1) = .false.
     goto 1000

     1000 continue
  end do
  1020 continue

  !     END OF COMPUTATIONS.

  if (ILSTK+IXSTK.eq.1) then
     IRC = 0
  else
     IRC = 3
  end if

  !     IF THE RESULT IS UNDEFINED, BRANCH TO CORRECT RETURN CODE.

  if (LREG(MXL)) goto 2010
  return
  2010 continue
  IRC = 1
  return
  2020 continue
  IRC = 2
  return
  2040 continue
  IRC = 4
  return
end subroutine ALGEVL
