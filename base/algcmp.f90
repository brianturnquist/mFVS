!===============================!
!   Compile infix expressions   !
!===============================!
! BASE $Id$
subroutine ALGCMP (IRC,LIF,CEXPRS,LENEX,JOSTND,LDEBUG,IBASCN, &
     & ISTACK,MXSTCK,IOPCOD,IOPS,MXOPS,CONSTS,ILCONS,ITCONS,MXCONS)
  implicit none

  !     CALLED FROM EVIF AND OTHER ROUTINES.  COMPILES EXPRESSIONS.

  !     AUG 1983 - FORESTRY SCIENCES LABORATORY - MOSCOW, ID 83843
  !     RECODED BY N.L. CROOKSTON IN APRIL 1987 TO ALLOW FOR
  !     FUNCTIONS WITH MORE THAN ONE ARGUMENT.  THE ORGINIAL ROUTINE
  !     WAS TOO HARD TO READ. THIS IS A TOTAL REWRITE.

  !     OVERALL DESCRIPTION:

  !     THE GOAL OF THIS ROUTINE IS TO TRANSLATE AN INFIX
  !     EXPRESSION STORED IN CEXPRS INTO A POSTFIX SET OF OPERATION CODES
  !     STORED IN A PART OF IOPCOD.

  !     THE POSTFIX REPRESENTATION OF AN EXPRESSION IS A SET OF OPERA-
  !     TION CODES ORGANIZED INTO REVERSE POLISH NOTATION.  FOR EXAMPLE
  !     CONSIDER THE EXPRESSION: (BTPA NE 100.).  THIS EXPRESSION IS
  !     TRANSLATED BY THIS ROUTINE INTO A SET OF OPERATIONS, SUCH AS:
  !        103  =    LOAD BTPA ONTO A REAL NUMBER STACK.
  !       1001  =    LOAD THE FIRST VALUE IN ARRAY CONSTS ONTO THE STACK.
  !                  CODE 1023 WOULD INDECATE THAT CONSTS(23) BE LOADED.
  !          5  =    COMPARE THE FIRST TWO VALUES ON THE REAL STACK
  !                  USING NOT EQUAL OPERATOR, PLACE THE LOGICAL
  !                  RESULT ON A LOGICAL STACK, AND POP THE REAL
  !                  REAL STACK TWO POSITIONS.
  !          0  =    ZERO INDECATES THAT THE EXPRESSION IS FINNISHED...
  !                  TAKE THE VALUE AT THE TOP OF THE LOGICAL STACK AS
  !                  THE ANSWER.

  !     THE EXPRESSIONS ARE EVALUATED IN ALGEVL WHICH IS CALLED BY EVMON.
  !     EVMON IS CALLED BY TREGRO TWICE EVERY CYCLE...IN THE FUTURE IT
  !     MAY BE CALLED MORE OFTEN.  THE FIRST CALL IS BEFORE CUTS AND THE
  !     SECOND IS AFTER CUTS.  ALGEVL IS ALSO CALLED WHEN EXPRESSIONS
  !     ARE USED AS PARAMETERS ON KEYWORDS.

  !     ARGUMENTS:

  !     IRC   = RETURN CODES: 0= OK, SEE BOTTOM FOR OTHER CODES.
  !     LIF   = TRUE IF THE EXPRESSION IS A LOGICAL EXPRESSION, FALSE
  !             IF IT IS AN ALGEBRAIC EXPRESSION.
  !     CEXPRS= CHAR*1 ARRAY HOLDING THE EXPRESSION TO BE COMPILED.
  !     LENEX = LENGTH OF THE EXPRESSION...POINTS TO THE LAST NON-BLANK
  !             CHARACTER IN CEXPRS.
  !     JOSTND= OUTPUT DATA SET REFERENCE (FOR DEBUG AND ERROR MSGS).
  !     LEDBUG= TRUE IF DEBUG OUTPUT IS WANTED.
  !     IOPCOD= ARRAY CONTAINING THE OPCODE.
  !     IBASCN= THE BASE OPCODE FOR CONSTANT STORAGE...1000 FOR BASE
  !             PROGNOSIS.
  !     IOPS  = FIRST VACANT POSITION IN OPCODE.
  !     MXOPS = MAX LENGTH OF IOPCOD.
  !     CONSTS= ARRAY OF CONSTANTS IN THE EXPRESSION.
  !     ILCONS= FIRST VACANT POSITION IN BOTTOM OF CONSTS.
  !     ITCONS= FIRST VACANT POSITION IN TOP OF CONSTS.
  !     MXCONS= MAX LENGTH OF CONSTS.
  !     ISTACK= WORK STACK THAT HOLDS THE BROKEN UP PARTS
  !             OF THE EXPRESSION (INDIVIDUAL KEYWORDS)
  !     MXSTCK= MAX SIZE OF ISTACK

  !     VARIABLES AND THEIR MEANINGS:
  !     CLPAR = THE LEFT PARENTHESE
  !     CRPAR = THE RIGHT PARENTHESE
  !     CBLNK = THE BLANK CHARACTER
  !     ITOP  = POINTER TO THE TOP OF ISTACK

  ! Variables
  integer :: MXTMP
  parameter (MXTMP = 50)
  logical :: LDEBUG,LNUMB,LSTONE,LIF
  character(len=1) :: CEXPRS(LENEX)
  character(len=1) :: CTOKEN(20)
  character(len=1) :: CLPAR
  character(len=1) :: CRPAR
  character(len=1) :: CBLNK
  character(len=20) :: CTOK
  character(len=1) :: COMMA
  character(len=1) :: CPLUS
  character(len=1) :: CMINUS
  character(len=1) :: CTIMES
  character(len=1) :: CDIV
  character(len=1) :: CCHAR
  character(len=11) :: CNUMBS
  character(len=8) :: COPS
  equivalence (CTOK,CTOKEN)
  integer :: ITMP(MXTMP),ISTACK(MXSTCK),IOPCOD(MXOPS)
  real    :: CONSTS(MXCONS)
  integer :: IPRMAP(16)

  integer :: IBOOL,ITEST,ICNT,J,K,ISYM,II,NCOMMA,NTMP,I,NUM,ICCN,IDELTA
  integer :: I1,I2,NCCN,ITOKEN,ITOP,ICEXP,ISTCON,ISLCON,ISOPS,ILPAR
  integer :: IRPAR,MXTOK,ICOMMA,MXCONS,MXOPS,MXSTCK
  integer :: ITCONS,ILCONS,IOPS,IBASCN,JOSTND,IRC,LENEX
  real    :: RLNUM

  data IPRMAP /3,2,1,4,4,4,4,4,4,0,5,5,6,6,8,7/
  data CLPAR/'('/,CRPAR/')'/,CBLNK/' '/,CNUMBS/'0123456789.'/, &
       & CPLUS/'+'/,CMINUS/'-'/,CTIMES/'*'/,CDIV/'/'/, &
       & COPS/'()+-*/, '/,COMMA/','/
  data ILPAR/-10001/,IRPAR/-10002/,MXTOK/20/,ICOMMA/-10003/

  !     SYMBOL        PRECEDENCE RANKING      OP CODE

  !     SQRT,EXP,ALOG         9               22,23,24
  !     ALOG10,FRAC,INT,ABS   9               25,26,27,34
  !     SIN,COS,TAN           9               28,29,30
  !     ARCSIN,ARCCOS,ARCTAN  9               31,32,33
  !     MOD,DECADE,... AND    9               101NN,... ETC.
  !     OTHERS, SEE ALGEVL
  !     WHERE NN IS THE NUMBER OF OPERANDS. FOR EXAMPLE 10205 IS DECADE
  !     WITH 5 OPERANDS, LIKE: DECADE(10,20,30,40,50).

  !     **                    8               15
  !     - (UNARY MINUS)       7               16
  !     *,/                   6               13,14
  !     +,-                   5               11,12
  !     EQ,NE                 4               4,5
  !     LT,LE                 4               8,9
  !     GT,GE                 4               6,7
  !     NOT                   3               1
  !     AND                   2               2
  !     OR                    1               3

  !     THE FOLLOWING ARE CLASSES OF OPERATOR CODES THAT RESULT IN
  !     LOADING OPERANDS.

  !     100'S ARE FROM PROGNOSIS TSTV1
  !     200'S ARE FROM PROGNOSIS TSTV2
  !     300'S ARE FROM PROGNOSIS TSTV3
  !     400'S ARE FROM PROGNOSIS TSTV4
  !     500-899 ARE FORM PROGNOSIS TSTV5 (USER DEFINED)
  !     900   IS LOAD RANN(X)
  !     1000'S ARE PROGNOSIS CONSTANTS LOADED INTO PARMS.

  !     SAVE THE ORIGINAL VALUES OF IOPS AND ILCONS FOR LATER USE.

  IRC = 0
  ISOPS = IOPS
  ISLCON = ILCONS
  ISTCON = ITCONS

  !     PARSE THE STRING IN THE CEXPRS VECTOR.  AS THE STRING IS PARSED,
  !     THE VARIOUS OPERANDS AND OPERATORS THAT ARE FOUND ARE PLACED INTO
  !     ISTACK. IF A LEFT OR RIGHT PARENTHSIS IS FOUND THEN ITS INTEGER
  !     EQUIVALENT IS PLACED INTO ISTACK.  IF AN ALGEBRAIC SYMBOL (+,-,
  !     *,/,**) IS FOUND THEN ITS OPCODE IS PLACED IN ISTACK.  IF THE
  !     STRING LOCATED IS NOT AN ALGEBRAIC OPERATOR OR PARENTHESIS THEN
  !     IT IS FIRST PASSED TO THE EVREAL ROUTINE TO SEE IF IT IS A REAL
  !     NUMBER.  IF IT IS NOT A REAL NUMBER, THEN IT IS SENT TO ALGKEY. IF
  !     THE STRING IS NOT A KEYWORD THEN AN ERROR CODE IS ISSUED. IF THE
  !     STRING IS A KEYWORD THEN ITS OPCODE IS PLACED ONTO ISTACK.

  ICEXP = 1
  ITOP = 1

  !     LSTONE IS TRUE IF A + OR - WILL BE UNARY, FALSE IF BINARY.

  LSTONE = .true.

  10 continue
  CCHAR = CEXPRS(ICEXP)

  !     CHECK FOR THE LEFT PARENTHESIS

  if (CCHAR.eq.CLPAR) then

     !        PLACE THE LEFT PARENTHESIS ON THE STACK

     ISTACK(ITOP) = ILPAR
     LSTONE = .true.
     goto 40
  end if

  !     CHECK FOR THE RIGHT PARENTHESIS

  if (CCHAR.eq.CRPAR) then

     !        PLACE THE RIGHT PARENTHESIS ON THE STACK

     ISTACK(ITOP) = IRPAR
     LSTONE = .false.
     goto 40
  end if

  !     CHECK FOR COMMA

  if (CCHAR.eq.COMMA) then

     !        PLACE THE COMMA ON THE STACK

     ISTACK(ITOP) = ICOMMA
     LSTONE = .true.
     goto 40
  end if

  !     CHECK FOR THE PLUS SIGN

  if (CCHAR.eq.CPLUS) then

     !        CHECK TO SEE IF THIS IS A BINARY PLUS, KEEP IT. IF UNARY PLUS
     !        THEN THROW IT AWAY.

     if (LSTONE) then
        LSTONE = .false.
        goto 50
     else

        !           PLACE THE PLUS SIGN ONTO THE STACK

        ISTACK(ITOP) = 11
        goto 40
     end if
  end if

  !     CHECK FOR THE MINUS SIGN

  if (CCHAR.eq.CMINUS) then

     !        IF THIS IS A BINARY MINUS, PUT IT ON THE STACK. IF IT IS
     !        A UNARY MINUS, THEN PLACE THAT ON THE STACK.

     if (LSTONE) then
        LSTONE = .false.
        ISTACK(ITOP) = 16
     else
        ISTACK(ITOP) = 12
     end if
     goto 40
  end if

  !     CHECK FOR THE DIVISION SYMBOL

  if (CCHAR.eq.CDIV) then

     !        PLACE THE DIVISION SIGN ONTO THE STACK

     ISTACK(ITOP) = 14
     LSTONE = .false.
     goto 40
  end if

  !     CHECK FOR THE MULTIPLICATION OR EXPONENTIATION SYMBOL

  if (CCHAR.eq.CTIMES) then

     !        LOAD THE MULTIPLICATION OPERATION ONTO THE STACK

     ISTACK(ITOP) = 13

     !        LOOK FOR ANOTHER *, IF SO IT IS EXPONENTIATION

     if (ICEXP.lt.LENEX) then
        if (CEXPRS(ICEXP+1).eq.CTIMES) then
           ISTACK(ITOP) = 15
           ICEXP = ICEXP+1
        end if
     end if
     LSTONE = .false.
     goto 40
  end if

  !     CHECK FOR THE BLANK CHARACTER, IF BLANK, SKIP TO THE NEXT CHAR.

  if (CCHAR.eq.CBLNK) goto 50

  !     THIS PORTION OF THE EXPRESSION DOES NOT CONTAIN AN OPERATOR
  !     AS SUCH.  THEREFORE, IT MUST CONTAIN A NUMBER OR A KEYWORD.
  !     LOAD THE FIRST CHARACTER ONTO THE CTOKEN ARRAY AND SCAN FOR
  !     THE END OF THE TOKEN.  SET LNUMB TRUE IF THE TOKEN IS A
  !     NUMBER, AND SET LNUMB FALSE OTHERWISE.

  ITOKEN = 1
  CTOK = ' '
  CTOKEN(1) = CCHAR
  LNUMB = INDEX(CNUMBS,CCHAR).gt.0
  25 continue
  CCHAR = CEXPRS(ICEXP)
  if (LNUMB) then
     if (CCHAR.eq.'E') then
        CTOKEN(ITOKEN) = CCHAR
        ITOKEN = ITOKEN+1
        if (ITOKEN.gt.MXTOK) goto 460
        ICEXP = ICEXP+1
        if (ICEXP.gt.LENEX) goto 100
        CCHAR = CEXPRS(ICEXP)
        if (CCHAR.eq.'+' .or. CCHAR.eq.'-') goto 20
     end if
  end if
  if (INDEX(COPS,CCHAR).gt.0) goto 100
  20 continue
  CTOKEN(ITOKEN) = CCHAR
  ITOKEN = ITOKEN+1
  if (ITOKEN.gt.MXTOK) goto 460
  ICEXP = ICEXP+1
  if (ICEXP.gt.LENEX) goto 100

  !     BRANCH BACK TO GET ANOTHER CHARACTER.

  goto 25
  40 continue
  ITOP = ITOP+1
  if (ITOP.gt.MXSTCK) goto 430
  50 continue
  ICEXP = ICEXP+1
  if (ICEXP.gt.LENEX) goto 200
  goto 10
  100 continue

  !     NOW THAT A STRING OF CHARACTERS HAS BEEN FOUND, PASS THEM TO THE
  !     REAL NUMBER SUBROUTINE TO CHECK IF IT IS A REAL NUMBER.

  ITOKEN = ITOKEN-1
  if (LDEBUG) write(JOSTND,'(A20)') CTOK
  if (LNUMB) then
     if (INDEX(CTOK,'.').le.0) then
        ITOKEN = ITOKEN+1
        CTOKEN(ITOKEN) = '.'
     end if
     read (CTOK,'(G20.0)') RLNUM

     !        SET THE SEARCH RANGE...SEARCH FROM I1 TO I2 USING IDELTA
     !        FOR THE INCREMENT.  IF ITCONS IS POSITIVE, THEN SEARCH FROM
     !        THE 'TOP' OF CONSTS TO ITCONS.  IF ITCONS IS -1, THEN
     !        SEARCH FROM NORMALLY.

     if (ITCONS.gt.-1) then
        if (ILCONS.gt.ITCONS) goto 450
        if (ITCONS.eq.MXCONS) then

           !              THIS IS THE FIRST CONSTANT IN THE LIST.

           NCCN = MXCONS
           ITCONS = ITCONS-1
           goto 120
        end if
        I1 = MXCONS
        I2 = ITCONS+1
        IDELTA = -1
     else
        if (ILCONS.gt.MXCONS) goto 450
        if (ILCONS.eq.1) then

           !              THIS IS THE FIRST CONSTANT IN THE LIST.

           NCCN = 1
           ILCONS = ILCONS+1
           goto 120
        end if
        I1 = 1
        I2 = ILCONS-1
        IDELTA = 1
     end if

     !        SEARCH THE LIST TO SEE IF THE CONSTANT IS ALREADY PRESENT.

     do ICCN = I1,I2,IDELTA
        if (CONSTS(ICCN).eq.RLNUM) then
           NCCN = ICCN
           goto 130
        end if
        110 continue
     end do

     !        IT IS NOT PRESENT.  PLACE IT IN THE LIST AND INCREMENT
     !        THE POINTER.  PLACE AT THE BOTTOM IF ITCONS IS GT -1 AND
     !        AT THE TOP OTHERWISE.

     if (ITCONS.gt.-1) then
        NCCN = ITCONS
        ITCONS = ITCONS-1
     else
        NCCN = ILCONS
        ILCONS = ILCONS+1
     end if
     120 continue
     CONSTS(NCCN) = RLNUM
     130 continue
     ISTACK(ITOP) = IBASCN+NCCN
     if (LDEBUG) write(JOSTND,140) ILCONS,ITCONS,NCCN,CONSTS(NCCN), &
          & CTOK,ITOP,ISTACK(ITOP)
     140 format(' ILCONS=',I4,'; ITCONS=',I4,'; CONSTS(',I4,') = ', &
          & E15.7,'; CTOK= ',A20,'; ISTACK(',I4,')=',I7)
     LSTONE = .false.
  else
     call ALGKEY (CTOK,ITOKEN,NUM,IRC)
     if (IRC.ne.0) then
        call EVMKV  (CTOK)
        call ALGKEY (CTOK,ITOKEN,NUM,IRC)
     end if
     if (IRC.ne.0) goto 470
     LSTONE = (NUM.ge.1.and.NUM.le.9)
     ISTACK(ITOP) = NUM
  end if
  ITOP = ITOP+1
  if (ITOP.gt.MXSTCK) goto 430
  if (ICEXP.gt.LENEX) goto 200
  goto 10

  200 continue
  ITOP = ITOP-1
  if (ITOP.le.0) goto 490

  !     IF DEBUG IS ON, WRITE THE LIST OF OPS ON THE STACK.

  if (LDEBUG) write (JOSTND,210) (I,ISTACK(I),I = 1,ITOP)
  210 format (5(:' ISTACK(',I4,')=',I6))

  !     ***** INFIX TO POSTFIX CONVERSION *****

  !     LOAD THE OPERATORS AND OPERANDS INTO IOPCOD IN POSTFIX
  !     NOTATION. (REVERSE POLISH NOTATION).

  NTMP = 0
  NCOMMA = 0

  !     LOOP THROUGH THE STACK OF OPERATORS AND OPERANDS.

  do II = 1,ITOP
     ISYM = ISTACK(II)

     !     IF THE CODE IS A LOAD OPERAND, THEN PLACE IT IN THE
     !     OPERATION CODE ARRAY.

     if (ISYM.lt.0 .and. ISYM.gt.-10000 .or. &
          & (ISYM.ge.100 .and. ISYM.lt.10000)) then
        IOPCOD(IOPS) = ISYM
        IOPS = IOPS+1
        if (IOPS.gt.MXOPS) goto 400
     else

        !     ELSE: THE CODE IS A PARENTHESIS, COMMA, OR OPERATOR.

        !        IF THE TEMPORARY OPERATOR STACK IS EMPTY, OR IF THE
        !        CODE IS A LEFT PAREN, THEN: LOAD IT ONTO THE TEMP STACK.

        if (NTMP.eq.0 .or. ISYM.eq.ILPAR) then

           !           IF THE CODE IS A COMMA OR A RIGHT PAREN, THEN: THERE
           !           IS A MISSING OPERATOR.  BRANCH TO ISSUE ERROR CODE.

           if (ISYM.eq.IRPAR .or. ISYM.eq.ICOMMA) goto 410
           NTMP = NTMP+1
           if (NTMP.gt.MXTMP) goto 430

           !           IF THE CURRENT CODE QUALIFIES FOR COMMAS THEN:  STORE THE
           !           COMMA COUNT AS PART OF THE OPERATION AND RESET NCOMMA.

           if (ISYM.gt.10000) then
              ISYM = ISYM+NCOMMA
              NCOMMA = 0
           end if
           ITMP(NTMP) = ISYM
        else

           !           IF THE CODE IS A RIGHT PAREN OR A COMMA, THEN: POP THE
           !           TEMPORARY STACK AND PLACE THE CONTENTS ONTO THE OP CODE
           !           ARRAY.  POP THE STACK UNTIL A LEFT PAREN IS REACHED OR
           !           UNTIL THE TEMP STACK IS EMPTY.

           !           IF THERE IS A LEFT PAREN ON THE STACK, THEN: IF THE CODE
           !           IS A COMMA, THEN COUNT THE COMMAS AND LEAVE THE LEFT
           !           PAREN ON THE TEMP STACK.  IF THE CODE IS A RIGHT PAREN,
           !           CLEAR THE LEFT PAREN FROM THE STACK.

           if (ISYM.eq.IRPAR .or. ISYM.eq.ICOMMA) then
              do K = 1,NTMP
                 J = NTMP-K+1
                 if (ITMP(J).ne.ILPAR) then

                    !                 IF THE OPERATION CODE CAN HAVE MORE THAN ONE
                    !                 OPERAND, THEN THE NUMBER OF OPERANDS IS THE NUMBER
                    !                 OF COMMAS PLUS ONE.  CONVERT THE OPERATION CODE TO
                    !                 INCLUDE THE NUMBER OF OPERANDS.  THE CODE ON ITMP
                    !                 CARRIES THE NUMBER OF COMMAS PRESENT IN THE
                    !                 EXPRESSION BEFORE THE CURRENT CODE WAS SPECIFIED.
                    !                 THIS NUMBER IS REMOVED FROM THE CODE AND USED TO
                    !                 RESET NCOMMA. THIS ALLOWS NESTED MULT-OP-CODE CODES
                    !                 LIKE: (DECADE(10,MOD(4,5),TIME(MOD(3,2),5)))

                    if (ITMP(J).gt.10000) then
                       ICNT = MOD(ITMP(J),100)
                       IOPCOD(IOPS) = (ITMP(J)/100*100)+NCOMMA+1
                       NCOMMA = ICNT
                    else
                       IOPCOD(IOPS) = ITMP(J)
                    end if
                    IOPS = IOPS+1
                    if (IOPS.gt.MXOPS) goto 400
                 else
                    if (ISYM.eq.ICOMMA) then
                       NCOMMA = NCOMMA+1
                       if (NCOMMA.gt.98) goto 446
                    else
                       J = J-1
                    end if
                    goto 320
                 end if
                 310 continue
              end do
              320 continue
              NTMP = J
           else

              !              THE CODE IS NOT A LEFT OR RIGHT PAREN, A
              !              COMMA, OR A LOAD OPERAND.  IF THE TEMP-STACK-
              !              OPERATOR IS A LEFT PAREN, THEN: PLACE THE
              !              OPERATOR ONTO THE TEMP STACK. (NOTE THAT THIS
              !              IS BRANCH BACK TARGET.  SEE THE COMMENTS IN
              !              THE CODE BELOW).

              330 continue
              if (ITMP(NTMP).eq.ILPAR) then
                 NTMP = NTMP+1
                 if (NTMP.gt.MXTMP) goto 430
                 if (ISYM.gt.10000) then
                    ISYM = ISYM+NCOMMA
                    NCOMMA = 0
                 end if
                 ITMP(NTMP) = ISYM
              else

                 !                 CHECK THE PRECEDENCE OF THE OPERATOR AND THE
                 !                 OPERATOR ON THE TEMPORARY STACK.  IF THE
                 !                 PRECEDENCE OF THE OPERATOR IS EQUAL TO THE
                 !                 PRECEDENCE OF THE TEMP-STACK-OPERATOR, THEN:
                 !                 MOVE THE TEMP-STACK-OP TO THE OPERATOR CODE
                 !                 ARRAY AND PLACE THE OPERATOR ONTO THE TEMP
                 !                 STACK. IF THE OPERATOR HAS GREATER PRECEDENCE
                 !                 THAN THE TEMP-STACK-OP, THEN ADD IT TO THE
                 !                 TEMP STACK. IF THE OPERATOR HAS LESS
                 !                 PRECEDENCE THAN THE TEMP-STACK-OP, THEN MOVE
                 !                 THE TEMP-STACK-OP TO THE OP-CODE ARRAY, POP
                 !                 THE TEMP STACK, AND BRANCH BACK TO COMPARE THE
                 !                 OPERATOR PRECEDENCE TO WHAT IS LEFT ON THE
                 !                 STACK (IF THE STACK IS EMPTY, SIMPLY ADD THE
                 !                 OPCODE TO THE TEMP STACK).

                 if (ISYM.le.16) then
                    J = IPRMAP(ISYM)
                 else
                    J = 9
                 end if
                 if (ITMP(NTMP).le.16) then
                    ITEST = IPRMAP(ITMP(NTMP))
                 else
                    ITEST = 9
                 end if
                 if (J.le.ITEST) then
                    if (ITMP(NTMP).gt.10000) then
                       ICNT = MOD(ITMP(NTMP),100)
                       IOPCOD(IOPS) = (ITMP(NTMP)/100*100)+NCOMMA+1
                       NCOMMA = ICNT
                    else
                       IOPCOD(IOPS) = ITMP(NTMP)
                    end if
                    IOPS = IOPS+1
                    if (IOPS.gt.MXOPS) goto 400

                    !                    SEE THE COMMENTS, ABOVE.

                    if (NTMP.eq.1 .or. J.eq.ITEST) then
                       ITMP(NTMP) = ISYM
                    else

                       !                       DECREMENT THE TEMP STACK AND BRANCH TO RECHECK.

                       NTMP = NTMP-1
                       goto 330
                    end if
                 else

                    !                    PLACE THE OPERATOR ONTO THE TEMP STACK.

                    NTMP = NTMP+1
                    if (NTMP.gt.MXTMP) goto 430
                    if (ISYM.gt.10000) then
                       ISYM = ISYM+NCOMMA
                       NCOMMA = 0
                    end if
                    ITMP(NTMP) = ISYM
                 end if
              end if
           end if
        end if
     end if
     350 continue
  end do

  !     IF THERE ARE MORE OPERATORS ON THE TEMP STACK, THEN PLACE
  !     THEM ON THE OPERATION STACK.

  if (NTMP.gt.0) then
     do II = 1,NTMP
        J = NTMP-II+1

        !        IF THE OPERATOR ON THE TEMP STACK IS A LEFT PAREN, THEN:
        !        THERE ARE MISMATCHED PARENS.

        if (ITMP(J).eq.ILPAR) goto 410
        if (ITMP(J).gt.10000) then
           ICNT = MOD(ITMP(J),100)
           IOPCOD(IOPS) = (ITMP(J)/100*100)+NCOMMA+1
           NCOMMA = ICNT
        else
           IOPCOD(IOPS) = ITMP(J)
        end if
        IOPS = IOPS+1
        if (IOPS.gt.MXOPS) goto 400
        360 continue
     end do
  end if

  !     IF THE NUMBER OF COMMAS IS NOT NOW EQUAL TO ZERO, THEN:
  !     THERE WERE COMMAS USED WITH OPERATORS THAT DO NOT ALLOW
  !     FOR MORE THAN ONE OPERAND.

  if (NCOMMA.gt.0) goto 440

  !     LOAD THE LAST OP-CODE...THIS ZERO SIGNIFIES THE END OF THE
  !     EXPRESSION.

  IOPCOD(IOPS) = 0

  !     ***** THIS IS THE END OF THE INFIX TO POSTFIX CONVERSION *****

  if (LIF) then

     !        CHECK TO SEE IF THE NUMBER OF RELATIONAL OPERATORS
     !        IS EQUAL TO THE NUMBER OF BOOLEAN OPERATORS-1. IF THIS
     !        IS NOT TRUE THEN ISSUE AN ERROR CODE INDICATING AN
     !        INVALID EXPRESSION.

     ICNT = 0
     IBOOL = 0

     do K = ISOPS,IOPS
        ITEST = IOPCOD(K)
        if (ITEST.lt.10) then
           if (ITEST.ge.2.and.ITEST.le.3) IBOOL = IBOOL+1
           if (ITEST.ge.4.and.ITEST.le.9) ICNT = ICNT+1
        end if
        370 continue
     end do
     if (ICNT-1.ne.IBOOL) goto 480
  end if

  !     BRANCH TO EXIT.

  goto 500

  !     ERROR CODES:

  400 continue
  write (JOSTND,405)
  405 format (/T12,'NOT ENOUGH STORAGE TO STORE EXPRESSION.')
  IRC = 1
  goto 600
  410 continue
  write (JOSTND,415)
  415 format (/T12,'MISMATCHED OR OTHER MISUSE OF PARENTHESIS.')
  IRC = 2
  goto 600
  430 continue
  write (JOSTND,435)
  435 format (/T12,'NOT ENOUGH STORAGE TO COMPILE EXPRESSION.')
  IRC = 4
  goto 600
  440 continue
  write (JOSTND,445)
  445 format (/T12,'A COMMA WAS USED WITHOUT AN OPERATOR THAT', &
       & ' ALLOWS ONE.')
  IRC = 5
  goto 600
  446 continue
  write (JOSTND,447)
  447 format (/T12,'TOO MANY COMMAS IN ONE FUNCTION.')
  IRC = 5
  goto 600
  450 continue
  write (JOSTND,455)
  455 format (/T12,'NOT ENOUGH STORAGE TO STORE CONSTANTS IN', &
       & ' THE EXPRESSION.')
  IRC = 6
  goto 600
  460 continue
  write (JOSTND,465)
  465 format (/T12,'A VARIABLE OR NUMBER (TOKEN) IS TOO LONG.')
  IRC = 6
  goto 600
  470 continue
  write (JOSTND,475)
  475 format (/T12,'AN ILLEGAL VARIABLE WAS FOUND.')
  IRC = 7
  goto 600
  480 continue
  write (JOSTND,485)
  485 format (/T12,'A LOGICAL EXPRESSION WAS EXPECTED BUT NOT FOUND.')
  IRC = 8
  goto 600
  490 continue
  write (JOSTND,495)
  495 format (/T12,'NO EXPRESSION WAS FOUND.')
  IRC = 9
  goto 600
  500 continue
  IRC = 0
  if (LDEBUG) write(JOSTND,550) ISOPS,IOPS,(IOPCOD(I),I = ISOPS,IOPS)
  550 format(/,'LEAVING ALGCMP, IOPCOD(',I5,' TO',I5,') ='/ &
       & ((10I7)))
  IOPS = IOPS+1
  if (IOPS.gt.MXOPS) goto 400
  return
  600 continue
  IOPS = ISOPS
  ILCONS = ISLCON
  ITCONS = ISTCON
  return
end subroutine ALGCMP
