!===================!
!   wwpb : bmcbkp   !
!===================!
! WWPB $Id$
subroutine BMCBKP (ISTD,IYR,OLDGRF)

  ! CALLED BY:  BMDRV
  ! CALLS:      GPGET2
  !             GPADD

  !  **BMCBKP  Date of last revision:  09/28/05

  !  Purpose
  !     Calculate the BKP resulting from trees killed in the previous year

  !  Call list definitions:
  !     OLDGRF: last year's GRF

  !  Local variable definitions:
  !     IDSIZ:  Loop counter over dead woody pool size categories
  !     IDTYP:  Loop counter over dead woody pool types (Ips)
  !     INC:    Array containing "increase" amounts for each pine beetle
  !             species and dbh size class
  !     ISIZ:   Loop counter over dbh size classes
  !     LBAD:   Logical flag for is this a bad year for reproduction
  !     REPRD:  Generation multiplier for non-Ips REPRDuction this year
  !     REPRDI: Generation multiplier for Ips REPRDuctions this year
  !     SLINC:  Increase factor for REPRD for Ips in slash

  !  Common block variables and parameters:
  !     ALLKLL: From BMCOM; Array containing the number of stems killed
  !             by Ips in each size class
  !     BADREP: The reproductive value in the bad year, for all trees
  !     IBADYR: The year in which reproduction is "bad"
  !     IBADBB: The species for which reproduction is "bad"
  !     MSBA:   From BMCOM; BA in each size class
  !     MXDWHC: From BMPRM; Max number of Dead Woody pool types (IPS)
  !     MXDWSZ: From BMPRM; Max number of Dead Woody Pool Size categories
  !     NBGEN:  From BMCOM; the number of MPB/WPB generations/year
  !     NIBGEN: From BMCOM; the number of IPS generations/year
  !     NSCL:   From BMPRM; the number of dbh size classes
  !     OLDBKP: From BMCOM; array containing the BKP in a stand before dispersal
  !     PSLASH: From BMCOM; the proportion slash inhabited by Ips
  !     TOPKLL: From BMCOM; Array containing the proportion of tops attacked
  !             by Ips in each size class
  !     WPBA:   From BMCOM; Array containing the BA in each
  !              Dead Woody Pool size class

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.

  include 'PPCNTL.f90'
  include 'BMCOM.f90'

  !.... Variable declarations.

  ! Variables
  integer :: IDSIZ,IDTYP
  integer :: DUM(1)
  logical :: LOK
  real    :: REPRD,REPRDI,SLINC
  real    :: PRMS(7)
  real    :: OLDGRF(NSCL)

  save

  if (LBMDEB) write (JBMBPR,1) IYR, ISTD
  1 format (' IN BMCBKP:  IYR=',I4, 'ISTD= ', I6)

  !     Increase factor for ips in slash
  SLINC = 5.0

  !...    REPRD is to account for multiple generations

  if (NBGEN .eq.1) REPRD = 1.0
  if (NBGEN .eq.2) REPRD = 1.5
  if (NBGEN .eq.3) REPRD = 2.0
  if (NBGEN .gt.3) REPRD = 2.5

  !....    turns off Ips as a DV completely

  if (PBSPEC .ne.3.and..not. IPSON) REPRDI = 0.

  !....    REPRDI is for Ips, when Ips is driving var.

  if (NIBGEN .eq.1) REPRDI = 1.0
  if (NIBGEN .eq.2) REPRDI = 1.5
  if (NIBGEN .eq.3) REPRDI = 2.0
  if (NIBGEN .gt.3) REPRDI = 2.5

  !.... if Ips main pest then make sure REPRD reflects this
  !     and make sure there are no other main beetles accidently on

  if (PBSPEC .eq.3) then
     if (REPRD .ne.0.0) REPRDI = REPRD
     REPRD = 0.0
  end if

  !     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

  if (ICNT .ge. BMEND) ICNT = 0
  ICNT = ICNT + 1

  if (ICNT .eq.1) then

     IYR1 = IYR
     NPRMS = 4
     LBAD = .false.

     !     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
     !     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
     !     SET OF PARAMETERS.

     call GPGET2 (317, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

     if (LOK) then

        LBAD = .true.

        IYR2 = IYR1 + IFIX(PRMS(1)) - 1
        IBADBB = INT(PRMS(2))

        if (IBADBB .eq.4) then
           BADREP(PBSPEC) = PRMS(3)
           BADREP(3) = PRMS(4)
        else
           BADREP(IBADBB) = PRMS(3)
        end if

        if (LBMDEB) write (JBMBPR,101) MICYC, IYR1, IBADBB
        101 format (/' IN BMSETP: MICYC=', I5, 'IYR1=',I5,' BBTYPE=',I5)

        if (IYR2.ge.MIY(MICYC)) then
           PRMS(1) = IYR2 - MIY(MICYC) + 1
           call GPADD (KODE, MIY(MICYC), 317, NPRMS, PRMS(1), 1, DUM)
           IYR2 = MIY(MICYC) - 1
           if (LBMDEB) write (JBMBPR,103) PRMS(1),IYR2
           103 format (/' IN BMCBKP: "BAD REPRODUCTION" IS SCHEDULED FOR ',&
                &'THE NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,&
                &'  NEW IYR2=',I5)
        end if
     else
        !         Make sure that we will still get into the proper loops.
        if (IYR .le. IYR2) LBAD = .true.
     end if
  end if

  !.... PBKill was changed to trees/acre, STRIP is trees/acre,
  !     TOPKLL is a proportion and ALLKLL was changed to trees/acre.

  do ISIZ = 1,NSCL

     if (LBAD) then

        !         If this is a bad climatic year then reproduction of the beetles is reduced
        !         and is constant for all size classes. Note that we assume there is no
        !         reproduction in strip-killed trees.

        if (IBADBB .ne.3.and. PBSPEC .ne.3) then

           if (INT(FINAL(ISTD,2)) .eq. ISIZ) then

              !            First, figure out the bkp emerging from the last tree killed
              !            if in this size class. final(1)=bkp used, (2)=size class,
              !            (3)=tpa killed
              PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - FINAL(ISTD,3)
              BKP(ISTD) = BKP(ISTD) + FINAL(ISTD,1) * FINAL(ISTD,3)&
                   &* BADREP(PBSPEC)

              FINAL(ISTD,1) = 0.0
              FINAL(ISTD,2) = 0.0
              FINAL(ISTD,3) = 0.0
           end if

           BKP(ISTD) = BKP(ISTD) + PBKILL(ISTD,ISIZ) * MSBA(ISIZ)&
                &* BADREP(PBSPEC)
           PBKILL(ISTD,ISIZ) = 0.0

        end if
        if (IBADBB .ge.3) then

           BKPIPS(ISTD) = BKPIPS(ISTD) + BADREP(3) * TOPKLL(ISTD,ISIZ)&
                &* MSBA(IPSMIN) * TREE(ISTD,ISIZ,1)
           BKPIPS(ISTD) = BKPIPS(ISTD) + BADREP(3) * ALLKLL(ISTD,ISIZ)&
                &* MSBA(IPSMIN)
           ALLKLL(ISTD,ISIZ) = 0.0

        end if

     else

        !     Note that reproduction is based on the amount of BKP that actually
        !     went into a tree

        if (PBSPEC .ne.3) then

           !          First, figure out the bkp emerging from the last tree killed
           !          if in this size class. final(1)=bkp used, (2)=size class,
           !          (3)=tpa killed.  Set in BMISTD

           if (INT(FINAL(ISTD,2)) .eq. ISIZ) then

              PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - FINAL(ISTD,3)

              if (PBKILL(ISTD,ISIZ).lt.0.0) PBKILL(ISTD,ISIZ) = 0.0 !AJM 9/05

              BKP(ISTD) = BKP(ISTD) + INC(PBSPEC,ISIZ) *&
                   &FINAL(ISTD,1) * FINAL(ISTD,3)
              !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
              !c troubleshoot ajm 9/05
              !           write(99,105)istd,iyr,isiz,bkp(istd),pbkill(istd,isiz),
              !     >     final(istd,1),final(istd,2),final(istd,3),inc(pbspec,isiz),
              !     >     msba(isiz)
              !  105  format('istd=',i4,'  iyr=',i4,'  isiz=',i3,'   bkp=',f10.6,
              !     >       '  pbkill=',f10.6,'  final1(bkp)=',f10.6,'  final2(sc)=',
              !     >        f4.1,'  final3(tpakld)=',f10.6,'  inc=',f5.3,'  msba=',
              !     >        f6.3)
              !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
              FINAL(ISTD,1) = 0.0
              FINAL(ISTD,2) = 0.0
              FINAL(ISTD,3) = 0.0
           end if

           !          then figure out the reproduction from all completely filled trees
           BKP(ISTD) = BKP(ISTD) + MSBA(ISIZ) * PBKILL(ISTD,ISIZ)&
                &* INC(PBSPEC,ISIZ)
           PBKILL(ISTD,ISIZ) = 0.0

           !....      STRIP attacks produce brood, but not quite as well. Since strip attacks
           !          are always last, use the GRF to modify the amount immediately.

           BKP(ISTD) = BKP(ISTD) + MSBA(ISIZ) * INC(PBSPEC,ISIZ)&
                &* 0.75 * STRIP(ISTD,ISIZ)

           !          It seems to me that there is no reason to use GRF as a modifier so
           !          term OLDGRF(ISIZ) is drop here.  S.Zhang, 4/13/98

           !           * OLDGRF(ISIZ)

           STRIP(ISTD, ISIZ) = 0.0

        end if

        !        Ips reproduction from topkilled and totally killed trees. Note that
        !        since Ips only attacks and colonizes the part of the tree less than
        !        3" diameter, all the INCrease factors are the same.

        BKPIPS(ISTD) = BKPIPS(ISTD) + INC(3,ISIZ) * TOPKLL(ISTD,ISIZ)&
             &* TREE(ISTD,ISIZ,1) * MSBA(IPSMIN)
        BKPIPS(ISTD) = BKPIPS(ISTD) + INC(3,ISIZ) * ALLKLL(ISTD,ISIZ)&
             &* MSBA(IPSMIN)

        ALLKLL(ISTD,ISIZ) = 0.0
        !            (TOPKLL may be needed in a later routine so is not zeroed here)

     end if
     100 continue
  end do

  !.... to account for multiple generations

  BKP(ISTD) = BKP(ISTD) * REPRD
  BKPIPS(ISTD) = BKPIPS(ISTD) * REPRDI

  !...Modification to BKP if feedback is invoked. AJM. 12/1/99

  !      IF (LFDBK) THEN
  !        IF (BKP(ISTD) .GE. TFDBK) THEN
  !          BKP(ISTD) = BKP(ISTD) * SFDBK
  !        END IF
  !      END IF

  if (LFDBK) then
     if (BKP(ISTD) .gt. OLDBKP(ISTD)) then
        if (OLDBKP(ISTD) .ge. TFDBK) BKP(ISTD) = TFDBK
     end if
  end if

  !.... PSLASH() is stems/acre

  !.... Ips does very well in slash, irrespective of size, except in bad years

  REPRDI = REPRDI * SLINC
  do IDTYP = 1,MXDWHC
     do IDSIZ = 1,MXDWSZ
        if (LBAD .and. IBADBB .ge.3) then
           BKPIPS(ISTD) = BKPIPS(ISTD) + PSLASH(ISTD,IDTYP,IDSIZ)&
                &* WPBA(IDSIZ) * BADREP(3)
        else
           BKPIPS(ISTD) = BKPIPS(ISTD) + PSLASH(ISTD,IDTYP,IDSIZ)&
                &* WPBA(IDSIZ) * REPRDI
        end if
        PSLASH(ISTD,IDTYP,IDSIZ) = 0.0
        210 continue
     end do
     200 continue
  end do

  if (PBSPEC .ne.3) then
     OLDBKP(ISTD) = BKP(ISTD)
  else
     OLDBKP(ISTD) = BKPIPS(ISTD)
  end if

  if (LBMDEB) write (JBMBPR,2) IYR, ISTD
  2 format (' LEAVING BMCBKP:   IYR=',I4, 'ISTD= ', I6)

  return
end
