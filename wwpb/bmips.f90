!==================!
!   wwpb : bmips   !
!==================!
! WWPB $Id$
subroutine BMIPS(ISTD,IYR)

  !     CALLED FROM: BMDRV
  !     CALLS:  SPLAAR
  !             BMRANN

  !  **BMIPS  Date of last revision:  SEPT 14, 1994

  !  Purpose:
  !     This subroutine deals with Ips. Currently, it fills up 90%
  !     of the slash with Ips. Then any remaining BKP attack trees, either
  !     top-killing them or totally killing them, depending on the size of
  !     the tree.

  !     Routine is used for Ips as a Driving Variable or as the main beetle.

  !  Call list definitions:

  !  Local variable definitions:
  !     ATTP:   Proportion of trees/record to attack at any one time
  !     ATTPRP: Density? of trees in record to attack
  !     BKPIP:  Temporary value containing BKP of Ips for stand
  !     CHECK:  Counter that indicates how  many size classes have
  !             been checked (for presence of host trees/Ips attacked trees?)
  !     DEBUG:  Logical flag to turn debug on and off.
  !     FULL:   Array with flag indicating whether it is has been
  !             entirely attacked or not
  !     IDSIZ:  Loop counter over current size class
  !     ISIZ:   Loop counter for dbh size classes
  !     NEED:   Host volume in current size class
  !             needed to accomodate the BKP for Ips
  !     PSCALE: Array (by size class) that scales the likelihood that trees
  !             will be attacked depending on the cumulative host density
  !     TEMP:   Amount of Ips BKP left after available host dead wood (slash,
  !             downed wood) is taken up by Ips
  !     TOTHST: Total density of host trees for Ips
  !     X:      Value of uniform random number
  !  Common block variables and parameters:
  !     TOPKLL: Array holding proportion of stems top-killed by Ips in
  !             each size class
  !     ALLKLL: Array holding number of stems totally killed by Ips in
  !             each size class
  !     DWPHOS: From BMCOM; Array to hold volumes of downed host tree
  !             volume for Ips (stratified by standing/dead and size class)
  !     IPSMIN:
  !     MSBA:   From BMCOM; Array containing the BA in each DBH size class
  !     MXDWHC: From BMPRM; Maximum number of dead woody pool host types
  !             for Ips (1=standing; 2=downed)
  !     MXDWSZ: From BMPRM; Number of dead woody pool size classes
  !     NSCL:   From BMPRM; Number of dbh size classes
  !     PFSLSH: From BMPRM; Prop'n of slash to fill with Ips before trying
  !             to kill trees
  !     PSLASH: From BMCOM;
  !     PIE:    From BMPRM; The value of pi
  !     UPSIZ:  From BMCOM; Array containing the endpoints of each size class
  !     WPBA:   From BMCOM; Array containing the BA in each Dead Woody Pool
  !             size class

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.

  include 'BMCOM.f90'

  !.... Variable declarations.

  ! Variables
  integer :: CHECK
  integer :: IDSIZ
  integer :: FULL(NSCL)
  integer :: SCKL
  real    :: ADD
  real    :: ATTP
  real    :: ATTPRP
  real    :: BKPIP
  real    :: NEED
  real    :: PSCALE(NSCL)
  real    :: TEMP
  real    :: TOTATT
  real    :: TOTSCL
  real    :: X

  !.... Initialize local variables

  CHECK = 0.0
  do ISIZ = 1,NSCL
     FULL(ISIZ) = 0
     PSCALE(ISIZ) = 0.0
     TOPKLL(ISTD,ISIZ) = 0.0
     5 continue
  end do

  !.... Check for debug.

  if (LBMDEB) write (JBMBPR,10) IYR, ISTD
  10 format(' Begin BMIPS: Year= ',I5, 'Stand= ', I6)

  !.... Begin Routine

  BKPIP = BKPIPS(ISTD)

  !.... only continue if have Ips present

  if (BKPIP .le.1E-6) goto 9999

  !     get the area of the current stand

  call SPLAAR(ISTD,CAREA,IRC)

  !.... loop over the number of dead woody pool size classes

  do IDSIZ = 1,MXDWSZ

     !....   in this case, standing or down

     do IDTYP = 1,MXDWHC

        if (BKPIP .gt.1E-6) then
           TEMP = BKPIP - (DWPHOS(ISTD,IDTYP,IDSIZ)&
                &* WPBA(IDSIZ) * PFSLSH)
           if (TEMP .gt.0.0) then
              PSLASH(ISTD,IDTYP,IDSIZ) = PFSLSH
              if (DWPHOS(ISTD,IDTYP,IDSIZ) .eq.0.0)&
                   &PSLASH(ISTD,IDTYP,IDSIZ) = 0.0
              BKPIP = TEMP
           else
              PSLASH(ISTD,IDTYP,IDSIZ) = BKPIP&
                   &/ (DWPHOS(ISTD,IDTYP,IDSIZ) * WPBA(IDSIZ))
              BKPIP = 0.0
           end if
        else

           !....       if no BKP left then exit out of routine

           goto 9999
        end if

        110 continue
     end do

     !          if no BKP left then exit out of routine

     if (BKPIP .le.1E-6) goto 9999

     100 continue
  end do

  !.... Once the slash fills up, then any remaining Ips start attacking
  !     trees randomly choose tree size to attack (Ips doesn't care what
  !     size the tree is since it only likes the part less than
  !     3 inches diam)

  TOTSCL = 0.0
  AVKLBA = 0.0
  MAXISZ = 0
  do ISIZ = IPSMIN,NSCL

     if (TREE(ISTD,ISIZ,1) .eq.0.0) then
        CHECK = CHECK + 1
        goto 300
     end if
     if (ISIZ .gt. MAXISZ) MAXISZ = ISIZ

     !....  scales the random variable by density and GRF (RV)

     PSCALE(ISIZ) = TREE(ISTD,ISIZ,1) / GRF(ISTD,ISIZ)
     TOTSCL = TOTSCL + PSCALE(ISIZ)

     !         IF (ISIZ .GT. IPSMAX) THEN
     !            ISZ = IPSMAX
     !         ELSE
     !            ISZ = ISIZ
     !         ENDIF
     !         AVKLBA = AVKLBA + PSCALE(ISIZ) * MSBA(ISZ)
     AVKLBA = AVKLBA + PSCALE(ISIZ) * MSBA(IPSMIN)

     300 continue
  end do

  !     If there is no host in the stand then exit out of routine

  if (TOTSCL .lt.1E-4) goto 9999

  !     Calculate the average basal area that can be attacked and the
  !     average number of attacks that can occur.

  AVKLBA = AVKLBA / TOTSCL
  TOTKL = BKPIP * CAREA / AVKLBA

  ! Use the following "group-kill" process if two or more kills are
  ! expected in all eligible size classes.  The loop may be needed more
  ! than once.

  !     NUMSIZ is the number of size classes which have host and could
  !     be attacked

  NUMSIZ = NSCL - CHECK - (IPSMIN - 1)

  330 continue

  !       Consider each size class.  See if there is suitable host in
  !       that size class. If so, calculate how much live host there is,
  !       and how many kills will be attracted to that size class.

  do ISIZ = IPSMIN,MAXISZ
     if (PSCALE(ISIZ) .gt.1E-4) then
        HOST = TREE(ISTD,ISIZ,1) * (1 - ALLKLL(ISTD,ISIZ)&
             &- TOPKLL(ISTD,ISIZ))
        SCKL = INT(TOTKL * PSCALE(ISIZ) / TOTSCL)

        !           Determine whether the size class can absorb the target # of
        !           kills, and calculate ATTPRP accordingly.  Reduce BKP for
        !           the amount used.

        if (SCKL .ge. HOST * CAREA) then
           ATTPRP = HOST / TREE(ISTD,ISIZ,1)
           BKPUSE = HOST * MSBA(IPSMIN)
        else
           ATTPRP = SCKL / (TREE(ISTD,ISIZ,1) * CAREA)
           BKPUSE = (SCKL / CAREA) * MSBA(IPSMIN)
        end if

        BKPIP = BKPIP - BKPUSE

        !           If trees are larger than IPSMAX, they are only topkilled,
        !           while if they are smaller, they are entirely killed.
        !           Both arrays are a proportion.

        if (ISIZ .gt. IPSMAX) then
           TOPKLL(ISTD,ISIZ) = TOPKLL(ISTD,ISIZ) + ATTPRP

           if (TOPKLL(ISTD,ISIZ) .gt.1.0) then
              TOPKLL(ISTD,ISIZ) = 1.0
           end if
        else
           ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) + ATTPRP

           if (ALLKLL(ISTD,ISIZ) .gt.1.0) then
              ALLKLL(ISTD,ISIZ) = 1.0
           end if
        end if

     end if
     350 continue
  end do

  !       Recalculate TOTSCL and AVKLBA to remove extinct size classes
  !       from the cumulative distribution.  If there is no suitable
  !       host left, set BKP to zero (all remaining BKP is lost).

  do ISIZ = IPSMIN,MAXISZ
     TOTATT = TOPKLL(ISTD, ISIZ) + ALLKLL(ISTD, ISIZ)
     if (TOTATT .ge.1.0) then
        PSCALE(ISIZ) = 0.0
        FULL(ISIZ) = 1
     end if
     500 continue
  end do

  TOTSCL = 0.0
  AVKLBA = 0.0
  do ISIZ = IPSMIN, MAXISZ
     TOTSCL = TOTSCL + PSCALE(ISIZ)
     AVKLBA = AVKLBA + (PSCALE(ISIZ) * MSBA(IPSMIN))
     510 continue
  end do

  if (TOTSCL .gt.1E-4) then
     AVKLBA = AVKLBA / TOTSCL
     TOTKL = BKPIP * CAREA / AVKLBA
  else
     BKPIP = 0.0
     TOTKL = 0.0
  end if

  if (TOTKL .ge. (2 * NUMSIZ)) then
     !       There is enough remaining Beetle Kill Potential el ips to kill
     !       two trees in each available size class so must perform the
     !       group kill process again.

     goto 330
  end if

  555 continue

  ! If there is any BKP left, start using the individual kill routine.

  !.... 1/carea = 1 tree

  ATTP = (1.0 / CAREA)

  !     First, turn PSCALE into a cummulative distribution

  if (IPSMIN .eq.1) then
     PSCALE(1) = PSCALE(1) / TOTSCL
     IMIN = 2
  else
     do ISIZ = 1, IPSMIN
        PSCALE(ISIZ) = 0.0
        425 continue
     end do
     IMIN = IPSMIN
  end if

  do ISIZ = IMIN,NSCL
     PSCALE(ISIZ) = PSCALE(ISIZ-1) + PSCALE(ISIZ) / TOTSCL
     450 continue
  end do

  !     Choose a random number which will determine the size class
  !     to attack.

  400 continue

  if (BKPIP .gt.1E-6) then

     !       Determine which size class to attack

     call BMRANN(X)

     do ISIZ = IPSMIN, MAXISZ
        if (X .lt. PSCALE(ISIZ)) goto 480
        475 continue
     end do
     if (ISIZ .gt. MAXISZ) ISIZ = MAXISZ
     480 continue

     if (TREE(ISTD,ISIZ,1) .gt.0.0) then
        ATTPRP = ATTP / TREE(ISTD,ISIZ,1)

        !            Amount of BKP going into tree. Even if the tree is large,
        !            more shouldn't go in than would kill a tree of IPSMAX
        !            size.
        !             IF (ISIZ .GT. IPSMAX) THEN
        !                ISZ = IPSMAX
        !             ELSE
        !                ISZ = ISIZ
        !             ENDIF
        !             ADD = MSBA(ISZ) * ATTP

        !            All trees absorb the same amount that the smallest tree does
        ADD = MSBA(IPSMIN) * ATTP

        !            Amount to kill (or top-kill)
        NEED = ADD * GRF(ISTD,ISIZ)

        if (BKPIP .ge. NEED) then
           BKPIP = BKPIP - ADD

           !            Topkill and Allkill are proportions

           if (ISIZ .gt. IPSMAX) then
              TOPKLL(ISTD,ISIZ) = TOPKLL(ISTD,ISIZ) + ATTPRP
              if (TOPKLL(ISTD,ISIZ) .gt.1.0)&
                   &TOPKLL(ISTD,ISIZ) = 1.0
           else
              ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) + ATTPRP
              if (ALLKLL(ISTD,ISIZ) .gt.1.0)&
                   &ALLKLL(ISTD,ISIZ) = 1.0
           end if

        else

           BKPIP = 0.0

        end if

        !....        Check if all classes are full:

        TOTATT = TOPKLL(ISTD, ISIZ) + ALLKLL(ISTD, ISIZ)
        if ((TOTATT .ge.1.0) .and. (FULL(ISIZ) .eq.0)) then
           FULL(ISIZ) = 1
           CHECK = CHECK + 1

           !                If a new class is full, then remove it from pscale, to
           !                make sure the program wont waste time picking it again.
           PSCALE(ISIZ) = 0.0
           TEMP = 0.0
           do ISZ = IPSMIN,NSCL
              PSCALE(ISZ) = PSCALE(ISZ) * TOTSCL
              TEMP = TEMP + TOTSCL
              625 continue
           end do
           TOTSCL = TEMP
           if (IPSMIN .eq.1) then
              PSCALE(1) = PSCALE(1) / TOTSCL
              IMIN = 2
           else
              IMIN = IPSMIN
           end if
           do ISZ = IMIN,NSCL
              PSCALE(ISZ) = PSCALE(ISZ-1) + PSCALE(ISZ) / TOTSCL
              650 continue
           end do

        end if
        if (CHECK .eq. (NSCL - IPSMIN + 1)) BKPIP = 0.0

     end if
  end if
  if (BKPIP .gt.1E-6) goto 400

  do ISIZ = 1, NSCL

     if (TOPKLL(ISTD,ISIZ) .gt.1.0) TOPKLL(ISTD,ISIZ) = 1.0
     if (ALLKLL(ISTD,ISIZ) .gt.1.0) ALLKLL(ISTD,ISIZ) = 1.0

     !        Change ALLKLL into stems/acre

     ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) * TREE(ISTD,ISIZ,1)

     !        Add killed trees to standing dead pools
     J = L2D(ISIZ) + 1
     K = IQPTYP(ISTD,1)
     SDWP(ISTD,K,J,1) = SDWP(ISTD,K,J,1) +&
          &ALLKLL(ISTD,ISIZ) * TVOL(ISTD,ISIZ,1)

     600 continue
  end do

  !.... Common Return

  9999 continue

  BKPIPS(ISTD) = 0.0

  if (LBMDEB) write(JBMBPR,99) ISTD
  99 format(' End BMIPS: Stand= ', I6)

  return
end
