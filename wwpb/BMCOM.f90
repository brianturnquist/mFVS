!==================!
!   wwpb : BMCOM   !
!==================!
! WWPB $Id$

!  **BMCOM  Date of last revision: 12/08/05

!    Revision History:
!      Additional variables declared for BMFDBK keyword by Matt Oberle (MJO, May98)
!      Add'l vars. added 8/26/99 (ajm) (for BMATCT)
!      June 27 2005.  ajm  Changing VOLREM to CVOLREM (for cumulative for cycle)
!      THis variable will behave as original--zeroed once per cycle. ("C" = cumulative.)
!      New variable (VOLREM) will hold current YEAR's volume removed,
!      and will be reset annually.

!  Purpose:
!     Common block variables related to the Pine Beetle Model routines.

!  PPE-Beetle (landscape level) Model Common block variables:

!     ATTBYK: ATTractiveness of each stand to itself, divided by its K value
!     ATTC:   the 'ATTractiveness Constant' of each stand (eqn.9)
!     AREAL:  total area of the Landscape, in sq.mi.
!     AREALP: approximate area of the Landscape that is within RMAX of the
!             Outside (P for 'prime'), in sq.mi.
!     AREALS: total Stockable area of the Landscap, in sq.mi.
!     AREAO:  total area of the Outside, in sq.mi.
!     BADREP: The reproductive value in a climatic "bad" year
!     BMINI:  The initial values of beetle-killed trees by stand (col 1) and
!             beetle species (col 2). Column 3: 1=size class, 2=tpa.
!     CBAHO:  'Constant' value of Basal Area per acre of Host in the Outside
!     CBAO:   'Constant' value of total Basal Area per acre in the Outside
!     CBASPO: 'Constant' value of Basal Area per acre of SPecial trees Outside
!     CBKPO:  'Constant' value of BKP produced per acre in the Outside
!     CRASH:  Prop'n of eligible stems to windthrow
!     CRVOND: 'Constant' value of host Rating Value Outside except Not Drought
!     CSPO:   'Constant' value of Basal Area per acre of Host in the Outside
!     DBHMAX: DBH at which maximum reproduction is first reached.
!     DENS:   Strike density (strikes/acre)
!     DRYON:  Logical flag for the drought model
!     IBADBB: Beetle type that is affected by a bad reproductive year
!     IPSMIN: Smallest size class that Ips will attack
!     IPSMAX: Maximum size class that Ips can kill
!     IPSON:  Turns Ips on as a driving variable (if =true)
!     ISCMIN: Index of lowest attractive size class for each beetle spp.
!             1=MPB, 2=WPB, 3=Ips
!     LBAD:   Logical flag which is TRUE if this is a bad reproductive year.
!     MSBA:   Array containing the BA in each DBH size class
!     NBGEN:  The number of MPB/WPB generations/year
!     NIBGEN: The number of IPS generations/year
!     OUTOFF: logical variable indicating that Outside world is not to be used
!     PBSPEC: Pine Beetle species: 1=MPB, 2=WPB, 3=Ips, 4=WPB&MPB
!             (3,4 not working yet)
!     PFSLSH= The proportion of slash to fill with Ips before killing trees
!     STOCK:  logical variable to indicate whether each stand is STOCKable
!     STOCKO: proportion of the Outside that is STOCKable
!     RADL:   RADius of a circle just large enough to enclose the Landscape
!     RADLC:  RADius that the Landscape would have if it was circular
!     REPLAC: DBH at which BKP just replaces itself (R=1)
!     REPMAX: Maximum reproductive rate of BKP
!     RVOD:   host Rating Value Outside due to Drought effects
!     SCMIN:  minimum DBH for attack (set by keyword)
!     SELFA:  The "n" in the attractiveness equation (modifies attr. at small dists)
!     WTHRSH: Default min # stems to windthrow
!     UPBA:   Array containing the upper-most basal area in a dbh size class
!     UPSIZ:  Array containing the upper size limit in a dbh size class
!     USERC:  User supplied or default "c" values (by beetle species)
!     USERA:  User supplied or default "a" values (by beetle species)
!     WPSIZ:  Array containing the upper size limit in a dbh size class
!             for the dead woody pools
!     LINVON: Logical variable. If TRUE, inventory beetle attacks are
!             included, subject to the constraint of DEDYRS.
!     DEDYRS: Years prior to first master cycle, from which inventoried
!             beetle attack will be used to initialize the model.
!     DDECRT: Array containing annual decay rates of dead woody pool
!             size classes (dbh) -- Downed wood
!     SDECRT: Same except for Standing dead wood
!     FALLRT: Falldown rate by pool categories for standing to dead

!  Beetle Model (stand level) Common block variables:

!     ALLKLL: Array holding number of stems killed by Ips in each size class
!     ATRPHE: Attractant pheromone...
!     AVDBH:  basal-area-weighted average of SCDBH across the size classes
!     AVHIGH: stand-area-weighted average of HIGH
!     AVSDI:  stand-area-weighted average value of BMSDI
!     AVWET:  stand-area-weighted average of WET
!     BASTD:  Array containg the total basal area of each stand. (AJM 7/1/99)
!     BKP:    Array containing the total BKP
!     BKPIps: Array containing the total BKP for Ips
!     BMSDI:  Beetle Model's Stand Density Index for each stand,
!             calculated from TREE (host + nonhost), BAH and BANH so that it
!             is updated for beetle activity during the growth cycle.
!     CRS:    Array containing tree crowns (compressed ICR variable)
!     CVOLREM Array holding volume removals for the master cycle (old VOLREM array)
!             AJM 6/27/05
!     DDWP:   Array to hold volumes of downed dead wood
!     DWPHOS: Array to hold volumes of downed host tree volume for
!             Ips (stratified by standing/dead and size class)
!     FINAL:  arrray containt information about the final tree killed in a stand.
!             1=BKP used, 2=size class attacked, 3=tpa killed
!     GRF:    Size-class GRF, indexed by stand
!     SDMR:   Size-class DMR, indexed by stand
!     SRR:    Size-class Root disease, indexed by stand
!     SSR:    Size-class Stem rust, indexed by stand
!     GRFSTD: Stand-level GRF
!     HGS:    Average height growth of trees in the size class
!     HTS:    Average height of trees in the size-class
!     IQPTYP: Quality pool type for Standing dead wood (based on most
!             numerous tree species)
!     LFAC:   0:50 array of log(factorial(i))
!     LCDENS: Logical variable for whether to include stand density effects on RV
!     NUMER:  Array of Numerators for scoring equation for #special trees.
!             First element is for MPB/WPB; second is for Ips.
!     NYR:    Number of years into a defoliator outbreak
!     OAKILL: Array containing the density of trees in each size class
!             and tree type killed by "other agents" (i.e. not pine beetles)
!     OLDBKP: Array containing the amount of BKP in the stand before dispersal
!     OTHATT: Proportion of trees in each size class attacked but not killed
!     OTPA:   Original TPA at beginning of master cycle.
!     PBKILL: Array containing prop'n of pine beetle killed trees
!             in each size class
!     PITCH:  Array containing density? of pitchout trees (trees attacked
!             but able to "pitch out" beetles) by size class
!     PSLASH: Array containing *******CHECK WITH SARAH FOR INTERP***
!     REPPHE: Repellant pheromone...
!     RVDNST: Rating value due to stand density.  New stand-level function
!             added 7/1/99 to replace individual-based function. (AJM)
!     RVDSC:  Rating Value due to Drought in each Size Class in the current
!             year (dimensioned by stand and size class)
!     RVDFOL: Rating value due to defoliators
!     SC:     Proportion of trees in size class severely scorched by fire
!     SCDBH:  DBH of a tree with the average basal area of the Size Class
!     SDDBH:  standard deviation of SCDBH (basal-area-weighted)
!     SDHIGH: standard deviation of HIGH (stand-area-weighted)
!     SDSDI:  standard deviation of BMSDI (stand-area-weighted)
!     SDWET:  standard deviation of WET (stand-area-weighted)
!     SDWP:   Array to hold volumes of standing dead wood
!     SPCLT:  Array containing the prop'n of special trees in each dbh class
!     SREMOV: Total stems removed from harvesting (Sanitation cuts)
!     SSBATK: Array containing BA killed (by stand).  Zeroed out the
!             following year.  Added 8/99 (AJM).
!     STRIKE: Array containing proportion of trees in each size class struck
!             by lightning
!     TOTKILL Array holding proportion of stems top-killed by Ips in
!             each size class
!     TFOOD:  Total beetle attractive 'food'
!     TPBK:   Predicted mortality for FAST(1), SLOW(2) and BEETLE(3)
!             processes, where the number is the fourth index of the var,
!             accumulated annually over the master cycle.
!     TREE:   TPA in the various size classes, in each stand, by host/non-host
!     TREEND: TPA that are attacked with the last bit of BKP
!     TRURVD: the 'true' rating value due to drought of each size class
!             in each stand, as calculated by DMDRGT.  Unlike RVDSC, this
!             value can be > 1 and < 0.01.  It is this value that is used
!             to provide memory of drought effects between years (i.e. this
!             year's TRURVD is the average of the rating value due to
!             drought effects in the current year and last year's TRURVD).
!     TVOL:   Total cubic ft volume of average tree in size class
!     VOLREM: Total cubic ft volume removed each year (1=SANI. 2=SALV)
!     WASDRY: logical variable that indicates whether the drought model
!             was run last year (from 'WAS DRY')
!     WPBA:   Array containing the BA in each Dead Woody Pool size class
!     LBMDAM: LOGICAL array indicating WPB-attacked records in inventory
!     LBMINT  logical variable controlling model initialization. True
!             means subroutine BMINIT needs to be called.
!     ATREEI  Array containing the initial condition, at the begining of the
!             cycle, of the stands, i.e. basal area by size class of host trees
!             (RNH Aug98).
!     AREMS   Array containing the accumulated sum of trees removed during
!             Salvage actions. By stand by size class.  Host tree is implicit.
!    MYLSTS   Temporary array to hold the list of stands (from MYLST) slated
!             for sanitization during the current year(RNH Aug98).
!    MAXSCS   Maximum size class for Sanitation to be passed from SANITIZE
!             to BMKILL modules (RNH Aug98).
!    MINSCS   Miximum size class for Sanitation to be passed from SANITIZE
!             to BMKILL modules (RNH Aug98).
!    LOKS     Logical variable to trigger Sanitization accounting in BMKILL
!    LOKS1    Logical turns off LOKS if no SANI during cycle

! PPBMCC variables

! Variables
character(len=4) :: BMMGID
character(len=26) :: BMSTDS
character(len=26) :: NSSTDS
character(len=26) :: WTSTDS

! PPBMCOM variables---LANDSCAPE SCALE

logical :: ACDONE
logical :: DRYON
logical :: IPSON
logical :: LBAD
logical :: LBMDEB
logical :: LBMSETP !AJM DEC 05 FOR BMSETP ROUTINE, BYPASSES PARTS ONCE DONE
logical :: LBMSPR
logical :: LCDENS
logical :: LNEAR
logical :: LINVON
logical :: OUTOFF
logical :: WASDRY

integer :: BMEND
integer :: BMCNT
integer :: BMSTND
integer :: DEDYRS
integer :: HSPEC(3,MAXSP)
integer :: IBADBB
integer :: IBMYR1,IBMYR2,IBMMRT
integer :: IPSMIN,IPSMAX
integer :: ISCMIN(3)
integer :: ISPFLL(MAXSP)
integer :: JBMBPR
integer :: L2D(NSCL)
integer :: MAXSCS,MINSCS
integer :: NBGEN
integer :: NIBGEN
integer :: NSSTND
integer :: PBSPEC
integer :: SCMIN
integer :: UPSIZ(NSCL)
integer :: WTCNT
integer :: WPSIZ(MXDWHZ)

real    :: AREAL,AREALP(2),AREALS,AREAO(2)
real    :: AVHIGH,AVSDI,AVWET
real    :: BADREP(3)
real    :: CBAHO(2),CBAO,CBASPO(2),CSPO(2)
real    :: CBKPO(2),CRVOND
real    :: CRASH
real    :: DBHMAX
real    :: DDECRT(MXDWSZ)
real    :: DENS
real    :: FALLRT(MXDWPC)
real    :: INC(3,NSCL)
real    :: MSBA(NSCL)
real    :: PFSLSH
real    :: REPLAC
real    :: REPMAX
real    :: RADL
real    :: RADLC
real    :: RVOD
real    :: SDECRT(MXDWSZ)
real    :: SELFA(3)
real    :: STOCKO
real    :: STKACR !AJM DEC 05 SEE BMSETP (STOCKED ACRES)
real    :: TACRES ! HERETOFORE TREATED AS STOCKED ACRES BUT WAS ALWAYS ACTUALLY TOTAL ACRES
real    :: UPBA(NSCL)
real    :: USERA(3)
real    :: USERC(3)
real    :: WTHRSH

! BMCOM variables--STAND-SCALE

logical :: LBMDAM(MAXTRE)
logical :: LBMINT,LOKS,LOKS1
logical :: STOCK(MXSTND)

integer :: BMSDIX(MXSTND)
integer :: IQPTYP(MXSTND,2)
integer :: MYLSTS(MXSTND)
integer :: NSSDIX(MXSTND)
integer :: NYR(MXSTND)

real    :: ALLKLL(MXSTND,NSCL+1)
real    :: AREMS(MXSTND,NSCL+1)
real    :: ATREEI(MXSTND,NSCL+1)
real    :: ATRPHE(MXSTND)
real    :: ATTBYK(2,MXSTND)
real    :: ATTC(2,MXSTND)
real    :: AVDBH(MXSTND)
real    :: BAH(MXSTND,NSCL+1)
real    :: BANH(MXSTND,NSCL+1)
real    :: BASTD(MXSTND)
real    :: BKP(MXSTND)
real    :: BKPIPS(MXSTND)
real    :: BMINI(MXSTND,3,2)
real    :: BMSDI(MXSTND)
real    :: CRS(MXSTND,NSCL,2)
real    :: CVOLREM(MXSTND,2) ! REPLACES VOLREM, USED IN BMKILL (VOLREM NOW ANNUAL FOR OUTPUT)
real    :: DDWP(MXSTND,MXDWSZ,MXDWAG)
real    :: DWPHOS(MXSTND,MXDWHC,MXDWHZ)
real    :: DNSTD(MXSTND,NSCL+1)
real    :: FINAL(MXSTND,3)
real    :: GRF(MXSTND,NSCL)
real    :: GRFDEN(MAXTRE)
real    :: GRFSTD(MXSTND)
real    :: HGS(MXSTND,NSCL,2)
real    :: HIGH(MXSTND)
real    :: HTS(MXSTND,NSCL,2)
real    :: NUMER(2,MXSTND)
real    :: OAKILL(MXSTND,NSCL,2)
real    :: OLDBKP(MXSTND)
real    :: OTHATT(MXSTND,NSCL)
real    :: OTPA(MXSTND,NSCL,2)
real    :: PBKILL(MXSTND,NSCL)
real    :: PITCH(MXSTND,NSCL)
real    :: PSLASH(MXSTND,MXDWHC,MXDWSZ)
real    :: REPPHE(MXSTND)
real    :: RVDFOL(MXSTND,NSCL)
real    :: RVDNST(MXSTND)
real    :: RVDSC(MXSTND,NSCL)
real    :: SCDBH(MXSTND,NSCL)
real    :: SDDBH(MXSTND)
real    :: SDHIGH,SDSDI,SDWET
real    :: SDMR(MXSTND,NSCL)
real    :: SCORCH(MXSTND,NSCL)
real    :: SDD !(MJO July98)
real    :: SDWP(MXSTND,MXDWPC,MXDWHZ+1,MXDWAG)
real    :: SPCLT(MXSTND,NSCL,2)
real    :: SPRAY(MXSTND,2)
real    :: SREMOV(MXSTND)
real    :: SRR(MXSTND,NSCL),SSR(MXSTND,NSCL)
real    :: SSBATK(MXSTND)
real    :: STRIKE(MXSTND,NSCL)
real    :: STRIP(MXSTND,NSCL)
real    :: TFOOD(MXSTND,2)
real    :: TOPKLL(MXSTND,NSCL+1)
real    :: TPBK(MXSTND,NSCL,2,3)
real    :: TREE(MXSTND,NSCL+1,2)
real    :: TRURVD(MXSTND,NSCL)
real    :: TVOL(MXSTND,NSCL,2)
real    :: URMAX(3)
real    :: UFLOAT !BMATCT, BMINIT, BMPPIN
real    :: WET(MXSTND)
real    :: WPBA(MXDWSZ)
real    :: WTSC(MXSTND)

! The following added for BMFDBK keyword (MJO, May98)

integer :: LAG
logical :: LFDBK
real    :: LSBKPA
real    :: LSBKPB
real    :: SFDBK
real    :: TFDBK

common /PPBMCC/ BMSTDS(MXSTND), BMMGID(MXSTND), NSSTDS(MXSTND),&
     &WTSTDS(MXSTND)

common/PPBMCOM/&
     &ACDONE,  AREAL, AREALP, AREALS, AREAO,AVHIGH, AVSDI, AVWET,&
     &BADREP,  BMCNT,  BMEND, BMSTND,&
     &CBAHO,    CBAO, CBASPO,   CSPO, CBKPO, CRVOND, CRASH,&
     &DBHMAX, DDECRT, DEDYRS,   DENS, DRYON,&
     &FALLRT,  HSPEC,&
     &IBADBB, IBMYR1, IBMYR2, IBMMRT,&
     &INC,    IPSMIN, IPSMAX,  IPSON, ISCMIN, ISPFLL, JBMBPR,&
     &LBAD,   LBMDEB, LBMSPR, LCDENS, LINVON,  LNEAR,    L2D,&
     &LBMSETP,&
     &MAXSCS, MINSCS,   MSBA,  NBGEN, NIBGEN, NSSTND,&
     &OUTOFF, PBSPEC, PFSLSH,&
     &RADL,    RADLC, REPLAC, REPMAX, RVOD,&
     &STKACR, STOCKO,  SCMIN, SDECRT,  SELFA,&
     &TACRES,   UPBA,  UPSIZ,  USERA, USERC,&
     &WPSIZ,  WTHRSH, WASDRY,  WTCNT

common/BMCOM/&
     &ALLKLL, ATRPHE,   ATTC, ATTBYK, ATREEI,  AREMS,  AVDBH,&
     &BAH,   BANH,  BASTD,    BKP, BKPIPS,  BMINI, BMSDIX, BMSDI,&
     &CRS,CVOLREM,   DDWP,  DNSTD, DWPHOS,  FINAL,&
     &GRF, GRFDEN, GRFSTD,    HGS,   HIGH,    HTS, IQPTYP,&
     &LAG, LBMDAM, LBMINT,  LFDBK,   LOKS,  LOKS1, LSBKPA,&
     &LSBKPB, MYLSTS,  NUMER, NSSDIX,    NYR,&
     &OAKILL, OLDBKP, OTHATT,   OTPA, PBKILL,  PITCH, PSLASH,&
     &REPPHE, RVDNST, RVDFOL,  RVDSC,&
     &SCDBH, SCORCH,    SDD,  SDDBH, SDHIGH,   SDMR,  SDSDI, SDWET,&
     &SDWP,  SFDBK,  SPCLT,  SPRAY, SREMOV,    SRR, SSBATK,   SSR,&
     &STRIKE,  STRIP,  STOCK,&
     &TFDBK,  TFOOD, TOPKLL,   TPBK,   TREE, TRURVD,   TVOL,&
     &UFLOAT,  URMAX,    WET,   WTSC,   WPBA

!.... End segment.
