!==========================!
!   vdbsqlite : dbssumry   !
!==========================!
! VDBSQLITE $Id$
subroutine DBSSUMRY(IYEAR, IAGE, NPLT, ITPA, IBA,&
     &ISDI, ICCF, ITOPHT,FQMD, ITCUFT,&
     &IMCUFT, ISCUFT, IBDFT, IRTPA, IRTCUFT,&
     &IRMCUFT, IRSCUFT, IRBDFT, IATBA, IATSDI,&
     &IATCCF, IATTOPHT, FATQMD, IPRDLEN, IACC,&
     &IMORT, YMAI, IFORTP, ISZCL, ISTCL)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
  !              OUTPUT.

  !     THE METRIC VARIANTS (BC, ON; LMETRICVAR) WRITE TABLE FVS_Summary_Metric,
  !     OR FVS_Summary_East_Metric FOR ON. THEIR CALLER PASSES VALUES ALREADY
  !     CONVERTED TO METRIC UNITS; ISCUFT AND IRSCUFT ARE NOT USED THERE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,IAGE,IPRDLEN,IACC,IMORT,ITPA,IBA,ISDI,ICCF,ITOPHT,ITCUFT,IMCUFT,ISCUFT,IBDFT,IRTPA,&
       &IRTCUFT,IRMCUFT,IRSCUFT,IRBDFT,IATBA,IATSDI,IATCCF,IATTOPHT,IFORTP,ISZCL,ISTCL
  integer :: ColNumber,iRet
  double precision :: FQMDB,FATQMDB,YMAIB
  real    :: FQMD,FATQMD,YMAI
  character(len=2000) :: SQLStmtStr
  character(len=*) :: NPLT

  !OMMONS END

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_addcolifabsent

  if (ISUMARY.ne.1) return

  call DBSCASE(1)

  !     DEFINE TABLENAME

  if (LMETRICVAR) then
     if ((VARACD .eq. 'CS') .or. (VARACD .eq. 'LS') .or.&
          &(VARACD .eq. 'NE') .or. (VARACD .eq. 'SN') .or.&
          &(VARACD .eq. 'ON')) then
        iRet = fsql3_tableexists(IoutDBref,&
             &'FVS_Summary_East_Metric'//CHAR(0))
     else
        iRet = fsql3_tableexists(IoutDBref,'FVS_Summary_Metric'//CHAR(0))
     end if
  else
     iRet = fsql3_tableexists(IoutDBref,'FVS_Summary'//CHAR(0))
  end if

  if (iRet.eq.0) then

     if (LMETRICVAR) then

        !       EASTERN VARIANT VOLUME NOMENCLATURE

        if ((VARACD .eq. 'CS') .or. (VARACD .eq. 'LS') .or.&
             &(VARACD .eq. 'NE') .or. (VARACD .eq. 'SN') .or.&
             &(VARACD .eq. 'ON')) then

           SQLStmtStr = 'CREATE TABLE FVS_Summary_East_Metric('//&
                &'CaseID text not null,'//&
                &'StandID text not null,'//&
                &'Year int,'//&
                &'Age int,'//&
                &'Tph int,'//&
                &'BA int,'//&
                &'SDI int,'//&
                &'CCF int,'//&
                &'TopHt int,'//&
                &'QMD real,'//&
                &'TCuM int,'//&
                &'MCuM int,'//&
                &'CCuM int,'//&
                &'RTph int,'//&
                &'RTCuM int,'//&
                &'RMCuM int,'//&
                &'RCCuM int,'//&
                &'ATBA int,'//&
                &'ATSDI int,'//&
                &'ATCCF int,'//&
                &'ATTopHt int,'//&
                &'ATQMD real,'//&
                &'PrdLen int,'//&
                &'Acc int,'//&
                &'Mort int,'//&
                &'MAI real,'//&
                &'ForTyp int,'//&
                &'SizeCls int,'//&
                &'StkCls int);'//CHAR(0)
        else

           !       WESTERN VARIANT VOLUME NOMENCLATURE

           SQLStmtStr = 'CREATE TABLE FVS_Summary_Metric('//&
                &'CaseID text not null,'//&
                &'StandID text not null,'//&
                &'Year int,'//&
                &'Age int,'//&
                &'Tph int,'//&
                &'BA int,'//&
                &'SDI int,'//&
                &'CCF int,'//&
                &'TopHt int,'//&
                &'QMD real,'//&
                &'TCuM int,'//&
                &'MCuM int,'//&
                &'BdNA int,'//&
                &'RTph int,'//&
                &'RTCuM int,'//&
                &'RMCuM int,'//&
                &'RBdNA int,'//&
                &'ATBA int,'//&
                &'ATSDI int,'//&
                &'ATCCF int,'//&
                &'ATTopHt int,'//&
                &'ATQMD real,'//&
                &'PrdLen int,'//&
                &'Acc int,'//&
                &'Mort int,'//&
                &'MAI real,'//&
                &'ForTyp int,'//&
                &'SizeCls int,'//&
                &'StkCls int);'//CHAR(0)
        end if
     else

        SQLStmtStr = 'CREATE TABLE FVS_Summary('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int,'//&
             &'Age int,'//&
             &'Tpa int,'//&
             &'BA int,'//&
             &'SDI int,'//&
             &'CCF int,'//&
             &'TopHt int,'//&
             &'QMD real,'//&
             &'TCuFt int,'//&
             &'MCuFt int,'//&
             &'SCuFt int,'//&
             &'BdFt int,'//&
             &'RTpa int,'//&
             &'RTCuFt int,'//&
             &'RMCuFt int,'//&
             &'RSCuFt int,'//&
             &'RBdFt int,'//&
             &'ATBA int,'//&
             &'ATSDI int,'//&
             &'ATCCF int,'//&
             &'ATTopHt int,'//&
             &'ATQMD real,'//&
             &'PrdLen int,'//&
             &'Acc int,'//&
             &'Mort int,'//&
             &'MAI real,'//&
             &'ForTyp int,'//&
             &'SizeCls int,'//&
             &'StkCls int);'//CHAR(0)
     end if

     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        ISUMARY = 0
        return
     end if
  end if

  !     CHECK TABLE FOR COLUMN(S) ADDED WITH NVB UPGRADE (2024)
  !     `SCuFt`,
  !     `RSCuFt`,
  !     TO ACCOUNT FOR ADDING TO DATABASE CREATED PROIR TO UPGRADE

  if (.not. LMETRICVAR) then
     iRet = fsql3_addcolifabsent(IoutDBref,"FVS_Summary"//CHAR(0),&
          &"SCuFt"//CHAR(0),"int"//CHAR(0))

     iRet = fsql3_addcolifabsent(IoutDBref,"FVS_Summary"//CHAR(0),&
          &"RSCuFt"//CHAR(0),"int"//CHAR(0))
  end if

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  FQMDB = FQMD
  FATQMDB = FATQMD
  YMAIB = YMAI

  if (LMETRICVAR) then
     if ((VARACD .eq. 'CS') .or. (VARACD .eq. 'LS') .or.&
          &(VARACD .eq. 'NE') .or. (VARACD .eq. 'SN') .or.&
          &(VARACD .eq. 'ON')) then

        write(SQLStmtStr,*)'INSERT INTO FVS_Summary_East_Metric',&
             &' (CaseID,StandID,',&
             &'Year,Age,Tph,BA,SDI,CCF,TopHt,QMD,MCuM,SCuM,NCuM,',&
             &'RTph,RMCuM,RSCuM,RNCuM,ATBA,ATSDI,ATCCF,ATTopHt,',&
             &'ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls)',&
             &'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,',&
             &'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
     else
        write(SQLStmtStr,*)'INSERT INTO FVS_Summary_Metric',&
             &' (CaseID,StandID,',&
             &'Year,Age,Tph,BA,SDI,CCF,TopHt,QMD,TCuM,MCuM,BdNA,',&
             &'RTph,RTCuM,RMCuM,RBdNA,ATBA,ATSDI,ATCCF,ATTopHt,',&
             &'ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls)',&
             &'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,',&
             &'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
     end if
  else
     write(SQLStmtStr,*)'INSERT INTO FVS_Summary',&
          &' (CaseID,StandID,Year,Age,Tpa,',& !L1 C5
          &'BA,SDI,CCF,TopHt,QMD,',& !L2 C5
          &'TCuFt,MCuFt,SCuFt,BdFt,',& !L3 C4
          &'RTpa,RTCuFt,RMCuFt,RSCuFt,RBdFt,',& !L6 C5
          &'ATBA,ATSDI,ATCCF,ATTopHt,ATQMD,',& !L9 C5
          &'PrdLen,Acc,Mort,MAI,ForTyp,',& !L10 C5
          &'SizeCls,StkCls)',& !L11 C2
          &'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,',& !L1 = 5
          &'?,?,?,?,?,',& !L2 = 5
          &'?,?,?,?,',& !L3 = 4
          &'?,?,?,?,?,',& !L6 = 5
          &'?,?,?,?,?,',& !L9 = 5
          &'?,?,?,?,?,',& !L10 = 5
          &'?,?);' !L11 = 2
  end if

  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     ISUMARY = 0
     return
  end if

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IAGE)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ITPA)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IBA)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISDI)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ICCF)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ITOPHT)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,FQMDB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ITCUFT)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IMCUFT)

  if (.not. LMETRICVAR) then
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ISCUFT)
  end if

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDFT)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IRTPA)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IRTCUFT)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IRMCUFT)

  if (.not. LMETRICVAR) then
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IRSCUFT)
  end if

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IRBDFT)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IATBA)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IATSDI)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IATCCF)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IATTOPHT)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,FATQMDB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IPRDLEN)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IACC)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IMORT)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,YMAIB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IFORTP)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISZCL)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTCL)

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     ISUMARY = 0
  end if
  return
end

subroutine DBSSUMRY2
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH SUMMARY STATISTICS

  !     THE METRIC VARIANTS (BC, ON; LMETRICVAR) WRITE TABLE FVS_Summary2_Metric,
  !     OR FVS_Summary2_East_Metric FOR ON, IN METRIC UNITS. THEY HAVE NO SCUFT,
  !     SDI-METHOD OR GMD COLUMNS, AND THEY REPORT ACCRETION AND MORTALITY ON
  !     THE LAST (END OF PROJECTION) RECORD TOO. THESE DIFFERENCES ARE KEPT
  !     AS THEY WERE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'
  include 'OPCOM.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'SUMTAB.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IYEAR,ICCF,ITOPHT,IOSDI,IPRDLEN,IHRVC,IAGEOUT,IFRTP,SDIX,IZSDI,IRSDI
  double precision :: DPTPA,DPTPTPA,DPBA,DPQMD,DPTCUFT,DPTPTCUFT,DPMCUFT,DPTPMCUFT,DPBDFT,DPTPBDFT,&
       &DPACC,DPMORT,DPMAI,DPRTPA,DPRTCUFT,DPRMCUFT,DPRBDFT,DPSCUFT,DPTPSCUFT,DPRSCUFT,DPRELDEN,DPDR016
  double precision :: DX
  integer :: ColNumber,iRet,I
  integer :: IX
  character(len=2000) :: SQLStmtStr
  character(len=24) :: TABLENAME

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_addcolifabsent

  if (ISUMARY.ne.2) return

  IYEAR = IY(ICYC)
  IAGEOUT = IOSUM(2,ICYC)
  ICCF = IBTCCF(ICYC)
  ITOPHT = IBTAVH(ICYC)
  IOSDI = ISDI(ICYC)
  if (.not. LMETRICVAR) then
     IZSDI = NINT(SDIBC2)
     IRSDI = NINT(SDIBC)
  end if
  DPTPA = OLDTPA/GROSPC
  DPBA = OLDBA/GROSPC
  DPQMD = ORMSQD
  if (.not. LMETRICVAR) DPDR016 = ODR016
  IHRVC = 0
  if (ICYC.gt.NCYC) then
     DPTCUFT = OCVCUR(7)/GROSPC
     DPMCUFT = OMCCUR(7)/GROSPC
     if (.not. LMETRICVAR) DPSCUFT = OSCCUR(7)/GROSPC
     DPBDFT = OBFCUR(7)/GROSPC
  else
     DPTCUFT = TSTV1(4)
     DPMCUFT = TSTV1(5)
     if (.not. LMETRICVAR) DPSCUFT = TSTV1(20)
     DPBDFT = TSTV1(6)
  end if
  DPTPTPA = DPTPA   + (TRTPA/GROSPC)
  DPTPTCUFT = DPTCUFT + (TRTCUFT/GROSPC)
  DPTPMCUFT = DPMCUFT + (TRMCUFT/GROSPC)
  if (.not. LMETRICVAR) DPTPSCUFT = DPSCUFT + (TRSCUFT/GROSPC)
  DPTPBDFT = DPBDFT  + (TRBDFT/GROSPC)
  DPRTPA = 0.
  DPRTCUFT = 0.
  DPRMCUFT = 0.
  if (.not. LMETRICVAR) DPRSCUFT = 0.
  DPRBDFT = 0.
  IPRDLEN = 0
  DPACC = 0.
  if (.not. LMETRICVAR) DPMORT = 0.
  IPRDLEN = IOSUM(14,ICYC)
  if (LMETRICVAR) then
     DPACC = OACC(7)/GROSPC
     DPMORT = OMORT(7)/GROSPC
  else
     SDIX = NINT(BTSDIX)
     DPRELDEN = real(IOSDI)/BTSDIX
     if (ICYC.le.NCYC) then
        !       NO ACCCRETION OR MORTALITY ON LAST RECORD OF SUMMARY (END OF PROJECTION)
        DPACC = OACC(7)/GROSPC
        DPMORT = OMORT(7)/GROSPC
     end if
  end if
  DPMAI = BCYMAI(ICYC)
  if (.not. LMETRICVAR) IFRTP = IOSUM(18,ICYC) ! added 1/18/2014
  if (ICYC.le.NCYC) then
     DPRTPA = ONTREM(7)/GROSPC
     DPRTCUFT = OCVREM(7)/GROSPC
     DPRMCUFT = OMCREM(7)/GROSPC
     if (.not. LMETRICVAR) DPRSCUFT = OSCREM(7)/GROSPC
     DPRBDFT = OBFREM(7)/GROSPC
     if (DPRTPA.le.0.) then
        IPRDLEN = IOSUM(14,ICYC)
        DPACC = OACC(7)/GROSPC
        DPMORT = OMORT(7)/GROSPC
        DPMAI = BCYMAI(ICYC)
     else
        IHRVC = 1
     end if
  end if

  call DBSCASE(1)

  !     DEFINE TABLENAME

  if (LMETRICVAR) then
     if ((VARACD .eq. 'CS') .or. (VARACD .eq. 'LS') .or.&
          &(VARACD .eq. 'NE') .or. (VARACD .eq. 'SN') .or.&
          &(VARACD .eq. 'ON')) then
        TABLENAME = 'FVS_Summary2_East_Metric'
     else
        TABLENAME = 'FVS_Summary2_Metric'
     end if
  else
     TABLENAME = 'FVS_Summary2'
  end if

  iRet = fsql3_tableexists(IoutDBref,TRIM(TABLENAME)//CHAR(0))
  if (iRet.eq.0) then

     if (LMETRICVAR) then

        !       EASTERN VARIANT VOLUME NOMENCLATURE

        if ((VARACD .eq. 'CS') .or. (VARACD .eq. 'LS') .or.&
             &(VARACD .eq. 'NE') .or. (VARACD .eq. 'SN') .or.&
             &(VARACD .eq. 'ON')) then

           SQLStmtStr = 'CREATE TABLE '//TRIM(TABLENAME)//&
                &' (CaseID text not null,'//&
                &'StandID text not null,'//&
                &'Year int,'//&
                &'RmvCode int,'//&
                &'Age int,'//&
                &'Tph real,'//&
                &'TPrdTph real,'//&
                &'BA real,'//&
                &'SDI int,'//&
                &'CCF int,'//&
                &'TopHt int,'//&
                &'QMD real,'//&
                &'MCuM real,'//&
                &'TPrdMCuM real,'//&
                &'SCuM real,'//&
                &'TPrdSCuM real,'//&
                &'NCuM real,'//&
                &'TPrdNCuM real,'//&
                &'RTph real,'//&
                &'RMCuM real,'//&
                &'RSCuM real,'//&
                &'RNCuM real,'//&
                &'PrdLen int,'//&
                &'Acc real,'//&
                &'Mort real,'//&
                &'MAI real,'//&
                &'ForTyp int,'//&
                &'SizeCls int,'//&
                &'StkCls int);'//CHAR(0)
        else

           !       WESTERN VARIANT VOLUME NOMENCLATURE

           SQLStmtStr = 'CREATE TABLE '//TRIM(TABLENAME)//&
                &' (CaseID text not null,'//&
                &'StandID text not null,'//&
                &'Year int,'//&
                &'RmvCode int,'//&
                &'Age int,'//&
                &'Tph real,'//&
                &'TPrdTph real,'//&
                &'BA real,'//&
                &'SDI int,'//&
                &'CCF int,'//&
                &'TopHt int,'//&
                &'QMD real,'//&
                &'TCuM real,'//&
                &'TPrdTCuM real,'//&
                &'MCuM real,'//&
                &'TPrdMCuM real,'//&
                &'NABdM real,'//&
                &'TPrdNABdM real,'//&
                &'RTph real,'//&
                &'RTCuM real,'//&
                &'RMCuM real,'//&
                &'RNABdM real,'//&
                &'PrdLen int,'//&
                &'Acc real,'//&
                &'Mort real,'//&
                &'MAI real,'//&
                &'ForTyp int,'//&
                &'SizeCls int,'//&
                &'StkCls int);'//CHAR(0)
        end if
     else

        SQLStmtStr = 'CREATE TABLE '//TRIM(TABLENAME)//&
             &' (CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int,'//&
             &'RmvCode int,'//&
             &'Age int,'//&
             &'Tpa real,'//&
             &'TPrdTpa real,'//&
             &'BA real,'//&
             &'SDI int,'//&
             &'ZeideSDI int,'//&
             &'ReinekeSDI int,'//&
             &'SDIMax int,'//&
             &'RDSDI real,'//&
             &'CCF int,'//&
             &'TopHt int,'//&
             &'QMD real,'//&
             &'GMD real,' //&
             &'TCuFt real,'//&
             &'TPrdTCuFt real,'//&
             &'MCuFt real,'//&
             &'TPrdMCuFt real,'//&
             &'SCuFt real,'//&
             &'TPrdSCuFt real,'//&
             &'BdFt real,'//&
             &'TPrdBdFt real,'//&
             &'RTpa real,'//&
             &'RTCuFt real,'//&
             &'RMCuFt real,'//&
             &'RSCuFt real,'//&
             &'RBdFt real,'//&
             &'PrdLen int,'//&
             &'Acc real,'//&
             &'Mort real,'//&
             &'MAI real,'//&
             &'ForTyp int,'//&
             &'SizeCls int,'//&
             &'StkCls int);'//CHAR(0)
     end if

     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        ISUMARY = 0
        return
     end if
  end if

  !     CHECK EXISTING TABLE FOR COLUMN(S) ADDED WITH NVB UPGRADE (2024)
  !     `SCuFt`,
  !     `TPrdSCuFt`,
  !     `RSCuFt`,
  !     TO ACCOUNT FOR ADDING TO DATABASE CREATED PROIR TO UPGRADE
  if (.not. LMETRICVAR) then
     iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),&
          &"SCuFt"//CHAR(0),"real"//CHAR(0))

     iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),&
          &"TPrdSCuFt"//CHAR(0),"real"//CHAR(0))

     iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),&
          &"RSCuFt"//CHAR(0),"real"//CHAR(0))

     iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),&
          &"SDIMax"//CHAR(0),"int"//CHAR(0))

     iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),&
          &"RDSDI"//CHAR(0),"real"//CHAR(0))

     iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),&
          &"GMD"//CHAR(0),"real"//CHAR(0))

     iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),&
          &"ZeideSDI"//CHAR(0),"int"//CHAR(0))

     iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),&
          &"ReinekeSDI"//CHAR(0),"int"//CHAR(0))
  end if

  do I = 1,2
     if (IHRVC.eq.1) then
        DPTPTPA = DPTPTPA   - DPRTPA
        DPTPTCUFT = DPTPTCUFT - DPRTCUFT
        DPTPMCUFT = DPTPMCUFT - DPRMCUFT
        if (.not. LMETRICVAR) DPTPSCUFT = DPTPSCUFT - DPRSCUFT
        DPTPBDFT = DPTPBDFT  - DPRBDFT
     end if

     if (LMETRICVAR) then
        if ((VARACD .eq. 'CS') .or. (VARACD .eq. 'LS') .or.&
             &(VARACD .eq. 'NE') .or. (VARACD .eq. 'SN') .or.&
             &(VARACD .eq. 'ON')) then

           SQLStmtStr = 'INSERT INTO '//TRIM(TABLENAME)//&
                &' (CaseID,StandID,Year,RmvCode,Age,Tph,TPrdTph,BA,SDI,'//&
                &'CCF,TopHt,QMD,MCuM,TPrdMCuM,SCuM,TPrdSCuM,NCuM,'//&
                &'TPrdNCuM,RTph,RMCuM,RSCuM,RNCuM,'//&
                &'PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls'//&
                &")VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,?,?,?,"//&
                &'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'&
                &//CHAR(0)
        else
           SQLStmtStr = 'INSERT INTO '//TRIM(TABLENAME)//&
                &' (CaseID,StandID,Year,RmvCode,Age,Tph,TPrdTph,BA,SDI,'//&
                &'CCF,TopHt,QMD,TCuM,TPrdTCuM,MCuM,TPrdMCuM,NABdM,'//&
                &'TPrdNABdM,RTph,RTCuM,RMCuM,RNABdM,'//&
                &'PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls'//&
                &")VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,?,?,?,"//&
                &'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'&
                &//CHAR(0)
        end if
     else
        SQLStmtStr = 'INSERT INTO '//TRIM(TABLENAME)//&
             &' (CaseID,StandID,Year,RmvCode,Age,Tpa,TPrdTpa,BA,SDI,'//&
             &'ZeideSDI,ReinekeSDI,SDIMax,RDSDI,'//&
             &'CCF,TopHt,QMD,GMD,TCuFt,TPrdTCuFt,MCuFt,TPrdMCuFt,'//&
             &'SCuFt,TPrdSCuFt,BdFt,'//&
             &'TPrdBdFt,RTpa,RTCuFt,RMCuFt,RSCuFt,RBdFt,'//&
             &'PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls'//&
             &")VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,?,?,?,?,?,"//&
             &'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'&
             &//CHAR(0)
     end if

     iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        ISUMARY = 0
        return
     end if
     if (LMETRICVAR) then
        ColNumber = 1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IHRVC)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IAGEOUT)

        ColNumber = ColNumber+1
        DX = DPTPA/ACRtoHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPTPTPA/ACRtoHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPBA*FT2pACRtoM2pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        IX = NINT(IOSDI/ACRtoHA)
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IX)

        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ICCF)

        ColNumber = ColNumber+1
        IX = NINT(ITOPHT*FTtoM)
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IX)

        ColNumber = ColNumber+1
        DX = DPQMD*INtoCM
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPTCUFT*FT3pACRtoM3pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPTPTCUFT*FT3pACRtoM3pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPMCUFT*FT3pACRtoM3pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPTPMCUFT*FT3pACRtoM3pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPBDFT*0.0
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPTPBDFT*0.0
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPRTPA/ACRtoHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPRTCUFT*FT3pACRtoM3pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPRMCUFT*FT3pACRtoM3pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPRBDFT*0.0
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IPRDLEN)

        ColNumber = ColNumber+1
        DX = DPACC*FT3pACRtoM3pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPMORT*FT3pACRtoM3pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        DX = DPMAI*FT3pACRtoM3pHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DX)

        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IFORTP)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ISZCL)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTCL)
     else
        ColNumber = 1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IHRVC)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IAGEOUT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPA)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPTPA)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPBA)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IOSDI)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IZSDI)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IRSDI)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,SDIX)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRELDEN)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ICCF)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ITOPHT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPQMD)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPDR016)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTCUFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPTCUFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMCUFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPMCUFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPSCUFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPSCUFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPBDFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPBDFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRTPA)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRTCUFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRMCUFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRSCUFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRBDFT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IPRDLEN)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPACC)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMORT)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMAI)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IFRTP)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ISZCL)
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTCL)
     end if
     iRet = fsql3_step(IoutDBref)
     if (IHRVC.eq.0) exit
     IHRVC = 2
     IOSDI = ISDIAT(ICYC)
     if (.not. LMETRICVAR) then
        IZSDI = NINT(SDIAC2)
        IRSDI = NINT(SDIAC)
     end if
     ICCF = NINT(ATCCF/GROSPC)
     ITOPHT = NINT(ATAVH)
     DPQMD = ATAVD
     if (.not. LMETRICVAR) DPDR016 = ATDR016
     DPBA = ATBA/GROSPC
     DPTPA = ATTPA/GROSPC
     DPTCUFT = MAX(0.,DPTCUFT-DPRTCUFT)
     DPMCUFT = MAX(0.,DPMCUFT-DPRMCUFT)
     if (.not. LMETRICVAR) DPSCUFT = MAX(0.,DPSCUFT-DPRSCUFT)
     DPBDFT = MAX(0.,DPBDFT -DPRBDFT)
     DPRTPA = 0.
     DPRTCUFT = 0.
     DPRMCUFT = 0.
     if (.not. LMETRICVAR) DPRSCUFT = 0.
     DPRBDFT = 0.
     if (.not. LMETRICVAR) then
        SDIX = NINT(ATSDIX)
        DPRELDEN = real(IOSDI)/BTSDIX
     end if
  end do
  iRet = fsql3_finalize(IoutDBref)

  if (iRet.ne.0) then
     ISUMARY = 0
  end if
  return
end
