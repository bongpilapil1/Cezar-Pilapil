# Cezar-Pilapil
My Storage

Simple Subfile program

Physical File - CUSTPF

      **************************************************************
      ** CUSTPF - Customer File (Physical)
      **************************************************************
      *  
     A                                      UNIQUE
     A          R CUSTR                     TEXT('CUSTOMER LIST')
      * 
     A            CUNBR          7S 0       TEXT('Customer No.')
     A            CUFST         15          TEXT('First Name')
     A            CUMID          1          TEXT('Middle Initial')
     A            CULST         15          TEXT('Last Name')
     A            CUPHNO        40          TEXT('Phone Number')
     A            CUEMAIL       40          TEXT('Email Address')
      *		 
      ** Record keys
      * 
     A          K CUNBR
     A          K CULST
     A          K CUFST
     A          K CUMID
     


Logical File - CUSTLF

      **************************************************************
      ** CUSTLF - Customer File (Logical) - by CUNBR
      **************************************************************
      * 	
     A                                      UNIQUE
     A          R CUSTR                     PFILE(CUSTPF)
     A          K CUNBR



Display File - CUSTDSPF

      ***************************************************************
      ** CUSTDSPF - Customer Maintenance 
      ***************************************************************
      *
     A                                      DSPSIZ(24 80 *DS3)
     A          R CUSTSF                    SFL
     A            CUNBR          7S 0H
     A            CUNAME        25A  O  7  9
     A            CUPHNBR       14A  O  7 37
     A            CUEML         25A  O  7 54
     A            SELECT         1A  B  7  6
     A  40                                  DSPATR(PR)
     A  42                                  DSPATR(RI)
     A  40                                  DSPATR(ND)
     A          R CUSTCTL                   SFLCTL(CUSTSF)
     A                                      SFLSIZ(16)
     A                                      SFLPAG(15)
     A                                      CF03
     A                                      CF05
     A                                      CF06
     A                                      ROLLUP(81)
     A                                      ROLLDOWN(82)
     A                                      OVERLAY
     A N20                                  SFLDSP
     A N20                                  SFLDSPCTL
     A  20                                  SFLCLR
     A            RECN           4S 0H      SFLRCDNBR(CURSOR)
     A                                  1 27'Customer Maintenance'
     A                                      COLOR(WHT)
     A                                  5  9'Name'
     A                                  5 37'Phone'
     A                                  5 43'Number'
     A                                  5 54'Email Address'
     A                                  6  9'-------------------------   -------
     A                                      --------   -------------------------
     A                                      -'	
     A                                  3 62'Cursor'
     A                                  3 69'Row:'
     A                                      COLOR(YLW)
     A                                  4 69'Col:'
     A                                      COLOR(YLW)
     A            ROW            2S 0O  3 76COLOR(YLW)
     A            COL            2S 0O  4 76COLOR(YLW)
     A                                  3  6'D=Delete E=Edit'
     A                                      COLOR(BLU)     
     A                                  4  9'Position to:'
     A            POS           25A  B  4 22CHECK(LC)
     A            TOTAL          4  0O  2 74COLOR(YLW)
     A                                  2 58'Number'
     A                                      COLOR(YLW)
     A                                  2 65'Records:'
     A                                      COLOR(YLW)
      *
     A          R CUSTFTR
     A                                      OVERLAY
     A                                 23  2'F3=Exit'
     A                                      COLOR(BLU)
     A  13        DSPMSG        78A  O 24  2DSPATR(RI)
     A                                 23 23'F6=Add'
     A                                      COLOR(BLU)
     A                                 23 11'F5=Refresh'
     A                                      COLOR(BLU)
     A          R CUSTDTL
     A                                      CF03
     A N41                                  CF09
     A                                      CF12
     A                                  1 27'Customer Maintenance'
     A                                      COLOR(WHT)
     A                                 23  2'F3=Exit'
     A                                      COLOR(BLU)
     A  13        DSPMSG        78A  O 24  2DSPATR(RI)
     A                                 23 33'F12=Cancel'
     A                                      COLOR(BLU)
     A                                  6 10'Record ID.....:'
     A            CUNBR          9S 0O  6 26
     A                                  7 10'First Name....:'
     A            CUFST         15A  B  7 26
     A  41                                  DSPATR(PR)
     A                                      CHECK(LC)
     A                                  9 10'Last Name.....:'
     A            CULST         15A  B  9 26
     A  41                                  DSPATR(PR)
     A                                      CHECK(LC)
     A                                  8 10'Middle Initial:'
     A            CUMID          1A  B  8 26
     A  41                                  DSPATR(PR)
     A                                      CHECK(LC)
     A                                 10 10'Phone Number..:'
     A                                 11 10'Email Address.:'
     A            CUPHNO        40A  B 10 26
     A  41                                  DSPATR(PR)
     A                                      CHECK(LC)
     A            CUEMAIL       40A  B 11 26
     A  41                                  DSPATR(PR)
     A                                      CHECK(LC)
     A  41                             14 10'Delete record - are you sure?:'
     A                                      COLOR(RED)
     A  41        DLTFLAG         1A  B 14 41
     A N41                             23 12'F9=Update and exit'
     A                                      COLOR(BLU)



RPG Program - CUSTPGM1

      ***************************************************************
      ** CUSTPGM1 - Customer Maintenance (Main Program)
      ***************************************************************
      *
      ** Customer primary file
     FCUSTPF    if a e           k disk
      *
      ** Customer primary file – by Customer no.
     FCUSTLF    uf   e           k disk
     F                                     rename(CUSTR:CUSTKEY)
      *
      ** Subfile
     FCUSTDSPF  cf   e             workstn
     F                                     sfile(CUSTSF:RecN)
     F                                     infds(FileDs)
      * 
      ** Define row, column, totals, etc
     D FileDs          DS
     D  RowCol               370    371B 0
     D  SFRR#                376    377B 0
     D  TopSF#               378    379B 0
     D  TotSF#               380    381B 0
      * 
      ** Subfile recs per page
     D SFPag           C                   const(15)
      * 
      ** Pgm Variables
     D @EOF            s              1
     D @SFI            s              1
     D Cancel          s              1
      *
      **
     C     *Like         define    CuNbr         @CuNbr
     C     *Like         define    CuNbr         SaveKey
     C     *Like         define    CuFst         SaveFst
     C     *Like         define    CuLst         SaveLst
     C     *Like         define    CuMid         SaveMid
     C     *Like         define    CuPhNo        SavePhone
     C     *Like         define    CuEmail       SaveEmail
     C     *Like         define    CuName        @CuName
      *
     C     *Like         define    RecN          Count                          
     C     *Like         define    RecN          TopCTR                         
     C     *Like         define    RecN          BotCtr                         
     C     *Like         define    RecN          iRow                           
      * 
      ** Load screen (Initialisation)
     C                   exsr      Init
      *
      ** Display screen (loop until F3 pressed)
     C                   exsr      DspScrn
      * 
      ** Loop until F3 pressed
     C                   Dow       *inkc = *Off
      * 
      ** Process input
     C                   select
      *
      ** F3=Exit 
     C                   when      *inkc = *On
     C                   leave
      *
      ** F5=Refresh
     C                   when      *inke = *On
     C                   exsr      Init
      *
      ** Add record
     C                   when      *inkf = *On
     C                   exsr      AddRec
      *
      ** Roll Up
     C                   when      *in81 = *On
     C                   exsr      RollUp
      *
      ** Enter
     C                   other
     C                   exsr      Enter
     C                   endsl
      *
     C                   exsr      DspScrn
      *
     C                   enddo
      * 
      ** End program
     C                   eval      *inLR = *On
      *
      ***************************************************************
      ** Init : Initialise
      ***************************************************************
     C     Init          begsr
      *
      ** Clear subfile
     C                   eval      *in20 = *On
     C                   write     CUSTCTL
     C                   eval      *in20 = *Off
      *
      ** Load the first screen
     C     *LoVal        setll     CUSTR
     C                   eval      RecN = 1
     C                   eval      TopCTR = 0
     C                   eval      BotCTR = 0
     C                   eval      @EOF= 'N'
     C                   exsr      LoadSub
      * 
     C                   endsr
      *
      ***************************************************************
      ** DspScrn: Display the screen
      ***************************************************************
     C     DspScrn       begsr
      * 
     C                   eval      RecN = TopCtr
     C                   write     CUSTFTR
     C                   exfmt     CUSTCTL
     C                   eval      TopCtr = TopSF#
      * 
     C                   endsr
      *
      ***************************************************************
      ** RollUp: Roll Up routine
      ***************************************************************
     C     RollUp        begsr
      * 
     C                   eval      RecN = BotCtr +1
     C                   exsr      LoadSub
      * 
     C                   endsr
      *
      ***************************************************************
      ** Enter: Enter key pressed
      ***************************************************************
     C     Enter         begsr
      * 
     C** Get the row and column of the cursor position
     C     RowCol        div       256           CrsRow            2 0
     C                   mvr                     CrsCol            2 0
      *
      ** Ignore subfile if the is positioning (searching)
      ** else, iterate through the subfile
     C                   if        Pos <> *blanks
     C                   exsr      ProcPos
     C                   else
     C                   exsr      ProcSub
     C                   endif                                                  
      * 
     C                   endsr
      *
      ***************************************************************
      ** ProcPos : Position subfile based on input value
      ***************************************************************
     C     ProcPos         begsr
      * 
      ** Refresh the screen
     C                   exsr      Init
      * 
      ** Loop until match is found or EOF
     C                   Dow       (Pos > @CuName)and (@EOF = 'N')
     C                   exsr      RollUp
     C                   enddo                                                  
     C**
     C** Position the cursor, unless the last entry is less than Pos
     C                   if        @CuName > Pos
     C                   eval      RecN = TopCtr
     C     RecN          chain     CUSTSF                            91
     C                   Dow       Pos > CuName
     C                   eval      RecN = RecN + 1
     C     RecN          chain     CUSTSF                            91
     C                   enddo                                                  
      * 
     C                   eval      TopCtr = RecN
     C                   endif                                                  
      * 
     C                   eval      Pos = *blanks
      * 
     C                   endsr
      *
      ***************************************************************
      ** ProcSub: Process subfile
      ***************************************************************
     C     ProcSub       begsr
      * 
     C                   eval      Cancel = 'N'
      * 
     C                   for       iRow = 1 to TotSF#
     C                   eval      RecN = iRow
     C     RecN          chain     CUSTSF                            91
      * 
     C                   select
      ** Delete?
     C                   when      Select = 'D'
     C                   exsr      DltRec
      *
      ** Edit?
     C                   when      Select = 'E'
     C                   exsr      EdtRec
      *
      ** Invalid Option
     C                   when      select <> *blanks
     C                   exsr      InvOpt
      * 
      ** No selection here - clear the subfile error indicator if it was set
     C                   other
     C                   exsr      ClearSubI
     C                   if        @SFI = 'Y'
     C                   update    CUSTSF
     C                   endif
      * 
     C                   endsl
      * 
      ** Cancel
     C                   if        Cancel = 'Y'
     C                   eval      DspMsg = 'Cancelled'
     C                   leave
     C                   endif                                                  
      * 
     C                   endfor                                                 
      * 
     C                   endsr
      *
      ***************************************************************
      ** LoadSub: Load a page to the subfile 
      ** - Set @EOF to 'Y' if EOF, 'N' if not
      ** - Update TopCTR: Pointing to top of screen
      **          BotCTR: Pointing to bottom of screen
      ***************************************************************
     C     LoadSub        begsr
      * 
      ** Do nothing if EOF
     C                   if        @EOF <> 'Y'
     C                   eval      @EOF = 'N'
     C                   eval      Count = 0
      * 
      ** Loop for number of rows in the subfile
     C                   for       iRow = 1 to SFPag
      * 
      ** Next record from the database
     C                   read      CUSTR                                90
      * 
      ** Check for EOF
      ** If this is first time pass, write ‘No records found’
     C                   if        *in90 = *On
     C                   eval      @EOF = 'Y'
      *
     C                   if        iRow = 1
     C                   exsr      WrtNR
     C                   Z-ADD     1             TopCTR
      *
     C                   else
     C                   exsr      WrtEOF
     C                   endif                                                  
      *
     C                   leave
     C                   endif                                                  
      * 
      ** Write record to subfile
     C                   exsr      WrtSub
     C                   eval      Count = Count + 1
      * 
     C                   endfor                                                 
      * 
     C                   eval      TopCtr = BotCtr + 1
     C                   eval      BotCtr = BotCtr + Count        
     C                   endif                          
      * 
     C                   endsr
      *
      ***************************************************************
      ** - WrtSub : write record to subfile
      ***************************************************************
     C     WrtSub        begsr
      * 
     C                   exsr      RecToSub
     C                   write     CUSTSF
     C                   eval      @CuName = CuName
     C                   eval      RecN = RecN + 1
      * 
     C                   endsr
      *
      ***************************************************************
      ** WrtNR : Write ‘No Records Found’
      ***************************************************************
     C     WrtNR         begsr
      * 
     C                   eval      *in40 = *On
     C                   eval      CuName = 'No Records Found'
     C                   eval      CuPhNbr = *blanks
     C                   eval      CuEml = *blanks
     C                   write     CUSTSF
     C                   eval      RecN = RecN + 1
      * 	
     C                   endsr
      * 	
      ***************************************************************
      ** WrtEOF : Write ‘End of File’
      ***************************************************************
     C     WrtEOF        begsr
      * 
     C                   eval      *in40 = *On
     C                   eval      CuNbr = 0
     C                   eval      CuName = *blanks
     C                   eval      CuPhNbr = 'End of File'
     C                   eval      CuEml = *blanks
     C                   eval      Select = *blanks
     C                   write     CUSTSF
     C                   eval      RecN = RecN + 1
      * 
     C                   endsr
      *
      ***************************************************************
      ** RecToSub : Write record fields to subfile fields
      ***************************************************************
     C     RecToSub         begsr
      * 
      ** Clear the subfile error indicators
     C                   exsr      ClearSubI
     C                   eval      Select = *blanks
      * 
      ** Parse customer name
     C                   eval      CuName=%trim(CuLst) + ', '
     C                              + %trim(CuFst) + ' ' + %trim(CuMid)
      * 
     C                   eval      CuPhNbr = CuPhNo
     C                   eval      CuEml = CuEmail
      * 
     C                   endsr
      *
      ***************************************************************
      ** DltRec : Delete record 
      ***************************************************************
     C     DltRec        begsr
      * 
      ** Clear the subfile error indicators
     C                   exsr      ClearSubI
      * 
      ** Deleted record message
     C                   eval      Select = *blanks
     C                   eval      CuName = '(Deleted)'
     C                   eval      CuPhNbr = *blanks
     C                   eval      CuEml = *blanks
      *
     C                   endsr
      *
      ***************************************************************
      ** ClearSubI: Clear subfile error indicators
      ** Set @SFI = 'Y' if an error indicator had been set
      **          = 'N' if not 
      ***************************************************************
     C     ClearSubI     begsr
      * 
     C                   if        (*in40 = '1') or (*in42 = '1')
     C                   eval      @SFI = 'Y'
     C                   setoff                                       4042
      *
     C                   else
     C                   eval      @SFI = 'N'
     C                   endif                                                  
      * 
     C                   endsr
      *
      ***************************************************************
      ** AddRec: Add record
      ***************************************************************
     C     AddRec        begsr
      * 	
     C                   exsr      GetKey
     C                   eval      CuNbr = @CuNbr
     C                   eval      CuFst = *blanks
     C                   eval      CuLst = *blanks
     C                   eval      CuMid = *blanks
     C                   eval      CuPhNo = *blanks
     C                   eval      CuEmail = *blanks
     C                   exfmt     CUSTDTL
      * 
      ** Add the record unless cancelled
     C                   if        (*inkc = *Off) and (*inkl = *Off)
     C                   exsr      GetKey
     C                   eval      CuNbr = @CuNbr
     C                   write     CUSTR
      * 
     C                   endif                                                  
      * 
     C                   endsr
      *
      ***************************************************************
      ** DltRec: Delete routine
     C***************************************************************
     C     DltRec        begsr
     C     CUNBR         chain     CUSTKEY                           99
     C                   if        *in99 = *Off 
     C                   eval      *in41 = *On
     C                   eval      DltFlag = 'N'
     C                   exfmt     CUSTDTL
      * 
     C                   select
      * 
      ** F3 = Exit
     C                   when      *inkc = *On
      * 
     C** F12 = Cancel
     C                   when      *inkl = *On
     C                   eval      Cancel = 'Y'
      * 
      ** Enter keyed here - delete if DltFlag = ‘Y’
     C                   other
     C                   if        DltFlag='Y'
     C                   delete    CUSTKEY
     C                   exsr      ClearSubI
     C                   exsr      DltRec
     C                   update    CUSTSF
     C                   endif
      * 
     C                   endsl
      * 
     C                   endif                                                  
      * 
     C                   endsr
      *
      ***************************************************************
      ** EdtRec: Edit routine
      ***************************************************************
     C     EdtRec        begsr
      * 
     C     CUNBR         chain     CUSTKEY                           99
     C                   if        *in99 = *Off
     C                   eval      *in41 = *Off
     C                   dou          *inkc = *On
     C                             or *inki = *On
     C                             or *inkl = *On
     C                   exfmt     CUSTDTL
      *
     C                   select
      * 
      ** F3 = Exit
     C                   when      *inkc = *On
      * 
      ** F12 = Cancel
      **
     C                   when      *inkl = *On
     C                   eval      Cancel = 'Y'
      * 
      ** Enter or F9 - update
     C                   other
     C                   exsr      ClearSubI
     C                   update    CUSTKEY
     C                   exsr      RecToSub
      * 
     C                   update    CUSTSF
     C                   if        *inki = *On
     C     CUNBR         chain     CUSTKEY                           99
     C     RecN          chain     CUSTSF                            91
     C                   endif                                                  
      * 
     C                   endsl
     C                   enddo                                                  
      *			
     C                   endif                                                  
      * 
     C                   endsr
      *
      ***************************************************************
      ** InvOpt: Invalid Option
      ***************************************************************
     C     InvOpt        begsr
      * 
     C                   exsr      ClearSubI
     C                   eval      Select = *blanks
     C                   eval      *in42 = *On
     C                   update    CUSTSF
      * 
     C                   endsr
      *
      ***************************************************************
      ** GetKey: Get a new Primary Key
      ***************************************************************
     C     GetKey        begsr
      * 
     C                   eval      SaveKey = CuNbr
     C                   eval      SaveFst = CuFst
     C                   eval      SaveLst = CuLst
     C                   eval      SaveMid = CuMid
     C                   eval      SavePhone = CuPhNo
     C                   eval      SaveEmail = CuEmail
      * 
     C     *HIVAL        setll     CUSTKEY
     C                   readP     CUSTKEY                               99
     C                   if        *in99 = *On
     C                   eval      @CuNbr = 1
      *
     C                   else
     C                   eval      @CuNbr = CuNbr+1
     C                   endif                                                  
      *
      ** Reset file cursor position    
     C                   eval      CuNbr = SaveKey
     C     CuNbr         chain     CUSTKEY                           99
     C                   eval      CuFst = SaveFst
     C                   eval      CuLst = SaveLst
     C                   eval      CuMid = SaveMid
     C                   eval      CuPhNo = SavePhone
     C                   eval      CuEmail = SaveEmail
      * 
     C                   endsr
