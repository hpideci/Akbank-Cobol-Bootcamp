       IDENTIFICATION DIVISION.
       PROGRAM-ID. MYDCALC.
       AUTHOR HUSNU CAN PIDECI
      *MADE AS A HOMEWORK.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-INFO ASSIGN TO PRINTIT
                             STATUS ST-PRINT-INFO.
           SELECT DATA-REC   ASSIGN TO DATAIN
                             STATUS ST-DATA-REC.
       DATA DIVISION.
       FILE SECTION.
       FD  PRINT-INFO RECORDING MODE F.
         01 PRINT-DATA.
           05 DATA-ID-O            PIC X(04).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 DATA-NAME-O          PIC X(15).
           05 DATA-SURNAME-O       PIC X(15).
           05 DATA-DATE-O          PIC 9(08).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 DATA-NDATE-O         PIC 9(08).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 DATA-LDAY-O          PIC 9(05).
           05 FILLER               PIC X(03) VALUE SPACES.
           05 DATA-AGE-O           PIC 9(03).
           05 FILLER               PIC X(04) VALUE SPACES.
       FD  DATA-REC RECORDING MODE F.
         01  DATA-IN.
           05 DATA-ID               PIC X(04).
           05 DATA-NAME             PIC X(15).
           05 DATA-SURNAME          PIC X(15).
           05 DATA-DATE             PIC 9(08).
           05 DATA-NDATE            PIC 9(08).

       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           05 ST-DATA-REC           PIC 9(02).
             88 DATA-REC-EOF                      VALUE 10.
             88 DATA-REC-OK                       VALUE 00 97.
           05 ST-PRINT-INFO         PIC 9(02).
             88 PRINTABLE                        VALUE 00 97.
         01  CALC.
           05 DATA-DATE-INT         PIC 9(08).
           05 DATA-NDATE-INT        PIC 9(08).
           05 DATA-LDAY             PIC 9(08).
           05 DATA-AGE              PIC 9(03).
         01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).

         01  HEADER-1.
           05  FILLER         PIC X(15) VALUE 'Costumers Life '.
           05  FILLER         PIC X(60) VALUE SPACES.
         01  HEADER-2.
           05  FILLER         PIC X(05) VALUE 'Year '.
           05  HDR-YR         PIC 9(04).
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE 'Month '.
           05  HDR-MO         PIC X(02).
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(04) VALUE 'Day '.
           05  HDR-DAY        PIC X(02).
           05  FILLER         PIC X(48) VALUE SPACES.

         01  HEADER-3.
           05  FILLER         PIC X(05) VALUE 'C.No '.
           05  FILLER         PIC X(01) VALUE SPACE.
           05  FILLER         PIC X(05) VALUE 'Name '.
           05  FILLER         PIC X(10) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE 'Surname '.
           05  FILLER         PIC X(07) VALUE SPACES.
           05  FILLER         PIC X(09) VALUE 'Birthday '.
           05  FILLER         PIC X(01) VALUE SPACE.
           05  FILLER         PIC X(07) VALUE "T.Date ".
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE "Lived".
           05  FILLER         PIC X(03) VALUE SPACE.
           05  FILLER         PIC X(04) VALUE "Age ".
           05  FILLER         PIC X(03) VALUE SPACES.


      *
         01  HEADER-4.
           05  FILLER         PIC X(04) VALUE '----'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE '----------'.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE '----------'.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE '--------'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE '--------'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE '-----'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(03) VALUE '---'.
           05  FILLER         PIC X(04) VALUE SPACES.

      *---------------
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM WRITE-HEADERS.
           PERFORM H200-READ-NEXT-RECORD UNTIL DATA-REC-EOF.
           PERFORM H999-PROGRAM-EXIT.
           STOP RUN.
       0000-END. EXIT.
      *---- H100 paragrafi ilk once DATA-REC,PRINT-INFO dosyasini açiyor
      *      ve DATA-REC dosyasini bir kez okuyor.
      *      hata varsa çikis yapiyor ve hata mesaji döndürüyor.
       H100-OPEN-FILES.
           OPEN INPUT  DATA-REC.
           OPEN OUTPUT PRINT-INFO.
           IF (NOT DATA-REC-OK)
           DISPLAY 'UNABLE TO OPEN INPFILE: ' ST-DATA-REC
           MOVE ST-DATA-REC TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (NOT PRINTABLE)
           DISPLAY 'UNABLE TO OPEN OUTFILE ' ST-DATA-REC
           MOVE ST-PRINT-INFO TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           READ DATA-REC.
           IF (NOT DATA-REC-OK)
           DISPLAY 'UNABLE TO READ INPFILE: ' ST-DATA-REC
           MOVE ST-DATA-REC TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.

       H200-READ-NEXT-RECORD.
           PERFORM CALC-DATA
           READ DATA-REC.
       H200-END. EXIT.

       CALC-DATA.
           COMPUTE DATA-DATE-INT = FUNCTION INTEGER-OF-DATE(DATA-DATE)
           COMPUTE DATA-NDATE-INT = FUNCTION INTEGER-OF-DATE(DATA-NDATE)
           COMPUTE DATA-LDAY = DATA-NDATE-INT - DATA-DATE-INT
           COMPUTE DATA-AGE = DATA-LDAY / 365
           PERFORM WRITE-RECORD.
       CALC-END. EXIT.

       WRITE-RECORD.
           MOVE DATA-ID          TO DATA-ID-O.
           MOVE DATA-NAME        TO DATA-NAME-O.
           MOVE DATA-SURNAME     TO DATA-SURNAME-O.
           MOVE DATA-DATE        TO DATA-DATE-O.
           MOVE DATA-NDATE       TO DATA-NDATE-O.
           MOVE DATA-LDAY        TO DATA-LDAY-O.
           MOVE DATA-AGE         TO DATA-AGE-O.
           WRITE PRINT-DATA.
       WRITE-END. EXIT.
       WRITE-HEADERS.
           MOVE SPACES TO PRINT-DATA.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR  TO HDR-YR.
           MOVE WS-CURRENT-MONTH TO HDR-MO.
           MOVE WS-CURRENT-DAY   TO HDR-DAY.
           WRITE PRINT-DATA  FROM HEADER-1.
           WRITE PRINT-DATA  FROM HEADER-2.
           MOVE SPACES TO PRINT-DATA.
           WRITE PRINT-DATA AFTER ADVANCING 1 LINES.
           WRITE PRINT-DATA  FROM HEADER-3.
           WRITE PRINT-DATA  FROM HEADER-4.
           MOVE SPACES TO PRINT-DATA .
       WRITE-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE DATA-REC.
           CLOSE PRINT-INFO.
       H999-END. EXIT.
