       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAMTOQ.
       AUTHOR HUSNU CAN PIDECI
      *MADE AS A HOMEWORK.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE   ASSIGN IDXFILE
                             ORGANIZATION INDEXED
                             ACCESS RANDOM
                             RECORD KEY IDX-KEY
                             STATUS ST-IDX.
           SELECT OUT-FILE   ASSIGN TO OUTFILE
                             STATUS ST-OUT.
           SELECT INP-FILE   ASSIGN TO INPFILE
                             STATUS ST-INP.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
         01  IDX-REC.
           03 IDX-KEY.
              05 IDX-ID         PIC S9(05) COMP-3.
              05 IDX-DVZ        PIC S9(03) COMP.
           03 IDX-NAME          PIC X(15).
           03 IDX-SURNAME       PIC X(15).
           03 IDX-ODATE         PIC S9(07) COMP-3.
           03 IDX-BALANCE       PIC S9(15) COMP-3.
       FD  OUT-FILE RECORDING MODE F.
         01  OUT-REC.
           05 OUT-ID-O          PIC 9(05).
           05 FILLER            PIC X(01) VALUE SPACE.
           05 OUT-DVZ-O         PIC 9(03).
           05 FILLER            PIC X(01) VALUE SPACE.
           05 OUT-NAME-O        PIC X(15).
           05 OUT-SURNAME-O     PIC X(15).
           05 OUT-ODATE-O       PIC 9(08).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 OUT-CDATE-O       PIC X(08).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 OUT-BALANCE-O     PIC 9(15).
       FD  INP-FILE RECORDING MODE F.
         01  INP-KEY.
           05 INP-ID            PIC X(05).
           05 INP-DVZ           PIC X(03).

       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           05 ST-IDX            PIC 9(02).
             88 IDX-SUCCESS              VALUE 00 97.
           05 ST-INP            PIC 9(02).
             88 INP-EOF                   VALUE 10.
             88 INP-SUCCESS               VALUE 00 97.
           05 ST-OUT            PIC 9(2).
             88 OUT-SUCCESS               VALUE 00 97.
           05 GREG-ODATE                   PIC 9(08).
           05 INT-DATE                     PIC 9(07).
           05 ADDBALANCE                   PIC S9(15) COMP.

         01  HEADER-1.
           05  FILLER         PIC X(23) VALUE 'Costumers Bank Accounts'.
           05  FILLER         PIC X(52) VALUE SPACES.

         01  HEADER-2.
           05  FILLER         PIC X(05) VALUE 'C.No '.
           05  FILLER         PIC X(01) VALUE SPACE.
           05  FILLER         PIC X(03) VALUE 'Dvz'.
           05  FILLER         PIC X(01) VALUE SPACE.
           05  FILLER         PIC X(05) VALUE 'Name '.
           05  FILLER         PIC X(10) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE 'Surname '.
           05  FILLER         PIC X(07) VALUE SPACE.
           05  FILLER         PIC X(07) VALUE "O.Date ".
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE "N.Date ".
           05  FILLER         PIC X(03) VALUE SPACE.
           05  FILLER         PIC X(08) VALUE "Balance ".
           05  FILLER         PIC X(07) VALUE SPACES.

          01  HEADER-3.
           05  FILLER         PIC X(05) VALUE '-----'.
           05  FILLER         PIC X(01) VALUE SPACE.
           05  FILLER         PIC X(03) VALUE '---'.
           05  FILLER         PIC X(01) VALUE SPACE.
           05  FILLER         PIC X(10) VALUE '----------'.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(10) VALUE '----------'.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE '--------'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(08) VALUE '--------'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(15) VALUE '---------------'.

      *--------------------
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES
           PERFORM H150-WRITE-HEADERS
           PERFORM H200-READ-FIRST UNTIL INP-EOF
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-FILE.
           OPEN INPUT  IDX-FILE.
           IF (NOT INP-SUCCESS)
           DISPLAY 'UNABLE TO OPEN INPFILE: ' ST-INP
           MOVE ST-INP TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (NOT OUT-SUCCESS)
           DISPLAY 'UNABLE TO OPEN OUTFILE: ' ST-OUT
           MOVE ST-OUT TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (NOT IDX-SUCCESS)
           DISPLAY 'UNABLE TO OPEN IDXILE: ' ST-IDX
           MOVE ST-IDX TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           READ INP-FILE.
           IF (NOT INP-SUCCESS)
           DISPLAY 'UNABLE TO READ INPFILE: ' ST-INP
           MOVE ST-INP TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.

       H150-WRITE-HEADERS.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC  FROM HEADER-1.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC AFTER ADVANCING 1 LINES.
           WRITE OUT-REC  FROM HEADER-2.
           WRITE OUT-REC  FROM HEADER-3.
       H150-END. EXIT.

       H200-READ-FIRST.
           COMPUTE IDX-ID = FUNCTION NUMVAL-C (INP-ID)
           COMPUTE IDX-DVZ = FUNCTION NUMVAL (INP-DVZ)
           READ IDX-FILE KEY IDX-KEY
           INVALID KEY     PERFORM H250-WRONG-KEY
           NOT INVALID KEY PERFORM H300-DATA-OPARATIONS
           END-READ.
           READ INP-FILE.
       H200-END. EXIT.

       H250-WRONG-KEY.
           MOVE SPACES TO OUT-REC
           MOVE IDX-ID            TO  OUT-ID-O
           MOVE IDX-DVZ           TO  OUT-DVZ-O
           MOVE 'INVALID COST OR' TO  OUT-NAME-O
           MOVE ' ACCOUNT NUMBER' TO  OUT-SURNAME-O
           WRITE OUT-REC.
       H250-END. EXIT.

       H300-DATA-OPARATIONS.
           COMPUTE INT-DATE = FUNCTION INTEGER-OF-DAY(IDX-ODATE)
           COMPUTE GREG-ODATE = FUNCTION DATE-OF-INTEGER(INT-DATE).
           IF (IDX-DVZ = 840)
           MOVE 150000 TO ADDBALANCE
           END-IF.
           IF (IDX-DVZ = 949)
           MOVE 3000 TO ADDBALANCE
           END-IF.
           IF (IDX-DVZ = 978)
           MOVE 4500 TO ADDBALANCE
           END-IF.
           COMPUTE IDX-BALANCE = ADDBALANCE + IDX-BALANCE
           PERFORM H500-WRITE-RECORD.
       H300-END. EXIT.

       H500-WRITE-RECORD.
           MOVE SPACES TO OUT-REC
           MOVE IDX-ID       TO  OUT-ID-O
           MOVE IDX-DVZ      TO  OUT-DVZ-O
           MOVE IDX-NAME     TO  OUT-NAME-O
           MOVE IDX-SURNAME  TO  OUT-SURNAME-O
           MOVE GREG-ODATE   TO  OUT-ODATE-O
           MOVE FUNCTION CURRENT-DATE TO  OUT-CDATE-O
           MOVE IDX-BALANCE  TO  OUT-BALANCE-O
           WRITE OUT-REC.
       H500-END. EXIT.
       H999-PROGRAM-EXIT.
           CLOSE INP-FILE
           CLOSE OUT-FILE
           CLOSE IDX-FILE.
           STOP RUN.
       H999-END. EXIT.
