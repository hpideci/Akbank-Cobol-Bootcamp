       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCALC.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE
                             STATUS ST-PRINT-LINE.
           SELECT NAME-REC   ASSIGN TO NAMEREC
                             STATUS ST-NAME-REC.
       DATA DIVISION.
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
         01  PRINT-REC.
           03 REC-NAME-O          PIC X(10).
           03 REC-SURNAME-O       PIC X(12).
       FD  NAME-REC RECORDING MODE F.
         01  NAMEIN.
           03 REC-NAME            PIC X(10).
           03 REC-SURNAME         PIC X(12).
       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           03 ST-NAME-REC        PIC 9(2).
           88 NAME-REC-EOF                   VALUE 10.
           03 ST-PRINT-LINE      PIC 9(2).
      *--------------------
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES
           PERFORM H200-READ-NEXT-RECORD UNTIL NAME-REC-EOF.
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.
      *---- H100 programi ilk once  DATA-REC dosyasini aciyor
       H100-OPEN-FILES.
           OPEN INPUT  NAME-REC.
           OPEN OUTPUT PRINT-LINE.
           IF (ST-NAME-REC NOT = 0) AND (ST-NAME-REC NOT = 97)
           DISPLAY 'UNABLE TO OPEN INPFILE: ' ST-NAME-REC
           MOVE ST-NAME-REC TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (ST-PRINT-LINE NOT = 0) AND (ST-PRINT-LINE NOT = 97)
           DISPLAY 'UNABLE TO OPEN OUTFILE: ' ST-PRINT-LINE
           MOVE ST-PRINT-LINE TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           READ NAME-REC.
           IF (ST-NAME-REC NOT = 0) AND (ST-NAME-REC NOT = 97)
           DISPLAY 'UNABLE TO READ INPFILE: ' ST-NAME-REC
           MOVE ST-NAME-REC TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.

       H200-READ-NEXT-RECORD.
               PERFORM WRITE-RECORD
               READ NAME-REC.
       H200-END. EXIT.
      *
       WRITE-RECORD.
           MOVE REC-NAME     TO  REC-NAME-O.
           MOVE REC-SURNAME  TO  REC-SURNAME-O 
           WRITE PRINT-REC.
       WRITE-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE NAME-REC.
           CLOSE PRINT-LINE.
       H999-END. EXIT.
           STOP RUN.

      *
