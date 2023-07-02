      *-----------------------
      * Copyright Contributors to the COBOL Programming Course
      * SPDX-License-Identifier: CC-BY-4.0
      *-----------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADSOYAD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  AD           PIC A(17).
       77  IND          PIC 9(2).
       77  KAREKTER     PIC X.

       PROCEDURE DIVISION.
           MOVE "HUSNU CAN PIDECI" TO AD.
           MOVE 1 TO IND.
           PERFORM UNTIL IND >= LENGTH OF AD
              MOVE AD(IND:1) TO KAREKTER
              DISPLAY KAREKTER
              ADD 1 TO IND
           END-PERFORM.
           GOBACK.

