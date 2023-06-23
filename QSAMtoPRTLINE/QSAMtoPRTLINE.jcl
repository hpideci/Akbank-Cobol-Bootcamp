//DENEMEJ JOB 1,NOTIFY=&SYSUID
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(DENEME),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(DENEME),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//RUN     EXEC PGM=DENEME
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//NAMEREC   DD DSN=&SYSUID..QSAM.EX,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//*PRTLINE   DD DSN=&SYSUID..DATA.RPT,DISP=(NEW,CATLG,DELETE),
//*             SPACE=(TRK,(20,20),RLSE),
//*             DCB=(RECFM=FB,LRECL=150,BLKSIZE=27900),UNIT=3390
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
