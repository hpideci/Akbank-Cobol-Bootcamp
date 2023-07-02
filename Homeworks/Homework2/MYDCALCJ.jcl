//MYDCALCJ JOB 1,NOTIFY=&SYSUID.
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(MYDCALC),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(MYDCALC),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//DELETEE  EXEC PGM=IEFBR14
//FILE01    DD DSN=Z95638.QSAM.EE,
//             DISP=(MOD,DELETE,DELETE),SPACE=(TRK,0)
//RUN     EXEC PGM=MYDCALC
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//DATAIN    DD DSN=&SYSUID..QSAM.BB,DISP=SHR
//PRINTIT   DD DSN=&SYSUID..QSAM.EE,DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(20,20),RLSE),
//             DCB=(RECFM=FB,LRECL=72,BLKSIZE=0),UNIT=3390
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
