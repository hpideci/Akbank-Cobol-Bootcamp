//SORTREPR JOB ' ',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=X,NOTIFY=&SYSUID
//DELET100 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USERID.QSAM.AA NONVSAM
  IF LASTCC LE 08 THEN SET MAXCC = 00
//SORT0200 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD *
0002MUSTAFA        YILMAZ         19150617
0001MEHMET         AYDIN          19231029
//SORTOUT  DD DSN=&SYSUID..QSAM.AA,
//             DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=42)
//SYSIN    DD *
  SORT FIELDS=COPY
//DELET300 EXEC PGM=IEFBR14
//FILE01   DD DSN=&SYSUID..QSAM.BB,
//             DISP=(MOD,DELETE,DELETE),SPACE=(TRK,0)
//SORT0400 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=&SYSUID..QSAM.AA,DISP=SHR
//SORTOUT  DD DSN=&SYSUID..QSAM.BB,
//             DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=50)
//SYSIN DD *
  SORT FIELDS=(1,4,CH,A)
  INCLUDE COND=(1,1,CH,EQ,C'0')
  OUTREC FIELDS=(1,42,DATE1)
