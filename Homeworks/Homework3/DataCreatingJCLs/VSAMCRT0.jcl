//VSAMCRT0 JOB ' ',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=X,NOTIFY=&SYSUID
//* This JCL Creating a vsam.aa from qsam.ff which have binary codes and packet 
//DELET500 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
  DELETE Z95638.VSAM.AA CLUSTER PURGE
  IF LASTCC LE 08 THEN SET MAXCC = 00
  DEF CL ( NAME(Z95638.VSAM.AA)      -
           FREESPACE( 20 20 )        -
           SHR( 2,3 )                -
           KEYS(5 0)                 -
           INDEXED SPEED             -
           RECSZ(47 47)              -
           TRK (10 10)               -
           LOG(NONE)                 -
           VOLUME (VPWRKB)           -
           UNIQUE )                  -
   DATA (NAME(Z95638.VSAM.AA.DATA))  -
   INDEX ( NAME(Z95638.VSAM.AA.INDEX))
//REPRO600 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INN001   DD DSN=Z95638.QSAM.FF,DISP=SHR
//OUT001   DD DSN=Z95638.VSAM.AA,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INN001) OUTFILE(OUT001)
