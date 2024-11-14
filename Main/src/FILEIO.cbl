      * In MEDIKA/src/FILEIO.cob (Example - You might not need a separate file for this)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PATIENT-FILE ASSIGN TO "data/PATIENT.DAT"
        ORGANIZATION IS SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PATIENT-FILE.
       01  PATIENT-RECORD.
           05  PATIENT-NAME PIC X(30).
           05  PATIENT-ID   PIC 9(9).
           05  STUDENT-NUMBER PIC X(10).
           05  HEALTH-CONDITION PIC X(50).
           05  DATE-OF-VISIT PIC X(10).

       PROCEDURE DIVISION.
      *  This program could contain paragraphs for common file operations
      *  like opening, reading, writing, and closing files, which can then
      *  be called from other programs (MAINMENU.cob, PATIENT.cob) using
      *  the COBOL CALL statement.
