      * In MEDIKA/src/PATIENT.cob
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PATIENT.

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

       WORKING-STORAGE SECTION.
       01  ACTION-CODE PIC X(10).
       01  EOF-FLAG PIC X VALUE "N".
       01  SEARCH-ID PIC 9(9).

       PROCEDURE DIVISION.
           ACCEPT ACTION-CODE FROM ARGUMENT-VALUE.

           EVALUATE ACTION-CODE
               WHEN "ADD"
                   PERFORM ADD-PATIENT
               WHEN "VIEW"
                   PERFORM VIEW-PATIENTS
               WHEN "SEARCH"
                   PERFORM SEARCH-PATIENT
               WHEN "UPDATE"
                   PERFORM UPDATE-PATIENT
               WHEN OTHER
                   DISPLAY "Invalid action code."
           END-EVALUATE.

           STOP RUN.

       ADD-PATIENT.
           OPEN OUTPUT PATIENT-FILE.
           DISPLAY "Enter Patient Name: ".
           ACCEPT PATIENT-NAME.
           DISPLAY "Enter Patient ID: ".
           ACCEPT PATIENT-ID.
           DISPLAY "Enter Student Number (if applicable): ".
           ACCEPT STUDENT-NUMBER.
               DISPLAY "Enter Health Condition: ".
           ACCEPT HEALTH-CONDITION.
           DISPLAY "Enter Date of Visit (YYYY-MM-DD): ".
           ACCEPT DATE-OF-VISIT.
               WRITE PATIENT-RECORD.
           CLOSE PATIENT-FILE.

       VIEW-PATIENTS.
           OPEN INPUT PATIENT-FILE.
           READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG.
           PERFORM UNTIL EOF-FLAG = "Y"
               DISPLAY PATIENT-RECORD
               READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG
           END-PERFORM.
           CLOSE PATIENT-FILE.

       SEARCH-PATIENT.
           OPEN INPUT PATIENT-FILE.
           DISPLAY "Enter Patient ID to search: ".
           ACCEPT SEARCH-ID.
           READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG.
           PERFORM UNTIL EOF-FLAG = "Y"
               IF PATIENT-ID = SEARCH-ID THEN
                   DISPLAY PATIENT-RECORD
                   MOVE "Y" TO EOF-FLAG
               END-IF
               READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG
               END-PERFORM.
           CLOSE PATIENT-FILE.

       UPDATE-PATIENT.
      * (Implementation for updating patient records - This would likely
      * involve reading the file, finding the record, allowing edits,
      * and then rewriting the updated file.)
