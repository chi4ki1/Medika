       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIKA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PATIENT-FILE ASSIGN TO
           "/Users/apple/Documents/MedikaOpenCBL/Main/data/PATIENT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PATIENT-FILE.
       01  PATIENT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01  USER-CHOICE PIC X.
       01  EOF-FLAG PIC X VALUE "N".
       01  SEARCH-NAME PIC X(30).
       01  WS-PATIENT-NAME PIC X(30).
       01  WS-STUDENT-NUMBER PIC X(10).
       01  WS-HEALTH-CONDITION PIC X(30).
       01  WS-DATE-OF-VISIT PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM DISPLAY-MENU.
           PERFORM UNTIL USER-CHOICE = "x" OR USER-CHOICE = "X"
               ACCEPT USER-CHOICE
               EVALUATE USER-CHOICE
                   WHEN "a"
                       PERFORM ADD-PATIENT
                   WHEN "b"
                       PERFORM VIEW-PATIENTS
                   WHEN "c"
                       PERFORM SEARCH-PATIENT
                   WHEN "d"
                       PERFORM UPDATE-PATIENT
                   WHEN "x"
                       PERFORM EXIT-PROGRAM
                   WHEN "X"
                       PERFORM EXIT-PROGRAM
                   WHEN OTHER
                       DISPLAY "Invalid choice."
               END-EVALUATE
               IF USER-CHOICE NOT = "x" AND USER-CHOICE NOT = "X" THEN
                   PERFORM DISPLAY-MENU
               END-IF
           END-PERFORM.
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY "Medika Main Menu".
           DISPLAY "a) Add New Patient Record".
           DISPLAY "b) View All Patient Records".
           DISPLAY "c) Search Patient Record".
           DISPLAY "d) Update Patient Info".
           DISPLAY "x) Exit Program".

       ADD-PATIENT.
           OPEN EXTEND PATIENT-FILE.
           DISPLAY "Enter Patient Name: ".
           ACCEPT WS-PATIENT-NAME.
           DISPLAY "Enter Student Number (if applicable): ".
           ACCEPT WS-STUDENT-NUMBER.
           DISPLAY "Enter Health Condition: ".
           ACCEPT WS-HEALTH-CONDITION.
           DISPLAY "Enter Date of Visit (YYYY-MM-DD): ".
           ACCEPT WS-DATE-OF-VISIT.
           MOVE WS-PATIENT-NAME TO PATIENT-RECORD(1:30).
           MOVE WS-STUDENT-NUMBER TO PATIENT-RECORD(31:10).
           MOVE WS-HEALTH-CONDITION TO PATIENT-RECORD(41:30).
           MOVE WS-DATE-OF-VISIT TO PATIENT-RECORD(71:10).
           WRITE PATIENT-RECORD.
           CLOSE PATIENT-FILE.

       VIEW-PATIENTS.
           OPEN INPUT PATIENT-FILE.
           READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG.
           PERFORM UNTIL EOF-FLAG = "Y"
               MOVE PATIENT-RECORD(1:30) TO WS-PATIENT-NAME
               MOVE PATIENT-RECORD(31:10) TO WS-STUDENT-NUMBER
               MOVE PATIENT-RECORD(41:30) TO WS-HEALTH-CONDITION
               MOVE PATIENT-RECORD(71:10) TO WS-DATE-OF-VISIT
               DISPLAY "Patient Name: " WS-PATIENT-NAME
               DISPLAY "Student Number: " WS-STUDENT-NUMBER
               DISPLAY "Health Condition: " WS-HEALTH-CONDITION
               DISPLAY "Date of Visit: " WS-DATE-OF-VISIT
               DISPLAY " "
               READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG
           END-PERFORM.
           CLOSE PATIENT-FILE.

       SEARCH-PATIENT.
           OPEN INPUT PATIENT-FILE.
           DISPLAY "Enter Patient Name to search: ".
           ACCEPT SEARCH-NAME.
           READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG.
           PERFORM UNTIL EOF-FLAG = "Y"
               MOVE PATIENT-RECORD(1:30) TO WS-PATIENT-NAME
               MOVE PATIENT-RECORD(31:10) TO WS-STUDENT-NUMBER
               MOVE PATIENT-RECORD(41:30) TO WS-HEALTH-CONDITION
               MOVE PATIENT-RECORD(71:10) TO WS-DATE-OF-VISIT
               IF WS-PATIENT-NAME = SEARCH-NAME THEN
                   DISPLAY "Patient Name: " WS-PATIENT-NAME
                   DISPLAY "Student Number: " WS-STUDENT-NUMBER
                   DISPLAY "Health Condition: " WS-HEALTH-CONDITION
                   DISPLAY "Date of Visit: " WS-DATE-OF-VISIT
                   MOVE "Y" TO EOF-FLAG
               END-IF
               READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG
           END-PERFORM.
           CLOSE PATIENT-FILE.

       UPDATE-PATIENT.
      * (Implementation for updating patient records)

       EXIT-PROGRAM.
           DISPLAY "Exiting Medika.".
