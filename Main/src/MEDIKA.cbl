       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIKA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PATIENT-FILE ASSIGN TO
           "/home/zelly/PATIENTS.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PATIENT-FILE.
       01  PATIENT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01  USER-CHOICE PIC X.
       01  EOF-FLAG PIC X VALUE "N".
       01  FILE-STATUS PIC XX.
       01  SEARCH-NAME PIC X(30).
       01  WS-PATIENT-NAME PIC X(30).
       01  WS-STUDENT-NUMBER PIC X(15).
       01  WS-STUDENT-CYS PIC X(10).
       01  WS-DATE-OF-BIRTH PIC X(10).
       01  WS-PATIENT-AGE PIC 99.
       01  WS-PATIENT-SEX PIC X(1).
       01  WS-HEALTH-CONDITION PIC X(30).
       01  WS-DATE-OF-VISIT PIC X(10).
       01  NEW-LINE PIC X(1) VALUE X'0A'.
       01  FRA-ME PIC X(35) VALUE "===================================".*> for main header terminal-based design purposes
      *
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY FRA-ME.
           DISPLAY "      MEDIKA PATIENT RECORD".
           DISPLAY FRA-ME.
           DISPLAY "            MAIN MENU".
           DISPLAY FRA-ME.
           DISPLAY "Good Day, Admin!".
           DISPLAY "What would you like to do?".
           PERFORM DISPLAY-MENU.
           DISPLAY NEW-LINE
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
      *
       DISPLAY-MENU.
           DISPLAY NEW-LINE.
           DISPLAY "a) Add New Patient Record".
           DISPLAY "b) View All Patient Records".
           DISPLAY "c) Search Patient Record".
           DISPLAY "d) Update Patient Info".
           DISPLAY "x) Exit Program".
           DISPLAY "Enter a letter to proceed: " WITH NO ADVANCING.
      *
       ADD-PATIENT.
           OPEN EXTEND PATIENT-FILE.
           MOVE "00" TO FILE-STATUS.

           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file."
               EXIT PROGRAM
           END-IF.

           DISPLAY FRA-ME.
           DISPLAY "      MEDIKA PATIENT RECORD".
           DISPLAY FRA-ME.
           DISPLAY "        ADD PATIENT RECORD".
           DISPLAY FRA-ME.

           DISPLAY "Student ID Number: " WITH NO ADVANCING.
           ACCEPT WS-STUDENT-NUMBER.

           DISPLAY "Name (FN, MI, LN): " WITH NO ADVANCING.
           ACCEPT WS-PATIENT-NAME.

           DISPLAY "Course/Section: " WITH NO ADVANCING.
           ACCEPT WS-STUDENT-CYS.

           DISPLAY "Date of Birth (yyyy/mm/dd): " WITH NO ADVANCING.
           ACCEPT WS-DATE-OF-BIRTH.

           DISPLAY "Age: " WITH NO ADVANCING.
           ACCEPT WS-PATIENT-AGE.

           DISPLAY "Assigned Sex at Birth: " WITH NO ADVANCING.
           ACCEPT WS-PATIENT-SEX.

           DISPLAY "Enter Student Health Condition: " WITH NO ADVANCING.
           ACCEPT WS-HEALTH-CONDITION.

           DISPLAY "Date of Visit (yyyy/mm/dd): " WITH NO ADVANCING.
           ACCEPT WS-DATE-OF-VISIT.

           DISPLAY FRA-ME.
           DISPLAY "Add New Record? (Y/N): " WITH NO ADVANCING.
           ACCEPT USER-CHOICE.

           IF USER-CHOICE = "Y"
               MOVE WS-PATIENT-NAME TO PATIENT-RECORD(1:30)
               MOVE WS-STUDENT-NUMBER TO PATIENT-RECORD(31:10)
               MOVE WS-HEALTH-CONDITION TO PATIENT-RECORD(41:30)
               MOVE WS-DATE-OF-VISIT TO PATIENT-RECORD(71:10)
               WRITE PATIENT-RECORD
           ELSE
               DISPLAY "Record not added.".

           CLOSE PATIENT-FILE.
      *
       VIEW-PATIENTS.
           OPEN INPUT PATIENT-FILE.
           READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG.
           PERFORM UNTIL EOF-FLAG = "Y"
               MOVE PATIENT-RECORD(1:30) TO WS-PATIENT-NAME
               MOVE PATIENT-RECORD(31:10) TO WS-STUDENT-NUMBER
               MOVE PATIENT-RECORD(41:30) TO WS-HEALTH-CONDITION
               MOVE PATIENT-RECORD(71:10) TO WS-DATE-OF-VISIT
               DISPLAY "Patient Name: " WS-PATIENT-NAME
               DISPLAY "Age: " WS-PATIENT-AGE
               DISPLAY "Sex: " WS-PATIENT-SEX
               DISPLAY "Date of Birth: " WS-DATE-OF-BIRTH
               DISPLAY "Course/Section: " WS-STUDENT-CYS
               DISPLAY "Student Number: " WS-STUDENT-NUMBER
               DISPLAY "Health Condition: " WS-HEALTH-CONDITION
               DISPLAY "Date of Visit: " WS-DATE-OF-VISIT
               DISPLAY " "
               READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG
           END-PERFORM.
           CLOSE PATIENT-FILE.
      *
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
               DISPLAY "Age: " WS-PATIENT-AGE
               DISPLAY "Sex: " WS-PATIENT-SEX
               DISPLAY "Date of Birth: " WS-DATE-OF-BIRTH
               DISPLAY "Course/Section: " WS-STUDENT-CYS
               DISPLAY "Student Number: " WS-STUDENT-NUMBER
               DISPLAY "Health Condition: " WS-HEALTH-CONDITION
               DISPLAY "Date of Visit: " WS-DATE-OF-VISIT
                   MOVE "Y" TO EOF-FLAG
               END-IF
               READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG
           END-PERFORM.
           CLOSE PATIENT-FILE.
      *

      *
       UPDATE-PATIENT.
      * (Implementation for updating patient records)

       EXIT-PROGRAM.
           DISPLAY "Exiting Medika.".
