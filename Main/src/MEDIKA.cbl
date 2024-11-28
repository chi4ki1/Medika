       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIKA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PATIENT-FILE ASSIGN TO
           "/home/zelly/PATIENTS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PATIENT-FILE.
       01  PATIENT-RECORD PIC X(260).

       WORKING-STORAGE SECTION.
       01  USER-CHOICE PIC X.
       01  EOF-FLAG PIC X VALUE "N".
       01  FILE-STATUS PIC XX.
       01  SEARCH-ID PIC X(30).
      *> PERSONAL INFORMATION 
       01  WS-PATIENT-NAME PIC X(30).
       01  WS-STUDENT-NUMBER PIC X(15).
       01  WS-STUDENT-CYS PIC X(10).
       01  WS-DATE-OF-BIRTH PIC X(10).
       01  WS-PATIENT-AGE PIC 99.
       01  WS-PATIENT-SEX PIC X(1).
       01  WS-PATIENT-CONTACT PIC 99999999999.
       01  WS-PATIENT-EMAIL PIC X(50).
      *> MEDICATION INFORMATION
       01  WS-HEALTH-CONDITION PIC X(30).
       01  WS-MEDICATION-NAME PIC X(30).
       01  WS-PROVIDER PIC X(30).
       01  WS-DATE-OF-VISIT PIC X(10).
      *> HEADER PURPOSES
       01  NEW-LINE PIC X(1) VALUE X'0A'.
       01  HEADER-MESSAGE PIC X(25) VALUE "MEDIKA PATIENT RECORD".
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
           DISPLAY "(a) Add New Patient Record".
           DISPLAY "(b) View All Patient Records".
           DISPLAY "(c) Search Patient Record".
           DISPLAY "(d) Update Patient Info".
           DISPLAY "(x) Exit Program".
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

           DISPLAY "PERSONAL INFORMATION."
           DISPLAY "   Student ID: " WITH NO ADVANCING.
           ACCEPT WS-STUDENT-NUMBER.

           DISPLAY "   Full Name: " WITH NO ADVANCING.
           ACCEPT WS-PATIENT-NAME.

           DISPLAY "   Course/Section [N/A if none]: "WITH NO ADVANCING.
           ACCEPT WS-STUDENT-CYS.

           DISPLAY "   Date of Birth [YYYY/MM/DD]: " WITH NO ADVANCING.
           ACCEPT WS-DATE-OF-BIRTH.

           DISPLAY "   Sex [F/M]: " WITH NO ADVANCING.
           ACCEPT WS-PATIENT-SEX.

           DISPLAY "   Age: " WITH NO ADVANCING.
           ACCEPT WS-PATIENT-AGE.

           DISPLAY "EMERGENCY CONTACT INFORMATION."
           DISPLAY "   Phone Number: " WITH NO ADVANCING.
           ACCEPT WS-PATIENT-CONTACT.

           DISPLAY "   Email Address:" WITH NO ADVANCING.
           ACCEPT WS-PATIENT-EMAIL. 

           DISPLAY "MEDICATION."
           DISPLAY "   Health Condition: " WITH NO ADVANCING.
           ACCEPT WS-HEALTH-CONDITION.

           DISPLAY "   Medication Name: " WITH NO ADVANCING.
           ACCEPT WS-MEDICATION-NAME.

           DISPLAY "   Prescribing Provider: " WITH NO ADVANCING.
           ACCEPT WS-PROVIDER.

           DISPLAY "   Date of Visit [YYYY/MM/DD]: " WITH NO ADVANCING.
           ACCEPT WS-DATE-OF-VISIT.

           DISPLAY FRA-ME.
           DISPLAY "Add New Patient Record? [Y/N]: " WITH NO ADVANCING.
           ACCEPT USER-CHOICE.

           IF USER-CHOICE = "Y" OR USER-CHOICE = "y"
               WRITE PATIENT-RECORD
               MOVE FRA-ME TO PATIENT-RECORD
               WRITE PATIENT-RECORD
               MOVE WS-PATIENT-NAME TO PATIENT-RECORD(1:30)
               MOVE WS-STUDENT-NUMBER TO PATIENT-RECORD(31:15)
               MOVE WS-STUDENT-CYS TO PATIENT-RECORD(51:10)
               MOVE WS-DATE-OF-BIRTH TO PATIENT-RECORD(66:10)
               MOVE WS-PATIENT-AGE TO PATIENT-RECORD(81:2)
               MOVE WS-PATIENT-SEX TO PATIENT-RECORD(88:1)
               MOVE WS-PATIENT-CONTACT TO PATIENT-RECORD (94:12)
               MOVE WS-PATIENT-EMAIL TO PATIENT-RECORD(111:50)
               MOVE WS-HEALTH-CONDITION TO PATIENT-RECORD(161:30)
               MOVE WS-MEDICATION-NAME TO PATIENT-RECORD(196:30)
               MOVE WS-PROVIDER TO PATIENT-RECORD(231:10)
               MOVE WS-DATE-OF-VISIT TO PATIENT-RECORD(246:10)
               WRITE PATIENT-RECORD
           ELSE
               DISPLAY "Record not added.".

           CLOSE PATIENT-FILE.
      *
       VIEW-PATIENTS.
           OPEN INPUT PATIENT-FILE.
           READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG.
           PERFORM UNTIL EOF-FLAG = "Y"
               MOVE PATIENT-RECORD (1:30) TO WS-PATIENT-NAME
               MOVE PATIENT-RECORD (31:15) TO WS-STUDENT-NUMBER
               MOVE PATIENT-RECORD (51:30) TO WS-STUDENT-CYS
               MOVE PATIENT-RECORD (66:10) TO WS-DATE-OF-BIRTH
               MOVE PATIENT-RECORD (81:2) TO WS-PATIENT-AGE
               MOVE PATIENT-RECORD (88:1) TO WS-PATIENT-SEX
               MOVE PATIENT-RECORD (94:12) TO WS-PATIENT-CONTACT
               MOVE PATIENT-RECORD (111:50) TO WS-PATIENT-EMAIL
               MOVE PATIENT-RECORD (161:30) TO WS-HEALTH-CONDITION
               MOVE PATIENT-RECORD (196:30) TO WS-MEDICATION-NAME
               MOVE PATIENT-RECORD (231:10) TO WS-PROVIDER
               MOVE PATIENT-RECORD (246:10) TO WS-DATE-OF-VISIT
               DISPLAY FRA-ME
               DISPLAY "PERSONAL INFORMATION."
               DISPLAY FRA-ME
               DISPLAY "   Patient Name: " WS-PATIENT-NAME
               DISPLAY "   Age: " WS-PATIENT-AGE
               DISPLAY "   Sex: " WS-PATIENT-SEX
               DISPLAY "   Date of Birth: " WS-DATE-OF-BIRTH
               DISPLAY "   Course/Section: " WS-STUDENT-CYS
               DISPLAY "   Student Number: " WS-STUDENT-NUMBER
               DISPLAY FRA-ME
               DISPLAY "EMERGENCY CONTACT INFORMATION."
               DISPLAY FRA-ME
               DISPLAY "   Phone Number: " WS-PATIENT-CONTACT
               DISPLAY "   Email Address: " WS-PATIENT-EMAIL
               DISPLAY FRA-ME
               DISPLAY "MEDICATION."
               DISPLAY FRA-ME
               DISPLAY "   Health Condition: " WS-HEALTH-CONDITION
               DISPLAY "   Medication Provided: " WS-MEDICATION-NAME
               DISPLAY "   Prescribing Provider: " WS-PROVIDER
               DISPLAY "   Date of Visit: " WS-DATE-OF-VISIT
               DISPLAY FRA-ME
               DISPLAY " "
               READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG
           END-PERFORM.
           CLOSE PATIENT-FILE.
      *
       SEARCH-PATIENT.
           OPEN INPUT PATIENT-FILE.
           DISPLAY "Enter Patients' STUDENT ID to search: ".
           ACCEPT SEARCH-ID.
           
           READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG.

           PERFORM UNTIL EOF-FLAG = "Y"
               MOVE PATIENT-RECORD (1:30) TO WS-PATIENT-NAME
               MOVE PATIENT-RECORD (31:15) TO WS-STUDENT-NUMBER
               MOVE PATIENT-RECORD (51:30) TO WS-STUDENT-CYS
               MOVE PATIENT-RECORD (66:10) TO WS-DATE-OF-BIRTH
               MOVE PATIENT-RECORD (81:2) TO WS-PATIENT-AGE
               MOVE PATIENT-RECORD (88:1) TO WS-PATIENT-SEX
               MOVE PATIENT-RECORD (94:12) TO WS-PATIENT-CONTACT
               MOVE PATIENT-RECORD (111:50) TO WS-PATIENT-EMAIL
               MOVE PATIENT-RECORD (161:30) TO WS-HEALTH-CONDITION
               MOVE PATIENT-RECORD (196:30) TO WS-MEDICATION-NAME
               MOVE PATIENT-RECORD (231:10) TO WS-PROVIDER
               MOVE PATIENT-RECORD (246:10) TO WS-DATE-OF-VISIT
              
               IF WS-STUDENT-NUMBER = SEARCH-ID THEN
                   DISPLAY FRA-ME
                   DISPLAY "PERSONAL INFORMATION."
                   DISPLAY "   Patient Name: " WS-PATIENT-NAME
                   DISPLAY "   Age: " WS-PATIENT-AGE
                   DISPLAY "   Sex: " WS-PATIENT-SEX
                   DISPLAY "   Date of Birth: " WS-DATE-OF-BIRTH
                   DISPLAY "   Course/Section: " WS-STUDENT-CYS
                   DISPLAY "   Student Number: " WS-STUDENT-NUMBER
                   DISPLAY "EMERGENCY CONTACT INFORMATION."
                   DISPLAY "   Phone Number: " WS-PATIENT-CONTACT
                   DISPLAY "   Email Address: " WS-PATIENT-EMAIL
                   DISPLAY "MEDICATION."
                   DISPLAY "   Health Condition: " WS-HEALTH-CONDITION
                   DISPLAY "   Medication Provided: " WS-MEDICATION-NAME
                   DISPLAY "   Prescribing Provider: " WS-PROVIDER
                   DISPLAY "   Date of Visit: " WS-DATE-OF-VISIT
                   DISPLAY FRA-ME
                   MOVE "Y" TO EOF-FLAG
               END-IF

               READ PATIENT-FILE AT END MOVE "Y" TO EOF-FLAG
           END-PERFORM.

           CLOSE PATIENT-FILE.
      *
       UPDATE-PATIENT.
      * (Implementation for updating patient records)

       EXIT-PROGRAM.
           DISPLAY "Exiting Medika.".
