       IDENTIFICATION DIVISION.
       *> This division identifies the program.
       PROGRAM-ID. MEDIKA.

       ENVIRONMENT DIVISION.
       *> This division defines the environment in which the program will run.
       INPUT-OUTPUT SECTION.
       *> This section handles input and output operations.
       FILE-CONTROL.
       *> This paragraph links the program to external files.
           SELECT PATIENT-FILE ASSIGN TO
           "/Users/apple/Documents/MEDIKA/PATIENT.DAT"
       *> If you try run this program in other OS, change the DIR path.
               ORGANIZATION IS LINE SEQUENTIAL.
               *> Defines a file named PATIENT-FILE and connects it to the
               *> physical file PATIENT.DAT. The file is organized as line sequential.

       DATA DIVISION.
       *> This division describes the data used by the program.
       FILE SECTION.
       *> This section describes the structure of files.
       FD  PATIENT-FILE.
       *> Defines the file description (FD) for PATIENT-FILE.
       01  PATIENT-RECORD.
       *> Defines a record structure named PATIENT-RECORD within the file.
           05  STUDENT-NUMBER      PIC X(10).
           *> Defines a field named STUDENT-NUMBER with a size of 10 characters.
           05  PATIENT-NAME        PIC X(30).
           *> Defines a field named PATIENT-NAME with a size of 30 characters.
           05  STUDENT-CYS         PIC X(30).
           *> Defines a field named STUDENT-CYS with a size of 30 characters.
           05  DATE-OF-BIRTH       PIC X(10).
           *> Defines a field named DATE-OF-BIRTH with a size of 10 characters.
           05  PATIENT-AGE         PIC 99.
           *> Defines a numeric field named PATIENT-AGE with a size of 2 digits.
           05  PATIENT-SEX         PIC X(1).
           *> Defines a field named PATIENT-SEX with a size of 1 character.
           05  EMERGENCY-PHONE     PIC X(15).
           *> Defines a field named EMERGENCY-PHONE with a size of 15 characters.
           05  EMERGENCY-EMAIL     PIC X(30).
           *> Defines a field named EMERGENCY-EMAIL with a size of 30 characters.
           05  HEALTH-CONDITION    PIC X(30).
           *> Defines a field named HEALTH-CONDITION with a size of 30 characters.
           05  MEDICATION-NAME     PIC X(30).
           *> Defines a field named MEDICATION-NAME with a size of 30 characters.
           05  PRESCRIBER          PIC X(30).
           *> Defines a field named PRESCRIBER with a size of 30 characters.
           05  DATE-OF-VISIT       PIC X(11).
           *> Defines a field named DATE-OF-VISIT with a size of 11 characters.

       WORKING-STORAGE SECTION.
       *> This section defines variables used for processing within the program.
       01  USER-CHOICE             PIC X.
           *> Defines a variable named USER-CHOICE to store user input (1 character).
       01  WS-EOF-FLAG             PIC X VALUE "N".
           *> Defines a flag named WS-EOF-FLAG to indicate end-of-file (initialized to "N").
       01  FRA-ME                  PIC X(35)
           VALUE "===================================".
           *> Defines a variable named FRA-ME to store a string of "=" characters.
       01  NEW-LINE                PIC X VALUE X"0A".
           *> Defines a variable named NEW-LINE to store a newline character.
       01  WS-EDIT-CHOICE         PIC X.
           *> Defines a variable named WS-EDIT-CHOICE to store user input for editing choice (1 character).
       01  PATIENT-DATA.
           *> Defines a group variable named PATIENT-DATA to hold patient information.
           05  WS-STUDENT-NUMBER    PIC X(10).
           *> Defines a field named WS-STUDENT-NUMBER with a size of 10 characters.
           05  WS-PATIENT-NAME      PIC X(30).
           *> Defines a field named WS-PATIENT-NAME with a size of 30 characters.
           05  WS-STUDENT-CYS       PIC X(30).
           *> Defines a field named WS-STUDENT-CYS with a size of 30 characters.
           05  WS-DATE-OF-BIRTH     PIC X(10).
           *> Defines a field named WS-DATE-OF-BIRTH with a size of 10 characters.
           05  WS-PATIENT-AGE       PIC 99.
           *> Defines a numeric field named WS-PATIENT-AGE with a size of 2 digits.
           05  WS-PATIENT-SEX       PIC X(1).
           *> Defines a field named WS-PATIENT-SEX with a size of 1 character.
           05  WS-EMERGENCY-PHONE   PIC X(15).
           *> Defines a field named WS-EMERGENCY-PHONE with a size of 15 characters.
           05  WS-EMERGENCY-EMAIL   PIC X(30).
           *> Defines a field named WS-EMERGENCY-EMAIL with a size of 30 characters.
           05  WS-HEALTH-CONDITION  PIC X(30).
           *> Defines a field named WS-HEALTH-CONDITION with a size of 30 characters.
           05  WS-MEDICATION-NAME   PIC X(30).
           *> Defines a field named WS-MEDICATION-NAME with a size of 30 characters.
           05  WS-PRESCRIBER        PIC X(30).
           *> Defines a field named WS-PRESCRIBER with a size of 30 characters.
           05  WS-DATE-OF-VISIT     PIC X(11).
           *> Defines a field named WS-DATE-OF-VISIT with a size of 11 characters.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM DISPLAY-MENU
      * Display the main menu to the user.
           PERFORM UNTIL USER-CHOICE = "x" OR USER-CHOICE = "X"
               DISPLAY NEW-LINE
               DISPLAY "Enter your choice: " WITH NO ADVANCING
               ACCEPT USER-CHOICE
      * Accept user input for menu selection.

               EVALUATE USER-CHOICE
      * Evaluate the user's choice.
                   WHEN "a"
      * Check for option "a" or "A" to add a patient record.
                   WHEN "A"
                       PERFORM ADD-PATIENT
                   WHEN "b"
      * Check for option "b" or "B" to view all patient records.
                   WHEN "B"
                       PERFORM VIEW-PATIENTS
                   WHEN "c"
      * Check for option "c" or "C" to search for a specific patient.
                   WHEN "C"
                       PERFORM SEARCH-PATIENT
                   WHEN "d"
      * Check for option "d" or "D" to update a patient record.
                   WHEN "D"
                       PERFORM UPDATE-PATIENT
                   WHEN "x"
      * Check for option "x" or "X" to exit the program.
                   WHEN "X"
                       PERFORM EXIT-PROGRAM
                   WHEN OTHER
      * Handle invalid input from the user.
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
           END-PERFORM
           STOP RUN.
      * End the program execution.


       DISPLAY-MENU.
           CALL "SYSTEM" USING "clear"
      * Clears the screen to display a fresh menu.
           DISPLAY FRA-ME
      * Displays a frame for visual separation.
           DISPLAY "      MEDIKA PATIENT RECORD SYSTEM"
      * Displays the system title.
           DISPLAY FRA-ME
      * Displays another frame for emphasis.
           DISPLAY "What would you like to do?"
      * Prompt for user selection.
           DISPLAY NEW-LINE
      * Adds a blank line for spacing.
           DISPLAY "a) Add New Patient Record"
      * Option to add a new patient record.
           DISPLAY "b) View All Patient Records"
      * Option to view all patient records.
           DISPLAY "c) Search Patient Record"
      * Option to search for a specific patient.
           DISPLAY "d) Update Patient Info"
      * Option to update patient information.
           DISPLAY "x) Exit Program".
      * Option to exit the program.


       ADD-PATIENT.
           *> This paragraph handles adding new patient records to the file.
           CALL "SYSTEM" USING "clear"
           *> Clears the screen.
           OPEN EXTEND PATIENT-FILE
           *> Opens the PATIENT-FILE in EXTEND mode (to add records at the end).
           DISPLAY FRA-ME
           *> Displays the frame (a line of "=" characters).
           DISPLAY "        ADD PATIENT RECORD"
           *> Displays the title "ADD PATIENT RECORD".
           DISPLAY FRA-ME

           DISPLAY "   Student ID: " WITH NO ADVANCING
           *> Prompts the user to enter the Student ID.
           ACCEPT WS-STUDENT-NUMBER
           *> Accepts the input and stores it in WS-STUDENT-NUMBER.
           DISPLAY "   Full Name: " WITH NO ADVANCING
           *> Prompts for the Full Name.
           ACCEPT WS-PATIENT-NAME
           *> Stores the input in WS-PATIENT-NAME.
           DISPLAY "   Course/Section [N/A if none]: " WITH NO ADVANCING
           *> Prompts for the Course/Section.
           ACCEPT WS-STUDENT-CYS
           *> Stores the input in WS-STUDENT-CYS.
           DISPLAY "   Date of Birth [YYYY/MM/DD]: " WITH NO ADVANCING
           *> Prompts for the Date of Birth.
           ACCEPT WS-DATE-OF-BIRTH
           *> Stores the input in WS-DATE-OF-BIRTH.
           DISPLAY "   Sex [F/M]: " WITH NO ADVANCING
           *> Prompts for the Sex.
           ACCEPT WS-PATIENT-SEX
           *> Stores the input in WS-PATIENT-SEX.
           DISPLAY "   Age: " WITH NO ADVANCING
           *> Prompts for the Age.
           ACCEPT WS-PATIENT-AGE
           *> Stores the input in WS-PATIENT-AGE.

           DISPLAY "   Emergency Contact Phone: " WITH NO ADVANCING
           *> Prompts for the Emergency Contact Phone.
           ACCEPT WS-EMERGENCY-PHONE
           *> Stores the input in WS-EMERGENCY-PHONE.
           DISPLAY "   Emergency Email Address: " WITH NO ADVANCING
           *> Prompts for the Emergency Email Address.
           ACCEPT WS-EMERGENCY-EMAIL
           *> Stores the input in WS-EMERGENCY-EMAIL.

           DISPLAY "   Health Condition: " WITH NO ADVANCING
           *> Prompts for the Health Condition.
           ACCEPT WS-HEALTH-CONDITION
           *> Stores the input in WS-HEALTH-CONDITION.
           DISPLAY "   Medication Name: " WITH NO ADVANCING
           *> Prompts for the Medication Name.
           ACCEPT WS-MEDICATION-NAME
           *> Stores the input in WS-MEDICATION-NAME.
           DISPLAY "   Prescribing Provider: " WITH NO ADVANCING
           *> Prompts for the Prescribing Provider.
           ACCEPT WS-PRESCRIBER
           *> Stores the input in WS-PRESCRIBER.
           DISPLAY "   Date of Visit [YYYY/MM/DD]: " WITH NO ADVANCING
           *> Prompts for the Date of Visit.
           ACCEPT WS-DATE-OF-VISIT
           *> Stores the input in WS-DATE-OF-VISIT.

           *> The following MOVE statements transfer the data from the
           *> working-storage variables (WS-*) to the corresponding fields
           *> in the PATIENT-RECORD structure.
           MOVE WS-STUDENT-NUMBER TO STUDENT-NUMBER
           MOVE WS-PATIENT-NAME TO PATIENT-NAME
           MOVE WS-STUDENT-CYS TO STUDENT-CYS
           MOVE WS-DATE-OF-BIRTH TO DATE-OF-BIRTH
           MOVE WS-PATIENT-SEX TO PATIENT-SEX
           MOVE WS-PATIENT-AGE TO PATIENT-AGE
           MOVE WS-EMERGENCY-PHONE TO EMERGENCY-PHONE
           MOVE WS-EMERGENCY-EMAIL TO EMERGENCY-EMAIL
           MOVE WS-HEALTH-CONDITION TO HEALTH-CONDITION
           MOVE WS-MEDICATION-NAME TO MEDICATION-NAME
           MOVE WS-PRESCRIBER TO PRESCRIBER
           MOVE WS-DATE-OF-VISIT TO DATE-OF-VISIT

           WRITE PATIENT-RECORD
           *> Writes the PATIENT-RECORD to the file.
           DISPLAY "Record successfully added!"
           *> Informs the user that the record has been added.
           CLOSE PATIENT-FILE.
           *> Closes the PATIENT-FILE.
           DISPLAY FRA-ME
           *> Displays the frame.
           DISPLAY "Add New Patient Record? [Y/N]: " WITH NO ADVANCING.
           *> Asks the user if they want to add another record.
           ACCEPT USER-CHOICE
           *> Accepts the user's choice (Y/N).

           IF USER-CHOICE = "Y" OR USER-CHOICE = "y"
           *> If the user enters "Y" or "y",
               PERFORM ADD-PATIENT
               *> Calls the ADD-PATIENT paragraph again to add another record.
           ELSE
               DISPLAY "Record not added."
               *> Otherwise, informs the user that no record was added.
           PERFORM DISPLAY-MENU.
           *> Calls the DISPLAY-MENU paragraph to show the main menu.

       VIEW-PATIENTS.
           CALL "SYSTEM" USING "clear"
           OPEN INPUT PATIENT-FILE
           DISPLAY FRA-ME
           DISPLAY "      VIEW ALL PATIENT RECORDS"
           DISPLAY FRA-ME
           MOVE "N" TO WS-EOF-FLAG

           PERFORM UNTIL WS-EOF-FLAG = "Y"
               READ PATIENT-FILE INTO PATIENT-RECORD
                   AT END
                       MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       DISPLAY "Student ID: " STUDENT-NUMBER
                       DISPLAY "Name: " PATIENT-NAME
                       DISPLAY "Course/Section: " STUDENT-CYS
                       DISPLAY "Date of Birth: " DATE-OF-BIRTH
                       DISPLAY "Age: " PATIENT-AGE
                       DISPLAY "Sex: " PATIENT-SEX
                       DISPLAY "Emergency Phone: " EMERGENCY-PHONE
                       DISPLAY "Emergency Email: " EMERGENCY-EMAIL
                       DISPLAY "Health Condition: " HEALTH-CONDITION
                       DISPLAY "Medication: " MEDICATION-NAME
                       DISPLAY "Prescriber: " PRESCRIBER
                       DISPLAY "Date of Visit: " DATE-OF-VISIT
                       DISPLAY FRA-ME
                       DISPLAY ""
               END-READ
           END-PERFORM

           CLOSE PATIENT-FILE
           DISPLAY "Press Enter to return to the main menu."
           WITH NO ADVANCING
           ACCEPT USER-CHOICE.
           PERFORM DISPLAY-MENU.



       SEARCH-PATIENT.
           *> This paragraph handles searching for a patient record by Student ID.
           CALL "SYSTEM" USING "clear"
           *> Clears the screen.
           OPEN INPUT PATIENT-FILE
           *> Opens the PATIENT-FILE in INPUT mode (for reading data).
           DISPLAY FRA-ME
           *> Displays the frame (a line of "=" characters).
           DISPLAY "      SEARCH PATIENT RECORD"
           *> Displays the title "SEARCH PATIENT RECORD".
           DISPLAY FRA-ME
           DISPLAY "Enter Student ID to search: " WITH NO ADVANCING
           *> Prompts the user to enter the Student ID to search for.
           ACCEPT WS-STUDENT-NUMBER
           *> Accepts the input and stores it in WS-STUDENT-NUMBER.
           MOVE "N" TO WS-EOF-FLAG
           *> Sets the end-of-file flag (WS-EOF-FLAG) to "N" (not end-of-file).

           *> This loop continues until the end of the file is reached
           *> or the record with the matching Student ID is found.
           PERFORM UNTIL WS-EOF-FLAG = "Y"
               READ PATIENT-FILE INTO PATIENT-RECORD
                   *> Reads a record from PATIENT-FILE into PATIENT-RECORD.
                   AT END
                       *> If the end of the file is reached,
                       MOVE "Y" TO WS-EOF-FLAG
                       *> Set the end-of-file flag to "Y".
                       IF WS-EOF-FLAG = "Y"
                           *> If the end-of-file flag is "Y" (no match found),
                           DISPLAY "Record not found."
                           *> Display "Record not found.".
                   NOT AT END
                       *> If a record is read successfully,
                       IF STUDENT-NUMBER = WS-STUDENT-NUMBER
                           *> If the STUDENT-NUMBER in the record matches the
                           *> entered Student ID,
                           DISPLAY "Student ID: " STUDENT-NUMBER
                           *> Display the Student ID.
                           DISPLAY "Name: " PATIENT-NAME
                           *> Display the Patient Name.
                           DISPLAY "Course/Section: " STUDENT-CYS
                           *> Display the Course/Section.
                           DISPLAY "Date of Birth: " DATE-OF-BIRTH
                           *> Display the Date of Birth.
                           DISPLAY "Age: " PATIENT-AGE
                           *> Display the Age.
                           DISPLAY "Sex: " PATIENT-SEX
                           *> Display the Sex.
                           DISPLAY "Emergency Phone: " EMERGENCY-PHONE
                           *> Display the Emergency Phone.
                           DISPLAY "Emergency Email: " EMERGENCY-EMAIL
                           *> Display the Emergency Email.
                           DISPLAY "Health Condition: " HEALTH-CONDITION
                           *> Display the Health Condition.
                           DISPLAY "Medication: " MEDICATION-NAME
                           *> Display the Medication.
                           DISPLAY "Prescriber: " PRESCRIBER
                           *> Display the Prescriber.
                           DISPLAY "Date of Visit: " DATE-OF-VISIT
                           *> Display the Date of Visit.
                           DISPLAY FRA-ME
                           *> Display the frame.
                           MOVE "Y" TO WS-EOF-FLAG
                           *> Set the end-of-file flag to "Y" to exit the loop.
                       END-IF
               END-READ
           END-PERFORM

           CLOSE PATIENT-FILE
           *> Closes the PATIENT-FILE.
           DISPLAY "Press Enter to return to the main menu."
               WITH NO ADVANCING
           *> Prompts the user to press Enter to return to the main menu.
           ACCEPT USER-CHOICE
           *> Waits for the user to press Enter.
           PERFORM DISPLAY-MENU.
           *> Calls the DISPLAY-MENU paragraph to show the main menu.





       UPDATE-PATIENT.
           *> This paragraph handles updating existing patient records in the file.
           CALL "SYSTEM" USING "clear"
           *> Clears the screen.
           OPEN I-O PATIENT-FILE
           *> Opens the PATIENT-FILE in I-O mode (for both input and output).
           DISPLAY FRA-ME
           *> Displays the frame (a line of "=" characters).
           DISPLAY "      MEDIKA PATIENT RECORD"
           *> Displays the title "MEDIKA PATIENT RECORD".
           DISPLAY FRA-ME
           DISPLAY "        UPDATE PATIENT INFO"
           *> Displays the subtitle "UPDATE PATIENT INFO".
           DISPLAY FRA-ME

           DISPLAY "Enter Student ID to update: " WITH NO ADVANCING
           *> Prompts the user to enter the Student ID of the record to update.
           ACCEPT WS-STUDENT-NUMBER
           *> Accepts the input and stores it in WS-STUDENT-NUMBER.
           MOVE "N" TO WS-EOF-FLAG
           *> Sets the end-of-file flag (WS-EOF-FLAG) to "N" (not end-of-file).

           *> This loop continues until the end of the file is reached
           *> or the record with the matching Student ID is found.
           PERFORM UNTIL WS-EOF-FLAG = "Y"
               READ PATIENT-FILE INTO PATIENT-RECORD
                   *> Reads a record from PATIENT-FILE into PATIENT-RECORD.
                   AT END
                       *> If the end of the file is reached,
                       MOVE "Y" TO WS-EOF-FLAG
                       *> Set the end-of-file flag to "Y".
                       DISPLAY "Record not found."
                       *> Display "Record not found.".
                   NOT AT END
                       *> If a record is read successfully,
                       IF STUDENT-NUMBER = WS-STUDENT-NUMBER
                           *> If the STUDENT-NUMBER in the record matches the
                           *> entered Student ID,
                           DISPLAY FRA-ME
                           DISPLAY "Patient Record Found!"
                           PERFORM EDIT-RECORD
                               *> Call the EDIT-RECORD paragraph to handle editing the record.
                           MOVE "Y" TO WS-EOF-FLAG
                               *> Set the end-of-file flag to "Y" to exit the loop.
                       END-IF
               END-READ
           END-PERFORM

           CLOSE PATIENT-FILE
           *> Closes the PATIENT-FILE.
           DISPLAY FRA-ME
           *> Displays the frame.
           DISPLAY "Patient record updated."
           DISPLAY "Press Enter to return to the main menu."
               WITH NO ADVANCING
           *> Prompts the user to press Enter to return to the main menu.
           ACCEPT USER-CHOICE
           *> Waits for the user to press Enter.
           PERFORM DISPLAY-MENU.
           *> Calls the DISPLAY-MENU paragraph to show the main menu.

       EDIT-RECORD.
           *> This paragraph handles the editing of a patient record.
           DISPLAY "   0) Student ID: " STUDENT-NUMBER
           *> Displays the current Student ID.
           DISPLAY "   1) Full Name: " PATIENT-NAME
           *> Displays the current Full Name.
           DISPLAY "   2) Course/Section: " STUDENT-CYS
           *> Displays the current Course/Section.
           DISPLAY "   3) Date of Birth: " DATE-OF-BIRTH
           *> Displays the current Date of Birth.
           DISPLAY "   4) Age: " PATIENT-AGE
           *> Displays the current Age.
           DISPLAY "   5) Sex: " PATIENT-SEX
           *> Displays the current Sex.
           DISPLAY "   6) Health Condition: " HEALTH-CONDITION
           *> Displays the current Health Condition.
           DISPLAY "   7) Medication Name: " MEDICATION-NAME
           *> Displays the current Medication Name.
           DISPLAY "   8) Prescriber: " PRESCRIBER
           *> Displays the current Prescriber.
           DISPLAY "   9) Date of Visit: " DATE-OF-VISIT
           *> Displays the current Date of Visit.
           DISPLAY "Select the field to edit [0-9]: "
               WITH NO ADVANCING
           *> Prompts the user to select a field to edit.

           PERFORM GET-VALID-CHOICE.
           *> Calls the GET-VALID-CHOICE paragraph to get a valid input for the field to edit.

           *> This EVALUATE statement checks the user's choice and performs the
           *> corresponding action to update the selected field.
           EVALUATE WS-EDIT-CHOICE
               WHEN "0"
                   DISPLAY "Enter the new Student ID: "
                   WITH NO ADVANCING
                   ACCEPT WS-STUDENT-NUMBER
                   MOVE WS-STUDENT-NUMBER TO STUDENT-NUMBER
               WHEN "1"
                   DISPLAY "Enter the new Full Name: "
                   WITH NO ADVANCING
                   ACCEPT WS-PATIENT-NAME
                   MOVE WS-PATIENT-NAME TO PATIENT-NAME
               WHEN "2"
                   DISPLAY "Enter the new Course/Section: "
                   WITH NO ADVANCING
                   ACCEPT WS-STUDENT-CYS
                   MOVE WS-STUDENT-CYS TO STUDENT-CYS
               WHEN "3"
                   DISPLAY "Enter the new Date of Birth [YYYY/MM/DD]: "
                   WITH NO ADVANCING
                   ACCEPT WS-DATE-OF-BIRTH
                   MOVE WS-DATE-OF-BIRTH TO DATE-OF-BIRTH
               WHEN "4"
                   DISPLAY "Enter the new Age: "
                   WITH NO ADVANCING
                   ACCEPT WS-PATIENT-AGE
                   MOVE WS-PATIENT-AGE TO PATIENT-AGE
               WHEN "5"
                   DISPLAY "Enter the new Sex: "
                   WITH NO ADVANCING
                   ACCEPT WS-PATIENT-SEX
                   MOVE WS-PATIENT-SEX TO PATIENT-SEX
               WHEN "6"
                   DISPLAY "Enter the new Health Condition: "
                   WITH NO ADVANCING
                   ACCEPT WS-HEALTH-CONDITION
                   MOVE WS-HEALTH-CONDITION TO HEALTH-CONDITION
               WHEN "7"
                   DISPLAY "Enter the new Medication Name: "
                   WITH NO ADVANCING
                   ACCEPT WS-MEDICATION-NAME
                   MOVE WS-MEDICATION-NAME TO MEDICATION-NAME
               WHEN "8"
                   DISPLAY "Enter the new Prescriber: "
                   WITH NO ADVANCING
                   ACCEPT WS-PRESCRIBER
                   MOVE WS-PRESCRIBER TO PRESCRIBER
               WHEN "9"
                   DISPLAY "Enter the new Date of Visit [YYYY/MM/DD]: "
                   WITH NO ADVANCING
                   ACCEPT WS-DATE-OF-VISIT
               WHEN OTHER
                   DISPLAY "Invalid choice"
                   DISPLAY "No record updated!."
           END-EVALUATE

           REWRITE PATIENT-RECORD
           *> Rewrites the updated PATIENT-RECORD to the file.
           DISPLAY "Record updated successfully!"
           *> Informs the user that the record has been updated.

           DISPLAY "Do you want to edit another field? (Y/N): "
               WITH NO ADVANCING
           *> Asks the user if they want to edit another field.
           ACCEPT USER-CHOICE
           *> Accepts the user's choice (Y/N).
           IF USER-CHOICE = "Y" OR USER-CHOICE = "y"
               PERFORM EDIT-RECORD
           *> If the user enters "Y" or "y", call EDIT-RECORD again to edit another field.
           END-IF.

       GET-VALID-CHOICE.
           *> This paragraph ensures the user enters a valid choice for the field to edit.
           ACCEPT WS-EDIT-CHOICE
           *> Accepts the user's choice for the field to edit.
           PERFORM UNTIL WS-EDIT-CHOICE >= "0" AND WS-EDIT-CHOICE <= "9"
               *> This loop continues until the user enters a valid choice between 0 and 9.
               DISPLAY "Invalid input range. Please re-enter (0-9): "
                   WITH NO ADVANCING
               *> Prompts the user to re-enter a valid choice.
               ACCEPT WS-EDIT-CHOICE
                   *> Accepts the user's input again.
           END-PERFORM.

       EXIT-PROGRAM.
           *> This paragraph handles exiting the program.
           CALL "SYSTEM" USING "clear"
           *> Clears the screen.
           DISPLAY "Exiting Medika."
           *> Displays "Exiting Medika.".
           STOP RUN.
           *> Stops the program execution.
