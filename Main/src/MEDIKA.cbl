       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIKA.
       AUTHOR. JAYSON & NYZEL.
       INSTALLATION. COBOLT CO.
       DATE-WRITTEN. 2024-11-05.
       DATE-COMPILED.2024-12-11.
       SECURITY. Confidential.
       REMARKS. Empowering Clinics: COBOL towards reliable healthcare management.
       STATUS. Production.
       PURPOSE. Manage patient records and streamline clinic operations.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PATIENT-FILE ASSIGN TO 
           "/home/zelly/PATIENTS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
               *> Holds patient records for reading and processing.
           SELECT TEMP-FILE ASSIGN TO 
           "/home/zelly/TEMP.txt"
               ORGANIZATION IS LINE SEQUENTIAL. 
               *> Temporary file for safely updating records.
       
       DATA DIVISION.
       FILE SECTION.
       FD  PATIENT-FILE.
       01  PATIENT-RECORD.
               *> Structure for each patient’s stored data.
           05  STUDENT-NUMBER      PIC X(20).                           *> Unique ID of the student.
           05  PATIENT-NAME        PIC X(30).                           *> Full name of the patient.
           05  STUDENT-CYS         PIC X(30).                           *> Course and section details.
           05  DATE-OF-BIRTH       PIC X(15).                           *> Date of birth (YYYY/MM/DD).
           05  PATIENT-AGE         PIC X(5).                            *> Age of the patient.
           05  PATIENT-SEX         PIC X(5).                            *> Gender of the patient.
           05  EMERGENCY-PHONE     PIC X(15).                           *> Emergency contact number.
           05  EMERGENCY-EMAIL     PIC X(30).                           *> Emergency email address.
           05  HEALTH-CONDITION    PIC X(30).                           *> Health condition description.
           05  MEDICATION-NAME     PIC X(30).                           *> Prescribed medication name.
           05  PRESCRIBER          PIC X(30).                           *> Name of the prescribing provider.
           05  DATE-OF-VISIT       PIC X(11).                           *> Last visit date (YYYY/MM/DD).
       
       FD TEMP-FILE.                                                    *> Temporary record for processing updates.
       01 TEMP-RECORD.                                                  *> Temporary variables for input and editing operations.
           05  TEMP-STUDENT-NUMBER      PIC X(20).                      
           05  TEMP-PATIENT-NAME        PIC X(30).
           05  TEMP-STUDENT-CYS         PIC X(30).
           05  TEMP-DATE-OF-BIRTH       PIC X(15).
           05  TEMP-PATIENT-AGE         PIC X(5).
           05  TEMP-PATIENT-SEX         PIC X(5).
           05  TEMP-EMERGENCY-PHONE     PIC X(15).
           05  TEMP-EMERGENCY-EMAIL     PIC X(30).
           05  TEMP-HEALTH-CONDITION    PIC X(30).
           05  TEMP-MEDICATION-NAME     PIC X(30).
           05  TEMP-PRESCRIBER          PIC X(30).
           05  TEMP-DATE-OF-VISIT       PIC X(11). 
       
       WORKING-STORAGE SECTION.
       01  USER-CHOICE             PIC X.
       01  WS-EOF-FLAG             PIC X VALUE "N".
       01  FRA-ME                  PIC X(35) 
           VALUE "===================================".
       01  NEW-LINE                PIC X VALUE X"0A".
       01  WS-EDIT-CHOICE         PIC X.
       01  PATIENT-DATA.
           05  WS-STUDENT-NUMBER    PIC X(20).
           05  WS-PATIENT-NAME      PIC X(30).
           05  WS-STUDENT-CYS       PIC X(30).
           05  WS-DATE-OF-BIRTH     PIC X(15).
           05  WS-PATIENT-AGE       PIC X(5).
           05  WS-PATIENT-SEX       PIC X(5).
           05  WS-EMERGENCY-PHONE   PIC X(15).
           05  WS-EMERGENCY-EMAIL   PIC X(30).
           05  WS-HEALTH-CONDITION  PIC X(30).
           05  WS-MEDICATION-NAME   PIC X(30).
           05  WS-PRESCRIBER        PIC X(30).
           05  WS-DATE-OF-VISIT     PIC X(11). 
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.                                                  *> Main control loop for user interactions.
           PERFORM DISPLAY-MENU
           PERFORM UNTIL USER-CHOICE = "x" OR USER-CHOICE = "X"
               DISPLAY NEW-LINE
               DISPLAY "Enter your choice: " WITH NO ADVANCING
               ACCEPT USER-CHOICE
               EVALUATE USER-CHOICE
                   WHEN "a"
                   WHEN "A"
                       PERFORM ADD-PATIENT
                   WHEN "b"
                   WHEN "B"
                       PERFORM VIEW-PATIENTS
                   WHEN "c"
                   WHEN "C"
                       PERFORM SEARCH-PATIENT
                   WHEN "d"
                       WHEN "D"
                       PERFORM UPDATE-PATIENT
                   WHEN "x"
                   WHEN "X"
                       PERFORM EXIT-PROGRAM
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
           END-PERFORM
           STOP RUN.
       
       DISPLAY-MENU.
           CALL "SYSTEM" USING "clear"                                  *> Clear screen and show the main menu.
           DISPLAY FRA-ME
           DISPLAY "      MEDIKA PATIENT RECORD SYSTEM"
           DISPLAY FRA-ME
           DISPLAY "What would you like to do?"
           DISPLAY NEW-LINE
           DISPLAY "a) Add New Patient Record"
           DISPLAY "b) View All Patient Records"
           DISPLAY "c) Search Patient Record"
           DISPLAY "d) Update Patient Info"
           DISPLAY "x) Exit Program".
       
       ADD-PATIENT.                                                     *> Collect and save a new patient's record.
           CALL "SYSTEM" USING "clear"
           OPEN EXTEND PATIENT-FILE
           DISPLAY FRA-ME
           DISPLAY "        ADD PATIENT RECORD"
           DISPLAY FRA-ME

           DISPLAY "PERSONAL INFORMATION."

           DISPLAY "   Student ID: " WITH NO ADVANCING
           ACCEPT WS-STUDENT-NUMBER
           DISPLAY "   Full Name: " WITH NO ADVANCING
           ACCEPT WS-PATIENT-NAME
           DISPLAY "   Course/Section: " WITH NO ADVANCING
           ACCEPT WS-STUDENT-CYS
           DISPLAY "   Date of Birth [YYYY/MM/DD]: " WITH NO ADVANCING
           ACCEPT WS-DATE-OF-BIRTH
           DISPLAY "   Sex [F/M]: " WITH NO ADVANCING
           ACCEPT WS-PATIENT-SEX
           DISPLAY "   Age: " WITH NO ADVANCING
           ACCEPT WS-PATIENT-AGE

           DISPLAY "EMERGENCY CONTACT INFORMATION."

           DISPLAY "   Emergency Contact Phone: " WITH NO ADVANCING
           ACCEPT WS-EMERGENCY-PHONE
           DISPLAY "   Emergency Email Address: " WITH NO ADVANCING
           ACCEPT WS-EMERGENCY-EMAIL

           DISPLAY "MEDICATION."

           DISPLAY "   Health Condition: " WITH NO ADVANCING
           ACCEPT WS-HEALTH-CONDITION
           DISPLAY "   Medication Name: " WITH NO ADVANCING
           ACCEPT WS-MEDICATION-NAME
           DISPLAY "   Prescribing Provider: " WITH NO ADVANCING
           ACCEPT WS-PRESCRIBER
           DISPLAY "   Date of Visit [YYYY/MM/DD]: " WITH NO ADVANCING
           ACCEPT WS-DATE-OF-VISIT
                                                                        *> Transfer input data to patient record structure.
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
           DISPLAY FRA-ME.
           DISPLAY "Record successfully added!"
           CLOSE PATIENT-FILE.
           DISPLAY FRA-ME.
           DISPLAY "Add New Patient Record? [Y/N]: " WITH NO ADVANCING
           ACCEPT USER-CHOICE.
       
           IF USER-CHOICE = "Y" OR USER-CHOICE = "y"                    *> Recursive prompt for adding another record.
               PERFORM ADD-PATIENT
           ELSE
               DISPLAY "Record not added.".
           PERFORM DISPLAY-MENU.
       
       VIEW-PATIENTS.
           CALL "SYSTEM" USING "clear"                                  *> Clear the screen before displaying all records.
           OPEN INPUT PATIENT-FILE
           DISPLAY FRA-ME
           DISPLAY "      VIEW ALL PATIENT RECORDS"
           DISPLAY FRA-ME
           MOVE "N" TO WS-EOF-FLAG                                      *> Initialize end-of-file flag.
       
           PERFORM UNTIL WS-EOF-FLAG = "Y"                              *> Loop through all records in the file until EOF.
               READ PATIENT-FILE INTO PATIENT-RECORD
                   AT END
                       MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END                                           *> Display patient details for the current record.
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
                       DISPLAY " " 
               END-READ
           END-PERFORM
       
           CLOSE PATIENT-FILE                                           *> Return to the main menu after user acknowledges.
           DISPLAY "Press Enter to return to the main menu." 
           WITH NO ADVANCING
           ACCEPT USER-CHOICE. 
           PERFORM DISPLAY-MENU.
              
       SEARCH-PATIENT.
           CALL "SYSTEM" USING "clear"                                  
           OPEN INPUT PATIENT-FILE
           DISPLAY FRA-ME
           DISPLAY "      SEARCH PATIENT RECORD"
           DISPLAY FRA-ME
           DISPLAY "Enter Student ID to search: " WITH NO ADVANCING     *> Prompt the user for the Student ID to search.
           ACCEPT WS-STUDENT-NUMBER
           MOVE "N" TO WS-EOF-FLAG
       
           PERFORM UNTIL WS-EOF-FLAG = "Y"                              *> Loop through all records to find the matching Student ID.
               READ PATIENT-FILE INTO PATIENT-RECORD
                   AT END
                       MOVE "Y" TO WS-EOF-FLAG
                       IF WS-EOF-FLAG = "Y"                             *> If EOF is reached without finding, display not found.
                           DISPLAY "Record not found."
            NOT AT END
                IF STUDENT-NUMBER = WS-STUDENT-NUMBER
                    DISPLAY "Student ID: " STUDENT-NUMBER               *> Display details of the matching patient.
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
                    MOVE "Y" TO WS-EOF-FLAG 
                END-IF
        END-READ
           END-PERFORM
       
           CLOSE PATIENT-FILE
           DISPLAY "Press Enter to return to the main menu."
               WITH NO ADVANCING
           ACCEPT USER-CHOICE
           PERFORM DISPLAY-MENU.          
              
       UPDATE-PATIENT.                                                  *> Code for updating a patient record
           
                                                                        *> Describe opening file(s), updating specific fields, and overwriting files
           CALL "SYSTEM" USING "clear"
           OPEN INPUT PATIENT-FILE
           OPEN OUTPUT TEMP-FILE 
           DISPLAY FRA-ME
           DISPLAY "      MEDIKA PATIENT RECORD"
           DISPLAY FRA-ME
           DISPLAY "        UPDATE PATIENT INFO"
           DISPLAY FRA-ME
           
           DISPLAY "Enter Student ID to update: " WITH NO ADVANCING
           ACCEPT WS-STUDENT-NUMBER
           MOVE "N" TO WS-EOF-FLAG
       
           PERFORM UNTIL WS-EOF-FLAG = "Y" 
        READ PATIENT-FILE INTO PATIENT-RECORD
            AT END
                MOVE "Y" TO WS-EOF-FLAG
            NOT AT END
                IF STUDENT-NUMBER = WS-STUDENT-NUMBER
                    PERFORM EDIT-RECORD 
                END-IF
                
                MOVE STUDENT-NUMBER TO TEMP-STUDENT-NUMBER
                MOVE PATIENT-NAME TO TEMP-PATIENT-NAME
                MOVE STUDENT-CYS TO TEMP-STUDENT-CYS
                MOVE DATE-OF-BIRTH TO TEMP-DATE-OF-BIRTH
                MOVE PATIENT-AGE TO TEMP-PATIENT-AGE
                MOVE PATIENT-SEX TO TEMP-PATIENT-SEX
                MOVE EMERGENCY-PHONE TO TEMP-EMERGENCY-PHONE
                MOVE EMERGENCY-EMAIL TO TEMP-EMERGENCY-EMAIL
                MOVE HEALTH-CONDITION TO TEMP-HEALTH-CONDITION
                MOVE MEDICATION-NAME TO TEMP-MEDICATION-NAME
                MOVE PRESCRIBER TO TEMP-PRESCRIBER
                MOVE DATE-OF-VISIT TO TEMP-DATE-OF-VISIT
                WRITE TEMP-RECORD 
        END-READ
           END-PERFORM
       
           CLOSE PATIENT-FILE
           CLOSE TEMP-FILE
       
       CALL "SYSTEM" USING "rm /home/zelly/PATIENTS.txt"
       CALL "SYSTEM" 
           USING "mv /home/zelly/TEMP.txt" &
                 " /home/zelly/PATIENTS.txt"
       
           DISPLAY FRA-ME
           DISPLAY "Patient record updated."
           DISPLAY "Press Enter to return to the main menu."
           WITH NO ADVANCING
           ACCEPT USER-CHOICE
           PERFORM DISPLAY-MENU.      
       
       EDIT-RECORD.                                                     *> Provide details of editing a specific patient record, explaining the steps
           DISPLAY "   0. Student ID: " STUDENT-NUMBER
           DISPLAY "   1. Full Name: " PATIENT-NAME
           DISPLAY "   2. Course/Section: " STUDENT-CYS
           DISPLAY "   3. Date of Birth: " DATE-OF-BIRTH
           DISPLAY "   4. Age: " PATIENT-AGE
           DISPLAY "   5. Sex: " PATIENT-SEX
           DISPLAY "   6. Health Condition: " HEALTH-CONDITION
           DISPLAY "   7. Medication Name: " MEDICATION-NAME
           DISPLAY "   8. Prescriber: " PRESCRIBER
           DISPLAY "   9. Date of Visit: " DATE-OF-VISIT 
           DISPLAY "Select the field to edit [0-9]: " 
               WITH NO ADVANCING
       
           PERFORM GET-VALID-CHOICE.                                    *> Validate user input for selecting a specific field to update
       
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
           END-EVALUATE. 
       
       GET-VALID-CHOICE.
           ACCEPT WS-EDIT-CHOICE
          PERFORM UNTIL WS-EDIT-CHOICE >= "0" AND WS-EDIT-CHOICE <= "9"
               DISPLAY "Invalid input range. Please re-enter (0-9): " 
                   WITH NO ADVANCING
               ACCEPT WS-EDIT-CHOICE
           END-PERFORM.
              
       
       EXIT-PROGRAM.
           CALL "SYSTEM" USING "clear"
           DISPLAY "Exiting Medika.".                                   *> Display exit message and terminate the program.
           STOP RUN.
     
