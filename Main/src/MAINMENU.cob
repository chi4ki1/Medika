// In MEDIKA/src/MAINMENU.cob
IDENTIFICATION DIVISION.
PROGRAM-ID. MAINMENU.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. IBM-PC. 
OBJECT-COMPUTER. IBM-PC.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  USER-CHOICE PIC X.

PROCEDURE DIVISION.
MAIN-PARAGRAPH.
    DISPLAY "Medika Main Menu".
    DISPLAY "a) Add New Patient Record".
    DISPLAY "b) View All Patient Records".
    DISPLAY "c) Search Patient Record".
    DISPLAY "d) Update Patient Info".
    DISPLAY "e) Exit Program".
    ACCEPT USER-CHOICE.

    EVALUATE USER-CHOICE
        WHEN "a"
            PERFORM ADD-PATIENT
        WHEN "b"
            PERFORM VIEW-PATIENTS
        WHEN "c"
            PERFORM SEARCH-PATIENT
        WHEN "d"
            PERFORM UPDATE-PATIENT
        WHEN "e"
            PERFORM EXIT-PROGRAM
        WHEN OTHER
            DISPLAY "Invalid choice."
    END-EVALUATE.

    STOP RUN.

ADD-PATIENT.
    * Code to add a new patient record
    CALL "PATIENT" USING "ADD". 

VIEW-PATIENTS.
    * Code to view all patient records
    CALL "PATIENT" USING "VIEW". 

SEARCH-PATIENT.
    * Code to search for a patient record
    CALL "PATIENT" USING "SEARCH". 

UPDATE-PATIENT.
    * Code to update a patient record
    CALL "PATIENT" USING "UPDATE". 

EXIT-PROGRAM.
    DISPLAY "Exiting Medika."
