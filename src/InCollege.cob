>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. INCOLLEGE.


ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    *> Test input file: one line per user response
    SELECT USER-IN ASSIGN TO 'InCollege-Input.txt'
       ORGANIZATION IS LINE SEQUENTIAL.
    *> All displayed messages are also duplicated here
    SELECT USER-OUT ASSIGN TO 'InCollege-Output.txt'
       ORGANIZATION IS LINE SEQUENTIAL.
    *> Persistent storage of up to 5 accounts
    *> OPTIONAL so the first run works even if the file does not exist yet
    SELECT OPTIONAL ACCOUNTS ASSIGN TO 'accounts.dat'
       ORGANIZATION IS LINE SEQUENTIAL.


DATA DIVISION.
FILE SECTION.
FD USER-IN.
01 USER-IN-REC     PIC X(80).
FD USER-OUT.
01 USER-OUT-REC    PIC X(80).
FD ACCOUNTS.
01 ACC-REC         PIC X(80).      *> One line: "username,password"

WORKING-STORAGE SECTION.
01 MSG             PIC X(80).      *> Reusable message buffer for display/logging
01 CHOICE          PIC 99 VALUE 0.       *> Menu choice read as text (e.g., "1", "2")
01 USERNAME        PIC X(15).      *> Limit username to 15 (storage size)
01 PASSWORD        PIC X(12).      *> Password stored max 12 chars
01 INPUT-USER      PIC X(80).      *> Sratch for username length gating
01 USER-LEN        PIC 99 VALUE 0. *> Length of INPUT-USER
01 SKILLS-SELECTION PIC 99 VALUE 0.     *> skills menu choice (numeric)
01  EOF-FLAG          PIC X VALUE "N".    *> "Y" at end of input

*> In-memory table (max 5 accounts)
01 ACCOUNT-COUNT  PIC 9 VALUE 0.
01 USER-TABLE.
       05 USER-ENTRY OCCURS 5 INDEXED BY U-IX.
           10 T-USERNAME  PIC X(15).
           10 T-PASSWORD  PIC X(12).

*> Password validation flags & helpers
01 HAS-UPPER      PIC X VALUE "N".
01 HAS-DIGIT      PIC X VALUE "N".
01 HAS-SPECIAL    PIC X VALUE "N".
01 PW-LEN         PIC 99 VALUE 0.      *> Length of the (trimmed) PASSWORD
01 I              PIC 99 VALUE 0.      *> Loop index for scanning PASSWORD
01 PW-CHAR        PIC X.               *> Current char while scanning PASSWORD
01 FOUND-FLAG     PIC X VALUE "N".     *> "Y" if username is already taken
01 TMP-USER       PIC X(15).           *> Scratch for file load
01 TMP-PASS       PIC X(12).           *> Scratch for file load

PROCEDURE DIVISION.
MAIN-PARA.
    *> Open input/output streams
    OPEN INPUT USER-IN
    OPEN OUTPUT USER-OUT

    *> Load existing accounts (if any) into memory
    PERFORM LOAD-ACCOUNTS

    *> Prompt
    MOVE "Welcome to InCollege!" TO MSG
    PERFORM ECHO-DISPLAY
    MOVE "1. Log In" TO MSG
    PERFORM ECHO-DISPLAY
    MOVE "2. Create New Account" TO MSG
    PERFORM ECHO-DISPLAY
    MOVE "Enter your choice:" TO MSG
    PERFORM ECHO-DISPLAY

    READ USER-IN
       AT END
           MOVE 0 TO CHOICE
    END-READ
    IF FUNCTION TEST-NUMVAL(USER-IN-REC) = 0
        MOVE FUNCTION NUMVAL(USER-IN-REC) TO CHOICE
    ELSE
        MOVE 0 TO CHOICE
    END-IF

    EVALUATE CHOICE
        WHEN 1
           PERFORM LOGIN-UNLIMITED

         WHEN 2
           *> Create New Account
           *> Enforce global limit of 5 accounts.
           IF ACCOUNT-COUNT = 5
               MOVE "All permitted accounts have been created, please come back later" TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               *> Read desired username (validate raw input BEFORE storing to USERNAME)
               MOVE "Please enter your username:" TO MSG
               PERFORM ECHO-DISPLAY
               READ USER-IN

               *> Username empty check
               IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
                   MOVE "Username cannot be empty." TO MSG
                   PERFORM ECHO-DISPLAY
               ELSE
                   *> Length check: Max 15 character
                   IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) > 15
                       MOVE "Username must be 1-15 characters long." TO MSG
                       PERFORM ECHO-DISPLAY
                   ELSE
                       MOVE FUNCTION TRIM(USER-IN-REC) TO USERNAME
                       *> Case-insensitive uniqueness check against in-memory table
                       PERFORM EXISTS-USERNAME
                       IF FOUND-FLAG = "Y"
                           MOVE "Username already exists." TO MSG
                           PERFORM ECHO-DISPLAY
                       ELSE
                           *> Prompt for password and enforce 8–12 via truncation detector
                           MOVE "Please enter your password:" TO MSG
                           PERFORM ECHO-DISPLAY
                           READ USER-IN
                           MOVE FUNCTION TRIM(USER-IN-REC) TO PASSWORD
                           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) >
                              FUNCTION LENGTH(FUNCTION TRIM(PASSWORD))
                              MOVE "Password must be 8-12 characters long." TO MSG
                              PERFORM ECHO-DISPLAY
                           ELSE
                               PERFORM VALIDATE-PASSWORD
                               *> Accept only if all flags satisfied
                               IF HAS-UPPER = "Y" AND HAS-DIGIT = "Y" AND HAS-SPECIAL = "Y"
                                  AND PW-LEN >= 8 AND PW-LEN <= 12
                                   *> Append to table
                                   ADD 1 TO ACCOUNT-COUNT
                                   SET U-IX TO ACCOUNT-COUNT
                                   MOVE USERNAME TO T-USERNAME (U-IX)
                                   MOVE PASSWORD TO T-PASSWORD (U-IX)
                                   *> Save all to persistence
                                   PERFORM SAVE-ACCOUNTS
                                   MOVE "Account created." TO MSG
                                   PERFORM ECHO-DISPLAY
                                ELSE
                                   *> Show ONLY the first failing rule (priority: length → upper → digit → special)
                                   PERFORM REPORT-PASSWORD-ERRORS
                                END-IF
                           END-IF
                        END-IF
                     END-IF
               END-IF
           END-IF

         WHEN OTHER
           MOVE "Invalid choice." TO MSG
           PERFORM ECHO-DISPLAY
     END-EVALUATE.

    CLOSE USER-IN
    CLOSE USER-OUT
    STOP RUN.


*> HELPER FUNCTIONS

*> Centralized print: every MSG goes to screen AND the output file
*> to satisfy the requirement that outputs match exactly.
ECHO-DISPLAY.
       DISPLAY MSG
       MOVE MSG TO USER-OUT-REC
       WRITE USER-OUT-REC.


*> Return an existing username (case-insensitive match) or spaces if not found
EXISTS-USERNAME.
       MOVE "N" TO FOUND-FLAG
       IF ACCOUNT-COUNT > 0
           SET U-IX TO 1
           PERFORM UNTIL U-IX > ACCOUNT-COUNT
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(USERNAME))
                    = FUNCTION UPPER-CASE(FUNCTION TRIM(T-USERNAME (U-IX)))
                    MOVE "Y" TO FOUND-FLAG
                    EXIT PERFORM
               ELSE
                    SET U-IX UP BY 1
               END-IF
           END-PERFORM
       END-IF
       EXIT.


*> Load accounts from accounts.dat into the USER-TABLE
*> Split by comma; both parts must be non-empty; cap at 5
LOAD-ACCOUNTS.
       MOVE 0 TO ACCOUNT-COUNT
       OPEN INPUT ACCOUNTS
       PERFORM UNTIL 1 = 0
           READ ACCOUNTS
               AT END EXIT PERFORM
           END-READ

           *> Split by comma; both parts must be non-empty; cap at 5
           UNSTRING ACC-REC DELIMITED BY ","
               INTO TMP-USER TMP-PASS
           END-UNSTRING
           IF FUNCTION LENGTH(FUNCTION TRIM(TMP-USER)) > 0
                AND FUNCTION LENGTH(FUNCTION TRIM(TMP-PASS)) > 0
                AND ACCOUNT-COUNT < 5
                ADD 1 TO ACCOUNT-COUNT
                SET U-IX TO ACCOUNT-COUNT
                MOVE FUNCTION TRIM(TMP-USER) TO T-USERNAME (U-IX)
                MOVE FUNCTION TRIM(TMP-PASS) TO T-PASSWORD (U-IX)
           END-IF
       END-PERFORM
       CLOSE ACCOUNTS
       EXIT.


*> Save entire table back to accounts.dat (full rewrite)
SAVE-ACCOUNTS.
       OPEN OUTPUT ACCOUNTS
       IF ACCOUNT-COUNT > 0
           SET U-IX TO 1
           PERFORM UNTIL U-IX > ACCOUNT-COUNT
               MOVE SPACES TO ACC-REC
               STRING
                   FUNCTION TRIM(T-USERNAME (U-IX))
                   "," DELIMITED BY SIZE
                   FUNCTION TRIM(T-PASSWORD (U-IX))
                   DELIMITED BY SIZE
                   INTO ACC-REC
               END-STRING
               WRITE ACC-REC
               SET U-IX UP BY 1
           END-PERFORM
       END-IF
       CLOSE ACCOUNTS
       EXIT.


*> Function to validate password
*> Criteria: Minimum of 8 characters, a maximum of 12 characters, at least one capital letter,
*>           one digit, and one special character
VALIDATE-PASSWORD.
       MOVE "N" TO HAS-UPPER HAS-DIGIT HAS-SPECIAL
       MOVE FUNCTION LENGTH(FUNCTION TRIM(PASSWORD)) TO PW-LEN
       MOVE 0 TO I
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > PW-LEN
           MOVE PASSWORD (I:1) TO PW-CHAR
           IF PW-CHAR >= "A" AND PW-CHAR <= "Z"
                MOVE "Y" TO HAS-UPPER
           END-IF
           IF PW-CHAR >= "0" AND PW-CHAR <= "9"
                MOVE "Y" TO HAS-DIGIT
           END-IF
           *> Special = not letter and not digit
           IF NOT (PW-CHAR >= "A" AND PW-CHAR <= "Z")
              AND NOT (PW-CHAR >= "a" AND PW-CHAR <= "z")
              AND NOT (PW-CHAR >= "0" AND PW-CHAR <= "9")
                MOVE "Y" TO HAS-SPECIAL
           END-IF
       END-PERFORM
       EXIT.


*> Function to show the error messages related to password setup
REPORT-PASSWORD-ERRORS.
       *> Does not meet length requirement
       IF PW-LEN < 8 OR PW-LEN > 12
           MOVE "Password must be 8-12 characters long." TO MSG
           PERFORM ECHO-DISPLAY
       ELSE
           *> No uppercase
           IF HAS-UPPER NOT = "Y"
               MOVE "Password must include at least one uppercase letter." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               *> No digit
               IF HAS-DIGIT NOT = "Y"
                   MOVE "Password must include at least one digit." TO MSG
                   PERFORM ECHO-DISPLAY
               ELSE
                   *> No special character
                   IF HAS-SPECIAL NOT = "Y"
                       MOVE "Password must include at least one special character." TO MSG
                       PERFORM ECHO-DISPLAY
                   END-IF
               END-IF
           END-IF
       END-IF
       EXIT.


*> Login with unlimited attempts.
*> Success prints the exact required message and exits the loop.
LOGIN-UNLIMITED.
       PERFORM UNTIL 1 = 0
           MOVE "Please enter your username:" TO MSG
           PERFORM ECHO-DISPLAY
           READ USER-IN
               AT END EXIT PERFORM
           END-READ
           MOVE FUNCTION TRIM(USER-IN-REC) TO USERNAME

           MOVE "Please enter your password:" TO MSG
           PERFORM ECHO-DISPLAY
           READ USER-IN
               AT END EXIT PERFORM
           END-READ
           MOVE FUNCTION TRIM(USER-IN-REC) TO PASSWORD

           PERFORM EXISTS-USERNAME

           IF FOUND-FLAG = "Y"
               IF PASSWORD = T-PASSWORD (U-IX)
                   MOVE "You have successfully logged in" TO MSG
                   PERFORM ECHO-DISPLAY
                   PERFORM NAVIGATION-MENU
                   EXIT PERFORM
               ELSE
                   MOVE "Incorrect username/password, please try again" TO MSG
                   PERFORM ECHO-DISPLAY
               END-IF
           ELSE
               MOVE "Incorrect username/password, please try again" TO MSG
               PERFORM ECHO-DISPLAY
           END-IF
       END-PERFORM
       EXIT.

*> Tien's Implementations on September 9th, 2025
NAVIGATION-MENU.
       MOVE "N" TO EOF-FLAG
       MOVE 0 TO CHOICE
       PERFORM UNTIL EOF-FLAG = "Y" OR CHOICE = 9
           PERFORM DISPLAY-MENU

           READ USER-IN INTO USER-IN-REC
               AT END MOVE "Y" TO EOF-FLAG
           END-READ

           IF EOF-FLAG NOT = "Y"
               IF FUNCTION TEST-NUMVAL(USER-IN-REC) = 0
                   MOVE FUNCTION NUMVAL(USER-IN-REC) TO CHOICE
               ELSE
                   MOVE 0 TO CHOICE
               END-IF

               PERFORM NAV-MENU-CHOICE
           END-IF
       END-PERFORM
       EXIT.

DISPLAY-MENU.
       MOVE "=============================" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "     InCollege Main Menu" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "=============================" TO MSG
       PERFORM ECHO-DISPLAY

       MOVE "  1) Job Search" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "  2) Find Someone You Know" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "  3) Learn a New Skill" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "  9) Exit" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "=============================" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "Enter your choice: " TO MSG
       PERFORM ECHO-DISPLAY
       EXIT.

NAV-MENU-CHOICE.

       EVALUATE CHOICE
           WHEN 1
               MOVE "Job search/internship is under construction." TO MSG
               PERFORM ECHO-DISPLAY
           WHEN 2
               MOVE "Find someone you know is under construction." TO MSG
               PERFORM ECHO-DISPLAY
           WHEN 3
               PERFORM SKILLS-MENU
           WHEN 9
               MOVE "Exiting program..." TO MSG
               PERFORM ECHO-DISPLAY
           WHEN OTHER
               MOVE "Invalid choice, please try again." TO MSG
               PERFORM ECHO-DISPLAY
       END-EVALUATE
       EXIT.




SKILLS-MENU.
    *> Reset skill selection each time this menu is shown
    MOVE 0 TO SKILLS-SELECTION

    *> Skills loop: repeat until user chooses Go Back (9) or EOF
    PERFORM UNTIL SKILLS-SELECTION = 9 OR EOF-FLAG = "Y"
        *> Print skills menu
        MOVE "Choose a skill to learn:" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "  1) Python" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "  2) Excel" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "  3) Public Speaking" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "  4) Time Management" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "  5) Leadership" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "  9) Go Back" TO MSG
        PERFORM ECHO-DISPLAY

        *> Read next choice for skills
        MOVE 0 TO SKILLS-SELECTION
        READ USER-IN INTO USER-IN-REC
            AT END MOVE "Y" TO EOF-FLAG
        END-READ

        IF EOF-FLAG NOT = "Y"
            IF FUNCTION TEST-NUMVAL(USER-IN-REC) = 0
                MOVE FUNCTION NUMVAL(USER-IN-REC) TO SKILLS-SELECTION
            ELSE
                MOVE 0 TO SKILLS-SELECTION  *> Invalid input becomes 0
            END-IF
        END-IF

        *> Handle skill choice
        EVALUATE SKILLS-SELECTION
            WHEN 1
                MOVE "You chose: Python (Under Construction)" TO MSG
                PERFORM ECHO-DISPLAY
            WHEN 2
                MOVE "You chose: Excel (Under Construction)" TO MSG
                PERFORM ECHO-DISPLAY
            WHEN 3
                MOVE "You chose: Public Speaking (Under Construction)" TO MSG
                PERFORM ECHO-DISPLAY
            WHEN 4
                MOVE "You chose: Time Management (Under Construction)" TO MSG
                PERFORM ECHO-DISPLAY
            WHEN 5
                MOVE "You chose: Leadership (Under Construction)" TO MSG
                PERFORM ECHO-DISPLAY
            WHEN 9
                MOVE "Returning to main menu..." TO MSG
                PERFORM ECHO-DISPLAY
                EXIT PERFORM
            WHEN OTHER
                MOVE "Invalid choice, please try again." TO MSG
                PERFORM ECHO-DISPLAY
        END-EVALUATE
    END-PERFORM
    EXIT.


