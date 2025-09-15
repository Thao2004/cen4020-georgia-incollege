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
01 USER-IN-REC     PIC X(512).
FD USER-OUT.
01 USER-OUT-REC    PIC X(80).
FD ACCOUNTS.
01 ACC-REC         PIC X(80).      *> One line: "username,password"

WORKING-STORAGE SECTION.
01 MSG             PIC X(80).      *> Reusable message buffer for display/logging
01 CHOICE          PIC 9 VALUE 0.  *> Menu choice for login and create account only
01 NAV-CHOICE      PIC 9 VALUE 0.  *> Navigation choice
01 USERNAME        PIC X(15).      *> Limit username to 15 (storage size)
01 PASSWORD        PIC X(12).      *> Password stored max 12 chars
01 INPUT-USER      PIC X(80).      *> Sratch for username length gating
01 USER-LEN        PIC 99 VALUE 0. *> Length of INPUT-USER
01 SKILLS-SELECTION PIC 99 VALUE 0.     *> skills menu choice (numeric)
01 EOF-FLAG          PIC X VALUE "N".    *> "Y" at end of input

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
01 PROFILE-FIRSTNAME   PIC X(20).      *> Store profile first name
01 PROFILE-LASTNAME    PIC X(20).      *> Store profile last name
01 PROFILE-UNIVERSITY  PIC X(40).      *> Store profile university
01 PROFILE-MAJOR       PIC X(30).      *> Store profile major
01 PROFILE-YEAR        PIC 9(4).       *> Store profile graduation year
01 PROFILE-ABOUT       PIC X(200).     *> Store profile about me section
01 EXP-COUNT           PIC 9 VALUE 0.
01 EXP-TITLE           PIC X(80).
01 EXP-COMPANY         PIC X(80).
01 EXP-DATES           PIC X(80).
01 DESC-LEN            PIC 999.
01 EXP-ID              PIC 9     VALUE 0.
01 EXP-ID-TXT          PIC X     VALUE SPACE.
01 NEXT-TITLE-SEED     PIC X(80) VALUE SPACES.




PROCEDURE DIVISION.
MAIN-PARA.
        *> Open input/output streams
        OPEN INPUT USER-IN
        OPEN OUTPUT USER-OUT

        *> Load existing accounts (if any) into memory
        PERFORM LOAD-ACCOUNTS

        PERFORM MAIN-LOOP

        CLOSE USER-IN
        CLOSE USER-OUT
        STOP RUN.


MAIN-LOOP.
    MOVE "N" TO EOF-FLAG
    PERFORM UNTIL EOF-FLAG = "Y"
        MOVE "Welcome to InCollege!" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "1. Log In" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "2. Create New Account" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "Enter your choice:" TO MSG
        PERFORM ECHO-DISPLAY

        READ USER-IN
            AT END MOVE "Y" TO EOF-FLAG
        END-READ
        IF EOF-FLAG = "Y"
            EXIT PERFORM
        END-IF

        IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
            CONTINUE
        ELSE
            IF FUNCTION TEST-NUMVAL(USER-IN-REC) = 0
                MOVE FUNCTION NUMVAL(USER-IN-REC) TO CHOICE
            ELSE
                MOVE 999 TO CHOICE
            END-IF

            EVALUATE CHOICE
                WHEN 1
                    PERFORM LOGIN-UNLIMITED
                    IF EOF-FLAG = "Y"
                        EXIT PERFORM
                    END-IF
                WHEN 2
                    PERFORM CREATE-ACCOUNT
                    IF EOF-FLAG = "Y"
                        EXIT PERFORM
                    END-IF
                WHEN OTHER
                    MOVE "Invalid choice." TO MSG
                    PERFORM ECHO-DISPLAY
                    CONTINUE
            END-EVALUATE
        END-IF
    END-PERFORM
    EXIT.



CREATE-ACCOUNT.
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
               AT END MOVE "Y" TO EOF-FLAG
           END-READ
           IF EOF-FLAG = "Y"
               EXIT PARAGRAPH
           END-IF

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
                           AT END MOVE "Y" TO EOF-FLAG
                       END-READ
                       IF EOF-FLAG = "Y"
                           EXIT PARAGRAPH
                       END-IF

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
       EXIT.


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
               AT END
                   MOVE "Y" TO EOF-FLAG
                   EXIT PERFORM
           END-READ
           MOVE FUNCTION TRIM(USER-IN-REC) TO USERNAME

           MOVE "Please enter your password:" TO MSG
           PERFORM ECHO-DISPLAY
           READ USER-IN
               AT END
                   MOVE "Y" TO EOF-FLAG
                   EXIT PERFORM
           END-READ
           MOVE FUNCTION TRIM(USER-IN-REC) TO PASSWORD

           PERFORM EXISTS-USERNAME

           IF FOUND-FLAG = "Y"
               IF PASSWORD = T-PASSWORD (U-IX)
                   MOVE "You have successfully logged in" TO MSG
                   PERFORM ECHO-DISPLAY
                   MOVE "Welcome, " TO MSG
                   STRING "Welcome, " FUNCTION TRIM(USERNAME) "!" DELIMITED BY SIZE INTO MSG
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
       MOVE 0 TO NAV-CHOICE

       PERFORM UNTIL EOF-FLAG = "Y"
           PERFORM DISPLAY-MENU

           READ USER-IN INTO USER-IN-REC
               AT END MOVE "Y" TO EOF-FLAG
           END-READ
           IF EOF-FLAG = "Y"
               EXIT PERFORM
           END-IF

           *> Tolerate blank lines by skipping them quietly
           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               CONTINUE
           ELSE
               IF FUNCTION TEST-NUMVAL(USER-IN-REC) = 0
                   MOVE FUNCTION NUMVAL(USER-IN-REC) TO NAV-CHOICE
               ELSE
                   MOVE 999 TO NAV-CHOICE
               END-IF
               PERFORM NAV-MENU-CHOICE
               IF EOF-FLAG = "Y" EXIT PERFORM
           END-IF
       END-PERFORM
       EXIT.


DISPLAY-MENU.
       *> Print a blank line for spacing
       MOVE " " TO MSG
       PERFORM ECHO-DISPLAY

       MOVE "=============================" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "     InCollege Main Menu" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "=============================" TO MSG
       PERFORM ECHO-DISPLAY

       MOVE "  1. Create/Edit My Profile" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "  2. View My Profile" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "  3. Search for a job" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "  4. Find someone you know" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "  5. Learn a New Skill" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "=============================" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "Enter your choice: " TO MSG
       PERFORM ECHO-DISPLAY
       EXIT.


NAV-MENU-CHOICE.
       EVALUATE NAV-CHOICE
           WHEN 1
               PERFORM CREATE-PROFILE
           WHEN 2
               PERFORM VIEW-PROFILE
           WHEN 3
               MOVE "Search for a job is under construction." TO MSG
               PERFORM ECHO-DISPLAY
           WHEN 4
               MOVE "Find someone you know is under construction." TO MSG
               PERFORM ECHO-DISPLAY
           WHEN 5
               PERFORM SKILLS-MENU
           WHEN OTHER
               *> 0, 999, or any other number is invalid
               MOVE "Invalid choice, please try again." TO MSG
               PERFORM ECHO-DISPLAY
       END-EVALUATE
       EXIT.

CREATE-PROFILE.
       MOVE "--- Create/Edit Profile ---" TO MSG
       PERFORM ECHO-DISPLAY

       PERFORM GET-FIRST          *> Get first name
       PERFORM GET-LAST           *> Get last name
       PERFORM GET-UNIV           *> Get university/college
       PERFORM GET-MAJOR          *> Get major
       PERFORM GET-YEAR           *> Get graduation year
       PERFORM GET-ABOUT          *> Get about me (optional)
       PERFORM GET-EXPERIENCE     *> Get experience (optional)
       PERFORM GET-EDUCATION      *> Get education (optional)

       MOVE "Profile saved successfully!" TO MSG
       PERFORM ECHO-DISPLAY
       EXIT.


*> First name
GET-FIRST.
       PERFORM UNTIL 1 = 0
           MOVE "Enter First Name:" TO MSG
           PERFORM ECHO-DISPLAY

           READ USER-IN
               AT END
                   MOVE "Y" TO EOF-FLAG
                   EXIT PARAGRAPH
           END-READ

           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "First Name is required." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO PROFILE-FIRSTNAME
               EXIT PERFORM
           END-IF
       END-PERFORM
       EXIT PARAGRAPH.


*> Last name
GET-LAST.
       PERFORM UNTIL 1 = 0
           MOVE "Enter Last Name:" TO MSG
           PERFORM ECHO-DISPLAY
           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
           END-READ
           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "Last Name is required." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO PROFILE-LASTNAME
               EXIT PERFORM
           END-IF
       END-PERFORM
       EXIT PARAGRAPH.


*> University/College
GET-UNIV.
       PERFORM UNTIL 1 = 0
           MOVE "Enter University/College Attended:" TO MSG
           PERFORM ECHO-DISPLAY
           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
           END-READ
           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "University/College is required." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO PROFILE-UNIVERSITY
               EXIT PERFORM
           END-IF
       END-PERFORM
       EXIT PARAGRAPH.


*> Major
GET-MAJOR.
       PERFORM UNTIL 1 = 0
           MOVE "Enter Major:" TO MSG
           PERFORM ECHO-DISPLAY
           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
           END-READ
           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "Major is required." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO PROFILE-MAJOR
               EXIT PERFORM
           END-IF
       END-PERFORM
       EXIT PARAGRAPH.


*> Graduation year
GET-YEAR.
       PERFORM UNTIL 1 = 0
           MOVE "Enter Graduation Year (YYYY):" TO MSG
           PERFORM ECHO-DISPLAY
           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
           END-READ

           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "Graduation Year is required." TO MSG
               PERFORM ECHO-DISPLAY
               CONTINUE
           END-IF

           IF FUNCTION TEST-NUMVAL(USER-IN-REC) = 0
               MOVE FUNCTION NUMVAL(USER-IN-REC) TO PROFILE-YEAR
               IF PROFILE-YEAR >= 1900 AND PROFILE-YEAR <= 2100
                   EXIT PERFORM
               END-IF
           END-IF

           MOVE "Please enter a valid 4-digit year (e.g., 2025)." TO MSG
           PERFORM ECHO-DISPLAY
       END-PERFORM
       EXIT PARAGRAPH.


*> About me section
GET-ABOUT.
       PERFORM UNTIL 1 = 0
           MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):" TO MSG
           PERFORM ECHO-DISPLAY

           READ USER-IN
               AT END
                   MOVE "Y" TO EOF-FLAG
                   EXIT PARAGRAPH
           END-READ

           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE SPACES TO PROFILE-ABOUT
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) > 200
               MOVE "About Me must be at most 200 characters." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO PROFILE-ABOUT
               EXIT PERFORM
           END-IF
       END-PERFORM
       EXIT PARAGRAPH.


*> Experience section
*> Add up to 3 experiences. After each entry, user may type DONE to stop.
*> Experience section
GET-EXPERIENCE.
       MOVE EXP-COUNT TO EXP-ID
       ADD 1 TO EXP-ID
       MOVE EXP-ID TO EXP-ID-TXT

       *> Show banner once
       MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO MSG
       PERFORM ECHO-DISPLAY


       *> Title (required; re-prompt until non-blank)
       MOVE SPACES TO EXP-TITLE
       PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(EXP-TITLE)) > 0
           MOVE SPACES TO MSG
           STRING "Experience #" DELIMITED BY SIZE
                  EXP-ID-TXT     DELIMITED BY SIZE
                  " - Title:"    DELIMITED BY SIZE
             INTO MSG
           END-STRING
           PERFORM ECHO-DISPLAY

           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PERFORM
           END-READ

           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "Title is required." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO EXP-TITLE
           END-IF
       END-PERFORM

       *> Company/Organization (required; re-prompt until non-blank)
       MOVE SPACES TO EXP-COMPANY
       PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(EXP-COMPANY)) > 0
           MOVE SPACES TO MSG
           STRING "Experience #" DELIMITED BY SIZE
                  EXP-ID-TXT     DELIMITED BY SIZE
                  " - Company/Organization:" DELIMITED BY SIZE
             INTO MSG
           END-STRING
           PERFORM ECHO-DISPLAY

           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
           END-READ

           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "Company/Organization is required." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO EXP-COMPANY
           END-IF
       END-PERFORM

       *> Dates (required; re-prompt until non-blank)
       MOVE SPACES TO EXP-DATES
       PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(EXP-DATES)) > 0
           MOVE SPACES TO MSG
           STRING "Experience #" DELIMITED BY SIZE
                  EXP-ID-TXT     DELIMITED BY SIZE
                  " - Dates (e.g., Summer 2024):" DELIMITED BY SIZE
             INTO MSG
           END-STRING
           PERFORM ECHO-DISPLAY

           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
           END-READ

           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "Dates are required." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO EXP-DATES
           END-IF
       END-PERFORM


       EXIT PARAGRAPH.



*> TO-DO: --------- NEED TO IMPLEMENT ---------
GET-EDUCATION.
      EXIT PARAGRAPH.


*> TO-DO: --------- NEED TO IMPLEMENT ----------
VIEW-PROFILE.
       MOVE "--- Your Profile ---" TO MSG
       PERFORM ECHO-DISPLAY
       EXIT PARAGRAPH.


SKILLS-MENU.
    *> Reset skill selection each time this menu is shown
    MOVE 0 TO SKILLS-SELECTION

    *> Skills loop: repeat until user chooses Go Back (6) or EOF
    PERFORM UNTIL SKILLS-SELECTION = 6 OR EOF-FLAG = "Y"
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
        MOVE "  6) Go Back" TO MSG
        PERFORM ECHO-DISPLAY
        MOVE "Enter your choice:" TO MSG
        PERFORM ECHO-DISPLAY

        *> Read next choice for skills
        READ USER-IN INTO USER-IN-REC
            AT END MOVE "Y" TO EOF-FLAG
        END-READ

        IF EOF-FLAG = "Y"
            EXIT PERFORM
        END-IF

        *> Skip blank lines quietly
        IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
            CONTINUE
        ELSE
            IF FUNCTION TEST-NUMVAL(USER-IN-REC) = 0
               MOVE FUNCTION NUMVAL(USER-IN-REC) TO SKILLS-SELECTION
            ELSE
               MOVE 999 TO SKILLS-SELECTION
            END-IF

           *> Handle skill choice
           EVALUATE SKILLS-SELECTION
               WHEN 1 THRU 5
                   MOVE "This skill is under construction." TO MSG
                   PERFORM ECHO-DISPLAY
               WHEN 6
                   MOVE "Returning to main menu..." TO MSG
                   PERFORM ECHO-DISPLAY
                   EXIT PERFORM
               WHEN OTHER
                   *> 0 or any other invalid number
                   MOVE "Invalid choice, please try again." TO MSG
                   PERFORM ECHO-DISPLAY
           END-EVALUATE
        END-IF
    END-PERFORM
    EXIT.


