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
    *> Persistent storage for user profiles
    SELECT OPTIONAL PROFILES ASSIGN TO 'profiles.dat'
       ORGANIZATION IS LINE SEQUENTIAL.
    *> Persistent storage for connections
    SELECT OPTIONAL CONNECTIONS ASSIGN TO 'connections.dat'
       ORGANIZATION IS LINE SEQUENTIAL.
    *> Persistent storage for friend requests
    SELECT OPTIONAL REQUESTS ASSIGN TO 'requests.dat'
       ORGANIZATION IS LINE SEQUENTIAL.


DATA DIVISION.
FILE SECTION.
FD USER-IN.
01 USER-IN-REC     PIC X(512).
FD USER-OUT.
01 USER-OUT-REC    PIC X(80).
FD ACCOUNTS.
01 ACC-REC         PIC X(80).      *> One line: "username,password"
FD PROFILES.
01 PROFILE-REC     PIC X(800).     *> Profile data storage
FD CONNECTIONS.
01 CONN-REC        PIC X(80).      *> One line: "user1,user2 (established connections only)"
FD REQUESTS.
01 REQ-REC         PIC X(80).      *> One line: "sender|receiver|status"

WORKING-STORAGE SECTION.
01 MSG             PIC X(80).      *> Reusable message buffer for display/logging
01 CHOICE           PIC S9  VALUE 0.  *> Menu choice for login and create account only
01 NAV-CHOICE       PIC S9(3) VALUE 0. *> Navigation choice
01 USERNAME        PIC X(15).      *> Limit username to 15 (storage size)
01 PASSWORD        PIC X(12).      *> Password stored max 12 chars
01 INPUT-USER      PIC X(80).      *> Sratch for username length gating
01 USER-LEN        PIC 99 VALUE 0. *> Length of INPUT-USER
01 SKILLS-SELECTION PIC S99 VALUE 0.     *> skills menu choice (numeric)
01 EOF-FLAG          PIC X VALUE "N".    *> "Y" at end of input

*> Temporary variable for safe year validation
01 TEMP-YEAR       PIC S9(8) VALUE 0.   *> Temporary year holder for validation

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

*> Additional temporary variables for profile parsing
01 TMP-FIELD1     PIC X(50).           *> Temporary field for skipping data
01 TMP-FIELD2     PIC X(50).           *> Temporary field for skipping data
01 TMP-FIELD3     PIC X(50).           *> Temporary field for skipping data
01 TMP-COUNT1     PIC 9.               *> Temporary count for skipping
01 TMP-COUNT2     PIC 9.               *> Temporary count for skipping
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
01 EXP-DESC            PIC X(100).
01 DESC-LEN            PIC 999.
01 EXP-ID              PIC 9     VALUE 0.
01 EXP-ID-TXT          PIC X     VALUE SPACE.

*> Temporary variable for education entry numbering
01 EDU-ID              PIC 9     VALUE 0.
01 EDU-ID-TXT          PIC X     VALUE SPACE.

*> Education entries (up to 3)
01 EDU-COUNT           PIC 9 VALUE 0.
01 EDUCATION-TABLE.
       05 EDU-ENTRY OCCURS 3 INDEXED BY EDU-IX.
           10 EDU-DEGREE      PIC X(50).
           10 EDU-UNIVERSITY  PIC X(50).
           10 EDU-YEARS       PIC X(20).

*> Experience entries (up to 3) - structured storage
01 EXPERIENCE-TABLE.
       05 EXP-ENTRY OCCURS 3 INDEXED BY EXP-IX.
           10 EXP-ENTRY-TITLE       PIC X(80).
           10 EXP-ENTRY-COMPANY     PIC X(80).
           10 EXP-ENTRY-DATES       PIC X(80).
           10 EXP-ENTRY-DESC        PIC X(100).

*> Profile management flags
01 PROFILE-EXISTS      PIC X VALUE "N".
01 CURRENT-USER        PIC X(15).      *> Store logged-in username

*> Multi-user profile storage
01 MAX-STORED-PROFILES PIC 9 VALUE 5.
01 STORED-PROFILE-COUNT PIC 9 VALUE 0.
01 STORED-PROFILES.
    05 STORED-PROFILE OCCURS 5 INDEXED BY SP-IX.
        10 SP-USERNAME     PIC X(15).
        10 SP-FIRSTNAME    PIC X(20).
        10 SP-LASTNAME     PIC X(20).
        10 SP-UNIVERSITY   PIC X(40).
        10 SP-MAJOR        PIC X(30).
        10 SP-YEAR         PIC 9(4).
        10 SP-ABOUT        PIC X(200).
        10 SP-EXP-COUNT    PIC 9.
        10 SP-EDU-COUNT    PIC 9.
        10 SP-EXPERIENCES.
            15 SP-EXP-ENTRY OCCURS 3 INDEXED BY SP-EXP-IX.
                20 SP-EXP-TITLE    PIC X(80).
                20 SP-EXP-COMPANY  PIC X(80).
                20 SP-EXP-DATES    PIC X(80).
                20 SP-EXP-DESC     PIC X(100).
        10 SP-EDUCATIONS.
            15 SP-EDU-ENTRY OCCURS 3 INDEXED BY SP-EDU-IX.
                20 SP-EDU-DEGREE   PIC X(50).
                20 SP-EDU-UNIV     PIC X(50).
                20 SP-EDU-YEARS    PIC X(20).

*> Display variables for numbered entries
01 DISPLAY-EXP-NUM     PIC 9.               *> For displaying experience number
01 DISPLAY-EDU-NUM     PIC 9.               *> For displaying education number


*> Variables for displaying long text in chunks
01 LONG-TEXT-POS       PIC 999 VALUE 1.     *> Position in long text
01 LONG-TEXT-LEN       PIC 999 VALUE 0.     *> Length of long text
01 CHUNK-SIZE          PIC 99 VALUE 80.     *> Size of each display chunk
01 REMAINING-LEN       PIC 999 VALUE 0.     *> Remaining characters to display
01 CHUNK-LEN           PIC 99 VALUE 0.      *> Length of current chunk
01 INPUT-LEN           PIC 999 VALUE 0.     *> Length of user input for validation

*> Search inputs (Stories 3–6,7)
01 SRCH-FIRSTNAME      PIC X(20).
01 SRCH-LASTNAME       PIC X(20).

*> ===== Established connections.dat (Epic #4) =====
01 MAX-CONNECTIONS         PIC 99  VALUE 50.
01 CONN-COUNT              PIC 99  VALUE 0.
01 CONN-TABLE.
   05 CONN-ENTRY OCCURS 50 INDEXED BY C-IX.
      10 C-USER1         PIC X(15).
      10 C-USER2         PIC X(15).

*> ===== Connection Requests (requests.dat) ====
01 MAX-REQUESTS        PIC 99 VALUE 50.
01 REQUESTS-COUNT      PIC 99 VALUE 0.
01 REQUESTS-TABLE.
   05 REQUESTS-ENTRY OCCURS 50 INDEXED BY R-IX.
   10 R-SENDER         PIC X(15).
   10 R-RECEIVER       PIC X(15).
   10 R-STATUS         PIC X(9). *> PENDING|ACCEPTED|REJECTED (max 8 chars + null)


*> Scratch for parsing connections
01 CT-TMP1                 PIC X(15).
01 CT-TMP2                 PIC X(15).
01 RT-TMP1                 PIC X(15).
01 RT-TMP2                 PIC X(15).
01 RT-TMP3                 PIC X(9).

*> Connections menu helpers
01 CONN-MENU-CHOICE        PIC S9 VALUE 0.
01 PENDING-COUNT           PIC 99 VALUE 0.
01 PENDING-INDEXES.
   05 PENDING-IDX OCCURS 50 INDEXED BY P-IX.
      10 P-ROW            PIC 99.
01 RESP-USER               PIC X(15).
01 RESP-ACT                PIC X(7).     *> ACCEPT / REJECT
01 OTHER-USER              PIC X(15).

*> --- Send request helpers ---
01 CHECK-USER            PIC X(15).   *> scratch for existence check
01 TARGET-USER           PIC X(15).   *> receiver username when sending
01 ANY-FOUND             PIC X VALUE "N".

01 USER-A               PIC X(15).
01 USER-B               PIC X(15).

01 FOUND-INDEX          PIC 99 VALUE 0.


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

                   MOVE SPACES TO MSG
                   STRING "Welcome, " FUNCTION TRIM(USERNAME) "!"
                     DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   *> Store current user for profile operations
                   MOVE USERNAME TO CURRENT-USER
                   *> Load existing profile when user logs in
                   PERFORM LOAD-PROFILE
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
       MOVE "  6. View My Pending Connection Requests" TO MSG
       PERFORM ECHO-DISPLAY
       MOVE "  7. View My Network" TO MSG
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
               PERFORM FIND-SOMEONE-YOU-KNOW
           WHEN 5
               PERFORM SKILLS-MENU
           WHEN 6
               PERFORM VIEW-AND-RESPOND-PENDING
           WHEN 7
               PERFORM VIEW-MY-CONNECTIONS
           WHEN OTHER
               *> 0, 999, or any other number is invalid
               MOVE "Invalid choice, please try again." TO MSG
               PERFORM ECHO-DISPLAY
       END-EVALUATE
       EXIT.

CREATE-PROFILE.
       MOVE "--- Create/Edit Profile ---" TO MSG
       PERFORM ECHO-DISPLAY

       *> Initialize counters for new profile creation
       MOVE 0 TO EXP-COUNT EDU-COUNT
       *> Load existing profile if it exists (for editing)
       PERFORM LOAD-PROFILE

       PERFORM GET-FIRST          *> Get first name
       PERFORM GET-LAST           *> Get last name
       PERFORM GET-UNIV           *> Get university/college
       PERFORM GET-MAJOR          *> Get major
       PERFORM GET-YEAR           *> Get graduation year
       PERFORM GET-ABOUT          *> Get about me (optional)
       PERFORM GET-EXPERIENCE     *> Get experience (optional)
       PERFORM GET-EDUCATION      *> Get education (optional)

       *> Save the profile
       PERFORM SAVE-PROFILE

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

           *> Check for decimal point first - reject if found
           IF FUNCTION SUBSTITUTE(USER-IN-REC, ".", "") NOT = USER-IN-REC
               *> Input contains decimal point - reject it
               CONTINUE
           ELSE
               IF FUNCTION TEST-NUMVAL(USER-IN-REC) = 0
                   *> Move to temporary variable first to avoid runtime errors
                   MOVE FUNCTION NUMVAL(USER-IN-REC) TO TEMP-YEAR
                   *> Validate range and ensure value is within PIC 9(4) limits
                   IF TEMP-YEAR >= 1900 AND TEMP-YEAR <= 2100
                      AND TEMP-YEAR >= 0 AND TEMP-YEAR <= 9999
                       MOVE TEMP-YEAR TO PROFILE-YEAR
                       EXIT PERFORM
                   END-IF
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

           *> Enhanced validation with better error handling
           MOVE FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) TO INPUT-LEN
           IF INPUT-LEN > 200
               MOVE "About Me must be at most 200 characters." TO MSG
               PERFORM ECHO-DISPLAY
               *> Continue the loop to re-prompt for input
           ELSE
               *> Input is valid, save it
               MOVE FUNCTION TRIM(USER-IN-REC) TO PROFILE-ABOUT
               EXIT PARAGRAPH
           END-IF
       END-PERFORM
       EXIT PARAGRAPH.


*> Experience section
*> Add up to 3 experiences. After each entry, user may type DONE to stop.
GET-EXPERIENCE.
       *> Show banner once
       MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO MSG
       PERFORM ECHO-DISPLAY

       *> Loop until user types DONE or reaches maximum
       PERFORM UNTIL EOF-FLAG = "Y"
           *> Check if already at maximum entries FIRST
           IF EXP-COUNT >= 3
               MOVE "Maximum of 3 experience entries allowed. Enter 'DONE' to continue." TO MSG
               PERFORM ECHO-DISPLAY

               *> Keep reading until user types DONE
               PERFORM UNTIL EOF-FLAG = "Y"
                   READ USER-IN
                       AT END MOVE "Y" TO EOF-FLAG EXIT PERFORM
                   END-READ

                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
                       EXIT PERFORM
                   END-IF

                   *> If not DONE, show error again
                   MOVE "Maximum of 3 experience entries allowed. Enter 'DONE' to continue." TO MSG
                   PERFORM ECHO-DISPLAY
               END-PERFORM
               EXIT PERFORM
           END-IF

           *> If not at maximum, proceed with adding new experience
           *> Set up entry number for display
           MOVE EXP-COUNT TO EXP-ID
           ADD 1 TO EXP-ID
           MOVE EXP-ID TO EXP-ID-TXT

           *> Title (required; re-prompt until non-blank or DONE)
           MOVE SPACES TO EXP-TITLE
           *> Check for DONE before showing first detailed prompt
           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PERFORM
           END-READ

           *> Check for DONE immediately - if DONE, exit without showing prompt
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
               EXIT PERFORM
           END-IF

           *> If not DONE, validate the input as title
           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "Title is required." TO MSG
               PERFORM ECHO-DISPLAY
               *> Now enter the re-prompt loop for empty title
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

                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
                       MOVE "This entry isn't recorded because it doesn't have enough required information." TO MSG
                       PERFORM ECHO-DISPLAY
                       EXIT PERFORM
                   END-IF

                   IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
                       MOVE "Title is required." TO MSG
                       PERFORM ECHO-DISPLAY
                   ELSE
                       MOVE FUNCTION TRIM(USER-IN-REC) TO EXP-TITLE
                   END-IF
               END-PERFORM
           ELSE
               *> Valid title entered on first try
               MOVE FUNCTION TRIM(USER-IN-REC) TO EXP-TITLE
           END-IF

           *> If user typed DONE or hit EOF, exit
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE" OR EOF-FLAG = "Y"
               EXIT PERFORM
           END-IF

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

               *> Check for DONE at Company prompt
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
                   MOVE "This entry isn't recorded because it doesn't have enough required information." TO MSG
                   PERFORM ECHO-DISPLAY
                   EXIT PERFORM
               END-IF

               IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
                   MOVE "Company/Organization is required." TO MSG
                   PERFORM ECHO-DISPLAY
               ELSE
                   MOVE FUNCTION TRIM(USER-IN-REC) TO EXP-COMPANY
               END-IF
           END-PERFORM

           *> If user typed DONE or hit EOF, exit
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE" OR EOF-FLAG = "Y"
               EXIT PERFORM
           END-IF

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

               *> Check for DONE at Dates prompt
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
                   MOVE "This entry isn't recorded because it doesn't have enough required information." TO MSG
                   PERFORM ECHO-DISPLAY
                   EXIT PERFORM
               END-IF

               IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
                   MOVE "Dates are required." TO MSG
                   PERFORM ECHO-DISPLAY
               ELSE
                   MOVE FUNCTION TRIM(USER-IN-REC) TO EXP-DATES
               END-IF
           END-PERFORM

           *> If user typed DONE or hit EOF, exit
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE" OR EOF-FLAG = "Y"
               EXIT PERFORM
           END-IF

           *> Description (optional; blank skips)
           MOVE SPACES TO MSG
           STRING "Experience #" DELIMITED BY SIZE
                  EXP-ID-TXT     DELIMITED BY SIZE
                  " - Description (optional, max 100 chars, blank to skip):"
                  DELIMITED BY SIZE
             INTO MSG
           END-STRING
           PERFORM ECHO-DISPLAY

           MOVE SPACES TO EXP-DESC
           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PERFORM
           END-READ

           *> Blank => skip description
           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE SPACES TO EXP-DESC
           ELSE
               MOVE FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) TO DESC-LEN
               IF DESC-LEN > 100
                   MOVE "Description must be at most 100 characters." TO MSG
                   PERFORM ECHO-DISPLAY
                   MOVE SPACES TO EXP-DESC
               ELSE
                   MOVE FUNCTION TRIM(USER-IN-REC) TO EXP-DESC
               END-IF
           END-IF

           *> Store experience in structured table
           ADD 1 TO EXP-COUNT
           SET EXP-IX TO EXP-COUNT
           MOVE EXP-TITLE TO EXP-ENTRY-TITLE (EXP-IX)
           MOVE EXP-COMPANY TO EXP-ENTRY-COMPANY (EXP-IX)
           MOVE EXP-DATES TO EXP-ENTRY-DATES (EXP-IX)
           MOVE EXP-DESC TO EXP-ENTRY-DESC (EXP-IX)

           *> Ask for next experience entry ONLY if under the limit
           IF EXP-COUNT < 3
               MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO MSG
               PERFORM ECHO-DISPLAY
           END-IF
       END-PERFORM

       EXIT PARAGRAPH.


*> Education section
GET-EDUCATION.
       *> Initialize education count if this is first time
       MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO MSG
       PERFORM ECHO-DISPLAY

       *> Loop until user types DONE or reaches maximum
       PERFORM UNTIL EOF-FLAG = "Y"
           *> Check if already at maximum entries FIRST
           IF EDU-COUNT >= 3
               MOVE "Maximum of 3 education entries allowed. Enter 'DONE' to continue." TO MSG
               PERFORM ECHO-DISPLAY

               *> Keep reading until user types DONE
               PERFORM UNTIL EOF-FLAG = "Y"
                   READ USER-IN
                       AT END MOVE "Y" TO EOF-FLAG EXIT PERFORM
                   END-READ

                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
                       EXIT PERFORM
                   END-IF

                   *> If not DONE, show error again
                   MOVE "Maximum of 3 education entries allowed. Enter 'DONE' to continue." TO MSG
                   PERFORM ECHO-DISPLAY
               END-PERFORM
               EXIT PERFORM
           END-IF

           *> If not at maximum, proceed with adding new education
           *> Set up entry number for display
           COMPUTE EDU-ID = EDU-COUNT + 1
           MOVE EDU-ID TO EDU-ID-TXT

           *> Get Degree (required for education entry)
           *> Check for DONE before showing first detailed prompt
           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
           END-READ

           *> Check for DONE immediately - if DONE, exit without showing prompt
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
               EXIT PERFORM
           END-IF

           *> If not DONE, validate the input as degree
           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "Degree is required." TO MSG
               PERFORM ECHO-DISPLAY
               *> Now enter the re-prompt loop for empty degree
               PERFORM UNTIL 1 = 0
                   MOVE SPACES TO MSG
                   STRING "Education #" DELIMITED BY SIZE
                          EDU-ID-TXT DELIMITED BY SIZE
                          " - Degree:" DELIMITED BY SIZE
                     INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   READ USER-IN
                       AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
                   END-READ

                   *> Check for DONE at Degree prompt
                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
                       EXIT PERFORM
                   END-IF

                   IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
                       MOVE "Degree is required." TO MSG
                       PERFORM ECHO-DISPLAY
                   ELSE
                       *> Add new education entry - now increment count and set index
                       ADD 1 TO EDU-COUNT
                       SET EDU-IX TO EDU-COUNT
                       MOVE FUNCTION TRIM(USER-IN-REC) TO EDU-DEGREE (EDU-IX)
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           ELSE
               *> Valid degree entered on first try
               ADD 1 TO EDU-COUNT
               SET EDU-IX TO EDU-COUNT
               MOVE FUNCTION TRIM(USER-IN-REC) TO EDU-DEGREE (EDU-IX)
           END-IF

           *> If user typed DONE, exit
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
               EXIT PERFORM
           END-IF

           *> Get University/College (required for education entry)
           PERFORM UNTIL 1 = 0
               MOVE SPACES TO MSG
               STRING "Education #" DELIMITED BY SIZE
                      EDU-ID-TXT DELIMITED BY SIZE
                      " - University/College:" DELIMITED BY SIZE
                 INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               READ USER-IN
                   AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
               END-READ

               *> Check for DONE at University prompt
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
                   MOVE "This entry isn't recorded because it doesn't have enough required information." TO MSG
                   PERFORM ECHO-DISPLAY
                   *> Remove the education entry that was partially added
                   SUBTRACT 1 FROM EDU-COUNT
                   EXIT PERFORM
               END-IF

               IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
                   MOVE "University/College is required." TO MSG
                   PERFORM ECHO-DISPLAY
               ELSE
                   MOVE FUNCTION TRIM(USER-IN-REC) TO EDU-UNIVERSITY (EDU-IX)
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> If user typed DONE, exit
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
               EXIT PERFORM
           END-IF

           *> Get Years Attended (required for education entry)
           PERFORM UNTIL 1 = 0
               MOVE SPACES TO MSG
               STRING "Education #" DELIMITED BY SIZE
                      EDU-ID-TXT DELIMITED BY SIZE
                      " - Years Attended (e.g., 2023-2025):" DELIMITED BY SIZE
                 INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               READ USER-IN
                   AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
               END-READ

               *> Check for DONE at Years prompt
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
                   MOVE "This entry isn't recorded because it doesn't have enough required information." TO MSG
                   PERFORM ECHO-DISPLAY
                   *> Remove the education entry that was partially added
                   SUBTRACT 1 FROM EDU-COUNT
                   EXIT PERFORM
               END-IF

               IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
                   MOVE "Years Attended is required." TO MSG
                   PERFORM ECHO-DISPLAY
               ELSE
                   MOVE FUNCTION TRIM(USER-IN-REC) TO EDU-YEARS (EDU-IX)
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> If user typed DONE, exit
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) = "DONE"
               EXIT PERFORM
           END-IF

           *> Ask for next education entry ONLY if under the limit
           IF EDU-COUNT < 3
               MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO MSG
               PERFORM ECHO-DISPLAY
           END-IF
       END-PERFORM

       EXIT PARAGRAPH.


*> VIEW PROFILE (current user)
VIEW-PROFILE.
       *> Load profile data first
       PERFORM LOAD-PROFILE

       IF PROFILE-EXISTS = "N"
           MOVE "No profile found. Please create a profile first." TO MSG
           PERFORM ECHO-DISPLAY
           EXIT PARAGRAPH
       END-IF

       MOVE "--- Your Profile ---" TO MSG
       PERFORM ECHO-DISPLAY

       *> Display basic information
       MOVE SPACES TO MSG
       STRING "Name: " FUNCTION TRIM(PROFILE-FIRSTNAME) " "
              FUNCTION TRIM(PROFILE-LASTNAME) DELIMITED BY SIZE INTO MSG
       END-STRING
       PERFORM ECHO-DISPLAY

       MOVE SPACES TO MSG
       STRING "University: " FUNCTION TRIM(PROFILE-UNIVERSITY) DELIMITED BY SIZE INTO MSG
       END-STRING
       PERFORM ECHO-DISPLAY

       MOVE SPACES TO MSG
       STRING "Major: " FUNCTION TRIM(PROFILE-MAJOR) DELIMITED BY SIZE INTO MSG
       END-STRING
       PERFORM ECHO-DISPLAY

       MOVE SPACES TO MSG
       STRING "Graduation Year: " PROFILE-YEAR DELIMITED BY SIZE INTO MSG
       END-STRING
       PERFORM ECHO-DISPLAY

       *> Display About Me if exists - handle long text properly
       IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-ABOUT)) > 0
           MOVE "About Me:" TO MSG
           PERFORM ECHO-DISPLAY
           PERFORM DISPLAY-LONG-TEXT
       END-IF

       *> Display Experience if exists
       IF EXP-COUNT > 0
           IF EXP-COUNT > 1
               *> Multiple experiences - show numbered entries
               SET EXP-IX TO 1
               PERFORM UNTIL EXP-IX > EXP-COUNT
                   MOVE EXP-IX TO DISPLAY-EXP-NUM
                   MOVE SPACES TO MSG
                   STRING "Experience #" DISPLAY-EXP-NUM ":" DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Title: " FUNCTION TRIM(EXP-ENTRY-TITLE (EXP-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Company: " FUNCTION TRIM(EXP-ENTRY-COMPANY (EXP-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Dates: " FUNCTION TRIM(EXP-ENTRY-DATES (EXP-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   IF FUNCTION LENGTH(FUNCTION TRIM(EXP-ENTRY-DESC (EXP-IX))) > 0
                       MOVE SPACES TO MSG
                       STRING " Description: " FUNCTION TRIM(EXP-ENTRY-DESC (EXP-IX))
                              DELIMITED BY SIZE INTO MSG
                       END-STRING
                       PERFORM ECHO-DISPLAY
                   END-IF

                   SET EXP-IX UP BY 1
               END-PERFORM
           ELSE
               *> Single experience - show without number
               MOVE "Experience:" TO MSG
               PERFORM ECHO-DISPLAY
               SET EXP-IX TO 1
               MOVE SPACES TO MSG
               STRING " Title: " FUNCTION TRIM(EXP-ENTRY-TITLE (EXP-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               MOVE SPACES TO MSG
               STRING " Company: " FUNCTION TRIM(EXP-ENTRY-COMPANY (EXP-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               MOVE SPACES TO MSG
               STRING " Dates: " FUNCTION TRIM(EXP-ENTRY-DATES (EXP-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               IF FUNCTION LENGTH(FUNCTION TRIM(EXP-ENTRY-DESC (EXP-IX))) > 0
                   MOVE SPACES TO MSG
                   STRING " Description: " FUNCTION TRIM(EXP-ENTRY-DESC (EXP-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY
               END-IF
           END-IF
       END-IF

       *> Display Education if exists
       IF EDU-COUNT > 0
           IF EDU-COUNT > 1
               *> Multiple education entries - show numbered entries
               SET EDU-IX TO 1
               PERFORM UNTIL EDU-IX > EDU-COUNT
                   MOVE EDU-IX TO DISPLAY-EDU-NUM
                   MOVE SPACES TO MSG
                   STRING "Education #" DISPLAY-EDU-NUM ":" DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Degree: " FUNCTION TRIM(EDU-DEGREE (EDU-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " University: " FUNCTION TRIM(EDU-UNIVERSITY (EDU-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Years: " FUNCTION TRIM(EDU-YEARS (EDU-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   SET EDU-IX UP BY 1
               END-PERFORM
           ELSE
               *> Single education entry - show without number
               MOVE "Education:" TO MSG
               PERFORM ECHO-DISPLAY
               SET EDU-IX TO 1
               MOVE SPACES TO MSG
               STRING " Degree: " FUNCTION TRIM(EDU-DEGREE (EDU-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               MOVE SPACES TO MSG
               STRING " University: " FUNCTION TRIM(EDU-UNIVERSITY (EDU-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               MOVE SPACES TO MSG
               STRING " Years: " FUNCTION TRIM(EDU-YEARS (EDU-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY
           END-IF
       END-IF

       MOVE "--------------------" TO MSG
       PERFORM ECHO-DISPLAY

       EXIT PARAGRAPH.

*> Helper routine to display long text with word wrapping
DISPLAY-LONG-TEXT.
       MOVE FUNCTION TRIM(PROFILE-ABOUT) TO PROFILE-ABOUT
       MOVE FUNCTION LENGTH(FUNCTION TRIM(PROFILE-ABOUT)) TO LONG-TEXT-LEN
       MOVE 1 TO LONG-TEXT-POS

       PERFORM UNTIL LONG-TEXT-POS > LONG-TEXT-LEN
           COMPUTE REMAINING-LEN = LONG-TEXT-LEN - LONG-TEXT-POS + 1

           IF REMAINING-LEN > CHUNK-SIZE
               MOVE CHUNK-SIZE TO CHUNK-LEN
               *> Check if we're breaking in the middle of a word
               PERFORM ADJUST-FOR-WORD-WRAP
           ELSE
               MOVE REMAINING-LEN TO CHUNK-LEN
           END-IF

           MOVE SPACES TO MSG
           MOVE PROFILE-ABOUT (LONG-TEXT-POS:CHUNK-LEN) TO MSG
           PERFORM ECHO-DISPLAY

           ADD CHUNK-LEN TO LONG-TEXT-POS
       END-PERFORM
       EXIT.

*> Adjust chunk length to avoid breaking words
ADJUST-FOR-WORD-WRAP.
       *> If the character at the break point is a space, no adjustment needed
       IF PROFILE-ABOUT (LONG-TEXT-POS + CHUNK-LEN - 1:1) = SPACE
           EXIT PARAGRAPH
       END-IF

       *> If the next character is a space, no adjustment needed
       IF LONG-TEXT-POS + CHUNK-LEN <= LONG-TEXT-LEN
           IF PROFILE-ABOUT (LONG-TEXT-POS + CHUNK-LEN:1) = SPACE
               EXIT PARAGRAPH
           END-IF
       END-IF

       *> Find the last space within the chunk to break at
       PERFORM VARYING I FROM CHUNK-LEN BY -1 UNTIL I < 1
           IF PROFILE-ABOUT (LONG-TEXT-POS + I - 1:1) = SPACE
               MOVE I TO CHUNK-LEN
               EXIT PERFORM
           END-IF
       END-PERFORM

       *> If no space found within chunk, keep original chunk size
       *> (this handles cases where a single word is longer than 80 chars)
       IF I < 1
           MOVE CHUNK-SIZE TO CHUNK-LEN
       END-IF
       EXIT.

*> Profile persistence functions
LOAD-PROFILE.
       MOVE "N" TO PROFILE-EXISTS
       *> Initialize counters
       MOVE 0 TO EXP-COUNT EDU-COUNT

       OPEN INPUT PROFILES
       PERFORM UNTIL 1 = 0
           READ PROFILES
               AT END EXIT PERFORM
           END-READ

           *> Check if this profile belongs to current user
           IF PROFILE-REC (1:15) = CURRENT-USER
               MOVE "Y" TO PROFILE-EXISTS
               *> Parse basic profile data from the record
               UNSTRING PROFILE-REC DELIMITED BY "|"
                   INTO TMP-USER PROFILE-FIRSTNAME PROFILE-LASTNAME
                        PROFILE-UNIVERSITY PROFILE-MAJOR PROFILE-YEAR
                        PROFILE-ABOUT EXP-COUNT EDU-COUNT
               END-UNSTRING

               *> Load experience entries if any exist
               IF EXP-COUNT > 0
                   SET EXP-IX TO 1
                   PERFORM EXP-COUNT TIMES
                       READ PROFILES
                           AT END EXIT PERFORM
                       END-READ
                       UNSTRING PROFILE-REC DELIMITED BY "|"
                           INTO EXP-ENTRY-TITLE (EXP-IX)
                                EXP-ENTRY-COMPANY (EXP-IX)
                                EXP-ENTRY-DATES (EXP-IX)
                                EXP-ENTRY-DESC (EXP-IX)
                       END-UNSTRING
                       SET EXP-IX UP BY 1
                   END-PERFORM
               END-IF

               *> Load education entries if any exist
               IF EDU-COUNT > 0
                   SET EDU-IX TO 1
                   PERFORM EDU-COUNT TIMES
                       READ PROFILES
                           AT END EXIT PERFORM
                       END-READ
                       UNSTRING PROFILE-REC DELIMITED BY "|"
                           INTO EDU-DEGREE (EDU-IX)
                                EDU-UNIVERSITY (EDU-IX)
                                EDU-YEARS (EDU-IX)
                       END-UNSTRING
                       SET EDU-IX UP BY 1
                   END-PERFORM
               END-IF

               EXIT PERFORM
           ELSE
               *> This profile belongs to another user, skip their data
               *> Read their exp_count and edu_count to know how many lines to skip
               UNSTRING PROFILE-REC DELIMITED BY "|"
                   INTO TMP-USER TMP-FIELD1 TMP-FIELD2 TMP-FIELD3
                        TMP-FIELD1 TMP-FIELD2 TMP-FIELD3
                        TMP-COUNT1 TMP-COUNT2
               END-UNSTRING

               *> Skip experience lines
               IF TMP-COUNT1 > 0
                   PERFORM TMP-COUNT1 TIMES
                       READ PROFILES
                           AT END EXIT PERFORM
                       END-READ
                   END-PERFORM
               END-IF

               *> Skip education lines
               IF TMP-COUNT2 > 0
                   PERFORM TMP-COUNT2 TIMES
                       READ PROFILES
                           AT END EXIT PERFORM
                       END-READ
                   END-PERFORM
               END-IF
           END-IF
       END-PERFORM
       CLOSE PROFILES
       EXIT.


*> Preserve other users' profiles
SAVE-PROFILE.
       *> Load all existing profiles into memory
       PERFORM LOAD-ALL-PROFILES

       *> Update current user's profile in memory
       PERFORM UPDATE-CURRENT-PROFILE

       *> Save all profiles back to file
       PERFORM SAVE-ALL-PROFILES
       EXIT.


*> Load all profiles from file into STORED-PROFILES table
LOAD-ALL-PROFILES.
       MOVE 0 TO STORED-PROFILE-COUNT

       OPEN INPUT PROFILES
       PERFORM UNTIL 1 = 0
           READ PROFILES
               AT END EXIT PERFORM
           END-READ

           *> Don't exceed our storage limit
           IF STORED-PROFILE-COUNT >= MAX-STORED-PROFILES
               *> Skip remaining profiles if we're at limit
               PERFORM SKIP-PROFILE-DATA
           ELSE
               *> Add this profile to our stored profiles
               ADD 1 TO STORED-PROFILE-COUNT
               SET SP-IX TO STORED-PROFILE-COUNT

               *> Parse basic profile data
               UNSTRING PROFILE-REC DELIMITED BY "|"
                   INTO SP-USERNAME (SP-IX) SP-FIRSTNAME (SP-IX)
                        SP-LASTNAME (SP-IX) SP-UNIVERSITY (SP-IX)
                        SP-MAJOR (SP-IX) SP-YEAR (SP-IX)
                        SP-ABOUT (SP-IX) SP-EXP-COUNT (SP-IX)
                        SP-EDU-COUNT (SP-IX)
               END-UNSTRING

               *> Load experience entries
               IF SP-EXP-COUNT (SP-IX) > 0
                   SET SP-EXP-IX TO 1
                   PERFORM SP-EXP-COUNT (SP-IX) TIMES
                       READ PROFILES
                           AT END EXIT PERFORM
                       END-READ
                       UNSTRING PROFILE-REC DELIMITED BY "|"
                           INTO SP-EXP-TITLE (SP-IX, SP-EXP-IX)
                                SP-EXP-COMPANY (SP-IX, SP-EXP-IX)
                                SP-EXP-DATES (SP-IX, SP-EXP-IX)
                                SP-EXP-DESC (SP-IX, SP-EXP-IX)
                       END-UNSTRING
                       SET SP-EXP-IX UP BY 1
                   END-PERFORM
               END-IF

               *> Load education entries
               IF SP-EDU-COUNT (SP-IX) > 0
                   SET SP-EDU-IX TO 1
                   PERFORM SP-EDU-COUNT (SP-IX) TIMES
                       READ PROFILES
                           AT END EXIT PERFORM
                       END-READ
                       UNSTRING PROFILE-REC DELIMITED BY "|"
                           INTO SP-EDU-DEGREE (SP-IX, SP-EDU-IX)
                                SP-EDU-UNIV (SP-IX, SP-EDU-IX)
                                SP-EDU-YEARS (SP-IX, SP-EDU-IX)
                       END-UNSTRING
                       SET SP-EDU-IX UP BY 1
                   END-PERFORM
               END-IF
           END-IF
       END-PERFORM
       CLOSE PROFILES
       EXIT.


*> Skip profile data when we're at storage limit
SKIP-PROFILE-DATA.
       *> Parse the counts to know how many lines to skip
       UNSTRING PROFILE-REC DELIMITED BY "|"
           INTO TMP-USER TMP-FIELD1 TMP-FIELD2 TMP-FIELD3
                TMP-FIELD1 TMP-FIELD2 TMP-FIELD3
                TMP-COUNT1 TMP-COUNT2
       END-UNSTRING

       *> Skip experience lines
       IF TMP-COUNT1 > 0
           PERFORM TMP-COUNT1 TIMES
               READ PROFILES
                   AT END EXIT PERFORM
               END-READ
           END-PERFORM
       END-IF

       *> Skip education lines
       IF TMP-COUNT2 > 0
           PERFORM TMP-COUNT2 TIMES
               READ PROFILES
                   AT END EXIT PERFORM
               END-READ
           END-PERFORM
       END-IF
       EXIT.


*> Update or add current user's profile in STORED-PROFILES
UPDATE-CURRENT-PROFILE.
       *> Look for existing profile for current user
       MOVE "N" TO FOUND-FLAG
       IF STORED-PROFILE-COUNT > 0
           SET SP-IX TO 1
           PERFORM UNTIL SP-IX > STORED-PROFILE-COUNT
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(SP-USERNAME (SP-IX)))
                    = FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
                   MOVE "Y" TO FOUND-FLAG
                   EXIT PERFORM
               ELSE
                   SET SP-IX UP BY 1
               END-IF
           END-PERFORM
       END-IF

       *> If not found and we have space, add new profile
       IF FOUND-FLAG = "N"
           IF STORED-PROFILE-COUNT < MAX-STORED-PROFILES
               ADD 1 TO STORED-PROFILE-COUNT
               SET SP-IX TO STORED-PROFILE-COUNT
           ELSE
               *> No space to add new profile, exit
               EXIT PARAGRAPH
           END-IF
       END-IF

       *> Update the profile data (whether existing or new)
       MOVE CURRENT-USER TO SP-USERNAME (SP-IX)
       MOVE PROFILE-FIRSTNAME TO SP-FIRSTNAME (SP-IX)
       MOVE PROFILE-LASTNAME TO SP-LASTNAME (SP-IX)
       MOVE PROFILE-UNIVERSITY TO SP-UNIVERSITY (SP-IX)
       MOVE PROFILE-MAJOR TO SP-MAJOR (SP-IX)
       MOVE PROFILE-YEAR TO SP-YEAR (SP-IX)
       MOVE PROFILE-ABOUT TO SP-ABOUT (SP-IX)
       MOVE EXP-COUNT TO SP-EXP-COUNT (SP-IX)
       MOVE EDU-COUNT TO SP-EDU-COUNT (SP-IX)

       *> Copy experience entries
       IF EXP-COUNT > 0
           SET EXP-IX TO 1
           SET SP-EXP-IX TO 1
           PERFORM EXP-COUNT TIMES
               MOVE EXP-ENTRY-TITLE (EXP-IX) TO SP-EXP-TITLE (SP-IX, SP-EXP-IX)
               MOVE EXP-ENTRY-COMPANY (EXP-IX) TO SP-EXP-COMPANY (SP-IX, SP-EXP-IX)
               MOVE EXP-ENTRY-DATES (EXP-IX) TO SP-EXP-DATES (SP-IX, SP-EXP-IX)
               MOVE EXP-ENTRY-DESC (EXP-IX) TO SP-EXP-DESC (SP-IX, SP-EXP-IX)
               SET EXP-IX UP BY 1
               SET SP-EXP-IX UP BY 1
           END-PERFORM
       END-IF

       *> Copy education entries
       IF EDU-COUNT > 0
           SET EDU-IX TO 1
           SET SP-EDU-IX TO 1
           PERFORM EDU-COUNT TIMES
               MOVE EDU-DEGREE (EDU-IX) TO SP-EDU-DEGREE (SP-IX, SP-EDU-IX)
               MOVE EDU-UNIVERSITY (EDU-IX) TO SP-EDU-UNIV (SP-IX, SP-EDU-IX)
               MOVE EDU-YEARS (EDU-IX) TO SP-EDU-YEARS (SP-IX, SP-EDU-IX)
               SET EDU-IX UP BY 1
               SET SP-EDU-IX UP BY 1
           END-PERFORM
       END-IF
       EXIT.


*> Save all profiles from STORED-PROFILES back to file
SAVE-ALL-PROFILES.
       OPEN OUTPUT PROFILES

       *> Write each stored profile
       IF STORED-PROFILE-COUNT > 0
           SET SP-IX TO 1
           PERFORM UNTIL SP-IX > STORED-PROFILE-COUNT
               *> Write basic profile information
               MOVE SPACES TO PROFILE-REC
               STRING SP-USERNAME (SP-IX) "|"
                      FUNCTION TRIM(SP-FIRSTNAME (SP-IX)) "|"
                      FUNCTION TRIM(SP-LASTNAME (SP-IX)) "|"
                      FUNCTION TRIM(SP-UNIVERSITY (SP-IX)) "|"
                      FUNCTION TRIM(SP-MAJOR (SP-IX)) "|"
                      SP-YEAR (SP-IX) "|"
                      FUNCTION TRIM(SP-ABOUT (SP-IX)) "|"
                      SP-EXP-COUNT (SP-IX) "|"
                      SP-EDU-COUNT (SP-IX)
                      DELIMITED BY SIZE INTO PROFILE-REC
               END-STRING
               WRITE PROFILE-REC

               *> Write experience entries
               IF SP-EXP-COUNT (SP-IX) > 0
                   SET SP-EXP-IX TO 1
                   PERFORM SP-EXP-COUNT (SP-IX) TIMES
                       MOVE SPACES TO PROFILE-REC
                       STRING FUNCTION TRIM(SP-EXP-TITLE (SP-IX, SP-EXP-IX)) "|"
                              FUNCTION TRIM(SP-EXP-COMPANY (SP-IX, SP-EXP-IX)) "|"
                              FUNCTION TRIM(SP-EXP-DATES (SP-IX, SP-EXP-IX)) "|"
                              FUNCTION TRIM(SP-EXP-DESC (SP-IX, SP-EXP-IX))
                              DELIMITED BY SIZE INTO PROFILE-REC
                       END-STRING
                       WRITE PROFILE-REC
                       SET SP-EXP-IX UP BY 1
                   END-PERFORM
               END-IF

               *> Write education entries
               IF SP-EDU-COUNT (SP-IX) > 0
                   SET SP-EDU-IX TO 1
                   PERFORM SP-EDU-COUNT (SP-IX) TIMES
                       MOVE SPACES TO PROFILE-REC
                       STRING FUNCTION TRIM(SP-EDU-DEGREE (SP-IX, SP-EDU-IX)) "|"
                              FUNCTION TRIM(SP-EDU-UNIV (SP-IX, SP-EDU-IX)) "|"
                              FUNCTION TRIM(SP-EDU-YEARS (SP-IX, SP-EDU-IX))
                              DELIMITED BY SIZE INTO PROFILE-REC
                       END-STRING
                       WRITE PROFILE-REC
                       SET SP-EDU-IX UP BY 1
                   END-PERFORM
               END-IF

               SET SP-IX UP BY 1
           END-PERFORM
       END-IF

       CLOSE PROFILES
       EXIT.


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


*> =========================
*> Search & View Other Users
*> Stories 3–6 and 7
*> =========================
FIND-SOMEONE-YOU-KNOW.
       MOVE "--- Find Someone You Know ---" TO MSG
       PERFORM ECHO-DISPLAY

       *> Prompt for First Name (required)
       PERFORM UNTIL 1 = 0
           MOVE "Enter First Name to search:" TO MSG
           PERFORM ECHO-DISPLAY
           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
           END-READ
           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "First Name is required." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO SRCH-FIRSTNAME
               EXIT PERFORM
           END-IF
       END-PERFORM
       IF EOF-FLAG = "Y" EXIT PARAGRAPH END-IF

       *> Prompt for Last Name (required)
       PERFORM UNTIL 1 = 0
           MOVE "Enter Last Name to search:" TO MSG
           PERFORM ECHO-DISPLAY
           READ USER-IN
               AT END MOVE "Y" TO EOF-FLAG EXIT PARAGRAPH
           END-READ
           IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
               MOVE "Last Name is required." TO MSG
               PERFORM ECHO-DISPLAY
           ELSE
               MOVE FUNCTION TRIM(USER-IN-REC) TO SRCH-LASTNAME
               EXIT PERFORM
           END-IF
       END-PERFORM
       IF EOF-FLAG = "Y" EXIT PARAGRAPH END-IF

       *> Load all profiles and search for exact full-name match (case-insensitive)
       PERFORM LOAD-ALL-PROFILES

       MOVE "N" TO FOUND-FLAG
       IF STORED-PROFILE-COUNT > 0
           SET SP-IX TO 1
           PERFORM UNTIL SP-IX > STORED-PROFILE-COUNT
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(SP-FIRSTNAME (SP-IX)))
                    = FUNCTION UPPER-CASE(FUNCTION TRIM(SRCH-FIRSTNAME))
                AND FUNCTION UPPER-CASE(FUNCTION TRIM(SP-LASTNAME (SP-IX)))
                    = FUNCTION UPPER-CASE(FUNCTION TRIM(SRCH-LASTNAME))
                   MOVE "Y" TO FOUND-FLAG
                   EXIT PERFORM
               ELSE
                   SET SP-IX UP BY 1
               END-IF
           END-PERFORM
       END-IF

       IF FOUND-FLAG = "Y"
           *> Display the matched user's full profile
           PERFORM VIEW-OTHER-PROFILE
       ELSE
           MOVE SPACES TO MSG
           STRING "No user named " FUNCTION TRIM(SRCH-FIRSTNAME) " "
                  FUNCTION TRIM(SRCH-LASTNAME) " was found."
                  DELIMITED BY SIZE INTO MSG
           END-STRING
           PERFORM ECHO-DISPLAY
       END-IF
       EXIT PARAGRAPH.


*> Display profile for SP-IX entry in easy-to-read format (Stories 2,4,5)
VIEW-OTHER-PROFILE.
       MOVE SPACES TO MSG
       STRING "--- Profile for " FUNCTION TRIM(SP-FIRSTNAME (SP-IX)) " "
              FUNCTION TRIM(SP-LASTNAME (SP-IX)) " ---" DELIMITED BY SIZE INTO MSG
       END-STRING
       PERFORM ECHO-DISPLAY

       MOVE SPACES TO MSG
       STRING "Name: " FUNCTION TRIM(SP-FIRSTNAME (SP-IX)) " "
              FUNCTION TRIM(SP-LASTNAME (SP-IX)) DELIMITED BY SIZE INTO MSG
       END-STRING
       PERFORM ECHO-DISPLAY

       MOVE SPACES TO MSG
       STRING "University: " FUNCTION TRIM(SP-UNIVERSITY (SP-IX)) DELIMITED BY SIZE INTO MSG
       END-STRING
       PERFORM ECHO-DISPLAY

       MOVE SPACES TO MSG
       STRING "Major: " FUNCTION TRIM(SP-MAJOR (SP-IX)) DELIMITED BY SIZE INTO MSG
       END-STRING
       PERFORM ECHO-DISPLAY

       MOVE SPACES TO MSG
       STRING "Graduation Year: " SP-YEAR (SP-IX) DELIMITED BY SIZE INTO MSG
       END-STRING
       PERFORM ECHO-DISPLAY

       IF FUNCTION LENGTH(FUNCTION TRIM(SP-ABOUT (SP-IX))) > 0
           MOVE SPACES TO MSG
           STRING "About Me: " FUNCTION TRIM(SP-ABOUT (SP-IX)) DELIMITED BY SIZE INTO MSG
           END-STRING
           PERFORM ECHO-DISPLAY
       END-IF

       *> Experiences
       IF SP-EXP-COUNT (SP-IX) > 0
           IF SP-EXP-COUNT (SP-IX) > 1
               SET SP-EXP-IX TO 1
               PERFORM SP-EXP-COUNT (SP-IX) TIMES
                   MOVE SP-EXP-IX TO DISPLAY-EXP-NUM
                   MOVE SPACES TO MSG
                   STRING "Experience #" DISPLAY-EXP-NUM ":" DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Title: " FUNCTION TRIM(SP-EXP-TITLE (SP-IX, SP-EXP-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Company: " FUNCTION TRIM(SP-EXP-COMPANY (SP-IX, SP-EXP-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Dates: " FUNCTION TRIM(SP-EXP-DATES (SP-IX, SP-EXP-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   IF FUNCTION LENGTH(FUNCTION TRIM(SP-EXP-DESC (SP-IX, SP-EXP-IX))) > 0
                       MOVE SPACES TO MSG
                       STRING " Description: "
                              FUNCTION TRIM(SP-EXP-DESC (SP-IX, SP-EXP-IX))
                              DELIMITED BY SIZE INTO MSG
                       END-STRING
                       PERFORM ECHO-DISPLAY
                   END-IF

                   SET SP-EXP-IX UP BY 1
               END-PERFORM
           ELSE
               MOVE "Experience:" TO MSG
               PERFORM ECHO-DISPLAY
               SET SP-EXP-IX TO 1

               MOVE SPACES TO MSG
               STRING " Title: " FUNCTION TRIM(SP-EXP-TITLE (SP-IX, SP-EXP-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               MOVE SPACES TO MSG
               STRING " Company: " FUNCTION TRIM(SP-EXP-COMPANY (SP-IX, SP-EXP-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               MOVE SPACES TO MSG
               STRING " Dates: " FUNCTION TRIM(SP-EXP-DATES (SP-IX, SP-EXP-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               IF FUNCTION LENGTH(FUNCTION TRIM(SP-EXP-DESC (SP-IX, SP-EXP-IX))) > 0
                   MOVE SPACES TO MSG
                   STRING " Description: "
                          FUNCTION TRIM(SP-EXP-DESC (SP-IX, SP-EXP-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY
               END-IF
           END-IF
       END-IF

       *> Education
       IF SP-EDU-COUNT (SP-IX) > 0
           IF SP-EDU-COUNT (SP-IX) > 1
               SET SP-EDU-IX TO 1
               PERFORM SP-EDU-COUNT (SP-IX) TIMES
                   MOVE SP-EDU-IX TO DISPLAY-EDU-NUM
                   MOVE SPACES TO MSG
                   STRING "Education #" DISPLAY-EDU-NUM ":" DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Degree: " FUNCTION TRIM(SP-EDU-DEGREE (SP-IX, SP-EDU-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " University: " FUNCTION TRIM(SP-EDU-UNIV (SP-IX, SP-EDU-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   MOVE SPACES TO MSG
                   STRING " Years: " FUNCTION TRIM(SP-EDU-YEARS (SP-IX, SP-EDU-IX))
                          DELIMITED BY SIZE INTO MSG
                   END-STRING
                   PERFORM ECHO-DISPLAY

                   SET SP-EDU-IX UP BY 1
               END-PERFORM
           ELSE
               MOVE "Education:" TO MSG
               PERFORM ECHO-DISPLAY
               SET SP-EDU-IX TO 1

               MOVE SPACES TO MSG
               STRING " Degree: " FUNCTION TRIM(SP-EDU-DEGREE (SP-IX, SP-EDU-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               MOVE SPACES TO MSG
               STRING " University: " FUNCTION TRIM(SP-EDU-UNIV (SP-IX, SP-EDU-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY

               MOVE SPACES TO MSG
               STRING " Years: " FUNCTION TRIM(SP-EDU-YEARS (SP-IX, SP-EDU-IX))
                      DELIMITED BY SIZE INTO MSG
               END-STRING
               PERFORM ECHO-DISPLAY
           END-IF
       END-IF

       *> Offer to send friend request to this user (uses SP-USERNAME (SP-IX))
       MOVE "Would you like to send a friend request? (1=Yes, 2=No)" TO MSG
       PERFORM ECHO-DISPLAY
       READ USER-IN
           AT END EXIT PARAGRAPH
       END-READ
       IF FUNCTION TRIM(USER-IN-REC) = "1"
           MOVE FUNCTION TRIM(SP-USERNAME (SP-IX)) TO TARGET-USER
           PERFORM SEND-FRIEND-REQUEST
       END-IF

       MOVE "--------------------" TO MSG
       PERFORM ECHO-DISPLAY
       EXIT PARAGRAPH.

*> ===== Connections Persistence =====
*> Read entire connections.dat into CONN-TABLE
LOAD-ALL-CONNECTIONS.
    MOVE 0 TO CONN-COUNT
    OPEN INPUT CONNECTIONS
    PERFORM UNTIL 1 = 0
        READ CONNECTIONS
           AT END EXIT PERFORM
        END-READ
        IF CONN-COUNT < MAX-CONNECTIONS
           ADD 1 TO CONN-COUNT
           SET C-IX TO CONN-COUNT
           UNSTRING CONN-REC DELIMITED BY ","
               INTO C-USER1 (C-IX) C-USER2 (C-IX)
           END-UNSTRING
           *> Trim
           MOVE FUNCTION TRIM(C-USER1 (C-IX)) TO C-USER1 (C-IX)
           MOVE FUNCTION TRIM(C-USER2 (C-IX)) TO C-USER2 (C-IX)
        END-IF
    END-PERFORM
    CLOSE CONNECTIONS
    EXIT.

*> Rewrite entire file from memory
SAVE-ALL-CONNECTIONS.
    *> Safety check removed - always create/truncate file
    OPEN OUTPUT CONNECTIONS
    IF CONN-COUNT > 0
        SET C-IX TO 1
        PERFORM UNTIL C-IX > CONN-COUNT
            MOVE SPACES TO CONN-REC
            STRING
                FUNCTION TRIM(C-USER1 (C-IX))   ","
                FUNCTION TRIM(C-USER2 (C-IX))
                DELIMITED BY SIZE
                INTO CONN-REC
            END-STRING

            WRITE CONN-REC
            SET C-IX UP BY 1
        END-PERFORM
    END-IF
    CLOSE CONNECTIONS
    EXIT.

LOAD-ALL-REQUESTS.
      MOVE 0 TO REQUESTS-COUNT
      OPEN INPUT REQUESTS
      PERFORM UNTIL 1 = 0
           READ REQUESTS
               AT END EXIT PERFORM
           END-READ
           IF REQUESTS-COUNT < MAX-REQUESTS
                  ADD 1 TO REQUESTS-COUNT
                  SET R-IX TO REQUESTS-COUNT
                  UNSTRING REQ-REC DELIMITED BY "|"
                      INTO R-SENDER (R-IX) R-RECEIVER (R-IX) R-STATUS (R-IX)
                  END-UNSTRING
                  MOVE FUNCTION TRIM(R-SENDER (R-IX))   TO R-SENDER (R-IX)
                  MOVE FUNCTION TRIM(R-RECEIVER (R-IX)) TO R-RECEIVER (R-IX)
                  MOVE FUNCTION TRIM(R-STATUS (R-IX))   TO R-STATUS (R-IX)
           END-IF
           END-PERFORM
           CLOSE REQUESTS
           EXIT.

SAVE-ALL-REQUESTS.
      OPEN OUTPUT REQUESTS
      IF REQUESTS-COUNT > 0
           SET R-IX TO 1
           PERFORM UNTIL R-IX > REQUESTS-COUNT
           MOVE SPACES TO REQ-REC
           STRING
               FUNCTION TRIM(R-SENDER (R-IX)) "|"
               FUNCTION TRIM(R-RECEIVER (R-IX)) "|"
               FUNCTION TRIM(R-STATUS (R-IX))
               DELIMITED BY SIZE
               INTO REQ-REC
               END-STRING
               WRITE REQ-REC
               SET R-IX UP BY 1
           END-PERFORM
       END-IF
       CLOSE REQUESTS
       EXIT.


*> Helper: get the other username for an ACCEPTED record (relative to CURRENT-USER)
GET-OTHER-USER.
    IF FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER1 (C-IX))) =
       FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
        MOVE C-USER2 (C-IX) TO OTHER-USER
    ELSE
        MOVE C-USER1 (C-IX) TO OTHER-USER
    END-IF
    EXIT.

*> Case-insensitive existence check for an arbitrary username in USER-TABLE
EXISTS-USER-BY-NAME.
    MOVE "N" TO FOUND-FLAG
    IF ACCOUNT-COUNT > 0
        SET U-IX TO 1
        PERFORM UNTIL U-IX > ACCOUNT-COUNT
            IF FUNCTION UPPER-CASE(FUNCTION TRIM(CHECK-USER))
               = FUNCTION UPPER-CASE(FUNCTION TRIM(T-USERNAME (U-IX)))
                MOVE "Y" TO FOUND-FLAG
                EXIT PERFORM
            ELSE
                SET U-IX UP BY 1
            END-IF
        END-PERFORM
    END-IF
    EXIT.


*> Does ANY record exist between USER-A and USER-B (either direction)?
FIND-ANY-CONNECTION.
    MOVE "N" TO ANY-FOUND
    IF CONN-COUNT > 0
        SET C-IX TO 1
        PERFORM UNTIL C-IX > CONN-COUNT
            IF ( FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER1 (C-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(USER-A))
             AND FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER2 (C-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(USER-B)) )
             OR ( FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER1 (C-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(USER-B))
             AND FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER2 (C-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(USER-A)) )
                MOVE "Y" TO ANY-FOUND
                EXIT PERFORM
            ELSE
                SET C-IX UP BY 1
            END-IF
        END-PERFORM
    END-IF
    EXIT.


*> Create a pending request CURRENT-USER -> TARGET-USER
SEND-FRIEND-REQUEST.
    *> Load both tables
    PERFORM LOAD-ALL-CONNECTIONS
    PERFORM LOAD-ALL-REQUESTS

    *> 1) No self-add
    IF FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) =
       FUNCTION UPPER-CASE(FUNCTION TRIM(TARGET-USER))
        MOVE "You cannot add yourself." TO MSG
        PERFORM ECHO-DISPLAY
        EXIT PARAGRAPH
    END-IF

    *> 2) Receiver must exist
    MOVE FUNCTION TRIM(TARGET-USER) TO CHECK-USER
    PERFORM EXISTS-USER-BY-NAME
    IF FOUND-FLAG NOT = "Y"
        MOVE "User does not exist." TO MSG
        PERFORM ECHO-DISPLAY
        EXIT PARAGRAPH
    END-IF

    *> 3) Check for existing requests in REQUESTS-TABLE
    MOVE "N" TO ANY-FOUND
    IF REQUESTS-COUNT > 0
        SET R-IX TO 1
        PERFORM UNTIL R-IX > REQUESTS-COUNT
            IF ( FUNCTION UPPER-CASE(FUNCTION TRIM(R-SENDER (R-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
             AND FUNCTION UPPER-CASE(FUNCTION TRIM(R-RECEIVER (R-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(TARGET-USER)) )
             OR ( FUNCTION UPPER-CASE(FUNCTION TRIM(R-SENDER (R-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(TARGET-USER))
             AND FUNCTION UPPER-CASE(FUNCTION TRIM(R-RECEIVER (R-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) )
                MOVE "Y" TO ANY-FOUND
                EXIT PERFORM
            ELSE
                SET R-IX UP BY 1
            END-IF
        END-PERFORM
    END-IF

    *> 4) Check for existing connections in CONN-TABLE
    IF ANY-FOUND = "N" AND CONN-COUNT > 0
        SET C-IX TO 1
        PERFORM UNTIL C-IX > CONN-COUNT
            IF ( FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER1 (C-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
             AND FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER2 (C-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(TARGET-USER)) )
             OR ( FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER1 (C-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(TARGET-USER))
             AND FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER2 (C-IX))) =
                 FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) )
                MOVE "Y" TO ANY-FOUND
                EXIT PERFORM
            ELSE
                SET C-IX UP BY 1
            END-IF
        END-PERFORM
    END-IF

    IF ANY-FOUND = "Y"
        MOVE "There is already a request or connection between you." TO MSG
        PERFORM ECHO-DISPLAY
        EXIT PARAGRAPH
    END-IF

    *> 5) Add new PENDING request to REQUESTS-TABLE
    IF REQUESTS-COUNT < MAX-REQUESTS
        ADD 1 TO REQUESTS-COUNT
        SET R-IX TO REQUESTS-COUNT
        MOVE FUNCTION TRIM(CURRENT-USER) TO R-SENDER   (R-IX)
        MOVE FUNCTION TRIM(TARGET-USER)  TO R-RECEIVER (R-IX)
        MOVE "PENDING"                   TO R-STATUS   (R-IX)
        PERFORM SAVE-ALL-REQUESTS
        MOVE "Friend request sent."      TO MSG
        PERFORM ECHO-DISPLAY
    ELSE
        MOVE "Cannot send request: requests storage is full." TO MSG
        PERFORM ECHO-DISPLAY
    END-IF
    EXIT PARAGRAPH.


*> ===== Connections Menu & Features (Juan) =====
CONNECTIONS-MENU.
    MOVE 0 TO CONN-MENU-CHOICE
    PERFORM UNTIL EOF-FLAG = "Y"
        PERFORM DISPLAY-CONNECTIONS-MENU

        READ USER-IN
            AT END MOVE "Y" TO EOF-FLAG
        END-READ
        IF EOF-FLAG = "Y" EXIT PERFORM END-IF

        IF FUNCTION LENGTH(FUNCTION TRIM(USER-IN-REC)) = 0
            CONTINUE
        ELSE
            IF FUNCTION TEST-NUMVAL(USER-IN-REC) = 0
                MOVE FUNCTION NUMVAL(USER-IN-REC) TO CONN-MENU-CHOICE
            ELSE
                MOVE 999 TO CONN-MENU-CHOICE
            END-IF

            EVALUATE CONN-MENU-CHOICE
                WHEN 1
                    PERFORM VIEW-AND-RESPOND-PENDING
                WHEN 2
                    PERFORM VIEW-MY-CONNECTIONS
                WHEN 3
                    EXIT PERFORM
                WHEN OTHER
                    MOVE "Invalid choice, please try again." TO MSG
                    PERFORM ECHO-DISPLAY
            END-EVALUATE
        END-IF
    END-PERFORM
    EXIT.

DISPLAY-CONNECTIONS-MENU.
    MOVE " " TO MSG
    PERFORM ECHO-DISPLAY
    MOVE "----- Connections -----" TO MSG
    PERFORM ECHO-DISPLAY
    MOVE "  1. View & Respond to Pending Requests" TO MSG
    PERFORM ECHO-DISPLAY
    MOVE "  2. View My Connections" TO MSG
    PERFORM ECHO-DISPLAY
    MOVE "  3. Go Back" TO MSG
    PERFORM ECHO-DISPLAY
    MOVE "Enter your choice:" TO MSG
    PERFORM ECHO-DISPLAY
    EXIT.

VIEW-AND-RESPOND-PENDING.
    PERFORM LOAD-ALL-REQUESTS

    MOVE 0 TO PENDING-COUNT
    *> Build index list of pending requests where CURRENT-USER is receiver
    IF REQUESTS-COUNT > 0
        SET R-IX TO 1
        PERFORM UNTIL R-IX > REQUESTS-COUNT
            IF FUNCTION UPPER-CASE(FUNCTION TRIM(R-RECEIVER (R-IX))) =
               FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
               AND FUNCTION UPPER-CASE(FUNCTION TRIM(R-STATUS (R-IX))) = "PENDING"
                ADD 1 TO PENDING-COUNT
                SET P-IX TO PENDING-COUNT
                MOVE R-IX TO P-ROW (P-IX)
            END-IF
            SET R-IX UP BY 1
        END-PERFORM
    END-IF

    IF PENDING-COUNT = 0
        MOVE "You have no pending friend requests." TO MSG
        PERFORM ECHO-DISPLAY
        EXIT PARAGRAPH
    END-IF

    MOVE "--- Pending Friend Requests ---" TO MSG
    PERFORM ECHO-DISPLAY

    *> List them numbered
    SET P-IX TO 1
    PERFORM PENDING-COUNT TIMES
        MOVE P-ROW (P-IX) TO I
        SET R-IX TO I
        MOVE SPACES TO MSG
        STRING P-IX ". From: " FUNCTION TRIM(R-SENDER (R-IX))
               DELIMITED BY SIZE INTO MSG
        END-STRING
        PERFORM ECHO-DISPLAY
        SET P-IX UP BY 1
    END-PERFORM

    *> Prompt to respond
    MOVE "Enter the sender's username to respond (blank to go back):" TO MSG
    PERFORM ECHO-DISPLAY
    READ USER-IN
        AT END MOVE "Y" TO EOF-FLAG
    END-READ
    IF EOF-FLAG = "Y" EXIT PARAGRAPH END-IF

    MOVE FUNCTION TRIM(USER-IN-REC) TO RESP-USER
    IF FUNCTION LENGTH(RESP-USER) = 0
        EXIT PARAGRAPH
    END-IF

    *> Find the exact request again - store the index immediately
    MOVE 0 TO I
    IF REQUESTS-COUNT > 0
        SET R-IX TO 1
        PERFORM UNTIL R-IX > REQUESTS-COUNT
            IF FUNCTION UPPER-CASE(FUNCTION TRIM(R-RECEIVER (R-IX))) =
               FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
            AND FUNCTION UPPER-CASE(FUNCTION TRIM(R-SENDER (R-IX))) =
               FUNCTION UPPER-CASE(FUNCTION TRIM(RESP-USER))
            AND FUNCTION UPPER-CASE(FUNCTION TRIM(R-STATUS (R-IX))) = "PENDING"
                MOVE R-IX TO I
                EXIT PERFORM
            ELSE
                SET R-IX UP BY 1
            END-IF
        END-PERFORM
    END-IF

    IF I = 0
        MOVE "No matching pending request from that user." TO MSG
        PERFORM ECHO-DISPLAY
        EXIT PARAGRAPH
    END-IF

    *> Ask action
    MOVE "Type ACCEPT or REJECT:" TO MSG
    PERFORM ECHO-DISPLAY
    READ USER-IN
        AT END MOVE "Y" TO EOF-FLAG
    END-READ
    IF EOF-FLAG = "Y" EXIT PARAGRAPH END-IF

    MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(USER-IN-REC)) TO RESP-ACT

    *> Use the stored index I directly
    SET R-IX TO I

    IF RESP-ACT = "ACCEPT"
        *> Update request status to ACCEPTED
        SET R-IX TO I
        MOVE "ACCEPTED" TO R-STATUS (R-IX)
        PERFORM SAVE-ALL-REQUESTS

        *> Add to established connections
        PERFORM LOAD-ALL-CONNECTIONS
        IF CONN-COUNT < MAX-CONNECTIONS
            ADD 1 TO CONN-COUNT
            SET C-IX TO CONN-COUNT

            *> Get the usernames from the original input
            MOVE FUNCTION TRIM(RESP-USER) TO C-USER1 (C-IX)
            MOVE FUNCTION TRIM(CURRENT-USER) TO C-USER2 (C-IX)

            PERFORM SAVE-ALL-CONNECTIONS
        END-IF

        MOVE "Request accepted. You are now connected." TO MSG
        PERFORM ECHO-DISPLAY
    ELSE
        IF RESP-ACT = "REJECT"
            MOVE "REJECTED" TO R-STATUS (R-IX)
            PERFORM SAVE-ALL-REQUESTS
            MOVE "Request rejected." TO MSG
            PERFORM ECHO-DISPLAY
        ELSE
            MOVE "Invalid response. Please type ACCEPT or REJECT next time." TO MSG
            PERFORM ECHO-DISPLAY
        END-IF
    END-IF
    EXIT PARAGRAPH.

VIEW-MY-CONNECTIONS.
    PERFORM LOAD-ALL-CONNECTIONS
    PERFORM LOAD-ALL-PROFILES
    MOVE "N" TO FOUND-FLAG

    MOVE "--- Your Network ---" TO MSG
    PERFORM ECHO-DISPLAY

    IF CONN-COUNT = 0
        MOVE "You have no connections yet." TO MSG
        PERFORM ECHO-DISPLAY
        EXIT PARAGRAPH
    END-IF

    SET C-IX TO 1
    PERFORM UNTIL C-IX > CONN-COUNT
        IF ( FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER1 (C-IX))) =
             FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
          OR FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER2 (C-IX))) =
             FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) )
            MOVE "Y" TO FOUND-FLAG

            *> Get the other user's name
            IF FUNCTION UPPER-CASE(FUNCTION TRIM(C-USER1 (C-IX))) =
               FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
                MOVE C-USER2 (C-IX) TO OTHER-USER
            ELSE
                MOVE C-USER1 (C-IX) TO OTHER-USER
            END-IF

            *> Find profile data for OTHER-USER
            PERFORM FIND-USER-PROFILE

            IF FOUND-FLAG = "Y"
                MOVE SPACES TO MSG
                STRING "Connected with: " FUNCTION TRIM(OTHER-USER)
                       " (University: " FUNCTION TRIM(SP-UNIVERSITY (SP-IX))
                       ", Major: " FUNCTION TRIM(SP-MAJOR (SP-IX)) ")"
                       DELIMITED BY SIZE INTO MSG
                END-STRING
                PERFORM ECHO-DISPLAY
            ELSE
                MOVE SPACES TO MSG
                STRING "Connected with: " FUNCTION TRIM(OTHER-USER)
                       " (Profile not found)"
                       DELIMITED BY SIZE INTO MSG
                END-STRING
                PERFORM ECHO-DISPLAY
            END-IF
        END-IF
        SET C-IX UP BY 1
    END-PERFORM

    IF FOUND-FLAG NOT = "Y"
        MOVE "You have no connections yet." TO MSG
        PERFORM ECHO-DISPLAY
    END-IF
    EXIT PARAGRAPH.

*> Helper function to find profile for OTHER-USER
FIND-USER-PROFILE.
    MOVE "N" TO FOUND-FLAG
    IF STORED-PROFILE-COUNT > 0
        SET SP-IX TO 1
        PERFORM UNTIL SP-IX > STORED-PROFILE-COUNT
            IF FUNCTION UPPER-CASE(FUNCTION TRIM(SP-USERNAME (SP-IX))) =
               FUNCTION UPPER-CASE(FUNCTION TRIM(OTHER-USER))
                MOVE "Y" TO FOUND-FLAG
                EXIT PERFORM
            ELSE
                SET SP-IX UP BY 1
            END-IF
        END-PERFORM
    END-IF
    EXIT.

