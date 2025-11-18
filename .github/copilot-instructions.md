# Copilot Agent Mode Instructions for InCollege (COBOL)

You are an automated development assistant for the **InCollege COBOL project**.
Your primary responsibilities:

1. Implement tasks defined in `TASKS.md`
2. Test the program automatically
3. Maintain existing behavior and output EXACTLY as defined in Epics 1–9
4. Never create new files except **test input/output files**
5. Only modify:
   **src/InCollege.cob**
   (No new modules, no new COBOL files)

Your outputs must include:
- Code modifications (when needed)
- Updates to `TASKS.md`
- Automatically generated input/output test files
- Summaries of verification steps taken

==============================================================
# 1. HOW YOU MUST NAVIGATE THE PROGRAM (STATE MACHINE)
==============================================================

You MUST treat the program as a deterministic, menu-driven state machine.
Never guess transitions. Never invent menu items.
State detection is based ONLY on the screen output printed by InCollege.

--------------------------------------
## STATE 1: MAIN MENU
--------------------------------------
Program prints:
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:

Valid Inputs:
1 → Login
2 → Create Account
Anything else → Invalid choice → returns to Main Menu

--------------------------------------
## STATE 2A: CREATE ACCOUNT
--------------------------------------
Prompts:
Please enter your username:
Please enter your password:

Program will print EXACTLY ONE of:

- Account created.
- Username already exists.
- All permitted accounts have been created, please come back later
- Password must be 8-12 characters long.
- Password must include at least one uppercase letter.
- Password must include at least one digit.
- Password must include at least one special character.

Then returns to **Main Menu**.

--------------------------------------
## STATE 2B: LOGIN
--------------------------------------
Prompts:
Please enter your username:
Please enter your password:

If login succeeds:
You have successfully logged in
Welcome, <username>!
→ transition to STATE 3

If login fails:
Incorrect username/password, please try again
→ return to STATE 1

--------------------------------------
## STATE 3: NAVIGATION MENU
--------------------------------------
=============================
     InCollege Main Menu
=============================
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
6. View My Pending Connection Requests
7. View My Network
8. Messages
=============================
Enter your choice:

--------------------------------------
## STATE 4: JOB SEARCH MENU
--------------------------------------
--- Job Search/Intership Menu ---
1. Post a Job/Internship
2. Browse Jobs/Internships
3. View My Applications
4. Back to Main Menu
Enter your choice:

--------------------------------------
## STATE 5: SKILLS MENU
--------------------------------------
Choose a skill to learn:
1) Python
2) Excel
3) Public Speaking
4) Time Management
5) Leadership
6) Go Back
Enter your choice:

--------------------------------------
## STATE 6: MESSAGES MENU
--------------------------------------
--- Messages Menu ---
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:

==============================================================
# 2. HOW TO GENERATE TESTS (MANDATORY FORMAT)
==============================================================

All test cases MUST be produced as:

One input per line
No quotes
No extra spacing

Example:
```
2
tien
Password1!
1
tien
Password1!
8
3
```

==============================================================
# 3. WHERE TO PUT TEST FILES
==============================================================

You must create this folder structure if it does not exist:

```
tests/
    Epic10/
        Story1/
            Test01-name-input.txt
            Test01-name-output.txt
        Story2/
            Test02-name-input.txt
            Test02-name-output.txt
        Story3/
        Story4/
        Story5/
```

Rules:
- `*-input.txt` = input to feed into program
- `*-output.txt` = the program output you capture
- Tests MUST be runnable via stdin redirection

==============================================================
# 4. HOW TO RUN THE PROGRAM FOR TESTING
==============================================================

Use the provided Makefile.

Build:
```
make clean
make build
```

Run program with a test file:
```
./bin/InCollege < tests/Epic10/Story1/Test01-name-input.txt > tests/Epic10/Story1/Test01-name-output.txt
```

If the directory does not exist → create it automatically.

==============================================================
# 5. HOW TO VERIFY OUTPUT
==============================================================

For every test case you generate:

1. Run the program with stdin redirect
2. Capture stdout
3. Compare output EXACTLY, including:
   - Spacing
   - Blank lines
   - Capitalization
   - Punctuation
4. If output is not an exact match:
   - Fix the bug
   - Re-run the test
   - Update `TASKS.md` when resolved

==============================================================
# 6. DEVELOPMENT RULES
==============================================================

You MUST follow these:

### ✔ Only modify ONE file:
**src/InCollege.cob**

### ✔ Never modify or delete:
- Makefile
- test files
- data files (accounts.dat, profiles.dat, etc.)

### ✔ NEVER create new COBOL modules
NO `InCollege-UserProfile.cbl`
NO splitting logic into separate source files
NO using CALL statements

### ✔ Maintain backward compatibility
All outputs for Epics 1–9 must remain identical.

### ✔ After every change:
Rebuild:
```
make clean
make build
```
Run regression tests:
At minimum:
- login tests
- account creation tests
- search tests
- profiles tests
- job tests
- messaging tests

==============================================================
# 7. HOW TO UPDATE TASKS.MD
==============================================================

After completing a task:
- Change `[ ]` to `[x]`
- Add a short summary under the completed task section
- Add any new subtasks discovered during development
- Maintain relevant files list

==============================================================
# 8. WHEN ASKING "Which task next?"
==============================================================

Follow this priority order:

1. Regression & Hardening: Registration & Login
2. Regression & Polish: Profile Creation & Display
3. Regression: User Search & Profile Viewing
4. Enhanced Display Formatting For Lists
5. UI/UX Improvements: Validation & Navigation

==============================================================
# 9. OUTPUT RULES
==============================================================

Your output may ONLY include:

- Test files
- Code diffs
- `TASKS.md` updates
- Verification logs

Never include:
- Explanations not tied to tasks
- Random commentary
- Irrelevant code changes

==============================================================
# END OF FILE – Copilot must obey all rules above
==============================================================