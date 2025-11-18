# Epic 10 – Final Polish & Regression Hardening
This TASKS.md defines the work Dev #1 must complete for Epic 10.
All tasks must be implemented **ONLY inside `src/InCollege.cob`**.
No new modules, no refactoring into separate files, no structural rewrites.

All functionality and output formatting from Epics 1–9 must remain EXACTLY the same.

============================================================
## Completed Tasks

- [x] Regression & Hardening: Registration & Login
  - ✅ All username validation working (1-15 chars, uniqueness)
  - ✅ All password validation working (8-12 chars, uppercase, digit, special)
  - ✅ Account limit enforced (max 5)
  - ✅ All success/error messages match spec exactly
  - ✅ Login messages correct ("You have successfully logged in", "Welcome, <username>!")
  - ✅ Tests generated and verified

- [x] UI/UX Improvements: Validation & Navigation
  - ✅ Standardized all "Invalid choice" messages to "Invalid choice, please try again."
  - ✅ All menus have clear titles and numbered options
  - ✅ Navigation options are clear and consistent

- [x] Enhanced Display Formatting For Lists
  - ✅ Job lists use format: N. <Job Title> at <Employer> (<Location>)
  - ✅ Messages use proper format with From:, Sent:, Message:, and separator
  - ✅ Connections list shows "Connected with: <First> <Last>" or "(Profile not found)"

- [x] Regression & Polish: Profile Creation, Editing & Display
  - ✅ All required fields validated (first, last, university, major, year)
  - ✅ Optional fields handled correctly (About Me max 200, Experience max 3, Education max 3)
  - ✅ Profile display formatting correct with proper labels
  - ✅ Word wrapping for long text at 80 chars

- [x] Regression: User Search & Profile Viewing
  - ✅ First/Last name search is case-insensitive
  - ✅ "No user named <First> <Last> was found." message correct
  - ✅ VIEW-OTHER-PROFILE formatting matches spec
  - ✅ Friend request prompt appears correctly

============================================================
## In Progress Tasks

(All tasks completed!)

============================================================
## Future Tasks

(Nothing yet — everything for Dev #1 lives in “In Progress”)

============================================================
## Implementation Plan

### 1. Regression & Hardening: Registration & Login
Verify and ensure all registration/login behavior matches project spec:

- Username length (1–15)
- Password rules (8–12 chars, uppercase, digit, special)
- Account limit (max 5)
- Login success messages:
  - You have successfully logged in
  - Welcome, <username>!
- Error messages:
  - Incorrect username/password, please try again.
  - Account created.
  - All permitted accounts have been created, please come back later
- Ensure **NOTHING about behavior or output changes**
- Ensure all Epic 1–9 tests still pass

### 2. Regression & Polish: Profile Creation, Editing & Display
Strengthen and clean profile-related behaviors:

- Required fields: first, last, university, major, grad year
- Optional fields: About Me, Experience, Education
- Correct persistence to profiles.dat
- Editing updates the same record, does *not* create duplicates
- Display formatting must match spec exactly
- 80-char wrapping for long text
- Match sample outputs from prior epics exactly

### 3. Regression: User Search & Profile Viewing
Ensure:

- First/Last name search works (case-insensitive)
- No-user-found message format:
  - No user named <First> <Last> was found.
- VIEW-OTHER-PROFILE formatting identical to spec
- Friend-request prompt appears:
  - Would you like to send a friend request? (1=Yes, 2=No)
- All Epic 6–9 search tests still pass

### 4. UI/UX Improvements: Validation & Navigation
Ensure all menus:

- Have a clear title
- Numbered options
- A final Back/Exit option
- Clean consistent prompts:
  Enter your choice:

Ensure invalid input ALWAYS results in:
  Invalid choice, please try again.

Ensure nothing contradicts the actual menu options.

### 5. Enhanced Display Formatting For Lists
Standardize formatting for:

**Job lists**
`N. <Job Title> at <Employer> (<Location>)`

**Application summaries**
Same exact format as job lists.

**Message viewer**
```
From: <Sender>
Sent: <Timestamp>
Message:
<wrapped content>
-----------------------------
```

**Connections list**
```
Connected with: <First> <Last>
```
or
```
Connected with: <username> (Profile not found)
```

Spacing and separators must match previous epics.

============================================================
## Relevant Files

### src/InCollege.cob
The ONLY file that must be edited for all tasks in this project.
Contains all program logic, menus, validation, persistence, and display code.

============================================================
## Additional Requirements for Copilot Agent Mode

1. NEVER create new COBOL modules or files.
2. NEVER modify the Makefile.
3. ALWAYS maintain 100% test output compatibility with all previous epics.
4. After every significant code change:
   - Rebuild using:
     ```
     make clean
     make run < input.txt > output.txt
     ```
   - Compare `output.txt` to expected outputs.
5. Update this TASKS.md after completing each task.