# InCollege Project

This project is an implementation of the InCollege system in COBOL, developed incrementally through Epics 1–6.

## Current features include

**Epic 1:** Create up to 5 accounts (unique username + validated password), unlimited login attempts.
**Epic 2:** Create, edit, and view user profiles (About Me, Experience, Education).
**Epic 3:** Search for users by full name and view their profiles.
**Epic 4:** Send, view, accept, and reject connection requests.
**Epic 5:** Finalized connection system. Users can manage pending requests (accept/reject), view their established network, and all inputs/outputs are file-based and persisted.
**Epic 6:** Job/Internship posting functionality. Users can post job listings with title, description, employer, location, and optional salary. All postings are persistently stored with unique auto-incrementing IDs.

## Persistence

All data is stored across runs in:

    - **accounts.dat** - account credentials

    - **profiles.dat** - user profiles

    - **requests.dat** -  pending/processed connection requests

    - **connections.dat** - established (accepted) connections
---

## Getting Started

### Prerequisites

Before you begin, make sure you have the following installed on your machine:

- **Docker Desktop:**
  Download and install from [Docker](https://www.docker.com/products/docker-desktop).
  Be sure Docker is running before you open the project.

- **Visual Studio Code (VS Code):**
  Download from [VS Code](https://code.visualstudio.com/).

- **VS Code Extensions:**
  - [Dev Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers):
    This extension lets you open the project inside a Docker container.

### Open the Project in a Devcontainer

1. **Clone the repository**

    Open a terminal and run:
    ```bash
    git clone https://github.com/Thao2004/cen4020-georgia-incollege.git
    cd cen4020-georgia-incollege
    ```

2. **Start Docker Desktop**

    Ensure Docker is running in the background (you should see the whale icon in your taskbar or menu bar).

3. **Launch the project**

    - Open the project in VS Code

    - Press `Cmd+Shift+P` **(on macOS)** or `Ctrl+Shift+P` **(on Windows/Linux)** to open the **Command Palette**.
    - Search for and select: `“Dev Containers: Reopen in Container”`

4. **Wait for the container to build**
    - The first build may take several minutes while Docker downloads and installs dependencies.
    - Once it’s ready, you’ll see a terminal prompt inside the container at `/workspace`.

5. **Verify COBOL is available**
    Run the following inside the container terminal:
    ```bash
    cobc -V
    ```
    You should see the version of GnuCOBOL installed.


---

## Running the Program

You have **two options**:

### Option 1: Using Makefile (recommended)

From inside the devcontainer terminal:
```bash
make run
```
This will:
- Compile `src/InCollege.cob` into `bin/InCollege`
- Run the program automatically

### Option 2: Manual compile and run

```bash
cobc -x -free -o bin/InCollege src/InCollege.cob
./bin/InCollege
```

---

## Preparing Input Files

All user input is read from a text file (`InCollege-Input.txt`).
The terminal shows prompts, but the actual responses come from this file.

### Rules

- First line = your menu choice
    - 1 = Log In
    - 2 = Create New Account
- Next lines = username and password
    - Username: 1–12 characters
    - Password: 8–12 characters, must include:
        - at least one uppercase letter
        - at least one digit
        - at least one special character (e.g., !, @, #, $)

## Examples

### Example 1: Create an account

Prepare `InCollege-Input.txt` with the following content:

```text
2
jennifer
Hellother1!
```

Then save and run
```bash
make run
```

- The account will be saved in `accounts.dat`
- Output will be shown on screen and in `InCollege-Output.txt`

### Example 2: Log in (with one failed attempt)

Prepare `InCollege-Input.txt` with the following content:
```text
1
jennifer
wrongPass1!
jennifer
Hellother1!
```

Then save and run:
```bash
make run
```

- The program will reject the first attempt, displaying: `Incorrect username/password, please try again`.
- On the second attempt, you’ll see: `You have succesfully logged in`

### Example 3: Create/Edit Profile

After logging in, choose option `1` from the main menu.
Required fields: first name, last name, university, major, graduation year.
Optional fields: About Me (200 chars max), up to 3 experiences, up to 3 education entries.

### Example 4: Search for User and Send Connection Request

After logging in, choose option `4` (Find someone you know).
Enter first and last name exactly. If found, the profile is displayed.
You may then choose to send a connection request.
Requests are saved in `connections.dat` with status `PENDING`.

### Example 5: View and Respond to Pending Requests

From the main menu, choose `5` (View My Pending Connection Requests).
You can accept or reject requests. Accepted connections move to your list; rejected ones disappear.

### Example 6: Post a Job/Internship
After logging in, choose option 3 (Search for a job) from the main menu.
Select option 1 (Post a Job/Internship) from the Job Search menu.
Enter the following required information: Job Title, Description, Employer Name, Location, Salary (optional - enter "NONE" to skip).

## Data Files

**InCollege-Input.txt**: User input file, sequence of actions entered as text.
**InCollege-Output.txt**: Contains a copy of all prompts and messages shown in the terminal.
**accounts.dat**: Stores created accounts in the format `username, password`
**connections.dat**: Stores connection requests and accepted connections.
**profiles.dat**: Stores user profiles (basic info, About Me, experiences, education).
**requests.dat**: Stores pending and processed connection requests
**job-postings.dat**: Stores job and internship postings in format: Job-ID|Title|Description|Employer|Location|Salary

## Cleaning Up

To remove compiled binary and generated files:
```bash
make clean
```
---

## File Structure

```
.
├── .devcontainer
│   └── devcontainer.json
│   └── Dockerfile
├── bin/                        # Compiled executable
│   └── InCollege
├── src/                        # COBOL source code
│   └── InCollege.cob
├── accounts.dat                # Account storage (generated)
├── profiles.dat                # Profile storage (generated)
├── connections.dat             # Connection requests & connections (generated)
├── requests.dat                # Pending connection requests (generated)
├── job-postings.dat            # Job/internship postings (generated)
├── InCollege-Input.txt         # Input file (edit this to test)
├── InCollege-Output.txt        # Output log (generated)
├── Epic5-StoryX-Test-Input     # Test input folder
│   └──Test01-....txt           # Test input files
├── Epic5-StoryX-Test-Output    # Test output folder
│   └──Test01-....txt           # Test output files
├── Epic6-StoryX-Test-Input     # Epic 6 test input folder
│   └──Test01-....txt           # Job posting test input files
├── Epic6-StoryX-Test-Output    # Epic 6 test output folder
│   └──Test01-....txt           # Job posting test output files
├── InCollege-Test.txt          # Test file
├── Makefile
└── README.md
```
