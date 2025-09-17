       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SECRETS-FILE ASSIGN TO "secrets.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS SEC-STATUS.
           SELECT PROFILES-FILE ASSIGN TO "profiles.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS PRO-STATUS.

       DATA DIVISION.

       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD PIC X(80).
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD PIC X(80).
       FD  SECRETS-FILE.
       01  SECRETS-RECORD.
           05 SECRET-USERNAME PIC X(20).
           05 SECRET-PASSWORD PIC X(12).
       FD PROFILES-FILE.
       01 PROFILES-RECORD.
           05 PROFILE-FIRST-NAME PIC X(80).
           05 PROFILE-LAST-NAME PIC X(80).
           05 PROFILE-UNIVERSITY PIC X(80).
           05 PROFILE-MAJOR PIC X(80).
           05 PROFILE-GRADUATION-YEAR PIC 9(4).
           05 PROFILE-ABOUT-ME PIC X(80).
           05 PROFILE-EXPERIENCES OCCURS 3 TIMES.
               10 PROF-EXP-TITLE PIC X(80).
               10 PROF-EXP-COMPANY PIC X(80).
               10 PROF-EXP-DATES PIC X(80).
               10 PROF-EXP-DESCRIPTION PIC X(80).
           05 PROFILE-EDUCATION OCCURS 3 TIMES.
               10 PROF-EDU-DEGREE PIC X(80).
               10 PROF-EDU-UNIVERSITY PIC X(80).
               10 PROF-EDU-YEARS PIC X(80).

       WORKING-STORAGE SECTION.

       01 YEARS-INPUT            PIC X(20).
       01 YEARS-LEN              PIC 99.
       01 YEARS-SEP              PIC X(1).
       01 YEAR-START             PIC 9(4).
       01 YEAR-END               PIC 9(4).
       01 YEARS-VALID-FLAG       PIC X VALUE 'N'.
          88 YEARS-VALID         VALUE 'Y'.
          88 YEARS-INVALID       VALUE 'N'.

       01 SEC-STATUS   PIC XX VALUE SPACES.
       01 PRO-STATUS   PIC XX VALUE SPACES.

       01 PROGRAM-STATUS.
           05 WS-EXIT-FLAG PIC A(1) VALUE 'N'.
               88 EXIT-PROGRAM VALUE 'Y'.

       01  TO-OUTPUT-BUF PIC X(80).
       01  INPUT-CHOICE-BUF PIC X(1).

       01  USER-RECORDS.
           05  USER-TABLE OCCURS 5 TIMES.
               10 USER-USERNAME PIC X(20).
               10 USER-PASSWORD PIC X(12).

       01 USER-PROFILES.
           05 USER-PROFILES-TABLE OCCURS 5 TIMES.
               10 USER-FIRST-NAME PIC X(80).
               10 USER-LAST-NAME PIC X(80).
               10 USER-UNIVERSITY PIC X(80).
               10 USER-MAJOR PIC X(80).
               10 USER-GRADUATION-YEAR PIC 9(4).
               10 USER-ABOUT-ME PIC X(80).
               10 USER-EXPERIENCES OCCURS 3 TIMES.
                   15 EXP-TITLE PIC X(80).
                   15 EXP-COMPANY PIC X(80).
                   15 EXP-DATES PIC X(80).
                   15 EXP-DESCRIPTION PIC X(80).
               10 USER-EDUCATION OCCURS 3 TIMES.
                   15 EDU-DEGREE PIC X(80).
                   15 EDU-UNIVERSITY PIC X(80).
                   15 EDU-YEARS PIC X(80).

       01  USER-COUNT PIC 9 VALUE 0.

       01  VALIDATION-VARS.
           05 PASSWORD-IS-VALID PIC A(1).
              88 IS-VALID VALUE 'Y'.
              88 IS-NOT-VALID VALUE 'N'.
           05 PASS-LEN PIC 99.
           05 CAPS-COUNT PIC 99.
           05 DIGIT-COUNT PIC 99.
           05 SPECIAL-COUNT PIC 99.
           05 I PIC 99.

       01 LOOP-VARS.
           05 J PIC 99 VALUE 0.
           05 COUNT-EXP PIC 9 VALUE 0.
           05 COUNT-EDU PIC 9 VALUE 0.

       01  TEMP-PASSWORD PIC X(80).

       01  CHAR-ORD PIC 9(3).

       01  LOGIN-VARS.
           05 LOGIN-USERNAME PIC X(20).
           05 LOGIN-PASSWORD PIC X(12).
           05 LOGGED-IN-RANK PIC 9.
           05 LOGIN-FOUND-FLAG PIC A(1).
              88 LOGIN-SUCCESSFUL VALUE 'Y'.

       01  MENU-EXIT-FLAG PIC A(1).
           88 EXIT-MENU VALUE 'Y'.

       01 SKILLS-MENU-EXIT-FLAG PIC A(1).
           88 EXIT-SKILLS-MENU VALUE 'Y'.

       01 SIGNUP-VARS.
           05 SIGNUP-USERNAME PIC X(20).
           05 USERNAME-EXISTS-FLAG PIC A(1).
               88 USERNAME-EXISTS VALUE "Y".
               88 USERNAME-DOESNT-EXIST VALUE "N".

       01 PROFILE-CREATION-FAILURE-FLAG PIC A(1).
           88 EXIT-PROFILE-CREATION VALUE 'Y'.

       01 EXP-SUBS PIC 9.
       01 EDU-SUBS PIC 9.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

           PERFORM LOAD-USERS-FROM-FILE.
           PERFORM LOAD-PROFILES-FROM-FILE.
           PERFORM INITIAL-PROMPT-PROCEDURE.

           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.

*> Loads users from file to read
       LOAD-USERS-FROM-FILE.

           INITIALIZE USER-RECORDS.
           MOVE 0 TO USER-COUNT.

           OPEN INPUT SECRETS-FILE.

           IF SEC-STATUS = "35"
               OPEN OUTPUT SECRETS-FILE
               CLOSE SECRETS-FILE
               OPEN INPUT SECRETS-FILE
               MOVE "00" TO SEC-STATUS
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               READ SECRETS-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       ADD 1 TO USER-COUNT
                       MOVE SECRETS-RECORD TO USER-TABLE(I)
               END-READ
           END-PERFORM.
           CLOSE SECRETS-FILE.

*> Loading Profiles from File to read
       LOAD-PROFILES-FROM-FILE.
           INITIALIZE USER-PROFILES.

           OPEN INPUT PROFILES-FILE.
           IF PRO-STATUS = "35"
               OPEN OUTPUT PROFILES-FILE
               CLOSE PROFILES-FILE
               OPEN INPUT PROFILES-FILE
               MOVE "00" TO PRO-STATUS
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
               READ PROFILES-FILE
                   AT END EXIT PERFORM
                   NOT AT END
                       MOVE PROFILES-RECORD TO USER-PROFILES-TABLE(I)
               END-READ
           END-PERFORM
           CLOSE PROFILES-FILE.


*> Prompts user the inital menu choice
       INITIAL-PROMPT-PROCEDURE.

           MOVE "Welcome to InCollege!:" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           MOVE "1) Log In." TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           MOVE "2) Create New Account" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           MOVE "Enter your choice:" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           PERFORM READ-INPUT-SAFELY.
           IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF.
           MOVE INPUT-RECORD(1:1) TO INPUT-CHOICE-BUF.

           IF INPUT-CHOICE-BUF = "1"
               PERFORM LOGIN-PROCEDURE
           END-IF.
           IF INPUT-CHOICE-BUF = "2"
               PERFORM SIGN-UP-PROCEDURE
           END-IF.
           IF INPUT-CHOICE-BUF NOT = "1" AND INPUT-CHOICE-BUF NOT = "2"
               PERFORM INITIAL-PROMPT-PROCEDURE
           END-IF.

       LOGIN-PROCEDURE.

           MOVE "N" TO LOGIN-FOUND-FLAG.

           MOVE "Please enter your username:" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           PERFORM READ-INPUT-SAFELY.
           IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF.
           MOVE FUNCTION TRIM(INPUT-RECORD) TO LOGIN-USERNAME.

           MOVE "Please enter your password:" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           PERFORM READ-INPUT-SAFELY.
           IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF.
           MOVE FUNCTION TRIM(INPUT-RECORD) TO LOGIN-PASSWORD.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
              IF USER-USERNAME(I) = LOGIN-USERNAME AND
                 USER-PASSWORD(I) = LOGIN-PASSWORD
                   SET LOGIN-SUCCESSFUL TO TRUE
                   MOVE I TO LOGGED-IN-RANK
                   EXIT PERFORM
              END-IF
           END-PERFORM.

           IF LOGIN-SUCCESSFUL

               MOVE "You have successfully logged in." TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM POST-LOGIN-NAVIGATION

           ELSE

               MOVE "Incorrect username/password, please try again."
               TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM INITIAL-PROMPT-PROCEDURE

           END-IF.
*> Performs Username and Password Checks
       SIGN-UP-PROCEDURE.

           IF USER-COUNT >= 5

               MOVE "All permitted accounts have been created, please" &
               " come back later" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM INITIAL-PROMPT-PROCEDURE

           ELSE

               MOVE "Please enter your username:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               MOVE FUNCTION TRIM(INPUT-RECORD) TO SIGNUP-USERNAME

               PERFORM CHECK-USERNAME-EXISTS

               IF USERNAME-EXISTS

                   MOVE "Username already exists. Please try another."
                   TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   PERFORM INITIAL-PROMPT-PROCEDURE

               ELSE

                   MOVE "Please enter your password:" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   PERFORM READ-INPUT-SAFELY
                   IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
                   MOVE INPUT-RECORD TO TEMP-PASSWORD

                   PERFORM VALIDATE-PASSWORD-PROCEDURE

                   IF IS-VALID

                       ADD 1 TO USER-COUNT
                       MOVE SIGNUP-USERNAME TO USER-USERNAME(USER-COUNT)
                       MOVE TEMP-PASSWORD TO USER-PASSWORD(USER-COUNT)

                       MOVE "Account created successfully." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT

                       PERFORM SAVE-USERS-TO-FILE

                       PERFORM INITIAL-PROMPT-PROCEDURE

                   ELSE

                       MOVE "Password does not meet the requirements."
                       TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                       PERFORM INITIAL-PROMPT-PROCEDURE

                   END-IF

               END-IF

           END-IF.

*> Makes sure the username is unique
       CHECK-USERNAME-EXISTS.
           SET USERNAME-DOESNT-EXIST TO TRUE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
               IF USER-USERNAME(I) = SIGNUP-USERNAME
                   SET USERNAME-EXISTS TO TRUE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

*> Checks password to make sure it fits all the requirements
       VALIDATE-PASSWORD-PROCEDURE.
           SET IS-VALID TO TRUE.
           INITIALIZE CAPS-COUNT, DIGIT-COUNT, SPECIAL-COUNT.
           COMPUTE PASS-LEN = FUNCTION LENGTH(FUNCTION TRIM(TEMP-PASSWORD)).
           IF PASS-LEN < 8 OR PASS-LEN > 12
               SET IS-NOT-VALID TO TRUE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PASS-LEN
              MOVE FUNCTION ORD(TEMP-PASSWORD(I:1)) TO CHAR-ORD

              IF CHAR-ORD >= 65 AND CHAR-ORD <= 90
                   ADD 1 TO CAPS-COUNT
              END-IF

              IF CHAR-ORD >= 48 AND CHAR-ORD <= 57
                   ADD 1 TO DIGIT-COUNT
              END-IF

              IF (CHAR-ORD >= 33 AND CHAR-ORD <= 47) OR
                 (CHAR-ORD >= 58 AND CHAR-ORD <= 64) OR
                 (CHAR-ORD >= 91 AND CHAR-ORD <= 96) OR
                 (CHAR-ORD >= 123 AND CHAR-ORD <= 126)
                   ADD 1 TO SPECIAL-COUNT
              END-IF

           END-PERFORM.

           IF CAPS-COUNT = 0 OR DIGIT-COUNT = 0 OR SPECIAL-COUNT = 0
               SET IS-NOT-VALID TO TRUE.

*> Saves created accounts to file
       SAVE-USERS-TO-FILE.
           OPEN OUTPUT SECRETS-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
               MOVE USER-USERNAME(I) TO SECRET-USERNAME
               MOVE USER-PASSWORD(I) TO SECRET-PASSWORD
               WRITE SECRETS-RECORD
           END-PERFORM.
           CLOSE SECRETS-FILE.

*> Saves created profiles to file
       SAVE-PROFILES-TO-FILE.
           OPEN OUTPUT PROFILES-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
               MOVE USER-PROFILES-TABLE(I) TO PROFILES-RECORD
               WRITE PROFILES-RECORD
           END-PERFORM.
           CLOSE PROFILES-FILE.

*> The menu the user is prompted after a sucessful login
       POST-LOGIN-NAVIGATION.

           MOVE "N" TO MENU-EXIT-FLAG.
           PERFORM UNTIL EXIT-MENU

               MOVE "1) Create/Edit My Profile" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "2) View My Profile" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "3) Search for a job" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "4) Find someone you know" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "5) Learn a new skill" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "6) Log Out" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "Enter your choice:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               MOVE INPUT-RECORD(1:1) TO INPUT-CHOICE-BUF

               IF INPUT-CHOICE-BUF = "1"
                   PERFORM CREATE-PROFILE-PROCEDURE
               END-IF

               IF INPUT-CHOICE-BUF = "2"
                   PERFORM VIEW-PROFILE-PROCEDURE
               END-IF

               IF INPUT-CHOICE-BUF = "3" OR INPUT-CHOICE-BUF = "4"
                   MOVE "Under construction." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF

               IF INPUT-CHOICE-BUF = "5"
                   PERFORM SKILLS-MENU-PROCEDURE
               END-IF

               IF INPUT-CHOICE-BUF = "6"
                   SET EXIT-MENU TO TRUE
               END-IF

           END-PERFORM.

*> Skils menu after selecting the skills option
       SKILLS-MENU-PROCEDURE.

           MOVE "N" TO MENU-EXIT-FLAG.
           PERFORM UNTIL EXIT-SKILLS-MENU

               MOVE "1) Advanced COBOL" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "2) JCL Management" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "3) Public Speaking" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "4) Data Analytics" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "5) UX/UI Design" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "6) Go Back" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "Enter your choice:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               MOVE INPUT-RECORD(1:1) TO INPUT-CHOICE-BUF

               IF INPUT-CHOICE-BUF >= "1" AND INPUT-CHOICE-BUF <= "5"
                   MOVE "This skill is under construction." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF

               IF INPUT-CHOICE-BUF = "6"
                   SET EXIT-SKILLS-MENU TO TRUE
               END-IF

           END-PERFORM.


*> Used to create profiles for the user
       CREATE-PROFILE-PROCEDURE.

           MOVE "--- Create/Edit Profile ---" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE "N" TO PROFILE-CREATION-FAILURE-FLAG.

*> All these performs make sure that the users enters a valid input for each field or else it will reprompt them.
           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter First Name:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF INPUT-RECORD = SPACES
                   MOVE "Invalid First Name" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM.
           MOVE FUNCTION TRIM(INPUT-RECORD) TO USER-FIRST-NAME(LOGGED-IN-RANK)

           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter Last Name:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF INPUT-RECORD = SPACES
                   MOVE "Invalid Last Name" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM.
           MOVE FUNCTION TRIM(INPUT-RECORD) TO USER-LAST-NAME(LOGGED-IN-RANK)

           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter University/College attended:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF INPUT-RECORD = SPACES
                   MOVE "Invalid University/College attended" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM
           MOVE FUNCTION TRIM(INPUT-RECORD) TO USER-UNIVERSITY(LOGGED-IN-RANK)

           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter Major:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF INPUT-RECORD = SPACES
                   MOVE "Invalid Major" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM
           MOVE FUNCTION TRIM(INPUT-RECORD) TO USER-MAJOR(LOGGED-IN-RANK)

           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter Graduation Year:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF

               IF INPUT-RECORD = SPACES
                   OR FUNCTION TRIM(INPUT-RECORD) IS NOT NUMERIC
                   OR FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) NOT = 4

                   MOVE "Invalid Graduation Year" TO
                   TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   EXIT PERFORM

               END-IF
           END-PERFORM.
           MOVE FUNCTION TRIM(INPUT-RECORD) TO USER-GRADUATION-YEAR(LOGGED-IN-RANK)

           MOVE "Enter About Me (Optional):" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT
           PERFORM READ-INPUT-SAFELY
           IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
           IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 0
               MOVE SPACES to USER-ABOUT-ME(LOGGED-IN-RANK)
           ELSE
               MOVE FUNCTION TRIM(INPUT-RECORD) TO USER-ABOUT-ME(LOGGED-IN-RANK)
           END-IF

           PERFORM SAVE-PROFILES-TO-FILE
           PERFORM EDIT-EXPERIENCES-PROCEDURE
           PERFORM SAVE-PROFILES-TO-FILE
           PERFORM EDIT-EDUCATION-PROCEDURE
           PERFORM SAVE-PROFILES-TO-FILE
           .
*> Function used to view profile
       VIEW-PROFILE-PROCEDURE.
           MOVE "--- Your Profile ---" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF.
           STRING "Name: " DELIMITED BY SIZE
               USER-FIRST-NAME(LOGGED-IN-RANK) DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               USER-LAST-NAME(LOGGED-IN-RANK) DELIMITED BY SPACE
               INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF.
           STRING "University: " DELIMITED BY SIZE
               USER-UNIVERSITY(LOGGED-IN-RANK) DELIMITED BY SIZE
               INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF.
           STRING "Major: " DELIMITED BY SIZE
               USER-MAJOR(LOGGED-IN-RANK) DELIMITED BY SIZE
               INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF.
           STRING "Graduation Year: " DELIMITED BY SIZE
               USER-GRADUATION-YEAR(LOGGED-IN-RANK) DELIMITED BY SPACE
               INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

*> Checks if there was any input for About Me, if no then don't print

           IF FUNCTION LENGTH(FUNCTION TRIM(USER-ABOUT-ME(LOGGED-IN-RANK))) > 0
               MOVE SPACES TO TO-OUTPUT-BUF
               STRING "About Me: " DELIMITED BY SIZE
                   FUNCTION TRIM(USER-ABOUT-ME(LOGGED-IN-RANK)) DELIMITED BY SIZE
                   INTO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-IF


*> Prints out all experiences

           PERFORM VARYING EXP-SUBS FROM 1 BY 1 UNTIL EXP-SUBS > 3

               IF EXP-TITLE(LOGGED-IN-RANK, EXP-SUBS) NOT = SPACES

                   MOVE "Experience:" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   STRING "    Title: " DELIMITED BY SIZE
                       EXP-TITLE(LOGGED-IN-RANK, EXP-SUBS) DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   MOVE SPACES TO TO-OUTPUT-BUF
                   STRING "    Company: " DELIMITED BY SIZE
                       EXP-COMPANY(LOGGED-IN-RANK, EXP-SUBS) DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   MOVE SPACES TO TO-OUTPUT-BUF
                   STRING "    Dates: " DELIMITED BY SIZE
                       EXP-DATES(LOGGED-IN-RANK, EXP-SUBS) DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   MOVE SPACES TO TO-OUTPUT-BUF
                   STRING "    Description: " DELIMITED BY SIZE
                       EXP-DESCRIPTION(LOGGED-IN-RANK, EXP-SUBS)
                       DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

               END-IF

           END-PERFORM.

*> Prints out all educations

           PERFORM VARYING EDU-SUBS FROM 1 BY 1 UNTIL EDU-SUBS > 3

               IF EDU-DEGREE(LOGGED-IN-RANK, EDU-SUBS) NOT = SPACES

                   MOVE "Education:" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   STRING "    Degree: " DELIMITED BY SIZE
                       EDU-DEGREE(LOGGED-IN-RANK, EDU-SUBS) DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   MOVE SPACES TO TO-OUTPUT-BUF
                   STRING "    University: " DELIMITED BY SIZE
                       EDU-UNIVERSITY(LOGGED-IN-RANK, EDU-SUBS)
                       DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   MOVE SPACES TO TO-OUTPUT-BUF
                   STRING "    Years: " DELIMITED BY SIZE
                       EDU-YEARS(LOGGED-IN-RANK, EDU-SUBS) DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF

           END-PERFORM.

           MOVE "--------------------" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

*> Function used to create and edit experiences
       EDIT-EXPERIENCES-PROCEDURE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               MOVE SPACES TO EXP-TITLE      (LOGGED-IN-RANK, J)
               MOVE SPACES TO EXP-COMPANY    (LOGGED-IN-RANK, J)
               MOVE SPACES TO EXP-DATES      (LOGGED-IN-RANK, J)
               MOVE SPACES TO EXP-DESCRIPTION(LOGGED-IN-RANK, J)
           END-PERFORM.

           MOVE 0 TO COUNT-EXP.

           PERFORM UNTIL COUNT-EXP = 3
               MOVE "Add a work experience? (Y/N):" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF FUNCTION UPPER-CASE(INPUT-RECORD(1:1)) NOT = "Y"
                   EXIT PERFORM
               END-IF

               ADD 1 TO COUNT-EXP
               MOVE COUNT-EXP TO J

*> All these perform until asks the user to input a valid input or else it reprompts

               PERFORM WITH TEST AFTER
                       UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
                   MOVE "  Title (required):" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   PERFORM READ-INPUT-SAFELY
                   IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
                   IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 0
                       MOVE "  Title cannot be blank." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
               END-PERFORM

               MOVE FUNCTION TRIM(INPUT-RECORD)
                    TO EXP-TITLE(LOGGED-IN-RANK, J)

               PERFORM WITH TEST AFTER
                       UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
                   MOVE "  Company (required):" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   PERFORM READ-INPUT-SAFELY
                   IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
                   IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 0
                       MOVE "  Company cannot be blank." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
               END-PERFORM

               MOVE FUNCTION TRIM(INPUT-RECORD)
                    TO EXP-COMPANY(LOGGED-IN-RANK, J)


               PERFORM WITH TEST AFTER
                       UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
                   MOVE "  Dates (required, e.g., Summer 2024):" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   PERFORM READ-INPUT-SAFELY
                   IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
                   IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 0
                       MOVE "  Dates cannot be blank." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
               END-PERFORM

               MOVE FUNCTION TRIM(INPUT-RECORD)
                    TO EXP-DATES(LOGGED-IN-RANK, J)

               MOVE "  Description (optional):" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 0
                   MOVE SPACES TO EXP-DESCRIPTION(LOGGED-IN-RANK, J)
               ELSE
                   MOVE FUNCTION TRIM(INPUT-RECORD)
                       TO EXP-DESCRIPTION(LOGGED-IN-RANK, J)
               END-IF
           END-PERFORM.
           EXIT PARAGRAPH.

*> Function used to create and edit education
       EDIT-EDUCATION-PROCEDURE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               MOVE SPACES TO EDU-DEGREE    (LOGGED-IN-RANK, J)
               MOVE SPACES TO EDU-UNIVERSITY(LOGGED-IN-RANK, J)
               MOVE SPACES TO EDU-YEARS     (LOGGED-IN-RANK, J)
           END-PERFORM.

           MOVE 0 TO COUNT-EDU.

           PERFORM UNTIL COUNT-EDU = 3
               MOVE "Add an education entry? (Y/N):" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF FUNCTION UPPER-CASE(INPUT-RECORD(1:1)) NOT = "Y"
                   EXIT PERFORM
               END-IF

               ADD 1 TO COUNT-EDU
               MOVE COUNT-EDU TO J

*> All these perform until asks the user to input a valid input or else it reprompts

                PERFORM WITH TEST AFTER
                       UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
                   MOVE "  Degree (e.g., B.S. Computer Engineering):" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   PERFORM READ-INPUT-SAFELY
                   IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
                   IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 0
                       MOVE "  Degree cannot be blank." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
               END-PERFORM

               MOVE FUNCTION TRIM(INPUT-RECORD)
                    TO EDU-DEGREE(LOGGED-IN-RANK, J)

               PERFORM WITH TEST AFTER
                       UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
                   MOVE "  University:" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   PERFORM READ-INPUT-SAFELY
                   IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
                   IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 0
                       MOVE "  University cannot be blank." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
               END-PERFORM

               MOVE FUNCTION TRIM(INPUT-RECORD)
                    TO EDU-UNIVERSITY(LOGGED-IN-RANK, J)

               MOVE 'N' TO YEARS-VALID-FLAG
               PERFORM WITH TEST AFTER UNTIL YEARS-VALID
                   MOVE "  Years (e.g., 2021–2025):" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   PERFORM READ-INPUT-SAFELY
                   IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF

                   MOVE FUNCTION TRIM(INPUT-RECORD) TO YEARS-INPUT
                   MOVE FUNCTION LENGTH(FUNCTION TRIM(YEARS-INPUT)) TO YEARS-LEN

                   IF (YEARS-LEN = 9) AND (YEARS-INPUT(1:4) IS NUMERIC) AND (YEARS-INPUT(6:4) IS NUMERIC) AND (YEARS-INPUT(5:1) = "-")
                           MOVE YEARS-INPUT(1:4) TO YEAR-START
                           MOVE YEARS-INPUT(6:4) TO YEAR-END
                       IF YEAR-START <= YEAR-END
                           SET YEARS-VALID TO TRUE
                       END-IF
                    END-IF

                    IF YEARS-INVALID
                       MOVE "  Please enter years as YYYY-YYYY(e.g, 2021-2025)." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
               END-PERFORM

               MOVE YEARS-INPUT TO EDU-YEARS(LOGGED-IN-RANK, J)

           END-PERFORM.
           EXIT PARAGRAPH.

       DISPLAY-AND-WRITE-OUTPUT.
           DISPLAY TO-OUTPUT-BUF.
           MOVE TO-OUTPUT-BUF TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

       READ-INPUT-SAFELY.
           READ INPUT-FILE
               AT END
                   SET EXIT-PROGRAM TO TRUE
           END-READ.

       EXIT-EARLY.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.
