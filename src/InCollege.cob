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
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROFILES-FILE ASSIGN TO "profiles.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
      
       DATA DIVISION.
           
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD           PIC X(80).
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD          PIC X(80).
       FD  SECRETS-FILE.
       01  SECRETS-RECORD.
           05 SECRET-USERNAME     PIC X(20).
           05 SECRET-PASSWORD     PIC X(12).

       WORKING-STORAGE SECTION.

       01 PROGRAM-STATUS.
           05 WS-EXIT-FLAG PIC A(1) VALUE 'N'.
               88 EXIT-PROGRAM VALUE 'Y'.

       01  TO-OUTPUT-BUF          PIC X(80).
       01  INPUT-CHOICE-BUF       PIC X(1).

       01  USER-RECORDS.
           05  USER-TABLE OCCURS 5 TIMES.
               10 USER-USERNAME   PIC X(20).
               10 USER-PASSWORD   PIC X(12).

       01 USER-PROFILES.
           05 USER-PROFILES-TABLE OCCURS 5 TIMES.
               10 USER-FIRST-NAME PIC X(80).
               10 USER-LAST-NAME PIC X(80).
               10 USER-UNIVERSITY PIC X(80).
               10 USER-MAJOR PIC X(80).
               10 USER-GRADUATION-YEAR PIC 9(4).
               10 USER-ABOUT-ME PIC X(80).

       01  USER-COUNT             PIC 9 VALUE 0.
       01  WS-EOF-FLAG            PIC A(1) VALUE 'N'.
           88 END-OF-SECRETS-FILE VALUE 'Y'.

       01  VALIDATION-VARS.
           05 PASSWORD-IS-VALID   PIC A(1).
              88 IS-VALID         VALUE 'Y'.
              88 IS-NOT-VALID     VALUE 'N'.
           05 PASS-LEN            PIC 99.
           05 CAPS-COUNT          PIC 99.
           05 DIGIT-COUNT         PIC 99.
           05 SPECIAL-COUNT       PIC 99.
           05 I                   PIC 99.

       01  TEMP-PASSWORD          PIC X(80).

       01  LOGIN-VARS.
           05 LOGIN-USERNAME      PIC X(20).
           05 LOGIN-PASSWORD      PIC X(12).
           05 LOGIN-FOUND-FLAG    PIC A(1).
              88 LOGIN-SUCCESSFUL VALUE 'Y'.

       01  MENU-EXIT-FLAG         PIC A(1).
           88 EXIT-MENU           VALUE 'Y'.

       01 SKILLS-MENU-EXIT-FLAG PIC A(1).
           88 EXIT-SKILLS-MENU VALUE 'Y'.

       01 SIGNUP-VARS.
           05 SIGNUP-USERNAME PIC X(20).
           05 USERNAME-EXISTS-FLAG PIC A(1).
               88 USERNAME-EXISTS VALUE "Y".
               88 USERNAME-DOESNT-EXIST VALUE "N".

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
      
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
      
           PERFORM LOAD-USERS-FROM-FILE.
           PERFORM INITIAL-PROMPT-PROCEDURE.
      
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.

       READ-INPUT-SAFELY.
           READ INPUT-FILE
               AT END
                   SET EXIT-PROGRAM TO TRUE
           END-READ.

       EXIT-EARLY.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.

       LOAD-USERS-FROM-FILE.
      
           OPEN INPUT SECRETS-FILE.
      
           INITIALIZE USER-RECORDS.
           MOVE 0 TO USER-COUNT.
           MOVE "N" TO WS-EOF-FLAG.
           PERFORM UNTIL END-OF-SECRETS-FILE
               READ SECRETS-FILE
                   AT END
                       SET END-OF-SECRETS-FILE TO TRUE
                   NOT AT END
                       IF USER-COUNT < 5
                           ADD 1 TO USER-COUNT
                           MOVE SECRET-USERNAME TO
                               USER-USERNAME(USER-COUNT)
                           MOVE SECRET-PASSWORD TO
                               USER-PASSWORD(USER-COUNT)
                       END-IF
               END-READ
           END-PERFORM.
      
           CLOSE SECRETS-FILE.

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
               PERFORM LOGIN-PROCEDURE.
           IF INPUT-CHOICE-BUF = "2"
               PERFORM SIGN-UP-PROCEDURE.

       LOGIN-PROCEDURE.
      
           MOVE "N" TO LOGIN-FOUND-FLAG.
      
           MOVE "Please enter your username:" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           PERFORM READ-INPUT-SAFELY.
           IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF.
           MOVE INPUT-RECORD TO LOGIN-USERNAME.

           MOVE "Please enter your password:" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           PERFORM READ-INPUT-SAFELY.
           IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF.
           MOVE INPUT-RECORD TO LOGIN-PASSWORD.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
              IF USER-USERNAME(I) = LOGIN-USERNAME AND
                 USER-PASSWORD(I) = LOGIN-PASSWORD
                   SET LOGIN-SUCCESSFUL TO TRUE
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
               MOVE INPUT-RECORD TO SIGNUP-USERNAME

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

       CHECK-USERNAME-EXISTS.
           SET USERNAME-DOESNT-EXIST TO TRUE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
               IF USER-USERNAME(I) = SIGNUP-USERNAME
                   SET USERNAME-EXISTS TO TRUE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       VALIDATE-PASSWORD-PROCEDURE.
      
           SET IS-VALID TO TRUE.
      
           INITIALIZE CAPS-COUNT, DIGIT-COUNT, SPECIAL-COUNT.
      
           COMPUTE PASS-LEN = FUNCTION LENGTH(
               FUNCTION TRIM(TEMP-PASSWORD)).

           IF PASS-LEN < 8 OR PASS-LEN > 12
               SET IS-NOT-VALID TO TRUE.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PASS-LEN
      
              IF TEMP-PASSWORD(I:1) >= "A" AND
                 TEMP-PASSWORD(I:1) <= "Z"
                   ADD 1 TO CAPS-COUNT
              END-IF
      
              IF TEMP-PASSWORD(I:1) >= "0" AND
                 TEMP-PASSWORD(I:1) <= "9"
                   ADD 1 TO DIGIT-COUNT
              END-IF
      
              IF TEMP-PASSWORD(I:1) = "!" OR
                 TEMP-PASSWORD(I:1) = "@" OR
                 TEMP-PASSWORD(I:1) = "#" OR
                 TEMP-PASSWORD(I:1) = "$" OR
                 TEMP-PASSWORD(I:1) = "%" OR
                 TEMP-PASSWORD(I:1) = "^" OR
                 TEMP-PASSWORD(I:1) = "&" OR
                 TEMP-PASSWORD(I:1) = "*"
                   ADD 1 TO SPECIAL-COUNT
              END-IF
      
           END-PERFORM.

           IF CAPS-COUNT = 0 OR DIGIT-COUNT = 0 OR SPECIAL-COUNT = 0
               SET IS-NOT-VALID TO TRUE.

       SAVE-USERS-TO-FILE.
           OPEN OUTPUT SECRETS-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
               MOVE USER-USERNAME(I) TO SECRET-USERNAME
               MOVE USER-PASSWORD(I) TO SECRET-PASSWORD
               WRITE SECRETS-RECORD
           END-PERFORM.
           CLOSE SECRETS-FILE.

       POST-LOGIN-NAVIGATION.
           MOVE "N" TO MENU-EXIT-FLAG.
           PERFORM UNTIL EXIT-MENU
               MOVE "1) Search for a job" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "2) Find someone you know" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "3) Learn a new skill" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "4) Log Out" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "Enter your choice:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               MOVE INPUT-RECORD(1:1) TO INPUT-CHOICE-BUF
      
               IF INPUT-CHOICE-BUF = "1" OR INPUT-CHOICE-BUF = "2"
                   MOVE "Under construction." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
               IF INPUT-CHOICE-BUF = "3"
                   PERFORM SKILLS-MENU-PROCEDURE
               END-IF
               IF INPUT-CHOICE-BUF = "4"
                   SET EXIT-MENU TO TRUE
               END-IF
           END-PERFORM.

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

       DISPLAY-AND-WRITE-OUTPUT.
           DISPLAY TO-OUTPUT-BUF.
           MOVE TO-OUTPUT-BUF TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
