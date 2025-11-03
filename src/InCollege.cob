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
           SELECT CONNECTIONS-FILE ASSIGN TO "connections.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS CONN-FILE-STATUS.
           SELECT NETWORKS-FILE ASSIGN TO "networks.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS NET-FILE-STATUS.
           SELECT JOBS-FILE ASSIGN TO "jobs.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS JOBS-FILE-STATUS.
           SELECT APPLICATIONS-FILE ASSIGN TO "applications.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS APP-FILE-STATUS.
           SELECT MESSAGES-FILE ASSIGN TO "messages.txt"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS MESSAGES-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD PIC X(256).
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD PIC X(256).
       FD  SECRETS-FILE.
       01  SECRETS-RECORD.
           05 SECRET-USERNAME PIC X(20).
           05 SECRET-PASSWORD PIC X(12).
       FD  PROFILES-FILE.
       01  PROFILES-RECORD.
           05 PROFILE-FIRST-NAME PIC X(80).
           05 PROFILE-LAST-NAME PIC X(80).
           05 PROFILE-UNIVERSITY PIC X(80).
           05 PROFILE-MAJOR PIC X(80).
           05 PROFILE-GRADUATION-YEAR PIC 9(4).
           05 PROFILE-ABOUT-ME PIC X(200).
           05 PROFILE-EXPERIENCES OCCURS 3 TIMES.
               10 PROF-EXP-TITLE PIC X(80).
               10 PROF-EXP-COMPANY PIC X(80).
               10 PROF-EXP-DATES PIC X(80).
               10 PROF-EXP-DESCRIPTION PIC X(100).
           05 PROFILE-EDUCATION OCCURS 3 TIMES.
               10 PROF-EDU-DEGREE PIC X(80).
               10 PROF-EDU-UNIVERSITY PIC X(80).
               10 PROF-EDU-YEARS PIC X(80).

       FD  CONNECTIONS-FILE.
       01  CONNECTIONS-RECORD.
           05 SENDER-USERNAME PIC X(20).
           05 RECEIVER-USERNAME PIC X(20).
           05 CONN-STATUS PIC X(20).

       FD  NETWORKS-FILE.
       01  NETWORKS-RECORD.
           05 NETWORKS-SENDER PIC X(20).
           05 NETWORKS-RECIEVER PIC X(20).

       FD  JOBS-FILE.
       01  JOBS-RECORD.
           05 JOB-POSTER        PIC X(20).
           05 JOB-TITLE         PIC X(80).
           05 JOB-DESCRIPTION   PIC X(200).
           05 JOB-EMPLOYER      PIC X(80).
           05 JOB-LOCATION      PIC X(80).
           05 JOB-SALARY        PIC X(20).

       FD  APPLICATIONS-FILE.
       01  APPLICATIONS-RECORD.
           05 APP-USERNAME      PIC X(20).
           05 APP-JOB-TITLE     PIC X(80).
           05 APP-JOB-EMPLOYER  PIC X(80).
           05 APP-JOB-LOCATION  PIC X(80).

       FD  MESSAGES-FILE.
       01  MESSAGES-RECORD.
           05 MSG-SENDER           PIC X(20).
           05 MSG-RECIPIENT        PIC X(20).
           05 MSG-CONTENT          PIC X(200).
           05 MSG-TIMESTAMP        PIC X(19).
        

       WORKING-STORAGE SECTION.

       01  YEARS-INPUT            PIC X(20).
       01  YEARS-LEN              PIC 99.
       01  YEARS-SEP              PIC X(1).
       01  YEAR-START             PIC 9(4).
       01  YEAR-END               PIC 9(4).
       01  YEARS-VALID-FLAG       PIC X VALUE 'N'.
           88 YEARS-VALID         VALUE 'Y'.
           88 YEARS-INVALID       VALUE 'N'.

       01  SEC-STATUS   PIC XX VALUE SPACES.
       01  PRO-STATUS   PIC XX VALUE SPACES.

       01  PROGRAM-STATUS.
           05 WS-EXIT-FLAG PIC A(1) VALUE 'N'.
               88 EXIT-PROGRAM VALUE 'Y'.

       01  TO-OUTPUT-BUF PIC X(256).
       01  INPUT-CHOICE-BUF PIC X(1).

       01  USER-RECORDS.
           05  USER-TABLE OCCURS 5 TIMES.
               10 USER-USERNAME PIC X(20).
               10 USER-PASSWORD PIC X(12).

       01  USER-PROFILES.
           05 USER-PROFILES-TABLE OCCURS 5 TIMES.
               10 USER-FIRST-NAME PIC X(80).
               10 USER-LAST-NAME PIC X(80).
               10 USER-UNIVERSITY PIC X(80).
               10 USER-MAJOR PIC X(80).
               10 USER-GRADUATION-YEAR PIC 9(4).
               10 USER-ABOUT-ME PIC X(200).
               10 USER-EXPERIENCES OCCURS 3 TIMES.
                   15 EXP-TITLE PIC X(80).
                   15 EXP-COMPANY PIC X(80).
                   15 EXP-DATES PIC X(80).
                   15 EXP-DESCRIPTION PIC X(100).
               10 USER-EDUCATION OCCURS 3 TIMES.
                   15 EDU-DEGREE PIC X(80).
                   15 EDU-UNIVERSITY PIC X(80).
                   15 EDU-YEARS PIC X(80).

       01  CONNECTIONS-DATA.
           05 CONNECTIONS-TABLE OCCURS 25 TIMES.
               10 CON-SENDER PIC X(20).
               10 CON-RECEIVER PIC X(20).
       01 CONNECTION-COUNT PIC 99 VALUE 0.

       01  NETWORK-DATA.
           05 NETWORK-COUNT PIC 99 VALUE 0.
           05 NETWORKS-TABLE OCCURS 100 TIMES.
               10 NETWORK-USER1 PIC X(20).
               10 NETWORK-USER2 PIC X(20).

       01  WS-JOBS-DATA.
           05 WS-JOB-COUNT PIC 99 VALUE 0.
           05 WS-JOBS-TABLE OCCURS 100 TIMES.
               10 WS-JOB-POSTER      PIC X(20).
               10 WS-JOB-TITLE       PIC X(80).
               10 WS-JOB-DESCRIPTION PIC X(200).
               10 WS-JOB-EMPLOYER    PIC X(80).
               10 WS-JOB-LOCATION    PIC X(80).
               10 WS-JOB-SALARY      PIC X(20).

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

       01  LOGIN-VARS.
           05 LOGIN-USERNAME PIC X(20).
           05 LOGIN-PASSWORD PIC X(12).
           05 LOGGED-IN-RANK PIC 9.
           05 LOGIN-FOUND-FLAG PIC A(1).
              88 LOGIN-SUCCESSFUL VALUE 'Y'.

       01  GRAD-YEAR-FLAG PIC X VALUE 'N'.
           88 GRAD-YEAR-SUCESSFUL VALUE 'Y'.

       01  MENU-EXIT-FLAG PIC A(1).
           88 EXIT-MENU VALUE 'Y'.

       01  SKILLS-MENU-EXIT-FLAG PIC A(1).
           88 EXIT-SKILLS-MENU VALUE 'Y'.

       01  SIGNUP-VARS.
           05 SIGNUP-USERNAME PIC X(20).
           05 USERNAME-EXISTS-FLAG PIC A(1).
               88 USERNAME-EXISTS VALUE "Y".
               88 USERNAME-DOESNT-EXIST VALUE "N".

       01  PROFILE-CREATION-FAILURE-FLAG PIC A(1).
           88 EXIT-PROFILE-CREATION VALUE 'Y'.

       01  EXP-SUBS         PIC 9(2) VALUE 0.
       01  EDU-SUBS         PIC 9(2) VALUE 0.


       01  WS-EOF-FLAG       PIC X VALUE "N".
           88 END-OF-FILE         VALUE "Y".
           88 NOT-END-OF-FILE     VALUE "N".

       01  CONNECTION-EXIST-FLAG PIC X VALUE 'N'.

       01  NETWORK-EXIST-FLAG PIC X VALUE 'N'.

       01  FULL-NAME              PIC X(50).
       01  SEARCH-NAME            PIC X(50).
       01  PROFILE-INDEX          PIC 9(3) VALUE 0.

       01  PROFILE-HEADING    PIC X(30).

       01  CONN-FILE-STATUS   PIC XX VALUE SPACES.
       01  REQUEST-SUCCESS   PIC X VALUE "N".
       01  NET-FILE-STATUS PIC XX VALUE SPACES.
       01  JOBS-FILE-STATUS PIC XX VALUE SPACES.
       01  APP-FILE-STATUS  PIC XX VALUE SPACES.

       01  WS-APPLICATIONS-DATA.
           05 WS-APP-COUNT      PIC 99 VALUE 0.
           05 WS-APPLICATIONS-TABLE OCCURS 100 TIMES.
               10 WS-APP-USERNAME      PIC X(20).
               10 WS-APP-JOB-TITLE     PIC X(80).
               10 WS-APP-JOB-EMPLOYER  PIC X(80).
               10 WS-APP-JOB-LOCATION  PIC X(80).

       01  WS-JOB-CHOICE       PIC 99.

       01  MESSAGES-FILE-STATUS    PIC XX VALUE SPACES.
       01  WS-MESSAGES-DATA.
           05 WS-MESSAGES-TABLE OCCURS 50 TIMES.
               10 WS-MSG-SENDER           PIC X(20).
               10 WS-MSG-RECIPIENT        PIC X(20).
               10 WS-MSG-CONTENT          PIC X(200).
               10 WS-MSG-TIMESTAMP        PIC X(19).
       01  MESSAGE-COUNT           PIC 99 VALUE 0.
       01  RECIPIENT-USERNAME      PIC X(20).
       01  MESSAGE-CONTENT         PIC X(200).
       01  CONNECTION-VALID-FLAG   PIC X VALUE 'N'.
       01  MESSAGES-MENU-EXIT-FLAG PIC X VALUE 'N'.

       01 CONNECTION-INDEX         PIC 99 VALUE 0.
       01 CURRENT-USER            PIC X(20).

       01  WS-CONNECTIONS-DATA.
           05 WS-CONNECTIONS-TABLE OCCURS 25 TIMES.
               10 WS-CONN-SENDER PIC X(20).
               10 WS-CONN-RECEIVER PIC X(20).
               10 WS-CONN-STATUS PIC X(20).


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

           PERFORM LOAD-USERS-FROM-FILE.
           PERFORM LOAD-PROFILES-FROM-FILE.
           PERFORM LOAD-CONNECTIONS-FROM-FILE.
           PERFORM LOAD-JOBS-FROM-FILE.
           PERFORM LOAD-APPLICATIONS-FROM-FILE.
           PERFORM LOAD-MESSAGES-FROM-FILE.
           PERFORM INITIAL-PROMPT-PROCEDURE.

           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.


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


       LOAD-CONNECTIONS-FROM-FILE.
           INITIALIZE CONNECTIONS-DATA.
           OPEN INPUT CONNECTIONS-FILE.

           IF CONN-FILE-STATUS = "35"
               OPEN OUTPUT CONNECTIONS-FILE
               CLOSE CONNECTIONS-FILE
               OPEN INPUT CONNECTIONS-FILE
           END-iF

           SET NOT-END-OF-FILE TO TRUE.

           PERFORM UNTIL END-OF-FILE
               READ CONNECTIONS-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO CONNECTION-COUNT
                       MOVE SENDER-USERNAME TO WS-CONN-SENDER(CONNECTION-COUNT)
                       MOVE RECEIVER-USERNAME TO WS-CONN-RECEIVER(CONNECTION-COUNT)
                       MOVE CONN-STATUS TO WS-CONN-STATUS(CONNECTION-COUNT)
               END-READ
           END-PERFORM

           CLOSE CONNECTIONS-FILE.


       LOAD-NETWORKS-FROM-FILE.
           INITIALIZE NETWORK-DATA.
           OPEN INPUT NETWORKS-FILE.

           IF NET-FILE-STATUS = "35"
               OPEN OUTPUT NETWORKS-FILE
               CLOSE NETWORKS-FILE
               EXIT PARAGRAPH
           END-IF.

           SET NOT-END-OF-FILE TO TRUE.

           PERFORM UNTIL END-OF-FILE
               READ NETWORKS-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO NETWORK-COUNT
                       MOVE NETWORKS-RECORD TO NETWORKS-TABLE(NETWORK-COUNT)
               END-READ
           END-PERFORM.

           CLOSE NETWORKS-FILE.

       LOAD-JOBS-FROM-FILE.
           INITIALIZE WS-JOBS-DATA.
           OPEN INPUT JOBS-FILE.
           IF JOBS-FILE-STATUS = "35"
               OPEN OUTPUT JOBS-FILE
               CLOSE JOBS-FILE
               OPEN INPUT JOBS-FILE
               MOVE "00" TO JOBS-FILE-STATUS
           END-IF.
           SET NOT-END-OF-FILE TO TRUE.
           PERFORM UNTIL END-OF-FILE
               READ JOBS-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-JOB-COUNT
                       MOVE JOBS-RECORD TO WS-JOBS-TABLE(WS-JOB-COUNT)
               END-READ
           END-PERFORM.
           CLOSE JOBS-FILE.

        LOAD-APPLICATIONS-FROM-FILE.
           INITIALIZE WS-APPLICATIONS-DATA.
           OPEN INPUT APPLICATIONS-FILE.
           IF APP-FILE-STATUS = "35"
               OPEN OUTPUT APPLICATIONS-FILE
               CLOSE APPLICATIONS-FILE
               OPEN INPUT APPLICATIONS-FILE
               MOVE "00" TO APP-FILE-STATUS
           END-IF.
           SET NOT-END-OF-FILE TO TRUE.
           PERFORM UNTIL END-OF-FILE
               READ APPLICATIONS-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-APP-COUNT
                       MOVE APPLICATIONS-RECORD
                       TO WS-APPLICATIONS-TABLE(WS-APP-COUNT)
                   END-READ
               END-PERFORM.
           CLOSE APPLICATIONS-FILE.

       SAVE-USERS-TO-FILE.
           OPEN OUTPUT SECRETS-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
               MOVE USER-USERNAME(I) TO SECRET-USERNAME
               MOVE USER-PASSWORD(I) TO SECRET-PASSWORD
               WRITE SECRETS-RECORD
           END-PERFORM.
           CLOSE SECRETS-FILE.


       SAVE-PROFILES-TO-FILE.
           OPEN OUTPUT PROFILES-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT
               MOVE USER-PROFILES-TABLE(I) TO PROFILES-RECORD
               WRITE PROFILES-RECORD
           END-PERFORM.
           CLOSE PROFILES-FILE.

       SAVE-CONNECTIONS-TO-FILE.
           OPEN OUTPUT CONNECTIONS-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONNECTION-COUNT
               MOVE CON-SENDER(I) TO SENDER-USERNAME
               MOVE CON-RECEIVER(I) TO RECEIVER-USERNAME
               WRITE CONNECTIONS-RECORD
           END-PERFORM.
           CLOSE CONNECTIONS-FILE.

       SAVE-JOBS-TO-FILE.
           OPEN OUTPUT JOBS-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-JOB-COUNT
               MOVE WS-JOBS-TABLE(I) TO JOBS-RECORD
               WRITE JOBS-RECORD
           END-PERFORM.
           CLOSE JOBS-FILE.


       INITIAL-PROMPT-PROCEDURE.

           MOVE "Welcome to InCollege!" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           MOVE "1) Log In" TO TO-OUTPUT-BUF.
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
               MOVE LOGIN-USERNAME TO CURRENT-USER
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
               " come back later." TO TO-OUTPUT-BUF
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
           COMPUTE PASS-LEN = FUNCTION LENGTH(FUNCTION TRIM(TEMP-PASSWORD)).
           IF PASS-LEN < 8 OR PASS-LEN > 12
               SET IS-NOT-VALID TO TRUE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PASS-LEN

              IF TEMP-PASSWORD(I:1) >= "A" AND TEMP-PASSWORD(I:1) <= "Z"
                   ADD 1 TO CAPS-COUNT
              END-IF

              IF TEMP-PASSWORD(I:1) >= "0"AND TEMP-PASSWORD(I:1) <= "9"
                   ADD 1 TO DIGIT-COUNT
              END-IF

              IF (TEMP-PASSWORD(I:1) >= "!") AND (TEMP-PASSWORD(I:1) <= "/") OR
                 (TEMP-PASSWORD(I:1) >= ":") AND (TEMP-PASSWORD(I:1) <= "@") OR
                 (TEMP-PASSWORD(I:1) >= "[") AND (TEMP-PASSWORD(I:1) <= "`") OR
                 (TEMP-PASSWORD(I:1) >= "{") AND (TEMP-PASSWORD(I:1) <= "~")
                   ADD 1 TO SPECIAL-COUNT
              END-IF

           END-PERFORM.

           IF CAPS-COUNT = 0 OR DIGIT-COUNT = 0 OR SPECIAL-COUNT = 0
               SET IS-NOT-VALID TO TRUE.


       POST-LOGIN-NAVIGATION.

           MOVE "N" TO MENU-EXIT-FLAG.
           PERFORM UNTIL EXIT-MENU

               MOVE "1) Create/Edit My Profile" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "2) View My Profile" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "3) Search For a Job" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "4) Find Someone You Know" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "5) Learn a New Skill" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "6) View My Pending Connection Requests" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "7) View My Network" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "8) Messages" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "9) Log Out" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "Enter your choice:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               MOVE INPUT-RECORD(1:1) TO INPUT-CHOICE-BUF

               IF FUNCTION TRIM(INPUT-CHOICE-BUF) = "1"
                   PERFORM CREATE-PROFILE-PROCEDURE
               END-IF

               IF FUNCTION TRIM(INPUT-CHOICE-BUF) = "2"
                   MOVE "--- Your Profile ---" TO PROFILE-HEADING
                   PERFORM VIEW-PROFILE-PROCEDURE

               END-IF

               IF FUNCTION TRIM(INPUT-CHOICE-BUF) = "3"
                   PERFORM JOB-SEARCH-MENU
               END-IF

               IF FUNCTION TRIM(INPUT-CHOICE-BUF) = "4"
                   PERFORM FIND-SOMEONE-PROCEDURE
               END-IF


               IF FUNCTION TRIM(INPUT-CHOICE-BUF) = "5"
                   PERFORM SKILLS-MENU-PROCEDURE
               END-IF

               IF FUNCTION TRIM(INPUT-CHOICE-BUF) = "6"
                   PERFORM PENDING-REQUESTS-PROCEDURE
               END-IF

               IF FUNCTION TRIM(INPUT-CHOICE-BUF) = "7"
                   PERFORM VIEW-NETWORK-PROCEDURE
               END-IF

               IF FUNCTION TRIM(INPUT-CHOICE-BUF) = "8"
                   PERFORM MESSAGES-MENU-PROCEDURE
               END-IF

               IF FUNCTION TRIM(INPUT-CHOICE-BUF) = "9"
                   SET EXIT-MENU TO TRUE
               END-IF

           END-PERFORM.

       JOB-SEARCH-MENU.
           MOVE "N" TO MENU-EXIT-FLAG.
           PERFORM UNTIL EXIT-MENU
               MOVE "--- Job Search/Internship Menu ---" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "1) Post a Job/Internship" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "2) Browse Jobs/Internships" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "3) View My Applications" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "4) Back to Main Menu" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "Enter your choice:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               MOVE INPUT-RECORD(1:1) TO INPUT-CHOICE-BUF

               EVALUATE FUNCTION TRIM(INPUT-CHOICE-BUF)
                   WHEN "1"
                       PERFORM POST-JOB-PROCEDURE
                   WHEN "2"
                       PERFORM BROWSE-JOBS-PROCEDURE
                   WHEN "3"
                       PERFORM VIEW-APPLICATIONS-REPORT
                   WHEN "4"
                       PERFORM POST-LOGIN-NAVIGATION
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again."
                       TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-EVALUATE
           END-PERFORM.

        MESSAGES-MENU-PROCEDURE.
           SET MESSAGES-MENU-EXIT-FLAG TO 'N'
           PERFORM UNTIL MESSAGES-MENU-EXIT-FLAG = 'Y'
               MOVE "--- Messages Menu ---" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "1) Send a New Message" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "2) View My Messages" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "3) Back to Main Menu" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "Enter your choice:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               
               READ INPUT-FILE
                   AT END
                       SET MESSAGES-MENU-EXIT-FLAG TO 'Y'
                   NOT AT END
                       MOVE INPUT-RECORD(1:1) TO INPUT-CHOICE-BUF
                       EVALUATE FUNCTION TRIM(INPUT-CHOICE-BUF)
                           WHEN "1"
                               PERFORM SEND-MESSAGE-PROCEDURE
                           WHEN "2"
                               MOVE "View My Messages is under construction." TO TO-OUTPUT-BUF
                               PERFORM DISPLAY-AND-WRITE-OUTPUT
                           WHEN "3"
                               SET MESSAGES-MENU-EXIT-FLAG TO 'Y'
                           WHEN OTHER
                               MOVE "Invalid choice." TO TO-OUTPUT-BUF
                               PERFORM DISPLAY-AND-WRITE-OUTPUT
                       END-EVALUATE
               END-READ
           END-PERFORM.

       SEND-MESSAGE-PROCEDURE.
           MOVE "Enter recipient's username (must be a connection):" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT
           
           READ INPUT-FILE
               AT END
                   MOVE "Error reading input." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-RECORD) TO RECIPIENT-USERNAME
                   PERFORM VALIDATE-RECIPIENT-CONNECTION
                   
                   IF CONNECTION-VALID-FLAG = 'Y'
                       MOVE "Enter your message (max 200 chars):" TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                       
                       READ INPUT-FILE
                           AT END
                               MOVE "Error reading message content." TO TO-OUTPUT-BUF
                               PERFORM DISPLAY-AND-WRITE-OUTPUT
                           NOT AT END
                               MOVE FUNCTION TRIM(INPUT-RECORD) TO MESSAGE-CONTENT
                               PERFORM SAVE-MESSAGE-TO-FILE
                               MOVE "Message sent to " TO TO-OUTPUT-BUF
                               STRING TO-OUTPUT-BUF DELIMITED BY SIZE
                                      FUNCTION TRIM(RECIPIENT-USERNAME) DELIMITED BY SIZE
                                      " successfully!" DELIMITED BY SIZE
                                      INTO TO-OUTPUT-BUF
                               PERFORM DISPLAY-AND-WRITE-OUTPUT
                               MOVE "---------------------" TO TO-OUTPUT-BUF
                               PERFORM DISPLAY-AND-WRITE-OUTPUT
                       END-READ
                   ELSE
                       MOVE "You can only message users you are connected with." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
           END-READ.

       VALIDATE-RECIPIENT-CONNECTION.
           SET CONNECTION-VALID-FLAG TO 'N'
           PERFORM VARYING CONNECTION-INDEX FROM 1 BY 1 
               UNTIL CONNECTION-INDEX > CONNECTION-COUNT
               IF (CON-SENDER(CONNECTION-INDEX) = CURRENT-USER
                   AND WS-CONN-RECEIVER(CONNECTION-INDEX) = RECIPIENT-USERNAME
                   AND WS-CONN-STATUS(CONNECTION-INDEX) = "ACCEPTED")
                   OR
                   (WS-CONN-RECEIVER(CONNECTION-INDEX) = CURRENT-USER
                   AND WS-CONN-SENDER(CONNECTION-INDEX) = RECIPIENT-USERNAME
                   AND WS-CONN-STATUS(CONNECTION-INDEX) = "ACCEPTED")
                   SET CONNECTION-VALID-FLAG TO 'Y'
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       SAVE-MESSAGE-TO-FILE.
           OPEN EXTEND MESSAGES-FILE
           IF MESSAGES-FILE-STATUS = "00"
               MOVE CURRENT-USER TO MSG-SENDER
               MOVE RECIPIENT-USERNAME TO MSG-RECIPIENT
               MOVE MESSAGE-CONTENT TO MSG-CONTENT
               MOVE FUNCTION CURRENT-DATE TO MSG-TIMESTAMP
               WRITE MESSAGES-RECORD
               CLOSE MESSAGES-FILE
           ELSE
               MOVE "Error saving message." TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-IF.

       LOAD-MESSAGES-FROM-FILE.
           OPEN INPUT MESSAGES-FILE
           IF MESSAGES-FILE-STATUS = "00"
               SET MESSAGE-COUNT TO 0
               PERFORM UNTIL MESSAGES-FILE-STATUS NOT = "00"
                   READ MESSAGES-FILE
                       AT END
                           EXIT PERFORM
                       NOT AT END
                           ADD 1 TO MESSAGE-COUNT
                           MOVE MSG-SENDER TO WS-MSG-SENDER(MESSAGE-COUNT)
                           MOVE MSG-RECIPIENT TO WS-MSG-RECIPIENT(MESSAGE-COUNT)
                           MOVE MSG-CONTENT TO WS-MSG-CONTENT(MESSAGE-COUNT)
                           MOVE MSG-TIMESTAMP TO WS-MSG-TIMESTAMP(MESSAGE-COUNT)
               END-PERFORM
               CLOSE MESSAGES-FILE
           END-IF.







       POST-JOB-PROCEDURE.
           MOVE "--- Post a New Job/Internship ---" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           ADD 1 TO WS-JOB-COUNT.
           MOVE USER-USERNAME(LOGGED-IN-RANK)
               TO WS-JOB-POSTER(WS-JOB-COUNT).

           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter Job Title:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF INPUT-RECORD = SPACES
                   MOVE "Job Title cannot be blank." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM.
           MOVE FUNCTION TRIM(INPUT-RECORD)
               TO WS-JOB-TITLE(WS-JOB-COUNT).

           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter Description (max 200 chars):" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF INPUT-RECORD = SPACES
                   MOVE "Description cannot be blank." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM.
           MOVE FUNCTION TRIM(INPUT-RECORD)
               TO WS-JOB-DESCRIPTION(WS-JOB-COUNT).

           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter Employer Name:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF INPUT-RECORD = SPACES
                   MOVE "Employer Name cannot be blank." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM.
           MOVE FUNCTION TRIM(INPUT-RECORD)
               TO WS-JOB-EMPLOYER(WS-JOB-COUNT).

           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter Location:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF INPUT-RECORD = SPACES
                   MOVE "Location cannot be blank." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM.
           MOVE FUNCTION TRIM(INPUT-RECORD)
               TO WS-JOB-LOCATION(WS-JOB-COUNT).

           MOVE "Enter Salary (optional, enter 'NONE' to skip):"
               TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT
           PERFORM READ-INPUT-SAFELY
           IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF.
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-RECORD)) = "NONE"
               MOVE SPACES TO WS-JOB-SALARY(WS-JOB-COUNT)
           ELSE
               MOVE FUNCTION TRIM(INPUT-RECORD)
                   TO WS-JOB-SALARY(WS-JOB-COUNT)
           END-IF.

           PERFORM SAVE-JOBS-TO-FILE.

           MOVE "Job posted successfully!" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.


       FIND-SOMEONE-PROCEDURE.
           MOVE "Enter the name of the person you want to find:" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT
           PERFORM READ-INPUT-SAFELY
           IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
           MOVE FUNCTION TRIM(INPUT-RECORD) TO SEARCH-NAME

           MOVE 0 TO PROFILE-INDEX

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > USER-COUNT

               MOVE SPACES TO FULL-NAME

               STRING
                   FUNCTION TRIM(USER-FIRST-NAME(I)) DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   FUNCTION TRIM(USER-LAST-NAME(I)) DELIMITED BY SIZE
                   INTO FULL-NAME
               END-STRING

               IF FUNCTION TRIM(SEARCH-NAME) = FUNCTION TRIM(FULL-NAME)

                   MOVE LOGGED-IN-RANK TO PROFILE-INDEX
                   MOVE I TO LOGGED-IN-RANK

                   MOVE "--- Found User Profile ---" TO PROFILE-HEADING
                   PERFORM VIEW-PROFILE-PROCEDURE

                   MOVE PROFILE-INDEX TO LOGGED-IN-RANK
                   MOVE I TO PROFILE-INDEX

                   PERFORM PROFILE-OPTIONS

                   EXIT PERFORM
               END-IF
           END-PERFORM


           IF PROFILE-INDEX = 0
               MOVE "No user found with that name." TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-IF.


       PROFILE-OPTIONS.
           MOVE "1) Send Connection Request" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT
           MOVE "2) Back to Main Menu" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT
           MOVE "Enter your choice:" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT

           PERFORM READ-INPUT-SAFELY
           IF EXIT-PROGRAM
               PERFORM EXIT-EARLY
           END-IF
           MOVE FUNCTION TRIM(INPUT-RECORD) TO INPUT-CHOICE-BUF


           EVALUATE INPUT-CHOICE-BUF
               WHEN "1"
                   IF FUNCTION TRIM(USER-USERNAME(LOGGED-IN-RANK)) = FUNCTION TRIM(USER-USERNAME(PROFILE-INDEX))
                       MOVE "Invalid. Can't send connection to yourself." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   ELSE
                       PERFORM SEND-CONNECTION-REQUEST
                       IF REQUEST-SUCCESS = "Y"
                       MOVE "Connection request sent successfully." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
                   PERFORM POST-LOGIN-NAVIGATION
               WHEN "2"
                   PERFORM POST-LOGIN-NAVIGATION
               WHEN OTHER
                   MOVE "Invalid choice, try again." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   PERFORM PROFILE-OPTIONS
           END-EVALUATE
           EXIT.



       SEND-CONNECTION-REQUEST.


           PERFORM LOAD-NETWORKS-FROM-FILE.
           MOVE "N" TO REQUEST-SUCCESS.
           MOVE "N" TO CONNECTION-EXIST-FLAG.


           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONNECTION-COUNT
               IF FUNCTION TRIM(CON-SENDER(I)) = FUNCTION TRIM(USER-USERNAME(LOGGED-IN-RANK))
               AND FUNCTION TRIM(CON-RECEIVER(I)) = FUNCTION TRIM(USER-USERNAME(PROFILE-INDEX))
                   MOVE "Y" TO CONNECTION-EXIST-FLAG
                   MOVE "You have already sent a request to this user." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               ELSE IF FUNCTION TRIM(CON-SENDER(I)) = FUNCTION TRIM(USER-USERNAME(PROFILE-INDEX))
               AND FUNCTION TRIM(CON-RECEIVER(I)) = FUNCTION TRIM(USER-USERNAME(LOGGED-IN-RANK))
                   MOVE "Y" TO CONNECTION-EXIST-FLAG
                   MOVE "This user has already sent you a request." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF

               IF CONNECTION-EXIST-FLAG = "Y"
                   EXIT PERFORM
               END-IF
           END-PERFORM.


           IF CONNECTION-EXIST-FLAG = "N"

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > NETWORK-COUNT
                   IF (FUNCTION TRIM(NETWORK-USER1(I)) = FUNCTION TRIM(USER-USERNAME(LOGGED-IN-RANK))
                   AND FUNCTION TRIM(NETWORK-USER2(I)) = FUNCTION TRIM(USER-USERNAME(PROFILE-INDEX)))
                   OR
                   (FUNCTION TRIM(NETWORK-USER1(I)) = FUNCTION TRIM(USER-USERNAME(PROFILE-INDEX))
                   AND FUNCTION TRIM(NETWORK-USER2(I)) = FUNCTION TRIM(USER-USERNAME(LOGGED-IN-RANK)))
                       MOVE "Y" TO CONNECTION-EXIST-FLAG
                       MOVE "Already connected with this user." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF


           IF CONNECTION-EXIST-FLAG = "N"
               ADD 1 TO CONNECTION-COUNT
               MOVE USER-USERNAME(LOGGED-IN-RANK) TO CON-SENDER(CONNECTION-COUNT)
               MOVE USER-USERNAME(PROFILE-INDEX) TO CON-RECEIVER(CONNECTION-COUNT)
               PERFORM SAVE-CONNECTIONS-TO-FILE
               MOVE "Y" TO REQUEST-SUCCESS
           END-IF.



       PENDING-REQUESTS-PROCEDURE.
           MOVE "----- Pending Connection Requests: -----" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT


           MOVE "N" TO CONNECTION-EXIST-FLAG


           PERFORM VARYING I FROM CONNECTION-COUNT BY -1 UNTIL I < 1
               IF FUNCTION TRIM(CON-RECEIVER(I)) = FUNCTION TRIM(USER-USERNAME(LOGGED-IN-RANK))
                   MOVE "Y" TO CONNECTION-EXIST-FLAG
                   PERFORM PROCESS-REQUEST-PROCEDURE
               END-IF
           END-PERFORM.


           IF CONNECTION-EXIST-FLAG = "N"
               MOVE "You have no pending connection requests." TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-IF

           MOVE "----------------------------------------" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           EXIT.


       PROCESS-REQUEST-PROCEDURE.
           PERFORM USER-CHOICE-PROCEDURE.

           IF INPUT-CHOICE-BUF = "1"
               PERFORM ESTABLISHED-NETWORK-PROCEDURE
               MOVE SPACES TO TO-OUTPUT-BUF
               STRING
                   "Connection request from: " DELIMITED BY SIZE
                   FUNCTION TRIM(FULL-NAME) DELIMITED BY SIZE
                   " accepted!" DELIMITED BY SIZE
                   INTO TO-OUTPUT-BUF
               END-STRING
               PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-IF

           IF INPUT-CHOICE-BUF = "2"
               MOVE SPACES TO TO-OUTPUT-BUF
               STRING
                   "Connection request from: " DELIMITED BY SIZE
                   FUNCTION TRIM(FULL-NAME) DELIMITED BY SIZE
                   " rejected!" DELIMITED BY SIZE
                   INTO TO-OUTPUT-BUF
               END-STRING
               PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-IF

           PERFORM REMOVE-PENDING-PROCEDURE
           PERFORM SAVE-CONNECTIONS-TO-FILE.


       REMOVE-PENDING-PROCEDURE.
           PERFORM VARYING J FROM I BY 1 UNTIL J >= CONNECTION-COUNT
               MOVE CONNECTIONS-TABLE(J + 1) TO CONNECTIONS-TABLE(J)
           END-PERFORM
           SUBTRACT 1 FROM CONNECTION-COUNT.


       ESTABLISHED-NETWORK-PROCEDURE.
           OPEN EXTEND NETWORKS-FILE.

           IF NET-FILE-STATUS = "35"
               CLOSE NETWORKS-FILE
               OPEN OUTPUT NETWORKS-FILE
           END-IF.

           MOVE CON-SENDER(I) TO NETWORKS-SENDER.
           MOVE CON-RECEIVER(I) TO NETWORKS-RECIEVER.
           WRITE NETWORKS-RECORD.
           CLOSE NETWORKS-FILE.


       USER-CHOICE-PROCEDURE.

           MOVE 0 TO PROFILE-INDEX
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > USER-COUNT
               IF FUNCTION TRIM(CON-SENDER(I)) = FUNCTION TRIM(USER-USERNAME(J))
                   MOVE J TO PROFILE-INDEX
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF PROFILE-INDEX > 0
               MOVE SPACES TO FULL-NAME
               STRING
                   FUNCTION TRIM(USER-FIRST-NAME(PROFILE-INDEX)) DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   FUNCTION TRIM(USER-LAST-NAME(PROFILE-INDEX)) DELIMITED BY SIZE
                   INTO FULL-NAME
               END-STRING

               MOVE SPACES TO TO-OUTPUT-BUF
               STRING
                   "Request from: " DELIMITED BY SIZE
                   FUNCTION TRIM(FULL-NAME) DELIMITED BY SIZE
                   INTO TO-OUTPUT-BUF
               END-STRING
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               MOVE "1) Accept" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "2) Reject" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               MOVE SPACES TO TO-OUTPUT-BUF
               STRING
                   "Enter your choice for " DELIMITED BY SIZE
                   FUNCTION TRIM(FULL-NAME) DELIMITED BY SIZE
                   ":" DELIMITED BY SIZE
                   INTO TO-OUTPUT-BUF
               END-STRING
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               PERFORM READ-INPUT-SAFELY

               IF EXIT-PROGRAM
                   PERFORM EXIT-EARLY
               END-IF
               MOVE FUNCTION TRIM(INPUT-RECORD) TO INPUT-CHOICE-BUF

               IF INPUT-CHOICE-BUF NOT = "1" AND INPUT-CHOICE-BUF NOT = "2"
                   MOVE "Invalid choice. Request Skipped." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-IF.


       VIEW-NETWORK-PROCEDURE.
           PERFORM LOAD-NETWORKS-FROM-FILE.

           MOVE 'N' TO NETWORK-EXIST-FLAG

           MOVE "------------- Your Network -------------" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NETWORK-COUNT
               IF FUNCTION TRIM(NETWORK-USER1(I)) = FUNCTION TRIM(USER-USERNAME(LOGGED-IN-RANK))
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > USER-COUNT
                       IF FUNCTION TRIM(NETWORK-USER2(I)) = FUNCTION TRIM(USER-USERNAME(J))
                           MOVE 'Y' TO NETWORK-EXIST-FLAG
                           MOVE J TO PROFILE-INDEX
                           PERFORM DISPLAY-NETWORKS-PROCEDURE
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               ELSE IF FUNCTION TRIM(NETWORK-USER2(I)) = FUNCTION TRIM(USER-USERNAME(LOGGED-IN-RANK))
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > USER-COUNT
                       IF FUNCTION TRIM(NETWORK-USER1(I)) = FUNCTION TRIM(USER-USERNAME(J))
                           MOVE 'Y' TO NETWORK-EXIST-FLAG
                           MOVE J TO PROFILE-INDEX
                           PERFORM DISPLAY-NETWORKS-PROCEDURE
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

           IF NETWORK-EXIST-FLAG = 'N'
               MOVE "You have no network connections." TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-IF

           MOVE "----------------------------------------" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT.


       DISPLAY-NETWORKS-PROCEDURE.
           MOVE SPACES TO TO-OUTPUT-BUF
               STRING
                   "Connected with: " DELIMITED BY SIZE
                   FUNCTION TRIM(USER-FIRST-NAME(PROFILE-INDEX)) DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   FUNCTION TRIM(USER-LAST-NAME(PROFILE-INDEX)) DELIMITED BY SIZE
                   " (University: " DELIMITED BY SIZE
                   FUNCTION TRIM(USER-UNIVERSITY(PROFILE-INDEX)) DELIMITED BY SIZE
                   ", Major: " DELIMITED BY SIZE
                   FUNCTION TRIM(USER-MAJOR(PROFILE-INDEX)) DELIMITED BY SIZE
                   ")" DELIMITED BY SIZE
                   INTO TO-OUTPUT-BUF
               END-STRING
               PERFORM DISPLAY-AND-WRITE-OUTPUT.


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



       CREATE-PROFILE-PROCEDURE.

           MOVE "--- Create/Edit Profile ---" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE "N" TO PROFILE-CREATION-FAILURE-FLAG.


           PERFORM WITH TEST AFTER
                   UNTIL FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) > 0
               MOVE "Enter First Name:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF
               IF INPUT-RECORD = SPACES
                   MOVE "Invalid First Name." TO TO-OUTPUT-BUF
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
                   MOVE "Invalid Last Name." TO TO-OUTPUT-BUF
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
                   MOVE "Invalid University/College attended." TO TO-OUTPUT-BUF
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
                   MOVE "Invalid Major." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM
           MOVE FUNCTION TRIM(INPUT-RECORD) TO USER-MAJOR(LOGGED-IN-RANK)

           MOVE 'N' TO GRAD-YEAR-FLAG
           PERFORM WITH TEST AFTER UNTIL GRAD-YEAR-SUCESSFUL
               MOVE "Enter Graduation Year (YYYY):" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF

               IF FUNCTION TRIM(INPUT-RECORD) IS NUMERIC
                   AND FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD)) = 4 AND INPUT-RECORD NOT = SPACES
                   MOVE FUNCTION TRIM(INPUT-RECORD) TO USER-GRADUATION-YEAR(LOGGED-IN-RANK)
                   SET GRAD-YEAR-SUCESSFUL TO TRUE
               ELSE
                   MOVE "Invalid Graduation Year." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM.

           MOVE "Enter About Me (Optional, max 200 characters, enter blank line to skip):" TO TO-OUTPUT-BUF
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

           MOVE "Profile saved successfully!" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT
           .

       VIEW-PROFILE-PROCEDURE.
           MOVE PROFILE-HEADING TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF.
           STRING "Name: " DELIMITED BY SIZE
               FUNCTION TRIM(USER-FIRST-NAME(LOGGED-IN-RANK)) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(USER-LAST-NAME(LOGGED-IN-RANK)) DELIMITED BY SIZE
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

           MOVE "Graduation Year: " TO TO-OUTPUT-BUF.
           IF USER-GRADUATION-YEAR(LOGGED-IN-RANK) NOT = 0
               STRING "Graduation Year: " DELIMITED BY SIZE
                   USER-GRADUATION-YEAR(LOGGED-IN-RANK) DELIMITED BY SPACE
                   INTO TO-OUTPUT-BUF
           END-IF
           PERFORM DISPLAY-AND-WRITE-OUTPUT.



           IF FUNCTION LENGTH(FUNCTION TRIM(USER-ABOUT-ME(LOGGED-IN-RANK))) > 0
               MOVE SPACES TO TO-OUTPUT-BUF
               STRING "About Me: " DELIMITED BY SIZE
                   FUNCTION TRIM(USER-ABOUT-ME(LOGGED-IN-RANK)) DELIMITED BY SIZE
                   INTO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-IF




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

               MOVE "  Description (optional, max 100 chars, blank to skip):" TO TO-OUTPUT-BUF
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

       BROWSE-JOBS-PROCEDURE.
           MOVE "--- Available Job Listings ---" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           IF WS-JOB-COUNT = 0
               MOVE "No jobs available." TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-JOB-COUNT
               MOVE SPACES TO TO-OUTPUT-BUF
           STRING
           I DELIMITED BY SIZE
           ") " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-TITLE(I)) DELIMITED BY SIZE
           " at " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-EMPLOYER(I)) DELIMITED BY SIZE
           " (" DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-LOCATION(I)) DELIMITED BY SIZE
           ")" DELIMITED BY SIZE
           INTO TO-OUTPUT-BUF
           END-STRING
           PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-PERFORM.

           PERFORM WITH TEST AFTER
               UNTIL (FUNCTION TRIM(INPUT-RECORD) IS NUMERIC AND
               (FUNCTION NUMVAL(FUNCTION TRIM(INPUT-RECORD)) >= 0 AND
               FUNCTION NUMVAL(FUNCTION TRIM(INPUT-RECORD)) <= WS-JOB-COUNT))

               MOVE "Enter job number to view details, or 0 to go back:"
               TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               PERFORM READ-INPUT-SAFELY
               IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF

                   IF NOT (FUNCTION TRIM(INPUT-RECORD) IS NUMERIC AND
                       (FUNCTION NUMVAL(FUNCTION TRIM(INPUT-RECORD)) >= 0 AND
                       FUNCTION NUMVAL(FUNCTION TRIM(INPUT-RECORD)) <= WS-JOB-COUNT))
                       MOVE "Invalid job number. Please try again." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
                       END-PERFORM.

           MOVE FUNCTION NUMVAL(FUNCTION TRIM(INPUT-RECORD)) TO WS-JOB-CHOICE.
           IF WS-JOB-CHOICE > 0
               PERFORM VIEW-JOB-DETAILS-PROCEDURE
               PERFORM BROWSE-JOBS-PROCEDURE
           END-IF.

       VIEW-JOB-DETAILS-PROCEDURE.
           MOVE "--- Job Details ---" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF
           STRING "Title: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-TITLE(WS-JOB-CHOICE))
           INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF
           STRING "Description: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-DESCRIPTION(WS-JOB-CHOICE))
           INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF
           STRING "Employer: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-EMPLOYER(WS-JOB-CHOICE))
           INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF
           STRING "Location: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-LOCATION(WS-JOB-CHOICE))
           INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE SPACES TO TO-OUTPUT-BUF
           STRING "Salary: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-SALARY(WS-JOB-CHOICE))
           INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE "-------------------" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           MOVE "1) Apply for this Job" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           MOVE "2) Back to Job List" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           MOVE "Enter your choice:" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           PERFORM READ-INPUT-SAFELY.
           IF EXIT-PROGRAM PERFORM EXIT-EARLY END-IF.
           MOVE INPUT-RECORD(1:1) TO INPUT-CHOICE-BUF.

           IF INPUT-CHOICE-BUF = "1"
               PERFORM APPLY-FOR-JOB-PROCEDURE
               END-IF.

       APPLY-FOR-JOB-PROCEDURE.
           MOVE "N" TO CONNECTION-EXIST-FLAG.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-APP-COUNT
               IF (WS-APP-USERNAME(I) = USER-USERNAME(LOGGED-IN-RANK)) AND
                   (WS-APP-JOB-TITLE(I) = WS-JOB-TITLE(WS-JOB-CHOICE)) AND
                   (WS-APP-JOB-EMPLOYER(I) = WS-JOB-EMPLOYER(WS-JOB-CHOICE))

                   MOVE "Y" TO CONNECTION-EXIST-FLAG
                   EXIT PERFORM
               END-IF
                   END-PERFORM.

           IF CONNECTION-EXIST-FLAG = "Y"
               MOVE "You have already applied for this job." TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               EXIT PARAGRAPH
               END-IF.

           OPEN EXTEND APPLICATIONS-FILE.
           MOVE USER-USERNAME(LOGGED-IN-RANK)
           TO APP-USERNAME.
           MOVE WS-JOB-TITLE(WS-JOB-CHOICE)
           TO APP-JOB-TITLE.
           MOVE WS-JOB-EMPLOYER(WS-JOB-CHOICE)
           TO APP-JOB-EMPLOYER.
           MOVE WS-JOB-LOCATION(WS-JOB-CHOICE)
           TO APP-JOB-LOCATION.
           WRITE APPLICATIONS-RECORD.
           CLOSE APPLICATIONS-FILE.

           ADD 1 TO WS-APP-COUNT.
           MOVE USER-USERNAME(LOGGED-IN-RANK)
           TO WS-APP-USERNAME(WS-APP-COUNT).
           MOVE WS-JOB-TITLE(WS-JOB-CHOICE)
           TO WS-APP-JOB-TITLE(WS-APP-COUNT).
           MOVE WS-JOB-EMPLOYER(WS-JOB-CHOICE)
           TO WS-APP-JOB-EMPLOYER(WS-APP-COUNT).
           MOVE WS-JOB-LOCATION(WS-JOB-CHOICE)
           TO WS-APP-JOB-LOCATION(WS-APP-COUNT).

           MOVE SPACES TO TO-OUTPUT-BUF.
           STRING "Your application for " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-TITLE(WS-JOB-CHOICE)) DELIMITED BY SIZE
           " at " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-EMPLOYER(WS-JOB-CHOICE)) DELIMITED BY SIZE
           " has been submitted." DELIMITED BY SIZE
           INTO TO-OUTPUT-BUF
           END-STRING.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

       DISPLAY-AND-WRITE-OUTPUT.
           DISPLAY FUNCTION TRIM(TO-OUTPUT-BUF TRAILING).
           MOVE TO-OUTPUT-BUF TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.

       READ-INPUT-SAFELY.
           READ INPUT-FILE
               AT END
                   SET EXIT-PROGRAM TO TRUE
           END-READ.


       VIEW-APPLICATIONS-REPORT.
           MOVE "--- Your Job Applications ---" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

           MOVE 0 TO J.
           MOVE "N" TO CONNECTION-EXIST-FLAG.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-APP-COUNT
               IF WS-APP-USERNAME(I) = USER-USERNAME(LOGGED-IN-RANK)
                   MOVE "Y" TO CONNECTION-EXIST-FLAG
                   ADD 1 TO J

           MOVE SPACES TO TO-OUTPUT-BUF
           STRING "Job Title: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-APP-JOB-TITLE(I))
           INTO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT

           MOVE SPACES TO TO-OUTPUT-BUF
           STRING "Employer: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-APP-JOB-EMPLOYER(I))
           INTO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT

           MOVE SPACES TO TO-OUTPUT-BUF
           STRING "Location: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-APP-JOB-LOCATION(I))
           INTO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT

           MOVE "--------------------" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-PERFORM.

           IF CONNECTION-EXIST-FLAG = "N"
               MOVE "You have not applied to any jobs." TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "--------------------" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF.

           MOVE SPACES TO TO-OUTPUT-BUF.
           STRING "Total Applications: " DELIMITED BY SIZE
           J DELIMITED BY SIZE
           INTO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.
           MOVE "--------------------" TO TO-OUTPUT-BUF.
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

       EXIT-EARLY.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.
