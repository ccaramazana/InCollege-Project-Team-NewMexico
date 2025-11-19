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
               ORGANIZATION IS LINE SEQUENTIAL
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

       FD  NETWORKS-FILE.
       01  NETWORKS-RECORD.
           05 NETWORKS-SENDER PIC X(20).
           05 NETWORKS-RECEIVER PIC X(20).

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
           05 MSG-TIMESTAMP        PIC X(25).

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

       01  JOB-MENU-EXIT-FLAG PIC A(1).
           88 EXIT-JOB-MENU VALUE 'Y'.

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

       01  SEARCH-USERNAME PIC X(20).

       01  PROFILE-INDEX          PIC 9(3) VALUE 0.

       01  PROFILE-HEADING    PIC X(30).

       01  CONN-FILE-STATUS   PIC XX VALUE SPACES.
       01  REQUEST-SUCCESS   PIC X VALUE "N".
       01  NET-FILE-STATUS PIC XX VALUE SPACES.
       01  JOBS-FILE-STATUS PIC XX VALUE SPACES.
       01  APP-FILE-STATUS  PIC XX VALUE SPACES.
       01  MESSAGES-FILE-STATUS    PIC XX VALUE SPACES.

       01  WS-APPLICATIONS-DATA.
           05 WS-APP-COUNT      PIC 99 VALUE 0.
           05 WS-APPLICATIONS-TABLE OCCURS 100 TIMES.
               10 WS-APP-USERNAME      PIC X(20).
               10 WS-APP-JOB-TITLE     PIC X(80).
               10 WS-APP-JOB-EMPLOYER  PIC X(80).
               10 WS-APP-JOB-LOCATION  PIC X(80).

       01  WS-JOB-CHOICE       PIC 99.

       01  WS-MESSAGES-DATA.

           05 WS-MESSAGE-COUNT           PIC 99 VALUE 0.
           05 WS-MESSAGES-TABLE OCCURS 50 TIMES.
               10 WS-MSG-SENDER           PIC X(20).
               10 WS-MSG-RECIPIENT        PIC X(20).
               10 WS-MSG-CONTENT          PIC X(200).
               10 WS-MSG-TIMESTAMP        PIC X(25).

       01  WS-TIMESTAMP-PARSER.
           05 WS-PARSE-TIMESTAMP-DATA PIC X(21).
           05 WS-PARSE-FIELDS REDEFINES WS-PARSE-TIMESTAMP-DATA.
               10 WS-PARSE-YEAR    PIC 9(4).
               10 WS-PARSE-MONTH   PIC 9(2).
               10 WS-PARSE-DAY     PIC 9(2).
               10 WS-PARSE-HOUR    PIC 9(2).
               10 WS-PARSE-MINUTE  PIC 9(2).
               10 WS-PARSE-SECOND  PIC 9(2).
               10 WS-PARSE-REST    PIC X(7).

       01  WS-FORMATTED-TIMESTAMP PIC X(25).
       01  RECIPIENT-USERNAME      PIC X(20).
       01  MESSAGE-CONTENT         PIC X(200).

       01  MESSAGE-VALID-FLAG      PIC X VALUE 'N'.

       01  CONNECTION-VALID-FLAG   PIC X VALUE 'N'.

       01  MESSAGES-MENU-EXIT-FLAG PIC X VALUE 'N'.

       01  CURRENT-USER            PIC X(20).

       01  PROFILE-COUNT           PIC 99 VALUE 0.


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

               EVALUATE FUNCTION TRIM(INPUT-CHOICE-BUF)
                   WHEN "1"
                       PERFORM CREATE-PROFILE-PROCEDURE
                   WHEN "2"
                       MOVE "--- Your Profile ---" TO PROFILE-HEADING
                       PERFORM VIEW-PROFILE-PROCEDURE
                   WHEN "3"
                       PERFORM JOB-SEARCH-MENU
                   WHEN "4"
                       PERFORM FIND-SOMEONE-PROCEDURE
                   WHEN "5"
                       PERFORM SKILLS-MENU-PROCEDURE
                   WHEN "6"
                       PERFORM PENDING-REQUESTS-PROCEDURE
                   WHEN "7"
                       PERFORM VIEW-NETWORK-PROCEDURE
                   WHEN "8"
                       PERFORM MESSAGES-MENU-PROCEDURE
                   WHEN "9"
                       SET EXIT-MENU TO TRUE
                   WHEN OTHER
                       MOVE "Invalid Choice, Please Try Again." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-EVALUATE

           END-PERFORM.

       SKILLS-MENU-PROCEDURE.

           MOVE "N" TO SKILLS-MENU-EXIT-FLAG.
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
           DISPLAY FUNCTION TRIM(TO-OUTPUT-BUF TRAILING).
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

       COPY "File-IO.cpy".
       COPY "User-Auth.cpy".
       COPY "Profile-Management.cpy".
       COPY "Job-Logic.cpy".
       COPY "Network-Logic.cpy".
       COPY "Message-Logic.cpy".

