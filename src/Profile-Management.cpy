
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

           PERFORM EDIT-EXPERIENCES-PROCEDURE
           PERFORM EDIT-EDUCATION-PROCEDURE
           PERFORM SAVE-PROFILES-TO-FILE

           MOVE "Profile saved successfully!" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT.

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
