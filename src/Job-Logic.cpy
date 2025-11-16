       JOB-SEARCH-MENU.
           MOVE "N" TO JOB-MENU-EXIT-FLAG
           PERFORM UNTIL EXIT-JOB-MENU
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
                       SET EXIT-JOB-MENU TO TRUE
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO TO-OUTPUT-BUF
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-EVALUATE
           END-PERFORM.

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
