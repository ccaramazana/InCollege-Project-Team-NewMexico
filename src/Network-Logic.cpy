
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
               WHEN "2"
                   NEXT SENTENCE
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
               MOVE FUNCTION TRIM(USER-USERNAME(LOGGED-IN-RANK)) TO CON-SENDER(CONNECTION-COUNT)
               MOVE FUNCTION TRIM(USER-USERNAME(PROFILE-INDEX)) TO CON-RECEIVER(CONNECTION-COUNT)
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
           ELSE IF INPUT-CHOICE-BUF = "2"
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
           MOVE CON-RECEIVER(I) TO NETWORKS-RECEIVER.
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
