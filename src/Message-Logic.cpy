
       MESSAGES-MENU-PROCEDURE.
           MOVE 'N' TO MESSAGES-MENU-EXIT-FLAG
           MOVE "--- Messages Menu ---" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT
           PERFORM UNTIL MESSAGES-MENU-EXIT-FLAG = 'Y' OR EXIT-PROGRAM
               MOVE "1) Send a New Message" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "2) View My Messages" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "3) Back to Main Menu" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT
               MOVE "Enter your choice:" TO TO-OUTPUT-BUF
               PERFORM DISPLAY-AND-WRITE-OUTPUT

               PERFORM READ-INPUT-SAFELY

               IF EXIT-PROGRAM
                   PERFORM EXIT-EARLY
               ELSE
                   MOVE INPUT-RECORD(1:1) TO INPUT-CHOICE-BUF
                   EVALUATE FUNCTION TRIM(INPUT-CHOICE-BUF)
                       WHEN "1"
                           PERFORM SEND-MESSAGE-PROCEDURE
                       WHEN "2"
                           PERFORM VIEW-MESSAGE-PROCEDURE
                       WHEN "3"
                           MOVE 'Y' TO MESSAGES-MENU-EXIT-FLAG
                       WHEN OTHER
                           MOVE "Invalid choice." TO TO-OUTPUT-BUF
                           PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-EVALUATE
               END-IF
           END-PERFORM.

       VIEW-MESSAGE-PROCEDURE.
           PERFORM LOAD-MESSAGES-FROM-FILE
           MOVE 'N' TO MESSAGE-VALID-FLAG

           MOVE "--- Your Messages ---" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-MESSAGE-COUNT
               IF (FUNCTION TRIM(WS-MSG-RECIPIENT(I)) = CURRENT-USER)
                   MOVE 'Y' TO MESSAGE-VALID-FLAG

                   MOVE SPACES TO TO-OUTPUT-BUF
                   STRING
                       "From: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-MSG-SENDER(I)) DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   END-STRING
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   MOVE SPACES TO TO-OUTPUT-BUF
                   STRING
                       "Message: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-MSG-CONTENT(I)) DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   END-STRING
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   MOVE WS-MSG-TIMESTAMP(I)(1:21) TO WS-PARSE-TIMESTAMP-DATA

                   MOVE SPACES TO WS-FORMATTED-TIMESTAMP
                   STRING
                       WS-PARSE-MONTH  DELIMITED BY SIZE
                       "/"             DELIMITED BY SIZE
                       WS-PARSE-DAY    DELIMITED BY SIZE
                       "/"             DELIMITED BY SIZE
                       WS-PARSE-YEAR   DELIMITED BY SIZE
                       " "             DELIMITED BY SIZE
                       WS-PARSE-HOUR   DELIMITED BY SIZE
                       ":"             DELIMITED BY SIZE
                       WS-PARSE-MINUTE DELIMITED BY SIZE
                       INTO WS-FORMATTED-TIMESTAMP
                   END-STRING

                   MOVE SPACES TO TO-OUTPUT-BUF
                   STRING
                       "Sent: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-FORMATTED-TIMESTAMP) DELIMITED BY SIZE
                       INTO TO-OUTPUT-BUF
                   END-STRING
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   MOVE "--------------------" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                END-IF
           END-PERFORM.

           IF (MESSAGE-VALID-FLAG = 'N')
                   MOVE "You have no messages at this time." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
                   MOVE "---------------------" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
           END-IF.

       SEND-MESSAGE-PROCEDURE.
           MOVE "Enter the username of the person you are looking for:" TO TO-OUTPUT-BUF
           PERFORM DISPLAY-AND-WRITE-OUTPUT

           PERFORM READ-INPUT-SAFELY

           IF EXIT-PROGRAM
              PERFORM EXIT-EARLY
           ELSE
              *> Parse the full name into first and last name
               MOVE FUNCTION TRIM(INPUT-RECORD) TO SEARCH-USERNAME

               PERFORM VALIDATE-RECIPIENT-CONNECTION

               IF CONNECTION-VALID-FLAG = 'Y'
                   MOVE "Enter your message (max 200 chars):" TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT

                   PERFORM READ-INPUT-SAFELY

                   IF EXIT-PROGRAM
                       PERFORM EXIT-EARLY
                   ELSE
                       MOVE FUNCTION TRIM(INPUT-RECORD) TO MESSAGE-CONTENT
                       PERFORM SAVE-MESSAGE-TO-FILE
                       MOVE SPACES TO TO-OUTPUT-BUF
                       STRING
                           "Message sent to " DELIMITED BY SIZE
                           FUNCTION TRIM(SEARCH-USERNAME) DELIMITED BY SIZE
                           " successfully!" DELIMITED BY SIZE
                           INTO TO-OUTPUT-BUF
                       END-STRING
                       PERFORM DISPLAY-AND-WRITE-OUTPUT
                   END-IF
               ELSE
                   MOVE "You can only message users you are connected with." TO TO-OUTPUT-BUF
                   PERFORM DISPLAY-AND-WRITE-OUTPUT
               END-IF
           END-IF.

       VALIDATE-RECIPIENT-CONNECTION.
           MOVE 'N' TO CONNECTION-VALID-FLAG

           *> Load current network data first
           PERFORM LOAD-NETWORKS-FROM-FILE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NETWORK-COUNT
              IF (FUNCTION TRIM(NETWORK-USER1(I)) = CURRENT-USER) AND (FUNCTION TRIM(NETWORK-USER2(I)) = SEARCH-USERNAME)
                  OR
                  (FUNCTION TRIM(NETWORK-USER2(I)) = CURRENT-USER) AND (FUNCTION TRIM(NETWORK-USER1(I)) = SEARCH-USERNAME)
                  MOVE 'Y' TO CONNECTION-VALID-FLAG
                  MOVE SEARCH-USERNAME TO RECIPIENT-USERNAME
                  EXIT PERFORM
              END-IF
           END-PERFORM.
