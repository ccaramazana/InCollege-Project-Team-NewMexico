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

       SAVE-MESSAGE-TO-FILE.
           OPEN EXTEND MESSAGES-FILE.
           MOVE CURRENT-USER TO MSG-SENDER.
           MOVE RECIPIENT-USERNAME TO MSG-RECIPIENT.
           MOVE MESSAGE-CONTENT TO MSG-CONTENT.
           MOVE FUNCTION CURRENT-DATE TO MSG-TIMESTAMP.
           WRITE MESSAGES-RECORD.
           CLOSE MESSAGES-FILE.

           ADD 1 TO WS-MESSAGE-COUNT
           MOVE CURRENT-USER TO WS-MSG-SENDER(WS-MESSAGE-COUNT).
           MOVE RECIPIENT-USERNAME TO WS-MSG-RECIPIENT(WS-MESSAGE-COUNT).
           MOVE MESSAGE-CONTENT TO WS-MSG-CONTENT(WS-MESSAGE-COUNT).
           MOVE MSG-TIMESTAMP TO WS-MSG-TIMESTAMP(WS-MESSAGE-COUNT).

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
            MOVE 0 TO PROFILE-COUNT.  *> Add this line

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
                        ADD 1 TO PROFILE-COUNT  *> Add this line
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
           END-IF

           SET NOT-END-OF-FILE TO TRUE.

           PERFORM UNTIL END-OF-FILE
               READ CONNECTIONS-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO CONNECTION-COUNT
                       MOVE SENDER-USERNAME TO CON-SENDER(CONNECTION-COUNT)
                       MOVE RECEIVER-USERNAME TO CON-RECEIVER(CONNECTION-COUNT)
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

       LOAD-MESSAGES-FROM-FILE.
           INITIALIZE WS-MESSAGES-DATA
           OPEN INPUT MESSAGES-FILE
           IF MESSAGES-FILE-STATUS = "35"
               OPEN OUTPUT MESSAGES-FILE
               CLOSE MESSAGES-FILE
               OPEN INPUT MESSAGES-FILE
               MOVE "00" TO MESSAGES-FILE-STATUS
           END-IF.
           SET NOT-END-OF-FILE TO TRUE.
           PERFORM UNTIL END-OF-FILE
               READ MESSAGES-FILE
                   AT END
                       SET END-OF-FILE to true
                   NOT AT END
                       ADD 1 TO WS-MESSAGE-COUNT
                       MOVE MESSAGES-RECORD TO WS-MESSAGES-TABLE(WS-MESSAGE-COUNT)
                   END-READ
               END-PERFORM.
           CLOSE MESSAGES-FILE.
