
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
