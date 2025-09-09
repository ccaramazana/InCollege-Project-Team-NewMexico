IDENTIFICATION DIVISION.                 *> CRITICAL: Ensure this period is here.
      *>****************************************************************
      *> Program: InCollege.cob
      *> Epic:    Epic #1: Log In, Part 1
      *> Author:  Julio Chavez & Kalyan Castro De Oliveira
      *> Date:    09-09-2025
      *> Purpose: Foundational skeleton for the InCollege application.
      *> Handles file definitions and dual output logic.
      *>****************************************************************
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.                *> CRITICAL: Ensure this period is here.
      *>****************************************************************
      *> Defines files the program will use.
      *>****************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OutputFile ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT UserFile ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.                       *> CRITICAL: Ensure this period is here.
      *>****************************************************************
      *> Defines file structures and variables.
      *>****************************************************************
       FILE SECTION.
       FD  InputFile.
       01  InputRecord         PIC X(80).

       FD  OutputFile.
       01  OutputRecord        PIC X(80).

       FD  UserFile.
       01  UserRecord          PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-OUTPUT-LINE      PIC X(80).

       PROCEDURE DIVISION.                  *> CRITICAL: Ensure this period is here.
      *>****************************************************************
      *> Main program logic begins here.
      *>****************************************************************
       100-MAIN-LOGIC.
           PERFORM 200-INITIALIZE-FILES.

           MOVE "Welcome to InCollege!" TO WS-OUTPUT-LINE.
           PERFORM 800-WRITE-TO-SCREEN-AND-FILE.

           PERFORM 900-TERMINATE-PROGRAM.

       800-WRITE-TO-SCREEN-AND-FILE.
           DISPLAY WS-OUTPUT-LINE.
           WRITE OutputRecord FROM WS-OUTPUT-LINE.

       200-INITIALIZE-FILES.
           OPEN INPUT InputFile.
           OPEN OUTPUT OutputFile UserFile.

       900-TERMINATE-PROGRAM.
           CLOSE InputFile
                 OutputFile
                 UserFile.
           STOP RUN.
