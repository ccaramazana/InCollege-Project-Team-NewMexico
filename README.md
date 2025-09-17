# InCollege-Project-Team-NewMexico

InCollege - an online tool that will be designed exclusively for college
students allowing them to connect, exchange information, and talk with each
other.

# Compiling

`[user@hostname:~]$ cobc -x -free ./src/InCollege.cob`

# Usage

Create three files in the root of the repo.

- `./secrets.txt`

- `./input.txt`

- `./output.txt`

- `./output.txt`

How to format your `input.txt`.

- 1 for Login or 2 for Create New Account
- Enter Username
- Enter Password
- If created account, enter a choice for inital menu. Else enter input following the menu options
- If choice is under construction, reprompt nav menu. Else if skills menu, select a valid input for skills. Else if Create/Edit Profile Follow format
- Every choice if invaid input is detected will require user to keep inputting a valid input before moving to the next part of profile creation

Then `[user@hostname:~]$ ./InCollege` to run the program with the given input.
