# InCollege-Project-Team-NewMexico

InCollege - an online tool that will be designed exclusively for college
students allowing them to connect, exchange information, and talk with each
other.

# Compiling

`[user@hostname:directory]$ cobc -x -free src/InCollege.cob`

# Usage

## For running

Create four files in the root of the repo.

- `input.txt`

- `output.txt`

- `secrets.txt`

- `profiles.txt`

- `connections.txt`

Compile, then `[user@hostname:directory]$ ./InCollege` to run the program with
the given input.

## For testing

Make sure to have [`typer`](https://typer.tiangolo.com/) installed locally on
your Python environment.

`testing/secrets.base.txt`, `testing/profiles.base.txt`, and
`testing/connections.base.txt` are the template files used for `secrets.txt`,
`profiles.txt`, and `connections.txt` during testing.

To create a test, create a folder. You must put inside that folder a file named
`input.txt`, which will be the input used for the test. You can also put files
named `profiles.txt`, `secrets.txt`, and `connections.txt` inside that folder,
and those will override the `testing/secrets.base.txt`,
`testing/profiles.base.txt`, and `testing/connections.base.txt` templates during
testing, but you don't have to.

Then, run `[user@hostname:directory]$ python testing/main.py
/path/to/your/test/folder/here` to test. You can view the output in a
`output.txt` file that is created in the test dir.

Examples of tests are located in `testing/epic#-tests`.

Example usage:

`[user@hostname:directory]$ python testing/main.py testing/epic3-tests/1`

The code is recompiled before each test. If you wish to disable this, use the
`--no-compile` flag. For instance,

`[user@hostname:directory]$ python testing/main.py testing/epic3-tests/1
--no-compile`
