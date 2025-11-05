#!/usr/bin/env python
import typer
import subprocess
import shutil
import os
from pathlib import Path
from typing_extensions import Annotated

SCRIPT_DIR = Path(__file__).parent.resolve()
PROJECT_ROOT = SCRIPT_DIR.parent

INPUT_FILE = PROJECT_ROOT / "input.txt"
OUTPUT_FILE = PROJECT_ROOT / "output.txt"
SECRETS_FILE = PROJECT_ROOT / "secrets.txt"
PROFILES_FILE = PROJECT_ROOT / "profiles.txt"
CONNECTIONS_FILE = PROJECT_ROOT / "connections.txt"
JOBS_FILE = PROJECT_ROOT / "jobs.txt"
NETWORKS_FILE = PROJECT_ROOT / "networks.txt"
APPLICATIONS_FILE = PROJECT_ROOT / "applications.txt"
MESSAGES_FILE = PROJECT_ROOT / "messages.txt"
COBOL_SOURCE_FILE = PROJECT_ROOT / "src" / "InCollege.cob"

DEFAULT_EXECUTABLE = PROJECT_ROOT / "InCollege"
DEFAULT_SECRETS_BASE = SCRIPT_DIR / "secrets.base.txt"
DEFAULT_PROFILES_BASE = SCRIPT_DIR / "profiles.base.txt"
DEFAULT_CONNECTIONS_BASE = SCRIPT_DIR / "connections.base.txt"
DEFAULT_JOBS_BASE = SCRIPT_DIR / "jobs.base.txt"
DEFAULT_APPLICATIONS_BASE = SCRIPT_DIR / "applications.base.txt"
DEFAULT_MESSAGES_BASE = SCRIPT_DIR / "messages.base.txt"
DEFAULT_NETWORKS_BASE = SCRIPT_DIR / "networks.base.txt"

app = typer.Typer(
    help="A command-line test runner for the InCollege COBOL application.",
    add_completion=False,
    pretty_exceptions_enable=False
)

def compile_cobol_program():
    """Compiles the COBOL source code into an executable."""
    print("... compiling COBOL source ...")

    if not COBOL_SOURCE_FILE.exists():
        typer.secho(f"ERROR: COBOL source file not found at '{COBOL_SOURCE_FILE}'", fg=typer.colors.RED)
        raise typer.Exit(code=1)

    try:
        command = ["cobc", "-x", "-free", str(COBOL_SOURCE_FILE.relative_to(PROJECT_ROOT))]
        result = subprocess.run(
            command,
            capture_output=True,
            text=True,
            cwd=PROJECT_ROOT
        )

        if result.returncode != 0:
            typer.secho("ERROR: COBOL compilation failed.", fg=typer.colors.RED)
            typer.secho(f"--- Compiler Output (STDERR) ---\n{result.stderr}", fg=typer.colors.YELLOW)
            raise typer.Exit(code=1)

        print("  - Compilation successful.")

    except FileNotFoundError:
        typer.secho("ERROR: 'cobc' command not found. Is GnuCOBOL installed and in your PATH?", fg=typer.colors.RED)
        raise typer.Exit(code=1)
    except Exception as e:
        typer.secho(f"An unexpected error occurred during compilation: {e}", fg=typer.colors.RED)
        raise typer.Exit(code=1)

def prepare_test_files(test_case_dir: Path):
    """
    Sets up the test environment based on a specific test case directory.
    - Clears the output file.
    - Copies input.txt from the test case directory.
    - Copies secrets.txt and profiles.txt if they exist, otherwise uses base files.
    """
    print("...  preparing test environment ...")

    if OUTPUT_FILE.exists():
        OUTPUT_FILE.unlink()
    OUTPUT_FILE.touch()
    print(f"  - Cleared {OUTPUT_FILE}")

    test_secrets_file = test_case_dir / "secrets.txt"
    if test_secrets_file.exists():
        shutil.copy(test_secrets_file, SECRETS_FILE)
        print(f"  - Using test-specific secrets from: {test_secrets_file}")
    else:
        shutil.copy(DEFAULT_SECRETS_BASE, SECRETS_FILE)
        print(f"  - Using base secrets from: {DEFAULT_SECRETS_BASE}")

    test_profiles_file = test_case_dir / "profiles.txt"
    if test_profiles_file.exists():
        shutil.copy(test_profiles_file, PROFILES_FILE)
        print(f"  - Using test-specific profiles from: {test_profiles_file}")
    else:
        shutil.copy(DEFAULT_PROFILES_BASE, PROFILES_FILE)
        print(f"  - Using base profiles from: {DEFAULT_PROFILES_BASE}")

    test_connections_file = test_case_dir / "connections.txt"
    if test_connections_file.exists():
        shutil.copy(test_connections_file, CONNECTIONS_FILE)
        print(f"  - Using test-specific connections from: {test_connections_file}")
    else:
        shutil.copy(DEFAULT_CONNECTIONS_BASE, CONNECTIONS_FILE)
        print(f"  - Using base connections from: {DEFAULT_CONNECTIONS_BASE}")

    test_jobs_file = test_case_dir / "jobs.txt"
    if test_jobs_file.exists():
        shutil.copy(test_jobs_file, JOBS_FILE)
        print(f"  - Using test-specific jobs from: {test_jobs_file}")
    else:
        shutil.copy(DEFAULT_JOBS_BASE, JOBS_FILE)
        print(f"  - Using base jobs from: {DEFAULT_JOBS_BASE}")

    test_networks_file = test_case_dir / "networks.txt"
    if test_networks_file.exists():
        shutil.copy(test_networks_file, NETWORKS_FILE)
        print(f"  - Using test-specific networks from: {test_networks_file}")
    else:
        shutil.copy(DEFAULT_NETWORKS_BASE, NETWORKS_FILE)
        print(f"  - Using base networks from: {DEFAULT_NETWORKS_BASE}")

    test_applications_file = test_case_dir / "applications.txt"
    if test_applications_file.exists():
        shutil.copy(test_applications_file, APPLICATIONS_FILE)
        print(f"  - Using test-specific applications from: {test_applications_file}")
    else:
        shutil.copy(DEFAULT_APPLICATIONS_BASE, APPLICATIONS_FILE)
        print(f"  - Using base applications from: {DEFAULT_APPLICATIONS_BASE}")

    test_messages_file = test_case_dir / "messages.txt"
    if test_messages_file.exists():
        shutil.copy(test_messages_file, MESSAGES_FILE)
        print(f"  - Using test-specific messages from: {test_messages_file}")
    else:
        shutil.copy(DEFAULT_MESSAGES_BASE, MESSAGES_FILE)
        print(f"  - Using base messages from: {DEFAULT_MESSAGES_BASE}")

    test_input_file = test_case_dir / "input.txt"
    if not test_input_file.exists():
        typer.secho(f"ERROR: Mandatory 'input.txt' not found in '{test_case_dir}'", fg=typer.colors.RED)
        raise typer.Exit(code=1)
    shutil.copy(test_input_file, INPUT_FILE)
    print(f"  - Copied test input from: {test_input_file}")

@app.command()
def run(
    test_case_dir: Annotated[Path, typer.Argument(
        exists=True, file_okay=False, dir_okay=True, readable=True,
        help="Path to the directory containing the test case files."
    )],
    cobol_executable: Annotated[Path, typer.Option(
        "--executable", "-e",
        help="Path to the compiled COBOL executable file.",
        exists=False, file_okay=True
    )] = DEFAULT_EXECUTABLE,
    no_compile: Annotated[bool, typer.Option(
        "--no-compile",
        help="Skip the compilation step and use the existing executable."
    )] = False,
):
    """Run a single test case from a directory against the COBOL application."""
    print(f"--- Starting test: {test_case_dir.name} ---")

    if not no_compile:
        try:
            compile_cobol_program()
        except typer.Exit as e:
            raise e
    else:
        print("... skipping compilation step ...")

    try:
        prepare_test_files(test_case_dir)
    except Exception as e:
        typer.secho(f"ERROR during setup: {e}", fg=typer.colors.RED)
        raise typer.Exit(code=1)

    print(f"... executing '{cobol_executable}' ...\n")
    if not os.access(cobol_executable, os.X_OK):
        typer.secho(f"ERROR: Executable '{cobol_executable}' is not executable.", fg=typer.colors.RED)
        typer.secho(f"Try running: chmod +x {cobol_executable}", fg=typer.colors.YELLOW)
        raise typer.Exit(code=1)

    print("--- PROGRAM OUTPUT (STDOUT) ---")
    try:
        result = subprocess.run(
            [os.path.abspath(cobol_executable)],
            capture_output=True, text=True, check=True, cwd=PROJECT_ROOT
        )
        print(result.stdout)
        print("--- END PROGRAM OUTPUT ---")

    except subprocess.CalledProcessError as e:
        typer.secho(f"ERROR: The COBOL program exited with an error (code {e.returncode}).", fg=typer.colors.RED)
        typer.secho(f"--- STDERR ---\n{e.stderr}", fg=typer.colors.YELLOW)
        raise typer.Exit(code=1)
    except Exception as e:
        typer.secho(f"An unexpected error occurred: {e}", fg=typer.colors.RED)
        raise typer.Exit(code=1)

    print("\n... execution finished ...")
    try:
        test_output_dest = test_case_dir / "output.txt"
        shutil.copy(OUTPUT_FILE, test_output_dest)
        print(f"... copied result to {test_output_dest} ...")
    except Exception as e:
        typer.secho(f"WARNING: Could not copy output file to test directory: {e}", fg=typer.colors.YELLOW)

    typer.secho(f"Test '{test_case_dir.name}' completed successfully.", fg=typer.colors.GREEN)
    print(f"Check '{test_case_dir / 'output.txt'}' for the captured program output.")
    print("--------------------------------------------------")

if __name__ == "__main__":
    app()
