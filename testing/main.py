#!/usr/bin/env python
import typer
import subprocess
import shutil
import os
from pathlib import Path
from typing_extensions import Annotated

# --- Configuration ---
# Dynamically determine the project root directory based on this script's location.
SCRIPT_DIR = Path(__file__).parent.resolve()
PROJECT_ROOT = SCRIPT_DIR.parent

# Define all paths as absolute paths relative to the project root.
INPUT_FILE = PROJECT_ROOT / "input.txt"
OUTPUT_FILE = PROJECT_ROOT / "output.txt"
SECRETS_FILE = PROJECT_ROOT / "secrets.txt"
PROFILES_FILE = PROJECT_ROOT / "profiles.txt"

# Default paths for CLI options and base files.
DEFAULT_EXECUTABLE = PROJECT_ROOT / "InCollege"
DEFAULT_SECRETS_BASE = SCRIPT_DIR / "secrets.base.txt"
DEFAULT_PROFILES_BASE = SCRIPT_DIR / "profiles.base.txt"

# Create the Typer app instance.
app = typer.Typer(
    help="A command-line test runner for the InCollege COBOL application.",
    add_completion=False,
    pretty_exceptions_enable=False
)

def prepare_test_files(test_case_dir: Path):
    """
    Sets up the test environment based on a specific test case directory.
    - Clears the output file.
    - Copies input.txt from the test case directory.
    - Copies secrets.txt and profiles.txt from the test case directory if they exist,
      otherwise uses the base files.
    """
    print("...  preparing test environment ...")

    # 1. Clear the output file for a clean run.
    if OUTPUT_FILE.exists():
        OUTPUT_FILE.unlink()
    OUTPUT_FILE.touch()
    print(f"  - Cleared {OUTPUT_FILE}")

    # 2. Handle secrets.txt: Use test-specific file or fall back to base.
    test_secrets_file = test_case_dir / "secrets.txt"
    if test_secrets_file.exists():
        shutil.copy(test_secrets_file, SECRETS_FILE)
        print(f"  - Using test-specific secrets from: {test_secrets_file}")
    else:
        shutil.copy(DEFAULT_SECRETS_BASE, SECRETS_FILE)
        print(f"  - Using base secrets from: {DEFAULT_SECRETS_BASE}")

    # 3. Handle profiles.txt: Use test-specific file or fall back to base.
    test_profiles_file = test_case_dir / "profiles.txt"
    if test_profiles_file.exists():
        shutil.copy(test_profiles_file, PROFILES_FILE)
        print(f"  - Using test-specific profiles from: {test_profiles_file}")
    else:
        shutil.copy(DEFAULT_PROFILES_BASE, PROFILES_FILE)
        print(f"  - Using base profiles from: {DEFAULT_PROFILES_BASE}")

    # 4. Handle mandatory input.txt.
    test_input_file = test_case_dir / "input.txt"
    if not test_input_file.exists():
        # This is a fatal error for a test case.
        typer.secho(f"❌ ERROR: Mandatory 'input.txt' not found in '{test_case_dir}'", fg=typer.colors.RED)
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
        exists=True, file_okay=True
    )] = DEFAULT_EXECUTABLE,
):
    """Run a single test case from a directory against the COBOL application."""
    print(f"--- 🚀 Starting test: {test_case_dir.name} 🚀 ---")

    try:
        prepare_test_files(test_case_dir)
    except Exception as e:
        typer.secho(f"❌ ERROR during setup: {e}", fg=typer.colors.RED)
        raise typer.Exit(code=1)

    print(f"... executing '{cobol_executable}' ...\n")
    if not os.access(cobol_executable, os.X_OK):
        typer.secho(f"❌ ERROR: Executable '{cobol_executable}' is not executable.", fg=typer.colors.RED)
        typer.secho(f"💡 Try running: chmod +x {cobol_executable}", fg=typer.colors.YELLOW)
        raise typer.Exit(code=1)

    print("--- PROGRAM OUTPUT (STDOUT) ---")
    try:
        # Use 'cwd' to run the executable from the project root.
        result = subprocess.run(
            [os.path.abspath(cobol_executable)],
            capture_output=True,
            text=True,
            check=True,
            cwd=PROJECT_ROOT
        )
        print(result.stdout)
        print("--- END PROGRAM OUTPUT ---")

    except subprocess.CalledProcessError as e:
        typer.secho(f"❌ ERROR: The COBOL program exited with an error (code {e.returncode}).", fg=typer.colors.RED)
        typer.secho(f"--- STDERR ---\n{e.stderr}", fg=typer.colors.YELLOW)
        raise typer.Exit(code=1)
    except Exception as e:
        typer.secho(f"❌ An unexpected error occurred: {e}", fg=typer.colors.RED)
        raise typer.Exit(code=1)

    print("\n... execution finished ...")
    typer.secho(f"✅ Test '{test_case_dir.name}' completed successfully.", fg=typer.colors.GREEN)
    print(f"Check '{OUTPUT_FILE}' for the captured program output.")
    print("--------------------------------------------------")


if __name__ == "__main__":
    app()

