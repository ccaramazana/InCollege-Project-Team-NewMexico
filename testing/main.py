#!/usr/bin/env python
import typer
import subprocess
import shutil
import os
from pathlib import Path
from typing_extensions import Annotated

# --- Configuration ---
# Dynamically determine the project root directory based on this script's location.
# This makes the script runnable from anywhere, as long as it's inside the 'testing' folder.
SCRIPT_DIR = Path(__file__).parent.resolve()
PROJECT_ROOT = SCRIPT_DIR.parent

# Define all paths as absolute paths relative to the project root.
INPUT_FILE = PROJECT_ROOT / "input.txt"
OUTPUT_FILE = PROJECT_ROOT / "output.txt"
SECRETS_FILE = PROJECT_ROOT / "secrets.txt"
PROFILES_FILE = PROJECT_ROOT / "profiles.txt"

# Default paths for CLI options, also constructed absolutely.
DEFAULT_EXECUTABLE = PROJECT_ROOT / "InCollege"
DEFAULT_SECRETS_BASE = SCRIPT_DIR / "secrets.base.txt"
DEFAULT_PROFILES_BASE = SCRIPT_DIR / "profiles.base.txt"

# Create the Typer app instance.
app = typer.Typer(
    help="A command-line test runner for the InCollege COBOL application.",
    add_completion=False,
    pretty_exceptions_enable=False # Disables Typer's own error handling to show our custom messages
)

def reset_test_environment(
    secrets_base: Path,
    profiles_base: Path,
):
    """Sets up the environment for a clean test run."""
    print("...  resetting test environment ...")

    if OUTPUT_FILE.exists():
        OUTPUT_FILE.unlink()
    OUTPUT_FILE.touch()
    print(f"  - Cleared {OUTPUT_FILE}")

    if secrets_base.exists():
        shutil.copy(secrets_base, SECRETS_FILE)
        print(f"  - Reset {SECRETS_FILE} from {secrets_base}")
    else:
        SECRETS_FILE.touch()
        print(f"  - Created empty {SECRETS_FILE} (base file not found)")

    if profiles_base.exists():
        shutil.copy(profiles_base, PROFILES_FILE)
        print(f"  - Reset {PROFILES_FILE} from {profiles_base}")
    else:
        PROFILES_FILE.touch()
        print(f"  - Created empty {PROFILES_FILE} (base file not found)")

@app.command()
def run(
    test_input_file: Annotated[Path, typer.Argument(
        exists=True, file_okay=True, dir_okay=False, readable=True,
        help="Path to the test input file (e.g., tests/i1.txt)."
    )],
    cobol_executable: Annotated[Path, typer.Option(
        "--executable", "-e",
        help="Path to the compiled COBOL executable file.",
        exists=True, file_okay=True
    )] = DEFAULT_EXECUTABLE,
    secrets_base_file: Annotated[Path, typer.Option(
        "--secrets", "-s",
        help="Path to the base secrets file for resetting accounts.",
        exists=True
    )] = DEFAULT_SECRETS_BASE,
    profiles_base_file: Annotated[Path, typer.Option(
        "--profiles", "-p",
        help="Path to the base profiles file for resetting profiles.",
        exists=True
    )] = DEFAULT_PROFILES_BASE,
):
    """Run a single test case against the COBOL application."""
    print(f"--- 🚀 Starting test: {test_input_file.name} 🚀 ---")

    try:
        reset_test_environment(secrets_base_file, profiles_base_file)
        shutil.copy(test_input_file, INPUT_FILE)
        print(f"... copied {test_input_file} to {INPUT_FILE} for test run ...")
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
        # **CRITICAL FIX**: Use 'cwd' to run the executable from the project root.
        # This ensures the COBOL program can find "input.txt", "output.txt", etc.
        result = subprocess.run(
            [os.path.abspath(cobol_executable)],
            capture_output=True,
            text=True,
            check=True,
            cwd=PROJECT_ROOT
        )
        print(result.stdout)
        print("--- END PROGRAM OUTPUT ---")
    except FileNotFoundError:
        typer.secho(f"❌ ERROR: Executable not found at '{cobol_executable}'. Please compile it first.", fg=typer.colors.RED)
        raise typer.Exit(code=1)
    except subprocess.CalledProcessError as e:
        typer.secho(f"❌ ERROR: The COBOL program exited with an error (code {e.returncode}).", fg=typer.colors.RED)
        typer.secho(f"--- STDERR ---\n{e.stderr}", fg=typer.colors.YELLOW)
        raise typer.Exit(code=1)
    except Exception as e:
        typer.secho(f"❌ An unexpected error occurred: {e}", fg=typer.colors.RED)
        raise typer.Exit(code=1)

    print("\n... execution finished ...")
    typer.secho(f"✅ Test '{test_input_file.name}' completed successfully.", fg=typer.colors.GREEN)
    print(f"Check '{OUTPUT_FILE}' for the captured program output.")
    print("--------------------------------------------------")

if __name__ == "__main__":
    app()

