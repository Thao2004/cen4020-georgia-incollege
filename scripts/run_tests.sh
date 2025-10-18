#!/usr/bin/env bash
set -euo pipefail

STORY="StoryX"
PROGRAM="bin/InCollege"

cmd="${1:-}"; shift || true

usage() {
  cat <<USAGE
Usage:
    $0 init [--epic N] [--replace] [--clear-output]
        Create Epic<EPIC>-StoryX-Input and Epic<EPIC>-StoryX-Output.
        --replace       : delete BOTH Input and Output folders first
        --clear-output  : delete ONLY the Output folder first (safer default)

    $0 run  [--epic N] [--no-reset] [--no-verify] [--clear-output] [--list] [--test N]
        Build and run tests in Epic<EPIC>-StoryX-Input.
        --no-reset      : keep accounts.dat/profiles.dat between tests
        --no-verify     : skip terminal vs file comparison
        --clear-output  : delete ONLY the Output folder before running
        --list          : list tests (naturally sorted) with numbers and exit
        --test N        : run only the Nth test from that list


    $0 clean [--epic N] [--yes]
        Delete Epic<EPIC>-StoryX-Input and Epic<EPIC>-StoryX-Output without recreating.
        --yes : skip confirmation prompt


Defaults:
    EPIC=3, verify=ON, reset state between tests=ON. Inputs are NEVER deleted by 'run'.
Examples:
    $0 init --epic 4 --clear-output
    $0 run  --epic 4
USAGE
}


# Shared flags (with sensible defaults)
EPIC="3"
CLEAR_OUTPUT="0"
REPLACE_ALL="0"   # (init only)
NO_RESET="0"      # (run only)
NO_VERIFY="0"     # (run only)
YES="0"           # for 'clean' confirmation
TEST_INDEX=""     # e.g., --test 5  -> run only the 5th test
LIST_ONLY="0"     # --list          -> list tests and exit


parse_common_flags() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
        --epic) EPIC="$2"; shift 2 ;;
        --clear-output) CLEAR_OUTPUT="1"; shift ;;
        --replace) REPLACE_ALL="1"; shift ;;         # only respected by 'init'
        --no-reset) NO_RESET="1"; shift ;;           # only respected by 'run'
        --no-verify) NO_VERIFY="1"; shift ;;         # only respected by 'run'
        --yes) YES="1"; shift ;;
        --test) TEST_INDEX="$2"; shift 2 ;;
        --list) LIST_ONLY="1"; shift ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Unknown arg: $1"; usage; exit 1 ;;
        esac
    done
}


init_cmd() {
    parse_common_flags "$@"
    local INPUT_DIR="Epic${EPIC}-${STORY}-Test-Input"
    local OUTPUT_DIR="Epic${EPIC}-${STORY}-Test-Output"

    if [[ "$REPLACE_ALL" == "1" ]]; then
        rm -rf "$INPUT_DIR" "$OUTPUT_DIR"
    elif [[ "$CLEAR_OUTPUT" == "1" ]]; then
        rm -rf "$OUTPUT_DIR"
    fi

    mkdir -p "$INPUT_DIR" "$OUTPUT_DIR"
    echo "[INIT] Ready:"
    echo "       $INPUT_DIR"
    echo "       $OUTPUT_DIR"
}


run_cmd() {
    parse_common_flags "$@"
    local INPUT_DIR="Epic${EPIC}-${STORY}-Test-Input"
    local OUTPUT_DIR="Epic${EPIC}-${STORY}-Test-Output"
    local RESET_STATE=$([[ "$NO_RESET" == "1" ]] && echo "0" || echo "1")
    local VERIFY=$([[ "$NO_VERIFY" == "1" ]] && echo "0" || echo "1")

    # Never delete inputs here. Optionally clear only outputs if asked.
    if [[ "$CLEAR_OUTPUT" == "1" ]]; then
        rm -rf "$OUTPUT_DIR"
    fi
    mkdir -p "$INPUT_DIR" "$OUTPUT_DIR"

    echo "[BUILD] Building program..."
    make build >/dev/null

    shopt -s nullglob
    mapfile -t tests < <(printf '%s\n' "$INPUT_DIR"/*.txt | sort -V)

    if [[ ${#tests[@]} -eq 0 ]]; then
        echo "[ERROR] No .txt input files found in $INPUT_DIR"
        exit 1
    fi

    # If user asked to list, show numbered table and exit
    if [[ "$LIST_ONLY" == "1" ]]; then
    echo "[LIST] ${#tests[@]} test(s) in $INPUT_DIR (natural order):"
        i=1
        for p in "${tests[@]}"; do
            printf "  %2d) %s\n" "$i" "$(basename "$p")"
            ((i++))
        done
        exit 0
    fi

    # If user asked for one test by index, validate and narrow selection
    if [[ -n "$TEST_INDEX" ]]; then
        if ! [[ "$TEST_INDEX" =~ ^[0-9]+$ ]] || (( TEST_INDEX < 1 || TEST_INDEX > ${#tests[@]} )); then
            echo "[ERROR] --test must be between 1 and ${#tests[@]} (got: $TEST_INDEX)"
            exit 1
        fi
        # Keep only the selected test
        tests=( "${tests[$((TEST_INDEX-1))]}" )
        echo "[RUN] 1 selected test: $(basename "${tests[0]}")"
    else
        echo "[RUN] ${#tests[@]} test(s): $INPUT_DIR -> $OUTPUT_DIR"
    fi


    echo "[RUN] ${#tests[@]} test(s): $INPUT_DIR -> $OUTPUT_DIR"
    for inpath in "${tests[@]}"; do
        local base="$(basename "$inpath")"
        local outname
        if [[ "$base" == *"-Input.txt" ]]; then
            outname="${base/-Input.txt/-Output.txt}"
        else
            outname="${base%.txt}-Output.txt"
        fi
        local outpath="${OUTPUT_DIR}/${outname}"

        # Pre-clean per test (do NOT touch InCollege-Input.txt)
        if [[ "$RESET_STATE" == "1" ]]; then
            rm -f InCollege-Output.txt accounts.dat profiles.dat
        fi

        # Provide input file expected by the program
        cp -f "$inpath" InCollege-Input.txt

        if [[ "$VERIFY" == "1" ]]; then
            # Run the program, capture stdout in a variable, but create no temp files.
            # Guard against set -e aborting on nonzero status.
            set +e
            local screen_out
            screen_out="$("./$PROGRAM")"
            local prog_status=$?
            set -e

            # Compare (ignoring trailing spaces on each line)
            if ! diff -u \
                <(printf '%s\n' "$screen_out" | sed 's/[[:space:]]\+$//') \
                <(sed 's/[[:space:]]\+$//' InCollege-Output.txt) >/dev/null; then
                echo "[MISMATCH] $base (screen != file)."
            fi
        else
            ./"$PROGRAM" >/dev/null
        fi


        if [[ -f InCollege-Output.txt ]]; then
            cp -f InCollege-Output.txt "$outpath"   # keep root output AND save paired copy
            echo "[OK] $base -> $(basename "$outpath")"
        else
            echo "[FAIL] InCollege-Output.txt not produced for $base"
            [[ "$RESET_STATE" == "1" ]] && rm -f accounts.dat profiles.dat
            exit 1
        fi

        # Post-clean (state only; never remove InCollege-Input.txt)
        if [[ "$RESET_STATE" == "1" ]]; then
            rm -f accounts.dat profiles.dat
        fi
    done

    echo "[DONE] Outputs in $OUTPUT_DIR"
}


clean_cmd() {
    parse_common_flags "$@"
    local INPUT_DIR="Epic${EPIC}-${STORY}-Test-Input"
    local OUTPUT_DIR="Epic${EPIC}-${STORY}-Test-Output"

    if [[ "$YES" != "1" ]]; then
        read -r -p "Delete '$INPUT_DIR' and '$OUTPUT_DIR'? [y/N] " ans
        [[ "$ans" =~ ^[Yy]$ ]] || { echo "[CLEAN] Aborted."; exit 0; }
    fi

    rm -rf "$INPUT_DIR" "$OUTPUT_DIR"
    echo "[CLEAN] Removed:"
    echo "        $INPUT_DIR"
    echo "        $OUTPUT_DIR"
}


case "$cmd" in
    init) init_cmd "$@" ;;
    run)  run_cmd "$@" ;;
    clean) clean_cmd "$@" ;;
    *) usage; exit 1 ;;
esac