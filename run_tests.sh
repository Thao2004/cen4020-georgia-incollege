#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'

# --- tiny logger ---
PASS=0
FAIL=0
TOTAL=0
if [[ -t 1 ]]; then
  GREEN=$'\033[32m'; RED=$'\033[31m'; DIM=$'\033[2m'; RESET=$'\033[0m'
else
  GREEN=''; RED=''; DIM=''; RESET=''
fi

say_pass(){ printf "%s[PASS]%s %s\n" "$GREEN" "$RESET" "$1"; PASS=$((PASS+1)); TOTAL=$((TOTAL+1)); }
say_fail(){ printf "%s[FAIL]%s %s\n" "$RED"   "$RESET" "$1"; FAIL=$((FAIL+1)); TOTAL=$((TOTAL+1)); }

# --- build once ---
build(){
  mkdir -p bin
  cobc -x -free -o bin/InCollege src/InCollege.cob
  printf "%s\n" "${DIM}Built: bin/InCollege${RESET}"
}

# run one program execution inside a work dir with given input, then append to log
run_prog(){
  local wd="$1"; local input="$2"
  mkdir -p "$wd"                     # ensure directory exists
  printf "%s" "$input" > "$wd/InCollege-Input.txt"
  ( cd "$wd" && ../../bin/InCollege >/dev/null 2>&1 || true )
  cat "$wd/InCollege-Output.txt" >> "$wd/test.log"
}

# assert that a pattern exists in the combined test log
expect_contains(){
  local wd="$1"; local name="$2"; shift 2
  local ok=1
  for pat in "$@"; do
    if ! grep -Fq "$pat" "$wd/test.log"; then
      printf "Missing: %s\n" "$pat" >> "$wd/test.log"
      ok=0
    fi
  done
  if [[ $ok -eq 1 ]]; then say_pass "$name"; else say_fail "$name (see $wd/test.log)"; fi
}

# helper: new isolated test dir
mkcase(){
  local name="$1"
  local wd="tests/$(printf '%02d' "$CASE_ID")_${name// /_}"
  mkdir -p "$wd"
  : > "$wd/test.log"
  # log to STDERR so command substitution doesn't capture it
  printf "%s\n" "${DIM}Running: $name -> $wd${RESET}" >&2
  # only print the path to STDOUT (this is what WD captures)
  printf "%s" "$wd"
}

# clean & build
rm -rf tests
mkdir -p tests
build

CASE_ID=0

# 1) Create account success
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "create_success")
run_prog "$WD" $'2\njennifer\nHellother1!\n'
expect_contains "$WD" "Create success" \
  "Welcome to InCollege!" "Account created."

# 2) Login success
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "login_success")
cp tests/01_create_success/accounts.dat "$WD/" || true
run_prog "$WD" $'1\njennifer\nHellother1!\n'
expect_contains "$WD" "Login success" "You have successfully logged in"

# 3) Login fail (wrong password)
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "login_fail_wrong_password")
cp tests/01_create_success/accounts.dat "$WD/" || true
run_prog "$WD" $'1\njennifer\nWrong111!\n'
expect_contains "$WD" "Login fails on wrong password" \
  "Incorrect username/password, please try again"

# 4) Username empty
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "username_empty")
run_prog "$WD" $'2\n\nAa1!aaaa\n'
expect_contains "$WD" "Username cannot be empty" "Username cannot be empty."

# 5) Username too long (>15)
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "username_too_long")
run_prog "$WD" $'2\naveryverylongname\nAa1!aaaa\n'
expect_contains "$WD" "Username length error" "Username must be 1-15 characters long."

# 6) Duplicate username (case-insensitive)
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "duplicate_username_case_insensitive")
run_prog "$WD" $'2\nALICE\nAa1!good!\n'
run_prog "$WD" $'2\nalice\nAa1!next!\n'
expect_contains "$WD" "Duplicate username blocked" "Username already exists."

# 7) Password too short (<8)
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "password_too_short")
run_prog "$WD" $'2\nshorty\nAa1!\n'
expect_contains "$WD" "PW short" "Password must be 8-12 characters long."

# 8) Password too long (>12)
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "password_too_long")
run_prog "$WD" $'2\nlonger\nAa1!ThisIsTooLong\n'
expect_contains "$WD" "PW long" "Password must be 8-12 characters long."

# 9) Password missing uppercase
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "password_missing_upper")
run_prog "$WD" $'2\nnoupper\npassword1!\n'
expect_contains "$WD" "PW missing uppercase" \
  "Password must include at least one uppercase letter."

# 10) Password missing digit
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "password_missing_digit")
run_prog "$WD" $'2\nnodigit\nPassword!!\n'
expect_contains "$WD" "PW missing digit" \
  "Password must include at least one digit."

# 11) Password missing special
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "password_missing_special")
run_prog "$WD" $'2\nnospecial\nPassword1\n'
expect_contains "$WD" "PW missing special" \
  "Password must include at least one special character."

# 12) Capacity limit after 5 accounts
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "capacity_limit_5")
run_prog "$WD" $'2\na1\nAa1!aaaa\n'
run_prog "$WD" $'2\na2\nAa1!bbbb\n'
run_prog "$WD" $'2\na3\nAa1!cccc\n'
run_prog "$WD" $'2\na4\nAa1!dddd\n'
run_prog "$WD" $'2\na5\nAa1!eeee\n'
run_prog "$WD" $'2\na6\nAa1!ffff\n'
expect_contains "$WD" "Capacity enforced" \
  "All permitted accounts have been created, please come back later"

# 13) Invalid menu choice
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "invalid_menu_choice")
run_prog "$WD" $'9\n'
expect_contains "$WD" "Invalid choice" "Invalid choice."

# 14) Persistence across runs (create then login in separate executions)
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "persistence_across_runs")
run_prog "$WD" $'2\npersist\nPers1!aaa\n'
run_prog "$WD" $'1\npersist\nPers1!aaa\n'
expect_contains "$WD" "Persistence works" \
  "Account created." "You have successfully logged in"

# 15) Trim input (spaces around username/password)
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "trim_input_spaces")
run_prog "$WD" $'2\n  alice  \n  Pass1!234  \n'
run_prog "$WD" $'1\nalice\nPass1!234\n'
expect_contains "$WD" "Trimmed input accepted" \
  "Account created." "You have successfully logged in"

# 16) Trimmed username/password create then login succeeds
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "trimmed_create_login")
run_prog "$WD" $'2\n  frank  \n  Frank1!234  \n'
run_prog "$WD" $'1\nfrank\nFrank1!234\n'
expect_contains "$WD" "Trimmed create/login" \
  "Account created." "You have successfully logged in"

# 17) Username length boundary (15 ok, 16 rejected)
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "username_length_boundaries")
run_prog "$WD" $'2\nfifteenCharsUsr\nAb1!cdef\n'
run_prog "$WD" $'2\nsixteenCharsUserX\nAb1!cdef\n'
expect_contains "$WD" "Username 15 vs 16" \
  "Account created." "Username must be 1-15 characters long."

# 18) Password length boundaries
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "password_length_boundaries")
run_prog "$WD" $'2\nlen8ok\nA1!bcdefg\n'
run_prog "$WD" $'2\nlen12ok\nAb1!cdefGhij\n'
run_prog "$WD" $'2\nlen7bad\nA1!bcde\n'
run_prog "$WD" $'2\nlen13bad\nAb1!cdefGhijk\n'
expect_contains "$WD" "PW length 8/12 good 7/13 bad" \
  "Account created." "Password must be 8-12 characters long."

# 19) Special character variants
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "special_char_variants")
run_prog "$WD" $'2\nspec_underscore\nAbcdef1_\n'
run_prog "$WD" $'2\nspec_comma\nAbc1,def\n'
run_prog "$WD" $'2\nspec_space\nAbc1 def\n'
expect_contains "$WD" "Special chars accepted" "Account created."

# 20) Login with nonexistent username
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "login_nonexistent")
run_prog "$WD" $'1\nnobody\nWhatever1!\n'
expect_contains "$WD" "Login nonexistent" \
  "Incorrect username/password, please try again"

# 21) Duplicate username exact-case
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "duplicate_exact_case")
run_prog "$WD" $'2\ngeorge\nGg1!alpha\n'
run_prog "$WD" $'2\ngeorge\nGg1!beta\n'
expect_contains "$WD" "Duplicate exact blocked" "Username already exists."

# 22) Login wrong twice then success
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "login_fail_then_success")
run_prog "$WD" $'2\ntestuser\nTt1!good\n'
run_prog "$WD" $'1\ntestuser\nWrong1!\n'
run_prog "$WD" $'1\ntestuser\nWrong2!\n'
run_prog "$WD" $'1\ntestuser\nTt1!good\n'
expect_contains "$WD" "Two fails then success" \
  "Incorrect username/password, please try again" \
  "You have successfully logged in"

# 23) Empty username create
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "empty_username_create")
run_prog "$WD" $'2\n\nAbcdef1!\n'
expect_contains "$WD" "Empty username" "Username cannot be empty."

# 24) Password missing uppercase, digit, special
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "pw_missing_types")
run_prog "$WD" $'2\nloweronly\npassword1!\n'
run_prog "$WD" $'2\nnodigituser\nPassword!!\n'
run_prog "$WD" $'2\nnospecialuser\nPassword1\n'
expect_contains "$WD" "PW missing type msgs" \
  "Password must include at least one uppercase letter." \
  "Password must include at least one digit." \
  "Password must include at least one special character."

# 25) Fill to capacity then sixth blocked
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "fill_capacity_then_block")
run_prog "$WD" $'2\nuA\nAa1!aaaa\n'
run_prog "$WD" $'2\nuB\nBb2!bbbb\n'
run_prog "$WD" $'2\nuC\nCc3!cccc\n'
run_prog "$WD" $'2\nuD\nDd4!dddd\n'
run_prog "$WD" $'2\nuE\nEe5!eeee\n'
run_prog "$WD" $'2\nuF\nFf6!ffff\n'
expect_contains "$WD" "Sixth account blocked" \
  "All permitted accounts have been created, please come back later"

# 26) Case-insensitive duplicate (ALex vs alex)
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "duplicate_case_insensitive")
run_prog "$WD" $'2\nALex\nAx1!aaaa\n'
run_prog "$WD" $'2\nalex\nAx1!bbbb\n'
expect_contains "$WD" "Duplicate case-insensitive" "Username already exists."

# 27) Username with only spaces
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "username_only_spaces")
run_prog "$WD" $'2\n     \nAa1!aaaa\n'
expect_contains "$WD" "Username only spaces invalid" "Username cannot be empty."

# 28) Invalid menu then valid create
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "invalid_then_valid")
run_prog "$WD" $'9\n2\nhannah\nHh1!hello\n'
expect_contains "$WD" "Invalid then valid create" \
  "Invalid choice." "Account created."

# 29) Create with spaces around name then login
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "create_with_spaces_then_login")
run_prog "$WD" $'2\n  isaac  \n  Isa4!good  \n'
run_prog "$WD" $'1\nisaac\nIsa4!good\n'
expect_contains "$WD" "Create with spaces login ok" \
  "Account created." "You have successfully logged in"

# 30) Password with mixed punctuation
CASE_ID=$((CASE_ID+1)); WD=$(mkcase "password_with_punctuation")
run_prog "$WD" $'2\nmixpunct\nAb1!?.#$\n'
expect_contains "$WD" "Mixed punctuation accepted" "Account created."


# --- summary ---
printf "\n== Summary: %s%d passed%s, %s%d failed%s, %d total ==\n" \
  "$GREEN" "$PASS" "$RESET" "$RED" "$FAIL" "$RESET" "$TOTAL"

exit $(( FAIL == 0 ? 0 : 1 ))

