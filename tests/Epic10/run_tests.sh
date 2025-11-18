#!/bin/bash
# Epic 10 Test Runner
# Runs all test cases and validates outputs

echo "========================================="
echo "InCollege Epic 10 Test Suite"
echo "========================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
FAILED=0

# Clean and build
echo "Building InCollege..."
make clean > /dev/null 2>&1
make build > /dev/null 2>&1

if [ ! -f "bin/InCollege" ]; then
    echo -e "${RED}✗ Build failed!${NC}"
    exit 1
fi
echo -e "${GREEN}✓ Build successful${NC}"
echo ""

# Run each test
for test in tests/Epic10/inputs/*.txt; do
    testname=$(basename "$test" .txt)
    TOTAL=$((TOTAL + 1))

    # Clean data files
    rm -f accounts.dat profiles.dat connections.dat requests.dat job-postings.dat applications.dat

    # Copy input and run
    cp "$test" InCollege-Input.txt
    ./bin/InCollege > "tests/Epic10/outputs/${testname}.txt" 2>&1

    # Check for expected output based on test name
    case "$testname" in
        *"valid-registration")
            if grep -q "Account created." "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"duplicate-username")
            if grep -q "Username already exists." "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"too-short")
            if grep -q "Password must be 8-12 characters long." "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"no-uppercase")
            if grep -q "Password must include at least one uppercase letter." "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"no-digit")
            if grep -q "Password must include at least one digit." "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"no-special")
            if grep -q "Password must include at least one special character." "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"account-limit")
            if grep -q "All permitted accounts have been created, please come back later" "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"successful-login")
            if grep -q "You have successfully logged in" "tests/Epic10/outputs/${testname}.txt" && \
               grep -q "Welcome, loginuser!" "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"wrong-username"|*"wrong-password")
            if grep -q "Incorrect username/password, please try again" "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"invalid-menu")
            if grep -q "Invalid choice, please try again." "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *"invalid-menu-choice")
            if grep -q "Invalid choice, please try again." "tests/Epic10/outputs/${testname}.txt"; then
                echo -e "${GREEN}✓${NC} $testname"
                PASSED=$((PASSED + 1))
            else
                echo -e "${RED}✗${NC} $testname"
                FAILED=$((FAILED + 1))
            fi
            ;;
        *)
            echo "  $testname (no validation)"
            ;;
    esac
done

echo ""
echo "========================================="
echo "Test Results"
echo "========================================="
echo "Total:  $TOTAL"
echo -e "${GREEN}Passed: $PASSED${NC}"
if [ $FAILED -gt 0 ]; then
    echo -e "${RED}Failed: $FAILED${NC}"
else
    echo "Failed: $FAILED"
fi
echo "Pass Rate: $((PASSED * 100 / TOTAL))%"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
