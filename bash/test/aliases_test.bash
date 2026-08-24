#!/usr/bin/env bash

root_=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
. "${root_}/bash/rc_post/aliases"

failures_=0

fail() {
    echo "FAIL: $*" >&2
    failures_=$((failures_ + 1))
}

assert_equal() {
    local expected_=$1
    local actual_=$2
    local description_=$3

    if [[ $actual_ != "$expected_" ]]; then
        fail "${description_}: expected '${expected_}', got '${actual_}'"
    fi
}

assert_status() {
    local expected_=$1
    local actual_=$2
    local description_=$3

    if (( actual_ != expected_ )); then
        fail "${description_}: expected status ${expected_}, got ${actual_}"
    fi
}

test_seconds_interval() {
    local sleep_calls_=0
    local actual_

    sleep() {
        [[ $1 == 2 ]] || return 2
        sleep_calls_=$((sleep_calls_ + 1))
        (( sleep_calls_ < 2 ))
    }

    actual_=$(seconds 2)
    assert_equal "0  2  " "$actual_" "seconds uses and displays the requested interval"
}

test_seconds_default_interval() {
    local sleep_calls_=0
    local actual_

    sleep() {
        [[ $1 == 1 ]] || return 2
        sleep_calls_=$((sleep_calls_ + 1))
        (( sleep_calls_ < 2 ))
    }

    actual_=$(seconds)
    assert_equal "0  1  " "$actual_" "seconds defaults to an interval of one"
}

test_minutes_interval() {
    local sleep_calls_=0
    local actual_

    sleep() {
        [[ $1 == 120 ]] || return 2
        sleep_calls_=$((sleep_calls_ + 1))
        (( sleep_calls_ < 2 ))
    }

    actual_=$(minutes 2)
    assert_equal "0  2  " "$actual_" "minutes uses and displays the requested interval"
}

test_minutes_default_interval() {
    local sleep_calls_=0
    local actual_

    sleep() {
        [[ $1 == 60 ]] || return 2
        sleep_calls_=$((sleep_calls_ + 1))
        (( sleep_calls_ < 2 ))
    }

    actual_=$(minutes)
    assert_equal "0  1  " "$actual_" "minutes defaults to an interval of one"
}

test_invalid_intervals() {
    local output_
    local status_

    output_=$(seconds 0 2>&1)
    status_=$?
    assert_status 2 "$status_" "seconds rejects zero"
    [[ $output_ == *"integer greater than or equal to 1"* ]] ||
        fail "seconds explains its interval requirement"

    output_=$(minutes 1.5 2>&1)
    status_=$?
    assert_status 2 "$status_" "minutes rejects non-integers"
    [[ $output_ == *"integer greater than or equal to 1"* ]] ||
        fail "minutes explains its interval requirement"

    output_=$(seconds "" 2>&1)
    status_=$?
    assert_status 2 "$status_" "seconds rejects an explicitly empty interval"
}

test_research_directory_helpers() {
    local test_home_
    local starting_directory_
    local output_
    local status_

    # Keep these tests independent of whether fd is installed on the host.
    fdfind() {
        if [[ $* != "--type d research" ]]; then
            return 2
        fi
        find . -type d -name '*research*' -printf '%P/\n' | sort
    }

    test_home_=$(mktemp -d)
    starting_directory_=$PWD
    mkdir -p \
        "${test_home_}/work/astro/research" \
        "${test_home_}/work/shared-one/research" \
        "${test_home_}/work/shared space/research" \
        "${test_home_}/work/shared-two/research-notes"

    output_=$(HOME=$test_home_ resfind shared)
    [[ $output_ == *"shared-one/research"* ]] || fail "resfind includes the first substring match"
    [[ $output_ == *"shared-two/research-notes"* ]] || fail "resfind includes the second substring match"

    HOME=$test_home_ rescd astro > "${test_home_}/rescd-output"
    assert_equal "${test_home_}/work/astro/research" "$PWD" "rescd changes to a unique match"
    output_=$(<"${test_home_}/rescd-output")
    assert_equal "$PWD" "$output_" "rescd prints the directory it changes to"

    cd "$starting_directory_" || exit
    HOME=$test_home_ rescd shared one > /dev/null
    assert_equal "${test_home_}/work/shared-one/research" "$PWD" "rescd matches all substrings"

    cd "$starting_directory_" || exit
    HOME=$test_home_ rescd one shared > /dev/null
    assert_equal "${test_home_}/work/shared-one/research" "$PWD" "rescd ignores substring order"

    cd "$starting_directory_" || exit
    output_=$(HOME=$test_home_ rescd shared 2>&1)
    status_=$?
    assert_status 1 "$status_" "rescd rejects ambiguous matches"
    assert_equal "$starting_directory_" "$PWD" "rescd leaves the directory unchanged when ambiguous"
    [[ $output_ == *"Refine with another substring or use a complete path:"* ]] ||
        fail "rescd explains how to refine ambiguous matches"
    [[ $output_ == *"rescd shared-one/research/"* &&
       $output_ == *"rescd shared-two/research-notes/"* ]] ||
        fail "rescd suggests commands for ambiguous matches"
    [[ $output_ == *"rescd shared\\ space/research/"* ]] ||
        fail "rescd shell-quotes suggested paths"

    output_=$(HOME=$test_home_ rescd missing 2>&1)
    status_=$?
    assert_status 1 "$status_" "rescd rejects missing matches"
    assert_equal "$starting_directory_" "$PWD" "rescd leaves the directory unchanged with no match"

    rm -rf "$test_home_"
}

test_seconds_interval
test_seconds_default_interval
test_minutes_interval
test_minutes_default_interval
test_invalid_intervals
test_research_directory_helpers

if (( failures_ > 0 )); then
    exit 1
fi

echo "All alias tests passed."
