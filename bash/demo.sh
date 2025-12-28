#!/usr/bin/env bash

echo "Hello Bash"

echo "=== TEST {} ==="
test_curly_bracket_expansion() {
  for v in {-2..10..2}; do
    echo -e "\e[31m$v\e[m"
  done
}

test_curly_bracket_expansion

echo "=== TEST \${} ==="
test_dollar_curly_bracket_expansion() {
  a="hello_world_in_bash"
  echo "origin: $a"
  echo "replace _ with -: ${a//_/-}"
  echo "slice range off=2 len=3: ${a:2:3}"
  echo "upper: ${a^^}"
  echo "lower: ${a,,}"
}

test_dollar_curly_bracket_expansion

echo "=== TEST \$(()) ==="
test_int_manipulation() {
  a="11"
  echo "oriign: $a"
  echo "parse int: $(($a))"
  echo "calc int 10x: $(($a * 10))"
}

test_int_manipulation

echo "=== TEST float ==="
test_float_manipulation() {
  a=11.11
  b=22.22
  result=$(awk -v a="$a" -v b="$b" 'BEGIN{ printf "%.2f", a+b }')
  echo "11.11+22.22 = $result"
}

test_float_manipulation

echo "=== TEST array ==="
test_array() {
  os_array=("WINDOWS" "LINUS")
  os_array[1]="LINUX"
  for os in "${os_array[@]}"; do
    echo "os: $os"
  done
}

test_array

echo "=== TEST association ==="
test_assoc() {
  declare -A os_lib_ext=(
    ["WINDOWS"]=".dll"
    ["LINUX"]=".so"
  )
  for k in "${!os_lib_ext[@]}"; do
    v=${os_lib_ext[$k]}
    echo "$k => $v"
  done
}

test_assoc

echo "=== TEST case ==="
test_case() {
  # os=${1:?Error: missing argument: os} # assertion
  os=${1:-"ANDROID"} # default value
  case $os in
  "WINDOWS")
    echo "os is Windows"
    ;;
  "ANDROID")
    echo "os is Android"
    ;;
  *)
    echo "os may be Linux"
    ;;
  esac
}

test_case "WINDOWS"
test_case "ANDROID"
test_case "ios"
test_case

echo "=== TEST read ==="
test_read() {
  old_ifs=$IFS # IFS: Internal Field Separator
  IFS=","
  read -rp "Input your name and age (sep:','): " name age
  age=$(($age + 10)) # string->int
  echo "name is $name"
  echo "age is $age"
  read -rp "Input your favorite colors (sep:','): " -a colors
  for c in "${colors[@]}"; do
    echo "color: $c"
  done
  IFS=$old_ifs
}

test_read
