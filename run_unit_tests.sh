project_file="src/Bachelor.fsproj"

if [ ! -f "$project_file" ]; then
  echo "Error: Project file not found: $project_file"
  exit 1
fi

# Loop over all .in files in the tests/unit directory
for input_file in tests/unit/*.in; do
  # Derive the base name and corresponding output file
  base_name=$(basename "$input_file" .in)
  output_file="tests/unit/$base_name.out"

  if [ ! -f "$input_file" ]; then
    echo "Error: Input file not found: $input_file"
    exit 1
  fi

  if [ ! -f "$output_file" ]; then
    echo "Error: Output file not found: $output_file"
    exit 1
  fi

  # Read the first line of the output file as options
  read -r -a options < "$output_file"

  # Read the rest of the output file as expected output
  expected=$(tail -n +2 "$output_file")

  # Run the dotnet command with the specified options and input file
  result=$(dotnet run --project "$project_file" "${options[@]}" "$input_file")

  # Compare the result with the expected output and print pass/fail
  if [ "$expected" = "$result" ]; then
    echo "$base_name: pass"
  else
    echo "expected: $expected"
    echo "got: $result"
    echo "$base_name: fail"
  fi
done