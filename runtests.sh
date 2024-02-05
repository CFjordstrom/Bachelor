project_file="src/Bachelor.fsproj"

# Check if the project file exists
if [ ! -f "$project_file" ]; then
  echo "Error: Project file not found: $project_file"
  exit 1
fi

# Specify the path to the tests directory
tests_directory="tests"

# Check if the tests directory exists
if [ ! -d "$tests_directory" ]; then
  echo "Error: Tests directory not found: $tests_directory"
  exit 1
fi

# Get a list of all .in files in the tests directory
input_files=("$tests_directory"/*.in)

# Check if there are any test files
if [ ${#input_files[@]} -eq 0 ]; then
  echo "Error: No test files found in $tests_directory"
  exit 1
fi

# Iterate through each test file and run the project
for test_file in "${input_files[@]}"; do
  echo "Running test: $test_file"
  dotnet run --project "$project_file" "$test_file"
done