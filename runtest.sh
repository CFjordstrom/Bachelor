project_file="src/Bachelor.fsproj"

if [ ! -f "$project_file" ]; then
  echo "Error: Project file not found: $project_file"
  exit 1
fi

input_file="tests/unit/$1.in"
output_file="tests/unit/$1.out"
if [ ! -f "$input_file" ]; then
  echo "Error: Input file not found: $input_file"
  exit 1
fi
if [ ! -f "$output_file" ]; then
  echo "Error: Output file not found: $input_file"
  exit 1
fi

read -r -a options < "$output_file"

expected=$(tail -n +2 "$output_file")

result=$(dotnet run --project "$project_file" "${options[@]}" "$input_file")

if [ "$expected" = "$result" ]; then
  echo "$1: pass"
else
  echo "$1: fail"
fi