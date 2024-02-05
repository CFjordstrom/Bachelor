project_file="src/Bachelor.fsproj"

if [ ! -f "$project_file" ]; then
  echo "Error: Project file not found: $project_file"
  exit 1
fi

if [ -z "$1" ]; then
  echo "Usage: $0 <input_file>"
  exit 1
fi

input_file="tests/$1"

if [ ! -f "$input_file" ]; then
  echo "Error: Input file not found: $input_file"
  exit 1
fi

dotnet run --project "$project_file" "$input_file"