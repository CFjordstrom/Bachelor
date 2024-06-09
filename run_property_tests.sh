project_file="src/Bachelor.fsproj"

if [ ! -f "$project_file" ]; then
  echo "Error: Project file not found: $project_file"
  exit 1
fi

for input_file in tests/property/*.in; do
    base_name=$(basename "$input_file" .in)
    read -r alphabet < "$input_file"
    input=$(tail -n +2 "$input_file")

    result=$(dotnet run --project "$project_file" "-regex" "-alphabet" "$alphabet" "$input")
    
    symdif=$(dotnet run --project "$project_file" "-regex" "-alphabet" "$alphabet" "($result) &! ($result)")

    all_words=$(dotnet run --project "$project_file" "-regex" "-alphabet" "$alphabet" "($result) |! ($result)")

    mindfa=$(dotnet run --project "$project_file" "-mindfa" "-alphabet" "$alphabet" "$input")

    round_trip=$(dotnet run --project "$project_file" "-mindfa" "-alphabet" "$alphabet" "$mindfa")

    echo "$base_name:"
    symdif_expected="[]"
    if [ "$symdif" = "$symdif_expected" ]; then
        echo "    Symmetric difference: pass"
    else
        echo "    Symmetric difference:"
        echo "        Expected: $symdif_expected"
        echo "        Got $symdif"
    fi

    length=$(expr length "$alphabet")
    if [ "$length" -eq 1 ]; then
        all_words_expected="$alphabet*"
    else
        alphabet_with_pipes=$(echo "$alphabet" | sed 's/./&|/g' | sed 's/\\|/\\/g' | sed 's/\\\\/\\\\|/g' | sed 's/|$//')
        all_words_expected="($alphabet_with_pipes)*"
    fi

    if [ "$all_words" = "$all_words_expected" ]; then
        echo "    Language of all words over the alphabet: pass"
    else
        echo "    Language of all words over the alphabet:"
        echo "        Expected: $all_words_expected"
        echo "        Got $all_words"
    fi

    if [ "$round_trip" = "$mindfa" ]; then
        echo "    Round-trip: pass"
    else
        echo "    Round-trip:"
        echo "        Expected: $mindfa"
        echo "        Got: $round_trip"
    fi
done