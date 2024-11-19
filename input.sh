aoc-input () {
    local -r year=$1
    local -r day=$2
    local -r source="https://adventofcode.com/$year/day/$day/input"
    local -r dest="src/main/resources/$year/day-`printf %02d $day`.txt"
    echo "reading from $source"
    echo "writing to $dest"
    curl --cookie session=$AOC_SESSION $source >| $dest
}
