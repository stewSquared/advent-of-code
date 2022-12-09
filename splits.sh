#!/usr/bin/bash

aoc-day () {
    jq -r --arg day $1 '.members | .[] | (
	.completion_day_level | select(has($day)) | .[$day]
	| .[] | .get_star_ts | tostring | strptime("%s")
	| ((strftime("%d") | tonumber) - ($day | tonumber)) as $days
	| (strftime("%H") | tonumber) as $hours
	| ($days * 24 + $hours | tostring) + strftime(":%M:%S")
    ) + " - " + .name' | sort -h
}

aoc-all-days () {
    local -r json=$1
    for DAY in {1..25}
    do
        echo "Day $DAY:"
        aoc-day $DAY <$json
    done
}
