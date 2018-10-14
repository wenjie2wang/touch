#!/bin/bash

# Note: this script is should be sourced from the project root directory

set -e

if [ "$(uname)" != "Linux" ]; then
    printf "Remeber to update date and version number.\n"
else
    printf "Updating date, version, and copyright year.\n"

    # define some variables
    yr=$(date +%Y)
    dt=$(date +%Y-%m-%d)
    citation=inst/CITATION
    version=$(grep "Version" DESCRIPTION | awk '{print $NF}')

    # update date in DESCRIPTION
    regexp1="s/Date: [0-9]{4}-[0-9]{1,2}-[0-9]{1,2}/Date: $dt/"
    sed -i -E "$regexp2" DESCRIPTION

    # update version and year in citation
    regexp2="s/version ([0-9]+\.*)+/version $version/"
    sed -i -E "$regexp3" $citation
    regexp3="/Manual/,/^\)$/ s/20[0-9]{2}/$yr/"
    sed -i -E "$regexp4" $citation

    # done
    printf "All updated.\n"
fi
