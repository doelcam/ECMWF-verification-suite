#!/bin/bash
echo ""
echo "---------SUBMIT JOB TO CDP---------------"
echo ""
fileName=$1
suiteName="${fileName%.*}"

if [[ -n "$fileName" ]]; then
    echo "The suite to be submitted is "$fileName", going to CDP..."
else
    echo "ERROR: need to pass suite in as an argument (including extension - assume suite name is the same as the passed filename, minus the extension) "
echo ""
    exit
fi

echo ""
echo ""
cdp << EOF
ecgate
play -r/$suiteName $fileName
exit
EOF

