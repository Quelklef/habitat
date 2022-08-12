#!/usr/bin/env bash
set -euo pipefail

# teehee
SECRET=$(nix eval --impure --expr '(import <secrets>).notion-loombot-secret' | cut -d\" -f2)

function help {
  echo 'Call with 3 arguments (prio, title, body)'
  echo 'Prio is one of: dlc, dl, kim, here'
  echo ' dlc : deadline + consequences'
  echo ' dl  : deadline'
  echo ' kim : keep in mind'
  echo ' here: here for when you need'
  exit 1
}

[ "$#" = 3 ] || help

prio=$1
title=$2
body=$3

case "$prio" in
  dlc)  prio='deadline + consequences' ;;
  dl)   prio='deadline' ;;
  kim)  prio='keep in mind' ;;
  here) prio='here for when you need' ;;
  *) help ;;
esac 

str_to_json() {
  echo -n "$1" | node -e 'console.log(JSON.stringify(require("fs").readFileSync(0, "utf-8")))'
}

json_prio=$(str_to_json "$prio")
json_title=$(str_to_json "$title")
json_body=$(str_to_json "$body")
json_now=$(str_to_json $(date --iso-8601=seconds))

payload=$(cat <<EOF
    {
      "parent": { "database_id": "4608ac8f08a540a1ad9ab993e632f3f1" },
      "properties": {
        "Title": { "title": [{ "text": { "content": ${json_title} } }] },
        "Priority": { "select": { "name": ${json_prio} } },
        "Loom time": { "date": {
          "start": ${json_now},
          "end": "9999-01-01T00:00:00Z"
        } }
      },
      "children": [
        {
          "object": "block",
          "type": "paragraph",
          "paragraph": {
            "rich_text": [
              {
                "type": "text",
                "text": {
                  "content": ${json_body}
                }
              }
            ]
          }
        }
      ]
    }
EOF
)

echo "$payload" | node -e '
  const j = require("fs").readFileSync(0, "utf-8");
  console.log(JSON.stringify(JSON.parse(j), null, 2));
'

curl \
  -X POST \
  'https://api.notion.com/v1/pages' \
  -H "Authorization: Bearer ${SECRET}" \
  -H "Content-Type: application/json" \
  -H "Notion-Version: 2022-02-22" \
  --data "$payload"
