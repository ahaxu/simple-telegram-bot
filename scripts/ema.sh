#!/usr/bin/env bash

# usage:
#
# export TA_API_KEY="your taapi key"
# export CHAT_ID="your chat id"
# export TELEGRAM_BOT_KEY="your telegram bot key"
# ./scripts/ema.sh "AHA/USDT" "1h" 12 2

SYMBOL=$1
INTERVAL=$2
BACKTRACKS=$3
THRESHOLD_PERCENTAGE=$4

INDICATOR="ema"
ENDPOINT="https://api.taapi.io/$INDICATOR"
URL="$ENDPOINT?secret=$TA_API_KEY&exchange=binance&symbol=$SYMBOL&interval=$INTERVAL&backtracks=$BACKTRACKS"
RESPONSE=`curl -s $URL | jq`
FIRST_BACKTRACK=`echo $RESPONSE | jq '.[0].value|tonumber'`
LAST_BACKTRACK=`echo $RESPONSE | jq '.[-1].value|tonumber'`

GAP=`echo "$FIRST_BACKTRACK-$LAST_BACKTRACK" | bc -l`
echo "first backtrack $FIRST_BACKTRACK"
echo "last backtrack $LAST_BACKTRACK"
echo "gap $GAP"

TREND="up"
BASE=$FIRST_BACKTRACK
if (( $(echo "$FIRST_BACKTRACK > $LAST_BACKTRACK" | bc -l) )); then
  TREND="down"
  BASE=LAST_BACKTRACK
fi
ABS_GAP=${GAP#-}
PERCENTAGE=`echo "$ABS_GAP/$BASE*100" | bc -l`

echo "trend $TREND"
echo "percentage $PERCENTAGE"
echo "base $BASE"

if (( $(echo "$PERCENTAGE >= $THRESHOLD_PERCENTAGE" | bc -l) )); then
    MSG="symbol: $SYMBOL trend: $TREND percentage: $PERCENTAGE base: $BASE gap: $GAP"
    TELEGRAM_API_URL="https://api.telegram.org/$TELEGRAM_BOT_KEY/sendmessage"
    curl -s $TELEGRAM_API_URL \
        --data-urlencode "chat_id=$CHAT_ID" \
        --data-urlencode "text=$MSG" | jq
else
    echo "not reach threshold yet!"
fi

