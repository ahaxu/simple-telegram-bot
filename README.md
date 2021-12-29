### Simple telegram bot to notify about EMA signal

- TAAPI https://taapi.io/indicators/exponential-moving-average/

### TODO
- [ ] Refactor code using Monad transformer
- [ ] Article or video to explain about
    - [ ] Items in the order
    ```
        > Function composition
        > Monoid 
        > Functor 
        > Applicative 
        > Monad 
        > Why we said monad can't compose 
        > Monad Transformer
    ```
    - [ ] Monad transformer

### Export env variables

```
# taapi key
export TA_API_KEY="paste your ta api key here"

# telegram chanel/group id
export CHAT_ID="paste your telegram chat group/chanel here"

# bot_key in format botxyz:AAAAA
export TELEGRAM_BOT_KEY="paste your telegram bot key here"

```

### Run haskell binary file
```
# cabal run simple-telegram-bot \
    $SYMBOL \
    $INTERVAL \
    $BACKTRACKS \
    $THRESHOLD_PERCENTAGE

cabal run simple-telegram-bot "BTC/USDT" "1h" 24 1

```

### Run bash script
link to [ema.sh](./scripts/ema.sh) file
```
# need to export env variables as above step
./scripts/ema.sh "BTC/USDT" "1h" 12 2
```
