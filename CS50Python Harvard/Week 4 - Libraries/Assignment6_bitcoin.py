import requests
import sys
import json

bitc = 0.00
try:
    bitc = float(sys.argv[1])
except IndexError:
    print("Missing command-line argument")
    sys.exit(1)
except ValueError:
    print("Command-line argument is not a number")
    sys.exit(1)

try:
    response = requests.get("https://rest.coincap.io/v3/assets/bitcoin?apiKey=311f94a6587b5483c126e94fdc4fac00ca5797f07e59911edd939d73846b94f6")
    o = response.json()
    price =float(o['data']['priceUsd'])
    #print("market_cap=", market_cap)
    #supply = float(o['data']['supply'])
    #print("supply = ", supply)
    btcValue = price*bitc
    print(f"${btcValue:,.4f}", end="")

except requests.RequestException as e:
    print("Exception = ", e)


