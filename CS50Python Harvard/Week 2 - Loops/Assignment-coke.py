amt = 50

while amt > 0:
    print("Amount Due:", amt)
    coin = int(input("Insert Coin: "))
    if coin in (25, 10 ,5):
        amt -= coin
    else:
        continue
print("Change Owed:",abs(amt))