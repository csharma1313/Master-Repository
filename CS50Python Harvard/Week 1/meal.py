def main():
    t = input("What time is it?")
    hour = convert(t)
    if 7 <= hour <=8:
        print("breakfast time")
    elif 12 <= hour <=13:
        print("lunch time")
    elif 18 <= hour <=19:
        print("dinner time")

def convert(time):
   h, minute = time.split(":")
   return int(h)+int(minute)/60

if __name__ == "__main__":
    main()