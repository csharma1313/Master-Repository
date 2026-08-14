import random

def main():
    count = 0
    attempt = 0
    score = 0
    n = get_level()
    while count < 10:
        x,y = generate_integer(n)
        z = x + y
        count +=1
        while attempt < 3:
            try:
                print(x,"+",y,"=",end="")
                ans=int(input())
                if ans == z:
                    score +=1
                    break
                else:
                    print("EEE")
                    if attempt == 2:
                        print(x,"+",y,"=",z)
            except:
                print("EEE")
                if attempt == 2:
                    print(x,"+",y,"=",z)
                pass
            attempt +=1
        attempt = 0
    print("Score:", score)

def get_level():
    while True:
        try:
            lev = int(input("Level:"))
            if lev == 1 or lev == 2 or lev == 3:
                return lev
        except:
            pass

def generate_integer(n):
        if n == 1:
            x = random.randint(0,9)
            y = random.randint(0,9)
        elif n == 2:
            x = random.randint(10,99)
            y = random.randint(10,99)
        elif n ==3:
            x = random.randint(100,999)
            y = random.randint(100,999)
        else:
            return Exception
        return x,y

if __name__ == "__main__":
    main()
