def main():
    while True:
        try:
            frac = input("Fraction: ")
            perc = convert(frac)
            status = gauge(perc)
            print(status)
            break
        except:
            pass

def convert(fraction):
        N,D = fraction.split("/")
        N=int(N)
        D=int(D)
        if N > D:
            raise ValueError
        if N < 0:
            raise ValueError
        if D == 0:
            raise ValueError
        fuel = round((N/D)*100)
        return fuel

def gauge(fuel):
    if fuel <= 1:
        return "E"
    elif fuel >= 99:
        return"F"
    else:
        return str(fuel) + "%"

if __name__ == "__main__":
    main()
