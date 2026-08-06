def main():
    while True:
        try:
            fraction = input("Fraction: ")
            N,D = fraction.split("/")
            fuel = int((N/D)*100)
            if fuel <= 0:
                print("E")
            elif fuel >= 99:
                print("F")
            else: 
                print(fuel,"%")
        except ValueError, ZeroDivisionError:
            pass

main()