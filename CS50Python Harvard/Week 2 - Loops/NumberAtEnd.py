def numatend(t):
    number_started = False 

    for ch in t:
        if ch.isdigit():

            if not number_started:
                number_started = True

                if ch =="0":
                    return False

        elif ch.isalpha():
            if number_started:
                return False
        else:
            return False
    return True

def main():
    plate = input("Plate: ")
    v3 = numatend(plate)
    if v3:
        print("Valid")
    else:
        print("Invalid")

main()
