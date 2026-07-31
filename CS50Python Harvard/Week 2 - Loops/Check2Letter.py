def check2letter(t):
    if t[0].isalpha() and t[1].isalpha():
        return True
    else:
        return False
def main():
    plate = input("Plate: ")
    v1 = check2letter(plate)
    print("v1 =", v1)
    if v1:
        print("Valid")
    else:
        print("Invalid")

main()
