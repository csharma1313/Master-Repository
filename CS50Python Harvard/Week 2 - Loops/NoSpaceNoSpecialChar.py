def no_space_nospchar(text):
    v1 = True
    v2 = True
    validity = ""
    for ch in text:
        if ch.isspace() == True:
            v1 = False

        if ch.isalnum() == False:
            v2 = False

    if v1 == False or v2 == False:
        validity = False
    else:
        validity = True
    return validity


def main():
    plate = input("Plate: ")
    v3 = no_space_nospchar(plate)
    if v3:
        print("Valid")
    else:
        print("Invalid")

main()
