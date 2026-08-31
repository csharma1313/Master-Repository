def main():
      p = input("Plate: ")
      ans = is_valid(p)
      if ans == True:
          print("Valid")
      else:
          print("Invalid")

def is_valid(plate):
    v1 = check2letter(plate)
    v2 = checklength(plate)
    v3 = numatend(plate)
    v4 = no_space_nospchar(plate)
    if v1 and v2 and v3 and v4:
        return True
    else:
        return False


def check2letter(t):
    if len(t) >= 2 and t[0].isalpha() and t[1].isalpha():
        return True
    else:
        return False

def checklength(t):
    if len(t) > 6 or len(t) < 2:
        return False
    else:
        return True

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

def no_space_nospchar(text):
    val1 = True
    val2 = True
    validity = ""
    for ch in text:
        if ch.isspace() == True:
            val1 = False

        if ch.isalnum() == False:
            val2 = False

    if val1 == False or val2 == False:
        validity = False
    else:
        validity = True
    return validity



if __name__ == "__main__":
    main()
