import inflect

p = inflect.engine()
namelist = []

try:
    while True:
        name = input("Name:")
        namelist.append(name)
except EOFError:
        print()
        print("Adieu, adieu, to " + p.join(namelist))

