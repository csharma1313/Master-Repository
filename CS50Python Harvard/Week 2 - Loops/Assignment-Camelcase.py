def main():
    varA = input("camelCase:")
    output = converttext(varA)
    print("snake_case: ", output)

def converttext(text):
    rettext = ""
    for ch in text:
        if ch.isupper():
            rettext += "_"+ch.lower()
        else:
            rettext += ch
    return rettext

main()
