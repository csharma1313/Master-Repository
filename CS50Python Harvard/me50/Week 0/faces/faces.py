def convert(text):
    return text.replace(":)","🙂").replace(":(","🙁")

def main():
    textA = input("User please provide input:")
    textB = convert(textA)
    print(textB)

main()