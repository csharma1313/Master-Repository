def main():
    intext = input("Input:")
    outtext = shorten(intext)
    print("Output:", outtext)

def shorten(text):
    otext=""
    for ch in text:
        if ch not in "aeiouAEIOU":
            otext +=ch
    return otext

if __name__ == "__main__":
    main()
