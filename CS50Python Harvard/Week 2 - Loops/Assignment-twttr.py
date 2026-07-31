def main():
    intext = input("Input:")
    outtext = take_vowels_out(intext)
    print("Output:", outtext)

def take_vowels_out(text):
    otext=""
    for ch in text:
        if ch not in "aeiouAEIOU":
            otext +=ch
    return otext

main()
