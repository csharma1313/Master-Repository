import sys
from pyfiglet import Figlet

figlet = Figlet()
fontlist = figlet.getFonts()

if len(sys.argv) == 3 and (sys.argv[1] == "-f" or sys.argv[1] == "--font"):
    fontname = sys.argv[2]
    if fontname in fontlist:
        text = str(input("Input:"))
        f = Figlet(font = fontname)
        print(f.renderText(text))
    else:
        print("Invalid usage")
        sys.exit(1)
elif len(sys.argv) == 1:
    text = str(input("Input:"))
    print(figlet.renderText(text))
else:
    print("Invalid usage")
    sys.exit(1)
