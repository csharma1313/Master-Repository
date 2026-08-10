def main():
    hello("world")
    goodbye("world")

def hello(name):
    print(f"hello, {name}")

def goodbye(name):
    print(f"goodbye, {name}")

if __name__ == "__main__":
    main()

#__name__ is a special var that python automatically creates. 
# its value depends on how the pythoin file is being used
#if you run the file directly python sets __name__ = "__main__"
#So if __name__ == "__main__":
#    main()
#becomes
#if True:
#    main()