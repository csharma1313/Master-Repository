from random import randint
from random import randrange
import sys

while True:
    try:
        n = int(input("Level:"))
        if n > 0:
            break
    except ValueError:
            pass

randomNumber = randint(1,n)

while True:
    try:
        guessNumber = int(input("Guess:"))
        if guessNumber <= 0:
            continue

        if guessNumber < randomNumber:
            print("Too small!")
        elif guessNumber > randomNumber:
            print("Too large!")
        else:
            print("Just right!")
            break
    except ValueError:
        pass

