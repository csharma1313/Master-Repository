import pytest
from plates import is_valid

def test_2letter():
    assert is_valid("H1") == False
    assert is_valid("CS50") == True
    assert is_valid("1Ht") == False
    assert is_valid("22T") == False
    assert is_valid("CS05") == False

def test_length():
    assert is_valid("H") == False
    assert is_valid("HT456YOMP") == False
    assert is_valid("CAL50") == True

def test_numatend():
    assert is_valid("H009T") == False
    assert is_valid("AAA222") == True
    assert is_valid("12305") == False
    assert is_valid("CS5A") == False
    assert is_valid("AAA2B") == False

def test_nospchar():
    assert is_valid("CS#50") == False
    assert is_valid("CS 50") == False
    assert is_valid("CS.50") == False
