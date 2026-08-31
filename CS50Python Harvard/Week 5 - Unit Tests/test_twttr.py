from twttr import shorten
import pytest

def test_lcase():
    assert shorten("apple") == "ppl"
    assert shorten("area") == "r"

def test_ucase():
    assert shorten("HAWAII") == "HW"
    assert shorten("HONOLULU") == "HNLL"

def test_str():
    assert shorten("123") == "123"
    assert shorten("h3456") == "h3456"

def test_punct():
    assert shorten("hello!") == "hll!"
    assert shorten("why?") == "why?"


