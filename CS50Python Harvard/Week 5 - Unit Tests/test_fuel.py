from fuel import convert, gauge
import pytest

def test_convert():
    assert convert("3/4") == 75
    with pytest.raises(ValueError):
        convert("123")
    with pytest.raises(ValueError):
        convert("a/b")
    with pytest.raises(ZeroDivisionError):
        convert("1/0")
    with pytest.raises(ValueError):
        convert("3/2")
    with pytest.raises(ValueError):
        convert("-2/1")

def test_gauge():
    assert gauge(0) == "E"
    assert gauge(1) == "E"
    assert gauge(99) == "F"
    assert gauge(75) == "75%"

