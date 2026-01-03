from datetime import datetime


def hello(name: str):
    print(f"{datetime.now()} - Hello {name.upper()}")
