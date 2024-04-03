from dotenv import load_dotenv
from getpass import getpass
import os


def load_or_ask(var_name: str, is_secret: bool = False) -> str:
    load_dotenv()
    ask = getpass if is_secret else input
    return os.getenv(var_name) or ask(f'Enter {var_name}: ')

