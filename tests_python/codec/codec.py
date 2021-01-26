import os
import subprocess
import json
import sys

from typing import List
from process.process_utils import format_command


class Codec:
    """Codec for datatypes used in Tezos.

    This class can be used to encode and decode datatypes used in a
    Tezos node.

    This class offers two commands `encode` and `decode`. The encoding
    name can be obtained using `tezos-codec list encodings`.
    """

    def __init__(self, codec_path: str):
        """
        Args:
            codec_path: path to the codec executable file
        """
        assert os.path.isfile(codec_path), f'{codec_path} is not a file'

        self._codec = codec_path

    def run(self, params: List[str], check: bool = True) -> str:
        """
        Args:
            params (list): list of parameters given to the tezos-codec,
            check (bool): raises an exception if codec call fails
        """
        cmd = [self._codec] + params
        print(format_command(cmd))

        stdout = ""
        stderr = ""
        completed_process = subprocess.run(
            cmd, capture_output=True, check=check, text=True
        )
        stdout = completed_process.stdout
        stderr = completed_process.stderr
        if stdout:
            print(stdout)
        if stderr:
            print(stderr, file=sys.stderr)

        if check:
            completed_process.check_returncode()

        return stdout.rstrip()

    def encode(self, encoding: str, data_json: dict) -> str:
        """
        Args:
           encoding (str): name of the encoding
           data_json (dict): data which needs to be encoded
        """
        data = json.dumps(data_json)
        cmd = ['encode', encoding, 'from', data]
        return self.run(cmd)

    def decode(self, encoding: str, data: str) -> str:
        """
        Args:
           encoding (str): name of the encoding
           data_json (dict): data which needs to be decoded in hex
        """
        cmd = ['decode', encoding, 'from', data]
        return json.loads(self.run(cmd))
