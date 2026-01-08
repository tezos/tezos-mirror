# Tezos SDK for Python

This SDK is designed to help Python developers interact with the Tezos blockchain.

## Usage

```python
from tezos import PublicKeyP256, P256Signature, forge_message

public_key = PublicKeyP256.from_b58check("p2pk68MV9UsLUvtAyWjSZne2VpxFxhur9a8fUXUPY2RFkzixXDmnY5G")
signature = P256Signature.from_b58check("p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD")
message = forge_message("a")

public_key.verify_signature(signature, message)
```
