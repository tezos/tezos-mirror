
This page details the structure and layout of messages used by the Tezos P2P layer.

The elementary types used for message or header fields are documented in the `Data_encoding <https://nomadic-labs.gitlab.io/data-encoding/data-encoding/Data_encoding/index.html>`_ module, for example:

- `unsigned 8-bit integer <https://nomadic-labs.gitlab.io/data-encoding/data-encoding/Data_encoding/index.html#val-uint8>`_
- `unsigned 16-bit integer <https://nomadic-labs.gitlab.io/data-encoding/data-encoding/Data_encoding/index.html#val-uint16>`_
- `signed 32-bit integer <https://nomadic-labs.gitlab.io/data-encoding/data-encoding/Data_encoding/index.html#val-int32>`_
- `signed 64-bit integer <https://nomadic-labs.gitlab.io/data-encoding/data-encoding/Data_encoding/index.html#val-int64>`_

Additionally, the type ``unsigned 30-bit integer`` is used to denote the subset of positive integers of type `signed 31-bit integers <https://nomadic-labs.gitlab.io/data-encoding/data-encoding/Data_encoding/index.html#val-int31>`_.

.. note::

  **31-bit integers**: The internal representation of integers in OCaml reserves one bit for GC tagging. The remaining bits encode a signed integer. For compatibility with 32-bit machines, we restrict these native integers to the 31-bit range.
