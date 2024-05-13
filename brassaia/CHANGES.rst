
Development Changelog
'''''''''''''''''''''

This list all changed to Brassaia still behind feature flag (or not used by
default in Octez yet).

Added
-----

- Brassaia:
    - Add pretty printers for `Commit`, `Tree`, `Info`, `Status`, `Branch` when using `utop` (backported from mirage/irmin #1839) (MR :gl:`!12902`)

- Index:
  - Vendor `mirage/index` (MR :gl:`!13281`)

Changed
-------

Deprecated
----------

Removed
-------

Fixed
-----

- Brassaia-Pack:
    - Fix index integrity check for v3 stores (backported from mirage/irmin #2267)  (MR :gl:`!12902`)

Security
--------