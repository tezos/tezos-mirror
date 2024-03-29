(env
 (release
  (flags
   (:standard -noassert))))

(documentation
 (package octez-libs)
 (mld_files tutorial))

(rule
 (targets tutorial.mld)
 (deps tutorial.md)
 (mode promote)
 (action
  (with-stdout-to
   %{targets}
   (run md2mld %{deps}))))
