module type PERMUTATION = S.PERMUTATION

module Permutation = struct
  module Poseidon = Poseidon
  module Rescue = Rescue
  module Anemoi = Anemoi
  module Griffin = Griffin
end

module Mode = struct
  module Jive = Jive
  (* module Sponge = Sponge *)
end
