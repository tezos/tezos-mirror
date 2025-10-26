# SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT
{ fetchurl }:
{
  g1 = fetchurl {
    url = "https://assets.nomadic-labs.cloud/dal_trusted_setup/srsu_g1";
    sha256 = "c48ce4add1de2a7561108f17bf0c16bc1e93c0bff24bc7da465c24e0b4b2653e";
  };

  g2 = fetchurl {
    url = "https://assets.nomadic-labs.cloud/dal_trusted_setup/srsu_g2";
    sha256 = "e7fbe747ae3648a5b664d8f8bd7c524996f7ed07f3331f905d2e73767d580f7c";
  };
}
