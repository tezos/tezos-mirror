//   Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>
//   Copyright (c) 2023 Marigold <contact@marigold.dev>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

var crypto = require("crypto");
const { ethers } = require("ethers");
const { keccak256 } = require("@ethersproject/keccak256");
const fs = require("fs");
const { exit } = require("process");

const RPC = "http://127.0.0.1:8545";

// for (let index = 0; index < 100; index++) {
//   var id = crypto.randomBytes(32).toString("hex");
//   var privateKey = `"${id}",`;
//   console.log(privateKey);
// }

const PRIVATE_KEYS = [
  "470df6ff859fa8feccc990757c458ec152f4e73b2271af7f3332948aa84dc53d",
  "b2ebd997f03e592216947509c1badd08983341eb854a233aa10d287c369c969e",
  "3fdeee1c340311efe18211d60203c2925119edd0f9e981f7ed1deddd9d0d68ec",
  "9a9410bbed8f2e05e5388a69b5b9453c53bbe5e3faa9b9ef175b7a9a730987c9",
  "088346c722274f6cabf42389ce5cfef725e6d259e9ea45740476a2a85ca80ec2",
  "1adcc3684ff4adf5d8079a9af11272dbe6a95f80c4eeb26bb1db7da133766cc2",
  "d5fa3534f555bd46f8757b1b1abcf0800ae1f3431c851b112a283db7bdb72bed",
  "d4d9cb09fbd863a1af7d995212cdf264a4f7c23e9d1d96229dda8256362836b8",
  "afcfff371b3c87796594e6b2a5c1ead1831d9ed138ebe926ee121b0b425bd2a8",
  "67ed7bf8895d8b6e3145782d86070e855840213af4047a3b297e837de6e29980",
  "7011fd2be820897e9cc99c6b3129e600219a210d226e3e9a0373c7a242c00c03",
  "248d1aaff425f2d1127f4ea5f2d83789617ce665fea666434ff3b622a70b9d1b",
  "b952c17b40b7106624d60a65c1d87bb560286ba8e4483c4faf6bf8fbe94a5067",
  "6291065d6fd25f5a798a20c55692a25ac66d02ee134c7375fadad624c899c7cd",
  "402d3c8087e26a3f5eb597241ee2d151a6e3847c4cfd9149ba1b72156f5b4e14",
  "9e5153a0fb82f1d5685f1483a62a0d04a3210b3d268aee07cbf0cb666fa7a5d6",
  "0e831e9085c9027e07a012e4fe2bc51a01b3421dfa28a0f836799719ca6df2db",
  "ff6e1eba5bfce55d55a4f93499f351be1a56f7e9f32f4a9d7aca61a78ae109ee",
  "9a5dfe0798ee870ad8268de5cc1a195ac37902be01b7bb24cc57877fe2d3ac87",
  "26c08931a55f443e40349215e309b8bc5c6c02cf63a06cbd501aec335544fc90",
  "ce1149ae44c6159490209dc2fbeb0e33a20383465c1a3d2af27ac276985b2678",
  "ef4081d59286b140c8fdbc9fa50268203e0000516831d169f2e7a00242e05be0",
  "fa9de4ce099b59378d8ff16292272772d61a12d25ec229b526277bd72f6097b8",
  "f26fc297ee617a6b7d2867f061616fca3e02985fc13ff82efd0d665b45452382",
  "0288960031e7a504aff09c4052451e5f2f242628a9f220afa27237e236eed19b",
  "8ce51ec19eedc319c9eba42c6d1d22ecdf11132ce28845c7570c6d8c6148d1d9",
  "bd48be1bf6cc519730a2db4cc31495d708644e012577d1231e2457ebf1f04aa2",
  "8c894b9fd433ab6833009328491ed73c6cbced5dc03aca7f9842f0553d7cb29d",
  "fdcb1c60f1d1e9d34f7137b29b3656be1d3e894f931ca20ad5cfd98b8596e2fc",
  "bab967d57c16858d1728171f6b467b3ad3cffcd7552cf5b4e018cecb75371ed7",
  "bb2c4479962c66437060ae1442a47f280e5bf821dfe941a55518c79181eeda0e",
  "cfc873115dbd407fc4b1f043297bbc94a9d2cbde5378768d9d0dc27fbe246e92",
  "b98147766f544b60208471ccdb7215980f5df3ec88f22297de8b001338be2a3a",
  "55598ee31bc43c3bcf80e42f734f3ed18b70d9a585fcb02fd52ae04c30af8d4a",
  "e3999db923d876e9968672b07b02173418b22d8435d9de763a1798b28f0c3e54",
  "57f140d57550e955abc201a16c5ba6c121d05f4c362cf471dfa4f40905bdc98a",
  "adc082a8dcf400d97b76a7374aad4913ac2f3c4c18a040889b40dec078281c82",
  "a296e1b7388ac5bf1e0e62738a62a185afb6eb29baca6b5f86c20e4a35e865ef",
  "3eeb0422e89979ac48140748ac34eef08d52a72d971837545abc1e1571819ef3",
  "b17ba26ca510fba27164edd08df97d0ca24dbf092821245b952634ce450caeaa",
  "a6a904876416591b3babe75189941664fbaf7f888a6fa5af4f36442687b534d4",
  "45972a699fbcf66c0ae5f25c4f7ebd4eac828e675cbb9d74af6e128c97872c3f",
  "71b58a24a34bc737c51ec6feaa0c234fa09913ec3045a52759eb7475add176cb",
  "33573d202b0339934c68e97e83077a04a26f10456d45b977b593816ae4b2a458",
  "41a3e71c246b62972d564fa1a87df4dde6ad6254c811a6ac84f51555ceb8d93f",
  "1b9e58abc91fee592aea0d982eb4b5f5999da926193834b34bbc8d8426f28595",
  "8e7d3365f547cae15278e2cbf31ecc7a5de40815f294251bf5ed25230d8a13f1",
  "c4e38bdb9ea8fa4eecd464a677b39c9263f912787ce7ff42e9fed3d0d93e5cec",
  "cafdb852005362898e13dbbf51a91cf423e5b7ee7b5a7a2770704dade9654f19",
  "1c2672d0b54edb08373ad474203b7f2f695f6f1795b076c7f542530731ba72aa",
  "9a8548d3b33ce1e0c4a14fc86440a34693599011518ceff46f884bf38328eecc",
  "42000f7265dd9a4ded705d9bfd9467991c2ef0b0a1f26a2c3c0ac00d0ec7184a",
  "c44cb833b3cf12e7f9b56d887eb37813db5ecc4ea30eab2ebc7ffc0e075c76e5",
  "6349bac3b6b96734fda4502a480fed9d8b77baecfdc29e4183f271f118a47c0e",
  "d0bd11cbd4a0818226a074b551d2c2c2d012f058dadef4e08e0c3a3597bd4b8c",
  "618ce55e390e2695ce75ad1e752fbf8c9ea4972dc2192f3423610c36f7d7a444",
  "d45ecdd2f2ec7aaebfd104a6e115e94ae1131e4a82187879c5a5e07023828d08",
  "2401f8cf13d81cdbdb65de84500792e8c0cc4a8032abc6eb827cc1d00c39d245",
  "e9abc39eb250d631868a6d9b971ffd077b359e5af5ac012c05f323c6b36abb04",
  "d93261d34e340165fa0a3c475b469a43c750c8be584d636b81b8a0fbc7529b83",
  "80fa502428629be8c373db6b12aeaf3292d2568dd9e8affa8e25918dce37324d",
  "97d4ac7b614b4d186df45754b5b50b884034bab6dfc658566c09d3e83e499ccf",
  "9a91360595f586b650f03dc42b7cff8cb8f1fe8447036cd2cc381a032544e0db",
  "e0b7b5ef369181c37ea2832128eb92c82adf2a5fed43e4cfca100cd9aa77b745",
  "224f29efe90841d249893206c2c311409059d73dcede06dfdc618a787ce16c6c",
  "1e90cb3f942f96a742e1e0d16a7fec00db6a2cdc4a49009e7e631472dd4fcc03",
  "9d8776f1c25616ad446b534ec2599bf4233feac3c61cbbb1358799ed8062bdfd",
  "a578c344021821945fe5eedb90a6ca7ab7393269e902de68ceccd951264c87dc",
  "0cbc820100c7fc83f84f8775aea91b23eb7fde088288fb4abdba9d7ae5dabef8",
  "58bb9f21af6f6469d1c5c9901f108aebfe6dec3a620f47126640bd7cb68ce1c5",
  "6bf476f355ca33cd14cb7bf09dd888b02d98443aff8cc67f0b2aaf38a88489e0",
  "5d911804dbcf6c28a363199f6aa53538aae4fd90c04f9b1fb65c47037026554d",
  "2b019691631e430e9073f6d23decc134633d2255ba3348e1c3553f04243fa59e",
  "bb5d3cfd5926810971455fe576b289a4c5ff2c23dfcf6f9ef00226b63dc49e24",
  "13fd6d6703b2a92ed4c5bac8f53787b4dc5300a48602a4e52379470c989df50e",
  "35301ec1b1fc9a8c09a02885113c5fc41eba84dd97fcae1f5cdcb4064307e40d",
  "1d110bb08519055440fdd4708dd49917b41a2fd69a668c00688d861b3960a632",
  "1b7a85f74c55735bf82b005418d11073b86030a8724f18bcc01aa4843a4c0c87",
  "b01dc067791d226ad341c437d30f1bb802794c159378b83b4b6a52e7daf804a6",
  "ca4aa5d548173135bfe5f8961c656bcac488ceb79714312d4e46687252b5b9ac",
  "8b9b3599f15a2d00e7977a9138e47bc70b5809fe9d19c588f7f90698a29f7b06",
  "1d0254d3fb7a4151e2fb8ed3b6af79304f6aba9bf9a592a9c10752986da8ee22",
  "fc878f2ef9246b51aead7c01036f67adcd779ac059a63e052f555ecc374ced04",
  "c7ca0529af18ce15712dafa36215484a35503e8fa850cc846387d7a5d7b6ae3a",
  "b68ef5b01e50987208541378558c6e7c8886f4ba40851a841b2188bd160cb6f4",
  "f99d0d2f2659ac354d66a49f246fc76c5cee21449d926a352e517a77cd4bf599",
  "1196635da12521f921ee4604de1f887fd57bea3a5c1b650bae5911e4e25df755",
  "903a08284641eee505976e6d46a45e9d392a303082d2d976b492876528a4a220",
  "1acc9da81ddf3d2e449bee3c21436b824d5e848f44d5a50d92e59c33eadc1730",
  "72f2729d9f6dcfe467264b90bed4b8d61390f5bba848c6001319271107aef21b",
  "21038fd71cdd6951b3cf28433ce24ab5c3bf6d5bbb6fd623f0fb22ad888e666d",
  "d2595c340bfc15074fa04423cf78061d391931f899a6dcfa8a06223b2bfb9e27",
  "bac2c21e7da9573ddb1c16cb94dcca1e4e0dbe674a5ab6cf828ad16138130c38",
  "e72bff6080d7a4f37f0fa3d995435f464078886294c51f2235c469f2a597f6cf",
  "ab17f022e1c949caa2410cc23ca11365cdfba1b7197d8737297446679cd58a16",
  "c556096aafc51beae260d8f6b28938444abf630667d2099908956b56f3455831",
  "681adfa9aa9cc688adfae81422614ffc672d602123690970643310f95c146820",
  "750526c6c389fd2f2a5fae909d908063c604e3ffa255e2e77f03f9a7d4e24a3a",
  "d9490cd97964c41bc996ef154ab7b4c60804433595ce07958fd0ea350ff86ed8",
  "53a42bb2edb590470ca7b98ad0d6b030ae094096bbe9e899b7525c29e413ec49",
];

const provider = new ethers.providers.JsonRpcProvider(RPC);

const generate_operation = async () => {
  for (let i = 0; i < PRIVATE_KEYS.length; i++) {
    const privateKey = PRIVATE_KEYS[i];
    const signer = new ethers.Wallet(privateKey, provider);
    const operation = await signer.signTransaction({
      to: ethers.constants.AddressZero,
      value: 10,
      gasLimit: 0x5208,
      gasPrice: 0x5208,
      nonce: 0,
      chainId: 1337,
    });

    let hash = ethers.utils.keccak256(operation);
    console.log(`${operation.slice(2)} ${hash}`);
  }
};

generate_operation();
