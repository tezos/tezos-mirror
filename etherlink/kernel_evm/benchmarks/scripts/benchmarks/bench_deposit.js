let deposit_message = '{ "payload" : "Pair (Pair 0x119811f34ef4491014fbc3c969c426d37067d6a4 (Pair 0x013629df3a46f68006b1a8067285528fc0de6f818700 (Pair Unit 50000000))) (Pair 1 0x)","sender" : "KT1DXADSWXucAJ6PujZeUSK9brpToFtC9fz6" }'

let NUMBER_OF_DEPOSITS = 100;

console.log("[[");
for (let i = 0; i < NUMBER_OF_DEPOSITS - 1; i++) {
    console.log(deposit_message + ",")
}
console.log(deposit_message)
console.log("]]");
