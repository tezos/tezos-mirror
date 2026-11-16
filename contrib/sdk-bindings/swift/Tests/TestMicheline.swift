// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

import XCTest
import Tezos

class TestMicheline: XCTestCase {

    public static var allTests = [
      ("testBuildBasicScriptMicheline", testBuildBasicScriptMicheline),
      ("testBuildSimpleDataMicheline", testBuildSimpleDataMicheline),
    ]

    /*
     octez-client convert data "
     { parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }
     " from Michelson to binary
     */
    func testBuildBasicScriptMicheline() {
        let micheline = Micheline.seq(
          seq: [
            Micheline.app(
              prim: Prim.kParameter,
              seq: [Micheline.app(prim: Prim.tUnit, seq: [], annots: [])],
              annots: []
            ),
            Micheline.app(
              prim: Prim.kStorage,
              seq: [Micheline.app(prim: Prim.tUnit, seq: [], annots: [])],
              annots: []
            ),
            Micheline.app(
              prim: Prim.kCode,
              seq: [
                Micheline.seq(
                  seq: [
                    Micheline.app(prim: Prim.iCdr, seq: [], annots: []),
                    Micheline.app(
                      prim: Prim.iNil,
                      seq: [Micheline.app(prim: Prim.tOperation, seq: [], annots: [])],
                      annots: []
                    ),
                    Micheline.app(prim: Prim.iPair, seq: [], annots: []),
                  ])
              ],
              annots: []
            ),
          ])

        let michelineManager = MichelineManager()

        let parsedMicheline = try! michelineManager.parse(
          micheline:
            "{ parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }"
        )
        XCTAssertTrue(michelineManager.equalMicheline(micheline1: micheline, micheline2: parsedMicheline))

        let rawMicheline = try! michelineManager.encode(micheline: micheline)
        let expectedBytes: [UInt8] = [
          0x02, 0x00, 0x00, 0x00, 0x17, 0x05, 0x00, 0x03, 0x6c, 0x05,
          0x01, 0x03, 0x6c, 0x05, 0x02, 0x02, 0x00, 0x00, 0x00, 0x08,
          0x03, 0x17, 0x05, 0x3d, 0x03, 0x6d, 0x03, 0x42
        ]
        XCTAssertEqual(Array(rawMicheline), expectedBytes)
    }

    /*
     octez-client convert data '(Pair :foo "string" 0 0x00)' from Michelson to binary
    */
    func testBuildSimpleDataMicheline() {
        let micheline = Micheline.app(
          prim: Prim.dPair,
          seq: [
            Micheline.string(string: "string"),
            Micheline.int(bigInt: BigInt.fromInt(num: 0)),
            Micheline.bytes(bytes: Data([0x00])),
          ],
          annots: [Annotation.type("foo")]
        )

        let michelineManager = MichelineManager()

        let parsedMicheline = try! michelineManager.parse(
          micheline: "(Pair :foo \"string\" 0 0x00)"
        )
        XCTAssertTrue(michelineManager.equalMicheline(micheline1: micheline, micheline2: parsedMicheline))

        let rawMicheline = try! michelineManager.encode(micheline: micheline)
        let expectedBytes: [UInt8] = [
          0x09, 0x07, 0x00, 0x00, 0x00, 0x13, 0x01, 0x00, 0x00, 0x00,
          0x06, 0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x00, 0x00, 0x0a,
          0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x04, 0x3a,
          0x66, 0x6f, 0x6f
        ]
        XCTAssertEqual(Array(rawMicheline), expectedBytes)
    }

}
