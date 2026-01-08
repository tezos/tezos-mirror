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
     { parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }
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
    }

    /*
     (Pair :foo "string" 0 0x00)
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
    }

}
