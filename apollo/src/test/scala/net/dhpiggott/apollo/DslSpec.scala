package net.dhpiggott.apollo

import fastparse._
import zio.test.Assertion._
import zio.test._

object DslSpec extends DefaultRunnableSpec {
  def spec = suite("DslSpec")(
    suite("InstrumentCall")(
      suite("InstrumentInstance")(
        test("unnamed")(
          assertParsesAs(
            """piano:""",
            InstrumentCall.parse(_),
            InstrumentCall.InstrumentInstance("piano", nickname = None)
          )
        ),
        test("named")(
          assertParsesAs(
            """piano "nickname":""",
            InstrumentCall.parse(_),
            InstrumentCall
              .InstrumentInstance("piano", nickname = Some("nickname"))
          )
        )
      ),
      suite("InstrumentGroup")(
        test("unnamed")(
          assertParsesAs(
            """piano/guitar:""",
            InstrumentCall.parse(_),
            InstrumentCall
              .InstrumentGroup(Seq("piano", "guitar"), groupAlias = None)
          )
        ),
        test("named")(
          assertParsesAs(
            """piano/guitar "groupAlias":""",
            InstrumentCall.parse(_),
            InstrumentCall.InstrumentGroup(
              Seq("piano", "guitar"),
              groupAlias = Some("groupAlias")
            )
          )
        )
      ),
      test("InstrumentGroupMember")(
        assertParsesAs(
          """groupAlias.piano:""",
          InstrumentCall.parse(_),
          InstrumentCall.InstrumentGroupMember(
            "groupAlias",
            stockInstrumentNameOrNickname = "piano"
          )
        )
      )
    )
  )

  private[this] def assertParsesAs[A](
      input: String,
      parser: P[_] => P[A],
      expectedValue: A
  ): TestResult =
    assert(parse(input, parser))(
      isSubtype[Parsed.Success[A]](
        hasField(
          "value",
          _.value,
          equalTo(expectedValue)
        )
      )
    )
}
