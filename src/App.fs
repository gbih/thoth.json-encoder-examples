module ThothEncoderExamples
open Tests.Types

(**
This library is based on the Elm JSON decoder/encoder libraries:

https://guide.elm-lang.org/effects/json.html
http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode
https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest
*)

(**
Note for module names:
http://fsharp.org/specs/language-spec/3.0/FSharpSpec-3.0-final.pdf
Any sequence of characters that is enclosed in double-backtick marks (``   ``), 
excluding newlines,tabs , and double-back tick pairs themselves, is treated as 
an identifier. Note that when an identifier is used for the name of a type, 
union type case, module, or namespace, the following characters are not allowed 
even inside double-backtick marks:
‘.', '+', '$', '&', '[', ']', '/', '\\', '*', '\"', '`'
*)

module Print =
    open Fable.Core

    [<Emit("JSON.stringify($0,null,3)")>]
    let JSONStringify x : 'a = jsNative

    let elementId = "elmish-app"
    let elem = Browser.Dom.document.getElementById(elementId)
    elem.setAttribute("style", "color:black; margin:1rem; display: block;font-family: monospace;font-size:1.1rem;white-space: pre-wrap;"; )
    
    let p input =
        let x = input
        let showElement = Browser.Dom.document.createElement("span")
        showElement.innerHTML <- sprintf "%A\n" x
        Browser.Dom.document.getElementById(elementId).appendChild showElement |> ignore

    let title input =
        let x = input
        let showElement = Browser.Dom.document.createElement("span")
        showElement.setAttribute("style", "margin-bottom:1rem;font-style:italic;color:blue"; )
        showElement.innerHTML <- sprintf "%A\n" x
        Browser.Dom.document.getElementById(elementId).appendChild showElement |> ignore

    let json input =
        let x = input
        let showElement = Browser.Dom.document.createElement("span")
        showElement.setAttribute("style", "margin-bottom:1rem;font-style:italic;color:green"; )
        showElement.innerHTML <- sprintf "%A\n" x
        Browser.Dom.document.getElementById(elementId).appendChild showElement |> ignore

let log = Print.p
let logtitle = Print.title
let logjson = Print.json

let HR () = "- - - - - - - - - - - - - - - - - - -" |> log
let SECTION() = "= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =" |> log 

//-------------------------------

module Random =
    let rand = System.Random()
    let int n = rand.Next(n)
    let float n = n * rand.NextDouble()
    let string n = System.String(Array.init n (fun _ -> char (rand.Next(97,123))))
    let bool = rand.NextDouble() >= 0.5

//---------------------

let jsonRecord =
    """{ "a": 1.0,
         "b": 2.0,
         "c": 3.0,
         "d": 4.0,
         "e": 5.0,
         "f": 6.0,
         "g": 7.0,
         "h": 8.0 }"""

let jsonRecordInvalid =
    """{ "a": "invalid_a_field",
         "b": "invalid_a_field",
         "c": "invalid_a_field",
         "d": "invalid_a_field",
         "e": "invalid_a_field",
         "f": "invalid_a_field",
         "g": "invalid_a_field",
         "h": "invalid_a_field" }"""

type RecordWithPrivateConstructor = private { Foo1: int; Foo2: float }
type UnionWithPrivateConstructor = private Bar of string | Baz

//-------------------------------
// Basic
//-------------------------------

(**
toString definition:

let toString (space: int) (value: JsonValue) : string =
    JS.JSON.stringify(value, !!null, space)
*)

SECTION()

"Basic" |> logtitle

SECTION()

module ``a string works`` =
    open Thoth.Json
    // which is more idiomatic?
    let actual =
        Encode.string "maxime"
        |> Encode.toString 0

    // or this?
    let actual2 = 
        Encode.toString 0 (Encode.string "maxime")

    // Probably the first style, since it's easier to use for longer, more complicated cases.

"module ``a string works``" |> logtitle
``a string works``.actual |> log

HR()

module ``an int works`` =
    open Thoth.Json
    let actual =
        Encode.int 1
        |> Encode.toString 0

"module ``an int works``" |> logtitle
``an int works``.actual |> log

HR()

module ``a float works`` =
    open Thoth.Json
    let actual =
        Encode.float 1.2
        |> Encode.toString 4

"module ``a float works``" |> logtitle
``a float works``.actual |> log

HR()

module ``an array works`` =
    open Thoth.Json
    let actual =
        Encode.array
            [|
                Encode.string "maxime"
                Encode.int 2
            |]
        // Using this last makes more sense for longer constructs like this:
        |> Encode.toString 4

"module ``an array works``" |> logtitle
``an array works``.actual |> log

HR()

module ``a list works`` =
    open Thoth.Json
    let actual =
        Encode.list
            [
                Encode.string "maxime"
                Encode.int 2
            ]
        |> Encode.toString 4

"module ``a list works``" |> logtitle
``a list works``.actual |> log

HR()

module ``a bool works`` =
    open Thoth.Json
    let actual =
        Encode.bool true
        |> Encode.toString 4

"module ``a bool works``" |> logtitle
``a bool works``.actual |> log

HR()

module ``a null works`` =
    open Thoth.Json
    let actual =
        //Encode.nil
        Encode.nil
        |> Encode.toString 4

"``a null works``" |> logtitle
``a null works``.actual |> log

HR()

module ``an object works`` =
    open Thoth.Json
    let actual =
        Encode.object
            [
                ("firstname", Encode.string "maxime")
                ("age", Encode.int 25)
            ]
            |> Encode.toString 4

"module ``an object works``" |> logtitle
``an object works``.actual |> log

HR()

module ``a dict works`` =
    open Thoth.Json
    let actual =
        Map.ofList
            [
                ("a", Encode.int 1)
                ("b", Encode.string "2")
                ("c", Encode.bool true)
            ]
        |> Encode.dict
        |> Encode.toString 4

"module ``a dict works``" |> logtitle
``a dict works``.actual |> log

HR()

module ``a bigint works`` =
    open Thoth.Json
    let actual =
        Encode.bigint 12I
        |> Encode.toString 4

"module ``a bigint works``" |> logtitle
``a bigint works``.actual |> log

HR()

module ``a datetime works`` =
    open Thoth.Json
    open System
    let actual =
        //DateTime(2018, 10, 1, 11, 12, 15, DateTimeKind.Utc)
        (DateTime.UtcNow)
        |> Encode.datetime
        |> Encode.toString 4

"module ``a datetime works``" |> logtitle
``a datetime works``.actual |> log

HR()

module ``a datetimeOffset works`` =
    open Thoth.Json
    open System
    let actual =
        DateTimeOffset(2018, 7, 2, 12, 23, 45, 0, TimeSpan.FromHours(2.))
        |> Encode.datetimeOffset
        |> Encode.toString 4

"module ``a datetimeOffset works``" |> logtitle
``a datetimeOffset works``.actual |> log

HR()

// // get error of: The UTC Offset for Utc DateTime instances must be 0.
// module ``a datetimeOffset with DateTimeUtcNow works`` =
//     open Thoth.Json
//     open System
//     let actual =
//         DateTimeOffset(DateTime.UtcNow, TimeSpan.FromHours(1.))
//         |> Encode.datetimeOffset
//         |> Encode.toString 4

// "module ``a datetimeOffset with DateTimeUtcNow works``" |> logtitle
// ``a datetimeOffset with DateTimeUtcNow works``.actual |> log

// HR()

module ``a timeSpan works`` =
    open Thoth.Json
    open System
    let actual =
        TimeSpan(1, 2, 3, 4, 5)
        |> Encode.timespan
        |> Encode.toString 4

"DateTimeUtcNow" |> logtitle
``a timeSpan works``.actual |> log

HR()

module ``a decimal works`` =
    open Thoth.Json
    let actual =
        0.7833M
        |> Encode.decimal
        |> Encode.toString 4

"module ``a decimal works``" |> logtitle
``a decimal works``.actual |> log

HR()

module ``a guid works`` =
    open Thoth.Json
    open System
    let actual =
        Guid.Parse("1e5dee25-8558-4392-a9fb-aae03f81068f")
        |> Encode.guid
        |> Encode.toString 4

"module ``a guid works``" |> logtitle
``a guid works``.actual |> log

HR()

module ``a guid works using NewGuid`` =
    open Thoth.Json
    open System
    let value = Guid.NewGuid()
    let actual =
        value
        |> Encode.guid
        |> Encode.toString 4

"module ``a guid works using NewGuid``" |> logtitle
``a guid works using NewGuid``.actual |> log

HR()

module ``an int64 works`` =
    open Thoth.Json
    let actual =
        79223209L
        |> Encode.int64
        |> Encode.toString 4

"module ``an int64 works``" |> logtitle
``an int64 works``.actual |>log

HR()

module ``an uint64 works`` =
    open Thoth.Json
    let actual =
        79223209UL
        |> Encode.uint64
        |> Encode.toString 4

"module ``an uint64 works``" |> logtitle
``an uint64 works``.actual |> log

HR()

module ``a tuple2 works`` =
    open Thoth.Json
    let actual =
        Encode.tuple2
            Encode.int
            Encode.string
            (1, "maxime")
        |> Encode.toString 4

"module ``a tuple2 works``" |> logtitle
``a tuple2 works``.actual |> log

HR()

module ``a tuple3 works`` =
    open Thoth.Json
    let actual =
        Encode.tuple3
            Encode.int
            Encode.string
            Encode.float
            (1, "maxime", 2.5)
        |> Encode.toString 4

"module ``a tuple3 works``" |> logtitle
``a tuple3 works``.actual |> log

HR()

module ``a tuple4 works`` =
    open Thoth.Json
    let actual =
        Encode.tuple4
            Encode.int
            Encode.string
            Encode.float
            SmallRecord.Encoder
            (1, "maxime", 2.5, { fieldA = "test" })
        |> Encode.toString 4

"module ``a tuple4 works``" |> logtitle
``a tuple4 works``.actual |> log

HR()

module ``a tuple5 works`` =
    open Thoth.Json
    open System
    let actual =
        Encode.tuple5
            Encode.int
            Encode.string
            Encode.float
            SmallRecord.Encoder
            Encode.datetime
            (1, "maxime", 2.5, { fieldA = "test" }, DateTime.UtcNow)
        |> Encode.toString 4

"module ``a tuple5 works``" |> logtitle
``a tuple5 works``.actual |> log

HR()

// To do: tuple6 -- tuple8

module ``using pretty space works`` =
    open Thoth.Json
    let actual =
        Encode.object
            [
                ("firstname", Encode.string "maxime")
                ("age", Encode.int 25)
            ]
            |> Encode.toString 4

"module ``using pretty space works``" |> logtitle
``using pretty space works``.actual |> log

HR()

module ``complex structure works`` =
    open Thoth.Json
    let actual =
        Encode.object
            [
                ("firstname", Encode.string "maxime")
                ("age", Encode.int 25)
                ("address", Encode.object
                    [ 
                        "street", Encode.string "12 Main Road"
                        "city", Encode.string "Bordeaux"
                    ]
                )
            ]
            |> Encode.toString 4

"module ``complex structure works``" |> logtitle
``complex structure works``.actual |> log

HR()

module ``option with a value Some works`` =
    open Thoth.Json
    let actual =
        Encode.object
            [
                ("id", Encode.int 1)
                ("operator", Encode.option Encode.string (Some "maxime"))
            ]
            |> Encode.toString 4

"module ``option with a value Some works``" |> logtitle
``option with a value Some works``.actual |> log

HR()

module ``option without value None works`` =
    open Thoth.Json
    let actual =
        Encode.object
            [
                ("Id", Encode.int 1)
                ("operator", Encode.option Encode.string None)
            ]
            |> Encode.toString 4

"module ``option without value None works``" |> logtitle
``option without value None works``.actual |> log

HR()

module ``by default we keep the case defined in type`` =
    open Thoth.Json
    let value =
        {
            Id = 0
            Name = "Maxime"
            Email = "mail@test.com"
            followers = 33
        }
    let actual =
        Encode.Auto.toString(4, value)

"module ``by default we keep the case defined in type``" |> logtitle
``by default we keep the case defined in type``.actual |> log

SECTION()

"Auto Encoders" |> log

SECTION()

module ``forceCamelCase works`` =
    open Thoth.Json
    let value =
        {
            Id = 0
            Name = "Maxime"
            Email = "mail@test.com"
            followers = 33
        }
    let actual =
        Encode.Auto.toString(4, value, true)

"module ``forceCamelCase works``" |> logtitle
``forceCamelCase works``.actual |> log

HR()

module ``Encode Auto generateEncoder works`` =
    open Thoth.Json
    open System
    let value = 
        { 
            a = 5
            b = "bar"
            c = [false, 3; true, 5; false, 10]
            d = [|Some(Foo 14); None|]
            e = Map [("oh", { a = 2.; b = 2. }); ("ah", { a = -1.5; b = 0. })]
            f = DateTime(2018, 11, 28, 11, 10, 29, DateTimeKind.Utc)
            g = set [{ a = 2.; b = 2. }; { a = -1.5; b = 0. }]
            h = TimeSpan.FromSeconds(5.)
        }
    let encoder = 
        Encode.Auto.generateEncoder<Record9>()
    let actual = 
        encoder value
        |> Encode.toString 4

"module ``Encode Auto generateEncoder works``" |> logtitle
``Encode Auto generateEncoder works``.actual |> log

HR()

module ``Encode Auto generateEncoderCached works`` =
    open Thoth.Json
    open System
    let value = 
        { 
            a = 5
            b = "bar"
            c = [false, 3; true, 5; false, 10]
            d = [|Some(Foo 14); None|]
            e = Map [("oh", { a = 2.; b = 2. }); ("ah", { a = -1.5; b = 0. })]
            f = DateTime(2018, 11, 28, 11, 10, 29, DateTimeKind.Utc)
            g = set [{ a = 2.; b = 2. }; { a = -1.5; b = 0. }]
            h = TimeSpan.FromSeconds(5.)
        }
    let encoder1 = Encode.Auto.generateEncoderCached<Record9>()
    let encoder2 = Encode.Auto.generateEncoderCached<Record9>()
    let actual1 = encoder1 value |> Encode.toString 0
    let actual2 = encoder2 value |> Encode.toString 0

"module ``Encode Auto generateEncoderCached works``" |> logtitle
``Encode Auto generateEncoderCached works``.actual1 |> log
``Encode Auto generateEncoderCached works``.actual2 |> log

HR()

module ``Encode Auto toString works with bigint extra`` =
    open Thoth.Json
    let extra =
        Extra.empty
        |> Extra.withBigInt
    let value = { bigintField = 9999999999999999999999I }
    let actual =
        Encode.Auto.toString(4, value, extra=extra)

"module ``Encode Auto toString works with bigint extra``" |> logtitle
``Encode Auto toString works with bigint extra``.actual |> log

HR()

module ``Encode Auto toString works with custom extra`` =
    open Thoth.Json
    let extra =
        Extra.empty
        |> Extra.withCustom ChildType.Encode ChildType.Decoder
    let value = { ParentField = { ChildField = "bumbabon" }}
    let actual =
        Encode.Auto.toString(4, value, extra=extra)

"module ``Encode Auto toString works with custom extra``" |> logtitle
``Encode Auto toString works with custom extra``.actual |> log

HR()

module ``Encode Auto toString serializes maps with Guid keys as JSON objects`` =
    open Thoth.Json
    open System
    let m = Map [Guid.NewGuid(), 1; Guid.NewGuid(), 2]
    let actual = Encode.Auto.toString(4, m)

"module ``Encode Auto toString serializes maps with Guid keys as JSON objects``" |> logtitle
``Encode Auto toString serializes maps with Guid keys as JSON objects``.actual |> log

HR()

module ``Encode Auto toString works with records with private constructors`` =
    open Thoth.Json
    // Foo1 and Foo2 are private
    let x = { Foo1 = 5; Foo2 = 7.8 } : RecordWithPrivateConstructor
    let actual = 
        Encode.Auto.toString(4, x, isCamelCase=true)
"module ``Encode Auto toString works with records with private constructors``" |> logtitle
``Encode Auto toString works with records with private constructors``.actual |> log

HR()

