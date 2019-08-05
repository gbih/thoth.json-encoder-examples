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

//-------------------------------
// Primitives
//-------------------------------

SECTION()

"Primitives" |> logtitle

SECTION()

