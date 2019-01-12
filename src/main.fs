module Spreadsheet

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop
open Fable.Import

open Evaluator

// ----------------------------------------------------------------------------
// DOMAIN MODEL
// ----------------------------------------------------------------------------

type Event =
  | UpdateValue of Position * string
  | StartEdit of Position

type State =
  { Rows : int list
    Cols : char list
    Active : Position option
    Cells : Map<Position, string> }

// ----------------------------------------------------------------------------
// EVENT HANDLING
// ----------------------------------------------------------------------------

let update msg state = 
  match msg with
  | UpdateValue (pos, value) ->
    let newCells = state.Cells |> Map.add pos value
    { state with Cells = newCells }, Cmd.Empty
  // state, Cmd.Empty
  | StartEdit pos ->
    { state with Active = Some pos }, Cmd.Empty

// ----------------------------------------------------------------------------
// RENDERING
// ----------------------------------------------------------------------------

let renderEditor (trigger:Event -> unit) pos value =
  td [ Class "selected"] [ 
    input [
      AutoFocus true
      // OnInput (fun e -> Browser.window.alert(e.target?value))
      OnInput (fun e -> trigger (UpdateValue (pos, unbox e.target?value)))
      Value value ]
  ]

let renderView trigger pos (value:option<_>) = 
  td 
    [ Style (if value.IsNone then [Background "#ffb0b0"] else [Background "white"]) 
      OnClick (fun _ -> trigger (StartEdit pos)) ] 
    [ str (Option.defaultValue "#ERR" value) ]

let renderCell trigger pos state =
  let value = state.Cells |> Map.tryFind pos
  if Some pos = state.Active then
  // match Some pos with
  // | state.Active ->
    renderEditor trigger pos (value |> Option.defaultValue "")
  // | _ ->
  else
    match value with 
    | None -> renderView trigger pos (Some "")
    | Some value ->
      let parsed = parse value |> Option.map (evaluate state.Cells >> string)
      //  |> Option.map string
      renderView trigger pos parsed 


    // let value = 
    //   match value with
    //   | Some value ->
    //     // parse value |> Option.bind (evaluate state.Cells)
    //     // let parsed = parse value 
    //     // match parsed with 
    //     // | Some pars ->
    //     //   pars |> evaluate state.Cells |> string |> Some
    //     // | None -> Some "fail"
    //   // | Some value -> parse value |> Option.bind (evaluate Set.empty state.Cells) |> Option.map string
    //     // let res = parse value |> Option.bind (evaluate state.Cells) 
    //     // res
    //     Some value
    //     // parse value |> Option.bind (evaluate Set.empty  parse value |> Option.bind (evaluate Set.empty state.Cells) |> Option.map stringstate.Cells) |> Option.map string
    //   | None -> Some ""
    // renderView trigger pos value

    // match value with
    // | Some v ->
    //   renderView trigger pos (parse v |> string |> Option.bind)

let view state trigger =
  let empty = td [] []
  let header h = th [] [str h] // let headers = [ empty; header "A"; header "B" ] 
  // let headers = state.Cols |> List.map (fun h -> header (string h)) 
  let headers = empty::(state.Cols |> List.map (string >> header))

  let row cells = tr [] cells

  let cells n = 
    let cells = state.Cols |> List.map (fun h -> renderCell trigger (h, n) state)
    header (string n) :: cells
  let rows = state.Rows |> List.map (cells >> row)

  table [] [
    tr [] headers
    tbody [] rows
  ]

// ----------------------------------------------------------------------------
// ENTRY POINT
// ----------------------------------------------------------------------------

let initial () = 
  { Cols = ['A' .. 'K']
    Rows = [1 .. 15]
    Active = None
    Cells = Map.empty },
  Cmd.Empty    
 
Program.mkProgram initial update view
|> Program.withReact "main"
|> Program.run
