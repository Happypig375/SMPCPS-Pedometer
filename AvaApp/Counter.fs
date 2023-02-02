﻿namespace AvaApp
open FSharp.Data.UnitSystems.SI.UnitSymbols 
type Pedometer =
    abstract IsSupported : bool
    abstract Step : IEvent<int * float<m> option>
module Counter =
    let mutable pedometerInstance : Pedometer option = None
    open Elmish
    open System
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Controls.Shapes
    open Avalonia.FuncUI.DSL
    open Avalonia.Svg.Skia
    open Avalonia.Layout
    open Avalonia.Platform
    open Avalonia.FuncUI.Elmish
    
    type Page = LoggedOut | LoggedIn | Menu of confirmSignOut:bool
    type State = { name: string; steps: int; distance: double<m>; page:Page }
    let init = { name = ""; steps = 0; distance = 0.<m>; page = LoggedOut }

    type Msg =
    | UpdateName of string
    | LogIn
    | StepDataUpdated of step:int * distance:double<m>
    | OpenMenu
    | CloseMenu
    | SignOut
    | SignOutConfirm

    let update (msg: Msg) (state: State) =
        match msg with
        | UpdateName name -> { state with name = name }
        | LogIn | CloseMenu -> { state with page = LoggedIn }
        | Stepped (step, distance) -> { state with steps = state.steps + step; distance = state.distance + distance }
        | OpenMenu -> { state with page = Menu false }
        | SignOut -> { state with page = Menu true }
        | SignOutConfirm -> init
        , Cmd.none
    
    let view (state: State) (dispatch) =
        Viewbox.create [
            Viewbox.clipToBounds false
            Viewbox.child (
                Canvas.create [
                    Canvas.clipToBounds false
                    Canvas.width 834
                    Canvas.height 1194
                    Canvas.children [
                        match state.page with
                        | Menu confirmSignOut ->
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.top -1e9
                                Rectangle.width 2e9
                                Rectangle.height 2e9
                                Rectangle.fill "#2B2B2B"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.top 137
                                Rectangle.width 2e9
                                Rectangle.height 57
                                Rectangle.fill "#48A346"
                            ]
                            Svg.create [
                                Svg.path (if confirmSignOut then "iPad Pro 11_ - 8.svg" else "iPad Pro 11_ - 7.svg")
                            ]
                            if confirmSignOut then
                                Button.create [
                                    Button.left 158
                                    Button.top 720
                                    Button.width 235
                                    Button.height 90
                                    Button.onClick (fun _ -> dispatch OpenMenu)
                                ]
                                Button.create [
                                    Button.left 435
                                    Button.top 720
                                    Button.width 235
                                    Button.height 90
                                    Button.onClick (fun _ -> dispatch SignOutConfirm)
                                ]
                            else
                                Button.create [
                                    Button.left 20
                                    Button.top 20
                                    Button.width 110
                                    Button.height 110
                                    Button.onClick (fun _ -> dispatch CloseMenu)
                                ]
                                Button.create [
                                    Button.left 30
                                    Button.top 1020
                                    Button.width 210
                                    Button.height 90
                                    Button.onClick (fun _ -> dispatch SignOut)
                                ]
                        | LoggedIn ->
                            let completed = state.distance >= 21000.<m>
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.bottom <| 1194. - 137.
                                Rectangle.width 2e9
                                Rectangle.height 2e9
                                Rectangle.fill "#2B2B2B"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.top 137
                                Rectangle.width 2e9
                                Rectangle.height 57
                                Rectangle.fill (if completed then "#EF2D8A" else "#48A346")
                            ]
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.top 190
                                Rectangle.width 2e9
                                Rectangle.height 706
                                Rectangle.fill (if completed then "white" else "#F2BF3C")
                            ]
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.top 896
                                Rectangle.width 2e9
                                Rectangle.height 241
                                Rectangle.fill (if completed then "#EF2D8A" else "#48A346")
                            ]
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.top 1134
                                Rectangle.width 2e9
                                Rectangle.height 2e9
                                Rectangle.fill "#2B2B2B"
                            ]
                            Svg.create [
                                match int<double<m>> state.distance / 5000 with
                                | _ when completed -> "iPad Pro 11_ - 6.svg"
                                | 1 -> "iPad Pro 11_ - 2.svg"
                                | 2 -> "iPad Pro 11_ - 3.svg"
                                | 3 -> "iPad Pro 11_ - 4.svg"
                                | 4 -> "iPad Pro 11_ - 5.svg"
                                | _ -> "iPad Pro 11_ - 1.svg"
                                |> Svg.path
                            ]
                            Button.create [
                                Button.left 20
                                Button.top 20
                                Button.width 110
                                Button.height 110
                                Button.onClick (fun _ -> dispatch OpenMenu)
                            ]
                            TextBlock.create [
                                if completed then
                                    TextBlock.foreground "#FFC1DB"
                                    TextBlock.background "#EF2D8A"
                                    TextBlock.textAlignment Media.TextAlignment.Center
                                else
                                    TextBlock.foreground "#ABECB1"
                                    TextBlock.background "#48A346"
                                    TextBlock.left 400
                                    TextBlock.top 140
                                TextBlock.text state.name
                                TextBlock.fontSize 40
                                TextBlock.minWidth 250
                            ]
                            |> if completed then fun x ->
                                ContentControl.create [
                                    ContentControl.clipToBounds false
                                    ContentControl.left 0
                                    ContentControl.top 140
                                    ContentControl.width 834
                                    ContentControl.height 57
                                    ContentControl.content x
                                    ContentControl.horizontalContentAlignment HorizontalAlignment.Center
                                ] :> FuncUI.Types.IView else fun x -> x
                            TextBlock.create [
                                TextBlock.left 70
                                TextBlock.top 1005
                                TextBlock.text $"%d{state.steps}"
                                TextBlock.fontSize 85
                                if completed then
                                    TextBlock.foreground "#FFC1DB"
                                    TextBlock.background "#EF2D8A"
                                else
                                    TextBlock.foreground "#ABECB1"
                                    TextBlock.background "#48A346"
                                TextBlock.minWidth 330
                            ]
                            TextBlock.create [
                                TextBlock.left 510
                                TextBlock.top 1005
                                TextBlock.text $"%d{int<double<m>> state.distance / 1000} / 21"
                                TextBlock.fontSize 85
                                if completed then
                                    TextBlock.foreground "#FFC1DB"
                                    TextBlock.background "#EF2D8A"
                                else
                                    TextBlock.foreground "#ABECB1"
                                    TextBlock.background "#48A346"
                                TextBlock.minWidth 300
                            ]
                            Button.create [
                                Button.content "Test step"
                                Button.onClick (fun _ -> Stepped(300, 300.<m>) |> dispatch)
                                Button.top 200
                                Button.left 100
                                Button.width 100
                                Button.height 100
                            ]
                        | LoggedOut ->
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.bottom <| 1194. - 322.
                                Rectangle.width 2e9
                                Rectangle.height 2e9
                                Rectangle.fill "#2B2B2B"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.top 322
                                Rectangle.width 2e9
                                Rectangle.height 127
                                Rectangle.fill "#48A346"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.top 446
                                Rectangle.width 2e9
                                Rectangle.height 688
                                Rectangle.fill "#F2BF3C"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e9
                                Rectangle.top 1134
                                Rectangle.width 2e9
                                Rectangle.height 2e9
                                Rectangle.fill "#48A346"
                            ]
                            Svg.create [
                                Svg.path "Login.svg"
                            ]
                            TextBox.create [
                                TextBox.text state.name
                                TextBox.onTextChanged (UpdateName >> dispatch)
                                TextBox.fontSize 30
                                TextBox.background "transparent"
                                TextBox.left 333
                                TextBox.top 580
                                TextBox.width 421
                                TextBox.height 65
                                TextBox.background "white"
                                TextBox.watermark "Enter your name here"
                                TextBox.verticalContentAlignment VerticalAlignment.Center
                                TextBox.watermark "Enter your name here"
                            ]
                            Button.create [
                                Button.left 372
                                Button.top 821
                                Button.width 299
                                Button.height 132
                                Button.onClick (fun _ -> dispatch LogIn)
                            ]
                    ]
                ]
            )
        ]
    
    let program = Program.mkProgram (fun () -> init, Cmd.ofEffect (fun dispatch ->
        match pedometerInstance with Some p when p.IsSupported -> p.Step)) update view