namespace AvaApp
open FSharp.Data.UnitSystems.SI.UnitSymbols 
type Pedometer =
    abstract IsSupported : bool
    abstract Step : IEvent<int * float<m> option>
module Counter =
    open Elmish
    open System
    open System.IO
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Controls.Shapes
    open Avalonia.FuncUI.DSL
    open Avalonia.Svg.Skia
    open Avalonia.Layout
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Elmish
    
    let mutable private pedometer: Pedometer option = None
    let private pedometerSetted = Event<_>()
    let setPedometer p =
        pedometer <- Some p
        pedometerSetted.Trigger p

    let fileName = Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "smpcps_pedometer_progress")
    type Page = LoggedOut | LoggedIn | Menu of confirmSignOut:bool
    type StepsAndDistance = int * double<m>
    type State = {
        previous_progress: StepsAndDistance
        current_origin: StepsAndDistance option // Tracking the origin should be done in app logic instead of platform code because we reset steps when log off (new origin)
        current_tracker: StepsAndDistance
        name: string
        page:Page
    } with
        member t.total_steps = fst t.previous_progress + fst t.current_tracker - Option.defaultValue 0 (Option.map fst t.current_origin)
        member t.total_distance = snd t.previous_progress + snd t.current_tracker - Option.defaultValue 0.<m> (Option.map snd t.current_origin)
    let init =
        using (System.IO.File.Open(fileName, IO.FileMode.OpenOrCreate)) ignore
        match FSharpPlus.Parsing.trySscanf "%d %g %s" (System.IO.File.ReadAllText fileName) with
        | Some (steps, distance, name) -> {
                previous_progress = steps, distance * 1.<m>
                current_origin = None
                current_tracker = 0, 0.<m>
                name = name
                page = LoggedIn
            }
        | None -> {
                previous_progress = 0, 0.<m>
                current_origin = None
                current_tracker = 0, 0.<m>
                name = ""
                page = LoggedOut
            }

    type Msg =
    | UpdateName of string
    | LogIn
    | CurrentTrackerUpdate of step:int * distance:double<m> option
    | OpenMenu
    | CloseMenu
    | SignOut
    | SignOutConfirm

    let update (msg: Msg) (state: State) =
        match msg with
        | UpdateName name -> { state with name = name }
        | LogIn ->
            File.WriteAllText(fileName, $"0 0 %s{state.name}")
            { state with page = LoggedIn }
        | CloseMenu -> { state with page = LoggedIn }
        | CurrentTrackerUpdate (steps, distance) ->
            // https://www.lewisgavin.co.uk/Step-Tracker-Android/
            // Let's assume woman average of 70cm.
            let distance = defaultArg distance (float steps * 0.70<m>)
            match state.current_origin with
            | None ->
                { state with current_origin = Some (steps, distance); current_tracker = steps, distance }
            | Some (origin_steps, origin_distance) ->
                let prev_steps, prev_distance = state.previous_progress
                File.WriteAllText(fileName, $"%d{prev_steps + steps - origin_steps} %g{prev_distance + distance - origin_distance} %s{state.name}")
                { state with current_tracker = steps, distance }
        | OpenMenu -> { state with page = Menu false }
        | SignOut -> { state with page = Menu true }
        | SignOutConfirm ->
            File.Delete fileName
            {
                previous_progress = 0, 0.<m>
                current_origin = None
                current_tracker = 0, 0.<m>
                name = ""
                page = LoggedOut
            }
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
                                Rectangle.left -1e6
                                Rectangle.top -1e6
                                Rectangle.width 2e6
                                Rectangle.height 2e6
                                Rectangle.fill "#2B2B2B"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.top 137
                                Rectangle.width 2e6
                                Rectangle.height 57
                                Rectangle.fill "#48A346"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.top 194
                                Rectangle.width 2e6
                                Rectangle.height 940
                                Rectangle.fill "#404040"
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
                                    Button.background "transparent"
                                ]
                                Button.create [
                                    Button.left 435
                                    Button.top 720
                                    Button.width 235
                                    Button.height 90
                                    Button.onClick (fun _ -> dispatch SignOutConfirm)
                                    Button.background "transparent"
                                ]
                            else
                                Button.create [
                                    Button.left 20
                                    Button.top 20
                                    Button.width 110
                                    Button.height 110
                                    Button.onClick (fun _ -> dispatch CloseMenu)
                                    Button.background "transparent"
                                ]
                                Button.create [
                                    Button.left 30
                                    Button.top 1020
                                    Button.width 210
                                    Button.height 90
                                    Button.onClick (fun _ -> dispatch SignOut)
                                    Button.background "transparent"
                                ]
                        | LoggedIn ->
                            let completed = state.total_distance >= 21000.<m>
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.bottom <| 1194. - 137.
                                Rectangle.width 2e6
                                Rectangle.height 2e6
                                Rectangle.fill "#2B2B2B"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.top 190
                                Rectangle.width 2e6
                                Rectangle.height 706
                                Rectangle.fill (if completed then "white" else "#F2BF3C")
                            ]
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.top 137
                                Rectangle.width 2e6
                                Rectangle.height 57
                                Rectangle.fill (if completed then "#EF2D8A" else "#48A346")
                            ]
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.top 896
                                Rectangle.width 2e6
                                Rectangle.height 241
                                Rectangle.fill (if completed then "#EF2D8A" else "#48A346")
                            ]
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.top 1134
                                Rectangle.width 2e6
                                Rectangle.height 2e6
                                Rectangle.fill "#2B2B2B"
                            ]
                            Svg.create [
                                match int<double<m>> state.total_distance / 5000 with
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
                                Button.background "transparent"
                            ]
                            ContentControl.create [
                                ContentControl.clipToBounds false
                                ContentControl.left 0
                                ContentControl.top 137
                                ContentControl.width 834
                                ContentControl.height 57
                                ContentControl.horizontalContentAlignment HorizontalAlignment.Center
                                ContentControl.content (
                                    TextBlock.create [
                                        if completed then
                                            TextBlock.foreground "#FFC1DB"
                                            TextBlock.background "#EF2D8A"
                                            TextBlock.text state.name
                                        else
                                            TextBlock.foreground "#ABECB1"
                                            TextBlock.background "#48A346"
                                            TextBlock.text $"Welcome, %s{state.name}"
                                        TextBlock.textAlignment Media.TextAlignment.Center
                                        TextBlock.fontSize 40
                                        TextBlock.minWidth 450
                                    ]
                                )
                            ]
                            TextBlock.create [
                                TextBlock.left (70.-30.)
                                TextBlock.top 1005
                                TextBlock.minWidth (330.+30.)
                                TextBlock.text $"%d{state.total_steps}"
                                TextBlock.fontSize 85
                                if completed then
                                    TextBlock.foreground "#FFC1DB"
                                    TextBlock.background "#EF2D8A"
                                else
                                    TextBlock.foreground "#ABECB1"
                                    TextBlock.background "#48A346"
                            ]
                            TextBlock.create [
                                TextBlock.left (510.-30.)
                                TextBlock.top 1005
                                TextBlock.minWidth (300.+30.)
                                TextBlock.text $"%.1f{state.total_distance / 1000.} / 21"
                                TextBlock.fontSize 85
                                if completed then
                                    TextBlock.foreground "#FFC1DB"
                                    TextBlock.background "#EF2D8A"
                                else
                                    TextBlock.foreground "#ABECB1"
                                    TextBlock.background "#48A346"
                            ]
                            Button.create [
                                Button.content "Test step"
                                Button.onClick (fun _ ->
                                    let steps, distance = state.current_tracker
                                    CurrentTrackerUpdate(steps + 300, None) |> dispatch
                                , SubPatchOptions.OnChangeOf state.current_tracker)
                                Button.top 200
                                Button.left 100
                                Button.width 100
                                Button.height 100
                                Button.background "transparent"
                            ]
                        | LoggedOut ->
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.bottom <| 1194. - 322.
                                Rectangle.width 2e6
                                Rectangle.height 2e6
                                Rectangle.fill "#2B2B2B"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.top 446
                                Rectangle.width 2e6
                                Rectangle.height 688
                                Rectangle.fill "#F2BF3C"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.top 322
                                Rectangle.width 2e6
                                Rectangle.height 127
                                Rectangle.fill "#48A346"
                            ]
                            Rectangle.create [
                                Rectangle.left -1e6
                                Rectangle.top 1134
                                Rectangle.width 2e6
                                Rectangle.height 2e6
                                Rectangle.fill "#48A346"
                            ]
                            Svg.create [
                                Svg.path "Login.svg"
                            ]
                            TextBox.create [
                                TextBox.text state.name
                                TextBox.onTextChanged (UpdateName >> dispatch)
                                TextBox.fontSize 30
                                TextBox.left 333
                                TextBox.top 580
                                TextBox.width 421
                                TextBox.height 65
                                TextBox.watermark "Enter your name here"
                                TextBox.background "white"
                                TextBox.verticalContentAlignment VerticalAlignment.Center
                                TextBox.foreground "black"
                            ]
                            Button.create [
                                Button.left 372
                                Button.top 821
                                Button.width 299
                                Button.height 132
                                Button.onClick (fun _ -> dispatch LogIn)
                                Button.background "transparent"
                            ]
                    ]
                ]
            )
        ]
    
    let program =
        Program.mkProgram (fun () -> init, Cmd.ofEffect (fun dispatch ->
            let listenToPedometer (p: Pedometer) = CurrentTrackerUpdate >> dispatch |> p.Step.Add
            pedometerSetted.Publish.Add listenToPedometer
            Option.iter listenToPedometer pedometer
        )) update view