open UIKit
open Foundation
open FSharp.Data.UnitSystems.SI.UnitSymbols
open System

type Pedometer = CoreMotion.CMPedometer
type PedometeriOS() =
    let pedometer = new CoreMotion.CMPedometer()
    let event = Event<int * float<m> option>()
    // steps from midnight
    do
        if Pedometer.IsStepCountingAvailable then
            pedometer.StartPedometerUpdates(NSDate.Now, // Data since now
                Action<_, _>(fun data _ ->
                    event.Trigger(
                        data.NumberOfSteps.Int32Value,
                        if Pedometer.IsDistanceAvailable then
                            data.Distance(*in meters*).DoubleValue * 1.<m> |> Some
                        else None
                    )
                )
            )
    interface AvaApp.Pedometer with
        member _.IsSupported = Pedometer.IsStepCountingAvailable
        member _.Step = event.Publish

// This is the main entry point of the application.
let [<EntryPoint>] Main(args: string array) =
    AvaApp.Counter.setPedometer <| PedometeriOS()
    // if you want to use a different Application Delegate class from "AppDelegate"
    // you can specify it here.
    UIApplication.Main(args, null, typeof<AvaApp.iOS.AppDelegate>)
    0