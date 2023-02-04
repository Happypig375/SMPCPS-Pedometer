namespace AvaApp.Android
open Android.App
open Android.OS
open Android.Content.PM
open Avalonia.Android
open Avalonia
open FSharp.Data.UnitSystems.SI.UnitSymbols

type PedometerListener(onChanged) =
    inherit Java.Lang.Object()
    interface Android.Hardware.ISensorEventListener with
        member _.OnAccuracyChanged(_, _) = ()
        member _.OnSensorChanged e = onChanged <| int e.Values.[0] (*steps since device boot*)
type PedometerAndroid() =
    let manager =  
        Application.Context.GetSystemService Android.Content.Context.SensorService
        :?> Android.Hardware.SensorManager
    let sensor = manager.GetDefaultSensor Android.Hardware.SensorType.StepCounter
    let event = Event<int>()         
    do
        if sensor <> null then
            manager.RegisterListener(new PedometerListener(event.Trigger), sensor, Android.Hardware.SensorDelay.Fastest)
            |> ignore
    interface AvaApp.Pedometer with
        member _.IsSupported = sensor <> null
        member _.Step = event.Publish |> Event.map (fun steps -> steps, None)

[<Application(Label = "SMPCPS Pedometer", Icon = "@drawable/Icon")>]
// Not Android.Hardware.Sensor.StringTypeStepCounter!
// Available features: https://developer.android.com/guide/topics/manifest/uses-feature-element#features-reference
[<UsesFeature("android.hardware.sensor.stepcounter", Required = true)>]
[<UsesPermission(Android.Manifest.Permission.ActivityRecognition)>]
do ()

[<Activity(Theme = "@style/MyTheme.NoActionBar", LaunchMode = LaunchMode.SingleInstance, ConfigurationChanges = (ConfigChanges.Orientation ||| ConfigChanges.ScreenSize))>]
type MainActivity() =
    inherit AvaloniaMainActivity()
    let [<Literal>] activityRecognitionRequestCode = 1483743225
    override this.OnCreate(bundle: Bundle) =
        base.OnCreate bundle
        if AndroidX.Core.Content.ContextCompat.CheckSelfPermission(this,
            Android.Manifest.Permission.ActivityRecognition) = Permission.Denied then
            // Ask for permission
            this.RequestPermissions([|Android.Manifest.Permission.ActivityRecognition|], activityRecognitionRequestCode)
        // Some devices return denied when this permission is unsupported but actually allowed
        AvaApp.Counter.setPedometer <| PedometerAndroid()
    override this.OnRequestPermissionsResult(requestCode: int, permissions: string[], [<Android.Runtime.GeneratedEnum>] grantResults: Permission[]) =
        base.OnRequestPermissionsResult(requestCode, permissions, grantResults)
        if requestCode = activityRecognitionRequestCode then
            if grantResults[0] = Permission.Denied then
                // Ask for permission again in case of misclick
                this.RequestPermissions([|Android.Manifest.Permission.ActivityRecognition|], activityRecognitionRequestCode)
            else AvaApp.Counter.setPedometer <| PedometerAndroid() // We got permission!