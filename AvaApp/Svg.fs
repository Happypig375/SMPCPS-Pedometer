namespace AvaApp
module [<AutoOpen>] Svg =
    open Avalonia.Controls
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Svg.Skia
    open Avalonia.Media
    type private T = T
    let create(attrs: IAttr<Svg> list): IView<Svg> =
        ViewBuilder.Create<Svg>(attrs)
        |> View.withConstructorArgs [|System.Uri "avares://AvaApp"|]

    type Svg with 
        static member enableCache<'t when 't :> Svg>(value: bool) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<bool>(Svg.EnableCacheProperty, value, ValueNone)
        static member path<'t when 't :> Svg>(value: string) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<string>(Svg.PathProperty, value, ValueNone)
        static member stretchDirection<'t when 't :> Svg>(value: StretchDirection) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<StretchDirection>(Svg.StretchDirectionProperty, value, ValueNone)
        static member stretch<'t when 't :> Svg>(value: Stretch) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<Stretch>(Svg.StretchProperty, value, ValueNone)
        static member onInvalidated<'t when 't :> Button>(func: Svg -> unit, ?subPatchOptions) : IAttr<'t> =
            AttrBuilder<'t>.CreateSubscription(nameof Unchecked.defaultof<Svg>.Invalidated, (fun (control, subscription, _) -> (control :?> Svg).Invalidated.AddHandler(fun sender _ -> subscription (sender :?> Svg))), func, ?subPatchOptions = subPatchOptions)