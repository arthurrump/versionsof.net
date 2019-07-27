module Query.Startup

open Microsoft.AspNetCore.Blazor.Hosting
open Microsoft.AspNetCore.Components.Builder
open Microsoft.Extensions.DependencyInjection
open Bolero.Remoting.Client

type Startup() =

    member __.ConfigureServices(services: IServiceCollection) =
        services.AddRemoting()

    member __.Configure(app: IComponentsApplicationBuilder) =
        app.AddComponent<App.App>("#main")

[<EntryPoint>]
let main args =
    BlazorWebAssemblyHost.CreateDefaultBuilder()
        .UseBlazorStartup<Startup>()
        .Build()
        .Run()
    0
