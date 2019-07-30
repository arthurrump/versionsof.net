module Query.Startup

open Microsoft.AspNetCore.Blazor.Hosting
open Microsoft.AspNetCore.Components.Builder
open Microsoft.Extensions.DependencyInjection

type Startup() =
    member __.ConfigureServices(services: IServiceCollection) = 
        services
    member __.Configure(app: IComponentsApplicationBuilder) =
        app.AddComponent<App.App>("#query-main")

[<EntryPoint>]
let main _ =
    BlazorWebAssemblyHost.CreateDefaultBuilder()
        .UseBlazorStartup<Startup>()
        .Build()
        .Run()
    0
