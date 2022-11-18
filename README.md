# versionsof.net

[![Build Status](https://dev.azure.com/arthurrump/versionsof.net/_apis/build/status/versionsof.net?branchName=master)](https://dev.azure.com/arthurrump/versionsof.net/_build/latest?definitionId=17&branchName=master)

.NET Core versioning is a mess. There are lots and lots and lots of version numbers, from runtime to SDK, from Visual Studio to languages and of course .NET Standard. So what SDK corresponds to which runtime again? I know there some sort of a system, but it differs per release. This website uses information published in the [.NET Core repo](https://github.com/dotnet/core/blob/master/release-notes/) to give an overview of all the versions of .NET Core.

## Building the website

This site is built using [Fake.StaticGen](https://github.com/arthurrump/Fake.StaticGen), which is a toolset for generating static websites, built on the [FAKE](https://fake.build) build system. To build the site, you need the .NET Core SDK and `fake-cli` tool (`dotnet tool install fake-cli`). 

To get the list of Mono releases, access to the GitHub API is required. The secrets for this are to be stored in a *secrets.toml* file with entries for `gh-client-id` and `gh-client-secret`. These secrets can be acquired by creating a [GitHub OAuth app](https://github.com/settings/developers). You can also create the *secrets.toml* file using the `Configure` target with FAKE:

```
dotnet run -- -t configure -- [GitHub Client ID] [GitHub Client Secret]
```

To generate the site, run

```
dotnet run
```
