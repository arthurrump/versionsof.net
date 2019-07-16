# versionsof.net

.NET Core versioning is a mess. There are lots and lots and lots of version numbers, from runtime to SDK, from Visual Studio to languages and of course .NET Standard. So what SDK corresponds to which runtime again? I know there some sort of a system, but it differs per release. This website uses information published in the [.NET Core repo](https://github.com/dotnet/core/blob/master/release-notes/) to give an overview of all the versions of .NET Core.

## Building the website

This site is built using [Fake.StaticGen](https://github.com/arthurrump/Fake.StaticGen), which is a toolset for generating static websites, built on the [FAKE](https://fake.build) build system. To build the site, you need the .NET Core SDK and `fake-cli` tool (`dotnet tool install fake-cli`). To generate the site, run

```
fake build
```
