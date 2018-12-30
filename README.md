# versionsof.net

.NET Core versioning is a mess. There are lots and lots and lots of version numbers, from runtime to SDK, from Visual Studio to languages and of course .NET Standard. So what SDK corresponds to which runtime again? I know there some sort of a system, but it differs per release. This website uses information published in the [.NET Core repo](https://github.com/dotnet/core/blob/master/release-notes/) to give an overview of all the versions of .NET Core.

## Building the website

This site is built using [Fable](http://fable.io), which converts the F# code in this repo into JavaScript that runs in your browser. To get it working, you first have to get all dependencies:
```
npm install
cd src
dotnet restore
```
You can then run a webpack dev server to compile the site and see it in your browser:
```
dotnet fable webpack-dev-server
```
To compile the JavaScript bundle and put it in the folder `/public`, run
```
dotnet fable webpack
```
The public folder then contains a deployable version of the site.