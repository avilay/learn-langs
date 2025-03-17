## Steps

Create a solution file in the **golden** directory.
```
dotnet new sln
```

Create a library project from the `golden` directory. This will create a **library** directory with the source files under it.
```
dotnet new classlib -o library
```

Add the newly created library project (i.e., `library.csproj`) to the golden solution.
```
dotnet sln add library/library.csproj
```

Add Newtonsoft.Json dependant package.
```
dotnet add library package Newtonsoft.Json
```
This will essentially add the following node to the library.csproj:
```
<ItemGroup>
    <PackageReference Include="Newtonsoft.Json" Version="10.0.1">
</ItemGroup>
```

There is something called `dotnet restore` that restores dependencies. Not sure if this is the same as `pip install -r requirements.txt` or not. Seems like it is.

Build all the projects under the solution
```
dotnet build
```

Create the test project
```
dotnet new xunit -o test-library
```

Add the test project to the solution
```
dotnet sln add test-library/test-library.csproj
```

The test-library project needs to reference the main library project. Add that reference.
```
dotnet add test-library/test-library.csproj reference library/library.csproj
```
Or manually edit the test-library.csproj file:
```
<ItemGroup>
    <ProjectReference Include="../library/library.csproj">
</ItemGroup>
```

To run the tests -
```
dotnet test test-library/test-library.csproj
```

To create a console app -
```
dotnet new console -o app
```

Add the console project to the solution
```
dotnet sln add app/app.csproj
```

Create a dependency on the library -
```
dotnet add app/app.csproj reference library/library.csproj
```

To the run the console app -
```
dotnet run -p app/app.csproj
```
