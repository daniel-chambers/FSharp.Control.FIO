// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "paket: groupref FakeBuild //"
#load "./.fake/build.fsx/intellisense.fsx"
#if !FAKE // https://github.com/ionide/ionide-vscode-fsharp/issues/839
  #r "netstandard"
  #r "Facades/netstandard"
#endif

open System
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Tools
open Fake.Api


#nowarn "52"

let gitHubToken = Environment.environVarOrDefault "GITHUB_TOKEN" ""
let nugetApiKey = Environment.environVarOrDefault "NUGET_KEY" ""

let project = "FSharp.Control.FIO"
let gitHubOwner = "daniel-chambers"
let gitHubName = "FSharp.Control.FIO"
let author = "Daniel Chambers & Contributors"
let solutionFile  = "FSharp.Control.FIO.sln"
let website = ""

let releaseNotes =
  File.read "./RELEASE_NOTES.md"
  |> ReleaseNotes.parseAll
let latestReleaseNotes = List.head releaseNotes
let previousReleaseNotes = List.tryItem 1 releaseNotes

Target.description "Tags the current commit with the version and pushes the tag"
Target.create "GitTagAndPush" <| fun _ ->
  if not <| Git.Information.isCleanWorkingCopy "." then
    failwith "Please ensure the working copy is clean before performing a release"

  let remoteUrl = sprintf "github.com/%s/%s" gitHubOwner gitHubName
  let remote =
    Git.CommandHelper.getGitResult "" "remote -v"
    |> Seq.filter (fun (s: string) -> s.EndsWith "(push)")
    |> Seq.tryFind (fun (s: string) -> s.Contains remoteUrl)
    |> function
      | Some (s: string) -> s.Split().[0]
      | None -> failwithf "Unable to determine remote for %s" remoteUrl

  let tag = sprintf "v%s" latestReleaseNotes.NugetVersion
  Git.Branches.pushBranch "." remote <| Git.Information.getBranchName "."
  Git.Branches.tag "." tag
  Git.Branches.pushTag "." remote tag

Target.description "Generates an AssemblyInfo file with version info"
Target.create "GenerateAssemblyInfoFile" <| fun _ ->
  AssemblyInfoFile.createFSharp "./src/obj/AssemblyInfo.Generated.fs" [
    AssemblyInfo.Title project
    AssemblyInfo.Product project
    AssemblyInfo.Company author
    AssemblyInfo.Copyright ("Copyright \169 Daniel Chambers & Contributors " + DateTime.Now.Year.ToString ())
    AssemblyInfo.Version latestReleaseNotes.AssemblyVersion
    AssemblyInfo.FileVersion latestReleaseNotes.AssemblyVersion
    AssemblyInfo.Metadata ("githash", Git.Information.getCurrentHash())
  ]

Target.description "Compiles the project using dotnet build"
Target.create "Build" <| fun _ ->
  DotNet.build id ("./" + solutionFile)

Target.description "Runs the expecto unit tests"
Target.create "Test" <| fun _ ->
  let result = DotNet.exec id "run" "--project ./test/FSharp.Control.FIO.Test.fsproj -c Release"
  if not result.OK then failwithf "Tests failed with code %i" result.ExitCode

Target.description "Deletes the contents of the ./bin directory"
Target.create "PaketClean" <| fun _ ->
  Shell.cleanDir "./bin"

Target.description "Creates the NuGet package"
Target.create "PaketPack" <| fun _ ->
  Paket.pack <| fun p ->
    { p with
        ReleaseNotes = latestReleaseNotes.Notes |> List.map (fun s -> "- " + s) |> String.concat "\n"
        Version = latestReleaseNotes.NugetVersion
        OutputPath = "./bin" }

Target.description "Ensures you have specified your NuGet API key in the NUGET_KEY env var"
Target.create "ValidateNugetApiKey" <| fun _ ->
  if String.IsNullOrWhiteSpace nugetApiKey then
    failwith "Please set the NUGET_KEY environment variable to your NuGet API Key"

Target.description "Pushes the NuGet package to the package repository"
Target.create "PaketPush" <| fun _ ->
  Paket.push <| fun p ->
    { p with
        WorkingDir = "./bin"
        ApiKey = nugetApiKey }

Target.description "Ensures you have specified your GitHub personal access token in the GITHUB_TOKEN env var"
Target.create "ValidateGitHubCredentials" <| fun _ ->
  if String.IsNullOrWhiteSpace gitHubToken then
    failwith "Please set the GITHUB_TOKEN environment variable to a GitHub personal access token with repo access."

Target.description "Creates a release on GitHub with the release notes"
Target.create "GitHubRelease" <| fun _ ->
  let gitHubReleaseNotes =
    [ yield "## Changelog"
      yield! latestReleaseNotes.Notes |> List.map (fun s -> "- " + s)
      yield ""
      match previousReleaseNotes with
      | Some prev ->
        yield sprintf "Full changelog [here](https://github.com/fsprojects/FSharp.Azure.Storage/compare/v%s...v%s)" prev.NugetVersion latestReleaseNotes.NugetVersion
      | None -> ()
    ]

  GitHub.createClientWithToken gitHubToken
  |> GitHub.draftNewRelease gitHubOwner gitHubName ("v" + latestReleaseNotes.NugetVersion) (latestReleaseNotes.SemVer.PreRelease <> None) gitHubReleaseNotes
  |> GitHub.publishDraft
  |> Async.RunSynchronously


// --------------------------------------------------------------------------------------
// Generate the documentation

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "./src/bin/Release"
let content    = __SOURCE_DIRECTORY__ @@ "docsrc/content"
let output     = __SOURCE_DIRECTORY__ @@ "docs"
let files      = __SOURCE_DIRECTORY__ @@ "docsrc/files"
let templates  = __SOURCE_DIRECTORY__ @@ "docsrc/tools/templates"
let formatting = __SOURCE_DIRECTORY__ @@ "packages/formatting/FSharp.Formatting"
let docTemplate = "docpage.cshtml"

let githubReleaseUser = Environment.environVarOrDefault "github_release_user" gitHubOwner
let githubLink = sprintf "https://github.com/%s/%s" githubReleaseUser gitHubName

// Specify more information about your project
let info =
  [ "project-name", "FIO"
    "project-author", "Daniel Chambers"
    "project-summary", "F# version of Scala's ZIO Environment"
    "project-github", githubLink
    "project-nuget", "http://nuget.org/packages/FSharp.Control.FIO" ]

let root = website

let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
layoutRootsAll.Add("en", [ templates;
                           formatting @@ "templates"
                           formatting @@ "templates/reference" ])

Target.create "CleanDocs" (fun _ ->
  Shell.cleanDirs ["docs"]
)

Target.create "ReferenceDocs" (fun _ ->
  Directory.ensure (output @@ "reference")

  [ "./src/bin/Release/netstandard2.0/FSharp.Control.FIO.dll" ]
  |> FSFormatting.createDocsForDlls (fun args ->
    { args with
        OutputDirectory = output @@ "reference"
        LayoutRoots =  layoutRootsAll.["en"]
        ProjectParameters =  ("root", root)::info
        SourceRepository = githubLink @@ "tree/master" }
       )
)

let copyFiles () =
  Shell.copyRecursive files output true
  |> Trace.logItems "Copying file: "
  Directory.ensure (output @@ "content")
  Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true
  |> Trace.logItems "Copying styles and scripts: "

Target.create "Docs" (fun _ ->
  File.delete "docsrc/content/release-notes.md"
  Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
  Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

  File.delete "docsrc/content/license.md"
  Shell.copyFile "docsrc/content/" "LICENSE.txt"
  Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE.txt"

  DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
  |> Seq.iter (fun d ->
    let name = d.Name
    if name.Length = 2 || name.Length = 3 then
      layoutRootsAll.Add(
        name, [templates @@ name
               formatting @@ "templates"
               formatting @@ "templates/reference" ]))
  copyFiles ()

  for dir in  [ content; ] do
    let langSpecificPath(lang, path:string) =
      path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
      |> Array.exists(fun i -> i = lang)
    let layoutRoots =
      let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
      match key with
      | Some lang -> layoutRootsAll.[lang]
      | None -> layoutRootsAll.["en"] // "en" is the default language

    FSFormatting.createDocs (fun args ->
      { args with
          Source = content
          OutputDirectory = output
          LayoutRoots = layoutRoots
          ProjectParameters  = ("root", root)::info
          Template = docTemplate } )
)

Target.create "GenerateDocs" ignore
Target.create "BeginRelease" ignore
Target.create "PublishRelease" ignore

open Fake.Core.TargetOperators

"GitTagAndPush"
?=> "GenerateAssemblyInfoFile"
==> "Build"
==> "Test"
==> "PaketClean"
==> "PaketPack"
==> "BeginRelease"
==> "PaketPush"
==> "GitHubRelease"
==> "PublishRelease"

"ValidateGitHubCredentials"
?=> "BeginRelease"

"ValidateGitHubCredentials"
==> "GitHubRelease"

"ValidateNugetApiKey"
?=> "BeginRelease"

"ValidateNugetApiKey"
==> "PaketPush"

// Only do a GitTagAndPush if we're pushing a new version
"GitTagAndPush"
==> "PaketPush"

"CleanDocs"
  ==>"Docs"
  ==> "ReferenceDocs"
  ==> "GenerateDocs"

Target.runOrDefault "PaketPack"
