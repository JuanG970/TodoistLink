BeginPackage["TodoistLink`"]

FetchTodoistProjects
TodoistProjects
TodoistTasks
GetTodoistProject
TodoistProjectTasks
$TodoistDB 
$TodoistFolder
closeTodoistDBConnection
parseTodoistFilter
NewTodoistTask
CloseTodoistTask
ReopenTodoistTask


(* remove this - not needed or private *)
getTodoistLabels
parseTodoistColor

$TodoistAPIToken := 
  With[
    {key = SystemCredential["TodoistAPIToken"]},
    If[!MissingQ@key,
      key["Data"]["Todoist API Token"]
      ,
      key
    ]
  ]
InitTodoistLink


(* Objects and wrappers *)
TodoistTask
TodoistLabel
TodoistProject
TodoistUser
TodoistLink


Begin["`Private`"]
Needs["DatabaseLink`"]
Needs["ImportMarkdown`"]
$TodoistFolder = FileNameJoin@{FileNameDrop[$UserBaseDirectory, -1], "TodoistLink"};
$TodoistDBPath = FileNameJoin@{$TodoistFolder, "todoistDB.hsqldb"};
$TodoistAPITasksEndPoint = "https://api.todoist.com/rest/v2/tasks";
$TodoistAPIEndPoint = "https://api.todoist.com/rest/v2";
pacletObj = PacletObject["TodoistLink"];
(* Extract the full path to a resource within the paclet: *)
flagICO = Import @ pacletObj["AssetLocation", "flag"];
calendarICO = Import @ pacletObj["AssetLocation", "calendar"];
todoistICO = Import @ pacletObj["AssetLocation", "todoist"];


openTodoistDBConnection[] := 
  If[!SQLConnectionOpenQ[$TodoistDB],
    $TodoistDB = 
      OpenSQLConnection[JDBC["HSQL(Standalone)", $TodoistDBPath]]
    , (* Else *)
    $TodoistDB
  ]

closeTodoistDBConnection[] := CloseSQLConnection[$TodoistDB]

(* TODO: Change table values *)
createTodoistDB[] := SQLExecute[$TodoistDB,
"CREATE TABLE Projects (
id bigint NOT NULL, 
parent_id varchar(255), 
ordering varchar(255),
color varchar(255), 
name varchar(255), 
comment_count varchar(255), 
is_shared bit, 
is_favourite bit, 
is_inbox_project bit, 
is_team_inbox bit,
url varchar(255),
view_style varchar(255),
PRIMARY KEY (id)
)"]

InitTodoistLink[] := 
  Module[
  {apikey},
  Once @
    Enclose[
      (* If there is no API Token defined, ask for one *)
      If[MissingQ @ $TodoistAPIToken,
        apikey = AuthenticationDialog[
          {"Todoist API Token"}, 
          SystemCredentialKey -> "TodoistAPIToken"
        ];
        ConfirmMatch[
          apikey,
          _Association,
          "API Token needed"
        ];
      ];

      If[!FileExistsQ @ $TodoistFolder,
        (* Create the app folder *)
        CreateDirectoryIfNeeded[$TodoistFolder];
        (* Init the database *)
        ConfirmMatch[
          openTodoistDBConnection[],
          _SQLConnection,
          "No database connection"
        ];
        createTodoistDB[];
        , (* Else, the database is already there *)
        (* Open the database connection *)
        ConfirmMatch[
          openTodoistDBConnection[],
          _SQLConnection,
          "No database connection"
        ];
      ];
      , (* Enclose - Error protection *)
      (
        Message[TodoistLink::InitFailure, #["Information"]];
        Abort[]
      )&
    ]
  ]

InitTodoistLink[];


(* ::Section:: *)
(* Project related *)

(* TODO: Should we use the Sync API? *)
(* https://developer.todoist.com/sync/v9/#add-an-item *)

columns = {"id", "parent_id", "ordering", "color", "name", 
  "comment_count", "is_shared", "is_favorite", "is_inbox_project", 
  "is_team_inbox", "url", "view_style"};


AddProjectToDB[project_Association] :=
 SQLExecute[$TodoistDB,
"INSERT INTO Projects (id, parent_id, ordering, color, name, \
comment_count, is_shared, is_favourite, is_inbox_project, \
is_team_inbox, url, view_style) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, \
?, ?)",
  Values@project
  ]


FetchTodoistProjects[] :=
 Module[{projects},
  projects = Association /@ URLExecute[
     HTTPRequest[
      "https://api.todoist.com/rest/v2/projects",
      <|
       Method -> "GET",
       "Headers" -> {"Authorization" -> "Bearer " <> $TodoistAPIToken}
       |>
      ], "JSON"
     ];
  Quiet@AddProjectToDB /@ projects;
  Association[Thread[columns -> #]] & /@ 
   SQLExecute[$TodoistDB, "select * from Projects"]
  ]

TodoistProjects := 
 TodoistProject[Association[Thread[columns -> #]]] & /@ 
  SQLExecute[$TodoistDB, "select * from Projects"]

getProjectByID[id_] := 
    Quiet @ Check[
    First@Cases[
    TodoistProjects/.{TodoistProject[info_] :> info},
    <|___, "project_id" -> id, __|>
  ], <|"name" -> "No project found"|>
  ]

(* Get project by name *)
GetTodoistProject[name_String] :=
  Block[{project, projs, id, editDistance},
  (* compare the name with edit distance and throw error if it is to big (greater than 6 or so) *)
  projs = TodoistProjects/.{TodoistProject[info_]:> info};
  project = TodoistProject @ Catch@
    Throw@
     Part[projs, 
      First @ Ordering[(
            If[#name == name, Throw@#];
            EditDistance[name, #name]
            ) & /@ projs,
           1
          ]
      ];
  If[EditDistance[name, #name] &@ First[project] > 6, Return @ Missing["ProjectNotFound"] ];
  Return @ project;
  ]


(* TODO: Create a project *)


(* FUTURE: Delete a project *)

(* ::Section:: *)
(* Sections related *)


(* TODO: Get all sections *)


(* TODO: Create a new section *)


(* ::Section:: *)
(* Task related *)

todoistPriorityParse =
  <|
   4 -> Red,
   3 -> Orange,
   2 -> Darker@Blue,
   1 -> Gray
   |>;


parseTodoistColor[color_String] := 
  With[
    {interpretedColor = RGBColor[color]},
    If[!ColorQ @ interpretedColor, Return @ White, Return @ interpretedColor]
  ]


ClearAll@TodoistProjectTasks
TodoistProjectTasks["ProjectID" -> id_Integer] :=
  (TodoistTask[parseTodoistTask@ResourceFunction["ToAssociations"][#]]) & /@ URLExecute[
   HTTPRequest[
    $TodoistAPITasksEndPoint,
    <|
     Method -> "GET",
      "Headers" -> {"Authorization" -> "Bearer " <> $TodoistAPIToken},
      "Body" -> <|"project_id" -> ToString@id|>
     |>
    ]
   ]

Attributes[parseTodoistFilter] = {HoldAll};
parseTodoistFilter[expr_] :=
  StringReplace[
    ToString[Unevaluated[expr]],
    {
      "&&" -> "&",
      "Today[]"|"Today" -> "today",
      "||" -> "|"
    }
  ]

Attributes[TodoistTasks] = {HoldFirst};
TodoistTasks["Filter" -> filter_, parseQ_: True] :=
  TodoistTask[parseTodoistTask@ResourceFunction["ToAssociations"][#]] &/@ (URLExecute @ HTTPRequest[
    $TodoistAPITasksEndPoint,
    <|
      Method -> "GET",
      "Headers" -> {
        "Authorization" -> "Bearer " <> $TodoistAPIToken,
        "Accept" -> "application/json",
        "Content-Type" -> "application/json"
      },
      "Body" -> <|"filter" -> If[parseQ, parseTodoistFilter@filter, filter]|>
     |>
    ])

TodoistProjectTasks[name_String] :=
  TodoistProjectTasks["ProjectID" -> GetTodoistProject[name]["id"]]


parseTodoistTask[task_Association] :=
  Module[
    {interpretedTask, projectInfo},
    interpretedTask = task;
    projectInfo = getProjectByID[task["project_id"]];
    If[task["due"] =!= Null,
      If[KeyMemberQ[task["due"], "datetime"],
        interpretedTask["due", "datetime"] =
          TimeZoneConvert[FromDateString[
            StringRiffle@ StringSplit[ task["due", "datetime"], { "T", "Z" } ] ,
            TimeZone -> "UTC"]
          ]
      ];
      interpretedTask["due", "date"] = FromDateString @ task["due", "date"];

      (* TODO: Parse project_id *)
      AssociateTo[interpretedTask, "project" -> projectInfo["name"]];

      (* TODO: Parse string recurrence key *)

      (* TODO: Parse labels *)

    ];

    Return @ interpretedTask
  ]

parseTodoistLabel[label_Association] :=
  Module[
    {parsedLabel = label, color},
    parsedLabel["color"] = parseTodoistColor@label["color"];
    Return @ TodoistLabel[parsedLabel];
  ]

getTodoistLabels[] :=
    parseTodoistLabel[Association@#] & /@ 
    URLExecute @
    HTTPRequest[
    URLBuild[{$TodoistAPIEndPoint, "labels"}],
      <|
        Method -> "GET",
        "Headers" -> {
          "Authorization" -> "Bearer " <> $TodoistAPIToken
        }
      |>
    ];


(* Task related wrapper *)
TodoistTask[asc_?AssociationQ][prop_] := Lookup[asc, prop]
TodoistTask[asc_?AssociationQ]["Properties"] := Keys[asc]

CloseTodoistTask[task_TodoistTask, opt: OptionsPattern[]] :=
  With[{
  r = URLExecute @
   HTTPRequest[
    URLBuild[{$TodoistAPITasksEndPoint, ToString @ task["id"], "close"}],
    <|
     Method -> "POST",
      "Headers" -> {"Authorization" -> "Bearer " <> $TodoistAPIToken}
    |>
    ]},
  If[r === "", 
    Return @ 
      Success["TaskClosed",
       <|"MessageTemplate" :> "Task:\"`t`\" marked as Done.", 
          "MessageParameters" -> <|"t" -> task["content"]|>,
        "TimeStamp" -> DateString[]
       |>
      ]
    , (* Else *)
    Return @ 
      Failure["InvalidResponse",  <|
        "MessageTemplate" -> "Todoist API unexpected response when closing the task `t` with task id `id`. Response: `res`",
        "MessageParameters" -> <|"t" -> task["content"], "id" -> task["id"], "res" -> r|>
      |>]
  ]
  ]

ReopenTodoistTask[task_TodoistTask, opt: OptionsPattern[]] :=
  URLExecute @
   HTTPRequest[
    URLBuild @ {$TodoistAPITasksEndPoint, ToString @ task["id"], "reopen"},
    <|
     Method -> "POST",
      "Headers" -> {"Authorization" -> "Bearer " <> $TodoistAPIToken}
    |>
   ]

(* TODO: NewTask[ ] *)
ClearAll[NewTodoistTask]
NewTodoistTask // Options = {
  "Description" -> Nothing,
  "Project" -> Nothing,
  "Section" -> Nothing,
  "Parent" -> Nothing,
  "Labels" -> {},
  "Priority" -> 1,
  "Due" -> Nothing
}

NewTodoistTask[content_String, opt: OptionsPattern[]] :=
  Module[
    {r, body},
    Block[
      {
        interpretedLabel,
        interpretedParent,
        interpretedProject,
        inferredPriority,
        interpretedDescription
      },
      (* Check description of the task *)
      With[
        {arg = OptionValue["Description"]},
        If[!MatchQ[arg, _String | Nothing],
          Message[NewTodoistTask::BadOptions, "Description", "A string was expected"];
          Return @ $Failed
          , (* Else data is correct *)
          interpretedDescription = arg;
        ];
      ];
      (* Check Priority *)
      With[
        {arg = OptionValue["Priority"]},
        If[! (arg <= 4 && arg >= 1 && IntegerQ[arg]),
          Message[NewTodoistTask::BadOptions, "Priority", "Priority should be an integer between 1 and 4. Inferring priority."];
          inferredPriority = First @ Nearest[Range[1, 4]][arg];
          , (* Else - Priority in range*)
          inferredPriority = OptionValue["Priority"]
        ];
      ];
      (* TODO: Section *)
        (* TODO: getProjectSections[project] *)
      (* TODO: Labels *)
        (* Parse labels *)
      (* TODO: Due *)
      (* TODO: Parent *)
        (* TODO: Ensure that parent exists *)
      (* TODO: Ensure that project exists or try to find it *)
        (* Find project name *)
      (* TODO: Construct the request *)
      body = Association @ Flatten @ {
        If[OptionValue["Description"] != Nothing, 
          "description" -> OptionValue["Description"]
          , (* Else *)
          Nothing
        ],
        If[OptionValue["Priority"] != Nothing, 
          "priority" -> inferredPriority
          , (* Else *)
          Nothing
        ]
      };
      r = URLExecute @
      HTTPRequest[
        $TodoistAPITasksEndPoint,
        <|
        Method -> "POST",
        "Headers" -> {
          "Authorization" -> "Bearer " <> $TodoistAPIToken,
          "Content-Type" -> "application/json",
          "X-Request-Id" -> CreateUUID[]
        },
        "Body" -> ExportString[body, "RawJSON"]
        |>
      ];
      If[MatchQ[r, _List],
        (* Request succeeded - Return a task object *)
        Return @ TodoistTask @ Association @ r;
        , (* Else - Task creation failed *)
        Return @ Failure["InvalidResponse",  <|
          "MessageTemplate" -> "Todoist API unexpected response when creating the task \"`t`\". Server Response: `res`",
          "MessageParameters" -> <|"t" -> content, "res" -> r|>
        |>];
      ];
    ];
  ]

NewTodoistTask::BadOptions="Option `1` is not a valid option. `2`";


(* TODO: EditTask[ ] *)


(* Visually format the tasks and parse date and similar constructs *)
TodoistTask /: MakeBoxes[obj: TodoistTask[task_Association], form: StandardForm] :=
  BoxForm`ArrangeSummaryBox[
    TodoistTask,
    obj,
    Image[todoistICO, ImageSize -> {20,20}],
    {
     (* Task title *)
     BoxForm`SummaryItem[{"", Style[ImportMarkdown@task["content"], 14] }],
     If[task["description"] != "",
       (* Task description *)
       BoxForm`SummaryItem[{"Description: ", ImportMarkdown @ task["description"]}]
       , (* Else *)
       Nothing
     ],
     If[task["due"] =!= Null,
      (* Due date *)
      BoxForm`SummaryItem[{"Due: " , 
        If[!MissingQ @ task["due", "datetime"],
          task["due", "datetime"],
          task["due", "date"]
        ]
      }]
      , (* Else *)
      Nothing
     ]
    },
    {
     BoxForm`SummaryItem[{"Expanded thing 1: ", "Hidden"} ], 
     BoxForm`SummaryItem[{"Expanded thing 1: ", "Hidden"} ], 
     BoxForm`SummaryItem[{"Expanded thing 1: ", "Hidden"} ]
    },
    form,
    "Interpretable" -> Automatic
  ]

(* ::Section:: *)
(* Macros *)
(* TODO: TodayTasks *)
(* TODO: This notebook tasks *)

(*
  Ideally we should link a project notebook to a Todoist project
*)

(* ::Section:: *)
(* Messages *)
TodoistLink::InitFailure="Failure to initialize the application. `1`";

(* ::Section:: *)
(* Utilities *)

ClearAll[CreateDirectoryIfNeeded];
Attributes[CreateDirectoryIfNeeded] = {Listable};
CreateDirectoryIfNeeded::InvalidArguments = 
  "Invalid arguments, String or list of Strings expected instead of \
`1`";
CreateDirectoryIfNeeded[path_String] := 
 If[! DirectoryQ@path && ! FileExistsQ@path, 
  CreateDirectory[path],(*Else*)path]
CreateDirectoryIfNeeded[
  expr___] := (Message[
   CreateDirectoryIfNeeded::InvalidArguments, {expr}]; $Failed)

End[] (* End `Private` *)

EndPackage[]
