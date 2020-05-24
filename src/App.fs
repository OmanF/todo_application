module App

// Imports
open System
open Elmish
open Elmish.React
open Feliz

// Model types
type TodoCompleted =
    | TodoCompleted
    | TodoPending

type Todo =
    { Description: string
      Completed: TodoCompleted
      Id: Guid }

type TodoBeingEdited = { Id: Guid; Description: string }

// Type to model current state of the completion status filter
type CompletionStatusFilterTab =
    | All
    | Completed
    | Pending

// Model
type State =
    { TodoBeingEdited: TodoBeingEdited option
      TodoList: Todo list
      PersistedTodoList: Todo list // `TodoList` changes based on the view. This holds the *complete* state
      NewTodo: string
      SelectedFilterTabGUI: CompletionStatusFilterTab }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid
    | CancelEdit
    | ApplyEdit
    | StartEditingTodo of Guid
    | SetEditedDescription of string
    | ChangeFilterTab of CompletionStatusFilterTab

let init () =
    { TodoList = []
      PersistedTodoList = []
      NewTodo = ""
      TodoBeingEdited = None
      SelectedFilterTabGUI = All }

// Update
let update (msg: Msg) (state: State): State =
    match msg with
    | SetNewTodo desc -> { state with NewTodo = desc }
    | ToggleCompleted todoId ->
        // I've used this method throughout the code to update both lists with one go
        // It's sub-optimal, being a non-exhaustive match, but it's the fastest way to destructure
        // the list-of-lists data structure, and since I'm defining the list-of-lists in the first
        // place, I'm guaranteed it's an exhaustive match
        let [ nextTodoList; nextPersistedTodoList ] =
            [ state.TodoList
              state.PersistedTodoList ]
            |> List.map (fun currentTodoList ->
                currentTodoList
                |> List.map (fun todo ->
                    if todo.Id = todoId then // Traverse the todo list to find the one that has been clicked
                        if todo.Completed = TodoCompleted then // Toggle its state
                            { todo with Completed = TodoPending }
                        else
                            { todo with Completed = TodoCompleted }
                    else // That's not the right todo
                        todo))

        { state with
              TodoList =
                  match state.SelectedFilterTabGUI with
                  // This function gets called when the `Completed` button gets toggled
                  // Since the button can be clicked when the filter is not necessarily `All`,
                  // it's this function responsibility to compute the todo's new state and only display
                  // it if the new state conforms with the filter's state
                  | All -> nextPersistedTodoList // `All` is the *complete* state, i.e. the persisted list
                  | Pending ->
                      nextTodoList
                      |> List.filter (fun todo -> todo.Completed = TodoPending)
                  | Completed ->
                      nextTodoList
                      |> List.filter (fun todo -> todo.Completed = TodoCompleted)

              PersistedTodoList = nextPersistedTodoList }

    | AddNewTodo when state.NewTodo = "" -> state
    | AddNewTodo ->
        let nextTodo =
            { Id = Guid.NewGuid()
              Description = state.NewTodo
              Completed = TodoPending }

        { state with
              NewTodo = ""
              TodoList =
                  // Only add to the tentative list, the one whose content is displayed on screen, if the filter
                  // is *not* set to `Completed`, since any new todo added, by definition is not completed
                  if state.SelectedFilterTabGUI <> Completed then
                      List.append state.TodoList [ nextTodo ]
                  else
                      state.TodoList
              // You *always* amend the *persisted* list, no matter what
              PersistedTodoList = List.append state.PersistedTodoList [ nextTodo ] }

    | DeleteTodo todoId ->
        let [ nextTodoList; nextPersistedTodoList ] =
            [ state.TodoList
              state.PersistedTodoList ]
            |> List.map (fun currentTodoList ->
                currentTodoList
                // Only keep the todos that don't match, i.e. "delete" the todo
                |> List.filter (fun todo -> todo.Id <> todoId))

        { state with
              TodoList = nextTodoList
              PersistedTodoList = nextPersistedTodoList }

    | StartEditingTodo todoId ->
        let nextEditModel =
            state.TodoList
            |> List.tryFind (fun todo -> todo.Id = todoId)
            |> Option.map (fun todo ->
                { Id = todoId
                  Description = todo.Description })

        { state with
              TodoBeingEdited = nextEditModel }

    | CancelEdit -> { state with TodoBeingEdited = None }

    | ApplyEdit ->
        match state.TodoBeingEdited with
        | None -> state
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
        | Some todoBeingEdited ->
            let [ nextTodoList; nextPersistedTodoList ] =
                [ state.TodoList
                  state.PersistedTodoList ]
                |> List.map (fun currentTodoList ->
                    currentTodoList
                    |> List.map (fun todo ->
                        if todo.Id = todoBeingEdited.Id then
                            { todo with
                                  Description = todoBeingEdited.Description }
                        else
                            todo))

            { state with
                  TodoList = nextTodoList
                  PersistedTodoList = nextPersistedTodoList }

    | SetEditedDescription newText ->
        let nextEditModel =
            state.TodoBeingEdited
            |> Option.map (fun todoBeingEdited ->
                { todoBeingEdited with
                      Description = newText })

        { state with
              TodoBeingEdited = nextEditModel }

    | ChangeFilterTab newFilter ->
        match newFilter with
        | All ->
            { state with
                  TodoList = state.PersistedTodoList
                  SelectedFilterTabGUI = All }
        | Completed ->
            let nextTodo =
                state.PersistedTodoList
                |> List.filter (fun todo -> todo.Completed = TodoCompleted)

            { state with
                  TodoList = nextTodo
                  SelectedFilterTabGUI = Completed }
        | Pending ->
            let nextTodo =
                state.PersistedTodoList
                |> List.filter (fun todo -> todo.Completed = TodoPending)

            { state with
                  TodoList = nextTodo
                  SelectedFilterTabGUI = Pending }

// View
// Helper function to dish-out `div` elements quickly
let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div
        [ prop.classes classes
          prop.children children ]

let appTitle =
    Html.p
        [ prop.className Blm.Title
          prop.text "Elmish Todo List" ]

let inputField state dispatch =
    div [ Blm.Field; Blm.HasAddons ]
        [ div [ Blm.Control; Blm.IsExpanded ]
              [ Html.input
                  [ prop.classes [ Blm.Input; Blm.IsMedium ]
                    prop.valueOrDefault state.NewTodo
                    prop.onChange (SetNewTodo >> dispatch) ] ]

          div [ Blm.Control ]
              [ Html.button
                  [ prop.classes
                      [ Blm.Button
                        Blm.IsPrimary
                        Blm.IsMedium ]
                    prop.onClick (fun _ -> dispatch AddNewTodo)
                    prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaPlus ] ] ] ] ] ]

let renderFilterTabs (state: State) dispatch =
    div
        [ Blm.Tabs
          Blm.IsToggle
          Blm.IsFullwidth ]
        [ Html.ul
            [ Html.li
                [ prop.className [ state.SelectedFilterTabGUI = All, Blm.IsActive ]
                  prop.onClick (fun _ -> dispatch (ChangeFilterTab All))
                  prop.children [ Html.a [ prop.text "All" ] ] ]

              Html.li
                  [ prop.className [ state.SelectedFilterTabGUI = Completed, Blm.IsActive ]
                    prop.onClick (fun _ -> dispatch (ChangeFilterTab Completed))
                    prop.children [ Html.a [ prop.text "Completed" ] ] ]

              Html.li
                  [ prop.className [ state.SelectedFilterTabGUI = Pending, Blm.IsActive ]
                    prop.onClick (fun _ -> dispatch (ChangeFilterTab Pending))
                    prop.children [ Html.a [ prop.text "Pending" ] ] ] ] ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
    div [ Blm.Box ]
        [ div
            [ Blm.Columns
              Blm.IsMobile
              Blm.IsVcentered ]
              [ div [ Blm.Column; Blm.Subtitle ]
                    [ Html.p
                        [ prop.className Blm.Subtitle
                          prop.text todo.Description ] ]

                div [ Blm.Column; Blm.IsNarrow ]
                    [ div [ Blm.Buttons ]
                          [ Html.button
                              [ prop.className
                                  [ true, Blm.Button
                                    todo.Completed = TodoCompleted, Blm.IsSuccess ]
                                prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
                                prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaCheck ] ] ] ]

                            Html.button
                                [ prop.classes [ Blm.Button; Blm.IsPrimary ]
                                  prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
                                  prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaEdit ] ] ] ]

                            Html.button
                                [ prop.classes [ Blm.Button; Blm.IsDanger ]
                                  prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                                  prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaTimes ] ] ] ] ] ] ] ]

let renderEditForm (todoBeingEdited: TodoBeingEdited) (dispatch: Msg -> unit) =
    div [ Blm.Box ]
        [ div [ Blm.Field; Blm.IsGrouped ]
              [ div [ Blm.Control; Blm.IsExpanded ]
                    [ Html.input
                        [ prop.classes [ Blm.Input; Blm.IsMedium ]
                          prop.valueOrDefault todoBeingEdited.Description
                          prop.onTextChange (SetEditedDescription >> dispatch) ] ]

                div [ Blm.Control; Blm.Buttons ]
                    [ Html.button
                        [ prop.classes [ Blm.Button; Blm.IsPrimary ]
                          prop.onClick (fun _ -> dispatch ApplyEdit)
                          prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaSave ] ] ] ]

                      Html.button
                          [ prop.classes [ Blm.Button; Blm.IsWarning ]
                            prop.onClick (fun _ -> dispatch CancelEdit)
                            prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaArrowRight ] ] ] ] ] ] ]

let todoList state dispatch =
    Html.ul
        [ prop.children
            [ for todo in state.TodoList ->
                match state.TodoBeingEdited with
                | Some todoBeingEdited when todoBeingEdited.Id = todo.Id -> renderEditForm todoBeingEdited dispatch
                | _ -> renderTodo todo dispatch ] ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div  // Can't be shortcut-ed via the `div` function above because it has style and no classes!
        [ prop.style [ style.padding 20 ]
          prop.children
              [ appTitle
                inputField state dispatch
                renderFilterTabs state dispatch
                todoList state dispatch ] ]

// Run application
Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
