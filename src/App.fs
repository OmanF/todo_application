module App

// Imports //
open System
open Elmish
open Elmish.React
open Feliz

// Model types //
type TodoCompleted =
    // Using explicit type with well defined names feels more correct than a bool.
    | TodoCompleted
    | TodoPending

type EditStatus =
    | Initialized
    | StartedEditing
    | ActiveEditing
    | Edited

type Todo =
    { Id: Guid
      CurrentText: string
      OriginalText: string
      EditStatus: EditStatus
      Completed: TodoCompleted }

type CompletionStatusFilterTab =
    | All
    | Completed
    | Pending

// Model //
type State =
    { NewTodo: string
      TodoList: Todo list
      PersistedTodoList: Todo list // `TodoList` changes based on the view. This holds the *complete* state.
      SelectedFilterTabGUI: CompletionStatusFilterTab }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid
    | CancelEdit of Guid
    | ApplyEdit of Guid
    | StartEditingTodo of Guid
    | SetEditedDescription of string * Guid
    | ChangeFilterTab of CompletionStatusFilterTab

// Update //
let init () =
    { NewTodo = ""
      TodoList = []
      PersistedTodoList = []
      SelectedFilterTabGUI = All }

let update (msg: Msg) (state: State): State =
    match msg with
    (*
     * When we enter new input to the input field, change the current "candidate"'s text to reflect what the user is seeing.
     * We only change the temporary list, `state.TodoList` since this *is* a visual change only. No actual todo's state is being changed.
     *)
    | SetNewTodo desc -> { state with NewTodo = desc }

    (*
     * For adding a new todo, if the text in the input field is the empty string, there's nothing to actually add, so return the state as it is.
     * Otherwise, we add the new todo to the persisted list, no matter what (obviously), but we only put in on the displayed list, `state.TodoList`
     * if the status tab is not `Completed`, since any new todo is, by definition, not completed (again, luckily, the other tabs are `All` and `Pending`
     * and a new, pending, todo fits both these filters).
     *)
    | AddNewTodo when state.NewTodo = "" -> state
    | AddNewTodo ->
        let nextTodo =
            { Id = Guid.NewGuid()
              CurrentText = state.NewTodo
              OriginalText = state.NewTodo
              EditStatus = Initialized
              Completed = TodoPending }

        { state with
              NewTodo = ""
              TodoList =
                  if state.SelectedFilterTabGUI <> Completed then
                      List.append state.TodoList [ nextTodo ]
                  else
                      state.TodoList
              PersistedTodoList = List.append state.PersistedTodoList [ nextTodo ] }

    | ToggleCompleted todoId ->
        (*
         * I've used this method throughout the code to update both lists with one go.
        * It's sub-optimal, being a non-exhaustive match, but it's the fastest way to destructure
        * the list-of-lists data structure, and since I'm defining the list-of-lists in the first
        * place, I'm guaranteed it's an exhaustive match.

        * Per each list, iterate through the items (the todos), until finding the item whose ID matches the one that triggered the call.
        * Once found, toggle it's state (luckily, this is a binary toggle so the code is of form "If state A, switch to B, if state B, switch to A".

        * We toggle update both lists, including the persisted one, since this effects the todo's state, it is more than just a visual cue for the user (that also happens, but is only half of what's *actually* happening).
        *)

        let [ nextTodoList; nextPersistedTodoList ] =
            [ state.TodoList
              state.PersistedTodoList ]
            |> List.map (fun currentTodoList ->
                currentTodoList
                |> List.map (fun todo ->
                    if todo.Id = todoId then
                        if todo.Completed = TodoCompleted then
                            { todo with Completed = TodoPending }
                        else
                            { todo with Completed = TodoCompleted }
                    else
                        todo))

        { state with
              TodoList =
                  match state.SelectedFilterTabGUI with
                  (*
                   * This function, `ToggleCompleted` gets called when the `Completed` button gets toggled.
                   * Since the button can be clicked when the filter is not necessarily `All`,
                   * it's this function responsibility to compute the todo's new state and only display
                   * it if the new state conforms with the current filter's state.
                   *)
                  | All -> nextPersistedTodoList // `All` is the *complete* state, i.e. the persisted list
                  | Pending ->
                      nextTodoList
                      |> List.filter (fun todo -> todo.Completed = TodoPending)
                  | Completed ->
                      nextTodoList
                      |> List.filter (fun todo -> todo.Completed = TodoCompleted)

              PersistedTodoList = nextPersistedTodoList }

    (*
     * In order to delete a specific todo we comb the list of todos for the todo with the matching ID, and we filter it out
     * (or, more accurately, we filter, and keep, all others).
     * Again, both current *and* persisted lists are updated since we change a todo's state, in this particular case, making its state non-existent anymore, but still, a state change).
     *)
    | DeleteTodo todoId ->
        let [ nextTodoList; nextPersistedTodoList ] =
            [ state.TodoList
              state.PersistedTodoList ]
            |> List.map (fun currentTodoList ->
                currentTodoList
                |> List.filter (fun todo -> todo.Id <> todoId))


        { state with
              TodoList = nextTodoList
              PersistedTodoList = nextPersistedTodoList }

    (*
     * A simple function to mark a todo as being edited.
     * Since this implementation takes the path of marking each todo's "editable" state in the todo's state itself (as opposed to keeping a list of editable todos),
     * I need to mark each todo that I clicked its "Edit" button separately.

     * Scan the temporary list until finding the triggering todo and simply edit its state to show it is now being edited (though the nature of the edition is undetermined at the moment).

     * This only changes the temporary list, `state.TodoList` since, while this is *not* a visual only function, it *does* have some state change to a todo,
     * the acted upon todo is not final yet, the edition can be cancelled instead of applied, and until a *final* "verdict" is cast, there can be no persisted change!
     *)
    | StartEditingTodo todoId ->
        let transformIntoEditable todo =
            { todo with
                  EditStatus = StartedEditing }

        { state with
              TodoList =
                  List.map (fun todo -> if todo.Id = todoId then transformIntoEditable todo else todo) state.TodoList }

    (*
     * Same as setting the input field, but for each specific, triggering, todo by itself.
     * As usual, only for the temporary list since this doesn't finalize the todo's state and *might* be considered (if you close one eye and blink hard with the other) a "visual" change.
     *)
    | SetEditedDescription (newText, todoId) ->
        let nextTodoList =
            state.TodoList
            |> List.map (fun currentTodo ->
                if currentTodo.Id = todoId then
                    if currentTodo.OriginalText <> newText then
                        { currentTodo with
                              EditStatus = ActiveEditing
                              CurrentText = newText }
                    else
                        { currentTodo with
                              EditStatus = StartedEditing }
                else
                    currentTodo)

        { state with TodoList = nextTodoList }

    (*
     * The fun part. Really, "Happy, happy! Joy, joy!" kind of fun.
     * First, we scan the temporary list and finalize each triggering todo. By "finalize" we also change its `OriginalText` to the `CurrentText`, the text the user sees on screen.
     * Since we decided to submit the edit, the todo's new "original" text is the current one.
     * One exception is if the new todo is the empty string where, according to the "specs" we decide that means the user actually made a mistake and wished to cancel the edition
     * and we revert to its previous state.

     * Once done with the temporary list, we scan the persisted list, one item at a time and compare each persisted item to the displayed items.
     * If we didn't find a match, that is we are viewing a filtering of the list and the persisted todo doesn't match the filter (a completed todo, when we view the pending list, or vice-versa)
     * we do nothing (i.e. return the persisted todo as is).
     * If we found a match we change the persisted todo's state to match the state of the corresponding temporary todo.
     *)
    | ApplyEdit todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map (fun todo ->
                if todo.Id = todoId then
                    match todo.CurrentText with
                    | "" ->
                        { todo with
                              CurrentText = todo.OriginalText
                              EditStatus = Initialized }
                    | _ ->
                        { todo with
                              EditStatus = Edited
                              OriginalText = todo.CurrentText }
                else
                    todo)

        let nextPersistedTodoList =
            state.PersistedTodoList
            |> List.map (fun pTodo ->
                let matchTodo =
                    nextTodoList
                    |> List.tryFind (fun cTodo -> cTodo.Id = pTodo.Id)

                match matchTodo with
                | None -> pTodo
                | Some mTodo ->
                    { pTodo with
                          EditStatus = Edited
                          OriginalText = mTodo.OriginalText
                          CurrentText = mTodo.OriginalText })

        { state with
              TodoList = nextTodoList
              PersistedTodoList = nextPersistedTodoList }

    (*
     * To cancel an edit we simply find the todo triggering the cancellation and revert its state to before the edit button was clicked. Really, that's all.
     *)
    | CancelEdit todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map (fun todo ->
                if todo.Id = todoId then
                    { todo with
                          EditStatus = Initialized
                          CurrentText = todo.OriginalText }
                else
                    todo)

        { state with TodoList = nextTodoList }

    (*
     * When we change the filter tab we go over the *persisted* list and filter to keep all the todos whose completed status matches that of the filter.
     * This is obviously a change for the temporary list only, since this is a hallmark example of a visual-only, no inherent state change, function.
     *)
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

// View //
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
                          prop.text todo.CurrentText ] ]

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

let renderEditForm (todo: Todo) (dispatch: Msg -> unit) =
    div [ Blm.Box ]
        [ div [ Blm.Field; Blm.IsGrouped ]
              [ div [ Blm.Control; Blm.IsExpanded ]
                    [ Html.input
                        [ prop.classes [ Blm.Input; Blm.IsMedium ]
                          prop.defaultValue todo.CurrentText
                          prop.onTextChange (fun newText -> dispatch (SetEditedDescription(newText, todo.Id))) ] ]

                div [ Blm.Control; Blm.Buttons ]
                    [ Html.button
                        [ prop.classes
                            [ Blm.Button
                              if todo.EditStatus = ActiveEditing then Blm.IsPrimary else Blm.IsOutlined ]
                          prop.onClick (fun _ -> dispatch (ApplyEdit todo.Id))
                          prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaSave ] ] ] ]

                      Html.button
                          [ prop.classes [ Blm.Button; Blm.IsWarning ]
                            prop.onClick (fun _ -> dispatch (CancelEdit todo.Id))
                            prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaArrowRight ] ] ] ] ] ] ]

let todoList state dispatch =
    Html.ul
        [ prop.children
            [ for todo in state.TodoList ->
                if (todo.EditStatus = StartedEditing
                    || todo.EditStatus = ActiveEditing) then
                    renderEditForm todo dispatch
                else
                    renderTodo todo dispatch ] ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div  // Can't be shortcut-ed via the `div` function above because it has style and no classes!
        [ prop.style [ style.padding 20 ]
          prop.children
              [ appTitle
                inputField state dispatch
                renderFilterTabs state dispatch
                todoList state dispatch ] ]

// Run application //
Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
