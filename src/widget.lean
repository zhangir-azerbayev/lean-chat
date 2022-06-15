import system.io
import query_api 

open widget tactic
section json 
meta def list.lookup_prod {α β} : (list (α × β)) → (α → bool) → option β
| [] _ := none
| (⟨a,b⟩::xs) p := if p a then pure b else xs.lookup_prod p

open except

meta def json.lookup : json → string → except string json
| (json.object kvs) str := 
  match kvs.lookup_prod $ λ k, k = str with
  | some v := except.ok v
  | none := except.error ("no key " ++ str)
  end
| _ _ := except.error "not an object"

meta def json.as_string : json → except string string
  | (json.of_string s) := except.ok s
  | _ := except.error "not a string"

meta def json.as_array : json → except string (list json)
  | (json.array xs) := ok xs
  | _ := error "not an array"

meta def except.liftOption {α}: option α → except string α
  | none := except.error "option was none"
  | (some a ) := except.ok a

end json 

meta def text_of_return_json (parsed : json) : except string string := do
  choices_json ← json.lookup parsed "choices",
  head :: _ ← json.as_array choices_json | except.error "empty array",
  t ← json.lookup head "text",
  s ← json.as_string t,
  return s

meta def run_except {α} : except string α → io α
  | (except.ok a) := pure a
  | (except.error e) := io.fail e
  


meta structure bubble :=
  (body : string) -- [todo] add formatting etc
  (user : string)

meta def chat_props := unit

meta structure chat_state : Type :=
  (bubbles : list bubble)
  (current_text : string)

/- @zhangir: write your code for getting response from codex here :-) -/
meta def get_response : chat_state → io string
| state := do {
    (head :: tail) ← pure $ state.bubbles | io.fail "no chat yet",
    let s := 
      match tail with
      | [] := head
      | tail := _
      end,
    let prompt := prompt_of_nl_statement s few_shot_prompt,  
    return_json ← get_completion_of_request {prompt:=prompt},
    io.put_str return_json,  
    (some maybe_return_parsed) ← pure (json.parse return_json) | io.fail "not json",
    t : string ← run_except $ text_of_return_json maybe_return_parsed,
    return (t ++ " :=")
  }

-- use some @gebner magic here
meta def unsafe_perform_io {α} (m : io α) : except io.error α :=
match (cast undefined m : unit → sum α io.error) () with
| sum.inl a := except.ok a
| sum.inr err := except.error err
end

meta def unsafe_get_response (input : chat_state) : string :=
  match unsafe_perform_io (get_response input) with
  | except.ok a := a
  | except.error e := "error"
  end
meta inductive chat_action
  | submit
  | text_change (s : string)

meta def chat_init (props : chat_props) (old_state : option chat_state) : chat_state :=
  {bubbles := [], current_text := ""} <| old_state

meta def chat_view (props : chat_props) (state : chat_state) : list (html chat_action) :=
  [h "div" [className "f4"] [
    h "div" [className "flex flex-column"] (
      state.bubbles.reverse.map (λ bubble,
        h "div" [className "pa2 ma2"] [
          h "span" [className "underline blue mr2"] [bubble.user],
          ": ",
          bubble.body
        ]
      )
    ),
    h "div" [] [
      textbox state.current_text chat_action.text_change,
      button "submit" chat_action.submit -- [todo] how to get it to trigger on enter?
    ]
  ]]

meta def push_bubble (b : bubble) (s : chat_state) : chat_state := {bubbles := b :: s.bubbles, ..s}

meta def chat_update (props : chat_props) (state : chat_state) : chat_action → (chat_state × option empty)
  | chat_action.submit :=
    let text := state.current_text in
    let state := {current_text := "", ..state} in
    let state := push_bubble {body := text, user := "self"} state in
    let state := push_bubble {body := unsafe_get_response state, user := "codex"} state in
    (state, none)
  | (chat_action.text_change s) := ({current_text := s, ..state}, none)

meta def chat_widget {π α : Type} : component π α :=
component.ignore_props $ component.ignore_action $ component.stateful chat_action _ chat_init  chat_update chat_view

-- @zhangir put your cursor on the #html token!
#html chat_widget ()