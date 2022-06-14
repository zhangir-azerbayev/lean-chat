import system.io
import query_api 

open widget tactic
section json 
meta def list.lookup_prod {α β} : (list (α × β)) → (α → bool) → option β
| [] _ := none
| (⟨a,b⟩::xs) p := if p a then pure b else xs.lookup_prod p


meta def json.lookup : json → string → option json
| (json.object kvs) str := kvs.lookup_prod $ λ k, k = str
| _ _ := none

end json 

meta def text_of_return_json (parsed : json) : string := 
match json.lookup parsed "choices" with 
| some choices_json :=
  match json.lookup choices_json[0] "text" with 
    |some wrapped_string := 
      match wrapped_string with 
      | json.of_string return_text := return_text 
      | _ := "error"
      end
    | none := "asd;lfjsa;" 
  end
| none := "Something else bad happend!"
end 

/- @zhangir: write your code for getting response from codex here :-) -/
meta def get_response : string → io string
| s := 
do {
let prompt := prompt_of_nl_statement s few_shot_prompt,  
return_json ← get_completion_of_request {prompt:=prompt},
io.put_str return_json,  
let maybe_return_parsed := json.parse return_json, 
match maybe_return_parsed with 
| some j := return $ (text_of_return_json j) ++ " :=" 
| none := return "Something bad happened!" 
end 
}

-- use some @gebner magic here
meta def unsafe_perform_io {α} (m : io α) : except io.error α :=
match (cast undefined m : unit → sum α io.error) () with
| sum.inl a := except.ok a
| sum.inr err := except.error err
end

meta def unsafe_get_response (input : string) : string :=
  match unsafe_perform_io (get_response input) with
  | except.ok a := a
  | except.error e := "error"
  end

meta structure bubble :=
  (body : string) -- [todo] add formatting etc
  (user : string)

meta def chat_props := unit

meta structure chat_state : Type :=
  (bubbles : list bubble)
  (current_text : string)

meta inductive chat_action
  | submit
  | text_change (s : string)

meta def chat_init (props : chat_props) (old_state : option chat_state) : chat_state :=
  {bubbles := [], current_text := ""} <| old_state

meta def chat_view (props : chat_props) (state : chat_state) : list (html chat_action) :=
  [h "div" [] [
    h "div" [className "flex flex-column"] (
      state.bubbles.reverse.map (λ bubble,
        h "div" [] [
          h "span" [className "underline blue"] [bubble.user],
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
    let state := push_bubble {body := unsafe_get_response text, user := "codex"} state in
    (state, none)
  | (chat_action.text_change s) := ({current_text := s, ..state}, none)

meta def chat_widget {π α : Type} : component π α :=
component.ignore_props $ component.ignore_action $ component.stateful chat_action _ chat_init  chat_update chat_view

-- @zhangir put your cursor on the #html token!
#html chat_widget ()