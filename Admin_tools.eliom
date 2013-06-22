open Eliom_content.Html5.D

let view_skills = Eliom_service.service ~path:[""] ~get_params:Eliom_parameter.unit ()

{server{
  let add_skill_rpc : (string*int64, unit) Eliom_pervasives.server_function
      = server_function Json.t<string*int64> (fun _ -> Lwt.return ())
}}

let template ~depth ~id =
  let _onclick = 
    {Dom_html.mouseEvent Js.t -> unit { fun _ ->
         Lwt.ignore_result (lwt () = %add_skill_rpc ("x",%id) in
                            Lwt.return () )
    }} in
  div []

let build roots info g  =
  let visited = ref Core_kernel.Std.Int64.Map.empty in
  let rec _f ~depth v =
    if Core_kernel.Std.Int64.Map.mem !visited v then
      template ~depth ~id:v 
    else 
      template ~depth ~id:v 
  in
  !visited 

