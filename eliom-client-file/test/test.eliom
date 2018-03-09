[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
]

module Test_app =
  Eliom_registration.App (
    struct
      let application_name = "test"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Test_app.register
    ~service:main_service
    (fun () () ->
      let _ = [%client
                  (Client_lib.init () : unit)
              ]
      in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"test"
           ~css:[["css";"test.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
           ])))
