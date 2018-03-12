[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D

    module React = Lwt_react
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
      Lwt.return
        (Eliom_tools.F.html
           ~title:"test"
           ~css:[["css";"test.css"]]
           Html5.F.(body [
                        h2 [pcdata "Welcome from Eliom's distillery!"];
    ])))

let print s =
  Lwt.async (fun () ->
      Lwt_io.(write_line stdout) s
    )

let rec call_every_second f i =
  Lwt_unix.sleep 1. >>= (fun () ->
    let s = string_of_int i in
    print (Printf.sprintf "sending %s" s);
    f s;
    call_every_second f (i + 1)
  )

let send_data f =
  call_every_second f 0

let () =
  let e,f = React.E.create () in
  let printgot = React.E.map (fun s -> print (Printf.sprintf"got %s\n" s)) e in
  Lwt.async (fun () -> send_data f)
