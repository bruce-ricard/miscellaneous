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

let print s =
  Lwt.async (fun () ->
      Lwt_io.(write_line stdout) s
    )

let rec aux f i =
  Lwt_unix.sleep 1. >>= (fun () ->
    let s = string_of_int i in
    print (Printf.sprintf "sending %s" s);
    f s;
    Lwt.return ();
  ) >>= fun () -> aux f (i + 1)

let send_data f =
  print "wtf";
  aux f 0

let () =
  let e,f = React.E.create () in
  let printgot = React.E.map (fun s -> print (Printf.sprintf"got %s\n" s)) e in
  let () = Lwt.async (fun () -> send_data f; Lwt.return ()) in
  f "dadoudadou";
  Test_app.register
    ~service:main_service
    (fun () () ->
      let r,f = React.E.create () in
      let printgot2 = React.E.map (fun s -> (print (Printf.sprintf "got2 %s\n" s))) r in
      let d = Eliom_react.Down.of_react r in
      let elt = div [pcdata "data"] in
      let _ = [%client
                  (Client_lib.init ();
                   Client_lib.update_html_content ~%elt ~%d;
                   let p = React.E.map print_endline ~%d in
                   () : unit)
              ]
      in
      f "plop";
      f "cacou";
      Lwt.async (fun () -> (send_data f; Lwt.return ()));
      Lwt.return
        (Eliom_tools.F.html
           ~title:"test"
           ~css:[["css";"test.css"]]
           Html5.F.(body [
                        h2 [pcdata "Welcome from Eliom's distillery!"];
                        br ();
                        elt;
    ])));
