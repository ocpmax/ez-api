open EzAPI.TYPES

type error_handler = (int -> string option -> unit)

module type S = EzReq_S.S
module type Interface = EzReq_S.Interface

let request_reply_hook = ref (fun () -> ())

let before_hook = ref (fun () -> ())

let decode_result ?error encoding f res =
  let res = match res with "" -> "{}" | res -> res in
  match EzEncoding.destruct encoding res with
  | res -> f res
  | exception exn ->
     match error with
     | None -> ()
     | Some error ->
       let msg = Printf.sprintf "Decoding error: %s in\n%s"
           (Printexc.to_string exn) res in
        error (-3) (Some msg)


let any_get = ref (fun ?meth:_m _msg _url ?headers:_ f ->
    f (Error (-2,Some "No http client loaded")))
let any_post = ref (fun ?meth:_m ?content_type:(_x="") ?content:(_y="") _msg _url ?headers:_ f ->
    f (Error (-2,Some "No http client loaded")))

module Make(S : Interface) : S = struct

  let init () =
    any_get := S.get;
    any_post := S.post

  (* print warnings generated when building the URL before
   sending the request *)
  let internal_get ?meth msg (URL url) ?headers ?error f =
    EzAPI.warnings (fun s -> Printf.kprintf EzDebug.log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m ->
      Some (String.uppercase_ascii @@ EzAPI.str_of_method m) in
    S.get ?meth msg url ?headers
      (fun code ->
         !request_reply_hook ();
         match code with
         | Ok res -> f res
         | Error (n, body) ->
           match error with
           | None -> ()
           | Some f -> f n body)

  let internal_post ?meth ?content_type ?content
      msg (URL url) ?headers ?error f =
    EzAPI.warnings (fun s -> Printf.kprintf EzDebug.log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m -> Some (
        String.uppercase_ascii @@ EzAPI.str_of_method m) in
    S.post ?meth ?content_type ?content ?headers msg url
      (fun code ->
         !request_reply_hook ();
         match code with
         | Ok res -> f res
         | Error (n, body) ->
           match error with
           | None -> ()
           | Some f -> f n body)

  let add_hook f =
    let old_hook = !before_hook in
    before_hook := (fun () -> old_hook (); f ())

  let add_reply_hook f =
    let old_hook = !request_reply_hook in
    request_reply_hook := (fun () -> old_hook (); f ())

  let handlers ?error service f =
    let error code msg =
      match EzAPI.service_errors service ~code, msg with
      | Some enc, Some s ->
        decode_result ?error enc (fun x -> f (Error x)) s
      | _, _ -> match error with
        | Some error -> error code msg (* unhandled *)
        | None -> ()
    in
    let encoding = EzAPI.service_output service in
    let ok = decode_result ~error encoding (fun x -> f (Ok x)) in
    ok, error

  let get = internal_get
  let post = internal_post

  module Raw = struct

    let get0 api
        ( service : ('output, 'error, 'security) EzAPI.service0 )
        msg
        ?(post=false)
        ?headers
        ?error
        ?(params=[])
        f () =
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      if post then
        let url = EzAPI.forge0 api service [] in
        let content = EzAPI.encode_args service url params in
        let content_type = EzUrl.content_type in
        let meth = if meth = `GET then `POST else meth in
        internal_post ~meth msg url ~content ~content_type ?headers ~error ok
      else
        let url = EzAPI.forge0 api service params in
        internal_get ~meth msg url ?headers ~error ok

    let get1 api
        ( service : ('arg, 'output, 'error, 'security) EzAPI.service1 )
        msg
        ?(post=false)
        ?headers
        ?error
        ?(params=[])
        f
        (arg : 'arg) =
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      if post then
        let url = EzAPI.forge1 api service arg []  in
        let content = EzAPI.encode_args service url params in
        let content_type = EzUrl.content_type in
        let meth = if meth = `GET then `POST else meth in
        internal_post ~meth msg url ~content ~content_type ?headers ~error ok
      else
        let url = EzAPI.forge1 api service arg params in
        internal_get ~meth msg url ?headers ~error ok

    let get2 api
        ( service : ('arg1, 'arg2, 'output, 'error, 'security) EzAPI.service2 )
        msg
        ?(post=false)
        ?headers
        ?error
        ?(params=[])
        f
        (arg1 : 'arg1) (arg2 : 'arg2)=
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      if post then
        let url = EzAPI.forge2 api service arg1 arg2 []  in
        let content = EzAPI.encode_args service url params in
        let content_type = EzUrl.content_type in
        let meth = if meth = `GET then `POST else meth in
        internal_post ~meth msg url ~content ~content_type ?headers ~error ok
      else
        let url = EzAPI.forge2 api service arg1 arg2 params in
        internal_get ~meth msg url ?headers ~error ok

    let post0 :
      type i.
      EzAPI.base_url ->
      (i, 'output, 'error, 'security) EzAPI.post_service0 ->
      string ->
      ?headers:(string * string) list ->
      ?error: error_handler ->
      ?params:(EzAPI.param * EzAPI.arg_value) list ->
      ?url_encode:bool ->
      input:i ->
      (('output, 'error) Result.result -> unit) ->
      unit =
      fun api service msg ?headers ?error ?(params=[]) ?(url_encode=false)
        ~input f ->
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      let input_encoding = EzAPI.service_input service in
      let url = EzAPI.forge0 api service params in
      let content, content_type = match input_encoding with
        | EzAPI.Empty -> "", "application/json"
        | EzAPI.Binary [] -> input, "multipart/form-data"
        | EzAPI.Binary (h :: _) -> input, h
        | EzAPI.Json enc ->
          if not url_encode then
            EzEncoding.construct enc input, "application/json"
          else
            EzUrl.encode_obj enc input, EzUrl.content_type in
      internal_post ~meth msg url ~content ~content_type ?headers ~error ok

    let post1 :
      type i.
      EzAPI.base_url ->
      ('arg, i, 'output, 'error, 'security) EzAPI.post_service1 ->
      string ->
      ?headers:(string * string) list ->
      ?error: error_handler ->
      ?params:(EzAPI.param * EzAPI.arg_value) list ->
      ?url_encode:bool ->
      input:i ->
      'arg ->
      (('output, 'error) Result.result -> unit) -> unit =
      fun api service msg ?headers ?error ?(params=[]) ?(url_encode=false)
        ~input arg f ->
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      let input_encoding = EzAPI.service_input service in
      let url = EzAPI.forge1 api service arg params in
      let content, content_type = match input_encoding with
        | EzAPI.Empty -> "", "application/json"
        | EzAPI.Binary [] -> input, "multipart/form-data"
        | EzAPI.Binary (h :: _) -> input, h
        | EzAPI.Json enc ->
          if not url_encode then
            EzEncoding.construct enc input, "application/json"
          else
            EzUrl.encode_obj enc input, EzUrl.content_type in
      internal_post msg ~meth url ~content ~content_type ?headers ~error ok

    let post2 :
      type i.
      EzAPI.base_url ->
      ('arg1, 'arg2, i, 'output, 'error, 'security) EzAPI.post_service2 ->
      string ->
      ?headers:(string * string) list ->
      ?error: error_handler ->
      ?params:(EzAPI.param * EzAPI.arg_value) list ->
      ?url_encode:bool ->
      input:i ->
      'arg1 -> 'arg2 ->
      (('output, 'error) Result.result -> unit) -> unit =
      fun api service msg ?headers ?error ?(params=[]) ?(url_encode=false)
        ~input arg1 arg2 f ->
      !before_hook ();
      let ok, error = handlers ?error service f in
      let meth = EzAPI.service_meth service in
      let input_encoding = EzAPI.service_input service in
      let url = EzAPI.forge2 api service arg1 arg2 params in
      let content, content_type = match input_encoding with
        | EzAPI.Empty -> "", "application/json"
        | EzAPI.Binary [] -> input, "multipart/form-data"
        | EzAPI.Binary (h :: _) -> input, h
        | EzAPI.Json enc ->
          if not url_encode then
            EzEncoding.construct enc input, "application/json"
          else
            EzUrl.encode_obj enc input, EzUrl.content_type in
      internal_post msg ~meth url ~content ~content_type ?headers ~error ok
  end

  include Raw

  module Legacy = struct

    type ('output, 'error, 'security) service0 =
      ('output) EzAPI.Legacy.service0
      constraint 'security = [< EzAPI.security_scheme ]

    type ('arg, 'output, 'error, 'security) service1 =
      ('arg, 'output) EzAPI.Legacy.service1
      constraint 'security = [< EzAPI.security_scheme ]

    type ('arg1, 'arg2, 'output, 'error, 'security) service2 =
      ('arg1, 'arg2, 'output) EzAPI.Legacy.service2
      constraint 'security = [< EzAPI.security_scheme ]

    type ('input, 'output, 'error, 'security) post_service0 =
      ('input, 'output) EzAPI.Legacy.post_service0
      constraint 'security = [< EzAPI.security_scheme ]

    type ('arg, 'input, 'output, 'error, 'security) post_service1 =
      ('arg, 'input, 'output) EzAPI.Legacy.post_service1
      constraint 'security = [< EzAPI.security_scheme ]

    type ('arg1, 'arg2, 'input, 'output, 'error, 'security) post_service2 =
      ('arg1, 'arg2, 'input, 'output) EzAPI.Legacy.post_service2
      constraint 'security = [< EzAPI.security_scheme ]

    let unresultize f = function
      | Ok res -> f res
      | Error u -> EzAPI.unreachable u

    let get0 api service
        msg ?post ?headers ?error ?params f () =
      Raw.get0 api service msg ?post ?headers ?error ?params (unresultize f) ()

    let get1 api service
        msg ?post ?headers ?error ?params f arg =
      Raw.get1 api service msg ?post ?headers ?error ?params (unresultize f) arg

    let get2 api service
        msg ?post ?headers ?error ?params f arg1 arg2 =
      Raw.get2 api service msg ?post ?headers ?error ?params (unresultize f) arg1 arg2

    let post0 api service
        msg ?headers ?error ?params ?url_encode ~input f =
      Raw.post0 api service msg ?headers ?error ?params ?url_encode ~input (unresultize f)

    let post1 api service
        msg ?headers ?error ?params ?url_encode ~input arg f =
      Raw.post1 api service msg ?headers ?error ?params ?url_encode ~input arg (unresultize f)

    let post2 api service
        msg ?headers ?error ?params ?url_encode ~input arg1 arg2 f =
      Raw.post2 api service msg ?headers ?error ?params ?url_encode ~input arg1 arg2 (unresultize f)

  end

end

module ANY : S = Make(struct
    let get ?meth msg url ?headers f =
      !any_get ?meth msg url ?headers f
    let post ?meth ?content_type ?content msg url ?headers f =
      !any_post ?meth ?content_type ?content msg url ?headers f
  end)
