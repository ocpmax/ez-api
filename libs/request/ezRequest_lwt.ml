open EzAPI.TYPES

module type S = EzReq_lwt_S.S
module type Interface = EzReq_lwt_S.Interface

let (>|=) = Lwt.(>|=)
let return = Lwt.return

type 'a api_error = 'a EzReq_lwt_S.api_error =
  | KnownError of { code : int ; error : 'a }
  | UnknownError of { code : int ; msg : string option }

type ('output, 'error) api_result = ('output, 'error) EzReq_lwt_S.api_result

let handle_error kn = function
  | KnownError {code; error} -> code, kn error
  | UnknownError {code; msg} -> code, msg

let string_of_error kn = function
  | KnownError {code; error} ->
    let content = match kn error with None -> "" | Some s -> ": " ^ s in
    Printf.sprintf "Error %d%s" code content
  | UnknownError {code; msg} ->
    let content = match msg with None -> "" | Some s -> ": " ^ s in
    Printf.sprintf "Unknown Error %d%s" code content

let request_reply_hook = ref (fun () -> ())

let before_hook = ref (fun () -> ())

let decode_result encoding err_encodings = function
  | Error (code, None) -> Error (UnknownError { code ; msg = None })
  | Error (code, Some msg) ->
    (match err_encodings ~code with
      | None -> Error (UnknownError { code ; msg = Some msg })
      | Some encoding ->
        try Error (
            KnownError { code ; error = EzEncoding.destruct encoding msg })
        with _ -> Error (UnknownError { code ; msg = Some msg })
    )
  | Ok res ->
    let res = match res with "" -> "{}" | res -> res in
    match EzEncoding.destruct encoding res with
    | res -> (Ok res)
    | exception exn ->
      let msg = Printf.sprintf "Decoding error: %s in\n%s"
          (Printexc.to_string exn) res in
      Error (UnknownError { code = -3; msg = Some msg })

let handle_result service res =
  let err_encodings = EzAPI.service_errors service in
  let encoding = EzAPI.service_output service in
  decode_result encoding err_encodings res

let any_get = ref (fun ?meth:_m ?headers:_ ?msg:_ _url ->
    return (Error (-2, Some "No http client loaded"))
  )
let any_post = ref (fun ?meth:_m ?content_type:_ ?content:_ ?headers:_ ?msg:_ _url ->
    return (Error (-2, Some "No http client loaded"))
  )


module Make(S : Interface) : S = struct

  let init () =
    any_get := S.get;
    any_post := S.post

  (* print warnings generated when building the URL before
   sending the request *)
  let internal_get ?meth ?headers ?msg (URL url) =
    EzAPI.warnings (fun s -> Printf.kprintf EzDebug.log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m -> Some (
        String.uppercase_ascii @@ EzAPI.str_of_method m) in
    S.get ?meth ?headers ?msg url >|= fun code ->
    !request_reply_hook ();
    code

  let internal_post ?meth ?content_type ?content ?headers ?msg (URL url) =
    EzAPI.warnings (fun s -> Printf.kprintf EzDebug.log "EzRequest.warning: %s" s);
    let meth = match meth with None -> None | Some m -> Some (
        String.uppercase_ascii @@ EzAPI.str_of_method m) in
    S.post ?meth ?content_type ?content ?headers ?msg url >|= fun code ->
    !request_reply_hook ();
    code

  let add_hook f =
    let old_hook = !before_hook in
    before_hook := (fun () -> old_hook (); f ())

  let add_reply_hook f =
    let old_hook = !request_reply_hook in
    request_reply_hook := (fun () -> old_hook (); f ())

  let get = internal_get
  let post = internal_post

  module Raw = struct

    let get0 ?(post=false) ?headers ?(params=[]) ?msg
        api (service: ('output, 'error, 'security) EzAPI.service0) =
      !before_hook ();
      let meth = EzAPI.service_meth service in
      if post then
        let url = EzAPI.forge0 api service [] in
        let content = EzAPI.encode_args service url params in
        let content_type = EzUrl.content_type in
        internal_post ~meth ~content ~content_type ?headers ?msg url >|=
        handle_result service
      else
        let url = EzAPI.forge0 api service params in
        internal_get ~meth ?headers ?msg url >|= handle_result service

    let get1 ?(post=false) ?headers ?(params=[]) ?msg
        api (service : ('arg,'output, 'error, 'security) EzAPI.service1) (arg : 'arg) =
      !before_hook ();
      let meth = EzAPI.service_meth service in
      if post then
        let url = EzAPI.forge1 api service arg []  in
        let content = EzAPI.encode_args service url params in
        let content_type = EzUrl.content_type in
        internal_post ~meth ~content ~content_type ?headers ?msg url >|=
        handle_result service
      else
        let url = EzAPI.forge1 api service arg params in
        internal_get ~meth ?headers ?msg url >|= handle_result service

    let get2 ?(post=false) ?headers ?(params=[]) ?msg
        api (service : ('arg1, 'arg2, 'output, 'error, 'security) EzAPI.service2)
        (arg1 : 'arg1) (arg2 : 'arg2) =
      !before_hook ();
      let meth = EzAPI.service_meth service in
      if post then
        let url = EzAPI.forge2 api service arg1 arg2 []  in
        let content = EzAPI.encode_args service url params in
        let content_type = EzUrl.content_type in
        internal_post ~meth ~content ~content_type ?headers ?msg url >|=
        handle_result service
      else
        let url = EzAPI.forge2 api service arg1 arg2 params in
        internal_get ~meth ?headers ?msg url >|= handle_result service

    let post0 :
      type i.
      ?headers:(string * string) list ->
      ?params:(EzAPI.param * EzAPI.arg_value) list ->
      ?msg:string ->
      ?url_encode:bool ->
      input:i ->
      EzAPI.base_url ->
      (i, 'output, 'error, 'security) EzAPI.post_service0 ->
      ('output, 'error) api_result Lwt.t =
      fun ?headers ?(params=[]) ?msg ?(url_encode=false) ~input api service ->
      !before_hook ();
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
      internal_post ~meth ~content ~content_type ?headers ?msg url >|=
      handle_result service

    let post1 :
      type i.
      ?headers:(string * string) list ->
      ?params:(EzAPI.param * EzAPI.arg_value) list ->
      ?msg:string ->
      ?url_encode:bool ->
      input:i ->
      EzAPI.base_url ->
      ('arg, i, 'output, 'error, 'security) EzAPI.post_service1 -> 'arg ->
      ('output, 'error) api_result Lwt.t =
      fun ?headers ?(params=[]) ?msg ?(url_encode=false) ~input api service arg ->
      !before_hook ();
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
      internal_post ~meth ~content ~content_type ?headers ?msg url >|=
      handle_result service

    let post2 :
      type i.
      ?headers:(string * string) list ->
      ?params:(EzAPI.param * EzAPI.arg_value) list ->
      ?msg:string ->
      ?url_encode:bool ->
      input:i ->
      EzAPI.base_url ->
      ('arg1, 'arg2, i, 'output, 'error, 'security) EzAPI.post_service2 -> 'arg1 -> 'arg2 ->
      ('output, 'error) api_result Lwt.t =
      fun ?headers ?(params=[]) ?msg ?(url_encode=false) ~input api service arg1 arg2 ->
      !before_hook ();
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
      internal_post ~meth ~content ~content_type ?headers ?msg url >|=
      handle_result service
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

    open EzAPI.Legacy

    let unresultize = function
      | Ok res -> Ok res
      | Error UnknownError { code ; msg } -> Error (code, msg)
      | Error KnownError { error ; _ } -> EzAPI.unreachable error


    let get0 ?post ?headers ?params ?msg
        api (service: 'output EzAPI.Legacy.service0) =
      get0 ?post ?headers ?params ?msg api service
      >|= unresultize

    let get1 ?post ?headers ?params ?msg
        api (service : ('arg,'output) service1) (arg : 'arg) =
      get1 ?post ?headers ?params ?msg api service arg
      >|= unresultize

    let get2 ?post ?headers ?params ?msg
        api (service : ('arg1, 'arg2, 'output) service2) (arg1 : 'arg1) (arg2 : 'arg2) =
      get2 ?post ?headers ?params ?msg api service arg1 arg2
      >|= unresultize

    let post0 ?headers ?params ?msg ?url_encode ~(input : 'input)
        api (service : ('input,'output) post_service0) =
      post0 ?headers ?params ?msg ?url_encode ~input api service
      >|= unresultize

    let post1 ?headers ?params ?msg ?url_encode ~(input : 'input)
        api (service : ('arg, 'input,'output) post_service1) (arg : 'arg) =
      post1 ?headers ?params ?msg ?url_encode ~input api service arg
      >|= unresultize

    let post2 ?headers ?params ?msg ?url_encode ~(input : 'input)
        api (service : ('arg1, 'arg2, 'input, 'output) post_service2)
        (arg1 : 'arg1) (arg2 : 'arg2) =
      post2 ?headers ?params ?msg ?url_encode ~input api service arg1 arg2
      >|= unresultize

  end

end

module ANY = Make(struct
    let get ?meth ?headers ?msg url = !any_get ?meth ?headers ?msg url
    let post ?meth ?content_type ?content ?headers ?msg url =
      !any_post ?meth ?content_type ?content ?headers ?msg url
  end)
