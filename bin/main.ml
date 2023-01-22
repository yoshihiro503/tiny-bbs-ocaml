open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

(* Define a database connection and a table to store messages *)
let db = Sqlite3.db_open "bbs.db"
let () =
  Sqlite3.exec db "CREATE TABLE messages (id INTEGER PRIMARY KEY, author TEXT, body TEXT, timestamp DATETIME)"
  |> ignore

module Buffer = struct
  type key = string
  type 'a t = (key, 'a) Hashtbl.t
  let create len : 'a t = Hashtbl.create len
  let add key value t = Hashtbl.add t key value
  let to_list t = Hashtbl.to_seq_values t |> List.of_seq
end

(* Define a route to handle GET requests for the home page *)
let handle_home_get _ _ =
  let buffer = Buffer.create 100 in
  Sqlite3.exec_no_headers db "SELECT * FROM messages ORDER BY timestamp DESC"
    ~cb:(fun row ->
      let id = row.(0) |> Option.value ~default:"12345"
      and author = row.(1) |> Option.value ~default:"anone"
      and body = row.(2) |> Option.value ~default:"bnone"
      and timestamp = row.(3) |> Option.value ~default:"timestampnone" in
      (* Render the message using the Mustache template *)
      let message =
        `O ["id", `String id;
            "author", `String author;
            "body", `String body;
            "timestamp", `String timestamp]
      in
      let message_template = "
        <div class='message'>
          <h3>{{author}}</h3>
          <p>{{body}}</p>
          <p class='timestamp'>{{timestamp}}</p>
        </div>"
                             |> Mustache.of_string
      in
      Mustache.render message_template message
      |> fun elem -> Buffer.add id elem buffer)
  |> fun _query_result ->
  (* Render the home page using the Mustache template *)
  let home_template = "
    <html>
      <head>
        <title>BBS</title>
      </head>
      <body>
        <h1>BBS</h1>
        {{#messages}}{{{.}}}{{/messages}}
        <form action='/' method='post'>
          <input type='text' name='author' placeholder='Your name'/>
          <br/>
          <textarea name='body' placeholder='Your message'></textarea>
          <br/>
          <input type='submit' value='Post'/>
        </form>
      </body>
    </html>"
                      |> Mustache.of_string
  in
  let messages = Buffer.to_list buffer in
  let home_page =
    `O ["messages", `A (List.map (fun s -> `String s) messages)]
  in
  let body = Mustache.render home_template home_page in
  let headers = Header.init () |> fun h -> Header.add h "content-type" "text/html" in
  Server.respond_string ~headers ~status:`OK ~body ()

(* Define a route to handle POST requests for the home page *)
let handle_home_post _ body =
  (* Extract the message from the request body *)
  body |> Cohttp_lwt.Body.to_string
  >>= fun body_str ->
  let params = Uri.query_of_encoded body_str in
  let author = List.assoc "author" params |> List.hd
  and body = List.assoc "body" params |> List.hd in
  (* Insert the message into the database *)
  let sql = "INSERT INTO messages (author, body, timestamp) VALUES ('" ^ author ^ "', '" ^ body ^ "', datetime('now'))" in
  Sqlite3.exec db sql |> Lwt.return
  (* Redirect to the home page *)
  >>= fun _ ->
  Server.respond_redirect ~uri:(Uri.of_string "/") ()

(* Define the server *)
let server =
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback:(fun _conn req _body ->
    match req.Request.meth with
    | `GET -> handle_home_get req _body
    | `POST -> handle_home_post req _body
    | _ -> Server.respond_string ~status:`Method_not_allowed ~body:"Method not allowed" ()) ()
  )

(* Start the server *)
let () = ignore (Lwt_main.run server)
