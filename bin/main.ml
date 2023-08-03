open Soup
open Str

let getBody url =
  let%lwt _, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
  (* Adapt response to a Markup.ml stream. *)
  let%lwt body = body |> Cohttp_lwt.Body.to_string in
  Lwt.return body

let find_str s search =
  let re = regexp_string search in
  try
    ignore (search_forward re s 0);
    true
  with Not_found -> false

let () =
  Lwt_main.run
    (let url = "https://www.bbc.com/zhongwen/simp/chinese-news-66371405" in
     let%lwt body = getBody url in
     let soup = body |> parse in
     let content = Soup.(soup $ "main") in
     let classList = Soup.classes content in
     let media =
       classList |> List.find_opt (fun name -> find_str name "StyledMediaItem")
     in
     match media with
     | Some _ ->
         print_endline "meida news";
         Lwt.return_unit
     | None ->
         List.iter (fun cls -> remove_class cls content) classList;
         let noscripts = Soup.(content $$ "noscript") in
         Soup.iter (fun node -> set_name "div" node) noscripts;
         descendants content |> elements
         |> iter (fun node ->
                if List.mem (name node) [ "header"; "figcaption" ] then
                  delete node
                else
                  match attribute "data-component" node with
                  | Some s ->
                      if
                        List.mem s
                          [
                            "links-block";
                            "include-block";
                            "topic-list";
                            "video-block";
                          ]
                      then delete node
                      else
                        classes node |> List.iter (fun s -> remove_class s node)
                  | None ->
                      classes node |> List.iter (fun s -> remove_class s node));

         content |> to_string |> write_file "download.html";

         (match content $? "img" with
         | Some img -> (
             match img |> attribute "src" with
             | Some s -> print_endline s
             | None -> print_endline "no img")
         | None -> print_endline "no img");
         Lwt.return_unit)
