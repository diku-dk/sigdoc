(* Utilities *)

val sigdoc_url = "http://github.com/melsman/sigdoc"

fun println s = print (s ^"\n")

structure Map = struct
  open StringMap
  fun argsForWhich (m:'a map) (p : 'a -> bool) : string list =
      Fold (fn ((s,v),a) => if p v then s::a else a) nil m
end

(* Concatenable strings *)
structure CS = CString
infix 6 &  (* as ^ *)
val op & = CS.&
val op $ = CS.$

(* Lexing of sml tokens *)
signature LEX = sig
  type id = string
  type excon = id
  type tycon = id
  type strid = id
  type sigid = id
  datatype t = ID of id
             | SEP of string
             | SPACE of string
             | COMMENT of string
             | KW of string
             | LINK of {href:string,elem:string}
  val lex : string -> t list
  val defs : t list -> id list * excon list * tycon list * (strid*sigid) list
  val conv : (strid -> sigid) -> t list -> t list
  val isKw : string -> bool
  val pp  : t list -> CS.t
end

fun htmlencode s : string =
    let fun enc #"<" = "&lt;"
	  | enc #">" = "&gt;"
	  | enc #"&" = "&amp;"
	  | enc #"\"" = "&quot;"
	  | enc c = String.str c
    in String.translate enc s
    end

fun taga t a e = $("<" ^ t ^ a ^ ">") & e & $("</" ^ t ^ ">")
fun tag t e = taga t "" e
fun taga0 t a = taga t a ($"")
fun tag0 t = $("<" ^ t ^ " />")
fun tdwl w e = taga "td" (" width='" ^ Int.toString w ^ "%' align='left'") e

fun encode id =
    let val chars = "#%&/<>-_+'~^:!@$=?|"
      fun i_to_s i = if i < 10 then "0" ^ Int.toString i else Int.toString i
      fun f c = case CharVector.findi (fn (_,e) => c = e) chars of
                  SOME (i,_) => "$" ^ i_to_s i
                | NONE => String.str c
    in String.translate f id
    end

fun plingencode id =
    String.translate (fn #"'" => "\\'" | c => String.str c) id

fun init_space s =
    (Char.isSpace(String.sub(s,0)))
    handle _ => false

fun remove_init_ws s =
    let fun loop n =
            if Char.isSpace(String.sub(s,n)) then loop (n+1)
            else n
    in String.extract(s,loop 0,NONE)
    end handle _ => ""

structure Lex : LEX =
struct
type id = string
type excon = id
type tycon = id
type strid = id
type sigid = id
datatype t = ID of id
           | SEP of string
           | SPACE of string
           | COMMENT of string
           | KW of string
           | LINK of {href:string,elem:string}

fun sOf (ID s) = $ s
  | sOf (SEP s) = $ s
  | sOf (SPACE s) = $ s
  | sOf (COMMENT s) = $ s
  | sOf (KW s) = tag "b" ($s)
  | sOf (LINK {href,elem}) = taga "a" (" href='" ^ href ^ "'") ($elem)

(* signature level key words *)
fun isKw s =
    case s of
      "and" => true
    | "eqtype" => true
    | "end" => true
    | "exception" => true
    | "sig" => true
    | "signature" => true
    | "structure" => true
    | "type" => true
    | "datatype" => true
    | "val" => true
    | "include" => true
    | _ => false

fun spaces t =
    case t of
      SPACE s :: rest =>
      (case spaces rest of
         NONE => SOME(s,rest)
       | SOME (sp,r) => SOME(s^sp,r))
    | _ => NONE

fun valId t =
    case t of
      ID "val" :: t =>
      (case spaces t of
         SOME(space, ID id :: rest) => SOME (SPACE space,id,rest)
       | _ => NONE)
    | _ => NONE

fun longid id =
    case String.fields (fn c => c = #".") id of
      [id1,id2] => SOME (id1,id2)
    | _ => NONE

fun lookupStructure m id =
    m id

fun conv m t =
    let fun conv0 t =
            case valId t of
              SOME(sp,id,rest) =>
              KW "val" :: sp :: LINK{href="#" ^ encode id,
                                     elem=id} :: conv0 rest
            | NONE =>
              case t of
                nil => nil
              | ID id :: rest =>
                if isKw id then KW id :: conv0 rest
                else
                  (case longid id of
                     SOME(id1,id2) =>
                     let val signat = lookupStructure m id1
                       val file = signat ^ ".sml.html"
                       val href1 = file ^ "#" ^ "$S" ^ id1
                       val href2 = file ^ "#" ^ "$T" ^ id2
                     in
                       LINK{href=href1,elem=id1} :: ID "." ::
                       LINK{href=href2,elem=id2} :: conv0 rest
                     end
                   | NONE => ID id :: conv0 rest)
              | e :: rest => e :: conv0 rest
    in conv0 t
    end

fun pp ts = CS.concat(map sOf ts)

type pos = int
type level = int

type stream = string * pos

fun getc (s,p) =
    if String.size s > p then
      SOME(String.sub(s,p),(s,p+1))
    else NONE

fun pr_error_pos (s,p) =
    "Error at position " ^ Int.toString p ^ ": " ^ String.extract(s,p,NONE)

fun read_chars (f:char->bool) (is:stream) (C:string -> t) =
    let fun read is a =
            case getc is of
              SOME(c,is') =>
              if f c then
                read is' (c::a)
              else if a <> [] then
                SOME(C(String.implode(rev a)),is)
              else NONE
            | NONE =>
              if a <> [] then
                SOME(C(String.implode(rev a)),is)
              else NONE
    in read is []
    end

fun read_id is =
    let fun isIdChar c =
            Char.isAlpha c orelse Char.isDigit c orelse c = #"'" orelse c = #"_" orelse c = #"."
    in read_chars isIdChar is ID
    end

fun read_symb is =
    let val symbolChars = "@$/*-+<>!#%?^~:|=&"
        fun isSymbolChar c = CharVector.exists (fn c' => c=c') symbolChars
    in read_chars isSymbolChar is ID
    end

fun read_sep is =
    let val sepChars = ",;{}[]()"
        fun isSepChar c = CharVector.exists (fn c' => c=c') sepChars
    in case getc is of
         SOME(c,is') =>
         if isSepChar c then SOME(SEP(String.str c),is')
         else NONE
       | NONE => NONE
    end

fun read_space is =
    read_chars Char.isSpace is SPACE

fun read_comment is =
    let fun read lev is a =
            case getc is of
              SOME(#"(",is1) =>
              (case getc is1 of
                 SOME(#"*",is2) =>
                 read (lev+1) is2 (#"*":: #"("::a)
               | _ => read lev is1 (#"("::a))
            | SOME(#"*",is1) =>
              (case getc is1 of
                 SOME(#")",is2) =>
                 let val a = #")":: #"*"::a
                 in if lev=1 then
                      SOME(COMMENT(implode(rev a)),is2)
                    else read (lev-1) is2 a
                 end
               | _ => read lev is1 (#"*"::a))
            | SOME(c,is1) => read lev is1 (c::a)
            | NONE => raise Fail "immature end of comment"
    in case getc is of
         SOME(#"(",is1) =>
         (case getc is1 of
            SOME(#"*",is2) =>
            read 1 is2 [#"*", #"("]
          | _ => NONE)
       | _ => NONE
    end

fun eos (s,i) = i >= String.size s

fun lex s =
    let fun read (is:stream) (a:t list) : t list =
            case read_space is of
              SOME(t,is) => read is (t::a)
            | NONE =>
            case read_comment is of
              SOME(t,is) => read is (t::a)
            | NONE =>
            case read_sep is of
              SOME(t,is) => read is (t::a)
            | NONE =>
            case read_symb is of
              SOME(t,is) => read is (t::a)
            | NONE =>
            case read_id is of
              SOME(t,is) => read is (t::a)
            | NONE =>
              if eos is then rev a
              else raise Fail ("Error reading string: " ^ pr_error_pos is)
    in read (s,0) nil
    end

fun defs ts =
    let fun tyvar id =
            case explode id of
              #"'" :: _ => true
            | _ => false
        fun eat_tyvars ts =
            case ts of
              ID id :: SPACE _ :: ts' =>
              if tyvar id then ts' else ts
            | _ => ts
        fun typespec "type" = true
          | typespec "eqtype" = true
          | typespec "datatype" = true
          | typespec _ = false

        fun loop (acc as (ids,tycons,excons,strs)) ts =
            case ts of
              ID "val" :: SPACE _ :: ID id :: ts =>
              loop (id::ids,tycons,excons,strs) ts
            | ID "exception" :: SPACE _ :: ID excon :: ts =>
              loop (ids,tycons,excon::excons,strs) ts
            | ID "structure" :: SPACE _ :: ID strid ::
              SPACE _ :: ID ":" :: SPACE _ :: ID sigid :: ts =>
              loop (ids,tycons,excons,(strid,sigid)::strs) ts
            | ID kw :: SPACE _ :: ts =>
              if typespec kw then
                (case eat_tyvars ts of
                   ID tycon :: ts =>
                   loop (ids,tycon::tycons,excons,strs) ts
                 | _ => loop acc ts)
              else loop acc ts
            | _ :: ts => loop acc ts
            | nil => acc
    in
      loop ([],[],[],[]) ts
    end
end

structure R = RegExp

type sigid = string
type strid = string
type id = string

datatype origin = ORIGIN_BASIS
                | ORIGIN_PKG of string
                | ORIGIN_NONE

type sigmap = {short_comment:string, long_comment:string,
               src:string, origin:origin, comments:string} Map.map   (* dom=sigid *)

fun pkg_id p =
    let val re = RegExp.fromString ".*github.com/.*/(.*)"
    in case RegExp.extract re p of
           SOME [id] => id
         | _ => p
    end

fun find_origin s =
    let val re_pkg = RegExp.fromString ".*(github.com/.*/.*)/.*"
        val re_bas = RegExp.fromString ".*basis.*"
    in case RegExp.extract re_pkg s of
           SOME [p] => ORIGIN_PKG p
         | _ => if RegExp.match re_bas s then ORIGIN_BASIS
                else ORIGIN_NONE
    end

fun readFile f =
    let val () = print ("Reading file: " ^ f ^ "\n")
        val is = TextIO.openIn f
    in let val s = TextIO.inputAll is
       in TextIO.closeIn is; s
       end handle ? => (TextIO.closeIn is; raise ?)
    end

exception SigFormatError of string
fun read_sig (f:string) : sigmap =
    let val s = readFile f
    in case R.extract (R.fromString ".*\\(\\*\\*(.*)\\*\\).*(signature ([0-9a-zA-Z_]+) .*end)[\n ]*\\(\\*\\*(.*)\\*\\).*") s of
         SOME [c,sigid,src,cs] =>
         let val origin = find_origin f
             val (shortc,longc) =
                 case R.extract (R.fromString "([^\n]*)\n[ ]*\n(.*)") c of
                   SOME [shortc,longc] => (shortc,longc)
                 | _ => ("",c)
         in Map.singleton(sigid, {short_comment=shortc,long_comment=longc,src=src,comments=cs,origin=origin})
         end
       | SOME ss => (List.app (fn s => print(s ^ "\n")) ss;
                     raise Fail "read_sig wrong format 0")
       | NONE => raise SigFormatError "content not on the form '(**...*)...signature XXX =...sig...end...(**...*)'"
    end

type strmap = {sigid:sigid,short_comment:string,origin:origin} Map.map   (* StrId -> SigId *)

fun read_impl (sigmap:sigmap) (f:string) : strmap =
    let val origin = find_origin f
        fun loop acc s =
            case R.extract (R.fromString "(.*)structure ([0-9a-zA-Z_]+) :>? ([0-9a-zA-Z_]+) .*") s of
              SOME [rest,strid,sigid] =>
              let val (rest, c) =
                      case R.extract (R.fromString "(.*)\\(\\*\\*([^\n]*)\\*\\)\n") rest of
                        SOME [rest,c] => (rest,c)
                      | SOME [c] => ("",c)
                      | _ => (rest,"")
                  val acc = case Map.lookup sigmap sigid of
                                NONE => (print ("Skipping " ^ strid ^ " : " ^ sigid ^
                                                ", as " ^ sigid ^ " is not known.\n");
                                         acc)
                              | SOME _ => Map.add(strid,{sigid=sigid,short_comment=c,origin=origin},acc)
              in loop acc rest
              end
            | SOME [strid,sigid] =>
              Map.add(strid,{sigid=sigid,short_comment="",origin=origin},acc)
            | SOME ss => (List.app (fn s => print(s ^ "\n")) ss;
                          raise Fail "read_impl wrong format 1")
            | NONE => acc
    in loop Map.empty (readFile f)
    end

fun match_id nil h = NONE
  | match_id (id::ids) h =
    if h = id orelse String.isPrefix (id ^ " ") h orelse
       String.isPrefix (id ^ "(") h orelse
       String.isSubstring (" " ^ id ^ " ") h then
      SOME id
    else match_id ids h

fun pp_comments (ids,tycons,excons,strs) s =
    let (*val () = print ("ids = " ^ Int.toString (length ids) ^"\n")*)
(*(*(*
        fun loop cs s =
            case R.extract (R.fromString "(.*)\\n\\[([\\-0-9a-zA-Z_:<>=&%#@|'!*/$(), ]+)\\](.*)") s of
              SOME [rest,h,b] =>
              loop ((h,b)::cs) rest
            | SOME [h,b] => ((h,b)::cs)
            | SOME ss => (List.app (fn s => print(s ^ "\n")) ss;
                          raise Fail "pp_comments wrong format 1")
            | NONE => cs
*)
        val lines = String.fields (fn #"\n" => true | _ => false) s
        val lines = map (fn s => s ^ "\n") lines
        fun read_head s =
            (case CharVector.findi (fn (_,#"]") => true | _ => false) s of
               SOME (i,_) =>
               (String.extract(s,1,SOME (i-1)),
                String.extract(s,i+1,NONE) handle _ => "")
             | NONE => raise Fail "read_head")
            handle _ => raise Fail ("read_head2:" ^ s)
        fun only_ws s = CharVector.all Char.isSpace s
        fun eat_wss (s::ss) = if only_ws s then eat_wss ss
                              else s::ss
          | eat_wss nil = nil
        fun finalize a cs =
            case eat_wss(rev a) of
              l0::ls =>
              let val (h,r) = read_head l0
              in (h, concat (r::ls))::cs
              end
            | nil => cs
        fun loop cs a (l::ls) =
            if String.isPrefix "[" l then
              loop (finalize a cs) [l] ls
            else loop cs (l::a) ls
          | loop cs a nil = rev(finalize a cs)
      val cs = loop [] [] lines
      fun layout_head h = if CS.toString h = "Discussion" then tag "i" h else ($"[") & tag "tt" h & ($"]")
      fun layout_body b =
          let
            val b = remove_init_ws b
            val lines = String.fields (fn #"\n" => true | _ => false)  b
            fun next_init_space ls =
                (init_space(hd ls))
                handle _ => false
            fun loop nil true acc = "</pre>\n"::acc
              | loop nil false acc = acc
              | loop (l0::ls) ispre acc =
                let val l = l0 ^ "\n"
                in
                  if ispre then
                    if init_space l0 then
                      if next_init_space ls then
                        loop ls true (l::acc)
                      else loop ls true acc
                    else loop ls false (l::"</pre>\n"::acc)
                  else if only_ws l then
                    loop ls false acc
                  else if init_space l then
                    loop ls true (l::"<pre>\n"::acc)
                  else loop ls false (l::acc)
                end
          in $(concat(rev(loop lines false nil)))
          end
    in tag "dl"
           (CS.concat
                (map (fn (h,b) =>
                         let
                           val h2 = htmlencode h
                           val h3 =
                               case match_id ids h of
                                 SOME id =>
                                 let val name = encode id
                                 in taga "a" (" name='" ^ name ^ "'") ($h2)
                                 end
                               | NONE => $ h2
                           val b = htmlencode b
                         in if only_ws b then tag "dt" (tag "b" (layout_head h3))
                            else tag "dt" (tag "b" (layout_head h3)) & ($" ")
                                     & tag "dd" (layout_body b) & tag0 "br"
                         end) cs))
    end

val libpath : string ref = ref ""

fun page h idx b =
    let val str_idx_html = "str_idx.html"
        val sig_idx_html = "sig_idx.html"
        val id_idx_html = "id_idx.html"
        val pkg_idx_html = "pkg_idx.html"
        val generated_tags_js = "generated_tags.js"

        val style_css = OS.Path.concat(!libpath,"style.css")
        val jquery_ui_css = OS.Path.concat(!libpath,"jquery-ui.css")
        val jquery_1_9_1_js = OS.Path.concat(!libpath,"jquery-1.9.1.js")
        val jquery_ui_js = OS.Path.concat(!libpath,"jquery-ui.js")

        val str_idx_link = taga "a" (" href='" ^ str_idx_html ^ "'") ($"Structures")
        val sig_idx_link = taga "a" (" href='" ^ sig_idx_html ^ "'") ($"Signatures")
        val id_idx_link = taga "a" (" href='" ^ id_idx_html ^ "'") ($"Identifiers")
        val pkg_idx_link = taga "a" (" href='" ^ pkg_idx_html ^ "'") ($"Packages")
        val search = taga0 "input" " id='tags' placeholder='Search' style='width:100%; margin-right:20px;'"
        val head =
          tag "head"
            (taga0 "link" (" rel='stylesheet' href='" ^ jquery_ui_css ^ "'") &
             taga0 "script" (" src='" ^ jquery_1_9_1_js ^ "'") &
             taga0 "script" (" src='" ^ jquery_ui_js ^ "'") &
             taga0 "script" (" src='" ^ generated_tags_js ^ "'") &
             taga0 "link" (" rel='stylesheet' href='" ^ style_css ^ "'") &
             tag "script"
               ($ "$(function() { \
                  \  $( '#tags' ).autocomplete({\
                  \    source: availableTags,\
                  \    select: function(event,ui){ window.location = ui.item.value; }\
                  \  });\
                  \});")
            )
        fun tr e = tag "tr" e
        fun tdr e = taga "td" " align='right'" e
        fun tdc e = taga "td" " align='center'" e
        fun td e = tag "td" e
    in
      tag "html"
          (tag "head" head &
           tag "body"
             (tag "p" (str_idx_link & $" | " & tdc sig_idx_link & $" | " & tdr id_idx_link & $" | " & tdr pkg_idx_link) &
              taga "p" " style='width:100%'" search &
              tag0 "hr" &
              tag "h4" h &
              tag "p" idx &
              b &
              tag0 "hr" &
              tag "i" ($"Generated by " & taga "a" (" href='" ^ sigdoc_url ^ "'") ($"SigDoc"))))
    end

fun strs_for_sigid sigid (strmap:strmap) =
    Map.argsForWhich strmap (fn {sigid=x,...} => sigid=x)

fun pp_origin origin =
    case origin of
        ORIGIN_BASIS => tag "i" ($ "(basis)")
      | ORIGIN_PKG p => tag "i"
                            ($"(pkg " &
                              taga "a" (" href='http://" ^ p ^ "'") ($ p) &
                              $")")
      | ORIGIN_NONE => tag "i" ($"(none)")

fun pp strmap (sigid, {short_comment,long_comment,src,comments,origin}) =
    let val ts = Lex.lex src
      val ids = Lex.defs ts
      val idmap = Map.singleton(sigid,ids)
      val ts = Lex.conv (fn x => x) ts
      val src2 = Lex.pp ts
      open Lex
      fun layout_struct x =
          tag "b" ($("structure " ^ x ^ " : " ^ sigid ^ " ") & pp_origin origin & ($"\n"))
      val space = $" "
      val comments =
          pp_comments ids comments
          handle Fail s => (print ("Warning: Failed to print comments for " ^ sigid ^ " - " ^ s ^ "\n"); $"")

      val output =
          page
              ($"Signature " & tag "code" ($sigid) & space & tag "tt" (pp_origin origin))
              ($(htmlencode short_comment))
              ($(htmlencode long_comment) &
                tag0 "hr" &
                tag "pre" (CS.concat (map layout_struct (strs_for_sigid sigid strmap))) &
                tag0 "hr" &
                tag "pre" src2 &
                tag0 "hr" &
                comments)
    in (output, idmap)
    end

fun writeFile f a =
    let val () = print ("Writing file: " ^ f ^ "\n")
        val os = TextIO.openOut f
    in (TextIO.output(os,a);
        TextIO.closeOut os)
       handle ? => (TextIO.closeOut os; raise ?)
    end

fun gen_idx {head: string,
             lines: 'a list,
             line_id: 'a -> string,
             line_entry: 'a -> CS.t,
             line_bodies: 'a -> CS.t list,
             sep: string} =
    let
      fun section ch part =
          if CS.toString part = "" then $""
          else
          let val h =
                  if Char.isAlpha ch then String.str (Char.toUpper ch)
                  else "Symbols"
              val h = taga "a" (" name='"^h^"'") ($h)
          in tag "h4" h & taga "table" " width='100%'" part
          end
      val (ch,ch_acc,acc,chs) =
          foldl (fn (line,(ch,ch_acc,acc,chs)) =>
                    let
                      val bodies = line_bodies line
                      val id = line_id line
                      val ch2 = String.sub(id,0)
                      val e = line_entry line
                      val n = List.length bodies
                      val bodies = List.map (tdwl (75 div n)) bodies
                      val entry = tag "tr" (tdwl 20 (tag "tt" e) & tdwl 5 ($sep) & (CS.concat bodies))
                    in if not(Char.isAlpha ch2) orelse ch=ch2 then
                         (ch,ch_acc & entry,acc,chs)
                       else (ch2,entry,acc & section ch ch_acc,ch2::chs)
                    end)
                (#"*",$"",$"",nil) lines
      val cs = acc & section ch ch_acc
      val idx =
          CS.concatWith " | "
          (map (fn c =>
                   let val h = String.str (Char.toUpper c)
                   in taga "a" (" href='#"^h^"'") ($h)
                   end) (rev chs))
    in page ($head) idx cs
    end

fun gen_sig_idx (sigmap:sigmap, strmap) =
    let val sigs = Map.list sigmap
        val sigs = ListSort.sort (fn (x,y) => String.compare (#1 x,#1 y)) sigs
        val im = List.map (fn (s,{short_comment,...}) =>
                              let val t = s ^ ".sml.html"
                                  val strs = strs_for_sigid s strmap
                                  val strs = List.foldl (fn (s,a) => tag "tt" ($s) & ($" ") & a) ($"") strs
                              in (s, taga "a" (" href='" ^ t ^ "' title='" ^ short_comment ^ "'")
                                          (tag "tt" ($s)), [strs])
                              end) sigs
        val cs = gen_idx {head="Signatures",
                          lines=im,
                          line_id= #1,
                          line_entry= #2,
                          line_bodies= #3,
                          sep="&nbsp;"}
    in writeFile "sig_idx.html" (CS.toString cs)
    end

fun pp_sigid s sigid =
    let val t = sigid ^ ".sml.html"
    in taga "a" (" href='" ^ t ^ "'") (tag "tt" ($s))
    end

fun gen_str_idx (sigmap:sigmap, strmap) =
    let val strs = Map.list strmap
        val strs = ListSort.sort (fn ((x,y),(x1,y1)) => String.compare(x,x1)) strs
        val im = List.map (fn (strid,{sigid,short_comment,origin}) =>
                              let val c =
                                    if short_comment <> "" then short_comment
                                    else
                                      case Map.lookup sigmap sigid of
                                        SOME {short_comment,...} => short_comment
                                      | NONE => ""
                              in (strid, taga "div" (" title='" ^ c ^ "'")
                                              ($strid), [pp_sigid sigid sigid])
                              end) strs
        val cs = gen_idx {head="Structures",
                          lines=im,
                          line_id= #1,
                          line_entry = #2,
                          line_bodies= #3,
                          sep=":"}
    in writeFile "str_idx.html" (CS.toString cs)
    end

type pkgmap = {full:string, sigs:unit Map.map, impls:string Map.map} Map.map

fun gen_pkg_idx (sigmap:sigmap, strmap:strmap) =
    let
      fun insert_sig (p,sigid,m) =
          let val id = pkg_id p
          in case Map.lookup m id of
                 SOME {full,sigs,impls} =>
                 Map.add(id,{full=full,sigs=Map.add(sigid,(),sigs),impls=impls},m)
               | NONE => Map.add(id,{full=p,sigs=Map.singleton(sigid,()),impls=Map.empty},m)
          end
      fun insert_str (p,strid,sigid,m) =
          let val id = pkg_id p
          in case Map.lookup m id of
                 SOME {full,sigs,impls} =>
                 Map.add(id,{full=full,sigs=sigs,impls=Map.add(strid,sigid,impls)},m)
               | NONE => Map.add(id,{full=p,sigs=Map.empty,impls=Map.singleton(strid,sigid)},m)
          end

      val pkgmap = Map.Fold (fn ((sigid,{origin,...}),a) =>
                                case origin of
                                    ORIGIN_PKG p => insert_sig(p,sigid,a)
                                  | ORIGIN_BASIS => insert_sig("basis",sigid,a)
                                  | ORIGIN_NONE => insert_sig("unknown",sigid,a)) Map.empty sigmap
      val pkgmap = Map.Fold (fn ((strid,{origin,sigid,...}),a) =>
                                case origin of
                                    ORIGIN_PKG p => insert_str(p,strid,sigid,a)
                                  | ORIGIN_BASIS => insert_str("basis",strid,sigid,a)
                                  | ORIGIN_NONE => insert_str("unknown",strid,sigid,a)) pkgmap strmap

      val im = List.map (fn (p,{full,sigs,impls}) =>
                            let val sigs = List.foldl (fn (s,a) => pp_sigid s s & ($" ") & a) ($"") (Map.dom sigs)
                                val strs = List.foldl (fn ((s,sigid),a) => pp_sigid s sigid & ($" ") & a) ($"") (Map.list impls)
                            in (p, taga "a" (" href='http://" ^ full ^ "' title='" ^ full ^ "'")
                                        (tag "tt" ($p)),
                                [$"Signatures: " & sigs,
                                 $"Structures: " & strs]
                               )
                            end) (Map.list pkgmap)
      val cs = gen_idx {head="Packages",
                        lines=im,
                        line_id= #1,
                        line_entry= #2,
                        line_bodies= #3,
                        sep="&nbsp;"}
    in writeFile "pkg_idx.html" (CS.toString cs)
    end

fun gen_id_idx (idmap, sigmap, strmap) =
    let val im = Map.Fold (fn ((sigid, (ids,_,_,_)),a) =>
                           let val strs = strs_for_sigid sigid strmap
                           in map (fn id => (id,sigid,strs)) ids @ a
                           end) nil idmap
      val im = ListSort.sort (fn((id,_,_),(id2,_,_)) => String.compare (id,id2)) im
      fun compact nil a = rev a
        | compact ((id,sigid,strs)::rest) nil =
             compact rest [(id,[(sigid,strs)])]
        | compact ((id,sigid,strs)::rest) (acc as ((id2,args)::acc2)) =
          if id = id2 then
            compact rest ((id,(sigid,strs)::args)::acc2)
          else
            compact rest ((id,[(sigid,strs)])::acc)
      val im = compact im nil
      fun layout_impls (id, nil) = $""
        | layout_impls (id, (sigid,strs)::rest) =
          List.foldl (fn (s,a) => a & taga "a" (" href='" ^ sigid ^ ".sml.html'") (tag "tt" ($s))
                                    & (tag "tt" ($("." ^ id))) & ($" "))
                     ($"") strs & layout_impls (id, rest)
      val cs =
          gen_idx {head="Identifiers",
                   lines=im,
                   line_id= #1,
                   line_entry = $ o #1,
                   line_bodies=fn x => [layout_impls x],
                   sep="&nbsp;"}
      fun qq s = "'" ^ s ^ "'"
      fun pair e1 e2 = "{label:" ^ e1 ^ ",value:" ^ e2 ^ "}"
      fun prtag id strid sigid = pair (qq (plingencode(strid ^ "." ^ id))) (qq(sigid ^ ".sml.html"))
      fun tags nil = $""
        | tags ((id, nil)::rest) = tags rest
        | tags ([(id, [(sigid,[strid])])]) = $(prtag id strid sigid)
        | tags ((id, (sigid,nil)::es)::rest) = tags ((id,es)::rest)
        | tags ((id, (sigid,strid::strids)::es)::rest) =
          $(prtag id strid sigid) & $"," & tags ((id,(sigid,strids)::es)::rest)
      val alltags = $"var availableTags = [" & tags im & $"];"
    in writeFile "id_idx.html" (CS.toString cs)
     ; writeFile "generated_tags.js" (CS.toString alltags)
    end

fun gen (sigfiles:string list, implfiles) =
    (* Assumption: One signature for each sigfile (named according to the file).
     * Look in all implfiles for strings of the form
          structure A : B
          structure A :> B
     *)
    let
      val sigmap : sigmap =
          foldl (fn (x,a) =>
                    let val m = read_sig x
                    in Map.plus(a,m)
                    end handle SigFormatError s =>
                               (print ("Skipping file: " ^ s ^ "\n");
                                a)
                ) Map.empty sigfiles
      val strmap : strmap =
          foldl (fn (x,a) => Map.plus(a,read_impl sigmap x)) Map.empty implfiles
      fun out (arg as (s,a), idmap) =
          let val (cstr, idmap2) = pp strmap arg
              val str = CS.toString cstr
              val file = s ^ ".sml.html"
          in writeFile file str;
             Map.plus(idmap,idmap2)
          end
      val idmap = Map.Fold out Map.empty sigmap
    in
       gen_sig_idx (sigmap, strmap);
       gen_str_idx (sigmap, strmap);
       gen_pkg_idx (sigmap, strmap);
       gen_id_idx (idmap, sigmap, strmap)
    end

fun help () =
    (print "Usage: sigdoc [-libpath p] [-sigs FILES] [-impl FILES]\n\n";
     print "For further information, please consult the Sigdoc\n";
     print "documentation at https://github.com/melsman/sigdoc\n")

val sigs : string list ref = ref nil
val impl : string list ref = ref nil

fun reg r xs = r := xs

val () = case ParseArg.run [ParseArg.Unary("-libpath",reg libpath),
                            ParseArg.Multi("-sigs",reg sigs),
                            ParseArg.Multi("-impl",reg impl)] of
           [] => if !sigs = nil then help()
                 else gen(!sigs,!impl)
         | _ => help()
