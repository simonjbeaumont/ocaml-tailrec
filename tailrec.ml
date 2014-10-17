open Asttypes
open Typedtree
open Lambda

(*---------------------------------------------------------------------------*)
(*         Hacked version of Simplif.emit_tail_infos from the compiler       *)
(*---------------------------------------------------------------------------*)
let annots = ref []
let rec emit_tail_infos is_tail lambda =
  let call_kind args =
    if is_tail
    && ((not !Clflags.native_code)
        || (!Simplif.is_tail_native_heuristic (List.length args)))
   then `Tail
   else `Stack in
  match lambda with
  | Lvar _ -> ()
  | Lconst _ -> ()
  | Lapply (func, l, loc) ->
      list_emit_tail_infos false l;
      (* Stypes.record (Stypes.An_call (loc, call_kind l)) *)
      annots := (loc, call_kind l) :: !annots
  | Lfunction (_, _, lam) ->
      emit_tail_infos true lam
  | Llet (_, _, lam, body) ->
      emit_tail_infos false lam;
      emit_tail_infos is_tail body
  | Lletrec (bindings, body) ->
      List.iter (fun (_, lam) -> emit_tail_infos false lam) bindings;
      emit_tail_infos is_tail body
  | Lprim (Pidentity, [arg]) ->
      emit_tail_infos is_tail arg
  | Lprim (Psequand, [arg1; arg2])
  | Lprim (Psequor, [arg1; arg2]) ->
      emit_tail_infos false arg1;
      emit_tail_infos is_tail arg2
  | Lprim (_, l) ->
      list_emit_tail_infos false l
  | Lswitch (lam, sw) ->
      emit_tail_infos false lam;
      list_emit_tail_infos_fun snd is_tail sw.sw_consts;
      list_emit_tail_infos_fun snd is_tail sw.sw_blocks;
      Misc.may  (emit_tail_infos is_tail) sw.sw_failaction
  | Lstringswitch (lam, sw, d) ->
      emit_tail_infos false lam;
      List.iter
        (fun (_,lam) ->  emit_tail_infos is_tail lam)
        sw ;
      Misc.may (emit_tail_infos is_tail) d
  | Lstaticraise (_, l) ->
      list_emit_tail_infos false l
  | Lstaticcatch (body, _, handler) ->
      emit_tail_infos is_tail body;
      emit_tail_infos is_tail handler
  | Ltrywith (body, _, handler) ->
      emit_tail_infos false body;
      emit_tail_infos is_tail handler
  | Lifthenelse (cond, ifso, ifno) ->
      emit_tail_infos false cond;
      emit_tail_infos is_tail ifso;
      emit_tail_infos is_tail ifno
  | Lsequence (lam1, lam2) ->
      emit_tail_infos false lam1;
      emit_tail_infos is_tail lam2
  | Lwhile (cond, body) ->
      emit_tail_infos false cond;
      emit_tail_infos false body
  | Lfor (_, low, high, _, body) ->
      emit_tail_infos false low;
      emit_tail_infos false high;
      emit_tail_infos false body
  | Lassign (_, lam) ->
      emit_tail_infos false lam
  | Lsend (_, meth, obj, args, loc) ->
      emit_tail_infos false meth;
      emit_tail_infos false obj;
      list_emit_tail_infos false args;
      (* Stypes.record (Stypes.An_call (loc, call_kind (obj :: args))) *)
      annots := (loc, call_kind (obj :: args)) :: !annots
  | Levent (lam, _) ->
      emit_tail_infos is_tail lam
  | Lifused (_, lam) ->
      emit_tail_infos is_tail lam
and list_emit_tail_infos_fun f is_tail =
  List.iter (fun x -> emit_tail_infos is_tail (f x))
and list_emit_tail_infos is_tail =
  List.iter (emit_tail_infos is_tail)

(*---------------------------------------------------------------------------*)
(*                              Debug functions                              *)
(*---------------------------------------------------------------------------*)
let string_of_call_kind = function `Tail -> "tail" | `Stack -> "stack"

let rec dump_annot_list = function
  | [] -> ()
  | (l,ann)::anns ->
    Location.print Format.std_formatter l;
    Printf.printf "Annot: %s\n%!" (string_of_call_kind ann);
    dump_annot_list anns

(*---------------------------------------------------------------------------*)
(*                           Actual checking code                            *)
(*---------------------------------------------------------------------------*)
let assert_tail_calls_for (f_ident : Ident.t) body : unit =
  Printf.printf "    Generating annotations for lambda term...\n%!";
  annots := [];
  emit_tail_infos true body;
  let rec iterlam f =  Lambda.iter (fun l -> f l; iterlam f l) in
  let assert_apply lam = match lam with
    | Lapply (Lvar i, args, loc) when i = f_ident ->
      Printf.printf "    Checking if the lambda term Lapply(%s, _, _) is a tail call...%!" (Ident.unique_name i);
      let (_, a) = List.find (fun (l, _) -> l = loc) !annots in
      begin match a with
      | `Tail -> 
        Printf.printf "OK!\n%!";
      | `Stack -> 
        Printf.printf "ERROR!\n%!";
        Location.print_error Format.err_formatter loc;
        Format.eprintf "this call to %s is not a tail-call!\n%!" f_ident.Ident.name;
      end
    | _ -> ()
  in
  iterlam assert_apply body

module ExpressionIteratorArg = struct
  include TypedtreeIter.DefaultIteratorArgument

  let enter_structure_item st =
    match st.str_desc with
    | Tstr_value(Recursive, ({vb_pat = {pat_desc = Tpat_var(f,_)}; vb_expr; vb_attributes}::_ as vbs)) ->
        if List.exists (fun (l, _) -> l.txt = "tailrec") vb_attributes then
          Printf.printf "  Found value %s at %s marked as tail-recursive...\n%!" (f.Ident.name) "[location]";
          let fname = Ident.unique_name f in
          let lam = Translcore.(transl_let Recursive vbs (transl_exp vb_expr)) in
          begin match lam with
          | Lletrec (bindings, body) ->
              Printf.printf "  Compiled %s into Lletrec lambda term...\n%!" fname;
              Printf.printf "  Checking all calls to %s in lambda body are tail calls...\n%!" fname;
              assert_tail_calls_for f body
          | _ -> failwith "Compiled value into something other than a Lletrec"
          end
    | _ -> ()
end

(*---------------------------------------------------------------------------*)
(*                                Entry point                                *)
(*---------------------------------------------------------------------------*)
let () =
  let open Cmt_format in
  for i = 1 to Array.length Sys.argv - 1 do
    let fn = Sys.argv.(i) in
    try
      let {cmt_annots; cmt_modname; _} = read_cmt fn in
      begin match cmt_annots with
      | Implementation st ->
        print_endline "Found implementation annotation in cmt.";
        let module I = TypedtreeIter.MakeIterator(ExpressionIteratorArg) in
        I.iter_structure st;
      | _ -> ()
      end;
    with exn ->
      Format.printf "Cannot read '%s': %s@." fn (Printexc.to_string exn)
  done
