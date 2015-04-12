(* USAGE

   ledit ocaml 
   #use "projet.ml" ;;

  OU BIEN Si vous n'avez pas installé l'outil ledit de la distribution caml

  ocaml 
  #use "projet.ml" ;;
 *)


(* LE PROJET 2015 (dans l'ordre croissant de difficultés) 

NOTE : tâche à réaliser 

  10 : interprete de MT

 + 1 : MT basiques : 
        1. avancer jusqu'au blanc à droite, 
        2. avancer jusqu'au dollar à droite, 
        3. effacer le ruban, 
           ex: $.1.0.1 -- erase --> $._._._

        4. incrémenter un entier binaire

 + 2 : MT complexes: 
        1. échanger deux cases consécutives à partir de la position courante
            
        2. décaler le mot d'un case à partir de la position courante pour y insérer un séparateur S
             ex: $.1.(0).1 --- dec --> $.1.#.0.1
                      |
        3. dupliquer le mot d'entrée en séparant les deux copies par 2 symboles S
             ex: ($).1.0.1 --- dup --> $.1.0.1.#.#.1.0.1

 + 3 : affichage graphique des configurations successives (mais pas du graphe de la MT) : voir TRACE D'EXÉCUTION en fin de fichier

 + 3 : extension de l'interpréteur pour les MT à deux bandes + exemple de MT à deux bandes (cf. palindrome en TD)

 + 3 : extension qui autorise à mettre une MT existante sur une transition : (q) -- M --> (q')

Vous trouverez en fin de fichier des exemples de TEST et de TRACE D'EXÉCUTION.

 *)



(* ALPHABET FIXÉ *)

type symbol =
  | D (* Debut de ruban *)
  | B (* Blanc *)
  | S (* Sarateur *)
  | Z (* bit Zero *)
  | U (* bit Unity *)


module Symbol =
  (struct
      
      let (_ALPHABET_ : symbol list) = [ D ; B ; S ; Z ; U] 

      let (pretty: symbol -> string) = fun symbol ->
match symbol with
| D -> "$"
| B -> "_"
| S -> "#"
| Z -> "0"
| U -> "1"
    end)
    
    
(* PATTERN matching on symbol *)

type 'a pattern = 
  | ANY
  | SMB of 'a
  | BUT of 'a 
  | IN  of 'a list
  | OUT of 'a list
      
module Pattern = 
  (struct 
      type 'a t = 'a pattern

      let (exactly: 'a pattern -> 'a -> bool) = fun pattern a -> 
pattern = SMB a

      let (matches: 'a pattern -> 'a -> bool) = fun pattern a' ->
match pattern with 
| ANY -> true
| SMB a -> a = a'
| BUT a -> a <> a'
| IN  aS -> List.mem a' aS
| OUT aS -> not (List.mem a' aS)

      let (pretty: ('a -> string) -> 'a pattern -> string) = fun pp pattern ->
match pattern with
| ANY -> "_"
| SMB a -> pp a
| BUT a -> "~" ^ (pp a)
| IN  aS -> "{" ^ (String.concat "," (List.map pp aS)) ^ "}"
| OUT aS -> "~{" ^ (String.concat "," (List.map pp aS)) ^ "}"

      let (print: ('a -> string) -> 'a pattern -> unit) = fun pp pattern -> 
print_string (pretty pp pattern)

    end)

    
    
(* TURING MACHINE ACTION: : reading, writing, moving *)

(* - Moving *)
    
type moving = Left | Here | Right

(* - Reading *)    

type reading = Match of symbol pattern

(* - Writing *)      

type writing =
  | No_Write
  | Write_smb of symbol

   
(* TURING MACHINE STATE *)

type state =
  | Qacc
  | Qrej
  | Q of int

module State =
  (struct

      let (pretty: state -> string) = fun state ->
match state with
| Qacc -> "A"
| Qrej -> "R"
| Q(i) -> "Q" ^ (string_of_int i)
  
    end)
    
    
(* ACTION *)
    
type action = reading * writing * moving 

(* INSTRUCTION *)

type instruction =
  | Action  of action

 
(* TRANSITION *)    

type transition = state * instruction * state

module Transition =
  (struct

      let (nop: state -> state -> transition) = fun source target ->  (source, Action(Match ANY, No_Write, Here), target)
      
    end)
    
(* TURING MACHINE *)

type name = string
      
type turing_machine = { name: name ; initial: state ; transitions: transition list }


module TM =
  (struct


      (* a collection of basic TM *)
      
      let (nop: turing_machine) = { name = "nop" ; initial = Q 0 ; transitions = [] }

      let (to_end: turing_machine) =
      let init = 0 and final = 1 in
      { name = ">B"
      ; initial = Q init
      ; transitions =
          [ (Q init, Action(Match(BUT B), No_Write, Right), Q init)
            ; (Q init, Action(Match(SMB B), No_Write, Here ), Q final)
	      ]
	      }
(* MT basiques :  *)
(*         1. avancer jusqu'au blanc à droite,  *)
(*         2. avancer jusqu'au dollar à droite,  *)
(*         3. effacer le ruban,  *)
(*            ex: $.1.0.1 -- erase --> $._._._ *)

(*         4. incrémenter un entier binaire en Little-Endian*)

(*Basique 1*)
      let (bas_1: turing_machine) =
let init = 0 and final = 1 in
{ name = ">B"
; initial = Q init
; transitions =
    [ (Q init, Action(Match(BUT B), No_Write, Right), Q init)
            ; (Q init, Action(Match(SMB B), No_Write, Here ), Q final)
    ]
}

(*Basique 2 ne prend pas en compte le $ initial*)
      let (bas_2: turing_machine) =
let init = 0 and final = 1 in
{ name = ">$"
; initial = Q init
; transitions =
    [ (Q init, Action(Match(SMB D), No_Write, Right), Q 2) (* On ne prend pas en compte le premier dollard *)
            ;(Q 2, Action(Match(BUT D), No_Write, Right), Q 2)
					; (Q 2, Action(Match(SMB D), No_Write, Here ), Q final)
            ]
}

      (*Basique 3*)
      let (bas_3: turing_machine) =
let init = 0 and final = 1 in
{ name = "_"
; initial = Q init
; transitions =
    [ (Q init, Action(Match(BUT B), Write_smb B, Right), Q init) 
					;(Q init, Action(Match(SMB B), No_Write, Here ), Q final)
            ]
}

      (*Basique 4 en Little-Endian*)
      let (bas_4: turing_machine) =
let init = 0 and final = 1 in
{ name = "+1"
; initial = Q init
; transitions =
    [ (Q init, Action(Match(SMB D), No_Write, Right), Q 2) (* On ne prend pas en compte le premier dollard *)
     ;(Q 2, Action(Match(IN [Z;B]), Write_smb U, Here), Q final)
					;(Q 2, Action(Match(SMB U), Write_smb Z, Right ), Q 2)
            ]
}

 (* + 2 : MT complexes:  *)
 (*        1. échanger deux cases consécutives à partir de la position courante *)
            
 (*        2. décaler le mot d'un case à partir de la position courante pour y insérer un séparateur S *)
 (*             ex: $.1.(0).1 --- dec --> $.1.#.0.1 *)
 (*                      | *)
 (*        3. dupliquer le mot d'entrée en séparant les deux copies par 2 symboles S *)
 (*             ex: ($).1.0.1 --- dup --> $.1.0.1.#.#.1.0.1 *)


  (*Complexe 1*)
      let (cmp_1: turing_machine) =
let init = 0 and final = 1 and dollard = 2 and q1 = 3 and q0 = 4 and rp1 = 5 and rp0 = 6 in
{ name = "swap"
; initial = Q init
; transitions =
     [ (Q init, Action(Match(SMB D), No_Write, Right), Q dollard)
     ;(Q init, Action(Match(SMB U), No_Write, Right), Q rp1)
     ;(Q init, Action(Match(SMB Z), No_Write, Right), Q rp0)
     ;(Q dollard, Action(Match(SMB U), Write_smb D, Left), Q q1)
     ;(Q dollard, Action(Match(SMB Z), Write_smb D, Left), Q q0)
     ;(Q rp1, Action(Match(SMB U), Write_smb U, Left), Q q1)
     ;(Q rp1, Action(Match(SMB Z), Write_smb U, Left), Q q0)
     ;(Q rp0, Action(Match(SMB U), Write_smb Z, Left), Q q1)
     ;(Q rp0, Action(Match(SMB Z), Write_smb Z, Left), Q q0)
     ;(Q q1, Action(Match(BUT B), Write_smb U, Right), Q final)
     ;(Q q0, Action(Match(BUT B), Write_smb Z, Right), Q final)
            ]
}


    end)

(*Complexe 2*)
let (cmp_2: turing_machine) = 
let init = 0 and final = 1 and qDollard = 2 and q1 = 3 and q0 = 4 in
{ name = "dec"
; initial = Q init
; transitions =
  [ (Q init, Action(Match(SMB D), Write_smb S, Right), qDollard) (*Lecture d'un dollard*)
  ;(Q init, Action(Match(SMB Z), Write_smb S, Right), q0) (*Lecture d'un 0 *)
  ;(Q init, Action(Match(SMB U), Write_smb S, Right), q1) (*Lecture d'un 1 *)
  ;(Q qDollard, Action(Match(SMB Z), Write_smb D, Right), q0) (* ecriture du $ et va dans l'etat 0 *)
  ;(Q qDollard, Action(Match(SMB U), Write_smb D, Right), q1) (* ecriture du $ et va dans l'etat 1 *)
  ;(Q q0, Action(Match(SMB Z), Write_smb 0, Right), q0)
  ;(Q q0, Action(Match(SMB U), Write_smb 0, Right), q1)
  ;(Q q1, Action(Match(SMB Z), Write_smb 1, Right), q0)
  ;(Q q1, Action(Match(SMB U), Write_smb 1, Right), q1)
  ;(Q q0, Action(Match(SMB B), Write_smb 0, Right), q final)
  ;(Q q1, Action(Match(SMB B), Write_smb 1, Right), q final)
  ]
  }
end)
    

    
(* BAND and DATA *)
    
type band = { left: symbol list ; head: symbol ; right: symbol list }

type data = symbol list
   
module Band =
  (struct

      let (empty: band) = { left = [] ; head = D ; right = [] }
          
      let (init_with: data -> band) = fun data -> { empty with right = data }

      (* /!\ The left part of the band is written in the reverse ordrer. It is easier to implement this way.
       A band containing  a b c d (e) f g h with the head on (e) will be encoded by
         { left = [d;c;b;a] ; head =e ; right = [f;g;h] }
       *)
          
      let (move_head_right: band -> band) = fun band ->
      match band.right with
      | []    -> { left = band.head::band.left ; head = B ; right = [] }
| s::ms -> { left = band.head::band.left ; head = s ; right = ms }
     
      let (move_head_left: band -> band) = fun band ->
      match band.left with 
      | []    -> { right = band.head::band.right ; head = D ; left = [] }
| s::ms -> { right = band.head::band.right ; head = s ; left = ms }

  
      let (do_move: moving -> band -> band) = fun moving band ->
      match moving with
      | Left  -> move_head_left band
| Right -> move_head_right band 
| Here  -> band
     
      let (do_write: writing -> band -> band) = fun writing band ->
match writing with
  | No_Write -> band
    | Write_smb(smb)-> { right = band.right ; head = smb ; left = band.left }

  
      let (update_wrt: (writing * moving) -> band -> band) = fun (writing,moving) band ->
do_move moving (do_write writing band)

      let (parenthesis: string -> string) = fun string -> "(" ^ string ^ ")"
   
      let (pretty: band -> string) = fun band ->
let strings = 
  (List.map Symbol.pretty (List.rev band.left))
          @
    [ parenthesis (Symbol.pretty band.head) ]
  @
    (List.map Symbol.pretty (band.right))
in
String.concat "." strings

    end)


    
(* CONFIGURATION and EXECUTION *)

type status = Final | Running

type configuration = { band: band ; state: state ; status: status }

module Configuration =
  (struct

      let (empty: configuration) = { band = Band.empty ; state = Q 0 ; status = Final }
     
      let (init_with: turing_machine -> data -> configuration) = fun mt data ->
{ band = Band.init_with data ; state = mt.initial ; status = Running }

      let  (take: transition -> configuration -> configuration) = fun (source,action,target) cfg ->
match action with
| Action( Match(pattern), writing, moving) ->
   if (Pattern.matches pattern cfg.band.head)
   then { cfg with state = target ; band = Band.update_wrt (writing,moving) cfg.band }
   else { cfg with status = Final }

   
   let (one_step: turing_machine -> configuration -> configuration) = fun mt cfg ->
     let (is_outgoing: transition -> bool) = fun (src,_,_) -> 
       match src,cfg.state with
  | Q(i),Q(j) -> i != j
  | _,_ -> true
     and (is_enabled: transition -> bool) = fun (state,action,_) ->  
       (match action with
	      | Action( Match(pattern), _, _) -> state=cfg.state && Pattern.matches pattern cfg.band.head
       )
     in 
             let enabled_transitions = List.filter is_enabled mt.transitions
     in
     match enabled_transitions with
     | [] -> { cfg with status = Final }
          | [transition] -> let conftmp= take transition cfg in
       {conftmp with
 status = (if is_outgoing transition
   then Final else Running)}
     | _ -> failwith "non deterministic MT in one_step"
          
          
      let (pretty: configuration -> string) = fun cfg ->
      String.concat ": " [ State.pretty cfg.state ; Band.pretty cfg.band ]
            
      let (print: configuration -> unit) = fun cfg ->
      print_string ("\n" ^ (pretty cfg) ^ "\n") 

    end)
    


(* IMPERATIVE FEATURES to run the MT STEP BY STEP **)

let _CFG = ref Configuration.empty ;;
  
let _TMA = ref TM.nop ;;

let (initialize: turing_machine -> data -> configuration) = fun mt data ->
  begin
    _TMA := mt ;
    _CFG := Configuration.init_with mt data ;
    Configuration.print (!_CFG) ;
    !(_CFG)
  end
;;

let (one_step: unit -> configuration) = fun () ->
  begin
    _CFG := Configuration.one_step !(_TMA) !(_CFG) ;
    Configuration.print (!_CFG) ;
    !(_CFG)
  end
;;

let rec (run: unit -> configuration) = fun () ->
  let cfg = !(_CFG) in
  if (cfg.status = Final) then !(_CFG)
  else
    begin
      one_step () ;
      run ()
    end 
;;
let rec (move: configuration -> int->configuration)= fun cfg dpl ->
  match dpl with
  |0 -> cfg
  |_ ->    match cfg.band.right with
   |[]->cfg
   |a::suite -> let cfgtmp:configuration =((suite,a,cfg.band.head::cfg.band.left),cfg.state,cfg.status) in 
       move cfgtmp (dpl-1)
;;

let (deplacement : int-> configuration)= fun dpl ->
  begin
    _CFG := move !(_CFG) dpl;
    !(_CFG);
  end
;;
  (* TEST *)    

(* affichage de la MT *)  

  TM.to_end ;;

  (* initialisation de la configuration de départ *)
  
  initialize TM.dup [U;Z;U] ;;

    (* Pour une execution pas à pas, utilisez 
         one_step () ;;
	   one_step () ;;
	     one_step () ;;
	       one_step () ;;
	         one_step () ;;
		   one_step () ;;
		     one_step () ;;
		       one_step () ;;
		         one_step () ;;
			   one_step () ;;
			     one_step () ;;
			       one_step () ;;
			       *)  


    (* Pour execution jusqu'à l'arrêt (si la MT termine...), utilisez 
   run () ;; 
     *)

    run ();;
      
      (* TRACE D'EXÉCUTIONS *)

      (* - traces obtenues avec run() : L'affichage graphique doit donner la même chose en plus joli...

Succession des configurations de la machine dec sur le mot d'entrée 1.0.1

Q3: ($).1.0.1
Q3: $.(1).0.1
Q1: $.#.(0).1
Q0: $.#.1.(1)
Q1: $.#.1.0.(_)
Q42: $.#.1.0.1.(_)
Q42: $.#.1.0.1.(_)

Succession des configurations de la machine dup sur le mot d'entrée 1.0.1

Q2: ($).1.0.1
Q2: $.(1).0.1
Q2: $.1.(0).1
Q2: $.1.0.(1)
Q2: $.1.0.1.(_)
Q3: $.1.0.1.(#)
Q3: $.1.0.(1).#
Q3: $.1.(0).1.#
Q3: $.(1).0.1.#
Q3: ($).1.0.1.#
Q4: (_).#.1.0.1.#
Q5: $.(#).1.0.1.#
Q6: $.#.(1).0.1.#
Q10: $.(#).#.0.1.#
Q1: $.1.(#).0.1.#
Q1: $.1.#.(0).1.#
Q1: $.1.#.0.(1).#
Q1: $.1.#.0.1.(#)
Q1: $.1.#.0.1.#.(_)
Q7: $.1.#.0.1.#.(1)
Q7: $.1.#.0.1.(#).1
Q8: $.1.#.0.(1).#.1
Q8: $.1.#.(0).1.#.1
Q8: $.1.(#).0.1.#.1
Q5: $.1.(#).0.1.#.1
Q6: $.1.#.(0).1.#.1
Q9: $.1.(#).#.1.#.1
Q0: $.1.0.(#).1.#.1
Q0: $.1.0.#.(1).#.1
Q0: $.1.0.#.1.(#).1
Q0: $.1.0.#.1.#.(1)
Q0: $.1.0.#.1.#.1.(_)
Q7: $.1.0.#.1.#.1.(0)
Q7: $.1.0.#.1.#.(1).0
Q7: $.1.0.#.1.(#).1.0
Q8: $.1.0.#.(1).#.1.0
Q8: $.1.0.(#).1.#.1.0
Q5: $.1.0.(#).1.#.1.0
Q6: $.1.0.#.(1).#.1.0
Q10: $.1.0.(#).#.#.1.0
Q1: $.1.0.1.(#).#.1.0
Q1: $.1.0.1.#.(#).1.0
Q1: $.1.0.1.#.#.(1).0
Q1: $.1.0.1.#.#.1.(0)
Q1: $.1.0.1.#.#.1.0.(_)
Q7: $.1.0.1.#.#.1.0.(1)
Q7: $.1.0.1.#.#.1.(0).1
Q7: $.1.0.1.#.#.(1).0.1
Q7: $.1.0.1.#.(#).1.0.1
Q8: $.1.0.1.(#).#.1.0.1
Q5: $.1.0.1.(#).#.1.0.1
Q6: $.1.0.1.#.(#).1.0.1
Q42: $.1.0.1.#.(#).1.0.1
Q42: $.1.0.1.#.(#).1.0.1

*)  



      (* - traceS obtenues avec one_step() dans l'interpréteur 

Succession des configurations de eff sur le mot d'entrée 1.0.1

Q0: ($).1.0.1
- : configuration =
{band = {left = []; head = D; right = [U; Z; U]}; id = "eff"; state = Q 0;
 status = Running}

Q0: $.(1).0.1
- : configuration =
{band = {left = [D]; head = U; right = [Z; U]}; id = "eff"; state = Q 0;
 status = Running}

Q0: $.1.(0).1
- : configuration =
{band = {left = [U; D]; head = Z; right = [U]}; id = "eff"; state = Q 0;
 status = Running}

Q0: $.1.0.(1)
- : configuration =
{band = {left = [Z; U; D]; head = U; right = []}; id = "eff"; state = Q 0;
 status = Running}

Q0: $.1.0.1.(_)
- : configuration =
{band = {left = [U; Z; U; D]; head = B; right = []}; id = "eff"; state = Q 0;
 status = Running}

Q1: $.1.0.1.(_)
- : configuration =
{band = {left = [U; Z; U; D]; head = B; right = []}; id = "eff"; state = Q 1;
 status = Running}

Q1: $.1.0.(1)._
- : configuration =
{band = {left = [Z; U; D]; head = U; right = [B]}; id = "eff"; state = Q 1;
 status = Running}

Q1: $.1.(0)._._
- : configuration =
{band = {left = [U; D]; head = Z; right = [B; B]}; id = "eff"; state = Q 1;
 status = Running}

Q1: $.(1)._._._
- : configuration =
{band = {left = [D]; head = U; right = [B; B; B]}; id = "eff"; state = Q 1;
 status = Running}

Q1: ($)._._._._
- : configuration =
{band = {left = []; head = D; right = [B; B; B; B]}; id = "eff"; state = Q 1;
 status = Running}

Q2: ($)._._._._
- : configuration =
{band = {left = []; head = D; right = [B; B; B; B]}; id = "eff"; state = Q 2;
 status = Running}

Q2: ($)._._._._
- : configuration =
{band = {left = []; head = D; right = [B; B; B; B]}; id = "eff"; state = Q 2;
 status = Final}

Q2: ($)._._._._
- : configuration =
{band = {left = []; head = D; right = [B; B; B; B]}; id = "eff"; state = Q 2;
 status = Final}


Même exécution avec run () ;;

Q0: $.(1).0.1
Q0: $.1.(0).1
Q0: $.1.0.(1)
Q0: $.1.0.1.(_)
Q1: $.1.0.1.(_)
Q1: $.1.0.(1)._
Q1: $.1.(0)._._
Q1: $.(1)._._._
Q1: ($)._._._._
Q2: ($)._._._._
Q2: ($)._._._._

       *)
